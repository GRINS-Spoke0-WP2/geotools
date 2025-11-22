library(plumber)
library(geotools)
library(httr)
library(jsonlite)
library(future)
library(dplyr)

map_R_to_SQL_type <- function(r_type) {
  
  type_mapping <- list(
    integer = "INT",
    numeric = "DOUBLE",
    character = "STRING",
    factor = "STRING",
    logical = "BOOLEAN",
    Date = "DATE",
    POSIXct = "TIMESTAMP"
  )
  
  if (r_type %in% names(type_mapping)) {
    return(type_mapping[[r_type]])
  } else {
    log("Error", "R type not supported")
    stop()
  }
}

log <- function(status, message, auth) {
  
  log_message <- sprintf(
    "%s [%s] - [%s] %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    auth$username,
    status,
    message
  )
  
  # file
  write(
    log_message,
    file = "geomatching_service.log",
    append = TRUE
  )
  
  # API
  POST(
    url = sprintf("https://%s:%s/externalService/insertStatusInformation", Sys.getenv("AMELIA_HOST"), Sys.getenv("AMELIA_PORT")),
    body = list(
      BearerToken = auth$bearer_token,
      username = auth$username,
      filename = auth$myp_table_name,
      statusCode = status,
      message = message
    ),
    encode = "json",
    httr::content_type_json()
  )
}

invoke_API <- function(url, body, auth){
  
  # setup
  attempt <- 1
  max_retries <- 5
  delay <- 10
  
  repeat {
    response <- tryCatch(
      {
        POST(
          url = url,
          body = body,
          encode = "json",
          content_type_json()
        )
      },
      error = function(e) {
        log("Error", sprintf("API call failed: %s", e$message), auth)
        return(NULL)
      }
    )
    
    if (!is.null(response)) {
      if (httr::status_code(response) == 200) {
        return(content(response))
      } else {
        log("Warning", sprintf("attempt %d of %d, status code %d", attempt, max_retries, status_code(response)), auth)
      }
    } else {
      log("Warning", sprintf("attempt %d of %d", attempt, max_retries), auth)
    }
    
    attempt <- attempt + 1
    
    if (attempt > max_retries) {
      log("Error", "max retries reached, unable to complete the request.", auth)
      stop()
    }
    
    Sys.sleep(delay)
  }
}

validate_body <- function(body, auth){
  
  # check #1: missing fields
  required_fields <- c("BearerToken", "username", "datasets", "aggregation_level")
  missing_fields <- setdiff(required_fields, names(body))
  if(length(missing_fields) > 0){
    log("Error", sprintf("missing fields: %s", paste(missing_fields, collapse = ", ")), auth)
    stop()
  }
  
  # check #2: 'datasets' field
  i <- 1
  for(dataset_i in body$datasets){
    
    # check #2.1: missing fields
    required_fields <- c("table_name", "x_column", "y_column", "temporal_column",
                         "harmonize_columns", "crs", "format", "data_type")
    missing_fields <- setdiff(required_fields, names(dataset_i))
    if(length(missing_fields) > 0){
      log("Error", sprintf("dataset %d, missing fields: %s", i, paste(missing_fields, collapse = ", ")), auth)
      stop()
    }
    
    # check #2.2: 'format' field
    if(!(dataset_i$format %in% c("long format (xyt)", "matrice 3D"))){
      log("Error", sprintf("Dataset %d, invalid format: '%s'. Allowed values are: 'long format (xyt)', 'matrice 3D'.", i, dataset_i$format), auth)
      stop()
    }
    
    # check #2.3: 'data_type' field
    if(!(dataset_i$data_type %in% c("griglia", "punto"))){
      log("Error", sprintf("Dataset %d, invalid data_type: '%s'. Allowed values are: 'griglia', 'punto'.", i, dataset_i$data_type), auth)
      stop()
    }
    
    i <- i + 1
  }
  
  # check #3: 'aggregation_level' field
  if(!(body$aggregation_level %in% c("municipale", "provinciale", "regionale"))){
    log("Error", sprintf("Invalid aggregation_level: '%s'. Allowed values are: 'municipale', 'provinciale', 'regionale'.", body$aggregation_level), auth)
    stop()
  }
}

validate_token <- function(auth){
  
  invoke_API(
    url = sprintf("https://%s:%s/externalService/getValidationBearerToken", Sys.getenv("AMELIA_HOST"), Sys.getenv("AMELIA_PORT")),
    body = list(
      BearerToken = auth$bearer_token,
      username = auth$username
    ),
    auth = auth
  )
}

download_dataset <- function(info, auth){
  
  # setup
  more_pages <- TRUE
  page_number <- 1
  all_data <- list()
  
  while(more_pages){
    
    # POST
    page_data <- invoke_API(
      url = sprintf("https://%s:%s/externalService/getTable", Sys.getenv("AMELIA_HOST"), Sys.getenv("AMELIA_PORT")),
      body = list(
        BearerToken = auth$bearer_token,
        username = auth$username,
        tableName = info$table_name,
        columns = list(
          info$x_column,
          info$y_column,
          info$temporal_column,
          unlist(info$harmonize_column)
        ),
        pageNumber = page_number,
        pageSize = 500
      ),
      auth = auth
    )
    
    # concat
    if (!is.null(page_data$data) && length(page_data$data) > 0) {
      all_data <- c(all_data, page_data$data)
    }
    
    # update page number
    if (!is.null(page_data$pagination)) {
      total_elements <- page_data$pagination$totalElements
      current_page <- page_data$pagination$pageNumber
      page_size <- page_data$pagination$pageSize
      
      if ((current_page * page_size) >= total_elements) {
        more_pages <- FALSE
      } else {
        page_number <- page_number + 1
      }
    } else {
      more_pages <- FALSE
    }
  }
  
  # from list to data.frame
  if (length(all_data) > 0) {
    df<- as.data.frame(do.call(rbind, lapply(all_data, as.data.frame)))
  } else {
    df <- data.frame()
  }
  
  log("Info", sprintf("dataset '%s' downloaded", info$table_name), auth)
  
  return(df)
}

process_geomatching_settings <- function(info, aggregation_level){
  
  # setup
  format_list <- list()
  type_list <- list()
  crs_list <- list()
  i <- 1
  
  # run
  for (item_i in info){
    
    # format
    if (item_i$format == "matrice 3D"){
      format_list[[i]] <- "matrix"
    } else {
      format_list[[i]] <- "xyt"
    }
    
    # type
    if (item_i$type == "griglia") {
      type_list[[i]] <- "grid"
    } else {
      type_list[[i]] <- "points"
    }
    
    # crs
    crs_list[[i]] <- item_i$crs
    i <- i + 1
  }
  
  # aggregation level
  if (aggregation_level == "municipale") {
    aggregation_level <- "mun"
  } else if (aggregation_level == "provinciale"){
    aggregation_level <- "prov"
  } else {
    aggregation_level <- "reg"
  }
  
  return(
    list(
      "format" = format_list,
      "type" = type_list,
      "crs" = crs_list,
      "aggregation_level" = aggregation_level
    )
  )
}

create_table <- function(results, auth){
  
  invoke_API(
    url = sprintf("https://%s:%s/externalService/createTable", Sys.getenv("AMELIA_HOST"), Sys.getenv("AMELIA_PORT")),
    body = list(
      BearerToken = auth$bearer_token,
      username = auth$username,
      tableName = auth$myp_table_name,
      columns = as.list(sapply(sapply(results, class), map_R_to_SQL_type))
    )
  )
}

insert_into_table <- function(df, auth) {
  
  # setup
  n <- nrow(df)
  batch_size <- 500
  
  if (nrow(df) == 0) {
    log("Error", "no data to insert into the table")
    stop()
  }
  
  cum_rows <- 0
  for (start in seq(1, n, by = batch_size)) {
    
    # from data.frame to list
    end <- min(start + batch_size - 1, n)
    batch <- df[start:end, , drop = FALSE]
    data_list <- lapply(seq_len(nrow(batch)), function(i) as.list(batch[i, ]))
    
    # POST
    invoke_API(
      url = sprintf("https://%s:%s/externalService/insertIntoTable", Sys.getenv("AMELIA_HOST"), Sys.getenv("AMELIA_PORT")),
      # url = "https://ameliadpcoll.grins.it:59182/externalService/insertIntoTable",
      body = list(
        BearerToken = auth$bearer_token,
        username = auth$username,
        tableName = auth$myp_table_name,
        data = data_list
      ),
      auth = auth
    )
    
    # log
    cum_rows <- cum_rows + nrow(batch)
    log("Info", sprintf("%d of %d rows (%.2f%%) loaded", cum_rows, n, (cum_rows / n) * 100), auth) 
  }
}

#* @post /invoke-geomatching
#* @serializer json
function(req, res){

  future::plan(multisession)
  
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  auth <- list()
  auth$bearer_token = body$BearerToken
  auth$username = body$username
  auth$myp_table_name <- paste0("results_geomatching_", gsub(" ","-",as.character(format(Sys.time(),"%Y_%m_%d_%H_%M_%S"))))
  
  future(
    {
      log("Start", "geomatching service started", auth)
      
      # validate body
      validate_body(body, auth)
      log("Info", "body validated", auth)
      
      # validate token
      validate_token(auth)
      log("Info", "token validated", auth)
      
      # download dataset/s
      data <- list()
      geomatching_settings <- list()
      i <- 1
      for (item_i in body$datasets) {
        
        # run
        temp <- download_dataset(
          item_i,
          auth
        )
        
        # reorder columns
        if(!(item_i$format == "matrice 3D")){
          data[[i]] <- temp %>% dplyr::select(
            dplyr::all_of(
              c(item_i$x_column, item_i$y_column, item_i$temporal_column)
            ),
            dplyr::everything()
          )
        }
        
        # save settings
        geomatching_settings[[i]] <- list(
          format = item_i$format,
          type = item_i$data_type,
          crs = item_i$crs
        )
        i <- i + 1
      }
      
      # process geomatching settings
      geomatching_settings <- process_geomatching_settings(
        geomatching_settings,
        body$aggregation_level
      )
      
      # perform geomatching
      log("Info", "geomatching started", auth)
      tryCatch(
        {
          results <- geotools::geomatching(
            data = data,
            settings = geomatching_settings[1:3],
            aggregate = TRUE,
            group_by = geomatching_settings$aggregation_level
          )
        },
        error = function(e) {
          log("Error", sprintf("geomatching failed: %s", e$message), auth)
          results <- NULL
        }
      )
      if (!is.null(response)){
        log("Info", "geomatching ended", auth)
        
        # create "My Processing" table
        create_table(results, auth)
        log("Info", sprintf("table '%s' created", auth$myp_table_name), auth)
        
        # insert into "My Processing" table
        insert_into_table(results, auth)
        log("Info", sprintf("table '%s' loaded in 'My Processing'", auth$myp_table_name), auth)
      }
    }
  )
  
  # response
  res <- list(
    message = "geomatching running in background"
  )
  return(res)
}
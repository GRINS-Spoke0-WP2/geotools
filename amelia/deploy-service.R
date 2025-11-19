library(plumber)
library(geotools)
library(httr)
library(jsonlite)
library(future)
library(dplyr)

plan(multisession)

log <- function(status, message) {
  
  # console
  message(
    sprintf(
      "%s [%s] - [%s] %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      .GlobalEnv$username,
      status,
      message
    )
  )
  
  # API
  POST(
    url = "https://ameliadpcoll.grins.it:59182/externalService/insertStatusInformation",
    body = list(
      BearerToken = .GlobalEnv$bearer_token,
      username = .GlobalEnv$username,
      filename = .GlobalEnv$myp_table_name,
      statusCode = status,
      message = message
    ),
    encode = "json",
    content_type_json()
  )
}

invoke_API <- function(url, body){
  
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
        log("Error", sprintf("API call failed: %s", e$message))
        return(NULL)
      }
    )
    
    if (!is.null(response)) {
      if (httr::status_code(response) == 200) {
        return(content(response))
      } else {
        log("Warning", sprintf("attempt %d of %d, status code %d", attempt, max_retries, status_code(response)))
      }
    } else {
      log("Warning", sprintf("attempt %d of %d", attempt, max_retries))
    }
    
    attempt <- attempt + 1
    
    if (attempt > max_retries) {
      log("Error", "max retries reached, unable to complete the request.")
      stop()
    }
    
    Sys.sleep(delay)
  }
}

validate_body <- function(body){

  # check #1: missing fields
  required_fields <- c("BearerToken", "username", "datasets", "aggregation_level")
  missing_fields <- setdiff(required_fields, names(body))
  if(length(missing_fields) > 0){
    log("Error", sprintf("missing fields: %s", paste(missing_fields, collapse = ", ")))
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
      log("Error", sprintf("dataset %d, missing fields: %s", i, paste(missing_fields, collapse = ", ")))
      stop()
    }

    # check #2.2: 'format' field
    if(!(dataset_i$format %in% c("long format (xyt)", "matrice 3D"))){
      log("Error", sprintf("Dataset %d, invalid format: '%s'. Allowed values are: 'long format (xyt)', 'matrice 3D'.", i, dataset_i$format))
      stop()
    }

    # check #2.3: 'data_type' field
    if(!(dataset_i$data_type %in% c("griglia", "punto"))){
      log("Error", sprintf("Dataset %d, invalid data_type: '%s'. Allowed values are: 'griglia', 'punto'.", i, dataset_i$data_type))
      stop()
    }

    i <- i + 1
  }

  # check #3: 'aggregation_level' field
  if(!(body$aggregation_level %in% c("municipale", "provinciale", "regionale"))){
    log("Error", sprintf("Invalid aggregation_level: '%s'. Allowed values are: 'municipale', 'provinciale', 'regionale'.", body$aggregation_level))
    stop()
  }
}

validate_token <- function(){

  invoke_API(
    url = "https://ameliadpcoll.grins.it:59182/externalService/getValidationBearerToken",
    body = list(
      BearerToken = .GlobalEnv$bearer_token,
      username = .GlobalEnv$username
    )
  )
}

download_dataset <- function(info){
  
  # setup
  more_pages <- TRUE
  page_number <- 1
  all_data <- list()
  
  while(more_pages){
    
    # POST
    page_data <- invoke_API(
      url = "https://ameliadpcoll.grins.it:59182/externalService/getTable",
      body = list(
        BearerToken = .GlobalEnv$bearer_token,
        username = .GlobalEnv$username,
        tableName = info$table_name,
        columns = list(
          info$x_column,
          info$y_column,
          info$temporal_column,
          unlist(info$harmonize_column)
        ),
        pageNumber = page_number,
        pageSize = 500
      )
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
  
  log("Info", sprintf("dataset '%s' downloaded", info$table_name))
  
  return(df)
}

insert_into_table <- function(df) {
  
  # setup
  n <- nrow(df)
  batch_size <- 500
  
  if (nrow(df) == 0) {
    log("Error", "no data to insert into the table")
    stop()
  }
  
  for (start in seq(1, n, by = batch_size)) {

    # from data.frame to list
    end <- min(start + batch_size - 1, n)
    batch <- df[start:end, , drop = FALSE]
    data_list <- lapply(seq_len(nrow(batch)), function(i) as.list(batch[i, ]))

    # POST
    invoke_API(
      url = "https://ameliadpcoll.grins.it:59182/externalService/insertIntoTable",
      body = list(
        BearerToken = .GlobalEnv$bearer_token,
        username = .GlobalEnv$username,
        tableName = .GlobalEnv$myp_table_name,
        data = data_list
      )
    )
  }
}

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

#* @post /invoke-geomatching
#* @serializer json
function(req, res){
  
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  .GlobalEnv$myp_table_name <- paste0("results_geomatching_", gsub(" ","-",as.character(format(Sys.time(),"%Y_%m_%d_%H_%M_%S"))))
  .GlobalEnv$bearer_token = body$BearerToken
  .GlobalEnv$username = body$username
  
  log("Start", "geomatching service started")

  # validate body
  validate_body(body)
  log("Info", "body validated")

  # validate token
  validate_token()
  log("Info", "token validated")
  
  # response
  res$status <- 200
  res$body <- list(
    message = "body and token validated, geomatching running in background"
  )
  
  future(
    {
      # download dataset/s
      data <- list()
      geomatching_settings <- list()
      i <- 1
      for (item_i in body$datasets) {
        
        # run
        temp <- download_dataset(
          item_i
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
      
      # create geomatching settings
      convert_name <- function(list_convert){
        list_convert_mod <- list_convert
        list_convert_mod[["format"]] <- as.list(gsub("long format \\(xyt\\)","xyt",list_convert_mod[["format"]]))
        list_convert_mod[["format"]] <- as.list(gsub("matrice 3D","matrix",list_convert_mod[["format"]]))
        list_convert_mod[["type"]] <- as.list(gsub("griglia","grid",list_convert_mod[["type"]]))
        list_convert_mod[["type"]] <- as.list(gsub("punto","points",list_convert_mod[["type"]]))
        list_convert_mod[["crs"]] <- list_convert_mod[["crs"]]
        return(list_convert_mod)}
      geomatching_settings <- lapply(geomatching_settings, convert_name)
      
      # cast aggregation level
      aggregation_level <- gsub("municipale", "mun", body$aggregation_level)
      aggregation_level <- gsub("provinciale", "prov", body$aggregation_level)
      aggregation_level <- gsub("regionale", "reg", body$aggregation_level)
      
      # perform geomatching
      log("Info", "geomatching started")
      results <- geomatching(
        data = data,
        settings = geomatching_settings,
        aggregate = TRUE,
        group_by = aggregation_level
      )
      log("Info", "geomatching ended")
      
      # create "My Processing" table
      invoke_API(
        url = "https://ameliadpcoll.grins.it:59182/externalService/createTable",
        body = list(
          BearerToken = .GlobalEnv$bearer_token,
          username = .GlobalEnv$username,
          tableName = .GlobalEnv$myp_table_name,
          columns = as.list(sapply(sapply(results, class), map_R_to_SQL_type))
        )
      )
      log("Info", sprintf("table '%s' created", .GlobalEnv$myp_table_name))
      
      # insert into "My Processing" table
      insert_into_table(
        results
      )
      log("Info", sprintf("table '%s' loaded in 'My Processing'", .GlobalEnv$myp_table_name))
    }
  )
}

# Da creare un ambiente dedicato per le variabili globali
# Da impostare host e porta tramite variabili globali
# Da capire se aggiungere o meno dei log quando vengono caricati i dati a batch
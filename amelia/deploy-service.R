library(plumber)
library(jsonlite)
library(geotools)
library(httr)
library(jsonlite)

log <- function(username, message) {
  
  print(
    sprintf(
      "%s [%s] - %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      username,
      message
    )
  )
}

base_validation <- function(body){

  # check #1: missing fields
  required_fields <- c("BearerToken", "username", "datasets", "aggregation_level")
  missing_fields <- setdiff(required_fields, names(body))
  if(length(missing_fields) > 0){
    return(
      list(
        error = TRUE,
        type = "MissingFields",
        message = sprintf("Missing fields: %s",
                        paste(missing_fields, collapse = ", ")),
        details = NULL
      )
    )
  }

  # check #2: 'datasets' field
  i <- 0
  for(dataset_i in body$datasets){

    # check #2.1: missing fields
    required_fields <- c("table_name", "x_column", "y_column", "temporal_column",
                         "harmonize_columns", "crs", "format", "data_type")
    missing_fields <- setdiff(required_fields, names(dataset_i))
    if(length(missing_fields) > 0){
      return(
        list(
          error = TRUE,
          type = "Datasetvalidation",
          message = sprintf("Dataset %d, missing fields: %s",
                          i, paste(missing_fields, collapse = ", ")),
          details = dataset_i
        )
      )
    }

    # check #2.2: 'format' field
    if(!(dataset_i$format %in% c("long format (xyt)", "matrice 3D"))){
      return(
        list(
          error = TRUE,
          type = "InvalidFormat",
          message = sprintf(
            "Dataset %d, invalid format: '%s'. Allowed values are: 'long format (xyt)', 'matrice 3D'.",
            i, dataset_i$format
          ),
          details = dataset_i
        )
      )
    }

    # check #2.3: 'data_type' field
    if(!(dataset_i$data_type %in% c("griglia", "punto"))){
      return(
        list(
          error = TRUE,
          type = "InvalidDataType",
          message = sprintf(
            "Dataset %d, invalid data_type: '%s'. Allowed values are: 'griglia', 'punto'.",
            i, dataset_i$data_type
          ),
          details = dataset_i
        )
      )
    }

    i <- i + 1
  }

  # check #3: 'aggregation_level' field
  if(!(body$aggregation_level %in% c("municipale", "provinciale", "regionale"))){
    return(
      list(
        error = TRUE,
        type = "InvalidAggregationLevel",
        message = sprintf(
          "Invalid aggregation_level: '%s'. Allowed values are: 'municipale', 'provinciale', 'regionale'.",
          body$aggregation_level
        ),
        details = NULL
      )
    )
  }

  return(list())
}

validate_token <- function(body){

  response <- POST(
    url = "https://ameliadpcoll.grins.it:59182/externalService/getValidationBearerToken", # da parametrizzare
    body = list(
      BearerToken = body$BearerToken,
      username = body$username
    ),
    encode = "json",
    content_type_json()
  )

  return(response)
}

download_dataset <- function(bearer_token, username, info){
  
  # setup
  more_pages <- TRUE
  page_number <- 1
  all_data <- list()
  
  while(more_pages){
    
    # POST
    response <- POST(
      url = "https://ameliadpcoll.grins.it:59182/externalService/getTable",
      body = list(
        BearerToken = bearer_token,
        username = username,
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
      encode = "json",
      content_type_json()
    )
    page_data <- content(response)
    
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
  
  # log
  log(username, sprintf("dataset '%s' downloaded", info$table_name))
  
  return(df)
}

insert_into_table <- function(bearer_token, username, table_name, df) {

  batch_size <- 500
  for (start in seq(1, n, by = batch_size)) {

    # from data.frame to list
    end <- min(start + batch_size - 1, n)
    batch <- df[start:end, , drop = FALSE]
    data_list <- lapply(seq_len(nrow(batch)), function(i) as.list(batch[i, ]))

    # POST
    resp <- httr::POST(
      url = "https://ameliadpcoll.grins.it:59182/externalService/insertIntoTable",
      body = list(
        BearerToken = bearer_token,
        username = username,
        tableName = table_name,
        data = data_list
      ),
      encode = "json",
      content_type_json()
    )
  }
}

#* @post /invoke-geomatching
#* @serializer json
function(req, res){

  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  
  # log
  log(req$username, "geomatching service started")

  # base validation
  base_val_result <- base_validation(body)
  if(length(base_val_result) > 0){
    res$status <- 400
    return(base_val_result)
  }
  
  # log
  log(req$username, "body validated")

  # token validation
  token_val_response <- validate_token(body)
  if (!(token_val_response$status_code %in% c(200, 403))) {
    res$status <- 400
    return(
      list(
        error = TRUE,
        type = "InvalidToken",
        message = "Bearer token is invalid or unauthorized.",
        details = content(token_val_response, "text")
      )
    )
  }
  
  # log
  log(req$username, "token validated")

  # download dataset/s
  data <- list()
  geomatching_settings <- list()
  i <- 1
  for (item_i in body$datasets) {
    data[[i]] <- download_dataset(
      body$BearerToken,
      body$username,
      item_i
    )
    geomatching_settings[[i]] <- list(
      format = item_i$format,
      type = item_i$data_type,
      crs = item_i$crs
    )
    i <- i + 1
  }

  # create geomatching settings
  convert_name <- function(list_convert){
    list_convert_mod <- list()
    list_convert_mod <- list_convert
    list_convert_mod[["format"]] <- as.list(gsub("long format \\(xyt\\)","xyt",list_convert_mod[["format"]]))
    list_convert_mod[["format"]] <- as.list(gsub("matrice 3D","matrix",list_convert_mod[["format"]]))
    list_convert_mod[["type"]] <- as.list(gsub("griglia","grid",list_convert_mod[["type"]]))
    list_convert_mod[["type"]] <- as.list(gsub("punto","points",list_convert_mod[["type"]]))
    list_convert_mod[["crs"]] <- list_convert_mod[["crs"]]
    return(list_convert_mod)}
  geomatching_settings <- lapply(geomatching_settings, convert_name)

  # cast aggregation level
  aggregation_level <- gsub("municipale", "mun", body$aggragation_level)
  aggregation_level <- gsub("provinciale", "prov", body$aggragation_level)
  aggregation_level <- gsub("regionale", "reg", body$aggragation_level)
  
  # log
  log(req$username, "geomatching started")

  # perform geomatching
  results <- geomatching(
    data = data,
    settings = geomatching_settings,
    aggregate = TRUE,
    group_by = aggregation_level
  )
  
  # log
  log(req$username, "geomatching ended")

  # create "My Processing" table
  table_name <- paste0("results_geomatching_", gsub(" ","-",as.character(format(Sys.time(),"%Y_%m_%d_%H_%M_%S"))))
  POST(
    url = "https://ameliadpcoll.grins.it:59182/externalService/createTable",
    body = list(
      BearerToken = body$BearerToken,
      username = body$username,
      tableName = table_name,
      columns = sapply(results, class)
    ),
    encode = "json",
    content_type_json()
  )
  
  # log
  log(req$username, sprintf("table '%s' created", table_name))

  # insert into "My Processing" table
  insert_into_table(
    body$BearerToken,
    body$username,
    table_name,
    results
  )
  
  # log
  log(req$username, sprintf("table '%s' loaded in 'My Processing'", table_name))
  
}
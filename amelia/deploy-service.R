library(plumber)
library(jsonlite)
library(geotools)

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

#* @post /invoke-geomatching
#* @serializer json
function(req, res){
  
  # get request body
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  
  # base validation
  val_result <- base_validation(body)
  if(length(val_result) > 0){
    res$status <- 400
    return(val_result)
  }
  
  # success response
  return(
    list(
      status = "OK",
      message = "JSON received successfully. All validations passed.",
      username = body$username,
      aggregation_level = body$aggregation_level
    )
  )
}
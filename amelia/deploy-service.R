library(plumber)
library(jsonlite)
library(geotools)
library(httr) #da aggiungere all immagine
library(jsonlite) #da aggiungere all immagine
library(DBI)
library(RPostgres)  # se stai usando Postgres

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
  # # 0. Acquisizione Token ####
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

  # 1. Validazione token ####
  # Definisci l’URL (verificare)
  url <- "https://ameliadpcoll.grins.it/externalService/getValidationBearerToken" #da mettere la porta?

  # Prepara i dati da inviare
  validation_data <- list(
    BearerToken = body$BearerToken,
    username = body$username
  )

  validation_data <- list(
    BearerToken = "abc",
    username = "alessandro.fustamoro"
  )


  # Invia la richiesta POST
  response <- POST(
    url,
    body = validation_data,
    encode = "json",
    content_type_json()  # imposta Content-Type: application/json
  )

  # Controlla la risposta
  # status_code(response)           # dovrebbe restituire 200 o 403
  # content(response, "parsed")     # mostra il corpo JSON della risposta
  # if(cstatus_code(response)==403){stop("Token not valid")}

  # # scrivere "Dataset presente in AMELIA"
  # # serve HOST, porta, nome del DB, schema (solo se POSTGRES), nome tabella, colonne tabella
  # # noi facciamo l'insert attraverso, serve anche un ID per trovare il corrispondente status (tra i molti?)
  # # QUINDI:
  # # Parametri forniti da loro
  # host <- "<host>"
  # port <- 5432          # di default PostgreSQL
  # dbname <- "<nome_db>"
  # user <- "<username>"
  # password <- "<password>"
  #
  # # Connessione
  # con <- dbConnect(
  #   RPostgres::Postgres(),
  #   host = host,
  #   port = port,
  #   dbname = dbname,
  #   user = user,
  #   password = password
  # )
  # # Inserire singole righe
  # dbExecute(con, "
  # INSERT INTO nome_tabella (col1, col2, col3)
  # VALUES ('valore1', 123, 'valore3')")

  # 2. Download dataset da AMELIA ####
  # Parametri base
  url <- "https://ameliadpcoll.grins.it/externalService/getValidationBearerToken" #porta?
  jwt_token <- body$BearerToken
  username <- body$username
  df <- list()
  settings_geo <- list()
  page_size <- 500
  for (i in 1:length(body$datasets)){ #"x_column", "y_column", "temporal_column", "harmonize_columns"
    table_name <- body$datasets[i]$table_name
    columns <- c(body$datasets[i]$x_column,
                 body$datasets[i]$y_column,
                 body$datasets[i]$temporal_column,
                 unlist(body$datasets[i]$harmonize_column))

    # Lista dove accumulare i dati
    all_data <- list()
    page_number <- 1
    more_pages <- TRUE

    while(more_pages) {

      # Corpo della richiesta
      body_data <- list(
        BearerToken = jwt_token,
        username = username,
        tableName = table_name,
        columns = columns,
        pageNumber = page_number,
        pageSize = page_size
      )

      # Chiamata POST
      response <- POST(
        url,
        body = body_data,
        encode = "json",
        content_type_json()
      )

      # Controlla lo status
      if(status_code(response) != 200) {
        stop(paste("Errore API:", status_code(response)))
      }

      # Estrai i dati
      page_data <- content(response, "parsed")

      # se nel caso sfortunatissimo l ultima pagina fosse da 500 righe
      rows <- page_data$rows
      if(is.null(rows)) rows <- list()

      # Accumula i dati in una lista
      all_data <- c(all_data, page_data$rows)  # supponendo che i dati siano in page_data$rows

      # Controlla se ci sono altre pagine
      if(length(page_data$rows) < page_size) {
        more_pages <- FALSE
      } else {
        page_number <- page_number + 1
      }
    }
    # Trasforma in data.frame (opzionale)
    df[[i]] <- do.call(rbind.data.frame, all_data)
    settings_geo[[i]] <- list(
      format = body$datasets[i]$format,
      type = body$datasets[i]$data_type,
      crs = body$datasets[i]$crs
    )
  }
  # 3. Geomatching ####
  convert_name <- function(list_convert){
    list_convert_mod <- list()
    list_convert_mod <- list_convert
    list_convert_mod[["format"]] <- as.list(gsub("long format \\(xyt\\)","xyt",list_convert_mod[["format"]]))
    list_convert_mod[["format"]] <- as.list(gsub("matrice 3D","matrix",list_convert_mod[["format"]]))
    list_convert_mod[["type"]] <- as.list(gsub("griglia","grid",list_convert_mod[["type"]]))
    list_convert_mod[["type"]] <- as.list(gsub("punto","points",list_convert_mod[["type"]]))
    list_convert_mod[["crs"]] <- list_convert_mod[["crs"]]
    return(list_convert_mod)}
  settings_geo <- lapply(settings_geo, convert_name)



  geomatching()
  # "long format (xyt)", "matrice 3D"
  # "griglia", "punto"
  # c("municipale", "provinciale", "regionale")

  # # chiudere la connessione
  # dbDisconnect(con)

}



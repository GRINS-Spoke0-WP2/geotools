# # getwd()
# # source("R/geomatching.R")
#
# # APIs ####
# library(plumber)
#
# # archivio delle richieste ricevute
# requests <- list()
#
# #* @post /submit
# #* @param data Dati inviati dal client
# function(data = "") {
#   time <- Sys.time()
#   requests <<- append(requests, list(list(timestamp = time, data = data)))
#   list(status = "OK", received = data, at = as.character(time))
# }
#
# function() {
#   list(
#     count = length(requests),
#     latest = tail(requests, 1)
#   )
# }
#
# function() {
#   requests <<- list()
#   list(status = "cleared")
# }

library(plumber)
library(jsonlite)

# archivio delle richieste
requests <- list()

#* @apiTitle Web service coerente con JSON specifico
#* @apiDescription Riceve e gestisce richieste contenenti BearerToken, username e datasets complessi

#* @post /submit
#* @serializer json
function(req, res) {
  # Leggi il corpo JSON della richiesta
  body <- fromJSON(req$postBody)

  # Controlla che i campi principali esistano
  required_fields <- c("BearerToken", "username", "datasets", "aggregation_level")
  missing <- setdiff(required_fields, names(body))

  if (length(missing) > 0) {
    res$status <- 400
    return(list(error = paste("Campi mancanti:", paste(missing, collapse = ", "))))
  }

  # Verifica che datasets sia una lista
  if (!is.list(body$datasets)) {
    res$status <- 400
    return(list(error = "Il campo 'datasets' deve essere una lista di oggetti."))
  }

  # Salva la richiesta in memoria
  time <- Sys.time()
  requests <<- append(requests, list(list(timestamp = time, payload = body)))

  # Risposta di conferma
  list(
    status = "OK",
    username = body$username,
    datasets_count = length(body$datasets),
    received_at = as.character(time)
  )
}

#* @get /check
#* @serializer json
function() {
  list(
    total_requests = length(requests),
    latest = if (length(requests) > 0) requests[[length(requests)]] else NULL
  )
}

#* @get /clear
#* @serializer json
function() {
  requests <<- list()
  list(status = "cleared")
}

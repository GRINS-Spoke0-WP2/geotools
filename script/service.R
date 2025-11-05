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

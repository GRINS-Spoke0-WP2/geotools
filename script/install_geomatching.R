# detach("package:geotools", unload = TRUE)
# remove.packages("geotools")
# .rs.restartR()  # se usi RStudio
#
#
# # install.packages("devtools")
# devtools::install_github("GRINS-Spoke0-WP2/geotools")
# library(geotools)
# geomatching
#
# # scarica il pacchetto in locale senza installarlo
# devtools::download_github("GRINS-Spoke0-WP2/geotools", destdir = tempdir())
#
# # poi cerca riferimenti a se stesso
# grep("library(geotools)", recursive = TRUE, path = "path/to/geotools")
# grep("require(geotools)", recursive = TRUE, path = "path/to/geotools")
#

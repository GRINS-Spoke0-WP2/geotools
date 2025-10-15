

# creare una griglia 0.01 x 0.01 "vuota" + il tempo (tempo dagli altri dataset forniti)
# utilizzare questa griglia come "prima" nel geomatching (aka che comanda)
#


# esempio
# nelle prime righe di geomatching una riga che prende il tempo dei dataset
# forniti, e crea la griglia e la aggiunge ai dataset forniti come prima


# conviene generare a parte la griglia 0.01 x 0.01 con già i codici comunali
# associati per ogni punto. Questo da fare prima di lanciare il geo matching.
# dopo di che il geomatching importa questo dataset come un file esterno e
# semplicemente lo moltiplica per i giorni


# mio codice esempio:
load("data/IT_adm_bounds_2025.RData")
library(sf)
library(ggplot2)
library(sp)
library(spacetime)
library(foreach)
library(doParallel)
# registerDoParallel(cores = 10)
mun_bounds <- st_transform(mun_bounds, st_crs(4326))
LAUs_sp <- as_Spatial(mun_bounds)
mun_bounds <- st_drop_geometry(mun_bounds)
rm(list = setdiff(ls(), c("LAUs_sp", "mun_bounds")))
gc()
hr_lat <- seq(35, 48, .01)
hr_lon <- seq(6, 19, .01)
hr_grid <- expand.grid(hr_lon, hr_lat)
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
coordinates(hr_grid) <- c("Var1", "Var2")
slot(hr_grid, "proj4string") <- crs_wgs84
hr_df <- over(hr_grid, LAUs_sp)
hr_grid <- hr_grid[!is.na(hr_df$PRO_COM)]
grid_LAUs <- over(hr_grid, LAUs_sp)
grid_LAUs <- as.data.frame(grid_LAUs[, c(2,3,6)])
# names(grid_LAUs) <- "PRO_COM"
# 65011 96034 miss
mun_bounds[mun_bounds$PRO_COM == "65011", ]
mun_bounds$PRO_COM[mun_bounds$COMUNE == "Amalfi"] # for 65011 -> 65006
mun_bounds[mun_bounds$PRO_COM == "96034", ]
mun_bounds$PRO_COM[mun_bounds$COMUNE == "Sagliano Micca"] # for 96034 -> 96056

hr_grid_LAUs <- SpatialPointsDataFrame(coords=hr_grid,data = grid_LAUs)
saveRDS(hr_grid_LAUs,file="data/hr_grid_LAUs.rds")

hr_grid_LAUs_df <- cbind(hr_grid_LAUs@coords,hr_grid_LAUs@data$PRO_COM)
names(hr_grid_LAUs_df)<-c("Longitude","Latitude","PRO_COM")
saveRDS(hr_grid_LAUs_df,file="data/hr_grid_LAUs_df.rds")

# facciamo girare geomatching ... output: griglia fine con COD_reg COD_prov e PRO_COM (comuni)
 # + le variabili degli altri dataset

# saltiamo idw2hr e andiamo a hr2poly

# ricordarci di quei due comuni che sono rimasti fuori! ovvero il 65011 e il 96034
# aggiungere manualmente e inserire i valori dei comuni vicini, quindi per il 65011 usimao il 65006
# e per il 96034 usiamo il 96056

# alla fine calcoliamo

# WARNING ! INSERIRE NELLA DOCUMENTAZIONE CHE NON SI DEVONO DATASET PIU FINI DEL 0.01 X 0.01 !





















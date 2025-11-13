load("~/Library/Mobile Documents/com~apple~CloudDocs/Lavoro/PhD Bergamo/R/GitHub/GRINS-Spoke0-WP2/AQ-FRK/v.3.0.0/A_input/data/input/WE_C3S_v100_ST_ERA5SL.rda")
WE_C3S_v100_ST_ERA5SL <- WE_C3S_v100_ST_ERA5SL[24:26,4015:4017]
gc()
df <- WE_C3S_v100_ST_ERA5SL@data
library(stargazer)
df <- df[,c(1:3,9)]
names(df)[4]<-"temperatura"
head(df)
df$time <- as.Date(df$time)
stargazer(head(df),summary = F)


# guide to merge geometries

library(readr)
library(dplyr)
library(sf)

#importing LAUs geometries
download.file(
  "https://www.istat.it/storage/cartografia/confini_amministrativi/generalizzati/2025/Limiti01012025_g.zip",
  destfile = "IT_adm_lim.zip")
unzip("IT_adm_lim.zip")
metadata_GRINS_LAUs <- st_read("Com01012025_g","Com01012025_g_WGS84")

#importing climate variables at municipal levels
GRINS_df_LAUs_Italy <- read.csv("yourdataframe.csv") #<- change to your dataframe

#merging with geometries
GRINS_df_LAUs_Italy_geom <- left_join(metadata_GRINS_LAUs,subset_CL,by="PRO_COM") #or COD_REG or COD_PROV





library(httr)
setwd("geo_tools/geo_matching")
path<-"https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/"
years <- 2002:2024
for (y in years) {
  url <- paste0(
    "Limiti0101",
    y,".zip"
  )
  if(y %in% c(2001,2021)){
    url <- paste0(
      path,"Limiti",
      y,".zip"
    )
  }
  if(y == 2011){
    url <- paste0(
      path,y,"/Limiti_",
      y,"_WGS84.zip"
    )
  }
  if(y > 2021){
    url <- paste0(
      path,y,"/Limiti0101",y,".zip"
    )
  }
  name_file <- paste0("dati/confini/zip/Limiti0101",y,".zip")
  download.file(url = url,
                destfile = name_file)
  unzip(zipfile = paste0("dati/confini/zip/Limiti0101",y,".zip"),
        exdir = "dati/confini/extract_zip/"
        )
}


# https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/2022/Limiti01012022.zip
# https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/Limiti2021.zip
# https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/Limiti01012019.zip
# https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/Limiti01012014.zip
# https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/Limiti01012003.zip
# https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/Limiti01012007.zip
# https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/Limiti01012002.zip
# https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/2011/Limiti_2011_WGS84.zip
# https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/Limiti01012017.zip

# Aprire i comuni e fare un STFDF
lf <- list.files(path="dati/confini/extract_zip")
for (y in 2001:2024) {
  lf1 <- grepl(y,lf)
  
  # list.files(paste0("dati/confini/extract_zip/"), # DA QUI 15/10/2024
}

# Dati meteorologici
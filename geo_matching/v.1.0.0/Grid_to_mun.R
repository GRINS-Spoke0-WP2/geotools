source("AQ-Modelling/v.1.0.0/script/packages.R")
#data
load("AQ-Modelling/v.1.0.0/data/ALL_pred_BAUs_df.rda")

#finer grid
x0 = seq(min(ALL_pred_BAUs_df$coords.x1),
         max(ALL_pred_BAUs_df$coords.x1),
         0.01)
y0 = seq(min(ALL_pred_BAUs_df$coords.x2),
         max(ALL_pred_BAUs_df$coords.x2),
         0.01)
df_downsc <- data.frame(x=rep(x0,each=length(y0)),
                        y=rep(y0,length(x0)))

coordinates(df_downsc)<-c("x","y")

for (i in unique(ALL_pred_BAUs_df$t)) {
  sub <- ALL_pred_BAUs_df[ALL_pred_BAUs_df$t==i,]
  coordinates(sub)<-c("coords.x1","coords.x2")
  gridded(sub)<-TRUE
  over_df <- over(df_downsc,sub)
  save(over_df,file=paste0("geo_tools/geo_matching/v.1.0.0/dati/all_df_",unique(over_df$t),".rda"))
  }

com24 <-
  st_read(dsn = "geo_tools/geo_matching/v.1.0.0/dati/confini/extract_zip/Limiti01012020/Com01012020",
          layer = "Com01012020_WGS84")
com24 <- st_transform(com24, 4326)
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
slot(df_downsc, "proj4string") <- crs_wgs84

com_points_df <- over(df_downsc,as_Spatial(com24))

for (i in unique(ALL_pred_BAUs_df$t)) {
  load(paste0("geo_tools/geo_matching/v.1.0.0/dati/all_df_",as.Date(i),".rda"))
  all_df <- cbind(over_df,com_points_df)
  mun_data <- all_df %>%
    group_by(PRO_COM,t) %>%
    summarise(lai_hv = mean(lai_hv,na.rm=T),
              lai_lv = mean(lai_lv,na.rm=T),
              rh = mean(rh,na.rm=T),
              ssr = mean(ssr,na.rm=T),
              t2m = mean(t2m,na.rm=T),
              tp = mean(tp,na.rm=T),
              windspeed = mean(windspeed,na.rm=T),
              blh = mean(blh,na.rm=T),
              EM_NO2 = mean(EM_NO2,na.rm=T),
              NO2_mu_FRK = mean(mu,na.rm=T),
              NO2_var_FRK = mean(var,na.rm=T))
  if (i == unique(ALL_pred_BAUs_df$t)[1]){
    all_mun_data <- mun_data
  } else{
    all_mun_data <- rbind(all_mun_data,mun_data)
  }
}



#
Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df <- merge(all_mun_data,com24,all.x=T)
names(Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df)[2]<-"time"
names(Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df)[3:10]<-paste0("WE_",names(Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df)[3:10])
names(Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df)[12:13]<-paste0("AQ_",names(Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df)[12:13])
Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df <- Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df[,c(1,2,12,13,3:10,11,14:26)]
Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df <- Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df[order(Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df$time,
                                                                                   Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df$PRO_COM),]
# save(Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df,file = "geo_tools/geo_matching/v.1.0.0/dati/Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df.rda")

Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df<- Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df[,-4]

MUN.AQ.WE.EM_df_pilot_dataset_D021 <- Pilot_Dataset_D021_UNIBG_AQ_WE_EM_df
MUN.AQ.WE.EM_df_pilot_dataset_D021 <- st_drop_geometry(MUN.AQ.WE.EM_df_pilot_dataset_D021)
save(MUN.AQ.WE.EM_df_pilot_dataset_D021,file = "geo_tools/geo_matching/v.1.0.0/dati/MUN.AQ.WE.EM_df_pilot_dataset_D021.rda")

a <- MUN.AQ.WE.EM_df_pilot_dataset_D021[,c(1:12)]
MUN.AQ.WE.EM_df_pilot_dataset_D021 <- MUN.AQ.WE.EM_df_pilot_dataset_D021[,c(19,1:12)]
names(MUN.AQ.WE.EM_df_pilot_dataset_D021)[1]<-"Municipality"
save(MUN.AQ.WE.EM_df_pilot_dataset_D021,file = "geo_tools/geo_matching/v.1.0.0/dati/MUN.AQ.WE.EM_df_pilot_dataset_D021.rda")




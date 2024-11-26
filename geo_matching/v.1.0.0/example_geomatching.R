# centri griglia
centri <- expand.grid(1:6,1:6)
# trasformarli in un oggetto spaziale
library(sf)
centri <- st_as_sf(centri,coords = c("Var1","Var2"))

#centri poligoni (comuni)
centr_c <- matrix(c(1.5,1.5,1.5,3.5,4.5,4.5),ncol=2,byrow=T)
# in oggetti spaziali
centr_c <- st_as_sf(as.data.frame(centr_c),coords=c("V1","V2"))
# creare poligoni
comA <- st_buffer(centr_c[1:2,],dist=1,endCapStyle = "SQUARE")
comB <- st_buffer(centr_c[3,],dist=2,endCapStyle = "SQUARE")

# come ricavara il centro dal poligono
centro_B <- st_centroid(comB)

library(ggplot2)
ggplot()+
  geom_sf(data=comB,fill="red",col="black")+
  geom_sf(data=comA,fill="blue",col="black")+
  geom_sf(data = centro_B,col="orange")+
  geom_sf(data=centri)

# assegnare ID comune (nel tuo caso è il nome)
# comA$id_comune <- 1:2
# comB$id_comune <- 3

# sequenza temporale
t <- seq.Date(from = as.Date("2021-01-01"),to=as.Date("2021-01-02"),by="days")
t

# estrarre coordinate e metterle in un dataframe
centri <- as.data.frame(st_coordinates(centri))
# aggiungere il tempo (ripetere le coords)
centri_df <- cbind(centri[rep(1:36,length(t)),],rep(t,each=36))
# simulare un processo (le tue emissioni)
centri_df$em <- runif(72)

library(spacetime)
library(sp)
#trasformare in un Spatial Points DataFrame il dataframe con i centri della griglia
coordinates(centri)<-c("X","Y")
#creare la griglia spazio-temporale
grid_st <- STFDF(sp=centri,
      time=t,
      data=centri_df)
comuni_sp <- as_Spatial(rbind(comA,comB)) #creare un oggetto spaziale (poligoni) per i comuni
# names(comuni_sp@data)<-paste0("sp_",names(comuni_sp@data)) #cambiare i nomi per non avere duplicati nell oggetto ST tra il dataframe e l'oggetto spaziale
comuni_df <- data.frame(comuni=c("comune A1","comune A2","comune B")) #lista dei comuni
comuni_df <- as.data.frame(comuni_df[rep(1:3,2),]) #ripetere per il tempo
# comuni_df <- cbind(as.data.frame(comuni_df[rep(1:3,2),1]),as.data.frame(rep(t,each=3))) #aggiungere il tempo
# names(comuni_df)<-c("id_comune","time") #rinominare 
comuni_st <- STFDF(sp=comuni_sp,t=t,data=comuni_df) #creare oggetto spazio-tempo per i comuni

# over
quale_comune <- over(grid_st,comuni_st) #ad ogni punto è associato il comune in cui cade

#creo un dataframe dall'oggetto spazio-temporale
grid_df <- grid_st@data

#aggiungiamo questa informazione ai punti di partenza
grid_df$comune <- quale_comune

library(dplyr)
grid_df %>%
  group_by(comune) %>%
  summarise(avg_concentraz = mean(em,na.rm=T)) # medie comunali

# contare mancanti

# e voilà

df_cities_yearly <- data %>% 
  group_by(Location=data$Location) %>%
  summarise(
    MinTemp = mean(MinTemp,na.rm=T), 
    MaxTemp = mean(MinTemp,na.rm=T), 
    Rainfall = mean(Rainfall, na.rm=T),
    Humidity = mean(cbind(Humidity9am, Humidity3pm),na.rm=T),
    MeanTemp = mean(cbind(Temp9am, Temp3pm),na.rm=T),
    WindSpeed = mean(cbind(WindSpeed9am, WindSpeed3pm),na.rm=T),
    Pressure = mean(cbind(Pressure9am, Pressure3pm),na.rm=T),
    WindGustSpeed = mean(WindGustSpeed,na.rm=T))

df_cities_yearly <- cbind(df_cities_yearly, Latitude=coords$Latitude, Longitude=coords$Longitude)
rownames(df_cities_yearly) <- df_cities_yearly$Location
PCA(df_cities_yearly[-1])

ggplot(df_cities_yearly, aes(x=Longitude, y=Latitude, col=Rainfall), xlim = c(112, 170), ylim = c(-47, -8)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour=F)


df_cities <- data.frame()
for (i in cities) {
  tmpdf <- data %>% 
    filter(Location == i) %>%
    group_by(Day) %>%
    summarise(Rainfall = mean(Rainfall, na.rm=T),
              Humidity = mean(cbind(Humidity9am, Humidity3pm),na.rm=T),
              MeanTemp = mean(cbind(Temp9am, Temp3pm),na.rm=T)) %>%
    select(Rainfall,MeanTemp,Humidity)
  tmp <- c(tmpdf$Rainfall,tmpdf$MeanTemp,tmpdf$Humidity)
  tmp[is.na(tmp)] <- 0
  df_cities <- rbind(df_cities,t(tmp))
}
names(df_cities) <- c(
  paste("Temp",seq(1:366),sep=""),
  paste("Rainfall",seq(1:366),sep=""),
  paste("Humidity",seq(1:366),sep="")
)
rownames(df_cities) <- cities
PCA(df_cities)
mod.kmeans <- kmeans(df_cities,5,algorithm="Hartigan-Wong")
fviz_cluster(mod.kmeans, data = df_cities,
             geom = "text",
             ellipse.type = "convex",
             ggtheme = theme_bw())




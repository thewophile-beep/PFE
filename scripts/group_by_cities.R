Temp_by_cities <- data %>% 
  group_by(Location=data$Location) %>%
  summarise(MinTemp = min(MinTemp, na.rm=T), MaxTemp = max(MaxTemp, na.rm=T))

Temp_by_cities <- cbind(Temp_by_cities, Latitude=coords$Latitude, Longitude=coords$Longitude)

head(Temp_by_cities)

ggplot(Temp_by_cities, aes(x=Longitude, y=Latitude, col=MinTemp), xlim = c(112, 170), ylim = c(-47, -8)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour=F)

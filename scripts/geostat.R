world <- ne_countries(scale = "medium", returnclass = "sf")

states <- st_as_sf(ozmap("states", quiet=T))

{
  png(paste(plots_path, "Australia_map.png"),width=1000,height=800)
  print(ggplot(data = world) +
    geom_sf(fill="antiquewhite1") +
    
    stat_density_2d(data=Temp_by_cities, aes(x=Longitude, y=Latitude, fill = ..density..), geom = "raster", contour=F,alpha=0.8) +
    geom_sf(data=states,fill=NA) +
    
    geom_point(data=coords, aes(x=Longitude, y=Latitude), size=2, shape=23, fill="darkred")+
    geom_text_repel(data = coords, aes(x = Longitude, y = Latitude, label = Ville), fontface = "bold", size=5) +
    coord_sf(xlim = c(112, 170), ylim = c(-47, -8), expand = T) +
    
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "aliceblue"),
          text=element_text(size=20))
  )
  dev.off()
}

ggplot(data = world) +
  stat_density_2d(data=Temp_by_cities, aes(x=Longitude, y=Latitude, fill = after_stat(density)), geom = "raster", contour=F) +
  geom_sf(data=states,fill=NA,color="darkgray") +
  geom_point(data=coords, aes(x=Longitude, y=Latitude), size=2, shape=23, fill="red")+
  geom_text_repel(data = coords, aes(x = Longitude, y = Latitude, label = Ville), fontface = "bold", size=5, color="white") +
  coord_sf(xlim = c(min(coords$Longitude), max(coords$Longitude)), ylim = c(min(coords$Latitude), max(coords$Latitude)), expand = F)

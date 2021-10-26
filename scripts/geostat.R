world <- ne_countries(scale = "medium", returnclass = "sf")

coords <- read.table("data/coords.txt", sep=",", header=T)
coords <- st_as_sf(coords, coords = c("Latitude", "Longitude"), remove = FALSE, crs = 4326, agr = "constant")

states <- st_as_sf(ozmap("states", quiet=T))

{
  pdf(paste(plots_path, "Australia_map.pdf"))
  print(ggplot(data = world) +
    geom_sf(fill="antiquewhite1") +
    
    geom_sf(data=states, fill="antiquewhite1") +
    # stat_density_2d(data=Temp_by_cities, aes(x=Longitude, y=Latitude, fill = ..density..), geom = "raster", contour=F) +
    
    geom_point(data=coords, aes(x=Longitude, y=Latitude), size=2, shape=23, fill="darkred")+
    geom_text_repel(data = coords, aes(x = Longitude, y = Latitude, label = Ville), fontface = "bold") +
    coord_sf(xlim = c(112, 170), ylim = c(-47, -8), expand = T) +
    
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))
  )
  dev.off()
}

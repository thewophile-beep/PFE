{
  png(paste(plots_path, "Australia_full_map.png", sep=""),width=1000,height=800)
  print(ggplot(data = world) +
    geom_sf(fill="antiquewhite1") +

    geom_sf(data=states,fill=NA) +
    
    geom_point(data = coords, aes(x=Longitude, y=Latitude), size=2, shape=23, fill="darkred")+
    geom_text_repel(data = coords, aes(x = Longitude, y = Latitude, label = Location), fontface = "bold", size=5) +
    coord_sf(xlim = c(112, 170), ylim = c(-47, -8), expand = T) +
    
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "aliceblue"),
          text=element_text(size=20))
  )
  dev.off()
}

# ggplot(data = world) +
#   stat_density_2d(data=Temp_by_cities, aes(x=Longitude, y=Latitude, fill = after_stat(density)), geom = "raster", contour=F) +
#   geom_sf(data=states,fill=NA,color="darkgray") +
#   geom_point(data=coords, aes(x=Longitude, y=Latitude), size=2, shape=23, fill="red")+
#   geom_text_repel(data = coords, aes(x = Longitude, y = Latitude, label = Ville), fontface = "bold", size=5, color="white") +
#   coord_sf(xlim = c(min(coords$Longitude), max(coords$Longitude)), ylim = c(min(coords$Latitude), max(coords$Latitude)), expand = F)

{
  png(paste(plots_path, "Australia_climates.png", sep=""),width=1000,height=800)
  print(ggplot(data = world) +
          geom_sf(fill="antiquewhite1") +
          geom_sf(data=states,fill=NA) +
          
          geom_point(data=coords, aes(x=Longitude, y=Latitude, fill=Climate), size=3, shape=23)+
          coord_sf(xlim = c(112, 170), ylim = c(-47, -8), expand = T) +
          
          annotation_scale(location = "bl", width_hint = 0.2) +
          annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) +
          theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
                panel.background = element_rect(fill = "aliceblue"),
                text=element_text(size=20))
        + labs(fill="Climats")
  )
  dev.off
}

# Après avoir trouvé les Locations les plus proches pour compléter
pdf(paste(plots_path,"Australia_map_segments_complete.pdf",sep=""),width=10,height=8)
for (var in varlist) {
  x=c()
  y=c()
  xend=c()
  yend=c()
  if (!is.null(dim(couples.var[[var]])[1])) {
    for (i in 1:dim(couples.var[[var]])[1]) {
      tmp.cities <- couples.var[[var]][i,]
      tmp.coords <- coords %>%filter(Location == tmp.cities[[1]])
      x <- c(x,tmp.coords$Longitude)
      y <- c(y,tmp.coords$Latitude)
      tmp.coords <- coords %>%filter(Location == tmp.cities[[2]])
      xend <- c(xend,tmp.coords$Longitude)
      yend <- c(yend,tmp.coords$Latitude)
    }
    segments_cities <- data.frame(x=x,y=y,xend=xend,yend=yend)
    
    print(ggplot(data = world) +
            geom_sf(fill="antiquewhite1") +
            geom_sf(data=states,fill=NA) +
            
            geom_point(data=segments_cities, aes(x=x, y=y, fill="blue"), size=3, shape=23) +
            geom_point(data=segments_cities, aes(x=xend, y=yend, fill="red"), size=2, shape=24) +
            
            coord_sf(xlim = c(112, 155), ylim = c(-47, -8), expand = T) +
            
            annotation_scale(location = "bl", width_hint = 0.2) +
            annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) +
            
            theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
                  panel.background = element_rect(fill = "aliceblue"),
                  text=element_text(size=20)) +
            
            scale_fill_discrete(name = "Locations des données", labels = c("Départ", "Arrivée")) +
            guides(fill = guide_legend(override.aes = list(shape = c(23,24), size = c(3, 2)))) +
            
            ggtitle(paste("Variable :",var)) +
            xlab("Longitude") +
            ylab("Latitude") +
            
            geom_segment(data = segments_cities, mapping = aes(x, y, xend=xend, yend=yend), size=0.6, color="black")
    )
  }
}
dev.off()




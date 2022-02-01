couples.var <- list(Cloud9am=list(), Cloud3pm=list(), Evaporation=list(), Sunshine=list(), WindGustDir=list(), WindGustSpeed=list())
for (var in varlist) {
  couples <- c()
  for (city in to.complete[[var]]) {
    city.position <- coords[coords$Location==city,c(3,2)]
    city.climate <- coords[coords$Location==city,4]
    
    closest.city <- list(Name1 = "", Name2 = "", Distance = Inf)
    closest.city.choice <- coords %>%
      filter(Climate == city.climate) %>%
      filter(Location != city) %>%
      filter(Location %in% complete.with[[var]]) %>%
      select(Location, Longitude, Latitude)
    
    
    for (i in 1:dim(closest.city.choice)[1]) {
      city2 <- closest.city.choice[i,]
      distance <- distm(city.position, city2[2:3])
      if (distance < as.numeric(closest.city["Distance"])) {
        closest.city <- list(Name1 = as.character(city2[[1]]), Name2 = city, Distance = distance)
      }
    }
    couples <- rbind(couples, c("Complete with" = closest.city$Name1, "This city" = closest.city$Name2))
  }
  couples.var[[var]] <- couples 
}

# Après avoir trouvé les Locations les plus proches pour compléter
# pdf(paste(plots_path,"Australia_map_segments_complete.pdf",sep=""),width=10,height=8)
for (var in varlist) {
  x=c()
  y=c()
  xend=c()
  yend=c()
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
  
  # print(ggplot(data = world) +
  #         geom_sf(fill="antiquewhite1") +
  #         geom_sf(data=states,fill=NA) +
  #         
  #         geom_point(data=segments_cities, aes(x=x, y=y, fill="blue"), size=3, shape=23) +
  #         geom_point(data=segments_cities, aes(x=xend, y=yend, fill="red"), size=2, shape=24) +
  #         
  #         coord_sf(xlim = c(112, 155), ylim = c(-47, -8), expand = T) +
  #         
  #         annotation_scale(location = "bl", width_hint = 0.2) +
  #         annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) +
  #         
  #         theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
  #               panel.background = element_rect(fill = "aliceblue"),
  #               text=element_text(size=20)) +
  #         
  #         scale_fill_discrete(name = "Location^s des données", labels = c("Départ", "Arrivée")) +
  #         guides(fill = guide_legend(override.aes = list(shape = c(23,24), size = c(3, 2)))) +
  #         
  #         ggtitle(paste("Variable :",var)) + 
  #         xlab("Longitude") + 
  #         ylab("Latitude") +
  #       
  #         geom_segment(data = segments_cities, mapping = aes(x, y, xend=xend, yend=yend), size=0.6, color="black")
  # )
}
# dev.off()


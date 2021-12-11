couples.var <- list(Cloud9am=list(), Cloud3pm=list(), Evaporation=list(), Sunshine=list())
for (var in c("Cloud9am", "Cloud3pm", "Evaporation", "Sunshine")) {
  couples <- c()
  for (city in to.complete[[var]]) {
    city.position <- climates[climates$Ville==city,c(2,3)]
    city.climate <- climates[climates$Ville==city,4]
    
    closest.city <- list(Name1 = "", Name2 = "", Distance = Inf)
    closest.city.choice <- climates %>%
      filter(Climat == city.climate) %>%
      filter(Ville != city) %>%
      filter(Ville %in% complete.with[[var]]) %>%
      select(Ville, Longitude, Latitude)
    
    
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

# Après avoir trouvé les villes les plus proches pour compléter
pdf(paste(plots_path,"Australia_map_segments_complete.pdf"),width=10,height=8)
for (var in c("Cloud9am", "Cloud3pm", "Evaporation", "Sunshine")) {
  x=c()
  y=c()
  xend=c()
  yend=c()
  for (i in 1:dim(couples.var[[var]])[1]) {
    tmp.cities <- couples.var[[var]][i,]
    tmp.coords <- coords %>%filter(Ville == tmp.cities[[1]])
    x <- c(x,tmp.coords$Longitude)
    y <- c(y,tmp.coords$Latitude)
    tmp.coords <- coords %>%filter(Ville == tmp.cities[[2]])
    xend <- c(xend,tmp.coords$Longitude)
    yend <- c(yend,tmp.coords$Latitude)
  }
  segments_cities <- data.frame(x=x,y=y,xend=xend,yend=yend)
  
  print(ggplot(data = world) +
          geom_sf(fill="antiquewhite1") +
          geom_sf(data=states,fill=NA) +
          
          geom_point(data=coords, aes(x=Longitude, y=Latitude, fill=climates$Climat), size=3, shape=23)+
          coord_sf(xlim = c(112, 155), ylim = c(-47, -8), expand = T) +
          
          annotation_scale(location = "bl", width_hint = 0.2) +
          annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) +
          theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
                panel.background = element_rect(fill = "aliceblue"),
                text=element_text(size=20)) +
          labs(fill="Climats") +
          geom_curve(data = segments_cities, mapping = aes(x, y, xend=xend, yend=yend), curvature = 0.8, angle=100, arrow=arrow(length=unit(0.35,"centimetres")), size=0.6, color="black")
  )
}
dev.off()


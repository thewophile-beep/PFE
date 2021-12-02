# pdf(paste(plots_path,"missmap.pdf"))
# missmap(data,main="Missing observations")
# dev.off()

for (city in c("Adelaide")) {
  data.city <- data %>% filter(Location == city) %>% select(Evaporation,Sunshine,Cloud9am,Cloud3pm)
  for (var in names(data.city)) {
    data.city.var <- data.city[,var]
    l <- length(data.city.var)
    ratio <- sum(is.na(data.city.var))/l
    if (ratio > 0.3) {
      city.position <- climates[climates$Ville==city,c(2,3)]
      city.climate <- climates[climates$Ville==city,4]
      
      closest.city <- c(Name = "", Distance = Inf)
      closest.city.choice <- climates %>%
        filter(Climat == city.climate) %>%
        filter(Ville != city) %>%
        select(Ville, Longitude, Latitude)
      

      for (i in 1:dim(closest.city.choice)[1]) {
        city2 <- closest.city.choice[i,]
        distance <- distm(city.position, city2[2:3])
        if (distance < as.numeric(closest.city["Distance"])) {
          closest.city <- c(Name = city2[[1]], Distance = distance)
        }
      }
      
      if (closest.city$Name != "") {
        data[data$Location==city,var] <- data[data$Location==closest.city$Name,var]
      }
    }
  }
}

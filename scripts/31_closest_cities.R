city = "Albury"

city.position <- climates[climates$Ville==city,c(2,3)]
city.climate <- climates[climates$Ville==city,4]

closest.city <- list(Name = "", Distance = Inf)
closest.city.choice <- climates %>%
  filter(Climat == city.climate) %>%
  filter(Ville != city) %>%
  select(Ville, Longitude, Latitude)


for (i in 1:dim(closest.city.choice)[1]) {
  city2 <- closest.city.choice[i,]
  distance <- distm(city.position, city2[2:3])
  if (distance < as.numeric(closest.city["Distance"])) {
    closest.city <- list(Name = as.character(city2[[1]]), Distance = distance)
  }
}

print(closest.city)


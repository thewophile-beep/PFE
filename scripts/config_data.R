rm(list=ls())

# Reading file ----
coords <- read.table("Data/coords.txt", sep=",", header=T)

data.read <- read.table("Data/weatherAUS.csv", header=T, sep=",", na.string="NA", dec=".", quote="")

# Useful variables ----

plots_path <- "Rapport/Images/"

world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(ozmap("states"))

directions = seq(0,337.5,22.5)
names(directions) = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
saisons = as.factor(c("Ete", "Automne", "Hiver", "Printemps"))
climates = as.factor(c("tempere", "subtropical", "desert", "tropical", "plaine"))
cities = unique(data.read$Location)

# Modifying values ----

data.raw = data.read %>% 
  mutate(
    Date = as.Date(Date),
    Month = as.numeric(format(Date, format="%m")),
    Season = case_when(
      Month == 12 | Month <= 2 ~ 1,
      Month >= 3 & Month <= 5 ~ 2,
      Month >= 6 & Month <= 8 ~ 3,
      Month >= 9 & Month <= 11 ~ 4
    ),
    Latitude = coords[Location, 2], 
    Longitude = coords[Location, 3],
    Climate = coords[Location, 4],
    Climate = case_when(
      Climate == "tempere" ~ 1,
      Climate == "subtropical" ~ 2,
      Climate == "desert" ~ 3,
      Climate == "tropical" ~ 4,
      Climate == "plaine" ~ 5
    ),
    WindGustDir = directions[WindGustDir],
    WindDir9am = directions[WindDir9am],
    WindDir3pm = directions[WindDir3pm]
  ) %>%
  select(-c(Month))

# Checking cities to complete ----

varlist = names(data.raw)

# Villes avec variables à compléter -> il manque au moins 30% des observations
to.complete <- vector(mode = "list", length = length(varlist))
names(to.complete) = varlist

# Villes avec lesquelles on peut compléter -> il y a plus de 30% des observations
complete.with <- vector(mode = "list", length = length(varlist))
names(to.complete) = varlist

for (city in cities) {
  data.city <- data.raw %>% filter(Location == city) %>% select(all_of(varlist))
  for (var in varlist) {
    data.city.var <- data.city[,var]
    l <- length(data.city.var)
    ratio <- sum(is.na(data.city.var))/l
    if (as.numeric(ratio) > 0.2) {
      to.complete[[var]] <- c(to.complete[[var]], city)
    } else if (l > 2500) {
      complete.with[[var]] <- c(complete.with[[var]], city)
    }
  }
}


# Finding closest cities to complete with ----
couples.var <- vector(mode = "list", length = length(varlist))
names(couples.var) = varlist
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
    
    l = dim(closest.city.choice)[1]
    if (l != 0) {
      for (i in 1:l) {
        city2 <- closest.city.choice[i,]
        distance <- distm(city.position, city2[2:3])
        if (distance < as.numeric(closest.city["Distance"])) {
          closest.city <- list(Name1 = as.character(city2[[1]]), Name2 = city, Distance = distance)
        }
      }
      couples <- rbind(couples, c("Complete with" = closest.city$Name1, "This city" = closest.city$Name2))
    }
  }
  couples.var[[var]] <- couples 
}

# Complete the data ----

data.completed <- data.raw 

for (var in varlist) {
  for (city.to.complete in couples.var[[var]][,2]) {
    for (city.complete.with in couples.var[[var]][,1]) {
      data.complete.with <- data.raw %>% filter(Location == city.complete.with) %>% select(Date, var)
      tmp.dates = as.Date(intersect(data.completed[data.completed$Location == city.to.complete, "Date"], data.complete.with$Date), origin = lubridate::origin)
      data.completed[data.completed$Date %in% tmp.dates & data.completed$Location == city.to.complete, var] = data.complete.with[data.complete.with$Date %in% tmp.dates, var]
    }
  }
}

# data.completed$Pressure9am[which(is.na(data.completed$Pressure9am))] <- mean(data.completed$Pressure9am, na.rm=T)
# 
# data.completed$Pressure3pm[which(is.na(data.completed$Pressure3pm))] <- mean(data.completed$Pressure3pm, na.rm=T)

data = na.omit(data.completed)

rm(list=ls())

coords <- read.table("Data/coords.txt", sep=",", header=T)

data.read <- read.table("Data/weatherAUS.csv", header=T, sep=",", na.string="NA", dec=".", quote="")

directions <- levels(data.read$WindGustDir)
saisons = as.factor(c("Ete", "Automne", "Hiver", "Printemps"))
climates = as.factor(c("tempere", "subtropical", "desert", "tropical", "plaine"))
cities = unique(data.read$Location)

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
    WindGustDir = as.numeric(WindGustDir),
    WindDir9am = as.numeric(WindDir9am),
    WindDir3pm = as.numeric(WindDir3pm)
  ) %>%
  select(-c(Month, Date))

plots_path <- "Rapport/Images/"

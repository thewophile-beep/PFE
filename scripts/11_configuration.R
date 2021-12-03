rm(list=ls())

data <- read.table("Data/weatherAUS.csv", header=T, sep=",", na.string="NA", dec=".", quote="")
data$Date <- as.Date(data$Date)
data$Day <- yday(data$Date)
data$Year=format(data$Date, format="%Y")
data$Month=format(data$Date, format="%m")
data$ID <- seq(1,dim(data)[1])
cities = levels(data$Location)
coords <- read.table("Data/coords.txt", sep=",", header=T)
climates <- coords[c(1,3,2,4)]
coords <- coords[-4]
coords <- st_as_sf(coords, coords = c("Latitude", "Longitude"), remove = FALSE, crs = 4326, agr = "constant")

directions <- levels(data$WindGustDir)
data$WindGustDir <- c(data$WindGustDir)
data$WindDir9am <- c(data$WindDir9am)
data$WindDir3pm <- c(data$WindDir3pm)

plots_path <- "Rapport/Images/Plots/"

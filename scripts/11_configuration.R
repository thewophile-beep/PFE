rm(list=ls())

data.raw <- read.table("Data/weatherAUS.csv", header=T, sep=",", na.string="NA", dec=".", quote="")
data.raw$Date <- as.Date(data.raw$Date)
data.raw$Day <- yday(data.raw$Date)
data.raw$Year=format(data.raw$Date, format="%Y")
data.raw$Month=format(data.raw$Date, format="%m")
data.raw$ID <- seq(1,dim(data.raw)[1])
cities = levels(data.raw$Location)
coords <- read.table("Data/coords.txt", sep=",", header=T)
climates <- coords[c(1,3,2,4)]
coords <- coords[-4]
coords <- st_as_sf(coords, coords = c("Latitude", "Longitude"), remove = FALSE, crs = 4326, agr = "constant")

directions <- levels(data.raw$WindGustDir)
data.raw$WindGustDir <- c(data.raw$WindGustDir)
data.raw$WindDir9am <- c(data.raw$WindDir9am)
data.raw$WindDir3pm <- c(data.raw$WindDir3pm)

plots_path <- "Rapport/Images/Plots/"

rm(list=ls())

library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
options(ggrepel.max.overlaps = Inf)
library(tools)
library(ozmaps)
library(dplyr)
library(ggspatial)
library(Amelia)
library("FactoMineR")
library("factoextra")
library(ggcorrplot)
library(tidyr)
library(reshape2)

data <- read.table("Data/weatherAUS.csv", header=T, sep=",", na.string="NA", dec=".", quote="")
data$Date <- as.Date(data$Date)
data$Day <- yday(data$Date)
data$Year=format(data$Date, format="%Y")
data$Month=format(data$Date, format="%m")
data$ID <- seq(1,dim(data)[1])
cities = levels(data$Location)
coords <- read.table("Data/coords.txt", sep=",", header=T)
climates <- coords[-2:-3]
coords <- coords[-4]
coords <- st_as_sf(coords, coords = c("Latitude", "Longitude"), remove = FALSE, crs = 4326, agr = "constant")

plots_path <- "Rapport/Images/Plots/"

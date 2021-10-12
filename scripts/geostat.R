library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggrepel")
options(ggrepel.max.overlaps = Inf)

world <- ne_countries(scale = "medium", returnclass = "sf")

coords <- read.table("data/coords.txt", sep=",", header=T)
coords <- st_as_sf(coords, coords = c("Latitude", "Longitude"), remove = FALSE, crs = 4326, agr = "constant")
  

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = coords) +
  geom_point(data=coords, aes(x=Longitude, y=Latitude), size=2, shape=23, fill="darkred")+
  geom_text_repel(data = coords, aes(x = Longitude, y = Latitude, label = Ville), fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25,-0.25, 0.5, 0.5, -0.5), label.size=0.25) +
  coord_sf(xlim = c(112, 170), ylim = c(-47, -8), expand = T)


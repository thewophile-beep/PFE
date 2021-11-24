data_uluru <- data %>%
  filter(Location == "Uluru")
missmap(data_uluru)

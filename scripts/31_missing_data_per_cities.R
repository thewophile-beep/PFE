pdf(paste(plots_path,"missing_per_cities.pdf"))
for (i in cities) {
  tmp.data_city <- data %>%
    filter(Location == i) %>%
    select(Sunshine, Evaporation, Cloud9am, Cloud3pm)
  missmap(tmp.data_city, main=i)
}
dev.off()

percent_na_cities <- c()
for (i in cities) {
  tmp.city <- data %>% filter(Location == i)
  percent_na_cities <- c(percent_na_cities,
    sum(is.na(tmp.city)) / (dim(tmp.city)[1] * dim(tmp.city)[2])
  )
}
names(percent_na_cities) <- cities
barplot(percent_na_cities)

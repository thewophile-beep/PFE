pdf(paste(plots_path,"missing_per_cities.pdf"))
for (i in cities) {
  tmp.data_city <- data %>%
    filter(Location == i) %>%
    select(Sunshine, Evaporation, Cloud9am, Cloud3pm)
  missmap(tmp.data_city, main=i)
}
dev.off()

percent_na_cities <- data.frame()
for (i in cities) {
  tmp.city <- data %>% filter(Location == i)
  percent_na_cities <- rbind(percent_na_cities,
    sum(is.na(tmp.city)) / (dim(tmp.city)[1] * dim(tmp.city)[2])
  )
}
rownames(percent_na_cities) <- cities
names(percent_na_cities) <- "percent"
ggplot(percent_na_cities, aes(x=percent, y=cities)) + geom_col()


# Villes avec variables à compléter -> il manque au moins 30% des observations
to.complete <- list(Cloud9am=c(), Cloud3pm=c(), Evaporation=c(), Sunshine=c())

# Villes avec lesquelles on peut compléter -> il y a plus de 30% des observations
complete.with <- list(Cloud9am=c(), Cloud3pm=c(), Evaporation=c(), Sunshine=c())

for (city in cities) {
  data.city <- data %>% filter(Location == city) %>% select(Evaporation,Sunshine,Cloud9am,Cloud3pm)
  for (var in c("Cloud9am", "Cloud3pm", "Evaporation", "Sunshine")) {
    data.city.var <- data.city[,var]
    l <- length(data.city.var)
    ratio <- sum(is.na(data.city.var))/l
    if (ratio > 0.3) {
      to.complete[[var]] <- c(to.complete[[var]], city)
    } else {
      complete.with[[var]] <- c(complete.with[[var]], city)
    }
  }
}

print(to.complete)

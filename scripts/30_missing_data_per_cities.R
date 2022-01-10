pdf(paste(plots_path,"missing_per_cities.pdf",sep=""))
for (i in cities) {
  tmp.data_city <- data.raw %>%
    filter(Location == i) %>%
    select(Sunshine, Evaporation, Cloud9am, Cloud3pm, WindGustDir, WindGustSpeed)
  missmap(tmp.data_city, main=i)
}
dev.off()

percent_na_cities <- data.frame()
for (i in cities) {
  tmp.city <- data.raw %>% filter(Location == i)
  percent_na_cities <- rbind(percent_na_cities,
    sum(is.na(tmp.city)) / (dim(tmp.city)[1] * dim(tmp.city)[2])
  )
}
rownames(percent_na_cities) <- cities
names(percent_na_cities) <- "percent"
ggplot(percent_na_cities, aes(x=percent, y=cities)) + geom_col()

varlist = c("Cloud9am", "Cloud3pm", "Evaporation", "Sunshine", "WindGustDir", "WindGustSpeed")

# Villes avec variables à compléter -> il manque au moins 30% des observations
to.complete <- list(Cloud9am=list(), Cloud3pm=list(), Evaporation=list(), Sunshine=list(), WindGustDir=list(), WindGustSpeed=list())

# Villes avec lesquelles on peut compléter -> il y a plus de 30% des observations
complete.with <- list(Cloud9am=list(), Cloud3pm=list(), Evaporation=list(), Sunshine=list(), WindGustDir=list(), WindGustSpeed=list())

for (city in cities) {
  data.city <- data.raw %>% filter(Location == city) %>% select(Evaporation,Sunshine,Cloud9am,Cloud3pm,WindGustDir,WindGustSpeed)
  for (var in varlist) {
    data.city.var <- data.city[,var]
    l <- length(data.city.var)
    ratio <- sum(is.na(data.city.var))/l
    if (as.numeric(ratio) > 0.4) {
      to.complete[[var]] <- c(to.complete[[var]], city)
    } else {
      complete.with[[var]] <- c(complete.with[[var]], city)
    }
  }
}

print(to.complete)
print(complete.with)

data.completed <- data.raw 

for (var in varlist) {
  for (city.to.complete in couples.var[[var]][,2]) {
    for (city.complete.with in couples.var[[var]][,1]) {
      data.complete.with <- data.raw %>% filter(Location == city.complete.with) %>% select(Date, all_of(varlist))
      data.to.complete <- data.raw %>% filter(Location == city.to.complete) %>% select(Date, all_of(varlist))
      
      data.completed[(data.completed$Date %in% data.complete.with$Date & data.completed$Location == city.to.complete),varlist] <- data.complete.with[,varlist]
    }
  }
}

data.completed$Pressure9am[which(is.na(data.completed$Pressure9am))] <- mean(data.completed$Pressure9am, na.rm=T)

data.completed$Pressure3pm[which(is.na(data.completed$Pressure3pm))] <- mean(data.completed$Pressure3pm, na.rm=T)

# print(missmap(data.completed, main="data.completed"))

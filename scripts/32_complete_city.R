var.to.complete = c("Cloud9am","Cloud3pm","Evaporation","Sunshine")
data.completed <- data.raw 

for (var in var.to.complete) {
  for (city.to.complete in couples.var[[var]][,2]) {
    for (city.complete.with in couples.var[[var]][,1]) {
      data.complete.with <- data.raw %>% filter(Location == city.complete.with) %>% select(Date, all_of(var.to.complete))
      data.to.complete <- data.raw %>% filter(Location == city.to.complete) %>% select(Date, all_of(var.to.complete))
      
      data.completed[(data.completed$Date %in% data.complete.with$Date & data.completed$Location == city.to.complete),var.to.complete] <- data.complete.with[,var.to.complete]
    }
  }
}

data.completed$Pressure9am[which(is.na(data.completed$Pressure9am))] <- mean(data.completed$Pressure9am, na.rm=T)

data.completed$Pressure3pm[which(is.na(data.completed$Pressure3pm))] <- mean(data.completed$Pressure3pm, na.rm=T)

missmap(data.completed, main="data.completed")

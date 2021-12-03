city.to.complete = "Albury"
city.complete.with = "WaggaWagga"
var.to.complete = c("Cloud9am","Cloud3pm","Evaporation","Sunshine")
data.completed <- data 

data.complete.with <- data %>% filter(Location == city.complete.with) %>% select(Date, all_of(var.to.complete))
data.to.complete <- data %>% filter(Location == city.to.complete) %>% select(Date, all_of(var.to.complete))

data.completed[(data.completed$Date %in% data.complete.with$Date &
                  data.completed$Location == city.to.complete),var.to.complete] <- data.complete.with[,var.to.complete]

data.completed

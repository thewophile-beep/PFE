data_by_day <- data %>%
  group_by(Location, Day) %>%
  summarise(
    MinTemp = mean(MinTemp, na.rm=T),
    MaxTemp = mean(MaxTemp, na.rm=T),
    Rainfall = mean(Rainfall, na.rm=T),
    Evaporation = mean(Evaporation, na.rm=T),
    Sunshine = mean(Sunshine, na.rm=T),
    MeanTemp = mean(cbind(Temp9am, Temp3pm))
  )

data_by_day_Albury <- data_by_day[data_by_day$Location == "Albury",]

ggplot(data=data_by_day_Albury, aes(Day, MeanTemp)) + 
  geom_ribbon(aes(ymin = MinTemp, ymax = MaxTemp), fill="lightblue") +
  geom_line() + 
  labs(x="Days", y="Temperature", title="Daily mean of temperature with min and max in Albury from 2008 to 2017",
       MaxTemp = "Max temperature",
       MinTemp = "Min temperature"
      ) 


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

data_by_month <- data %>%
  group_by(Location, Month) %>%
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

data_by_month_Canberra <- data_by_month[data_by_month$Location == "Canberra",]
data_by_day_Canberra <- data_by_day[data_by_day$Location == "Canberra",]

{
  pdf(paste(plots_path, "Temp_and_Rainfall.pdf"))
  for (i in cities) {
    print(ggplot(data=data_by_month[data_by_month$Location == i,], aes(x=Month)) +
      geom_line(aes(y=MaxTemp, group=2, col="MaxTemp")) +
      geom_point(aes(y=MaxTemp, group=2, col="MaxTemp")) +
      geom_line(aes(y=MinTemp, group=1, col="MinTemp")) +
      geom_point(aes(y=MinTemp, group=1, col="MinTemp")) +
      geom_col(aes(y=Rainfall, fill="Rainfall")) +
      labs(x="Months", y="Temperature", title=i, fill="Barplots", colour="Temperature") +
      scale_fill_manual(values = "lightblue") +
      scale_color_manual(values = c("red", "blue")) +
      theme(legend.position="right")
    )
  }
  dev.off()
}
 

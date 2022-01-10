data.periods <- na.omit(data.completed)

data_by_day <- data.periods %>%
  group_by(Location, Day) %>%
  summarise(
    MinTemp = mean(MinTemp, na.rm=T),
    MaxTemp = mean(MaxTemp, na.rm=T),
    Rainfall = mean(Rainfall, na.rm=T),
    Evaporation = mean(Evaporation, na.rm=T),
    Sunshine = mean(Sunshine, na.rm=T),
    MeanTemp = mean(cbind(Temp9am, Temp3pm))
  )

data_by_month <- data.periods %>%
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
  # for (c in unique(climates$Climat)) {
  #   name = sprintf("%sTemp_and_Rainfall%s.pdf",plots_path,c)
    pdf(name)
    for (i in climates[climates$Climat==c,]$Ville) {
      print(
        ggplot(data=data_by_month[data_by_month$Location == i,], aes(x=Month))+
        geom_line(aes(y=MaxTemp, group=1, col="Temp Max")) +
        geom_point(aes(y=MaxTemp, group=1, col="Temp Max")) +
        geom_line(aes(y=MinTemp, group=2, col="Temp Min")) +
        geom_point(aes(y=MinTemp, group=2, col="Temp Min")) +
        geom_point(aes(y=MeanTemp, group=3, col="Temp Moyenne")) +
        geom_line(aes(y=MeanTemp, group=3, col="Temp Moyenne")) +
        geom_col(aes(y=Rainfall, fill="Rainfall")) +
        labs(x="Months", y="Temperature", title=paste(i,climates[climates$Ville==i,]$Climat,sep=" : "), fill="Barplots", colour="Temperature") +
        scale_fill_manual(values = "lightblue") +
        scale_color_manual(values = c("red", "blue", "violet")) +
        theme(legend.position="right") + 
        scale_y_continuous(name="Température (°C)", sec.axis=sec_axis(~.*1, name="Rainfall (mm)"))
      )
    }
    dev.off()
  # }
}
 

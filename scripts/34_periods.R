data.periods <- na.omit(data.completed)

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
 

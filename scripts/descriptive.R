rm(list=ls())
library(ggplot2)
library(lubridate)

data <- read.table("data/weatherAUS.csv", header=T, sep=",", na.string="NA", dec=".", quote="")
data$Date <- as.Date(data$Date)
data$Day <- yday(data$Date)
data$Year=format(data$Date, format="%Y")

str(data)

statbase <- NULL
numeric_names <- c()
other_names <- c()
for (i in names(data)) {
  if (class(data[[i]]) %in% c("numeric", "integer")) {
    statbase <- cbind(statbase, summary(data[[i]]))
    numeric_names <- c(numeric_names, i)
  }
  else {
    other_names <- c(other_names, i)
  }
}
colnames(statbase) <- numeric_names

print("Summary of numerical values : ")
print(statbase)

# Cities observed
unique(data$Location)

# Example with Albury
# data_Albury <- data[data$Location=="Albury",]
# plot_Albury_Rainfall_Year <- ggplot(data_Albury, aes(x=Day, y=Rainfall, na.rm=T, color=Year)) +
#   geom_bar(stat = "identity") +
#   facet_wrap( ~ Year)
# plot_Albury_Rainfall_Year
# 
# plot_Albury_Rainfall_Daily <- ggplot(data_Albury, aes(x=Day, y=MinTemp, group=Day, color=Day, na.rm=T)) +
#   geom_point()  
# plot_Albury_Rainfall_Daily



rm(list=ls())

data <- read.table("data/weatherAUS.csv", header=T, sep=",", na.string="NA", dec=".", quote="")
data$Date <- as.Date(data$Date)

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

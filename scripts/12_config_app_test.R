data <- na.omit(data.completed %>% select(-c(Date, Location)))

data = upSample(data %>% select(-RainTomorrow), data$RainTomorrow, yname="RainTomorrow")

data$RainToday <- as.numeric(data$RainToday) - 1
data$RainTomorrow <- as.numeric(data$RainTomorrow) - 1
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(data$RainTomorrow, p=split, list=FALSE)
dataApp <- data[trainIndex,]
dataTest <- data[-trainIndex,]

y.app = dataApp$RainTomorrow
y.test = dataTest$RainTomorrow

# Centrage et réduction des données.
# DataApp
m = apply(dataApp, 2, mean)
ec = apply(dataApp, 2, sd)
idx = !(names(dataApp) %in% c("RainTomorrow", "RainToday"))
dataApp.cent = dataApp
dataTest.cent = dataTest
dataApp.cent[,idx] = scale(dataApp[,idx])
# Centrage et réduction de dataTest
for(j in which(idx)){
  dataTest.cent[,j] = (dataTest[,j] - m[j])/ec[j]
}

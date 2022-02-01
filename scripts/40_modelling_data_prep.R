data <- na.omit(data.completed[c(-25,-26,-27)])

# data = upSample(data[-23], data$RainTomorrow, yname="RainTomorrow")

data$RainToday <- as.numeric(data$RainToday) - 1
data$RainTomorrow <- as.numeric(data$RainTomorrow) - 1
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(data$RainTomorrow, p=split, list=FALSE)
dataApp <- data[trainIndex,]
dataTest <- data[-trainIndex,]

y.app = dataApp$RainTomorrow
y.test = dataTest$RainTomorrow

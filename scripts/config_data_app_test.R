SMOTE = F
data.model = data %>% select(-c(Date, Location))
data.model$RainToday <- as.numeric(data.model$RainToday) - 1
data.model$RainTomorrow <- as.numeric(data.model$RainTomorrow) - 1

if (SMOTE) {
  data.model = SMOTE(data.model, data.model$RainTomorrow)
  data.model = data.model$data
} 

# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(data.model$RainTomorrow, p=split, list=FALSE)
dataApp <- data.model[trainIndex,]
dataTest <- data.model[-trainIndex,]

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

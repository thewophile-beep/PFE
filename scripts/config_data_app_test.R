SMOTE = F
if (SMOTE) {
  # data.model.smote = SMOTE(data.model, data.model$RainTomorrow)
  data.model = data.model.smote$data %>% select(-class)
}

# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex = createDataPartition(data.model$RainTomorrow, p=split, list=FALSE)
dataApp = data.model[trainIndex,]
dataTest = data.model[-trainIndex,] 

# Centrage et réduction des données.
# DataApp
m = apply(dataApp, 2, mean)
ec = apply(dataApp, 2, sd)
idx = !(names(dataApp) %in% c("RainTomorrow", "RainToday"))
dataApp[,idx] = scale(dataApp[,idx])
# Centrage et réduction de dataTest
for(j in which(idx)){
  dataTest[,j] = (dataTest[,j] - m[j])/ec[j]
}

y.app = dataApp$RainTomorrow
y.test = dataTest$RainTomorrow

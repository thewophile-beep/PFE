# SMOTE ----
data.model.smote = SMOTE(data.onehot %>% select(-RainTomorrow), data.onehot$RainTomorrow)
data.model= data.model.smote$data %>% 
  mutate(RainTomorrow = as.factor(class)) %>% 
  select(-class)

# upsampling ----
data.model = upSample(x = data[, !names(data) %in% "RainTomorrow"], y = data[,"RainTomorrow"], yname = "RainTomorrow")
table(data.model$RainTomorrow)

# One-hot ----
data.model = data.onehot

# Normal ----
data.model = data

# dataApp & dataTest ----
{
  split=0.80
  trainIndex = createDataPartition(data.model$RainTomorrow, p=split, list=FALSE)
  dataApp = data.model[trainIndex,]
  dataTest = data.model[-trainIndex,] 
  y.app = as.numeric(dataApp$RainTomorrow) - 1
  y.test = as.numeric(dataTest$RainTomorrow) - 1
}

# Centrage et réduction des données. ----
{
  # DataApp
  idx = !(names(dataApp) %in% "RainTomorrow")
  m = apply(dataApp[,idx], 2, mean)
  ec = apply(dataApp[,idx], 2, sd)
  dataApp[,idx] = scale(dataApp[,idx])
  # Centrage et réduction de dataTest
  for(j in names(dataApp)[idx]){
    dataTest[,j] = (dataTest[,j] - m[j])/ec[j]
  }
}


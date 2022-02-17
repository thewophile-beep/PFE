# SMOTE ----
data.model = SMOTE(data.onehot, data.onehot$RainTomorrow)
data.model= data.model.smote$data %>% select(-class)

# upsampling ----
data.model = upSample(x = data.onehot %>% select(-RainTomorrow), 
                      y = data.onehot %>% select(RainTomorrow))

# One-hot ----
data.model = data.onehot

# dataApp & dataTest
split=0.80
trainIndex = createDataPartition(data.model$RainTomorrow, p=split, list=FALSE)
dataApp = data.model[trainIndex,]
dataTest = data.model[-trainIndex,] 
y.app = dataApp$RainTomorrow
y.test = dataTest$RainTomorrow

# Centrage et réduction des données.
# DataApp
m = apply(dataApp, 2, mean)
ec = apply(dataApp, 2, sd)
idx = (names(dataApp) %in% varlist.num)
dataApp[,idx] = scale(dataApp[,idx])
# Centrage et réduction de dataTest
for(j in which(idx)){
  dataTest[,j] = (dataTest[,j] - m[j])/ec[j]
}


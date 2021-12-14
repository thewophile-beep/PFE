source("scripts/fonctions_scores.R")

# Centrage et réduction des données.
# DataApp
m = apply(dataApp, 2, mean)
ec = apply(dataApp, 2, sd)
dataApp[,-c(1, 2, 8, 10, 11, 22, 23, 24)] = scale(dataApp[,-c(1, 2, 8, 10, 11, 22, 23, 24)])
# Centrage et réduction de dataTest
for(j in c(3:7,9,12:21)){
  dataTest[,j] = (dataTest[,j] - m[j])/ec[j]
}

# Construction de l’arbre maximal ----
mod.tmax = rpart(RainTomorrow ~., control=rpart.control(cp=0),data = dataApp)
summary(mod.tmax)
print(mod.tmax)
plot(mod.tmax, branch = 0.3, uniform = T)
text(mod.tmax, digit = 4,col=2)
title("Arbre maximal")
# nombre de feuilles de mod.tmax
nf.tmax = max(mod.tmax$cptable[,2]) + 1
cat(paste("Nombre de feuilles de T.max :",nf.tmax))
# Importance des variables
mod.tmax$variable.importance

# Construction des prédictions sur Eapp et Etest
y.tmax = (predict(mod.tmax) > 0.5) * 1
yt.tmax = (predict(mod.tmax, newdata = dataTest) > 0.5) * 1
# Calcul des performances

table(y.tmax, y.app)
table(yt.tmax, y.test)

{
  print("Sensibilité dataApp")
  print(sensitivity(factor(y.tmax), factor(y.app)))
  print("Spécificité dataApp")
  print(specificity(factor(y.tmax), factor(y.app)))
  
  print("Sensibilité dataTest")
  print(sensitivity(factor(yt.tmax), factor(y.test)))
  print("Spécificité dataTest")
  print(specificity(factor(yt.tmax), factor(y.test)))
}

# topt ----

mod.topt <- prune(mod.tmax, cp = 0.0001)
nf.topt = max(mod.topt$cptable[,2]) + 1
y.topt = (predict(mod.topt) > 0.5) * 1
yt.topt = (predict(mod.topt, newdata = dataTest) > 0.5) * 1

{
  print("Sensibilité dataApp")
  print(sensitivity(factor(y.topt), factor(y.app)))
  print("Spécificité dataApp")
  print(specificity(factor(y.topt), factor(y.app)))
  
  print("Sensibilité dataTest")
  print(sensitivity(factor(yt.topt), factor(y.test)))
  print("Spécificité dataTest")
  print(specificity(factor(yt.topt), factor(y.test)))
}


Ntrees = c(1:20)*50
set.seed(123)
mod.rf = randomForest(RainTomorrow~. , data=dataApp, ntree=1000)
mse.OOB = round(mod.rf$mse[Ntrees],2)
mse.OOB
EV.OOB = round(mod.rf$rsq[Ntrees]*100,2)
EV.OOB

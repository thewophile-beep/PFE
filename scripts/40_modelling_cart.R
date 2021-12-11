source("scripts/fonctions_scores.R")

# Construction de l’arbre maximal
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
y.tmax = (predict(mod.tmax)[,2] > 0.5) * 1
yt.tmax = (predict(mod.tmax, newdata = dataTest)[,2] > 0.5) * 1
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

# f1score

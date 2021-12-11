mod.glm <- glm(RainTomorrow ~ ., gaussian, data = dataApp)

# Construction des prédictions sur Eapp et Etest
y.glm = predict(mod.glm)
yt.glm = predict(mod.glm, newdata=dataTest)

source("scripts/fonctions_scores.R")
print(scorem(y.app,cbind(y.glm),"dataApp"))
print(scorem(y.test,cbind(yt.glm),"dataApp"))

mod.glm <- glm(RainTomorrow ~ ., gaussian, data = dataApp.cent)

# Construction des prédictions sur Eapp et Etest
threshold = 0.5
y.glm = (predict(mod.glm) > threshold) * 1
yt.glm = (predict(mod.glm, newdata = dataTest.cent) > threshold) * 1
confusionMatrix(table(yt.glm, y.test))


# tmax ----
mod.tmax = rpart(RainTomorrow ~., control=rpart.control(cp=0),data = dataApp)
# summary(mod.tmax)
# print(mod.tmax)
# plot(mod.tmax, branch = 0.3, uniform = T)
# text(mod.tmax, digit = 4,col=2)
# title("Arbre maximal")
# # nombre de feuilles de mod.tmax
max(mod.tmax$cptable[,2]) + 1
# cat(paste("Nombre de feuilles de T.max :",nf.tmax))
# # Importance des variables
# mod.tmax$variable.importance
# Construction des prédictions sur Eapp et Etest
y.tmax = as.vector((predict(mod.tmax) > 0.5) * 1)
yt.tmax = as.vector((predict(mod.tmax, newdata = dataTest) > 0.5) * 1)
# Calcul des performances
confusionMatrix(table(y.tmax, y.app))
confusionMatrix(table(yt.tmax, y.test))

# topt ----
mod.topt <- prune(mod.tmax, cp = 0.001)
max(mod.topt$cptable[,2]) + 1
plot(mod.topt, branch = 0.3, uniform = T)
text(mod.topt, digit = 4,col=2, cex=0.4)
threshold = 0.5
y.topt = predict(mod.topt)
# hist(y.topt, probability = T)
y.topt = (y.topt > threshold) * 1
yt.topt = predict(mod.topt, newdata = dataTest)
yt.topt = (yt.topt > threshold) * 1
confusionMatrix(table(y.topt, y.app))
confusionMatrix(table(yt.topt, y.test))

# rf ----
mod.rf = randomForest(RainTomorrow~. , data=dataApp, ntree=100)
mse.OOB = round(mod.rf$mse[Ntrees],2)
mse.OOB
EV.OOB = round(mod.rf$rsq[Ntrees]*100,2)
EV.OOB

# glm ----
mod.glm <- glm(RainTomorrow ~ ., gaussian, data = dataApp)
# Construction des prédictions sur Eapp et Etest
threshold = 0.5
y.glm = (predict(mod.glm) > threshold) * 1
yt.glm = (predict(mod.glm, newdata = dataTest) > threshold) * 1
confusionMatrix(table(y.glm, y.app))
confusionMatrix(table(yt.glm, y.test))

mod.glm.r = step(mod.glm)
# Construction des prédictions sur Eapp et Etest
threshold = 0.5
y.glm.r = (predict(mod.glm.r) > threshold) * 1
yt.glm.r = (predict(mod.glm.r, newdata = dataTest) > threshold) * 1
confusionMatrix(table(yt.glm.r, y.test))


# kmeans ----
mod.kmeans = kmeans(dataApp %>% select(-c(RainTomorrow)), centers = 2, nstart = 10)
y.kmeans = mod.kmeans$cluster - 1

confusionMatrix(table(y.kmeans, y.app))
confusionMatrix(table(y.kmeans, y.app))

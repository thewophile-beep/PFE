# tmax ----
mod.tmax = rpart(RainTomorrow ~., control=rpart.control(cp=0), data = dataApp)
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
y.tmax = as.vector(apply(predict(mod.tmax), 1,  which.max) - 1)
yt.tmax = as.vector(apply(predict(mod.tmax, newdata = dataTest), 1,  which.max) - 1)
# Calcul des performances
confusionMatrix(table(y.tmax, y.app))
confusionMatrix(table(yt.tmax, y.test))

# topt ----
mod.topt <- prune(mod.tmax, cp = 0.001)
max(mod.topt$cptable[,2]) + 1
plot(mod.topt, branch = 0.3, uniform = T)
text(mod.topt, digit = 4, col=3, cex=0.7)
title("Arbre optimal avec cp=0.001 (42 feuilles)")
threshold = 0.5
y.topt = as.vector(apply(predict(mod.topt), 1,  which.max) - 1)
yt.topt = as.vector(apply(predict(mod.topt, newdata = dataTest), 1,  which.max) - 1)
confusionMatrix(table(y.topt, y.app))
confusionMatrix(table(yt.topt, y.test))

# rf ----
mod.rf = randomForest(RainTomorrow~., data=dataApp, maxnodes = 1000)
y.rf = as.vector(predict(mod.rf))
yt.rf = as.vector(predict(mod.rf, newdata = dataTest))
confusionMatrix(table(y.rf, y.app))
confusionMatrix(table(yt.rf, y.test))

mod.rf.big = randomForest(RainTomorrow~.,data=dataApp, maxnodes = 2000)
y.rf.big = as.vector(predict(mod.rf.big))
yt.rf.big = as.vector(predict(mod.rf.big, newdata = dataTest))
confusionMatrix(table(y.rf.big, y.app))
confusionMatrix(table(yt.rf.big, y.test))

mod.rf.small = randomForest(RainTomorrow~., data=dataApp, maxnodes = 500)
y.rf.small = as.vector(predict(mod.rf.small))
yt.rf.small = as.vector(predict(mod.rf.small, newdata = dataTest))
confusionMatrix(table(y.rf.small, y.app))
confusionMatrix(table(yt.rf.small, y.test))

# glm ----
mod.glm = glm(RainTomorrow~ ., binomial, data = dataApp)
# confint(mod.glm)
threshold = 0.5
y.glm = (as.vector(predict(mod.glm, type="response")) > threshold) * 1
yt.glm = (as.vector(predict(mod.glm, newdata = dataTest, type="response")) > threshold) * 1
confusionMatrix(table(y.glm, y.app))
confusionMatrix(table(yt.glm, y.test))

# glm reduced ----
mod.glm.r = step(mod.glm)
threshold = 0.5
y.glm.r = (as.vector(predict(mod.glm.r, type="response")) > threshold) * 1
yt.glm.r = (as.vector(predict(mod.glm.r, newdata = dataTest, type="response")) > threshold) * 1
confusionMatrix(table(y.glm.r, y.app))
confusionMatrix(table(yt.glm.r, y.test))

# lda ----
mod.lda = lda(dataApp %>% select(-RainTomorrow, -Climate.5, -Season.4), dataApp[,"RainTomorrow"])
y.lda = predict(mod.lda)$class
yt.lda = predict(mod.lda, newdata = dataTest %>% select(-RainTomorrow, -Climate.5, -Season.4))$class
confusionMatrix(table(y.lda, y.app))
confusionMatrix(table(yt.lda, y.test))

# qda ----
mod.qda = qda(dataApp%>% select(-RainTomorrow, -Climate.5, -Season.4), dataApp[,"RainTomorrow"])
y.qda = predict(mod.qda)$class
yt.qda = predict(mod.qda, newdata = dataTest %>% select(-RainTomorrow, -Climate.5, -Season.4))$class
confusionMatrix(table(y.qda, y.app))
confusionMatrix(table(yt.qda, y.test))

# Response distribution ----
plot_distrib = function(x) {
  ggplot() +
    aes(x) +
    geom_histogram(binwidth=0.01, col="black", fill="white") +
    labs(title = "Distribution de la prédiction pour le modèle de régression logistique") +
    xlab("predicted probability")
}

plot_distrib(as.vector(predict(mod.glm, newdata = dataTest, type='link')))
plot_distrib(as.vector(predict(mod.tmax, newdata = dataTest)[,2]))
plot_distrib(as.vector(predict(mod.topt, newdata = dataTest)[,2]))

# res vs fitted ----
x = predict(mod.glm.r, newdata = dataTest)
y = residuals(mod.glm.r)
ggplot() +
  geom_point(aes(x=x, y=y))
autoplot(mod.glm.r, colour="RainTomorrow")
# McNemar test ----
mcnemar.test(table((y.test==yt.rf),(y.test==yt.rf.big)))

# Big Comparison ----
{
  res = NULL
  res = rbind(res, confusionMatrix(table(y.glm, y.app))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(y.glm.r, y.app))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(y.lda, y.app))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(y.qda, y.app))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(y.tmax, y.app))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(y.topt, y.app))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(y.rf, y.app))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(y.rf.big, y.app))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  
  res = rbind(res, confusionMatrix(table(yt.glm, y.test))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(yt.glm.r, y.test))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(yt.lda, y.test))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(yt.qda, y.test))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(yt.tmax, y.test))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(yt.topt, y.test))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(yt.rf, y.test))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  res = rbind(res, confusionMatrix(table(yt.rf.big, y.test))$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")])
  
  rownames(res) = c(
    "glm", 
    "glm.r", 
    "lda", 
    "qda", 
    "tmax", 
    "topt", 
    "rf (maxnodes = 1000)", 
    "rf (maxnodes = 2000)",
    "glm test", 
    "glm.r test", 
    "lda test", 
    "qda test",
    "tmax test", 
    "topt test", 
    "rf (maxnodes = 1000) test", 
    "rf (maxnodes = 2000) test"
)
  res = round(res, 2)
}

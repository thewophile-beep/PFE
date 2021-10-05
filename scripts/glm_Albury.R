glm_by_location <- function(location) {
  source("scripts/score.R")
  data <- data[data$Location==location,]
  data <- data[, !names(data) %in% c("Location", "Year")]
  if (sum(!is.na(data$Evaporation)) == 0) {
    data <- data[, !names(data) == "Evaporation"]
  }
  if(sum(!is.na(data$Sunshine)) == 0) {
    data <- data[, !names(data) == "Sunshine"]
  }
  # Construction des échantillons d’apprentissage et de test
  set.seed(111) # initialisation du générateur
  # Extraction des échantillons
  test.ratio = 0.25 # part de l’échantillon test
  npop = nrow(data) # nombre de lignes dans les données
  nvar = ncol(data) # nombre de colonnes
  ntest = ceiling(npop*test.ratio) # taille de l’échantillon test
  testi = sample(1:npop,ntest) # indices de l’échantillon test
  appri = setdiff(1:npop,testi) # indices complémentaires de l’échant. d’apprentissage
  # Construction des échantillons
  dataApp = na.omit(data[appri,]) # construction de l’échantillon d’apprentissage
  dataTest = na.omit(data[testi,]) # construction de l’échantillon test
  
  mod.glm <- glm(RainTomorrow ~ ., family = binomial, data = dataApp)
  print(summary(mod.glm))
  
  mod.glm.r <- step(mod.glm, trace=F)
  print(summary(mod.glm.r))
  
  print("======== Variables retirées ========")
  print(setdiff(attr(mod.glm$terms, "term.labels"), attr(mod.glm.r$terms, "term.labels")))
  
  y.app <- dataApp$RainTomorrow
  y.glm <- ifelse(predict(mod.glm, type="response", data=dataApp)>0.5,1,0)
  y.glm.r <- ifelse(predict(mod.glm.r, type="response", data=dataApp)>0.5,1,0)
  
  y.test <- dataTest$RainTomorrow
  yt.glm <- ifelse(predict(mod.glm, type="response", newdata=dataTest)>0.5,1,0)
  yt.glm.r <- ifelse(predict(mod.glm.r, type="response", newdata=dataTest)>0.5,1,0)
  
  print("======== Modèle complet ========")
  print("Echantillon d'apprentissage")
  print(score(y.app, y.glm))
  
  print("Echantillon test")
  print(score(y.test, yt.glm))
  
  print("======== Modèle réduit ========")
  print("Echantillon d'apprentissage")
  print(score(y.app, y.glm.r))
  
  print("Echantillon test")
  print(score(y.test, yt.glm.r))
  
  return(list("data"=data, "mod.glm"=mod.glm, "mod.glm.r"=mod.glm.r))
}

glm_Albury <- glm_by_location("Albury")


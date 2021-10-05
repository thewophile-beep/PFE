score = function(y, ychap){
  result = table(y, ychap)
  sensi = result[1,1] / (result[1,2] + result[1,1])
  speci =  result[2,2] / (result[2,1] + result[2,2])
  tauxClass = 1 - (result[1,2] + result[2,1]) / sum(result)
  Sc = c(tauxClass, sensi, speci) * 100
  names(Sc) = c("Taux bons Class.", "Sensibilité", "Spécificité")
  score = round(Sc,2)
}

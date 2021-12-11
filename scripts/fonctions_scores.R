score = function(y, ychap){
  rmse = sqrt(mean((y - ychap)^2))
  mae = sum(abs(y - ychap)) / length(y)
  r_square = 1 - sum((ychap - y)^2) / sum((y - mean(y))^2)
  Sc = c(rmse, mae, r_square)
  names(Sc) = c("RMSE", "MAE", "RSQUARE")
  score = round(Sc,2)
}

###########

scorem = function(y, ypred, noms.model){
  npred = ncol(ypred)
  SC = NULL
  for (j in 1:npred){
    SC = cbind(SC, score(y, ypred[,j]))
  }
  colnames(SC) = noms.model 
  scorem = SC
}

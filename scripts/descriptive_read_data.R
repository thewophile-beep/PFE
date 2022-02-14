numeric_var <- c()
factor_var <- c()
for (i in names(data.read)) {
  if (class(data.read[[i]]) %in% c("numeric", "integer")) {
    numeric_var <- c(numeric_var, i)
  }
  else {
    factor_var <- c(factor_var, i)
  }
}

statbase = NULL
Std = NULL
for (var in numeric_var) {
  statbase = rbind(statbase, summary(data.read[,var]))
  Std = c(Std, sd(data.read[,var], na.rm=T))
}
rownames(statbase) = numeric_var
statbase = cbind(statbase, "Std."=Std)
xtable(statbase, type='latex')

var = "RainTomorrow"
round(table(data.read[,var], useNA = "always") / length(data.read[,var]) * 100, 2)

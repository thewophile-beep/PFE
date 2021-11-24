location_ohe <- dcast(data = data, ID ~ Location, length)[-1]
windgustdir_ohe <- dcast(data = data, ID ~ WindGustDir, length)[-1]
names(windgustdir_ohe) <- paste("WindGustDir",names(windgustdir_ohe),sep="")
winddir9am_ohe <- dcast(data = data, ID ~ WindDir9am, length)[-1]
names(winddir9am_ohe) <- paste("WindDir9am",names(winddir9am_ohe),sep="")
winddir3pm_ohe <- dcast(data = data, ID ~ WindDir3pm, length)[-1]
names(winddir3pm_ohe) <- paste("WindDir3pm",names(winddir3pm_ohe),sep="")

data_ohe <- data[c(-2,-6,-7,-8,-10,-11,-18,-19)]
data_ohe <- cbind(data_ohe, windgustdir_ohe, winddir9am_ohe, winddir3pm_ohe)
data_ohe <- drop_na(data_ohe)

data = data_ohe

for (i in names(data))
  if (class(data[[i]]) %in% c("numeric", "integer"))
    numeric_names <- c(numeric_names, i)
  
mod.kmeans <- kmeans(data[numeric_names],49,iter.max=10,nstart=1)
mod.kmeans$size
unique(mod.kmeans$cluster)

hist(mod.kmeans$cluster)
cl1 <- data[mod.kmeans$cluster == 1,]

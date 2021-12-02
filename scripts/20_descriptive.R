str(data)

# statbse of numerical and factorial variables ----
statbase <- NULL
numeric_names <- c()
other_names <- c()
for (i in names(data)) {
  if (class(data[[i]]) %in% c("numeric", "integer")) {
    statbase <- cbind(statbase, c(round(summary(data[[i]]), 2), "sd"=round(sd(data[[i]],na.rm=T), 2)))
    numeric_names <- c(numeric_names, i)
  }
  else {
    other_names <- c(other_names, i)
  }
}
attr(statbase,"dimnames")[[2]] <- numeric_names

print("Summary of numerical values : ")
print(statbase)

lvls <- list()
for (i in other_names) {
  lvls[i] <- list(as.matrix(summary(data[[i]])))
}

# Nb of obs per cities & missmap ----

cities_obs <- data.frame(lvls$Location, check.names=T)
pdf(paste(plots_path,"hist_observations_cities.pdf"), width=15, height=8)
ggplot(data, aes(x=Location)) + 
  geom_bar(stat='count',aes(fill=..count..)) + 
  scale_fill_gradient(low="midnightblue", high="lightslateblue") +
  geom_text(stat='count', aes(label=..count..), angle=90, hjust=1.5, color="white") + 
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5), 
        legend.position="none") +
  labs(title="Nombre d'observations par ville",y="Nombre d'observations",x="Ville")
dev.off()
# Cities observed
unique(data$Location)

missmap(data)

# if we drop evaporation, sunshine and clouds we have :
dropped_data <- drop_na(data[c(-6:-7,-18:-19)])
sprintf("En retirant les nuages, l'évaporation et le soleil, on se retrouve avec %.1f%% des observations, soit %i observations", (dim(dropped_data)[1] / dim(data)[1]*100), dim(dropped_data)[1])
missmap(data[c(-6:-7,-18:-19)])


# Example with Albury
# data_Albury <- data[data$Location=="Albury",]
# plot_Albury_Rainfall_Year <- ggplot(data_Albury, aes(x=Day, y=Rainfall, na.rm=T, color=Year)) +
#   geom_bar(stat = "identity") +
#   facet_wrap( ~ Year)
# plot_Albury_Rainfall_Year
# 
# plot_Albury_Rainfall_Daily <- ggplot(data_Albury, aes(x=Day, y=MinTemp, group=Day, color=Day, na.rm=T)) +
#   geom_point()  
# plot_Albury_Rainfall_Daily

# Correlation between variables ----
correlations <- cor(data[numeric_names],use="na.or.complete")
pdf(paste(plots_path,"corr.pdf"),width=8,height=8)
ggcorrplot(
  correlations,
  method="circle",
  title="Corrélations entre les différentes variables",
  colors=c("darkcyan","white","brown4"),
  typ="upper",
  lab=T,
  lab_size=2
)
dev.off()  

ggplot(data, aes(x=WindGustDir)) +
  geom_bar()

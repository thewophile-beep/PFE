str(data.raw)

# statbse of numerical and factorial variables ----
statbase <- NULL
numeric_names <- c()
other_names <- c()
for (i in names(data.raw)) {
  if (class(data.raw[[i]]) %in% c("numeric", "integer")) {
    statbase <- cbind(statbase, c(round(summary(data.raw[[i]]), 2), "sd"=round(sd(data.raw[[i]],na.rm=T), 2)))
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
  lvls[i] <- list(as.matrix(summary(data.raw[[i]])))
}

# Nb of obs per cities & missmap ----

cities_obs <- data.frame(lvls$Location, check.names=T)
pdf(paste(plots_path,"hist_observations_cities.pdf",sep=""), width=15, height=8)
ggplot(data.raw, aes(x=Location)) + 
  geom_bar(stat='count',aes(fill=..count..)) + 
  scale_fill_gradient(low="midnightblue", high="lightslateblue") +
  geom_text(stat='count', aes(label=..count..), angle=90, hjust=1.5, color="white") + 
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5), 
        legend.position="none") +
  labs(title="Nombre d'observations par ville",y="Nombre d'observations",x="Ville")
dev.off()
# Cities observed
summary(data.raw %>% filter(Location == "Uluru") %>% select(Date))
summary(data.raw %>% filter(Location == "Katherine") %>% select(Date))
summary(data.raw %>% filter(Location == "Nhil") %>% select(Date))

# Direction du vent
ggplot(data.raw, aes(x=WindGustDir)) +
  geom_bar()
ggplot(data.raw, aes(x=WindDir9am)) +
  geom_bar()
ggplot(data.raw, aes(x=WindDir3pm)) +
  geom_bar()

ggplot() +
  geom_line(data = data.raw %>% filter(RainToday == "Yes"), aes(x=Date, y=..count.., color=RainToday))

ggplot(data.raw, aes(x=Date)) + 
  stat_bin(data=data.raw[which(is.na(data.raw$RainToday)),], aes(y=cumsum(..count..)), geom="line") 
ggplot(data.raw, aes(x=Date))+
  stat_bin(data=data.raw[which(is.na(data.raw$RainTomorrow)),], aes(y=cumsum(..count..)), geom="line") 

# if we drop evaporation, sunshine and clouds we have :
dropped_data <- drop_na(data.raw[c(-6:-7,-18:-19)])
sprintf("En retirant les nuages, l'évaporation et le soleil, on se retrouve avec %.1f%% des observations, soit %i observations", (dim(dropped_data)[1] / dim(data.raw)[1]*100), dim(dropped_data)[1])
missmap(data.raw[c(-6:-7,-18:-19)])

# Correlation between variables ----
correlations <- cor(data.raw[numeric_names],use="na.or.complete")
pdf(paste(plots_path,"corr.pdf",sep=""),width=8,height=8)
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


# statbase of data ----
statbase = NULL
Std = NULL
for (var in varlist.num) {
  statbase = rbind(statbase, summary(data.model[,var]))
  Std = c(Std, sd(data.model[,var], na.rm=T))
}
rownames(statbase) = varlist.num
statbase = cbind(statbase, "Std."=Std)
xtable(statbase, type='latex')

var = "RainTomorrow"
round(table(data.model[,var], useNA = "always") / length(data.model[,var]) * 100, 2)

# Nb of obs per cities & missmap of raw (after changing) ----
cities_obs <- data.frame(coords$Location, check.names=T)
ggplot(na.omit(data.raw), aes(x=Location)) + 
  geom_bar(stat='count',aes(fill=..count..)) + 
  scale_fill_gradient(low="midnightblue", high="lightslateblue") +
  geom_text(stat='count', aes(label=..count..), angle=90, hjust=1.5, color="white") + 
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5), 
        legend.position="none") +
  labs(title="Nombre d'observations par ville",y="Nombre d'observations",x="Ville")

ggplot() +
  geom_line(data = data.raw %>% filter(RainToday == "Yes"), aes(x=Date, y=..count.., color=RainToday))

ggplot(data.raw, aes(x=Date)) + 
  stat_bin(data=data.raw[which(is.na(data.raw$RainToday)),], aes(y=cumsum(..count..)), geom="line") 
ggplot(data.raw, aes(x=Date))+
  stat_bin(data=data.raw[which(is.na(data.raw$RainTomorrow)),], aes(y=cumsum(..count..)), geom="line") 

# Correlation between variables of raw ----
correlations <- cor(data.model %>% select(-all_of(varlist.not.num)),use="na.or.complete")
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

# Plotting continuous against continuous ----
data.model %>%
  ggplot(aes(x=Latitude, y=Temp9am)) +
    geom_smooth()

# Plotting factor variables against all others ----
# varlist.not.num
# [1] "RainTomorrow" "RainToday"    "Climate"      "Season"       "WindDir9am"   "WindDir3pm"   "WindGustDir" 
data.model %>%
  select(-varlist.not.num[c(2,3,4)]) %>%
  mutate(RainTomorrow = as.factor(RainTomorrow)) %>%
  gather(-RainTomorrow, key = "var", value = "value") %>%
  ggplot(aes(x = RainTomorrow, y = value, color = RainTomorrow)) +
    geom_boxplot() +
    facet_wrap(~ var, scales = "free") +
    theme(legend.position = "none") +
    ggtitle(paste("Boxplot des variables continues en fonction de RainTomorrow"))

# Periodicity (data.season) ----
data.season <- data.frame(
  data.model %>%
    group_by(Longitude, Latitude, Season, Climate) %>%
    summarise(
      MinTemp = round(mean(MinTemp, na.rm=T),2),
      MaxTemp = round(mean(MaxTemp, na.rm=T),2),
      Rainfall = mean(Rainfall, na.rm=T),
      MeanTemp = mean(cbind(Temp9am, Temp3pm), na.rm=T),
      Humidity9am = mean(Humidity9am, na.rm=T),
      Humidity3pm = mean(Humidity3pm, na.rm=T)
    )
  )

{
  for (c in climates) {
    # tmp.name = sprintf("%sTemp_and_Rainfall%s.pdf",plots_path,c)
    # pdf(tmp.name)
    tmp.cities = coords$Location[coords$Climate == c]
    for (i in tmp.cities) {
      print(
        ggplot(data = data.season %>% filter(Location == i), aes(x = Season))+
          geom_col(aes(y=MeanTemp, group=3, fill="Température Moyenne")) +
          geom_col(aes(y=Rainfall, fill="Rainfall")) +
          geom_text(aes(label=MaxTemp, y=MaxTemp), vjust=-.5) +
          geom_text(aes(label=MinTemp, y=MinTemp), vjust=1.5) +
          geom_errorbar(aes(y=MeanTemp, ymax=MaxTemp, ymin=MinTemp), width = 0.25) +
          labs(x="Season", y="Temperature", title=paste(i,c,sep=" : "), fill="Barplots", colour="Temperature") +
          scale_fill_manual(values = c("lightblue", "lightsalmon1")) +
          theme(legend.position="right") + 
          scale_y_continuous(name="Température (°C)", sec.axis=sec_axis(~.*1, name="Rainfall (mm)"))
      )
    }
    # dev.off()
  }
  }

# k-means climate (pivot_wider) ----
tmp.df = pivot_wider(data.season, names_from=c(Season), values_from=c(Rainfall, MinTemp, MaxTemp, MeanTemp, Humidity9am, Humidity3pm))

tmp.kmeans <- kmeans(tmp.df %>% select(-c(Climate)), centers = 5, nstart = 10)

print(ggplot(data = world) +
        geom_sf(fill="antiquewhite1") +
        geom_sf(data=states, fill=NA) +
        
        geom_point(data=tmp.df, aes(x=Longitude, y=Latitude, fill=as.factor(tmp.kmeans$cluster)), size=3, shape=23)+
        coord_sf(xlim = c(112, 170), ylim = c(-47, -8), expand = T) +
        
        annotation_scale(location = "bl", width_hint = 0.2) +
        annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) +
        theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
              panel.background = element_rect(fill = "aliceblue"),
              text=element_text(size=20))
      + labs(fill="Climates")
)

# completed distrib per cities ----
png(paste(plots_path,"completed_hist_observations_cities.png", sep=""), width=1000, height=500)
ggplot(data, aes(x=Location)) + 
  geom_bar(stat='count', aes(fill=..count..)) + 
  scale_fill_gradient(low="midnightblue", high="lightslateblue") +
  geom_text(stat='count', aes(label=..count..), angle=90, hjust=1.5, color="white") + 
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5), 
        legend.position="none") +
  labs(title="Nombre d'observations par ville", y="Nombre d'observations", x="Ville") +
  scale_x_discrete(limits = cities)
dev.off()

# pca data model ----
pca_res <- prcomp(data.model %>% select(-all_of(varlist.not.num)), scale. = TRUE)
autoplot(pca_res, x=2, y=3,
         data=data.model, 
         colour="RainTomorrow",
         loadings = TRUE, loadings.colour = 'royalblue',
         loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour = "black") +
  labs(title="ACP des données")

data = na.omit(data.completed)

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


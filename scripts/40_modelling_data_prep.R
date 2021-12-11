data <- na.omit(data.completed[c(-25,-26,-27)])
data$RainToday <- as.numeric(data$RainToday) - 1
data$RainTomorrow <- as.numeric(data$RainTomorrow) - 1

# Construction des échantillons App et test
# Réinitialisation du générateur aléatoire
set.seed(1111)
# Extraction des échantillons
test.ratio=.25 # part de l’échantillon test
npop=nrow(data) # nombre de lignes dans les données
ntest=ceiling(npop*test.ratio) # taille de l’échantillon test
testi=sample(1:npop,ntest) # indices de l’échantillon test
appri=setdiff(1:npop,testi) # indices complémentaires de l’échant. d’apprentissa
# Construction des échantillons avec les variables explicatiues .
dataApp=data[appri,] # construction de l’échantillon d’apprentissage
dataTest=data[testi,] # construction de l’échantillon test

y.app = dataApp$RainTomorrow
y.test = dataTest$RainTomorrow

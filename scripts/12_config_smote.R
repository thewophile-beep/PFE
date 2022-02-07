data.smote = data %>% select(-c(Date, Location))
data.smote$RainToday <- as.numeric(data.smote$RainToday) - 1
data.smote$RainTomorrow <- as.numeric(data.smote$RainTomorrow) - 1
data.smote = SMOTE(data.smote, data.smote$RainTomorrow)

data.model = data.smote$data

rm(list=ls())  # Efface les variables créées lors des exécutions précédentes
graphics.off() # Ferme les fenêtres ouvertes lors des exécutions précédentes

# Packages
library(corrplot)
library(ade4)
library(lubridate)  # Convertir données temporelles en secondes
library(car)  # fonction vif() et anova()

# Import data
data <- read.table(
  file = "ASA_data_projet.txt",
  stringsAsFactors = T,
  header = T,
  na.strings = "NA",
  dec = ",",
  sep = ""
)

str(data)



# Rename colnames ---------------------------------------------------------
colnames(data)[1:22] <- c(
  "ID",
  "eclosion_day",
  "sex",
  "FMR_day",
  "age_FMR",
  "mass",
  "FMR",
  "flight_day",
  "age_flight",
  "sun_flight",
  "wind_flight",
  "air_T",
  "dew_flight",
  "humidity_flight",
  "solar_radiation_flight",
  "flight_duration",
  "disturbed_flight",
  "stop_flight",
  "takeoff_time",
  "takeoff_T_thorax",
  "end_flight_time",
  "y"
)

# Data cleaning -----------------------------------------------------------

# On supprime la colonne "ID" inutile
data <- data[, !colnames(data) %in% "ID"]

# Conversion des variables "heures" en seconde
# data$takeoff_time <-  format(as.POSIXct(data$takeoff_time, format = "%H:%M:%S"), format = "%H:%M:%S")
# data$end_flight_time <-  format(as.POSIXct(data$end_flight_time, format = "%H:%M:%S"), format = "%H:%M:%S")

# --- Extraire les heures, les minutes et les secondes
for (variable_name in c("takeoff_time", "end_flight_time")){
  
  data[, variable_name] <- hms(data[, variable_name])
  heures <- hour(data[, variable_name])
  minutes <- minute(data[, variable_name])
  secondes <- second(data[, variable_name])
  
  # Convertir les heures, les minutes et les secondes en secondes
  data[, variable_name] <- heures * 3600 + minutes * 60 + secondes
}



# Conversion des variables "facteurs"
data$wind_flight <- as.factor(data$wind_flight)
data$sun_flight <- as.factor(data$sun_flight)
data$age_FMR <- as.factor(data$age_FMR)


# Identification des données manquantes (en %)
data_na <- apply(X = data, MARGIN = 2, FUN = function(x) round(sum(is.na(x)) / nrow(data) * 100, 2))
sort(data_na)  # On trie les données par ordre croissant

# --- On supprime les variables avec plus de 30% de données manquantes
data <- data[, !colnames(data) %in% names(data_na[data_na>30])]





# Data visualisation (distribution & outliers) ------------------------------------------------------
# --- Visualisation de la variable Y
par(mfrow = c(1,3))
hist(data$y, pch = 16, col = 'blue', xlab = "Mean temperature", main = "")
dotchart(data$y, pch = 16, col = 'blue')
qqnorm(data$y, pch = 16, col = 'blue')
qqline(data$y, col = 'red')

# ---- Visualisation des distribution des X
# distribution des facteurs
data_factor <- data[,sapply(data, is.factor)]  # factor columns identification

par(mfrow = c(2,4))
i <- 1
apply(X = data_factor, MARGIN = 2, function(x){
  
  bar <- barplot(table(x), main = colnames(data_factor)[i], cex.main = .8)
  bar
  text(x = bar, y = table(x) - 1, label = table(x), pos = 3)  # Effectifs par catégorie
  
  i <<- i + 1    
}
)

# --- On relevel sun_flight et wind_flight en ajoutant la modalité ("<2")
data$sun_flight <- ifelse(test = data$sun_flight==1 | data$sun_flight == 2, yes = "<2", no = data$sun_flight)
data$wind_flight <- ifelse(test = data$wind_flight==1 | data$wind_flight == 2, yes = "<2", no = data$wind_flight)
data$sun_flight  <- as.factor(data$sun_flight)
data$wind_flight <- as.factor(data$wind_flight)



# distribution des covariables
data_num <- data[,sapply(data, is.numeric)]  # factor columns identification

par(mfrow = c(2,4))
i <- 1
apply(X = data_num, MARGIN = 2, function(x){
  
  hist(x, main = colnames(data_num)[i])
  qqnorm(x, pch = 16)
  qqline(x, col = 'red')
  
  i <<- i + 1    
}
)


# --- Visualisation des distribution des y~X
# Facteurs
par(mfrow = c(2,4))
i <- 1
apply(X = data_factor, MARGIN = 2, function(x){
  
  boxplot(
    data$y ~ x,
    ylab = "Y",
    xlab = "",
    main = colnames(data_factor)[i], 
    ylim = c(
      min(data$y, na.rm = T) - 1,
      max(data$y  + 1, na.rm = T)
    )
  )
  i <<- i + 1    
}
)



# Covariables
par(mfrow = c(2,2))
i <- 1
apply(X = data_num, MARGIN = 2, function(x){
  
  plot(
    data$y  ~ x,
    ylab = "T",
    xlab = "",
    main = colnames(data_num)[i],
    ylim = c(
      min(data$y, na.rm = T) - 1,
      max(data$y  + 1, na.rm = T)
    )
  )
  abline(lm(data$y  ~ x), col = "red")
  
  i <<- i + 1    
}
)



# --- Interaction entre facteurs et covariables : température de l'air et SNP331 (voir bibliographie)
# Interactions between Latitude & Location
par(mfrow = c(1,1))
plot(
  data$y ~ data$air_T,
  ylim = c(
    min(data$y, na.rm = T) - 1,
    max(data$y  + 1, na.rm = T)
  )
)
points(data$y[data$Pgi_331 == "AA"] ~ data$air_T[data$Pgi_331 == "AA"], pch = 20, cex = 2, col = "red")
points(data$y[data$Pgi_331 == "CA"] ~ data$air_T[data$Pgi_331 == "CA"], pch = 20, cex = 2, col = "blue")
points(data$y[data$Pgi_331 == "CC"] ~ data$air_T[data$Pgi_331 == "CC"], pch = 20, cex = 2, col = "green")




# Colinéarité des variables x -------------------------------------------------------------
M <- cor(na.omit(data_num))
corrplot::corrplot.mixed(M,upper="square",lower.col="black", tl.col="black",cl.cex = 0.8,tl.cex = 0.7,number.cex =0.8)

# Corrélation des variables temporelles
# cor(na.omit(data$takeoff_time ~ data$end_flight_time))

# --- Suppression des variables corrélées (dew point ou humidity ?)
data <- data[!colnames(data) %in% c("eclosion_day", "flight_day", "age_flight", "dew_flight", "stop_flight", "end_flight_time")]





# Modélisation ------------------------------------------------------------
mod <- lm(formula = y ~., data = na.omit(data))
anova(mod)

# --- Multicollinéarité avec test VIF
vif(mod)  #on supprime la variable wind_flight
mod <-
  lm(
    formula = y ~ sex + FMR_day + age_FMR + mass + FMR + sun_flight +
      air_T + humidity_flight + solar_radiation_flight + flight_duration + disturbed_flight +
      takeoff_time + takeoff_T_thorax + Pgi_105 + Pgi_1083 + Pgi_331,
    data = na.omit(data)
  )

vif(mod)  #on supprime la variable sun_flight

mod <-
  lm(
    formula = y ~ sex + FMR_day + age_FMR + mass + FMR  +
      air_T + humidity_flight + solar_radiation_flight + flight_duration + disturbed_flight +
      takeoff_time + takeoff_T_thorax + Pgi_105 + Pgi_1083 + Pgi_331,
    data = na.omit(data)
  )

vif(mod)  #on supprime la variable wind_flight


# --- Selection du meilleur modèle
mod0 <- lm(formula = y ~ 1, data = na.omit(data))
selection_model <- step(object = mod, scope = list(mod0, mod), direction = "both")
selection_model$terms

mod_candidat <- lm(y ~ mass + air_T + flight_duration + disturbed_flight + takeoff_T_thorax, data = na.omit(data))
anova(mod_candidat)
summary(mod_candidat)

# --- Evaluations des résidus
par(mfrow = c(1, 2))
hist(mod_candidat$residuals, col = "blue", breaks = 20)
qqnorm(mod_candidat$residuals,pch=16,col='blue',xlab='')
qqline(mod_candidat$residuals,col='red')

# --- Distance de cook des résidus
par(mfrow = c(1, 1))
plot(cooks.distance(mod_candidat), type = "h", ylim = c(0, 1))
abline(h = 1, col = 2,lwd = 3)

# --- Pouvoir explicatif de chaque variable prédictive
ano_mod <- anova(mod_candidat)
var_expliquee <- round(ano_mod$`Sum Sq`/ sum(ano_mod$`Sum Sq`), 3)  # Ratio de variance expliquée par variable
names(var_expliquee) <- rownames(ano_mod)

bar <- barplot(var_expliquee, ylab = "% variance expliquée", ylim = c(0, .5), cex.names = .8, las = 2)
text(
  x = bar,
  y = var_expliquee + .05,
  labels = paste(var_expliquee * 100, "%"),
  pos = 1,
  cex = 1
)



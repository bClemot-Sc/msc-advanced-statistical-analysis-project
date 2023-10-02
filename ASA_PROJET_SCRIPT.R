rm(list=ls())  # Efface les variables créées lors des exécutions précédentes
graphics.off() # Ferme les fenêtres ouvertes lors des exécutions précédentes

# Packages
library(corrplot)

# Define WD
setwd("C:/Users/p_a_8/OneDrive/Bureau/M2 MODE/COURS/Semestre 3/ASA/PROJET")

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

# Conversion des variables "heures"
data$takeoff_time <-  format(as.POSIXct(data$takeoff_time, format = "%H:%M:%S"), format = "%H:%M:%S")
data$end_flight_time <-  format(as.POSIXct(data$end_flight_time, format = "%H:%M:%S"), format = "%H:%M:%S")

# Conversion des variables "facteurs"
data$wind_flight <- as.factor(data$wind_flight)
data$sun_flight <- as.factor(data$sun_flight)



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
par(mfrow = c(2,4), mar = c(2, 4, 4, .5))
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
corrplot.mixed(M,upper="square",lower.col="black", tl.col="black",cl.cex = 0.8,tl.cex = 0.7,number.cex =0.8)

# Corrélation des variables temporelles
# cor(na.omit(data$takeoff_time ~ data$end_flight_time))

# --- Suppression des variables corrélées (dew point ou humidity ?)
data <- data[!colnames(data) %in% c("eclosion_day", "flight_day", "age_flight", "dew_flight", "stop_flight")]


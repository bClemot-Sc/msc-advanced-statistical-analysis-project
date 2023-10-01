rm(list=ls())  # Efface les variables créées lors des exécutions précédentes
graphics.off() # Ferme les fenêtres ouvertes lors des exécutions précédentes


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



# Data cleaning -----------------------------------------------------------

# On supprime la colonne "ID" inutile
data <- data[, !colnames(data) %in% "Butterfly_ID"]

# Conversion des variables "heures"
data$Flight_takeoff_time_flight1 <-  format(as.POSIXct(data$Flight_takeoff_time_flight1, format = "%H:%M:%S"), format = "%H:%M:%S")
data$flight_end_time_flight1 <-  format(as.POSIXct(data$flight_end_time_flight1, format = "%H:%M:%S"), format = "%H:%M:%S")

# Conversion des variables "facteurs"
data$Wind_flight1 <- as.factor(data$Wind_flight1)
data$Sun_flight1 <- as.factor(data$Sun_flight1)


# Identification des données manquantes (en %)
data_na <- apply(X = data, MARGIN = 2, FUN = function(x) round(sum(is.na(x)) / nrow(data) * 100, 2))
sort(data_na)  # On trie les données par ordre croissant

# --- On supprime les variables avec plus de 30% de données manquantes
data <- data[, !colnames(data) %in% names(data_na[data_na>30])]





# Data visualisation (distribution & outliers) ------------------------------------------------------
# --- Visualisation de la variable Y
par(mfrow = c(1,3))
hist(data$Flight_Tb_average_C_flight1, pch = 16, col = 'blue', xlab = "Mean temperature", main = "")
dotchart(data$Flight_Tb_average_C_flight1, pch = 16, col = 'blue')
qqnorm(data$Flight_Tb_average_C_flight1, pch = 16, col = 'blue')
qqline(data$Flight_Tb_average_C_flight1, col = 'red')

# ---- Visualisation des distribution des X
# distribution des facteurs
data_factor <- data[,sapply(data, is.factor)]  # factor columns identification

par(mfrow = c(2,4))
i <- 1
apply(X = data_factor, MARGIN = 2, function(x){
  
  bar <- barplot(table(x), main = colnames(data_factor)[i])
  bar
  text(x = bar, y = table(x) - .2, label = table(x), pos = 3)  # Effectifs par catégorie
  
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
    data$Flight_Tb_average_C_flight1 ~ x,
    ylab = "Y",
    xlab = "",
    main = colnames(data_factor)[i], 
    ylim = c(
      min(data$Flight_Tb_average_C_flight1, na.rm = T) - 1,
      max(data$Flight_Tb_average_C_flight1  + 1, na.rm = T)
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
    data$Flight_Tb_average_C_flight1  ~ x,
    ylab = "T",
    xlab = "",
    main = colnames(data_num)[i],
    ylim = c(
      min(data$Flight_Tb_average_C_flight1, na.rm = T) - 1,
      max(data$Flight_Tb_average_C_flight1  + 1, na.rm = T)
    )
  )
  abline(lm(data$Flight_Tb_average_C_flight1  ~ x), col = "red")
  
  i <<- i + 1    
}
)



# --- Interaction entre facteurs et covariables : température de l'air et SNP331 (voir bibliographie)
# Interactions between Latitude & Location
par(mfrow = c(1,1))
plot(
  data$Flight_Tb_average_C_flight1 ~ data$Air_temperature_C_flight1,
  ylim = c(
    min(data$Flight_Tb_average_C_flight1, na.rm = T) - 1,
    max(data$Flight_Tb_average_C_flight1  + 1, na.rm = T)
  )
)
points(data$Flight_Tb_average_C_flight1[data$Pgi_331 == "AA"] ~ data$Air_temperature_C_flight1[data$Pgi_331 == "AA"], pch = 20, cex = 2, col = "red")
points(data$Flight_Tb_average_C_flight1[data$Pgi_331 == "CA"] ~ data$Air_temperature_C_flight1[data$Pgi_331 == "CA"], pch = 20, cex = 2, col = "blue")
points(data$Flight_Tb_average_C_flight1[data$Pgi_331 == "CC"] ~ data$Air_temperature_C_flight1[data$Pgi_331 == "CC"], pch = 20, cex = 2, col = "green")

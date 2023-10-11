#### Script pour imputer les NAs avec le package missMDA #####
library(missMDA)
library(lubridate)


# Importation et preparation des donnees -------------------------------------------------
# Importation
data <-
  read.table(
    file = "ASA_data_projet.txt",
    stringsAsFactors = T,
    header = T,
    na.strings = "NA",
    dec = ",",
    sep = ""
  )
# Renommer les colonnes
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
# On supprime la colonne "ID" inutile
data <- data[, !colnames(data) %in% "ID"]
# On extraie les heures, minutes et secondes
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


# Rappel des variables avec NAs -------------------------------------------
# Calcule des % de NAs
data_na <- apply(X = data, MARGIN = 2, FUN = function(x) round(sum(is.na(x)) / nrow(data) * 100, 2))
sort(data_na)


# Imputation avec FAMD --------------------------
# Application de la fonction
imputed_data <- imputeFAMD(data)
# Recuperation du df avec les NAs imputes
data <- imputed_data$completeObs
# Re-visualisation des NAs
data_na <- apply(X = new_df, MARGIN = 2, FUN = function(x) round(sum(is.na(x)) / nrow(data) * 100, 2))
sort(data_na)


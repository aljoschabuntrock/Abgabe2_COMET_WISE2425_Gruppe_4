### Abgabe 2

## Aufgabe 3
# Aufgabe 3.1
n<-8
n_Simulationen<-10000
kein_eigenes_Geschenk <- function(permutation) {
  !any(permutation == 1:length(permutation))
}
Anzahl_mit_eigenem_Geschenk <- 0

for (i in 1:n_Simulationen) {
  Geschenke <- sample(1:n)
  if (kein_eigenes_Geschenk(Geschenke)) {
    Anzahl_mit_eigenem_Geschenk <- Anzahl_mit_eigenem_Geschenk + 1
  }
}

Wahrscheinlichkeit <- Anzahl_mit_eigenem_Geschenk / n_Simulationen
print(Wahrscheinlichkeit)

# Aufgabe 3.2
wichtel_unglueck <- function(n, k, iterationen = 1e6) {
  anzahl_eigene_geschenke <- function(permutation) {
    sum(permutation == 1:length(permutation))
  }
  
  anzahl_erfolge <- 0
  
  for (i in 1:iterationen) {
    geschenke <- sample(1:n)
    
    eigene_geschenke <- anzahl_eigene_geschenke(geschenke)
    
    if (eigene_geschenke <= k) {
      anzahl_erfolge <- anzahl_erfolge + 1
    }
  }
  
  wahrscheinlichkeit <- anzahl_erfolge / iterationen
  return(wahrscheinlichkeit)
}

wahrscheinlichkeit <- wichtel_unglueck(8, 2)

# Aufgabe 3.3
# Funktion, die die Wahrscheinlichkeit berechnet, dass maximal k Personen ihr eigenes Geschenk bekommen
wichtel_unglueck <- function(n, k, iterationen = 1e6) {
  
  # Hilfsfunktion, die die Anzahl der Personen zählt, die ihr eigenes Geschenk bekommen
  anzahl_eigene_geschenke <- function(permutation) {
    sum(permutation == 1:length(permutation))  # Zählt, wie oft eine Person ihr eigenes Geschenk bekommt
  }
  
  # Zähler für die Anzahl der erfolgreichen Simulationen (weniger als oder gleich k eigene Geschenke)
  anzahl_erfolge <- 0
  
  # Durchführung der Simulationen
  for (i in 1:iterationen) {
    geschenke <- sample(1:n)  # Zufällige Zuweisung der Geschenke (Permutation der Zahlen 1 bis n)
    
    eigene_geschenke <- anzahl_eigene_geschenke(geschenke)  # Berechnung der Anzahl der Personen mit eigenem Geschenk
    
    if (eigene_geschenke <= k) {  # Prüfen, ob die Anzahl der eigenen Geschenke ≤ k ist
      anzahl_erfolge <- anzahl_erfolge + 1  # Falls ja, inkrementiere den Erfolgzähler
    }
  }
  
  # Berechnung der Wahrscheinlichkeit, dass maximal k Personen ihr eigenes Geschenk bekommen
  wahrscheinlichkeit <- anzahl_erfolge / iterationen
  return(wahrscheinlichkeit)  # Rückgabe der berechneten Wahrscheinlichkeit
}

# Berechnung der Wahrscheinlichkeit für n = 8 Personen und maximal 2 eigene Geschenke
wahrscheinlichkeit <- wichtel_unglueck(8, 2)

# Ausgabe der berechneten Wahrscheinlichkeit
wahrscheinlichkeit

# Aufgabe 3.4
library(testthat)

# Test 1: Wahrscheinlichkeit für n = 1 (eine Person) und k = 1
test_that("Test für n = 1, k = 1", {
  expect_equal(wichtel_unglueck(1, 1), 1)  # Bei nur einer Person bekommt diese immer ihr eigenes Geschenk
})

# Test 2: Wahrscheinlichkeit für n = 2 und k = 0 (niemand bekommt sein eigenes Geschenk)
test_that("Test für n = 2, k = 0", {
  expect_equal(wichtel_unglueck(2, 0), 0.5, tolerance = 0.1)  # Es gibt 2 Permutationen: 1 für niemanden eigenes Geschenk, 1 für beide
})

# Test 3: Wahrscheinlichkeit für n = 3 und k = 1 (maximal eine Person bekommt ihr eigenes Geschenk)
test_that("Test für n = 3, k = 1", {
  result <- wichtel_unglueck(3, 1)
  expect_true(result > 0 && result < 1)  # Die Wahrscheinlichkeit sollte zwischen 0 und 1 liegen
})

# Test 4: Wahrscheinlichkeit für n = 10 und k = 0 (niemand bekommt sein eigenes Geschenk)
test_that("Test für n = 10, k = 0", {
  result <- wichtel_unglueck(10, 0)
  expect_true(result > 0 && result < 1)  # Die Wahrscheinlichkeit sollte zwischen 0 und 1 liegen
  expect_false(result == 0)  # Sie sollte nicht gleich 0 sein, da es einige Permutationen ohne eigene Geschenke gibt
})

# Aufgabe 3.5
data <- read.csv(
  file = "/Users/aljoschabuntrock/Documents/Studium/Drittes Semester/Compt. M./Abgabe 2 COMET/bike_sharing_data_(with_NAs).csv", # Pfad zur .csv-Datei
  header = TRUE, # die Datei hat eine Kopfzeile
  sep = ",", # die Zellen sind durch Kommata getrennt
  quote = "\""
)
head(data)
subsec
data_group_4 <- subset(data, group == 4)
write.csv(data_group_4)
colnames(data)

anyNA(data_group_4)
sum(is.na(data_group_4))
colSums(is.na(data_group_4))
data_group_4[!complete.cases(data_group_4), ]
write.csv(data_group_4, file = "group_4_data.csv", row.names = FALSE)
which(is.na(data_group_4), arr.ind = TRUE)
## NAs werden eliminiert##
##day of year##
data_group_4[182,4] <- 182
data_group_4[217,4] <- 217
##day of week##
data_group_4[108,5] <- 3
data_group_4[299,5] <- 5
##month of year##
data_group_4[57,6] <- 2
data_group_4[184,6] <- 7
##precipation+windspeed##
data_group_4[is.na(data_group_4[, 7]), 7] <- 0
data_group_4[is.na(data_group_4[, 8]), 8] <- 0
##aim,average,max Temp##
# Mittelwert einer Spalte berechnen (NA-Werte ignorieren)
mean_value_min <- mean(data_group_4$min_temperature, na.rm = TRUE)

# NA-Werte in der Spalte durch den Mittelwert ersetzen
data_group_4[251,9]<- mean_value_min
##für avg##
mean_value_avg <- mean(data_group_4$average_temperature, na.rm = TRUE)
data_group_4[289,10] <- mean_value_avg
##für max##
mean_value_max <- mean(data_group_4$max_temperature, na.rm = TRUE)
data_group_4[13,11] <- mean_value_max
##count NA->durch mittelwert aus Tag davor und danach##
count_neu <- (333+206)/2 
data_group_4[175,12] <- count_neu
##datenanomalien##
##Minuswerte in precipation,windspeed und count##
# Alle negativen Werte in column_name durch 0 ersetzen
data_group_4$precipitation[data_group_4$precipitation < 0] <- 0
data_group_4$windspeed[data_group_4$windspeed < 0] <- 0
data_group_4$count[data_group_4$count < 0] <- 0
##Minuswerte bei Temp##
which(data_group_4$min_temperature < 0)
which(data_group_4$average_temperature < 0)
which(data_group_4$max_temperature < 0)
data_group_4[242,9] <- 65
data_group_4[179,10] <- 75
data_group_4[101,11] <- 67
##Fahrenheit in Celcius##
temperature_converter <- function(
    temp, from_unit = "F", to_unit = "C", digits = 2
) {
  if (from_unit == "F" && to_unit == "C") {
    temp_conv <- (temp - 32) * 5/9
  } else if (from_unit == "C" && to_unit == "F") {
    temp_conv <- (temp * 9/5) + 32
  } else {
    stop("Invalid units.")
  }
  return(round(temp_conv, digits = digits))
}

data_group_4$min_temperature <- temperature_converter(data_group_4$min_temperature)
data_group_4$average_temperature <- temperature_converter(data_group_4$average_temperature)
data_group_4$max_temperature <- temperature_converter(data_group_4$max_temperature)
##Bestimmen Sie den Monat mit der h¨ ochsten Gesamtanzahl ausgeliehener Fahrr¨ ader.##
data_group_4_januar <- subset(data_group_4, month_of_year == 1)
data_group_4_februar <- subset(data_group_4, month_of_year == 2)
data_group_4_märz <- subset(data_group_4, month_of_year == 3)
data_group_4_april <- subset(data_group_4, month_of_year == 4)
data_group_4_mai <- subset(data_group_4, month_of_year == 5)
data_group_4_juni <- subset(data_group_4, month_of_year == 6)
data_group_4_juli <- subset(data_group_4, month_of_year == 7)
data_group_4_august <- subset(data_group_4, month_of_year == 8)
data_group_4_september <- subset(data_group_4, month_of_year == 9)
data_group_4_oktober <- subset(data_group_4, month_of_year == 10)
data_group_4_november <- subset(data_group_4, month_of_year == 11)
data_group_4_december <- subset(data_group_4, month_of_year == 12)
head(data_group_4)
monthly_totals <- aggregate(count ~ month_of_year, data = data_group_4, sum)

top_month <- monthly_totals[which.max(monthly_totals$count), ]
print(top_month)

## Aufgabe 4
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("gapminder")
library(gapminder)
data(gapminder)

# Aufgabe 4.2
#a
ggplot(data_group_4, aes(x = average_temperature, y = count)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Zusammenhang zwischen Temperatur und Anzahl ausgeliehener Fahrräder",
    subtitle = "Schwarze Linie = Korrelation | Blaue Punkte = Ausprägungen",
    x = "Temperatur (°C)",
    y = "Anzahl ausgeliehener Fahrräder"
  ) +
  theme_minimal()

#b
data_group_4$date <- as.Date(data_group_4$date)
ggplot(data_group_4, aes(x = precipitation, y = count)) + 
  geom_point(color = "blue", alpha = 0.6) +  
  labs(
    title = "Zusammenhang zwischen Niederschlag und Anzahl ausgeliehener Fahrräder",
    subtitle = "Blaue Punkte = Ausprägungen",
    x = "Niederschlag (mm)",
    y = "Anzahl ausgeliehener Fahrräder"
  ) +
  theme_minimal()
cor(data_group_4$precipitation, data_group_4$count, use = "complete.obs")  
8.05
which(data_group_4$precipitation == 8.05 , arr.ind = TRUE)
data_group_4[272,7] <- NA

#c
ggplot(data_group_4, aes(x = windspeed, y = count)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Zusammenhang zwischen Windgeschwindigkeit und Anzahl ausgeliehener Fahrräder",
    subtitle = "Schwarze Linie = Korrelation | Blaue Punkte = Ausprägungen",
    x = "Windgeschwindigkeit",
    y = "Anzahl ausgeliehener Fahrräder"
  ) +
  theme_minimal()

#d
ggplot(data_group_4, aes(x = date, y = count)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Zusammenhang zwischen Zeit und Anzahl ausgeliehener Fahrräder",
    subtitle = "Schwarze Linie = Korrelation | Blaue Punkte = Ausprägungen",
    x = "Tag",
    y = "Anzahl ausgeliehener Fahrräder"
  ) +
  theme_minimal()

# Aufgabe 4.3
data_niederschlag <- data_group_4[data_group_4$precipitation > 0, ]
data_kein_niederschlag <- data_group_4[data_group_4$precipitation == 0, ]

##für mit regen##
ggplot(data_niederschlag, aes(x = average_temperature, y = count)) + 
  geom_point(color = "blue", alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Zusammenhang zwischen Temperatur und Verleihzahlen (Regen)",
    subtitle = "Schwarze Linie = Korrelation | Blaue Punkte = Ausprägungen",
    x = "Temperatur (°C)",
    y = "Anzahl ausgeliehener Fahrräder"
  ) +
  theme_minimal()

##für ohne regen##
##für ohne regen##
ggplot(data_kein_niederschlag, aes(x = average_temperature, y = count)) + 
  geom_point(color = "blue", alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Zusammenhang zwischen Temperatur und Verleihzahlen (ohen Regen)",
    subtitle = "Schwarze Linie = Korrelation | Blaue Punkte = Ausprägungen",
    x = "Temperatur (°C)",
    y = "Anzahl ausgeliehener Fahrräder"
  ) +
  theme_minimal()


# Aufgabe 4.4
ggplot(data_group_4, aes(x = count)) + 
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Verteilung der Anzahl ausgeliehener Fahrräder",
    subtitle = "Blaue Säulen = Häufigkeit verschiedener Ausleihungsmengen im Beobachtungszeitraum",
    x = "Anzahl ausgeliehener Fahrräder",
    y = "Häufigkeit"
  ) +
  theme_minimal()

ggplot(data_group_4, aes(x = average_temperature)) + 
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(
    title = "Verteilung der Temperatur (°C)",
    subtitle = "Rote Säulen = Häufigkeit verschiedener auftretener Temperaturen im Beobachtungszeitraum",
    x = "Temperatur (°C)",
    y = "Häufigkeit"
  ) +
  theme_minimal()

ggplot(data_group_4, aes(x = precipitation)) + 
  geom_histogram(binwidth = 0.1, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(
    title = "Verteilung der Niederschlagsmenge (mm)",
    subtitle = "Grüne Säulen = Häufigkeit verschiedener Niederschlagsmengen im Beobachtungszeitraum",
    x = "Niederschlag (mm)",
    y = "Häufigkeit"
  ) +
  theme_minimal()

ggplot(data_group_4, aes(x = windspeed)) + 
  geom_histogram(binwidth = 1, fill = "gold", color = "black", alpha = 0.7) +
  labs(
    title = "Verteilung der Windgeschwindigkeit (km/h)",
    subtitle = "Goldene Säulen = Häufigkeit verschiedener Windgeschwindigkeiten im Beobachtungszeitraum",
    x = "Windgeschwindigkeit (km/h)",
    y = "Häufigkeit"
  ) +
  theme_minimal()


# Aufgabe 4.5
#Datum nach Jahreszeiten aufteilen#
data_group_4$season <- case_when(
  data_group_4$month_of_year %in% c(3, 4, 5) ~ "Frühling",
  data_group_4$month_of_year %in% c(6, 7, 8) ~ "Sommer",
  data_group_4$month_of_year %in% c(9, 10, 11) ~ "Herbst",
  data_group_4$month_of_year %in% c(12, 1, 2) ~ "Winter",
  TRUE ~ NA_character_  
)

##Grafik erstellen##
ggplot(data_group_4, aes(x = count, fill = season)) + 
  geom_density(alpha = 0.2) +  # Transparenz für Überlagerung
  labs(
    title = "Verteilung der Anzahl ausgeliehener Fahrräder nach Jahreszeit",
    x = "Anzahl ausgeliehener Fahrräder",
    y = "Dichte"
  ) +
  scale_fill_manual(
    values = c(
      "Frühling" = "green",
      "Sommer" = "red",
      "Herbst" = "orange",
      "Winter" = "blue"
    )
  ) +
  theme_minimal()

# Aufgabe 4.6
install.packages("plotly")
library(plotly)
"3D_Grafik" <- plot_ly(
  data = data_group_4,
  x = ~average_temperature,
  y = ~windspeed,
  z = ~count,
  color = ~count,
  colors = c("#efedf5", "#bcbddc", "#756bb1"),
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 5, opacity = 0.8)
)
`3D_Grafik`%>% layout(scene = list(xaxis = list(title = "Temperatur in C°"),
                                   yaxis = list(title = "Windgeschwindigkeit"),
                                   zaxis = list(title = "Anzahl ausgeliehener Fahrräder")))


## Für die Abgabeaufgabe 

bike$data <- as.Date(bike$data)


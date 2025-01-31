# ----------------- COMET_Abgabe_2 -----------------

# ------------------- Aufgabe 3 --------------------

# ------------------- Aufgabe 3.1 -------------------
# Anzahl der Personen/Geschenke
n <- 8

# Anzahl der Simulationen, die durchgeführt werden
n_Simulationen <- 10000

# Funktion, die überprüft, ob jemand sein eigenes Geschenk erhält
kein_eigenes_Geschenk <- function(permutation) {
  # Prüft, ob es ein Element gibt, das an seinem ursprünglichen Platz bleibt
  !any(permutation == 1:length(permutation))
}

# Zähler für die Fälle, in denen niemand sein eigenes Geschenk erhält
Anzahl_mit_eigenem_Geschenk <- 0

# Schleife für die Monte-Carlo-Simulation
for (i in 1:n_Simulationen) {
  # Erstelle eine zufällige Permutation der Geschenke
  Geschenke <- sample(1:n)
  
  # Überprüfe, ob niemand sein eigenes Geschenk erhält
  if (kein_eigenes_Geschenk(Geschenke)) {
    # Falls ja, erhöhe den Zähler
    Anzahl_mit_eigenem_Geschenk <- Anzahl_mit_eigenem_Geschenk + 1
  }
}

# Berechnung der Wahrscheinlichkeit, dass niemand sein eigenes Geschenk erhält
Wahrscheinlichkeit <- Anzahl_mit_eigenem_Geschenk / n_Simulationen

# Ausgabe der Wahrscheinlichkeit
print(Wahrscheinlichkeit)

# ------------------- Aufgabe 3.2 -------------------
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

# ------------------- Aufgabe 3.4 -------------------
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

# ------------------- Aufgabe 3.5 -------------------
# CSV-Datei einlesen
data <- read.csv(
  file = "bike_sharing_data_(with_NAs).csv", # Pfad zur CSV-Datei mit fehlenden Werten (NAs)
  header = TRUE, # Die Datei enthält eine Kopfzeile
  sep = ",", # Die Spaltenwerte sind durch Kommas getrennt
  quote = "\"" # Anführungszeichen für Textwerte
)

# Zeigt die ersten Zeilen des Datensatzes an
head(data)

# Filtern der Daten für die Gruppe 4
data_group_4 <- subset(data, group == 4)

# Speichern des gefilterten Datensatzes als CSV-Datei
write.csv(data_group_4)

# Anzeigen der Spaltennamen des Datensatzes
colnames(data)

# Überprüfung, ob der gefilterte Datensatz fehlende Werte enthält
anyNA(data_group_4)

# Zählen der fehlenden Werte insgesamt
sum(is.na(data_group_4))

# Anzahl der fehlenden Werte pro Spalte
colSums(is.na(data_group_4))

# Anzeigen der Zeilen mit fehlenden Werten
data_group_4[!complete.cases(data_group_4), ]

# Speichern des bereinigten Datensatzes in eine neue CSV-Datei ohne Zeilennummern
write.csv(data_group_4, file = "group_4_data.csv", row.names = FALSE)

# Positionen der fehlenden Werte anzeigen
which(is.na(data_group_4), arr.ind = TRUE)

# ---------------- Fehlende Werte (NAs) ersetzen ----------------

## Fehlende Werte in "day_of_year" ersetzen
data_group_4[182, 4] <- 182
data_group_4[217, 4] <- 217

## Fehlende Werte in "day_of_week" ersetzen
data_group_4[108, 5] <- 3
data_group_4[299, 5] <- 5

## Fehlende Werte in "month_of_year" ersetzen
data_group_4[57, 6] <- 2
data_group_4[184, 6] <- 7

## Fehlende Werte in "precipitation" und "windspeed" durch 0 ersetzen
data_group_4[is.na(data_group_4[, 7]), 7] <- 0
data_group_4[is.na(data_group_4[, 8]), 8] <- 0

## Fehlende Werte in Temperaturspalten durch den Mittelwert ersetzen
# Berechnung des Mittelwerts für "min_temperature" (NA-Werte ignorieren)
mean_value_min <- mean(data_group_4$min_temperature, na.rm = TRUE)
data_group_4[251, 9] <- mean_value_min  # Ersetzen des fehlenden Werts

# Berechnung des Mittelwerts für "average_temperature"
mean_value_avg <- mean(data_group_4$average_temperature, na.rm = TRUE)
data_group_4[289, 10] <- mean_value_avg

# Berechnung des Mittelwerts für "max_temperature"
mean_value_max <- mean(data_group_4$max_temperature, na.rm = TRUE)
data_group_4[13, 11] <- mean_value_max

## Fehlender Wert in "count" durch Mittelwert aus dem vorherigen und nächsten Tag ersetzen
count_neu <- (333 + 206) / 2  # Durchschnitt aus zwei benachbarten Werten berechnen
data_group_4[175, 12] <- count_neu

# ---------------- Datenanomalien bereinigen ----------------

## Negative Werte in "precipitation", "windspeed" und "count" durch 0 ersetzen
data_group_4$precipitation[data_group_4$precipitation < 0] <- 0
data_group_4$windspeed[data_group_4$windspeed < 0] <- 0
data_group_4$count[data_group_4$count < 0] <- 0

## Negative Werte in Temperaturspalten manuell korrigieren
which(data_group_4$min_temperature < 0)  # Sucht nach negativen Werten
which(data_group_4$average_temperature < 0)
which(data_group_4$max_temperature < 0)

# Manuelle Korrektur einzelner falscher Temperaturwerte
data_group_4[242, 9] <- 65
data_group_4[179, 10] <- 75
data_group_4[101, 11] <- 67

# ---------------- Umrechnung von Fahrenheit in Celsius ----------------

# Funktion zur Umrechnung zwischen Fahrenheit und Celsius
temperature_converter <- function(temp, from_unit = "F", to_unit = "C", digits = 2) {
  if (from_unit == "F" && to_unit == "C") {
    temp_conv <- (temp - 32) * 5 / 9  # Fahrenheit nach Celsius
  } else if (from_unit == "C" && to_unit == "F") {
    temp_conv <- (temp * 9 / 5) + 32  # Celsius nach Fahrenheit
  } else {
    stop("Invalid units.")  # Falls falsche Einheiten angegeben werden
  }
  return(round(temp_conv, digits = digits))  # Ergebnis auf 2 Nachkommastellen runden
}

# Temperaturspalten von Fahrenheit in Celsius umwandeln
data_group_4$min_temperature <- temperature_converter(data_group_4$min_temperature)
data_group_4$average_temperature <- temperature_converter(data_group_4$average_temperature)
data_group_4$max_temperature <- temperature_converter(data_group_4$max_temperature)

# ---------------- Bestimmung des Monats mit den meisten ausgeliehenen Fahrrädern ----------------

# Erstellen von Teilmengen für jeden Monat
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
data_group_4_dezember <- subset(data_group_4, month_of_year == 12)

# Anzeige der ersten Zeilen des bereinigten Datensatzes
head(data_group_4)

# Berechnung der Gesamtanzahl der ausgeliehenen Fahrräder pro Monat
monthly_totals <- aggregate(count ~ month_of_year, data = data_group_4, sum)

# Bestimmung des Monats mit der höchsten Anzahl an ausgeliehenen Fahrrädern
top_month <- monthly_totals[which.max(monthly_totals$count), ]
print(top_month)  # Ausgabe des Monats mit den meisten Leihvorgängen


# ------------------- Aufgabe 4 -------------------
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("gapminder")
library(gapminder)
data(gapminder)

# ------------------- Aufgabe 4.2 -------------------
# ----------------- a) Einfluss der Temperatur auf die Anzahl der ausgeliehenen Fahrräder -----------------

ggplot(data_group_4, aes(x = average_temperature, y = count)) +  # Erstellen eines Scatterplots mit Temperatur als x-Achse und Anzahl der Fahrräder als y-Achse
  geom_point(color = "blue", alpha = 0.6) +  # Punkte in Blau darstellen, Transparenz (alpha) auf 0.6 setzen
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Lineare Regression hinzufügen (schwarze Linie), ohne Konfidenzintervall
  labs(
    title = "Zusammenhang zwischen Temperatur und Anzahl ausgeliehener Fahrräder",  # Titel des Diagramms
    subtitle = "Schwarze Linie = Korrelation | Blaue Punkte = Ausprägungen",  # Untertitel zur Erklärung der Darstellung
    x = "Temperatur (°C)",  # Bezeichnung der x-Achse
    y = "Anzahl ausgeliehener Fahrräder"  # Bezeichnung der y-Achse
  ) +
  theme_minimal()  # Minimalistisches Design für das Diagramm

# ----------------- b) Einfluss des Niederschlags auf die Anzahl der ausgeliehenen Fahrräder -----------------

data_group_4$date <- as.Date(data_group_4$date)  # Konvertieren der Spalte "date" in das Datumsformat

ggplot(data_group_4, aes(x = precipitation, y = count)) +  # Scatterplot mit Niederschlag als x-Achse und Anzahl der Fahrräder als y-Achse
  geom_point(color = "blue", alpha = 0.6) +  # Blaue Punkte für die Datenpunkte mit Transparenz 0.6
  labs(
    title = "Zusammenhang zwischen Niederschlag und Anzahl ausgeliehener Fahrräder",  # Titel des Diagramms
    subtitle = "Blaue Punkte = Ausprägungen",  # Erklärung der Darstellung
    x = "Niederschlag (mm)",  # Bezeichnung der x-Achse
    y = "Anzahl ausgeliehener Fahrräder"  # Bezeichnung der y-Achse
  ) +
  theme_minimal()  # Minimalistisches Design für das Diagramm

# Berechnung der Korrelation zwischen Niederschlag und Anzahl ausgeliehener Fahrräder
cor(data_group_4$precipitation, data_group_4$count, use = "complete.obs")  

# Überprüfen, an welcher Stelle im Datensatz der Niederschlagswert 8.05 auftritt
which(data_group_4$precipitation == 8.05 , arr.ind = TRUE)

# Setzen des Niederschlagswerts in Zeile 272 auf NA, falls dieser fehlerhaft ist
data_group_4[272, 7] <- NA

# ----------------- c) Einfluss der Windgeschwindigkeit auf die Anzahl der ausgeliehenen Fahrräder -----------------

ggplot(data_group_4, aes(x = windspeed, y = count)) +  # Scatterplot mit Windgeschwindigkeit als x-Achse und Anzahl der Fahrräder als y-Achse
  geom_point(color = "blue", alpha = 0.6) +  # Blaue Punkte für die Datenpunkte mit Transparenz 0.6
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Lineare Regression hinzufügen (schwarze Linie), ohne Konfidenzintervall
  labs(
    title = "Zusammenhang zwischen Windgeschwindigkeit und Anzahl ausgeliehener Fahrräder",  # Titel des Diagramms
    subtitle = "Schwarze Linie = Korrelation | Blaue Punkte = Ausprägungen",  # Erklärung der Darstellung
    x = "Windgeschwindigkeit",  # Bezeichnung der x-Achse
    y = "Anzahl ausgeliehener Fahrräder"  # Bezeichnung der y-Achse
  ) +
  theme_minimal()  # Minimalistisches Design für das Diagramm

# ----------------- d) Entwicklung der Anzahl ausgeliehener Fahrräder über die Zeit -----------------

ggplot(data_group_4, aes(x = date, y = count)) +  # Scatterplot mit Datum als x-Achse und Anzahl der Fahrräder als y-Achse
  geom_point(color = "blue", alpha = 0.6) +  # Blaue Punkte für die Datenpunkte mit Transparenz 0.6
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Lineare Regression hinzufügen (schwarze Linie), ohne Konfidenzintervall
  labs(
    title = "Zusammenhang zwischen Zeit und Anzahl ausgeliehener Fahrräder",  # Titel des Diagramms
    subtitle = "Schwarze Linie = Korrelation | Blaue Punkte = Ausprägungen",  # Erklärung der Darstellung
    x = "Tag",  # Bezeichnung der x-Achse
    y = "Anzahl ausgeliehener Fahrräder"  # Bezeichnung der y-Achse
  ) +
  theme_minimal()  # Minimalistisches Design für das Diagramm

# ------------------- Aufgabe 4.3 -------------------
# ----------------- Erstellen von zwei separaten Datensätzen basierend auf Niederschlag -----------------

# Datensatz für Tage mit Niederschlag (precipitation > 0)
data_niederschlag <- data_group_4[data_group_4$precipitation > 0, ]

# Datensatz für Tage ohne Niederschlag (precipitation == 0)
data_kein_niederschlag <- data_group_4[data_group_4$precipitation == 0, ]

# ----------------- a) Zusammenhang zwischen Temperatur und Verleihzahlen an Tagen MIT Niederschlag -----------------

ggplot(data_niederschlag, aes(x = average_temperature, y = count)) +  # Scatterplot mit Temperatur als x-Achse und Anzahl der Fahrräder als y-Achse
  geom_point(color = "blue", alpha = 0.6) +  # Blaue Punkte für Datenpunkte, Transparenz auf 0.6 setzen
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Lineare Regression hinzufügen (schwarze Linie), ohne Konfidenzintervall
  labs(
    title = "Zusammenhang zwischen Temperatur und Verleihzahlen (Regen)",  # Titel des Diagramms
    subtitle = "Schwarze Linie = Korrelation | Blaue Punkte = Ausprägungen",  # Erklärung der Darstellung
    x = "Temperatur (°C)",  # Beschriftung der x-Achse
    y = "Anzahl ausgeliehener Fahrräder"  # Beschriftung der y-Achse
  ) +
  theme_minimal()  # Minimalistisches Design für das Diagramm

# ----------------- b) Zusammenhang zwischen Temperatur und Verleihzahlen an Tagen OHNE Niederschlag -----------------

ggplot(data_kein_niederschlag, aes(x = average_temperature, y = count)) +  # Scatterplot mit Temperatur als x-Achse und Anzahl der Fahrräder als y-Achse
  geom_point(color = "blue", alpha = 0.6) +  # Blaue Punkte für Datenpunkte, Transparenz auf 0.6 setzen
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Lineare Regression hinzufügen (schwarze Linie), ohne Konfidenzintervall
  labs(
    title = "Zusammenhang zwischen Temperatur und Verleihzahlen (ohne Regen)",  # Titel des Diagramms (Tippfehler "ohen" korrigiert)
    subtitle = "Schwarze Linie = Korrelation | Blaue Punkte = Ausprägungen",  # Erklärung der Darstellung
    x = "Temperatur (°C)",  # Beschriftung der x-Achse
    y = "Anzahl ausgeliehener Fahrräder"  # Beschriftung der y-Achse
  ) +
  theme_minimal()  # Minimalistisches Design für das Diagramm


# ------------------- Aufgabe 4.4 -------------------
# ----------------- Histogramm zur Verteilung der Anzahl ausgeliehener Fahrräder -----------------

ggplot(data_group_4, aes(x = count)) +  # Histogramm für die Anzahl der ausgeliehenen Fahrräder
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +  # Balken in Himmelblau mit schwarzer Umrandung und 70% Deckkraft
  labs(
    title = "Verteilung der Anzahl ausgeliehener Fahrräder",  # Diagrammtitel
    subtitle = "Blaue Säulen = Häufigkeit verschiedener Ausleihungsmengen im Beobachtungszeitraum",  # Beschreibung der Visualisierung
    x = "Anzahl ausgeliehener Fahrräder",  # Beschriftung der x-Achse
    y = "Häufigkeit"  # Beschriftung der y-Achse
  ) +
  theme_minimal()  # Minimalistisches Design für das Diagramm

# ----------------- Histogramm zur Verteilung der durchschnittlichen Temperatur -----------------

ggplot(data_group_4, aes(x = average_temperature)) +  # Histogramm für die Durchschnittstemperatur
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black", alpha = 0.7) +  # Balken in Korallenrot mit schwarzer Umrandung und 70% Deckkraft
  labs(
    title = "Verteilung der Temperatur (°C)",  # Diagrammtitel
    subtitle = "Rote Säulen = Häufigkeit verschiedener auftretender Temperaturen im Beobachtungszeitraum",  # Beschreibung der Visualisierung
    x = "Temperatur (°C)",  # Beschriftung der x-Achse
    y = "Häufigkeit"  # Beschriftung der y-Achse
  ) +
  theme_minimal()  # Minimalistisches Design für das Diagramm

# ----------------- Histogramm zur Verteilung der Niederschlagsmenge -----------------

ggplot(data_group_4, aes(x = precipitation)) +  # Histogramm für die Niederschlagsmenge
  geom_histogram(binwidth = 0.1, fill = "lightgreen", color = "black", alpha = 0.7) +  # Balken in Hellgrün mit schwarzer Umrandung und 70% Deckkraft
  labs(
    title = "Verteilung der Niederschlagsmenge (mm)",  # Diagrammtitel
    subtitle = "Grüne Säulen = Häufigkeit verschiedener Niederschlagsmengen im Beobachtungszeitraum",  # Beschreibung der Visualisierung
    x = "Niederschlag (mm)",  # Beschriftung der x-Achse
    y = "Häufigkeit"  # Beschriftung der y-Achse
  ) +
  theme_minimal()  # Minimalistisches Design für das Diagramm

# ----------------- Histogramm zur Verteilung der Windgeschwindigkeit -----------------

ggplot(data_group_4, aes(x = windspeed)) +  # Histogramm für die Windgeschwindigkeit
  geom_histogram(binwidth = 1, fill = "gold", color = "black", alpha = 0.7) +  # Balken in Gold mit schwarzer Umrandung und 70% Deckkraft
  labs(
    title = "Verteilung der Windgeschwindigkeit (km/h)",  # Diagrammtitel
    subtitle = "Goldene Säulen = Häufigkeit verschiedener Windgeschwindigkeiten im Beobachtungszeitraum",  # Beschreibung der Visualisierung
    x = "Windgeschwindigkeit (km/h)",  # Beschriftung der x-Achse
    y = "Häufigkeit"  # Beschriftung der y-Achse
  ) +
  theme_minimal()  # Minimalistisches Design für das Diagramm

# ------------------- Aufgabe 4.5 -------------------
# ----------------- Datum nach Jahreszeiten aufteilen -----------------

data_group_4$season <- case_when(
  data_group_4$month_of_year %in% c(3, 4, 5) ~ "Frühling",  # Monate März, April, Mai = Frühling
  data_group_4$month_of_year %in% c(6, 7, 8) ~ "Sommer",  # Monate Juni, Juli, August = Sommer
  data_group_4$month_of_year %in% c(9, 10, 11) ~ "Herbst",  # Monate September, Oktober, November = Herbst
  data_group_4$month_of_year %in% c(12, 1, 2) ~ "Winter",  # Monate Dezember, Januar, Februar = Winter
  TRUE ~ NA_character_  # Falls eine andere Zahl vorkommt (sollte nicht passieren), wird NA gesetzt
)

# ----------------- Grafik zur Verteilung der Anzahl ausgeliehener Fahrräder nach Jahreszeit -----------------

ggplot(data_group_4, aes(x = count, fill = season)) +  # x-Achse = Anzahl der ausgeliehenen Fahrräder, Farbe nach Jahreszeit
  geom_density(alpha = 0.25) +  # Dichtekurve mit 25% Transparenz, um Überlagerung zu ermöglichen
  labs(
    title = "Verteilung der Anzahl ausgeliehener Fahrräder nach Jahreszeit",  # Titel des Diagramms
    x = "Anzahl ausgeliehener Fahrräder",  # Beschriftung der x-Achse
    y = "Dichte"  # Beschriftung der y-Achse
  ) +
  scale_fill_manual(  # Manuelles Setzen der Farben für die Jahreszeiten
    values = c(
      "Frühling" = "green",  # Frühling = Grün
      "Sommer" = "red",  # Sommer = Rot
      "Herbst" = "orange",  # Herbst = Orange
      "Winter" = "blue"  # Winter = Blau
    )
  ) +
  theme_minimal()  # Minimalistisches Design für eine klare Darstellung

# Aufgabe 4.6
install.packages("plotly")
library(plotly)
# ----------------- 3D-Grafik der Fahrradverleihzahlen in Abhängigkeit von Temperatur und Windgeschwindigkeit -----------------

"3D_Grafik" <- plot_ly(
  data = data_group_4,  # Datensatz für die Visualisierung
  x = ~average_temperature,  # x-Achse: Durchschnittliche Temperatur in °C
  y = ~windspeed,  # y-Achse: Windgeschwindigkeit
  z = ~count,  # z-Achse: Anzahl ausgeliehener Fahrräder
  color = ~count,  # Farbgebung basierend auf der Anzahl der ausgeliehenen Fahrräder
  colors = c("#efedf5", "#bcbddc", "#756bb1"),  # Farbpalette von hell bis dunkel für bessere Lesbarkeit
  type = "scatter3d",  # 3D-Streudiagramm
  mode = "markers",  # Punkte als Markierungen ohne Linien
  marker = list(size = 4, opacity = 0.8)  # Markergröße und Transparenz für bessere Sichtbarkeit
)

# ----------------- Achsentitel und Layout der 3D-Grafik -----------------

`3D_Grafik` %>% layout(
  scene = list(
    xaxis = list(title = "Temperatur in C°"),  # Titel der x-Achse
    yaxis = list(title = "Windgeschwindigkeit"),  # Titel der y-Achse
    zaxis = list(title = "Anzahl ausgeliehener Fahrräder")  # Titel der z-Achse
  )
)

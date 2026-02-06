#Aufgabe 2

# Charlotte
#i)

Metrische_Variablen <- function(data, var) {
  if(!var %in% names(data)) {
    stop("Variable ecistiert nicht im Datensatz.")
    
    #Existenz der Variable prüfen
    
  }
  x <- data[[var]]
  
  if(!is.numeric(x)) {
    stop("Variable ist nicht metrisch.")
    
    # Prüfen, ob metrisch
  }
  result <- data.frame(
    Variable = var,
    N = sum(!is.na(x)),
    Missing = sum(is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    standarddeviation = sd(x, na.rm = TRUE),
    Variance = var(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  )
  return(result)
}

###############################################################################

# Hanna 
# ii.

kategoriale_Variablen <- function(x) {
  
  if (!is.factor(x) && !is.character(x)) {
    stop("Variable ist nicht vom faktor- oder character-typ")
  }
  # Prüfen ob Variable entweder factor oder character ist.
  # Fehlermeldung wenn Variable kein factor oder character ist.
  
  x <- as.factor(x)
  
  abs_freq <- table(x, useNA = "ifany")
  # Gibt die absoluten Häufigkeiten an. Bei vorhandenen NAs werden diese in 
  # einer eigenen Katerogie gezählt.
  
  rel_freq <- prop.table(abs_freq)
  # Rechnet die relativen Häufigkeiten aus.
  
  modus <- names(abs_freq)[which.max(abs_freq)]
  # Gibt den Modus an
  
  n_kategorien <- nlevels(x)
  # Gibt an wie viele Kategorien es in den jeweiligen "Merkmalen" gibt
  
  na_anteil <- mean(is.na(x))
  # Ermittelt den Anteil der NAs
  
  return(list(
    absolute_haeufigkeiten = abs_freq,
    relative_haeufigkeiten = rel_freq,
    modus = modus,
    anzahl_kategorien = n_kategorien,
    na_anteil = na_anteil
  )) 
  # Gibt die Ergebnisse in einer Liste zurück.
}

Ueberlebt <- Titanic_1$Survived
Klassen <- Titanic_1$Pclass
Geschlecht <- Titanic_1$Sex
Hafen <- Titanic_1$Embarked
Titel <- Titanic_1$Title
Schiffsseite <- Titanic_1$Side
Deck <- Titanic_1$Deck
# Neue Zuordnung der kategorialen Variablen, für einfachere Nutzung der Funktion.

kategoriale_Variablen(Ueberlebt)
# Ausgabe der Endergebnisse zur jeweiligen Variable (z.B."Überlebt")

# Jule
# iii
# Deskriptive bivariate Statistiken für den Zusammenhang 
# zwischen zwei kategorialen Variablen

# Erstellung einer Kontigenztafel

Kontigenztafel <- function(x, y){
  #Die Merkmale müssen an den gleichen Merkmalsträgern erhoben sein,
  #somit muss die Länge übereinstimmen
  #Umgang mit NAs: Merkmalsträger mit fehlenden Werten werden nicht aufgeführt
  if(length(x) != length(y))
    return("x und y müssen dieselbe Länge haben")
  else{Tabelle <- addmargins(table(x, y))
  if(any(is.na(x) , is.na(y)))
    return(list("Auf Grund fehlender Werte werden nicht alle Merkmalsträger aufgeführt", 
                Tabelle))
  else{Tabelle <- table(x, y)
  Kontigenz <- addmargins(Tabelle)
  print(Kontigenz)}}
}

## Samuel
## v)
## Erstellung einer Funktion zur Visualisierung von drei kategoriellen Variablen

mosaic_kat <- function(data, variab) {
  if(length(variab) < 3 || length(variab) > 4) {
    stop("Es dürfen nur 3 oder 4 Variablen sein")
  }
  
  if(!all(variab %in% names(data))) {
    stop("Mindestens eine Variable existiert nicht im Datensatz")
  }
  
  if(!all(sapply(data[variab], is.factor))) {
    stop("Variablen müssen nominal oder ordinal skaliert sein")
  }

  # Mosaicplot
  mosaicplot(table(data[vars]),
             main = paste("Mosaicplot:", paste(vars, collapse = " × ")),
             color = TRUE, las = 1)
}

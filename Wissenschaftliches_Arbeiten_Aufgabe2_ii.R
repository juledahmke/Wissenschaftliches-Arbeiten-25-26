
###############################################################################

# ii.
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken für
# kategoriale Variablen berechnet und ausgibt

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

################################################################################

# Aufgabe 1

install.packages('readr')
library(readr) # Paket um csv Dateien einzulesen
Titanic_1 <- read_csv("C:\\Users\\Acer\\Downloads\\Uni\\Programmieren mit R\\titanic.csv")
# Einlesen der Datei titanic.csv über meinen Pfad, je nach Nutzer abändern/anpassen! 

###############################################################################

Titanic_1$Title <- sub(".*, ([^.]+)\\..*", "\\1", Titanic_1$Name)
# Zieht aus Titanic_1$Name den Titel der Person heraus, welcher in eine neue
# Spalte Titanic_1$Titel gepackt wird.
# sub(pattern, replacement, x)

Titanic_1$Title[Titanic_1$Title %in% c("Ms", "Mlle")] <- "Miss"
# Verschiedene Schreibweisenvon "Miss" werden zusammengeführt

Titanic_1$Title[Titanic_1$Title == "Mme"] <- "Mrs"
# Der französische AUsdruck "Mme" wird zu "Mrs" umbenannt

Titanic_1$Title[!Titanic_1$Title %in% c("Mr", "Mrs", "Miss", "Master")] <- "Other"
# Andere Titel als "Mr", "Mrs", "Miss" und "Master" werden zu "Other" umbennant.

table(Titanic_1$Title)

###############################################################################

Titanic_1$Survived <- factor(Titanic_1$Survived, 
                             levels = c(0, 1),
                             labels = c("Nein", "Ja"))
table(Titanic_1$Survived) 
# Ändert Variable zu factor mit "0"="Nein" und "1"="Ja"

Titanic_1$Sex <- factor(Titanic_1$Sex, 
                        levels = c("male", "female"), 
                        labels = c("Mann", "Frau"))
table(Titanic_1$Sex)
# Ändert Variable zu factor mit "male"="Mann" und "female"="Frau"

Titanic_1$Embarked <- factor(Titanic_1$Embarked, 
                             levels = c("C", "Q", "S"),
                             labels = c("Cherbourg", "Queenstown", "Southampton"))
table(Titanic_1$Embarked)
# Ändert Variable zu factor mit "C"="Cherbourg", "Q"="Queenstown" und 
# "S"="Southampton"

# Umgang mit NAs?
sum(is.na(Titanic_1$Embarked))
# 2 

###############################################################################

Klassen <- factor(Titanic_1$Pclass,
                           levels = c(3, 2, 1))
Titanic_1$Pclass <- ordered(Klassen)
# Klassen nach ihrer symbolischen Ordnung geordnet.
# Höhere Klassen repräsentieren einen höheren Status.

###############################################################################

age_by_title <- tapply(Titanic_1$Age, Titanic_1$Title, median, na.rm = TRUE)
# Alter je Anrede mithilfe des Medians berechnen (ohne NAs)

Titanic_1$Age <- mapply(function(age, title) {
  if (is.na(age)) {
    return(age_by_title[title])
  } else {
    return(age)
  }
} , Titanic_1$Age, Titanic_1$Title)
# NA werte ersetzen 

###############################################################################

cabin_number <- as.numeric(gsub("[^0-9]" , "" , Titanic_1$Cabin))
# Zahl aus Cabin ectrahieren 

Titanic_1$Side <- ifelse(is.na(cabin_number),
                         NA,
                         ifelse(cabin_number %% 2 == 1, 
                                "Steuerbord" ,
                                "Backbord"))
Titanic_1$Side <- factor(Titanic_1$Side)
#Backbord/Steuerbord bestimmen


Titanic_1$Deck <- substr(Titanic_1$Cabin, 1,1)
#Deck aus Cabin extrahieren (jeweils der erste Buchstabe)

Titanic_1$Deck[Titanic_1$Deck == "" | Titanic_1$Deck == " "] <- NA
#Leere/unbekannte Kabinen auf NA setzen

###############################################################################

Titanic_1 <- Titanic_1[, !(names(Titanic_1) %in%
                         c("PassengerId" , "Name" , "Ticket" , "Cabin"))]
#Variablen die entfernt werden sollen entfernen


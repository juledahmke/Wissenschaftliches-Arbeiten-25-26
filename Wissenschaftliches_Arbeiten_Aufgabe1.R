
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

Titanic_1$Title

###############################################################################

Titanic_1$Survived <- factor(Titanic_1$Survived, 
                             levels = c(0, 1),
                             labels = c("Nein", "Ja"))
Titanic_1$Survived 
# Ändert Variable zu factor mit "0"="Nein" und "1"="Ja"

Titanic_1$Sex <- factor(Titanic_1$Sex, 
                        levels = c("male", "female"), 
                        labels = c("Mann", "Frau"))
Titanic_1$Sex 
# Ändert Variable zu factor mit "male"="Mann" und "female"="Frau"

Titanic_1$Embarked <- factor(Titanic_1$Embarked, 
                             levels = c("C", "Q", "S"),
                             labels = c("Cherbourg", "Queenstown", "Southampton"))
Titanic_1$Embarked 
# Ändert Variable zu factor mit "C"="Cherbourg", "Q"="Queenstown" und 
# "S"="Southampton"
# Umgang mit NAs?

table(Titanic_1$Embarked)
table(Titanic_1$Sex)
table(Titanic_1$Survived)

###############################################################################

Klassen <- factor(Titanic_1$Pclass,
                           levels = c(3, 2, 1))
Titanic_1$Pclass <- ordered(Klassen)
# Klassen nach ihrer symbolischen Ordnung geordnet.
# Höhere Klassen repräsentieren einen höheren Status.
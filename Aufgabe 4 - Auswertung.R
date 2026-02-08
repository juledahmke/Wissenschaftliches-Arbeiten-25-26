#Aufgabe 4
#Setzen des richtigen Verzeichnisses 
#um den bereinigten Titanic Datensatz zu laden
#Laden der Daten
load("titanic_clean.RData")
Metrische_Variablen(Titanic_1, "Age")
#Die Passagiere an Bord waren eher jung, im Mittel ca. 30.

Metrische_Variablen(Titanic_1, "Fare")
#Auffällig ist, dass die Standardabweichung eher hoch ist, nämlich 49.69 
#und auch die Spannweite hoch ist.

kategoriale_Variablen(Titanic_1$Sex)
#Ungefähr zwei Drittel der Passagiere waren männlich.

kategoriale_Variablen(Titanic_1$Embarked)
#Die meisten Passagiere sind in Southhampton zugestiegen

kategoriale_Variablen(Titanic_1$Survived)
#Knapp weniger als zwei Drittel der Passagiere haben nicht überlebt. 
#Es sind 549 der Passagiere gestorben.

Kontigenztafel(Titanic_1$Survived, Titanic_1$Sex)
Kontigenztafel(Titanic_1$Survived, Titanic_1$Pclass)
Kontigenztafel(Titanic_1$Sex, Titanic_1$Pclass)
Kontigenztafel(Titanic_1$Pclass, Titanic_1$SibSp)

analyze_metric_dichotomous(metric_var = "SibSp", dichotomous_var = "Survived", 
                           data = Titanic_1)
## Der Unterschied beim Mittelwert zur Anzahl der Geschwister und Ehefrauen
## ist zwischen Überlebt "Ja" und "Nein" nicht sehr groß. Die Standardabweichung 
## ist bei denen die nicht überlebt haben um ca. 0.58 größer.

analyze_metric_dichotomous(metric_var = "Fare", dichotomous_var = "Survived", 
                           data = Titanic_1)
## Der Mittelwert des Preises und die Standardabweichung ist bei den 
## Überlebenden nahezu doppelt so groß.

## Ein Mosaikplot zur Dastellung der Fahrklasse, dem Hafen und dem Geschlecht
mosaic_kat(Titanic_1, c("Pclass", "Embarked", "Sex"))
## Die Klasse ist unter den Geschlechtern immer relativ gleichverteilt
## Die meisten sind aber insgesamt vom Hafen "Southampton"

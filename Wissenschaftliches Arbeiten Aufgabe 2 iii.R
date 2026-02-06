## iii
## Deskriptive bivariate Statistiken für den Zusammenhang 
## zwischen zwei kategorialen Variablen

##Erstellung einer Kontigenztafel

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

Korrelation <- function(x, y){if(!is.factor(x) | !is.factor(y))
                              return("x und y müssen ordinal skaliert sein")
  else{cor(x,y , use = complete.obs, method = "spearman")}
}


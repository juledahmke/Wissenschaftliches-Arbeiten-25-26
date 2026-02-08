## Funktion bei der das Skalenniveau einer Variable ausgegeben wird

Skalenniveau <- function(x) {
  
  if(is.factor(x) || is.character(x)) { return("nominal") }
  if(is.ordered(x)) { return("ordinal") }
  if(is.numeric(x) || is.integer(x)) { return("quantitativ")  }
  
  return("Ist nicht nominal, ordinal oder quantitativ")
}

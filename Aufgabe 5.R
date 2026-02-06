## Erstellung einer Funktion zur Visualisierung von drei kategoriellen Variablen

plot_mosaic_cat <- function(data, vars) {
  if (length(vars) < 3 || length(vars) > 4) {
    stop("Bitte genau 3 oder 4 Variablen angeben.")
  }
  
  if (!all(vars %in% names(data))) {
    stop("Mindestens eine Variable existiert nicht im Datensatz.")
  }
  
  # Variablen extrahieren und als Faktoren speichern
  df <- data[vars]
  for (v in vars) {
    df[[v]] <- as.factor(df[[v]])
  }
  
  # Kontingenztafel erzeugen
  tab <- table(df)
  
  # Mosaicplot
  mosaicplot(tab,
             main = paste("Mosaicplot:", paste(vars, collapse = " × ")),
             color = TRUE,
             las = 1)
}
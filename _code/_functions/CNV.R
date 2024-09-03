
# Función para renombrar columnas basadas en su orden
renombrar_columnas_por_orden <- function(data, nuevos_nombres) {
  if (ncol(data) == length(nuevos_nombres)) {
    colnames(data) <- nuevos_nombres
  } else {
    stop("El número de columnas no coincide con el número de nombres nuevos.")
  }
  return(data)
}

normalizar_texto <- function(texto) {
  texto <- tolower(texto)  # Convertir a minúsculas
  texto <- str_replace_all(texto, "[áàäâã]", "a")
  texto <- str_replace_all(texto, "[éèëê]", "e")
  texto <- str_replace_all(texto, "[íìïî]", "i")
  texto <- str_replace_all(texto, "[óòöôõ]", "o")
  texto <- str_replace_all(texto, "[úùüû]", "u")
  texto <- str_replace_all(texto, "ç", "c")
  return(texto)
}
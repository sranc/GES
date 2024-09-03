# Función para cargar y renombrar múltiples archivos Excel
cargar_y_renombrar_datos <- function(ruta_base, archivos, nuevos_nombres) {
  rutas <- paste0(ruta_base, archivos)
  
  # Crear una lista para almacenar los DataFrames cargados y renombrados
  lista_datos <- list()
  
  # Cargar y renombrar todos los conjuntos de datos en un bucle
  for (i in seq_along(rutas)) {
    # Cargar el archivo de datos
    data <- read_excel(rutas[i])

    # Renombrar las columnas
    data <- renombrar_columnas_por_orden(data, nuevos_nombres)
    
    data <- data %>%
      mutate(across(where(is.character), normalizar_texto))
    
    data <- data %>%
      mutate(across(where(is.numeric), as.numeric))
    
    # Guardar el DataFrame en la lista con un nombre identificativo
    lista_datos[[sub(".xlsx", "", archivos[i])]] <- data
  }
  
  return(lista_datos)
}

mes_selector <- function(mes){
  mensaje <- switch (as.character(mes),
                     "ene" = "Enero",
                     "feb" = "Febrero",
                     "mas" = "Marzo",
                     "abr" = "Abril",
                     "may" = "Mayo",
                     "jun" = "Junio",
                     "jul" = "Julio",
                     "ago" = "Agosto",
                     "sep" = "Septiembre",
                     "oct" = "Octubre",
                     "nov" = "Noviembre",
                     "agui" = "Aguinaldo",
                     "dic" = "Diciembre",
  )
  mensaje
}

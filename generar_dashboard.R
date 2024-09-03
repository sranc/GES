###############################################################################################################
#                                                                                                             #
#            *  Ejecucion : no se modifica nada del codigo solo es seleccinar todo y ejecutarlo               #
#                                                                                                             #
###############################################################################################################

library(rmarkdown)

file_names <- c("CC", "FC", "RD", "SR", "UJ")

# Directorio base donde se encuentran los archivos .Rmd
input_dir <- "_code/_pages/"
# Directorio de salida
output_dir <- "_htmlOutput"

# Códigos de color ANSI para errores
red <- "\033[31m"  # Texto rojo
reset <- "\033[0m" # Restablecer el color

# Bucle para recorrer cada nombre de archivo y renderizarlo
for (name in file_names) {
  # Construir la ruta completa para el archivo de entrada
  input_file <- paste0(input_dir, name, ".rmd")
  
  # Intentar renderizar el archivo .Rmd con manejo de errores
  tryCatch({
    rmarkdown::render(input = input_file, output_dir = output_dir)
    cat("Renderizado exitoso para:", name, "\n")
  }, error = function(e) {
    # Mostrar el error en rojo
    cat(red, "Error al renderizar", name, ":", e$message, reset, "\n")
  })
}
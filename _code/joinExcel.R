
# DataCombine: Función que combina datos de múltiples archivos Excel en un solo archivo Parquet.
# Parámetros:
#   - area: Nombre del área o categoría de los datos.
#   - output_parquet_name: Nombre del archivo Parquet de salida.
# Efectos secundarios:
#   - Crea un directorio "_data/area/data" para almacenar el archivo Parquet resultante.
#   - Guarda el archivo Parquet combinado en el directorio especificado.
#   - No devuelve ningún valor, pero proporciona mensajes informativos durante la ejecución.
DataCombine <- function(area,output_parquet_name) {
  # Instalar los paquetes si no están instalados
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse")
  }
  
  if (!requireNamespace("arrow", quietly = TRUE)) {
    install.packages("arrow")
  }
  
  library(tidyverse)
  library(arrow)
  library(readxl)
  # Lista para almacenar todos los data frames
  data_list <- list()

  # Leer cada archivo Excel y agregar la columna de bd
  directory_path_in <- paste0("_data/",area,"/temporal")
  
  for (excel_file in list.files(path = directory_path_in, pattern = "*.xlsx", full.names = TRUE)) {
    
    # Extraer el nombre del archivo sin extensión
    db_name <- tools::file_path_sans_ext(basename(excel_file))
    
    # Leer datos desde el archivo Excel
    excel_data <- read_excel(excel_file)
    
    # Agregar la columna de bd
    excel_data <- excel_data %>% 
      mutate(bd = db_name)
    
    # Agregar el data frame a la lista
    data_list[[db_name]] <- excel_data
  }
  
  # Combinar todos los data frames en uno solo
  merged_data <- bind_rows(data_list)
  
  directory_path_out <- paste0("_data/",area,"/data")
  dir.create(directory_path_out)
  
  # Guardar el resultado en formato Parquet
  write_parquet(merged_data, paste0(directory_path_out,"/", output_parquet_name,".parquet"))
}
  
  area <- "SR"
  output_parquet_name <- "2024"

  DataCombine(area, output_parquet_name)

  cc_data <- read_parquet("_data/sr/data/2024.parquet")
  view(cc_data)
  
  join_parquet_to_excel <- function(data_to_excel,direction) {
    data_excel <- data_format_convert(data_to_excel)
    data_excel <- data_excel %>% 
      filter(!is.na(cantidad))
    write.xlsx(data_excel,paste0("_data/",direction,sep = ""))
  }
  
  join_parquet_to_excel(cc_data,"sr/sr.xlsx")
  
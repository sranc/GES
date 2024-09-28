###############################################################################################################
#                                                                                                             #
#            *  gestion   : Asigna el año en curso. Este valor se actualiza una vez al año.                   #
#            *  mes       : Asigna el mes actual. Se actualiza cada mes al comenzar un nuevo periodo.         #
#            *  Ejecucion : una vez verificados los datos de gestion y mes selecciona todo el codigo y        #
#                           ejecutalo, si existe algun error en la consola te mostrara                        #
#                                                                                                             #
###############################################################################################################
source("_code/joinExcel.R",local = TRUE)
source("_code/_functions/DFC.R")
source("_code/_functions/JoGe.R")


gestion <- 2024
mes <- "AGOSTO"

dataset_names <- c("CC", "FC", "RD", "SR", "UJ")

red <- "\033[31m"
bold <- "\033[1m"  
reset <- "\033[0m"

for (dataset in dataset_names) {
  tryCatch({
    suppressWarnings({
      DataCombine(dataset, gestion, mes)
    })
    cat("Proceso para el conjunto de datos '", dataset, "' completado exitosamente.\n")
  }, error = function(e) {
    cat(red, "Error en el proceso para el conjunto de datos\n                '", 
        bold, 
        dataset, 
        reset, 
        red, "'\n",  
        e$message, 
        reset, "\n", 
        sep = "")
  })
}




for (dataset in dataset_names) {
  dir = paste0("_data/", dataset, "/data", sep = "")
  tryCatch({
    new_data <- join_gestions(dir)
    join_parquet_to_excel(new_data,paste0("_excelOutput/",dataset,".xlsx"))
    cat("Proceso para el conjunto de datos '", dataset, "' completado exitosamente.\n")
  }, error = function(e){
    cat(red, "Error en el proceso para el conjunto de datos\n                '", 
        bold, 
        dataset, 
        reset, 
        red, "'\n",  
        e$message, 
        reset, "\n", 
        sep = "")
  })
}

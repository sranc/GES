# directorios de las distintas areas

cc_dir <- "../_data/cc/data"
source("../../_code/_functions/DFC.R")
color_mark <-  "#909090"

# join_gestions: Función que combina datos de archivos Parquet en un solo data frame.
# Parámetros:
#   - data_directory: Ruta al directorio que contiene archivos Parquet.
# Devuelve:
#   - Un data frame que contiene la combinación de todos los datos de los archivos Parquet.
join_gestions <- function(data_directory){
  
  data_list <- list()
  
  for (parquet_field in list.files(path = data_directory, pattern = "*.parquet", full.names = TRUE)) {
    db_name <- tools::file_path_sans_ext(basename(parquet_field))
    
    parquet_data <- read_parquet(parquet_field)
    
    data_list[[db_name]] <- parquet_data
  }
  merge_data <- bind_rows(data_list)
  merge_data <- merge_data %>% 
    select(bd,gestion:JUN,JUN_REINT,JUL:DIC) 

  return(merge_data)
}
# Argumentos:
#   - data: El conjunto de datos original que contiene información de múltiples gestiones y meses.
# 
# Retorna:
#   - Un nuevo conjunto de datos que incluye información de la última gestión y del mes de
#     diciembre de las dos gestiónes anteriores.
get_data_graf <- function(data,ges_pass = FALSE, MES = "DIC"){
  new_data <- data %>% 
    filter(gestion == max(gestion))
  if(ges_pass == TRUE){
    dic_lastgestion_1 <- data %>% 
      filter(gestion == max(gestion) - 1 & mes == MES) %>% 
      mutate(mes = paste0(mes, " ",gestion, " (*)"))
   # view(dic_lastgestion_1)
    dic_lastgestion_2 <- data %>% 
      filter(gestion == max(gestion) - 2 & mes == MES) %>% 
      mutate(mes = paste0(mes, " ",gestion, " (*)"))
   # view(dic_lastgestion_2)
    new_data <- rbind(dic_lastgestion_2,dic_lastgestion_1,new_data)
  }
  return(new_data)
}

mes_selector <- function(mes){
  mensaje <- switch (mes,
                     "ENE" = "Enero",
                     "FEB" = "Febrero",
                     "MAR" = "Marzo",
                     "ABR" = "Abril",
                     "MAY" = "Mayo",
                     "JUN" = "Junio",
                     "JUL" = "Julio",
                     "AGO" = "Agosto",
                     "SEP" = "Septiembre",
                     "OCT" = "Octubre",
                     "NOV" = "Noviembre",
                     "AGUI" = "Aguinaldo",
                     "DIC" = "Diciembre",
  )
}

data_to_show <- function(data,init,fin,fin_2 = NULL){
  mes = toString(tail(data,1)$mes)
  
  bar <- get_data_graf(data,TRUE,MES = mes)
  
  last_month <- tail(bar$mes,1)
  
  bar <- bar %>% 
    mutate(mes = ifelse(mes == last_month, paste0(last_month," (*)"), mes))
  
  table_data <- data_format_convert(bar,col_ini = init,col_fin = fin,names = "TIPO",values = "CANTIDAD" )
  table_data <- table_data %>% 
    select(TIPO,MES=mes,CANTIDAD) %>% 
    mutate(CANTIDAD = format(CANTIDAD, big.mark = ",",decimal.mark = "."))
  table_data <- data_format_convert(table_data,type = "short",names = "MES",values = "CANTIDAD")
  if(!is.null(fin_2)){
    fin <- fin_2
  }
  pie_data <- tail(bar,1) %>% 
    select(mes,c(!!sym(init): !!sym(fin))) %>% 
    mutate(MES = mes, mes = "CANTIDAD")
  pie_data <- data_format_convert(pie_data,col_ini = init,col_fin = fin,names = "TIPO")
  pie_data <- data_format_convert(pie_data,type = "short")
  
  gestion_before_names <- bar %>% 
    select(gestion) %>% 
    group_by(gestion) %>% 
    ungroup()
  
  gestion_before_names <- unique(unlist(gestion_before_names$gestion))
  gestion_before_names <- as.array(gestion_before_names)
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = mes_selector(last_month), "gestions_names" = gestion_before_names))
}

# Función start_process: Transformación de datos para trámites de registro de CC.
# La función toma un conjunto de datos cc_data y realiza varias operaciones:
# 1. Filtra los trámites nacionales al inicio del proceso, excluyendo las columnas 'bd','clase','tipo'.
# 2. Filtra los trámites regionales, excluyendo aquellos con tipo_CC igual a "LA PAZ".
# 3. Calcula los totales para trámites regionales agrupados por gestión, clase y tipo.
# 4. Combina los totales regionales con los datos iniciales, ajustando las etiquetas de tipo_CC.
# 5. Convierte la tabla a un formato largo para facilitar cálculos y análisis.
# 6. Filtra y acumula solo valores mayores a 0 en el campo 'cantidad'.
# 7. Pivota la tabla de nuevo a un formato ancho para facilitar la visualización.
# 8. Calcula totales mensuales y un total general para cada gestión, clase y tipo.
# 9. Combina los resultados antes de devolver el resultado final.
start_process <- function(cc_data){
  # Filtrar datos iniciales de trámites nacionales
  ini_tram_nal <- cc_data %>%
    filter(tipo == "REGISTRO CC (INICIO DE TRÁMITES)") %>% 
    select(-bd,-clase,-tipo)
  
  # Filtrar trámites regionales
  tot_regional <- ini_tram_nal %>%
    filter(tipo_CC != "LA PAZ")
  
  # Calcular totales para trámites regionales
  totales_regionales <- tot_regional %>%
    group_by(gestion) %>%
    summarize(across(ENE:DIC, sum)) %>%
    mutate(tipo_CC = "REGIONALES")
  
  # Combinar totales regionales con datos iniciales
  ini_tram_nal_modificado <- bind_rows(ini_tram_nal, totales_regionales) %>%
    filter(tipo_CC %in% c("LA PAZ", "REGIONALES")) %>%
    mutate(tipo_CC = ifelse(tipo_CC == "LA PAZ", "OFICINA_CENTRAL", tipo_CC)) %>%
    arrange(tipo_CC)
  
  # Convertir a formato largo para facilitar cálculos
  new_table <- data_format_convert(ini_tram_nal_modificado)
  
  # Filtrar y acumular solo valores mayores a 0
  resultado <- new_table %>% 
    filter(cantidad > 0) %>% 
    group_by(gestion,tipo_CC) %>% 
    mutate(cantidad = cumsum(cantidad)) %>% 
    ungroup()
  
  # Convertir a formato corto
  resultado <- data_format_convert(resultado,"short")
  
  # Calcular totales mensuales y totales generales
  total_mes <- resultado %>% 
    group_by(gestion,tipo_CC = "TOTAL") %>% 
    summarise(across(ENE:DIC, sum), .groups = "drop")
  
  # Unir resultados 
  resultado <- bind_rows(resultado,total_mes) 
  
  resultado <- data_format_convert(resultado,"large")
  resultado <- data_format_convert(resultado,"short",names = "tipo_CC")
  mes = toString(tail(resultado,1)$mes)
  
  bar <- get_data_graf(resultado,TRUE,MES = mes)
  
  last_month <- tail(bar$mes,1)
  
  bar <- bar %>% 
    mutate(mes = ifelse(mes == last_month, paste0(last_month," (*)"), mes))
  
  table_data <- data_format_convert(bar,col_ini = "OFICINA_CENTRAL",col_fin = "TOTAL",names = "TIPO",values = "CANTIDAD" )
  table_data <- table_data %>% 
    select(TIPO,MES=mes,CANTIDAD) %>% 
    mutate(CANTIDAD = format(CANTIDAD, big.mark = ",",decimal.mark = "."))
  table_data <- data_format_convert(table_data,type = "short",names = "MES",values = "CANTIDAD")
  
  pie_data <- tail(bar,1) %>% 
    select(mes,OFICINA_CENTRAL,REGIONALES) %>% 
    mutate(MES = mes, mes = "CANTIDAD")
  pie_data <- data_format_convert(pie_data,col_ini = "OFICINA_CENTRAL",col_fin = "REGIONALES",names = "TIPO")
  pie_data <- data_format_convert(pie_data,type = "short")
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = mes_selector(last_month)))
}

compensation_process <- function(cc_data){
  # Filtrar datos iniciales de trámites nacionales
  ini_tram_nal <- cc_data %>%
    filter(tipo == "EMISION DE CERTIFICADOS DE CC") %>% 
    select(-bd,-clase,-tipo)
  # Filtrar trámites regionales
  tot_regional <- ini_tram_nal %>%
    filter(tipo_CC %in% c("MENSUAL", "GLOBAL"))
  # Convertir a formato largo para facilitar cálculos
  new_table <- data_format_convert(tot_regional)
  
  # Filtrar y acumular solo valores mayores a 0
  resultado <- new_table %>% 
    filter(cantidad > 0) %>% 
    group_by(gestion,tipo_CC) %>% 
    mutate(cantidad = cumsum(cantidad)) %>% 
    ungroup()
  # Convertir a formato corto
  resultado <- data_format_convert(resultado,"short")
  
  # Calcular totales mensuales y totales generales
  total_mes <- resultado %>% 
    group_by(gestion,tipo_CC = "TOTAL") %>% 
    summarise(across(ENE:DIC, sum), .groups = "drop")
  
  # Unir resultados 
  resultado <- bind_rows(resultado,total_mes) 
  
  resultado <- data_format_convert(resultado,"large")
  resultado <- data_format_convert(resultado,"short",names = "tipo_CC")
  mes = toString(tail(resultado,1)$mes)
  
  bar <- get_data_graf(resultado,TRUE,MES = mes)
  
  last_month <- tail(bar$mes,1)
  
  bar <- bar %>% 
    mutate(mes = ifelse(mes == last_month, paste0(last_month," (*)"), mes))
  
  table_data <- data_format_convert(bar,col_ini = "MENSUAL",col_fin = "TOTAL",names = "TIPO",values = "CANTIDAD" )
  table_data <- table_data %>% 
    select(TIPO,MES=mes,CANTIDAD) %>% 
    mutate(CANTIDAD = format(CANTIDAD, big.mark = ",",decimal.mark = "."))
  table_data <- data_format_convert(table_data,type = "short",names = "MES",values = "CANTIDAD")
  
  pie_data <- tail(bar,1) %>% 
    select(mes,MENSUAL,GLOBAL) %>% 
    mutate(MES = mes, mes = "CANTIDAD")
  pie_data <- data_format_convert(pie_data,col_ini = "MENSUAL",col_fin = "GLOBAL",names = "TIPO")
  pie_data <- data_format_convert(pie_data,type = "short")
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = mes_selector(last_month)))
}

process <- function(cc_data,data_type,filter_data = " "){
  data_1 <- "MENSUAL"
  data_2 <- "GLOBAL"
  # Filtrar trámites regionales
  if(data_type == "certificado"){
    ini_tram_nal <- cc_data %>%
      filter(tipo == filter_data) %>% 
      mutate(tipo_CC = toupper(tipo_CC)) %>% 
      select(-bd,-clase,-tipo)
    result <- ini_tram_nal %>%
      filter(tipo_CC %in% c(data_1, data_2))
  }
  if(data_type == "inicial"){
    data_1 <- "OFICINA_CENTRAL"
    data_2 <- "REGIONALES"
    ini_tram_nal <- cc_data %>%
      filter(tipo == "REGISTRO CC (INICIO DE TRÁMITES)") %>% 
      select(-bd,-clase,-tipo)
    
    # Filtrar trámites regionales
    tot_regional <- ini_tram_nal %>%
      filter(tipo_CC != "LA PAZ")
    
    # Calcular totales para trámites regionales
    totales_regionales <- tot_regional %>%
      group_by(gestion) %>%
      summarize(across(ENE:DIC, sum)) %>%
      mutate(tipo_CC = "REGIONALES")
    
    # Combinar totales regionales con datos iniciales
    result <- bind_rows(ini_tram_nal, totales_regionales) %>%
      filter(tipo_CC %in% c("LA PAZ", "REGIONALES")) %>%
      mutate(tipo_CC = ifelse(tipo_CC == "LA PAZ", "OFICINA_CENTRAL", tipo_CC)) %>%
      arrange(tipo_CC)
  }
  # Convertir a formato largo para facilitar cálculos
  new_table <- data_format_convert(result)
  
  # Filtrar y acumular solo valores mayores a 0
  resultado <- new_table %>% 
    filter(cantidad > 0) %>% 
    group_by(gestion,tipo_CC) %>% 
    mutate(cantidad = cumsum(cantidad)) %>% 
    ungroup()
  # Convertir a formato corto
  resultado <- data_format_convert(resultado,"short")
  
  # Calcular totales mensuales y totales generales
  total_mes <- resultado %>% 
    group_by(gestion,tipo_CC = "TOTAL") %>% 
    summarise(across(ENE:DIC, sum), .groups = "drop")
  
  # Unir resultados 
  resultado <- bind_rows(resultado,total_mes) 
  
  resultado <- data_format_convert(resultado,"large")
  resultado <- data_format_convert(resultado,"short",names = "tipo_CC")
  mes = toString(tail(resultado,1)$mes)
  
  bar <- get_data_graf(resultado,TRUE,MES = mes)
  
  last_month <- tail(bar$mes,1)
  
  bar <- bar %>% 
    mutate(mes = ifelse(mes == last_month, paste0(last_month," (*)"), mes))
  
  table_data <- data_format_convert(bar,col_ini = data_1,col_fin = "TOTAL",names = "TIPO",values = "CANTIDAD" )
  table_data <- table_data %>% 
    select(TIPO,MES=mes,CANTIDAD) %>% 
    mutate(CANTIDAD = format(CANTIDAD, big.mark = ",",decimal.mark = "."))
  table_data <- data_format_convert(table_data,type = "short",names = "MES",values = "CANTIDAD")
  
  pie_data <- tail(bar,1) %>% 
    select(mes,!!sym(data_1), !!sym(data_2)) %>% 
    mutate(MES = mes, mes = "CANTIDAD")
  pie_data <- data_format_convert(pie_data,col_ini = data_1,col_fin = data_2,names = "TIPO")
  pie_data <- data_format_convert(pie_data,type = "short")
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = mes_selector(last_month)))
}

process_n <- function(cc_data,filter_data, mes = "DIC"){
  resultado <- cc_data %>% 
    filter(clase == filter_data) %>% 
    select(-bd,-clase,-tipo) %>%
    data_format_convert() %>% 
    group_by(gestion,tipo_CC,mes) %>% 
    summarise(cantidad = sum(cantidad, na.rm = TRUE), .groups = 'drop') %>% 
    data_format_convert(type = "short")
  resultado <- resultado %>% 
    select(gestion,tipo_CC,ENE,FEB,MAR,ABR,MAY,JUN,JUN_REINT,JUL,AGO,SEP,OCT,NOV,AGUI,DIC)
  resultado <- data_format_convert(resultado,"large")
  resultado <- data_format_convert(resultado,"short",names = "tipo_CC") %>% 
    rename(MENSUAL = Mensual, GLOBAL = Global) 
  
  bar <- get_data_graf(resultado,TRUE,MES = mes) %>% 
    filter(MENSUAL > 0)
  
  last_month <- tail(bar$mes,1)
  
  bar <- bar %>% 
    mutate(mes = ifelse(mes == last_month, paste0(last_month," (*)"), mes))
  
  table_data <- data_format_convert(bar,col_ini = "GLOBAL",col_fin = "MENSUAL",names = "TIPO",values = "CANTIDAD" )
  table_data <- table_data %>% 
    select(TIPO,MES=mes,CANTIDAD) %>% 
    mutate(CANTIDAD = format(CANTIDAD, big.mark = ",",decimal.mark = "."))
  table_data <- data_format_convert(table_data,type = "short",names = "MES",values = "CANTIDAD")
  
  pie_data <- tail(bar,1) %>% 
    select(mes,MENSUAL,GLOBAL) %>% 
    mutate(MES = mes, mes = "CANTIDAD")
  pie_data <- data_format_convert(pie_data,col_ini = "GLOBAL",col_fin = "MENSUAL",names = "TIPO")
  pie_data <- data_format_convert(pie_data,type = "short")
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = mes_selector(last_month)))
}

process_mont <- function(cc_data,filter_data = " "){
  data_1 <- "MENSUAL"
  data_2 <- "GLOBAL"
  result <- cc_data %>%
    filter(tipo == filter_data) %>% 
    mutate(tipo_CC = toupper(tipo_CC)) %>% 
    select(-bd,-clase,-tipo)
  
  result <- result %>% 
    data_format_convert() %>% 
    data_format_convert(type = "short", names = "tipo_CC")
  
  result <- result %>% 
    filter(GLOBAL > 0 | MENSUAL > 0)
  
  result <- result %>% 
    group_by(gestion) %>% 
    mutate(GLOBAL = cumsum(GLOBAL),MENSUAL = cumsum(MENSUAL))
  result <- result %>% 
    mutate_at(vars(GLOBAL, MENSUAL), ~round(./10^6)) %>% 
    mutate(TOTAL = GLOBAL + MENSUAL) %>% 
    ungroup() %>% 
    select(gestion,mes,MENSUAL,GLOBAL,TOTAL)
  mes = toString(tail(result,1)$mes)
  bar <- get_data_graf(result,TRUE,MES = mes)
  
  last_month <- tail(bar$mes,1)
  
  bar <- bar %>% 
    mutate(mes = ifelse(mes == last_month, paste0(last_month," (*)"), mes))
  
  table_data <- data_format_convert(bar,col_ini = data_1,col_fin = "TOTAL",names = "TIPO",values = "CANTIDAD" )
  table_data <- table_data %>% 
    select(TIPO,MES=mes,CANTIDAD) %>% 
    mutate(CANTIDAD = format(CANTIDAD, big.mark = ",",decimal.mark = "."))
  table_data <- data_format_convert(table_data,type = "short",names = "MES",values = "CANTIDAD")
  
  pie_data <- tail(bar,1) %>% 
    select(mes,!!sym(data_1), !!sym(data_2)) %>% 
    mutate(MES = mes, mes = "CANTIDAD")
  pie_data <- data_format_convert(pie_data,col_ini = data_1,col_fin = data_2,names = "TIPO")
  pie_data <- data_format_convert(pie_data,type = "short")
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = mes_selector(last_month)))
}

process_sex <- function(cc_data,filter_tipo = NULL,filter_clase = NULL,filter_bd = NULL,filter_tipo_cc = NULL){
  result <- cc_data %>%
    mutate(tipo = iconv(tipo, to = "ASCII//TRANSLIT")) %>% 
    filter(if (!is.null(filter_bd)) bd %in% filter_bd else TRUE) %>%
    filter(if (!is.null(filter_clase)) clase %in% filter_clase else TRUE) %>%
    filter(if (!is.null(filter_tipo)) tipo %in% filter_tipo else TRUE) %>%
    filter(if (!is.null(filter_tipo_cc)) tipo_CC %in% filter_tipo_cc else TRUE) %>%
    mutate(tipo_CC = toupper(tipo_CC)) %>% 
    select(-bd,-clase,-tipo)
  
  result <- result %>% 
    data_format_convert() %>% 
    group_by(gestion,tipo_CC,mes) %>% 
    summarise(cantidad = sum(cantidad), .groups = "drop") %>% 
    ungroup() %>% 
    data_format_convert(result,type = "short") %>% 
    select(gestion, tipo_CC, ENE, FEB, MAR, ABR, MAY, JUN, JUN_REINT, JUL, AGO, SEP, OCT, NOV, AGUI, DIC) %>% 
    data_format_convert()
  
  result <- result %>% 
    data_format_convert(type = "short", names = "tipo_CC")
  result <- result %>% 
    filter(FEMENINO > 0 | MASCULINO > 0) 
  return(data_to_show(result,init = "FEMENINO",fin = "MASCULINO")) 
}

process_altas <- function(cc_data,filter_tipo = NULL,filter_clase = NULL,filter_bd = NULL,filter_tipo_cc = NULL){
  result <- cc_data %>%
    mutate(tipo = iconv(tipo, to = "ASCII//TRANSLIT")) %>% 
    filter(if (!is.null(filter_bd)) bd %in% filter_bd else TRUE) %>%
    filter(if (!is.null(filter_clase)) clase %in% filter_clase else TRUE) %>%
    filter(if (!is.null(filter_tipo)) tipo %in% filter_tipo else TRUE) %>%
    filter(if (!is.null(filter_tipo_cc)) tipo_CC %in% filter_tipo_cc else TRUE) %>%
    mutate(tipo_CC = toupper(tipo_CC)) %>% 
    select(-bd,-clase,-tipo) 
  
  result <- result %>% 
    mutate(tipo_CC = gsub(" ","_",tipo_CC)) %>% 
    mutate(tipo_CC = iconv(tipo_CC, to = "ASCII//TRANSLIT"))
  
  result <- result %>% 
    data_format_convert()
  
  result <- result %>% 
    data_format_convert(type = "short", names = "tipo_CC")
  
  result <- result %>% 
    filter( AUTOMATICO_MENSUAL > 0 | AUTOMATICO_GLOBAL > 0 | MANUAL_MENSUAL > 0 | MANUAL_GLOBAL > 0) %>% 
    select(gestion,mes,MANUAL_GLOBAL,MANUAL_MENSUAL,AUTOMATICO_GLOBAL,AUTOMATICO_MENSUAL) 
  result <- result %>% 
    group_by(gestion) %>% 
    mutate(MANUAL_GLOBAL=cumsum(MANUAL_GLOBAL),
           MANUAL_MENSUAL=cumsum(MANUAL_MENSUAL),
           AUTOMATICO_GLOBAL=cumsum(AUTOMATICO_GLOBAL),
           AUTOMATICO_MENSUAL=cumsum(AUTOMATICO_MENSUAL)) %>% 
    ungroup() %>% 
    mutate(TOTAL = MANUAL_GLOBAL+MANUAL_MENSUAL+AUTOMATICO_GLOBAL+AUTOMATICO_MENSUAL)
  
  return(data_to_show(result,init = "MANUAL_GLOBAL",fin = "TOTAL",fin_2 = "AUTOMATICO_MENSUAL")) 
}
  # Crea un gráfico de barras
  # Argumentos:
  #   - data: Datos para el gráfico
  #   - bars: Tipo de barras a incluir ("all", "regional", "central", "total")
  #   - theme_show: Tema del gráfico
  #   - graph_text: Texto principal del gráfico
  #   - graph_subText: Subtexto del gráfico

formato_con_separador_miles <- function(x) {
  return(format(x, big.mark = ",", scientific = FALSE))
}


# directorios de las distintas areas

cc_dir <- "../_data/cc/data"


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

data_format_convert <- function(data_to_convert, type = "large",col_ini = "ENE", col_fin = "DIC", names = "mes",values = "cantidad"){
  if(type == "large"){
    resultado <- data_to_convert %>% 
      pivot_longer(cols = c(as.symbol(col_ini):as.symbol(col_fin)), names_to = names, values_to = values) %>% 
      filter(!is.na(get(values)))
  }
  if(type == "short"){
    resultado <- data_to_convert %>% 
      pivot_wider(names_from = as.symbol(names), values_from = as.symbol(values))
  }
  return(resultado)
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
      mutate(mes = paste(paste("(*)",mes), gestion, sep = "_"))
    
    dic_lastgestion_2 <- data %>% 
      filter(gestion == max(gestion) - 2 & mes == MES) %>% 
      mutate(mes = paste(paste("(*)",mes), gestion, sep = "_"))
    
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
  bar <- get_data_graf(data,TRUE)
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
  
  get_name <- mes_selector(tail(pie_data$MES,1))
  
  gestion_before_names <- bar %>% 
    filter(gestion != max(gestion)) %>% 
    select(gestion)
  
  gestion_before_names <- unique(unlist(gestion_before_names$gestion))
  gestion_before_names <- as.array(gestion_before_names)
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = get_name, "gestions_names" = gestion_before_names))
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
    summarise(across(ENE:DIC, sum))
  
  # Unir resultados 
  resultado <- bind_rows(resultado,total_mes) 
  
  resultado <- data_format_convert(resultado,"large")
  resultado <- data_format_convert(resultado,"short",names = "tipo_CC")
  
  bar <- get_data_graf(resultado,TRUE)
  
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
  
  get_name <- mes_selector(tail(pie_data$MES,1))
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = get_name))
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
    summarise(across(ENE:DIC, sum))
  
  # Unir resultados 
  resultado <- bind_rows(resultado,total_mes) 
  
  resultado <- data_format_convert(resultado,"large")
  resultado <- data_format_convert(resultado,"short",names = "tipo_CC")
  
  bar <- get_data_graf(resultado,TRUE)
  
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
  
  get_name <- mes_selector(tail(pie_data$MES,1))
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = get_name))
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
    summarise(across(ENE:DIC, sum))
  
  # Unir resultados 
  resultado <- bind_rows(resultado,total_mes) 
  
  resultado <- data_format_convert(resultado,"large")
  resultado <- data_format_convert(resultado,"short",names = "tipo_CC")
  mes = toString(tail(resultado,1)$mes)
  
  bar <- get_data_graf(resultado,TRUE,MES = mes)
  
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
  
  get_name <- mes_selector(tail(pie_data$MES,1))
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = get_name))
}

process_n <- function(cc_data,filter_data){
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
  bar <- get_data_graf(resultado,TRUE) %>% 
    filter(MENSUAL > 0)
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
  get_name <- mes_selector(tail(pie_data$MES,1))
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = get_name))
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

  bar <- get_data_graf(result,TRUE)
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
  
  get_name <- mes_selector(tail(pie_data$MES,1))
  
  return(list("bar" = bar,"table" = table_data,"pie" = pie_data,"month" = get_name))
}

process_sex <- function(data,filter_tipo = NULL,filter_clase = NULL,filter_bd = NULL,filter_tipo_cc = NULL){
  result <- cc_data %>%
    filter(if (!is.null(filter_bd)) bd %in% filter_bd else TRUE) %>%
    filter(if (!is.null(filter_clase)) clase %in% filter_clase else TRUE) %>%
    filter(if (!is.null(filter_tipo)) tipo %in% filter_tipo else TRUE) %>%
    filter(if (!is.null(filter_tipo_cc)) tipo_CC %in% filter_tipo_cc else TRUE) %>%
    mutate(tipo_CC = toupper(tipo_CC)) %>% 
    select(-bd,-clase,-tipo)
  
  result <- result %>% 
    data_format_convert() %>% 
    group_by(gestion,tipo_CC,mes) %>% 
    summarise(cantidad = sum(cantidad)) %>% 
    ungroup()
  
  result <- result %>% 
    data_format_convert(type = "short", names = "tipo_CC")
  result <- result %>% 
    filter(FEMENINO > 0 | MASCULINO > 0) 
  return(data_to_show(result,init = "FEMENINO",fin = "MASCULINO")) 
}

process_altas <- function(data,filter_tipo = NULL,filter_clase = NULL,filter_bd = NULL,filter_tipo_cc = NULL){
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
bar_graph <- function(data, process = "start", theme_show = "default") {
  # Inicializa el gráfico
  graf <- data %>% 
    e_charts(mes) 
  
  # Agrega barras para datos regionales si se especifica
  if (process == "start") {
    graf <- graf %>% 
      e_bar(REGIONALES,
            itemStyle = list( color = "#073767",
                              borderRadius = 15,
                              shadowColor = "black",
                              shadowBlur = 20),
            stack = "grp",
            label = list(show = TRUE,
                         formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")))
    graf <- graf %>% 
      e_bar(OFICINA_CENTRAL, 
            seriesName = "Oficina central", 
            itemStyle = list(color = "#FFC502",
                             borderRadius = 10,
                             shadowColor = "black",
                             shadowBlur = 10),
            stack = "grp",
            label = list(show = TRUE,
                         formatter  = htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")))
  }
  if (process == "compensation" || process == "cursoPago") {
    graf <- graf %>% 
      e_bar(GLOBAL,
            seriesName = "GLOBAL",
            itemStyle = list( color = "#073767",
                              borderRadius = 15,
                              shadowColor = "black",
                              shadowBlur = 20),
            stack = "grp",
            label = list(show = TRUE,
                         formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")))
    graf <- graf %>% 
      e_bar(MENSUAL, 
            seriesName = "MENSUAL", 
            itemStyle = list(color = "#FFC502",
                             borderRadius = 10,
                             shadowColor = "black",
                             shadowBlur = 10),
            stack = "grp",
            label = list(show = TRUE,
                         formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")))
  }
  # Agrega etiquetas a las barras
  
  
  # Agrega línea para mostrar el total si se especifica
  if (process != "cursoPago") {
    graf <- graf %>% 
      e_line(TOTAL, 
             itemStyle = list(width=10, color = "#202C33"), 
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = 16,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                              ")),
             lineStyle = list(width = 2))
  }
  
  # Configura el tema del gráfico
  graf <- graf %>% 
    # Agrega función de zoom
    e_datazoom(
      orient = "horizontal",  
      height = 20,
      text = "MES",
      padding = c(20,20),
      handleSize = 1,
      textStyle = list(fontSize = 10),
      fillerColor = "#3398DB"
    ) %>%
    # Agrega tooltips
    e_tooltip() %>%
    # Agrega función de descarga de imagen
    e_toolbox_feature(feature = "saveAsImage") |>
    # Configura leyenda
    e_legend(orient = "horizontal", top = "bottom", padding = c(0,0)) %>% 
    e_animation(duration = 1500) |>
    # Marca el área de las barras
    e_mark_area(data = list(list(xAxis = 0),
                            list(xAxis = 1)
    ),
    itemStyle = list(color = color_mark,
                     opacity = 0.1)
    )
  
  return(graf)
}

graf_curso <- function(data,theme_show = "dark",with_total = FALSE){
  grafi <- data %>% 
    e_charts(mes)
  
  grafi <- grafi %>% 
    e_bar(GLOBAL,
          seriesName = "GLOBAL",
          itemStyle = list( color = "#FFC502",
                            borderRadius = 15,
                            shadowColor = "black",
                            shadowBlur = 20),
          stack = "grp")
  grafi <- grafi %>% 
    e_bar(MENSUAL, 
          seriesName = "MENSUAL", 
          itemStyle = list(color = "#073767",
                           borderRadius = 10,
                           shadowColor = "black",
                           shadowBlur = 10),
          stack = "grp")
  grafi <- grafi %>% 
    e_labels(position = "inside", 
             fontWeight = "bold", 
             fontSize = 16,
             formatter  =  htmlwidgets::JS("
                 function(params){
                   var value = params.value[1];
                   var parts = value.toString().split('.');
                   parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                   return parts.join('.');
                 }
                ")) |>
    # Marca el área de las barras
    e_mark_area(data = list(list(xAxis = 0),
                            list(xAxis = 1)
    ),
    itemStyle = list(color = "#00aae495",
                     opacity = 0.1)
    )
  if (with_total == TRUE) {
    grafi <- grafi %>% 
      e_line(TOTAL, 
             itemStyle = list(width=10, color = "#202C33"), 
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = 16,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                                 var value = params.value[1];
                                 var parts = value.toString().split('.');
                                 parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                 return parts.join('.');
                               }
                              ")),
             lineStyle = list(width = 2))
  }
  
  grafi <- grafi %>% 
    # Agrega función de zoom
    e_datazoom(
      orient = "horizontal",  
      height = 20,
      text = "MES",
      padding = c(20,20),
      handleSize = 1,
      textStyle = list(fontSize = 10),
      fillerColor = "#3398DB"
    ) %>%
    # Agrega tooltips
    e_tooltip() %>%
    # Agrega función de descarga de imagen
    e_toolbox_feature(feature = "saveAsImage") |>
    # Configura leyenda
    e_legend(orient = "horizontal", top = "bottom", padding = c(0,0)) %>% 
    e_animation(duration = 1500) 
  return(grafi)
}

graf_sex <- function(data,theme_show = "dark",with_total = FALSE){
  grafi <- data %>% 
    e_charts(mes)
  
  grafi <- grafi %>% 
    e_bar(FEMENINO,
          seriesName = "GLOBAL",
          itemStyle = list( color = "#FFC502",
                            borderRadius = 15,
                            shadowColor = "black",
                            shadowBlur = 20),
          stack = "grp")
  grafi <- grafi %>% 
    e_bar(MASCULINO, 
          seriesName = "MENSUAL", 
          itemStyle = list(color = "#073767",
                           borderRadius = 10,
                           shadowColor = "black",
                           shadowBlur = 10),
          stack = "grp")
  grafi <- grafi %>% 
    e_labels(position = "inside", 
             fontWeight = "bold", 
             fontSize = 16,
             formatter  =  htmlwidgets::JS("
                function(params){
                   var value = params.value[1];
                   var parts = value.toString().split('.');
                   parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                   return parts.join('.');
                 }
                ")) |>
    # Marca el área de las barras
    e_mark_area(data = list(list(xAxis = 0),
                            list(xAxis = 1)
    ),
    itemStyle = list(color = "#00aae495",
                     opacity = 0.1)
    )
  if (with_total == TRUE) {
    grafi <- grafi %>% 
      e_line(TOTAL, 
             itemStyle = list(width=10, color = "#202C33"), 
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = 16,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                                 var value = params.value[1];
                                 var parts = value.toString().split('.');
                                 parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                 return parts.join('.');
                               }
                              ")),
             lineStyle = list(width = 2))
  }
  
  grafi <- grafi %>% 
    # Agrega función de zoom
    e_datazoom(
      orient = "horizontal",  
      height = 20,
      text = "MES",
      padding = c(20,20),
      handleSize = 1,
      textStyle = list(fontSize = 10),
      fillerColor = "#3398DB"
    ) %>%
    # Agrega tooltips
    e_tooltip() %>%
    # Agrega función de descarga de imagen
    e_toolbox_feature(feature = "saveAsImage") |>
    # Configura leyenda
    e_legend(orient = "horizontal", top = "bottom", padding = c(0,0)) %>% 
    e_animation(duration = 1500) 
  return(grafi)
}

graf_four_bars <- function(data,theme_show = "dark",with_total = FALSE){
  grafi <- data %>% 
    e_charts(mes)
  
  grafi <- grafi %>% 
    e_bar(AUTOMATICO_MENSUAL, 
          seriesName = "AUTOMATICO MENSUAL", 
          itemStyle = list(color = "#00CED1",
                           borderRadius = 10,
                           shadowColor = "black",
                           shadowBlur = 10),
          stack = "grp",
          label = list(show = TRUE, 
                       position = "insideRight", 
                       fontWeight = "bold", 
                       fontSize = 16,
                       formatter  =  htmlwidgets::JS("
                              function(params){
                                 var value = params.value[1];
                                 var parts = value.toString().split('.');
                                 parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                 return parts.join('.');
                               }
                              ")))
  
  grafi <- grafi %>% 
    e_bar(AUTOMATICO_GLOBAL,
          seriesName = "AUTOMATICO GLOBAL",
          itemStyle = list( color = "#FF5733",
                            borderRadius = 15,
                            shadowColor = "black",
                            shadowBlur = 20),
          stack = "grp",
          label = list(show = TRUE, 
                       position = "insideLeft", 
                       fontWeight = "bold", 
                       fontSize = 16,
                       formatter  =  htmlwidgets::JS("
                              function(params){
                                 var value = params.value[1];
                                 var parts = value.toString().split('.');
                                 parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                 return parts.join('.');
                               }
                              ")))
  
  grafi <- grafi %>% 
    e_bar(MANUAL_MENSUAL, 
          seriesName = "MANUAL MENSUAL", 
          itemStyle = list(color = "#073767",
                           borderRadius = 10,
                           shadowColor = "black",
                           shadowBlur = 10),
          stack = "grp",
          label = list(show = TRUE, 
                       position = "inside", 
                       fontWeight = "bold", 
                       fontSize = 16,
                       formatter  =  htmlwidgets::JS("
                              function(params){
                                 var value = params.value[1];
                                 var parts = value.toString().split('.');
                                 parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                 return parts.join('.');
                               }
                              ")))
  
  grafi <- grafi %>% 
    e_bar(MANUAL_GLOBAL,
          seriesName = "MANUAL GLOBAL",
          itemStyle = list( color = "#FFC502",
                            borderRadius = 15,
                            shadowColor = "black",
                            shadowBlur = 20),
          stack = "grp",
          label = list(show = TRUE, 
                       position = "inside", 
                       fontWeight = "bold", 
                       fontSize = 16,
                       formatter  =  htmlwidgets::JS("
                              function(params){
                                 var value = params.value[1];
                                 var parts = value.toString().split('.');
                                 parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                 return parts.join('.');
                               }
                              ")))
  
  grafiss <- grafi %>% 
    e_labels(position = "inside", 
             fontWeight = "bold", 
             fontSize = 16,
             formatter  =  htmlwidgets::JS("
                function(params){
                   var value = params.value[1];
                   var parts = value.toString().split('.');
                   parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                   return parts.join('.');
                 }
                ")) |>
    # Marca el área de las barras
    e_mark_area(data = list(list(xAxis = 0),
                            list(xAxis = 1)
    ),
    itemStyle = list(color = "#00aae495",
                     opacity = 0.1)
    )
  if (with_total == TRUE) {
    grafi <- grafi %>% 
      e_line(TOTAL, 
             itemStyle = list(width=10, color = "#202C33"), 
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = 16,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                                 var value = params.value[1];
                                 var parts = value.toString().split('.');
                                 parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                 return parts.join('.');
                               }
                              ")),
             lineStyle = list(width = 2))
  }
  
  grafi <- grafi %>% 
    # Agrega función de zoom
    e_datazoom(
      orient = "horizontal",  
      height = 20,
      text = "MES",
      padding = c(20,20),
      handleSize = 1,
      textStyle = list(fontSize = 10),
      fillerColor = "#3398DB"
    ) %>%
    # Agrega tooltips
    e_tooltip() %>%
    # Agrega función de descarga de imagen
    e_toolbox_feature(feature = "saveAsImage") |>
    # Configura leyenda
    e_legend(orient = "horizontal", top = "bottom", padding = c(0,0)) %>% 
    e_animation(duration = 1500) 
  return(grafi)
}

data_table <- function(table,source,prepared = 'Area de planificación',other,title_other = '(*): '){
 n <- ncol(table)
 result <- datatable(head(table),
            extensions = c('Select','FixedColumns'),
            options = list(
              dom = 't',
              select = list(style = 'os', items = 'row'),
              selection = 'none',
              initComplete = JS('
               function(settings, json) {
                $("table.dataTable th").css("font-size", "10px");
                $("table.dataTable td").css("font-size", "12px");
               }'),
              columnDefs = list(
                list(className = 'dt-right', targets = c(2:n-1))# Alinea todas las demás columnas a la derecha
              )
            ),
            colnames = c(' ' = 'TIPO'),
            class = 'cell-border stripe',
            rownames = FALSE,
            caption = htmltools::tags$caption(
              style = 'caption-side: bottom; text-align: left;',
              htmltools::strong('Fuente : '), htmltools::em(source),
              htmltools::br(),
              htmltools::strong('Elaborado: '), htmltools::em(prepared),
              htmltools::br(),
              htmltools::strong(title_other), htmltools::em(other)
            )
  )
 return(result)
}

# Función para crear un gráfico circular
#data: Un dataframe que contiene dos columnas: 
#      model (una variable categórica) y CANTIDAD (una variable numérica).
#theme_show: Un string que indica el tema a utilizar para el gráfico.
#           Las opciones son "default", "light", "dark", "roma", "vintage", "walden",
#                            "macarons", "infographic", "shine" y "westeros".

grafico_pie <- function(data, theme_show = "default", position_type = "inside", rotate_type = 0, name_pie = "PIE") {
  
  # Se seleccionan las primeras 6 filas del dataframe
  data <- head(data)
  
  # Se convierten los nombres de las filas en una columna llamada "model"
  data <- tibble::rownames_to_column(data, "model")
  
  fontSize_large <- 24
  fontSize_small <- 16
  
  # Código JavaScript para detectar el ancho de la pantalla y ajustar el tamaño de la fuente
  js_code <- '
(function(){
  var fontSize = window.innerWidth > 768 ? fontSize_large : fontSize_small;
  var title = {
    textStyle: {
      fontSize: fontSize
    }
  };
  return title;
})()
'
  # Se crea el gráfico circular utilizando la librería echarts4r
  data %>%
    e_charts(TIPO) %>%
    e_pie(CANTIDAD,
          label = list(
            formatter = htmlwidgets::JS("function(params) {
              return params.percent + '%';
            }"),
            rotate = rotate_type,
            position = position_type,
            fontStyle = "oblique",
            fontWeight = "bold"),
          color = c('#FFC502','#073767','#00CED1','#FF5733')) %>%
    e_tooltip() %>%
    e_legend(orient = "vertical", 
             show = FALSE,
             right = "1%", 
             padding = c(10),
             textStyle = list(fontWeight = "bold", 
                              fontSize = 8)) %>% 
    e_title(text = paste0("Mes: ",name_pie), 
            left = "center", 
            top = "1%",
            textStyle = list(fontWeight = "bold")) %>% 
    e_grid(height = "10%", top = "10%")
}


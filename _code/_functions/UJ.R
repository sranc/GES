dir <- "../_data/uj/data"
source("../../_code/_functions/DFC.R")
#############################################      funciones privadas     #############################################

get_data_graf <- function(data,ges_pass = FALSE, MES = "DIC"){
  new_data <- data %>% 
    filter(gestion == max(gestion))
  if(ges_pass == TRUE){
    dic_lastgestion_1 <- data %>% 
      filter(gestion == max(gestion) - 1 & mes == MES) %>% 
      mutate(mes = paste0(mes, " ",gestion, " (*)"))
    
    dic_lastgestion_2 <- data %>% 
      filter(gestion == max(gestion) - 2 & mes == MES) %>% 
      mutate(mes = paste0(mes," ",gestion, " (*)"))
    
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

data_to_show <- function(data, init = "ENE", fin = "DIC", fin_2 = NULL){
  mes = toString(tail(data,1)$mes)
  bar <- get_data_graf(data,TRUE, MES = mes)
  
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

m_beneficiarios <- function(result,name) {
  result <- result %>% 
    group_by(gestion,tipo_juridica,mes) %>% 
    summarise(cantidad = sum(cantidad), .groups = "drop") %>% 
    ungroup()
  result <- data_format_convert(result,type = "short") %>% 
    select(gestion, tipo_juridica, ENE, FEB, MAR, ABR, MAY, JUN, JUL, AGO, SEP, OCT, NOV, AGUI, DIC) %>% 
    data_format_convert() %>% 
    data_format_convert(type = "short",names = "tipo_juridica",values = "cantidad") 
  result <- result %>% 
    filter(`MONTOS RECUPERADOS` > 0)
  result <- result %>% 
    group_by(gestion) %>% 
    mutate(`MONTOS RECUPERADOS` = cumsum(`MONTOS RECUPERADOS`)) %>% 
    ungroup()
  result <- result %>% 
    rename(!!name := `MONTOS RECUPERADOS`)
  return(result)
}

recuperacion <- function(result) {
  result <- result %>% 
    group_by(gestion,tipo_juridica,mes) %>% 
    summarise(cantidad = sum(cantidad), .groups = "drop") %>% 
    ungroup()
  result <- data_format_convert(result,type = "short") %>% 
    select(gestion, tipo_juridica, ENE, FEB, MAR, ABR, MAY, JUN, JUL, AGO, SEP, OCT, NOV, AGUI, DIC) %>% 
    data_format_convert() %>% 
    data_format_convert(type = "short",names = "tipo_juridica",values = "cantidad") 
  result <- result %>% 
    filter(RECUPERACION > 0)
  result <- result %>% 
    group_by(gestion) %>% 
    mutate(RECUPERACION = cumsum(RECUPERACION)) %>% 
    ungroup()
  return(result)
}

###################################################      funciones publicas     ####################################

join_gestions <- function(data_directory){
  # join_gestions: Función que combina datos de archivos Parquet en un solo data frame.
  # Parámetros:
  #   - data_directory: Ruta al directorio que contiene archivos Parquet.
  # Devuelve:
  #   - Un data frame que contiene la combinación de todos los datos de los archivos Parquet.
  
  data_list <- list()
  
  for (parquet_field in list.files(path = data_directory, pattern = "*.parquet", full.names = TRUE)) {
    db_name <- tools::file_path_sans_ext(basename(parquet_field))
    
    parquet_data <- read_parquet(parquet_field)
    
    data_list[[db_name]] <- parquet_data
  }
  merge_data <- bind_rows(data_list)
  merge_data <- merge_data %>% 
    select(bd,gestion:DIC) 
  
  result <- data_format_convert(merge_data)
  
  return(result)
}

consulta <- function(cc_data,filter_clase = NULL,filter_tipo = NULL,filter_tipo_juridica = NULL,filter_bd = NULL,type_resp = NULL, month = NULL,new_name) {
  result <- cc_data %>% 
    filter(if (!is.null(filter_bd)) bd %in% filter_bd else TRUE) %>%
    filter(if (!is.null(filter_clase)) clase %in% filter_clase else TRUE) %>%
    filter(if (!is.null(filter_tipo)) tipo %in% filter_tipo else TRUE) %>%
    filter(if (!is.null(filter_tipo_juridica)) tipo_juridica %in% filter_tipo_juridica else TRUE)
  
  if(type_resp == "m_beneficiarios"){
    result <- m_beneficiarios(result, name = new_name)
    result <- data_to_show(result,
                           init = new_name,
                           fin = new_name)
  }
  if(type_resp == "recuperacion"){
    result <- recuperacion(result)
    result <- data_to_show(result,init = "RECUPERACION",fin = "RECUPERACION")
  }
  if(type_resp == "sn_beneficiarios"){
    result <- sn_beneficiarios(result)
    result <- data_to_show(result,init = "Femenino",fin = "Masculino")
  }
  if(type_resp == "dn_beneficiarios"){
    result <- dn_beneficiarios(result,month)
  }
  return(result)
}

cant_mont <- function(t_cant,t_mont){
  bar <- left_join(t_cant, t_mont, by =  c("gestion", "mes"))
  bar <- bar %>% 
    mutate(
      `APORTES DEVENGADOS` = round(`APORTES DEVENGADOS`),
      `COBROS INDEBIDOS` = round(`COBROS INDEBIDOS`),
      TOTAL = round(`APORTES DEVENGADOS` + `COBROS INDEBIDOS`)
    )

  table_data <- data_format_convert(bar,col_ini = "COBROS INDEBIDOS",col_fin = "TOTAL",names = "TIPO",values = "CANTIDAD" )
  table_data <- table_data %>% 
    select(TIPO,MES=mes,CANTIDAD) %>% 
    mutate(CANTIDAD = format(CANTIDAD, big.mark = ",",decimal.mark = "."))
  table_data <- data_format_convert(table_data,type = "short",names = "MES",values = "CANTIDAD")
  
  pie_data <- tail(bar,1) %>% 
    select(mes,`APORTES DEVENGADOS`,`COBROS INDEBIDOS`) 
  
  pie_data <- data_format_convert(pie_data,col_ini = "APORTES DEVENGADOS",col_fin = "COBROS INDEBIDOS",names = "TIPO")
  pie_data <- data_format_convert(pie_data,type = "short")
  
  gestion_before_names <- bar %>% 
    select(gestion) %>% 
    group_by(gestion) %>% 
    ungroup()
  
  gestion_before_names <- unique(unlist(gestion_before_names$gestion))
  gestion_before_names <- as.array(gestion_before_names)
  return(list("bar" = bar, "table" = table_data, "pie" = pie_data, "gestion_name" = gestion_before_names))
}
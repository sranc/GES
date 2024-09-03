dir <- "../_data/fc/data"
source("../../_code/_functions/DFC.R")
color_mark <-  "#909090"
color_line <-  c("#404040","#FFA500")
color_bar <- c('#FFA500','#073767','#00CED1','#FF5733',"#3398DB")

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

data_to_show <- function(data, init = "ENE", fin = "DIC", mill = FALSE, total = FALSE){
  mes = toString(tail(data,1)$mes)
  bar <- get_data_graf(data,TRUE, MES = mes)
  
  last_month <- tail(bar$mes,1)
  
  bar <- bar %>% 
    mutate(mes = ifelse(mes == last_month, paste0(last_month," (*)"), mes))
  
  if(mill == TRUE){
    bar <- bar %>% 
      mutate(`RECUPERACIÓN Bs` = round(`RECUPERACIÓN Bs`/1000000,2))
  }
  
  if(total == TRUE){
    col_names <- names(data)[(ncol(data) - 1):ncol(data)]
    bar <- bar %>% 
      mutate(TOTAL = rowSums(select(., all_of(col_names))))
  }
  
  table_data <- data_format_convert(bar,col_ini = init,col_fin = fin,names = "TIPO",values = "CANTIDAD" )
  table_data <- table_data %>%
    select(TIPO,MES=mes,CANTIDAD) %>%
    mutate(CANTIDAD = format(CANTIDAD, big.mark = ",",decimal.mark = "."))
  table_data <- data_format_convert(table_data,type = "short",names = "MES",values = "CANTIDAD")

  pie_data <- tail(bar,1) %>%
    select(mes,c(!!sym(init): !!sym(fin))) %>%
    mutate(MES = mes, mes = "CANTIDAD")
  pie_data <- data_format_convert(pie_data,col_ini = init,col_fin = fin,names = "TIPO")
  pie_data <- data_format_convert(pie_data,type = "short")
  
  pie_data <- pie_data %>% 
    filter(TIPO != "TOTAL")

  gestion_before_names <- bar %>%
    select(gestion) %>%
    group_by(gestion) %>%
    ungroup()

  gestion_before_names <- unique(unlist(gestion_before_names$gestion))
  gestion_before_names <- as.array(gestion_before_names)
 
  return(list("bar" = bar, "table" = table_data, "pie" = pie_data, "gestion_name" = gestion_before_names))
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

consulta <- function(data,type_resp) {
  result <- data %>% 
    select(gestion,tipo_fisca,mes,cantidad) %>% 
    data_format_convert(type = "short",names = "tipo_fisca",values = "cantidad") %>% 
    filter(`Pequeñas` > 0 | Grandes > 0 | Privada > 0 | Publica > 0 | FISCALIZACIONES > 0 | `RECUPERACIÓN MM` > 0 | `RECUPERACIÓN Bs` > 0) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  if(type_resp == "tamano"){
    result <- result %>% 
      select(gestion,mes,`Pequeñas`,Grandes)
    result <- data_to_show(result,
                         init = "Pequeñas",
                        fin = "TOTAL",
                        total = TRUE)
  }
  if(type_resp == "tipo"){
    result <- result %>% 
      select(gestion,mes,Privada,Publica)
    result <- data_to_show(result,
                           init = "Privada",
                           fin = "TOTAL",
                           total = TRUE)
  }
  if(type_resp == "comportamiento"){
    result <- result %>% 
      select(gestion,mes, FISCALIZACIONES, `RECUPERACIÓN Bs`)
    result <- data_to_show(result,
                           init = "FISCALIZACIONES",
                           fin = "RECUPERACIÓN Bs",
                           mill = TRUE)
  }
  return(result)
}

cant_mont <- function(t_cant,t_mont){
  bar <- left_join(t_cant, t_mont, by =  c("gestion", "mes"))
  bar <- bar %>% 
    mutate(TOTAL = `APORTES DEVENGADOS` + `COBROS INDEBIDOS`)
  
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


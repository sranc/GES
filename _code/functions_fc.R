dir <- "../_data/fc/data"
color_mark <-  "#909090"
color_line <-  c("#404040","#FFA500")
color_bar <- c('#FFA500','#073767','#00CED1','#FF5733',"#3398DB")
#############################################      funciones privadas     #############################################

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

################################################      Graficas    ################################################

grafica <- function(bd, with_total = FALSE){
  n_columnas <- ncol(bd)
  names <- names(bd)
  if (n_columnas == 4) {
    legend_names <- names[3:4]
    names[3:4] <- c("cantidad1","total")
    Y <- 1
    total_color <- color_line[2]
  }else if (n_columnas == 5){
    legend_names <- names[3:5]
    names[3:5] <- c("cantidad1","cantidad2","total")
    Y <- 0
    total_color <- color_line[1]
  }
  
  names(bd) <- names
  
  grafi <- bd %>% 
    e_charts(mes)
  if(n_columnas == 5){
    grafi <- grafi %>% 
      e_bar(cantidad2,
          itemStyle = list( color = color_bar[1],
                            borderRadius = 10,
                            shadowColor = "black",
                            shadowBlur = 5),
          stack = "grp",
          name = legend_names[2],
          label = list(show = TRUE,
                       fontWeight = "bold", 
                       fontSize = 16,
                       backgroundColor = color_bar[1],
                       formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")))
  }
  grafi <- grafi %>% 
    e_bar(cantidad1, 
          itemStyle = list(color = color_bar[2],
                           borderRadius = 10,
                           shadowColor = "black",
                           shadowBlur = 5),
          stack = "grp",
          name = legend_names[1],
          label = list(show = TRUE,
                       fontWeight = "bold", 
                       fontSize = 16,
                       backgroundColor = color_bar[2],
                       formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")))
  grafi <- grafi %>% 
    # Marca el área de las barras
    e_mark_area(data = list(list(xAxis = 0),
                            list(xAxis = 1)
    ),
    itemStyle = list(color = color_mark,
                     opacity = 0.1)
    )
  if (with_total == TRUE) {
    grafi <- grafi %>% 
      e_line(total, 
             itemStyle = list(width=10, color = total_color), 
             symbol = "diamond", 
             symbolSize = 12, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = 18,
                          color = total_color,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                                var value = params.value[1];
                                var parts = value.toString().split('.');
                                parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                return parts.join('.');
                              }
                              ")),
             lineStyle = list(width = 3),
             name = legend_names[length(legend_names)],
             y_index = Y)
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
      fillerColor = color_bar[5]
    ) %>%
    # Agrega tooltips
    e_tooltip(trigger = "axis") %>%
    # Agrega función de descarga de imagen
    e_toolbox_feature(feature = "saveAsImage") |>
    # Configura leyenda
    e_legend(orient = "horizontal", top = "bottom", padding = c(0,0)) %>% 
    e_axis_labels(x = "Mes")
  return(grafi)
}

data_table <- function(table,source,prepared = 'Area de planificación',other,title_other = '(*) '){
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

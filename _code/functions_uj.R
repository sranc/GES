dir <- "../_data/uj/data"
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

graf_uno <- function(data){
  names <- names(data)
  legend_name <- names[3]
  names[3] <- "Cantidad"
  names(data) <- names
  
  grafi <- data %>% 
    e_charts(mes)
  
  grafi <- grafi %>% 
    e_bar(Cantidad, 
          seriesName = "Cantidad",
          itemStyle = list(color = color_bar[2],
                           borderRadius = 10,
                           shadowColor = "black",
                           shadowBlur = 10),
          stack = "grp",
          name = legend_name)
  grafi <- grafi %>% 
    e_labels(position = "inside", 
             fontWeight = "bold", 
             fontSize = 16,
             formatter  =  htmlwidgets::JS("
                function(params){
                   value = params.value[1];
                   value = parseInt(value,10);
                   value_format = value.toLocaleString();
                return(
                  value_format
                ) }
                ")) |>
    # Marca el área de las barras
    e_mark_area(data = list(list(xAxis = 0),
                            list(xAxis = 1)
    ),
    itemStyle = list(color = color_mark,
                     opacity = 0.1)
    )
  
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
    e_tooltip() %>%
    # Agrega función de descarga de imagen
    e_toolbox_feature(feature = "saveAsImage") |>
    # Configura leyenda
    e_legend(orient = "horizontal", top = "bottom", padding = c(0,0),legenName = "Anexo") %>% 
    e_animation(duration = 1500) 
  return(grafi)
}

graf_dos <- function(data,with_total = FALSE){
  
  names <- names(data)
  legend_names <- names[3:5]
  names[3:5] <- c("Cantidad1","cantidad2","Total")
  
  names(data) <- names
  
  grafi <- data %>% 
    e_charts(mes)
  
  grafi <- grafi %>% 
    e_bar(Cantidad1,
          seriesName = "Derechohabiente",
          itemStyle = list( color = color_bar[1],
                            borderRadius = 10,
                            shadowColor = "black",
                            shadowBlur = 5),
          stack = "grp",
          name = legend_names[1])
  grafi <- grafi %>% 
    e_bar(cantidad2, 
          seriesName = "Titular", 
          itemStyle = list(color = color_bar[2],
                           borderRadius = 10,
                           shadowColor = "black",
                           shadowBlur = 5),
          stack = "grp",
          name = legend_names[2])
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
    itemStyle = list(color = color_mark,
                     opacity = 0.1)
    )
  if (with_total == TRUE) {
    grafi <- grafi %>% 
      e_line(Total, 
             itemStyle = list(width=10, color = color_line[1]), 
             symbol = "diamond", 
             symbolSize = 12, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = 18,
                          color = color_line[1],
                          formatter  =  htmlwidgets::JS("
                              function(params){
                                var value = params.value[1];
                                var parts = value.toString().split('.');
                                parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                return parts.join('.');
                              }
                              ")),
             lineStyle = list(width = 3),
             name = legend_names[3])
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

grafico_pie <- function(data, theme_show = theme_graf, position_type = "inside", rotate_type = 0, name_pie = "PIE") {
  names <- names(data)
  legend_name <- names[2]
  names[2] <- "Cantidad"
  names(data) <- names
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
    e_pie(Cantidad,
          label = list(
            formatter = htmlwidgets::JS("function(params) {
              return params.percent + '%';
            }"),
            rotate = rotate_type,
            position = position_type,
            fontStyle = "oblique",
            fontWeight = "bold"),
            color = color_bar) %>%
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
                      class = 'stripe hover compact',
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

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
    graf <- graf %>% 
      e_bar(OFICINA_CENTRAL, 
            seriesName = "Oficina central", 
            itemStyle = list(color = "#FFC502",
                             borderRadius = 10,
                             shadowColor = "black",
                             shadowBlur = 10),
            stack = "grp",
            label = list(show = TRUE,
                         fontWeight = "bold", 
                         fontSize = 16,
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
    graf <- graf %>% 
      e_bar(MENSUAL, 
            seriesName = "MENSUAL", 
            itemStyle = list(color = "#FFC502",
                             borderRadius = 10,
                             shadowColor = "black",
                             shadowBlur = 10),
            stack = "grp",
            label = list(show = TRUE,
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
                          fontSize = 18,
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
          stack = "grp",
          label = list(show = TRUE,
                       fontWeight = "bold", 
                       fontSize = 16,
                       color = "#073767",
                       backgroundColor = "#FFC502",
                       formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")))
  grafi <- grafi %>% 
    e_bar(MENSUAL, 
          seriesName = "MENSUAL", 
          itemStyle = list(color = "#073767",
                           borderRadius = 10,
                           shadowColor = "black",
                           shadowBlur = 10),
          stack = "grp",
          label = list(show = TRUE,
                       fontWeight = "bold", 
                       fontSize = 16,
                       color = "#FFC502",
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
      e_line(TOTAL, 
             itemStyle = list(width=10, color = "#202C33"), 
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          border = "0",
                          fontSize = 18,
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
    itemStyle = list(color = color_mark,
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
                          fontSize = 18,
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

graf_four_bars <- function(data,theme_show = "dark",with_total = FALSE, NVars = 0){
  
  if(NVars == nrow(data) || NVars == 0){
    area_n <- 1
  }else{
    area_n <- 0
  }
  
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
                       backgroundColor = "#00CED1",
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
                       backgroundColor = "#FF5733",
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
                       backgroundColor = "#073767",
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
                       backgroundColor = "#FFC502",
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
                            list(xAxis = area_n)
    ),
    itemStyle = list(color = color_mark,
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
                          fontSize = 18,
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

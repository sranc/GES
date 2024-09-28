color_mark <-  "#909090"
color_line <-  c("#404040","#FFA500")
color_bar <- c('#FFA500','#073767','#00CED1','#FF5733',"#3398DB")
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
    e_legend(orient = "horizontal", top = "bottom", padding = c(0,0)) 
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

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

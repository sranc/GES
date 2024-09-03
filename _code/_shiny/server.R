library(shiny)
library(shinydashboard)
library(shinyjs)

server <- function(input, output) {
  
  valor1 <- reactive({
    cantidad <- totales_cantidad %>%
      filter(gestion == input$gestion) %>%
      select(cantidad) %>%
      pull()  # Extrae el valor como un vector
    
    # Formatear el número con separadores de miles y dos decimales
    prettyNum(cantidad, big.mark = ",", decimal.mark = ".", nsmall = 0)
  })
  valor2 <- reactive({
    cantidad <- totales_monto %>% 
      filter(gestion == input$gestion) %>% 
      select(cantidad) %>% 
      pull()  # Extrae el valor como un vector
    
    # Formatear el número con separadores de miles y dos decimales
    prettyNum(cantidad, big.mark = ",", decimal.mark = ".", nsmall = 0)
  })
  # Simulación de consulta a base de datos o cálculo para obtener los valores
  valor3 <- "73,911"  # Simula una consulta que devuelve 3000
  valor4 <- "1688 M Bs"  # Simula una consulta que devuelve 4000
  valor5 <- "707,250 M Bs"  # Simula una consulta que devuelve 5000
  valor6 <- "107 M Bs"  # Simula una consulta que devuelve 5000
  
  output$widget1 <- renderValueBox({
    valueBox(
      value = valor1(),
      subtitle = "N° Beneficiarios CC",
      icon = icon("chart-line"),
      color = "aqua"
    )
  })

  
  output$widget2 <- renderValueBox({
    valueBox(
      value = paste0(valor2()," M Bs"),
      subtitle = "Monto Desembolsado CC ",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$widget3 <- renderValueBox({
    valueBox(
      value = valor3,
      subtitle = "N° Beneficiarios RD",
      icon = icon("user-check"),
      color = "yellow"
    )
  })
  
  output$widget4 <- renderValueBox({
    valueBox(
      value = valor4,
      subtitle = "Monto Desembolsado RD",
      icon = icon("piggy-bank"),
      color = "purple"
    )
  })
  
  output$widget5 <- renderValueBox({
    valueBox(
      value = valor5,
      subtitle = "Monto recuperado",
      icon = icon("wallet"),
      color = "red"
    )
  })
  
  output$widget6 <- renderValueBox({
    valueBox(
      value = valor6,
      subtitle = "Monto Desembolsado RD",
      icon = icon("money-bill-wave"),
      color = "blue"
    )
  })
  
  # Datos procesados y filtrados según la selección CC
  datos_inicio <- reactive({
    CC %>%
      filter(gestion == input$gestion) %>%
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2])
  })
  
  datos_cantidad <- reactive({
    CC_cantidad %>% 
      filter(if (input$compensacion == "Todos") TRUE else subcategoria == tolower(input$compensacion)) %>%
      pivot_wider(names_from = subcategoria, values_from = cantidad, names_prefix = "cant_") %>% 
      mutate(total = rowSums(across(starts_with("cant_")), na.rm = TRUE)) %>% 
      select(gestion, mes, any_of(c("cant_global", "cant_mensual")), total) %>%
      filter(gestion == input$gestion) %>%
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2])
  })
  
  datos_monto <- reactive({
    
    CC_monto %>%
      filter(if (input$compensacion == "Todos") TRUE else subcategoria == tolower(input$compensacion)) %>%
      pivot_wider(names_from = subcategoria, values_from = cantidad, names_prefix = "mont_") %>%
      mutate(total = rowSums(across(starts_with("mont_")), na.rm = TRUE)) %>%
      # Reemplazar NA por 0 solo en las columnas que existen
      mutate(across(starts_with("mont_"), ~replace_na(., 0))) %>%
      mutate(total = replace_na(total, 0)) %>%
      # Seleccionar columnas condicionalmente si existen
      select(gestion, mes, any_of(c("mont_global", "mont_mensual")), total) %>%
      arrange(gestion, mes) %>%
      filter(gestion == input$gestion) %>%
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2])
  })
  
  datos_genero <- reactive({
    cc_genero %>%
      filter(gestion == input$gestion) 
  })
  
  datos_altas <- reactive({
    CC_altas %>%
      filter(gestion == input$gestion) 
  })
  
  datos_inicio_emitidos <- reactive({
    CC_inicio_emitidos %>%
      mutate(categoria = str_replace_all(categoria, " ", "_")) %>%
      pivot_wider(names_from = categoria, values_from = cantidad, names_prefix = "cant_") %>%
      filter(gestion == input$gestion) %>% 
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2])
  })
  
  # Generar el gráfico cantidad
  output$barras_gestion <- renderEcharts4r({
    datos <- datos_cantidad()
    num_meses <- if (length(unique(datos$mes)) > 7) 45 else 0
    # Crear gráfico de barras con total acumulado
    grafico <- datos %>%
      e_charts(mes)
    
    # Verificar si mont_global está presente y agregarlo al gráfico si es así
    if ("cant_global" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cant_global, 
              stack = "grp", 
              name = "Global", 
              itemStyle = list( color = "#FFC502",
                                borderRadius = 5,
                                shadowColor = "black",
                                shadowBlur = 2), 
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = FALSE)
    }
    
    # Verificar si mont_mensual está presente y agregarlo al gráfico si es así
    if ("cant_mensual" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cant_mensual, 
              stack = "grp", 
              name = "Mensual", 
              itemStyle = list(color = "#073767",
                               borderRadius = 5,
                               shadowColor = "black",
                               shadowBlur = 5),
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  = htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = FALSE)
    }
    
    # Agregar la línea del total
    grafico <- grafico %>%
      e_line(total, 
             itemStyle = list(width=10, color = "#202C33"), 
             name = "Total",
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = 12,
                          rotate = num_meses,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                              ")),
             lineStyle = list(width = 2)) %>% 
      e_tooltip(trigger = "axis") %>%  # Tooltip al pasar el mouse
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis(name = "Mes", type = "category") %>%
      e_y_axis(name = "Cantidad", axisLabel = list(rotate = 60)) 
    
    # Renderizar el gráfico
    grafico
  })
  
  # Generar el gráfico monto
  output$barras_monto <- renderEcharts4r({
    datos <- datos_monto()
    num_meses <- if (length(unique(datos$mes)) > 7) 45 else 0
    
    # Crear gráfico de barras dinámicamente según las columnas presentes
    grafico <- datos %>%
      e_charts(mes)
    
    # Verificar si mont_global está presente y agregarlo al gráfico si es así
    if ("mont_global" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(mont_global, 
              stack = "grp", 
              name = "Global", 
              itemStyle = list( color = "#FFC502",
                                borderRadius = 5,
                                shadowColor = "black",
                                shadowBlur = 2), 
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = TRUE)
    }
    
    # Verificar si mont_mensual está presente y agregarlo al gráfico si es así
    if ("mont_mensual" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(mont_mensual, 
              stack = "grp", 
              name = "Mensual", 
              itemStyle = list(color = "#073767",
                               borderRadius = 5,
                               shadowColor = "black",
                               shadowBlur = 5),
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  = htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = TRUE)
    }
    
    # Agregar la línea del total
    grafico <- grafico %>%
      e_line(total, 
             itemStyle = list(width=10, color = "#202C33"), 
             name = "Total",
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = 15,
                          rotate = num_meses,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                              ")),
             lineStyle = list(width = 2)) %>% 
      e_tooltip(trigger = "axis") %>%  # Tooltip al pasar el mouse
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis(name = "Mes", type = "category") %>%
      e_y_axis(name = "Cantidad") 
    
    # Renderizar el gráfico
    grafico
  })
  
  output$pie_genero <- renderEcharts4r({
    datos <- datos_genero()
    
    datos %>%
      e_charts(subcategoria) %>%
      e_pie(cantidad,label = list(
        formatter = htmlwidgets::JS("function(params) {
              return params.percent + '%';
            }"),
        position = "inside",
        fontStyle = "oblique",
        fontWeight = "bold"),
        color = c('#FFA500','#073767')) %>% 
      e_tooltip() %>%
      e_legend(orient = "vertical", 
               show = FALSE,
               right = "1%", 
               padding = c(10),
               textStyle = list(fontWeight = "bold", 
                                fontSize = 8)) %>% 
      e_title(text = paste0("Mes: ",mes_selector(unique(as.character(datos$mes)))), 
              left = "center", 
              top = "1%",
              textStyle = list(fontWeight = "bold")) %>% 
      e_grid(height = "10%", top = "10%")
  })
  
  output$barras_alta <- renderEcharts4r({
    datos <- datos_altas()
    
    datos %>% 
      e_charts(categoria) %>% 
      e_bar(cantidad, 
            stack = "grp", 
            itemStyle = list( color = "#FFC502",
                              borderRadius = 5,
                              shadowColor = "black",
                              shadowBlur = 2), 
            label = list(show = FALSE,
                         fontWeight = "bold", 
                         formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
            legend = TRUE) %>% 
      e_labels(show = TRUE, position = "right") %>%  
      e_legend(bottom = 0) %>% 
      e_tooltip(trigger = "axis") %>%
      e_flip_coords() %>% 
      e_y_axis(axisLabel = list(rotate = 70))
  })
  
  output$lineas_inicio_emitido <- renderEcharts4r({
    datos <- datos_inicio_emitidos()
    
    datos %>% 
      e_charts(mes) %>%
      e_line(cant_inicio_tramite,
             name = "inicio tramite", 
             itemStyle = list(color = "#FFC502"),
             label = list(show = TRUE),
             lineStyle = list(width = 2)) %>%
      e_line(cant_certificados_emitidos, 
             name = "certificados emitidos",
             itemStyle = list(color = "#073767"),
             label = list(show = TRUE,position = "bottom"),
             lineStyle = list(width = 2)) %>%
      e_tooltip(trigger = "axis") %>%  
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis(type = "category") %>%
      e_y_axis(axisLabel = list(rotate = 70)) 
  })
}

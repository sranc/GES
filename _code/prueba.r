
pacman::p_load(
  tidyverse,
  readxl,
  dplyr,
  sparklyr,
  ggplot2,
  plotly,
  DT,
  scales
)
reparto_cant <- read_excel("../_data/EstadisticaSistemaRepartoBeneficiario2023.xlsx")
reparto_mont <- read_excel("../_data/EstadisticaSistemaRepartoMontos2023.xlsx")
DIC_1 <- "DIC_2022"
DIC_2 <- "DIC_2021"
orden <- c(DIC_2, DIC_1, "ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV","AGUI", "DIC")

generar_data <- function(data, amount = FALSE){
  
  reparto_cant<- data %>% 
    select(gestion, clase, tipo, tipo_renta, ENE:DIC_2) %>% 
    pivot_longer(cols = c(ENE:DIC_2), names_to = "mes", values_to = "cantidad") %>%
    mutate(mes = case_when(
      mes == "DIC_1" ~ DIC_1,
      mes == "DIC_2" ~ DIC_2,
      TRUE ~ mes
    ))
  
  data <- reparto_cant %>% 
    filter(clase == "Sexo" & (tipo == "Titular" | tipo == "Derechohabiente")) %>% 
    group_by(mes,tipo) %>% 
    summarise(cantidad = sum(cantidad)) %>% 
    arrange(desc(tipo)) %>% 
    mutate(mes = factor(mes, levels = orden)) %>%
    filter(cantidad != 0)

  if(amount==TRUE) {
    data_before <- data %>% 
      filter(mes == DIC_1 | mes == DIC_2)
    data_actual <- data %>% 
      filter(mes != DIC_2) %>% 
      filter(mes != DIC_1) %>%
      group_by(tipo) %>%
      arrange(mes, tipo) %>%
      mutate(total_mes = cumsum(cantidad)) %>% 
      select(mes,tipo,cantidad = total_mes)
    data <- bind_rows(data_before,data_actual)%>%
      mutate(across(c(cantidad), ~ round(. / 1e6)))
  } 
  
  totales <- data %>%
    group_by(mes) %>%
    summarise(total = sum(cantidad)) 
  
  data_table <- merge(data, totales, by = "mes") 
  
  return(list("data_table" = data_table, "totales" = totales))
}

genera_data_table <- function(data){
  tabla_p <- data$data_table %>% 
    select(TIPO = tipo, mes, cantidad) %>% 
    mutate(cantidad = format(cantidad, big.mark = ".", decimal.mark = ",")) %>%
    pivot_wider(names_from = mes, values_from = cantidad) %>% 
    arrange(desc(TIPO)) %>%
    bind_rows(
      data$totales %>% 
        filter(total != 0) %>% 
        mutate(total = format(total, big.mark = ".", decimal.mark = ",")) %>%
        pivot_wider(names_from = mes, values_from = total) %>%
        mutate(TIPO = "Total")
    )
  return(generar_tabla(tabla_p))
}

genera_data_pie <- function(data, title){
  colors <- c('#FFC502','#073767')
  mes_fil <- data$data_table %>% 
    slice(n()) %>% 
    pull(mes) %>%
    as.character()
  
  torta <- data$data_table %>% 
    filter(mes == mes_fil) %>% 
    group_by(TIPO = tipo) %>% 
    summarise(cantidad = sum(cantidad))
  title <- paste(title, " - ", mes_selector(mes_fil))
  
  grafico <- generar_grafico_2(data = torta, label = "TIPO", value = "cantidad", hole = 0.4, colors = colors, title = title)
  
  return(grafico)
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

generar_grafico <- function(data, data_2 = data, bar_color, name_bar, name_line, line_color, line_text_color, x_var, y1_var, y2_var, tipo_var, titulo, ejex, ejey, ejey_2, two_data = FALSE) {
  # Crear el gráfico de barras
  bar_plot <- plot_ly(
    data = data,
    name = if (two_data) name_bar else NULL,
    x = ~get(x_var),
    y = ~get(y1_var),
    color = if (!two_data) {~get(tipo_var)} else NULL,
    colors = if (!two_data) bar_color else NULL,
    marker = if (two_data) list(color = bar_color) else NULL,
    type = "bar",
    text = ~paste(scales::number(get(y1_var), big.mark = ".", decimal.mark = ",", accuracy = 1)),
    textfont = list(size = 16, color = c()),
    insidetextanchor = "middle",
    hoverinfo = "text"
  ) %>% 
    layout(
      barmode = "stack",
      showlegend = TRUE,
      shapes = list(
        list(
          type = "line",
          x0 = 1.5,
          x1 = 1.5,
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = "red", dash = "dash", width = 3)
        )
      )
    )
  if (two_data) 
    {
      ay <- list(
        tickfont = list(color = "black"),
        overlaying = "y",
        side = "right",
        title = ejey_2)
    }
  
  font_prop <- list(
    color = "black",
    size = 18
  )
  
  # Crear el gráfico de líneas y puntos
  line_plot <- bar_plot %>% 
    add_trace(
      data = data_2,
      name = if (two_data) name_line else NULL,
      x = ~get(x_var),
      y = ~get(y2_var),
      yaxis = if (two_data) "y2" else NULL, 
      type = "scatter",
      mode = "lines+markers",
      line = list(color = line_color, width = 4.3),
      marker = list(color = line_color, size = 15, symbol = "diamond"),
      text = ~paste(scales::number(get(y2_var), big.mark = ".", decimal.mark = ",", accuracy = 1)),
      textposition = "top center",  
      textfont = list(size = 15, color = line_text_color),
      hoverinfo = "text",
      showlegend = two_data
    ) %>% 
    layout(
      title = list(text = titulo, font = font_prop, y = 0.965, x = 0.5, xanchor = 'center', yanchor =  'top'),
      yaxis2 = if (two_data) ay else NULL,
      xaxis = list(title = ejex),
      yaxis = list(title = ejey),
      legend = if (two_data) list(x = 1.05, y = 1, 
                    traceorder = 'normal', 
                    orientation = 'v', 
                    xanchor = 'left',
                    yanchor = 'top') else NULL
    ) %>%
    layout(
      xaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff')
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d","select2d","resetScale2d","hoverCompareCartesian","hoverClosest3d"),
      modeBarButtonsToAdd =c("drawline"),
      locale = "es"
    )
  
  return(line_plot)
}

generar_tabla <- function(tabla)
  {
  tabla <- datatable(tabla,
                     options = list(
                       searching = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       initComplete = JS('
                       function(settings, json) {
                       $("table.dataTable th, table.dataTable td").css("font-size", "14px");
                       }
                                         ')
                       )
                     )
  return(tabla)
}

generar_grafico_2 <- function(data, label, value, hole, colors, title){
  plot_ly(data = data,labels=~get(label),values=~get(value),type = "pie", hole=hole,
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 2))) %>% 
    layout(
      title = title,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d","select2d","resetScale2d","hoverCompareCartesian","hoverClosest3d"),
      locale = "es"
      )
}

beneficiario <- generar_data(data = reparto_cant)

monto <- generar_data(data = reparto_mont, amount = TRUE)

ben <- monto$data_table %>%
  pivot_wider(names_from = tipo, values_from = cantidad) %>%
  mutate(total = Titular + Derechohabiente)

  library(echarts4r)

ben |> 
  e_charts(mes) |> 
  e_bar(Titular, 
        seriesName = "Titular", 
        itemStyle = list(color = "#073767",
                         borderRadius = 20,
                         shadowColor = "black",
                         shadowBlur = 20),
        stack = "grp") |>  
  e_bar(Derechohabiente, 
        seriesName = "Derechohabiente", 
        itemStyle = list(color = "#FFC502", 
                     borderRadius = 20, 
                     shadowColor = "black", 
                     shadowBlur = 20
                     ),
        stack = "grp") |> 
  e_labels(position = "inside", 
           fontWeight = "bold", 
           fontSize = 16) |> 
  e_mark_area(data = list(list(xAxis = 0),
                          list(xAxis = 1)
                          ),
              itemStyle = list(color = "#073767",
                               opacity = 0.1)
              ) %>% 
  e_line(total, 
         itemStyle = list(width=20, color = "#073767"), 
         symbol = "diamond", symbolSize = 25, 
         label = list(show = TRUE, 
                      position = "top", 
                      fontWeight = "bold", 
                      fontSize = 16),
         lineStyle = list(width = 5)) |> 
  e_x_axis(name = "Mes", 
           nameLocation = "middle",
           nameTextStyle = list(fontWeight = "bold", 
                                fontSize = 16,
                                padding = c(20,0,0,0))) |>
  e_y_axis(name = "Cantidad",
           nameLocation = "middle",
           nameTextStyle = list(fontWeight = "bold", 
                                fontSize = 16,
                                padding = c(0,0,40,0))) |>
  e_title(text = "Nro DE BENEFICIARIOS DE RENTAS Y PENSIONES VITALICIAS", 
          left = "center", subtext = "DEL SISTEMA DE REPARTO",
          textStyle = list(color = "#333"),
          subtextStyle = list(fontWeight = "bold", 
                              fontSize = 18,
                              color = "#333")) |>
  e_toolbox_feature(feature = "saveAsImage") |>
  e_legend(orient = "vertical", left = "right", top = "top", padding = c(40,10))

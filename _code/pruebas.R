
pacman::p_load(
  tidyverse,
  readxl,
  dplyr,
  sparklyr,
  ggplot2,
  plotly,
  DT,
  scales,
  crosstalk
)

reparto_cant <- read_excel("_data/EstadisticaSistemaRepartoBeneficiario2023.xlsx")
reparto_mont <- read_excel("_data/EstadisticaSistemaRepartoMontos2023.xlsx")
DIC_1 <- "DIC_2022"
DIC_2 <- "DIC_2021"


reparto_cant_actual <- reparto_cant %>% 
  select(gestion, clase, tipo, tipo_renta, ENE:DIC_2)

cant_1 <- reparto_cant_actual %>% 
  pivot_longer(cols = c(ENE:DIC_2), names_to = "mes", values_to = "cantidad") %>%
  mutate(mes = case_when(
    mes == "DIC_1" ~ DIC_1,
    mes == "DIC_2" ~ DIC_2,
    TRUE ~ mes
  ))

orden_meses <- c(DIC_2, DIC_1, "ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV","AGUI", "DIC")

reparto_graf_1 <- cant_1 %>% 
  filter(clase == "Sexo" & (tipo == "Titular" | tipo == "Derechohabiente")) %>% 
  group_by(mes,tipo) %>% 
  summarise(cantidad = sum(cantidad)) %>% 
  arrange(desc(tipo)) %>% 
  mutate(mes = factor(mes, levels = orden_meses))

totales <- reparto_graf_1 %>%
  group_by(mes) %>%
  summarise(total = sum(cantidad)) 


reparto_graf_1 <- merge(reparto_graf_1, totales, by = "mes") %>%
  filter(cantidad != 0)

bar_plot <- ggplot(reparto_graf_1, aes(x = mes, y = cantidad, fill = tipo, text = paste("Cantidad: ", scales::number(cantidad, big.mark = ".", decimal.mark = ",", accuracy = 1), "<br>Tipo: ", tipo))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Nro DE BENEFICIARIOS DE RENTAS Y PENSIONES VITALICIAS DEL
SISTEMA DE REPARTO", x = "Mes", y = "Cantidad") +
  scale_fill_manual(values = c("Derechohabiente" = "#0072B2", "Titular" = "#E69F00"), name = NULL) +
  theme_minimal() +
  geom_vline(xintercept = 2.5, color = "red", linetype = "dashed", linewidth=1.5)  +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"# Ajuste de la alineación horizontal
  ) +
  geom_text(data = reparto_graf_1 ,
            aes(label = scales::number(cantidad, big.mark = ".", decimal.mark = ",", accuracy = 1), y = cantidad), 
            color = "black", vjust = -0.5, size = 4,
            position = position_stack(vjust = 0.5))

line_plot <- bar_plot +
  geom_line(aes(group = 1, y = total), data = reparto_graf_1, color = "#525659", size = 1.3) +
  geom_point(aes(y = total, text = paste("Total:", scales::number(total, big.mark = ".", decimal.mark = ",", accuracy = 1))), data = reparto_graf_1,color = "#525659", size = 2,fill="#525659") +
  geom_text(data = reparto_graf_1,
            aes(label = scales::number(total, big.mark = ".", decimal.mark = ",", accuracy = 1), y = total + 2000, text = NULL),
            vjust = -0.5, color = "#000000") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",", accuracy = 1))

graf_1 <- ggplotly(line_plot, tooltip = "text")

### MONTOs

reparto_cant_mont <- reparto_mont %>% 
  select(gestion, clase, tipo, tipo_renta, ENE:DIC_2)

mont_1 <- reparto_cant_mont %>% 
  pivot_longer(cols = c(ENE:DIC_2), names_to = "mes", values_to = "cantidad") %>%
  mutate(mes = case_when(
    mes == "DIC_1" ~ DIC_1,
    mes == "DIC_2" ~ DIC_2,
    TRUE ~ mes
  ))

reparto_graf_mont_1 <- mont_1 %>% 
  filter(clase == "Sexo" & (tipo == "Titular" | tipo == "Derechohabiente")) %>% 
  group_by(mes,tipo) %>% 
  summarise(cantidad = sum(cantidad)) %>% 
  arrange(desc(tipo))
reparto_graf_mont_1$mes <- factor(reparto_graf_mont_1$mes,levels = orden_meses)

reparto_graf_mont_1 <- reparto_graf_mont_1 %>%
  filter(cantidad != 0)

reparto_graf_mont_1_anterior <- reparto_graf_mont_1 %>% 
  filter(mes == DIC_1 | mes == DIC_2)

reparto_graf_mont_1_actual <- reparto_graf_mont_1 %>% 
  filter(mes != DIC_2) %>% 
  filter(mes != DIC_1)


reparto_graf_mont_1_actual <- reparto_graf_mont_1_actual %>%
  group_by(tipo) %>%
  arrange(mes, tipo) %>%
  mutate(total_mes = cumsum(cantidad)) %>% 
  select(mes,tipo,cantidad = total_mes)

reparto_graf_mont_1 <- bind_rows(reparto_graf_mont_1_anterior,reparto_graf_mont_1_actual)

totales <- reparto_graf_mont_1 %>% 
  group_by(mes) %>% 
  summarise(total = sum(cantidad))

reparto_graf_mont_1 <- merge(reparto_graf_mont_1, totales, by = "mes")

reparto_graf_mont_1_tabla <- reparto_graf_mont_1

reparto_graf_mont_1 <- reparto_graf_mont_1 %>%
  mutate(cantidad = round(cantidad / 1000000),  
         total = round(total / 1000000)) 


bar_plot_mont <- ggplot(reparto_graf_mont_1, aes(x = mes, y = cantidad, fill = tipo, text = paste("Cantidad: ", cantidad, "<br>Tipo: ", tipo))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "MONTO DESEMBOLSADO DE RENTAS Y PENSIONES VITALICIAS DEL SISTEMA
DE REPARTO (EXPRESADO EN MILLONES DE BOLIVIANOS)", x = "Mes", y = "Cantidad") +
  scale_fill_manual(values = c("Derechohabiente" = "#0072B2", "Titular" = "#E69F00"), name = NULL) +
  theme_minimal() +
  geom_vline(xintercept = 2.5, color = "red", linetype = "dashed", linewidth=1.5)  +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"# Ajuste de la alineación horizontal
  ) +
  geom_text(data = reparto_graf_mont_1 ,
            aes(label = scales::number(cantidad, big.mark = ".", decimal.mark = ",", accuracy = 1), y = cantidad), 
            color = "black", vjust = -0.5, size = 4,
            position = position_stack(vjust = 0.5))

line_plot_mont <- bar_plot_mont +
  geom_line(aes(group = 1, y = total), data = reparto_graf_mont_1, color = "#525659", linewidth = 1.3) +
  geom_point(aes(y = total, text = paste("Total:", total)), data = reparto_graf_mont_1,color = "#525659", size = 2,fill="#525659") +
  geom_text(data = reparto_graf_mont_1,
            aes(label = total, y = total + 150, text = NULL),
            vjust = -0.5, color = "#000000") 

ggplotly(line_plot_mont, tooltip = "text")


datos_combinados <- rbind(
  transform(totales, tipo = "Cantidad"),
  transform(totales_mont, tipo = "Monto")
)

barra <- ggplot(datos_combinados, aes(x = mes, y = total, fill = tipo,group = tipo, text = paste("Cantidad: ", scales::number(total, big.mark = ".", decimal.mark = ",", accuracy = 1), "<br>Tipo: ", tipo))) +
  geom_bar(data = subset(datos_combinados, tipo == "Cantidad"), stat = "identity") +
  scale_fill_manual(values = c("Cantidad" = "#073767", "Monto" = "#FFC502"), name = NULL) +
  geom_vline(xintercept = 2.5, color = "red", linetype = "dashed", linewidth=1.5) +
  geom_text(data = subset(datos_combinados, tipo == "Cantidad") ,
            aes(label = scales::number(total, big.mark = ".", decimal.mark = ",", accuracy = 1), y = total), 
            color = "white", vjust = -0.5, size = 5, fontface = "bold",
            position = position_stack(vjust = 0.5)) 

grafico_combinado <- barra  +
  geom_line(data = subset(datos_combinados, tipo == "Monto"), aes(y = total / 0.05 ), color = "#FFC502",linewidth = 1.3) +  # Ajusta el factor de escala según tu necesidad
  geom_point(data = subset(datos_combinados,tipo == "Monto"),aes(y = total/0.05),color = "#FFC502",size = 3) +
  geom_text(data = subset(datos_combinados, tipo == "Monto"),
            aes(label = scales::number(total, big.mark = ".", decimal.mark = ",", accuracy = 1), y = (total / 0.05) + 5000, text = NULL),
            vjust = -0.5, color = "#FFC502",size = 5, fontface = "bold") +
  labs(title = "MONTO DESEMBOLSADO Y Nro. DE BENEFICIARIOS DEL SISTEMA DE REPARTO", x = "Mes", y = "Total") +
  theme_minimal()

ggplotly(grafico_combinado, tooltip = "text")  %>%
  layout(
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = "Segundo eje y"  # Puedes ajustar el título según tus necesidades
    )
  )


fig <- plot_ly()
# Add traces
fig <- fig %>% 
  add_trace(data = subset(datos_combinados, tipo == "Cantidad"),
            x = ~mes, y = ~total,
            name = "yaxis data", 
            mode = "lines+markers", 
            type = "bar",
            marker = list(color = '#073767'),   # Ajustar el offset para centrar el texto
            hoverinfo = "text"
            )  %>%
  add_annotations(
    data = subset(datos_combinados, tipo == "Cantidad"),
    x = ~mes, y = ~total,
    text = ~paste(scales::number(total, big.mark = ".", decimal.mark = ",", accuracy = 1)),
    showarrow = FALSE,
    font = list(size = 16, color = 'white', fontface = 'bold'),
    xshift = 0,
    yshift = -100,
    xanchor = 'center',
    yanchor = 'middle'
  )

vline <- function(x = 0, color = "red") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot",width = 6)
  )
}

fig <- fig %>%
  layout(plot_bgcolor = "#e5ecf6", shapes = list(vline(1.5)))


ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "<b>secondary</b> yaxis title")

fig <- fig %>% 
  add_trace(data = subset(datos_combinados, tipo == "Monto"),
            x = ~mes, y = ~total,
            name = "yaxis 2 data", 
            yaxis = "y2", 
            mode = "lines+markers", 
            type = "scatter",
            marker = list(color = '#FFC502',size = 15, symbol = "diamond"),
            line = list(color = '#FFC502',width = 6),
            text = ~paste(scales::number(total, big.mark = ".", decimal.mark = ",", accuracy = 1)),
            textposition = "top center",  # Colocar el texto fuera de la barra
            textfont = list(size = 16,color = "#FFC502", fontface = "bold"),  #  # Ajustar el offset para centrar el texto
            hoverinfo = "text"
            )

# Set figure title, x and y-axes titles
fig <- fig %>% 
  layout(
    title = "Double Y Axis Example", 
    yaxis2 = ay,
    xaxis = list(title="xaxis title "),
    yaxis = list(title="<b>primary</b> yaxis title")
)%>%
  layout(
    xaxis = list(
      zerolinecolor = '#ffff',
      zerolinewidth = 2,
      gridcolor = 'ffff'),
    yaxis = list(
      zerolinecolor = '#ffff',
      zerolinewidth = 2,
      gridcolor = 'ffff')
  )

fig %>%
  config(displaylogo = FALSE,modeBarButtonsToRemove = c("lasso2d","select2d"),modeBarButtonsToAdd =c("drawline",'drawrect'))

map1 <- plot_mapbox() %>% 
  add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
  layout(
    mapbox = list(
      zoom = 0,
      center = list(lat = 65, lon = -75)
    )
  )

map2 <- plot_geo() %>% 
  add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
  layout(geo = list(projection = list(type = "mercator")))

library(htmltools)
browsable(tagList(map2, map2))

bar_plot <- plot_ly(
  data = reparto_graf_1,
  x = ~mes,
  y = ~cantidad,
  color = ~tipo,
  colors = c("Derechohabiente" = "#FFC502", "Titular" = "#073767"),
  type = "bar",
  text = ~paste(scales::number(cantidad, big.mark = ".", decimal.mark = ",", accuracy = 1)),
  textfont = list(size = 16,color = c()),
  insidetextanchor = "middle",
  hoverinfo = "text"
) %>% 
  layout(
    title = "Nro DE BENEFICIARIOS DE RENTAS Y PENSIONES VITALICIAS DEL SISTEMA DE REPARTO",
    xaxis = list(title = "Mes"),
    yaxis = list(title = "Cantidad"),
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
        line = list(color = "red", dash = "dash", width = 2)
      )
    )
  )

# Crear el gráfico de líneas y puntos
line_plot <- bar_plot %>% 
  add_trace(
  data = reparto_graf_1,
  x = ~mes,
  y = ~total,
  type = "scatter",
  mode = "lines+markers",
  line = list(color = "#525659", width = 4.3),
  marker = list(color = "#525659", size = 15,symbol = "diamond"),
  text = ~paste("Total:", scales::number(total, big.mark = ".", decimal.mark = ",", accuracy = 1)),
  textposition = "top center",  # Colocar el texto fuera de la barra
  textfont = list(size = 15,color = "black"),
  hoverinfo = "text",
  showlegend = FALSE
) 

line_plot %>% 
  config(displaylogo = FALSE,modeBarButtonsToRemove = c("lasso2d","select2d","resetScale2d","hoverCompareCartesian"),modeBarButtonsToAdd =c("drawline"))


tabla_1 <- reparto_graf_1 %>% 
  select(TIPO = tipo, mes, cantidad) %>% 
  mutate(cantidad = format(cantidad, big.mark = ".", decimal.mark = ",")) %>%
  pivot_wider(names_from = mes, values_from = cantidad) %>% 
  arrange(desc(TIPO)) %>%
  bind_rows(
    totales %>% 
      filter(total != 0) %>% 
      mutate(total = format(total, big.mark = ".", decimal.mark = ",")) %>%
      pivot_wider(names_from = mes, values_from = total) %>%
      mutate(TIPO = "Total")
  )

columnas <- colnames(tabla_1)
nombres_columnas <- ifelse(columnas == "TIPO", "", columnas)

datatable(tabla_1,
          options = list(
            searching = FALSE,
            paging = FALSE,
            info = FALSE,
            initComplete = JS('
              function(settings, json) {
                $("table.dataTable th, table.dataTable td").css("font-size", "14px");
              }
            ')
          ))


reparto_cant_mont <- reparto_mont %>%
  select(gestion, clase, tipo, tipo_renta, ENE:DIC_2)

mont_1 <- reparto_cant_mont %>%
  pivot_longer(cols = ENE:DIC_2, names_to = "mes", values_to = "cantidad") %>%
  mutate(mes = coalesce(DIC_1, DIC_2, mes))

reparto_graf_mont_1 <- mont_1 %>%
  filter(clase == "Sexo" & tipo %in% c("Titular", "Derechohabiente")) %>%
  group_by(mes, tipo) %>%
  summarise(cantidad = sum(cantidad)) %>%
  arrange(desc(tipo)) %>%
  mutate(mes = factor(mes, levels = orden_meses)) %>%
  filter(cantidad != 0)

reparto_graf_mont_1_actual <- reparto_graf_mont_1 %>%
  filter(mes != DIC_2) %>%
  filter(mes != DIC_1) %>%
  group_by(tipo) %>%
  arrange(mes, tipo) %>%
  mutate(total_mes = cumsum(cantidad)) %>%
  select(mes, tipo, cantidad = total_mes)

reparto_graf_mont_1_actual <- bind_rows(
  reparto_graf_mont_1 %>% filter(mes %in% c(DIC_1, DIC_2)),
  reparto_graf_mont_1_actual
)

totales_mont <- reparto_graf_mont_1 %>%
  group_by(mes) %>%
  summarise(total = sum(cantidad))

reparto_graf_mont_1 <- left_join(reparto_graf_mont_1_actual, totales_mont, by = "mes")

reparto_graf_mont_1_tabla <- reparto_graf_mont_1

reparto_graf_mont_1 <- reparto_graf_mont_1 %>%
  mutate(across(c(cantidad, total), ~ round(. / 1e6)))


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





grafico_1 <- function(datos,x,y1,y2,color,colores,titulo,ejex, ejey){
  bar_plot <- plot_ly(
    data = datos,
    x = ~get(x),
    y = ~get(y1),
    color = ~get(color),
    colors = colores,
    type = "bar",
    text = ~paste(scales::number(get(y1), big.mark = ".", decimal.mark = ",", accuracy = 1)),
    textfont = list(size = 16,color = c()),
    insidetextanchor = "middle",
    hoverinfo = "text"
  ) %>% 
    layout(
      title = titulo,
      xaxis = list(title = ejex),
      yaxis = list(title = ejey),
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
          line = list(color = "red", dash = "dash", width = 2)
        )
      )
    )
  
  # Crear el gráfico de líneas y puntos
  line_plot <- bar_plot %>% 
    add_trace(
      data = datos,
      x = ~get(x),
      y = ~get(y2),
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#525659", width = 4.3),
      marker = list(color = "#525659", size = 15,symbol = "diamond"),
      text = ~paste(scales::number(get(y2), big.mark = ".", decimal.mark = ",", accuracy = 1)),
      textposition = "top center",  # Colocar el texto fuera de la barra
      textfont = list(size = 15,color = "black"),
      hoverinfo = "text",
      showlegend = FALSE
    ) 
  
 return(line_plot)
}


beneficiarios <- grafico_1(reparto_graf_1,"mes","cantidad","total","tipo",c("Derechohabiente" = "#FFC502", "Titular" = "#073767"),"Nro DE BENEFICIARIOS DE RENTAS Y PENSIONES VITALICIAS<br>DEL SISTEMA DE REPARTO","Mes","<b>Cantidad</b> beneficiarios")










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
      title = list(text = titulo, font = font_prop, y = 0.98, x = 0.5, xanchor = 'center', yanchor =  'top'),
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
      modeBarButtonsToAdd =c("drawline")
    )
  
  return(line_plot)
}

# Ejemplo de uso
beneficiario <- generar_grafico(data = reparto_graf_1,x_var = "mes",
                                      bar_color = c("Derechohabiente" = "#FFC502", "Titular" = "#073767"),
                                      line_color = "#525659", line_text_color = "black",
                                      y1_var = "cantidad", y2_var = "total", tipo_var = "tipo", 
                                      titulo = "Nro DE BENEFICIARIOS DE RENTAS Y PENSIONES VITALICIAS<br>DEL SISTEMA DE REPARTO",
                                      ejex = "Mes", ejey = "<b>Cantidad</b> beneficiario")

monto <- generar_grafico(data = reparto_graf_mont_1,x_var = "mes",
                                      bar_color = c("Derechohabiente" = "#FFC502", "Titular" = "#073767"),
                                      line_color = "#525659", line_text_color = "black",
                                      y1_var = "cantidad", y2_var = "total", tipo_var = "tipo", 
                                      titulo = "MONTO DESEMBOLSADO DE RENTAS Y PENSIONES VITALICIAS<br>DEL SISTEMA DE REPARTO",
                                      ejex = "Mes", ejey = "<b>Monto</b> (en millones de Bs)")

monto_beneficiario <- generar_grafico(data = subset(datos_combinados, tipo == "Cantidad"),x_var = "mes",
                                      data_2 = subset(datos_combinados, tipo == "Monto"),
                                      name_bar = "Cantidad", name_line = "Monto",
                                      bar_color = "#073767",
                                      line_color = "#FFC502", line_text_color = "#FFC502",
                                      y1_var = "total", y2_var = "total", tipo_var = "tipo", 
                                      titulo = "MONTO DESEMBOLSADO Y N DE BENEFICIARIOS DEL SISTEMA DE REPARTO",
                                      ejex = "Mes", ejey = "<b>Cantidad</b> beneficiario", ejey_2 = "<b>Monto</b> (en millones de Bs)", two_data = TRUE)


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
generar_tabla(tabla_1)



generar_grafico_2 <- function(data, label, value, hole, colors, title){
  plot_ly(data = data,labels=~get(label),values=~get(value),type = "pie", hole=hole,
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 2))) %>% 
    layout(title = title,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d","select2d","resetScale2d","hoverCompareCartesian","hoverClosest3d"))
}

colors <- c('#FFC502','#073767')
title <- paste("Titular vs Derechohabiente -", mes_selector(mes_fil_mont))

generar_grafico_2(data = torta_mont, label = "TIPO", value = "cantidad", hole = 0.4, colors = colors, title = title)

generar_grafico_2(data = torta, label = "TIPO", value = "cantidad", hole = 0.4, colors = colors, title = title)



filter <- 'clase == "Sexo" & (tipo == "Titular" | tipo == "Derechohabiente")'

generar_data <- function(data, clase_tipo_filter, type, amount = FALSE){
  
  reparto_cant<- data %>% 
    select(gestion, clase, tipo, tipo_renta, ENE:DIC_2) %>% 
    pivot_longer(cols = c(ENE:DIC_2), names_to = "mes", values_to = "cantidad") %>%
    mutate(mes = case_when(
      mes == "DIC_1" ~ DIC_1,
      mes == "DIC_2" ~ DIC_2,
      TRUE ~ mes
    ))
  
  data <- reparto_cant %>% 
    filter(eval(parse(text = clase_tipo_filter))) %>% 
    group_by(mes,tipo = get(type)) %>% 
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

dat <- generar_data(data = reparto_cant,clase_tipo_filter = "clase == 'Regional_Pag_Dom'","tipo_renta") 

dat$data_table %>%
  select(tipo,cantidad) %>% 
  group_by(tipo) %>% 
  summarise(cantidad = sum(cantidad))


library(echarts4r)

ben <- monto$data_table %>%
  pivot_wider(names_from = tipo, values_from = cantidad) %>%
  mutate(total = Titular + Derechohabiente)
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



tbl <- monto$totales  %>% select(total) %>% rename(total_m = total)

bg: "#100C2A"
fg: "#d0d0d0"


pacman::p_load(
  tidyverse,
  readxl,
  dplyr,
  sparklyr,
  ggplot2,
  plotly,
  DT,
  scales,
  arrow,
  crosstalk,
  echarts4r,
  openxlsx
)

source("_code/functions.R",local = TRUE)

ccDir <- "_data/cc/data"

cc_data <- join_gestions(ccDir)

view(cc_data)


altas <- process_altas(cc_data,filter_tipo = "AFP PREVISION")

var <- c("todo en uno","todo en dos")

graf_four_bars(altas$bar, with_total = TRUE)

graf_four_bars <- function(data,theme_show = "dark",with_total = FALSE){
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
                       formatter  =  htmlwidgets::JS("
                              function(params){
                                 value = params.value[1];
                                 value = parseInt(value,10);
                                 value_format = value.toLocaleString();
                              return(
                                value_format
                              ) }
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
                       formatter  =  htmlwidgets::JS("
                              function(params){
                                 value = params.value[1];
                                 value = parseInt(value,10);
                                 value_format = value.toLocaleString();
                              return(
                                value_format
                              ) }
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
                       formatter  =  htmlwidgets::JS("
                              function(params){
                                 value = params.value[1];
                                 value = parseInt(value,10);
                                 value_format = value.toLocaleString();
                              return(
                                value_format
                              ) }
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
                       formatter  =  htmlwidgets::JS("
                              function(params){
                                 value = params.value[1];
                                 value = parseInt(value,10);
                                 value_format = value.toLocaleString();
                              return(
                                value_format
                              ) }
                              ")))
  
  grafiss <- grafi %>% 
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
    itemStyle = list(color = "#00aae495",
                     opacity = 0.1)
    )
  if (with_total == TRUE) {
    grafi <- grafi %>% 
      e_line(TOTAL, 
             itemStyle = list(width=10, color = "#f2f2f2"), 
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
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
                              ")),
             lineStyle = list(width = 2))
  }
  
  grafi <- grafi %>% 
    e_theme(theme_show) %>%
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


fin <- "bola"

if(!is.null(fin)){
  fin <- "hola"
}

result <- cc_data %>%
  mutate(tipo = iconv(tipo, to = "ASCII//TRANSLIT")) %>% 
  filter(tipo == "AFP PREVISION") %>%
  mutate(tipo_CC = toupper(tipo_CC)) %>% 
  select(-bd,-clase,-tipo) 
result <- result %>% 
  mutate(tipo_CC = gsub(" ","_",tipo_CC)) %>% 
  mutate(tipo_CC = iconv(tipo_CC, to = "ASCII//TRANSLIT"))
result <- result %>% 
  data_format_convert()

result <- result %>% 
  data_format_convert(type = "short", names = "tipo_CC")

result <- result %>% 
  filter( AUTOMATICO_MENSUAL > 0 | AUTOMATICO_GLOBAL > 0 | MANUAL_MENSUAL > 0 | MANUAL_GLOBAL > 0) %>% 
  select(gestion,mes,MANUAL_GLOBAL,MANUAL_MENSUAL,AUTOMATICO_GLOBAL,AUTOMATICO_MENSUAL) 
result <- result %>% 
  group_by(gestion) %>% 
  mutate(MANUAL_GLOBAL=cumsum(MANUAL_GLOBAL),
         MANUAL_MENSUAL=cumsum(MANUAL_MENSUAL),
         AUTOMATICO_GLOBAL=cumsum(AUTOMATICO_GLOBAL),
         AUTOMATICO_MENSUAL=cumsum(AUTOMATICO_MENSUAL)) %>% 
  ungroup() %>% 
  mutate(TOTAL = MANUAL_GLOBAL+MANUAL_MENSUAL+AUTOMATICO_GLOBAL+AUTOMATICO_MENSUAL)

graf <- graf %>% 
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
  itemStyle = list(color = "#00aae495",
                   opacity = 0.1)
  )

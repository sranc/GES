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
  shiny,
  htmltools,
  echarts4r,
  scales
)

source("_code/functions_sr.R",local = TRUE)

dir <- "_data/sr/data"

cc_data <- join_gestions(dir)

cc_data <- read_parquet("_data/sr/data/2024.parquet")

#############################################################################################

anexo <- function(result) {
  result <- result %>% 
    select(gestion,mes,cantidad) %>% 
    group_by(gestion,mes) %>% 
    summarise(cantidad = sum(cantidad)) %>% 
    ungroup()
  result <- data_format_convert(result,type = "short") %>% 
    select(gestion, ENE, FEB, MAR, ABR, MAY, JUN, JUL, AGO, SEP, OCT, NOV, AGUI, DIC) %>% 
    data_format_convert() 
  result <- result %>% 
    filter(cantidad > 0) 
  return(result)
}

reparto <- function(cc_data,filter_clase = NULL,filter_tipo = NULL,filter_tipo_reparto = NULL,filter_bd = NULL,type_resp = NULL, month = NULL) {
  result <- cc_data %>% 
    filter(if (!is.null(filter_bd)) bd %in% filter_bd else TRUE) %>%
    filter(if (!is.null(filter_clase)) clase %in% filter_clase else TRUE) %>%
    filter(if (!is.null(filter_tipo)) tipo %in% filter_tipo else TRUE) %>%
    filter(if (!is.null(filter_tipo_reparto)) tipo_reparto %in% filter_tipo_reparto else TRUE)
  if(type_resp == "n_beneficiarios"){
    result <- n_beneficiarios(result)
    result <- data_to_show(result,init = "Derechohabiente",fin = "TOTAL", fin_2 = "Titular")
  }
  if(type_resp == "m_beneficiarios"){
    result <- m_beneficiarios(result)
    result <- data_to_show(result,init = "Derechohabiente",fin = "TOTAL", fin_2 = "Titular")
  }
  if(type_resp == "sn_beneficiarios"){
    result <- sn_beneficiarios(result)
    result <- data_to_show(result,init = "Femenino",fin = "Masculino")
  }
  if(type_resp == "dn_beneficiarios"){
    result <- dn_beneficiarios(result,month)
  }
  if(type_resp == "pagos_beneficiarios"){
    result <- pagos_beneficiarios(result)
    result <- data_to_show(result,init = "Cantidad",fin = "Monto")
  }
  if(type_resp == "anexo"){
    result <- anexo(result)
    result <- data_to_show(result,init = "cantidad",fin = "cantidad")
  }
  return(result)
}

respuesta <- reparto(cc_data,filter_bd = "Estadistica", 
                     filter_tipo = c("IMPEDIMENTO","HOSPITALIZACIÓN","FALLECIMIENTO") , type_resp = "anexo")

graf_pago(respuesta$bar,total = FALSE)
view(respuesta)

########################################################################################


# global.R

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)

source("functions/CNV.R")
source("functions/CyR_BD.R")

# Definir los nuevos nombres de las columnas
nuevos_nombres <- c("tipo", "gestion", "clase", "categoria", "subcategoria", "mes", "cantidad")

# Definir los nombres de los archivos y sus rutas relativas
archivos <- c("CC.xlsx", "RD.xlsx", "SR.xlsx", "UJ.xlsx", "FC.xlsx")
ruta_base <- "../../_excelOutput/"

if(file.exists(ruta_base)){
  lista_datos <- cargar_y_renombrar_datos(ruta_base, archivos, nuevos_nombres)
  save(lista_datos, file = "datos/lista_datos.RData")
}else{
  load("datos/lista_datos.Rdata")
}

orden_meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
orden_meses_montos <- append(orden_meses, "agui", after = 11)
orden_meses_montos <- append(orden_meses_montos, "jun_reint", after = 6)

CC_cantidad <- lista_datos$CC %>%
  filter(tipo %in% c("cantidad")) %>%  # Filtra filas donde tipo es "cantidad" o "monto"
  filter(subcategoria %in% c("global", "mensual")) %>%  # Filtra por subcategoría "global" y "mensual"
  select(tipo, gestion, subcategoria, mes, cantidad) %>%  # Selecciona solo las columnas especificadas
  mutate(mes = factor(mes, levels = orden_meses_montos),
         cantidad = as.numeric(cantidad)) %>%
  group_by(gestion, tipo, subcategoria, mes) %>%  # Agrupa por gestion, tipo, subcategoria y mes
  summarize(cantidad = sum(cantidad, na.rm = TRUE), .groups = 'drop') %>%  # Suma la cantidad en cada grupo
  arrange(gestion, subcategoria, mes)  # Ordena por gestion, subcategoria, y mes para asegurar acumulación correcta


CC_monto <- lista_datos$CC %>%
  filter(tipo %in% c("monto")) %>%  # Filtra filas donde tipo es "cantidad" o "monto"
  filter(subcategoria %in% c("global", "mensual")) %>%  # Filtra por subcategoría "global" y "mensual"
  select(tipo, gestion, subcategoria, mes, cantidad) %>%  # Selecciona solo las columnas especificadas
  mutate(mes = factor(mes, levels = orden_meses_montos),
         cantidad = as.numeric(cantidad)) %>%
  group_by(gestion, tipo, subcategoria, mes) %>%  # Agrupa por gestion, tipo, subcategoria y mes
  summarize(cantidad = sum(cantidad, na.rm = TRUE), .groups = 'drop') %>%  # Suma la cantidad en cada grupo
  arrange(gestion, subcategoria, mes) %>%  # Ordena por gestion, subcategoria, y mes para asegurar acumulación correcta
  group_by(gestion, subcategoria) %>%  # Agrupa nuevamente para la acumulación
  mutate(cantidad = round(cumsum(cantidad/1000000),0))  # Aplica suma acumulada solo a "monto"

cc_genero <- lista_datos$CC %>% 
  filter(tipo == "cantidad") %>% 
  filter(subcategoria %in% c("femenino","masculino")) %>% 
  group_by(gestion,subcategoria,mes) %>% 
  summarize(cantidad = sum(cantidad, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(mes = factor(mes, levels = orden_meses_montos),
         cantidad = as.numeric(cantidad)) %>% 
  group_by(gestion, subcategoria) %>% 
  slice_max(order_by = as.numeric(mes), n = 1, with_ties = FALSE) %>% 
  ungroup()


CC_altas <- lista_datos$CC %>% 
  filter(subcategoria %in% c("automatico global","automatico mensual","manual global","manual mensual")) %>% 
  select(-tipo,-clase) %>% 
  mutate(mes = factor(mes, levels = orden_meses)) %>%
  group_by(gestion,categoria,mes) %>% 
  summarize(cantidad = sum(cantidad, na.rm = TRUE), .groups = 'drop') %>% 
  arrange(gestion,categoria, mes) %>% 
  group_by(gestion,categoria) %>% 
  mutate(cantidad = cumsum(cantidad)) %>% 
  slice_max(order_by = as.numeric(mes), n = 1, with_ties = FALSE)

#CC_inicio_altas <- bind_rows(CC_inicio,CC_altas)

CC_inicio <- lista_datos$CC %>%
  filter(categoria == "registro cc (inicio de tramites)") %>% 
  mutate(subcategoria = ifelse(subcategoria == "la paz", "oficina_central", "regionales")) %>% 
  mutate(mes = factor(mes, levels = orden_meses)) %>%
  mutate(categoria = "inicio tramite") %>%
  group_by(gestion,categoria, mes) %>% 
  summarize(cantidad = sum(cantidad, na.rm = TRUE), .groups = 'drop') %>% 
  arrange(gestion,categoria, mes)

CC_emitidos <- lista_datos$CC %>%
  filter(categoria == "emision de certificados de cc") %>%
  filter(subcategoria %in% c("mensual", "global")) %>%
  select(-tipo, -clase) %>%
  mutate(mes = factor(mes, levels = orden_meses),
         categoria = "certificados emitidos") %>%  # Cambiar el valor de 'categoria'
  group_by(gestion, categoria, mes) %>%
  summarize(cantidad = sum(cantidad, na.rm = TRUE), .groups = 'drop') %>%
  arrange(gestion, categoria, mes)
  
CC_inicio_emitidos <- bind_rows(CC_inicio,CC_emitidos)

totales_cantidad <- CC_cantidad %>% 
  group_by(gestion,mes) %>%  # Agrupa por gestion
  summarize(cantidad = sum(cantidad, na.rm = TRUE), .groups = 'drop') %>% 
  group_by(gestion) %>% 
  slice_tail(n = 1) %>% 
  ungroup()

totales_monto <- CC_monto %>% 
  group_by(gestion,mes) %>%  # Agrupa por gestion
  summarize(cantidad = sum(cantidad, na.rm = TRUE), .groups = 'drop') %>% 
  group_by(gestion) %>% 
  slice_tail(n = 1) %>% 
  ungroup()

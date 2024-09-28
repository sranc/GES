library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(arrow)


#########################################################################################################
##########################################      HEADER     ##############################################
#########################################################################################################

header <- dashboardHeader(title = "SENASIR")

#########################################################################################################
##########################################     SIDEBAR     ##############################################
#########################################################################################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Visualización", tabName = "visualizacion", icon = icon("chart-bar")),
    
    # SelectInput para seleccionar la gestión
    selectInput("gestion", "Seleccione la Gestión:", 
                choices = c("2021", "2022", "2023", "2024"),
                selected = "2024"),
    
    # SliderInput para seleccionar el mes
    sliderInput("mes", "Seleccione el Mes:",
                min = 1, max = 14, value = c(1, 14), step = 1),
    
    # Nuevo menú contraíble para Compensación
    menuItem("Compensación", icon = icon("balance-scale"),
             selectInput("compensacion", "Seleccione la Compensación:", 
                         choices = c("Todos", "Global", "Mensual"),
                         selected = "Todos")
    )
  )
)

#########################################################################################################
##########################################       BODY      ##############################################
#########################################################################################################
custom_css <- "
  .box {
    margin-bottom: 5px;  /* Reducir el margen inferior */
  }
  .row {
    margin-bottom: 5px;  /* Reducir el margen entre filas */
  }
  .skin-blue .main-header .logo {
    background-color: #1B2D4B; /* Azul */
  }
  .skin-blue .main-header .navbar {
    background-color: #1B2D5B; /* Azul */
  }
  .skin-blue .main-sidebar {
    background-color: #1B2D5B; /* Amarillo */
  }
  .box-primary {
    border-top-color: #1B2D5B; /* Azul */
  }
  .content-wrapper, .right-side {
    background-color: #F0F0F0; /* Blanco */
  }
  .main-header .logo {
    color: #D6BD48 !important;  /* Cambia el color del título */
    font-weight: bold;          /* Opción para hacer el texto más visible */
  }

  .small-box .inner h3 {
        font-size: 24px;  /* Cambia este valor al tamaño que desees */
        padding-top: 10px;
        padding-bottom: 10px;
  }
  .small-box .inner p {
        font-size: 11px;  /* Ajusta el tamaño de la fuente del subtítulo */
        font-weight:bold;
        color:black;
  }

"

body <- dashboardBody(
  useShinyjs(),  # Inicializa shinyjs
  tags$head(tags$style(HTML(custom_css))),
  
  fluidRow(
    valueBoxOutput("widget1", width = 2),
    valueBoxOutput("widget2", width = 2),
    valueBoxOutput("widget3", width = 2),
    valueBoxOutput("widget4", width = 2),
    valueBoxOutput("widget5", width = 2),
    valueBoxOutput("widget6", width = 2)
  ),
  fluidRow(
    tabBox(
      title = "",
      id = "tabset1", width = 12,
      
      tabPanel("COMPENSACIÓN DE COTIZACIONES",
               fluidRow(
                 box(title = "N° DE BENEFICIARIOS EN CURSO DE PAGO", status = "primary", solidHeader = FALSE, width = 5,
                     echarts4rOutput("barras_gestion", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "MONTO DESEMBOLSADO EN MILLONES DE BOLIVIANOS", status = "primary", solidHeader = FALSE, width = 7,
                     echarts4rOutput("barras_monto", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 )
               ),
               fluidRow(
                 box(title = "NÚMERO DE BENEFICIARIOS SEGÚN EL GÉNERO", status = "primary", solidHeader = FALSE, width = 3,
                     echarts4rOutput("pie_genero", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "ALTAS", status = "primary", solidHeader = FALSE, width = 6,
                     echarts4rOutput("barras_alta", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "INICIO DE TRAMITES VS CERTIFICADOS EMITIDOS", status = "primary", solidHeader = FALSE, width = 3,
                     echarts4rOutput("lineas_inicio_emitido", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 )
               )
      ),
      
      tabPanel("SISTEMA DE REPARTO",
               fluidRow(
                 box(title = "Gráfico 4", status = "primary", solidHeader = TRUE, width = 6,
                     plotOutput("plot4", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "Gráfico 5", status = "primary", solidHeader = TRUE, width = 6,
                     plotOutput("plot5", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 )
               ),
               fluidRow(
                 box(title = "Gráfico 6", status = "primary", solidHeader = TRUE, width = 12,
                     plotOutput("plot6", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 )
               )
      )
    )
  )
)

#########################################################################################################
##########################################       PAGE      ##############################################
#########################################################################################################
ui <- dashboardPage(header, sidebar, body)

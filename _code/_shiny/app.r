library(shiny)


source("global.R")

source("ui.R",local = TRUE)
source("server.R",local = TRUE)

shinyApp(ui, server)
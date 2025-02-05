# librerias ----
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(dashboardthemes)
library(leaflet)
library(DT)
library(scales)
library(daterangepicker)
library(plotly)
library(lubridate)
library(ggalluvial)
library(ggplot2)
library(RColorBrewer)

# Funciones -----
vline <- function(x = 0, color = "#1A5276") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}

# Datos -----
embarques <- readRDS("data/embarques.rds")
gastos <- readRDS("data/gastos.rds")

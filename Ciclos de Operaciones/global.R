# librerias

library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(readxl)
library(tidyverse)
library(dashboardthemes)
library(DT)
library(scales)
library(plotly)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(xts)

# Datos ----

data <- readRDS("data/data.rds")

# Funciones ----

vline <- function(x = 0, color = "gray0") {
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

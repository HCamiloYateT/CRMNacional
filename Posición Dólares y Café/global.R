## librerias----
library(apputils)
library(shinydashboard)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(readxl)
library(tidyverse)
library(dplyr)
library(dashboardthemes)
library(DT)
library(scales)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(RODBC)
library(Microsoft365R)
library(rhandsontable)


load("data/datos.Rdata")

## Iconos ----
ico_saco <- apputils::icon(list(src = "saco.png", width = "40px", height = "40px"), lib = "local")


  ## Margenes ----
m <- list(
  l = 1,
  r = 1,
  b = 1,
  t = 1
)

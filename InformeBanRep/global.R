Sys.setlocale("LC_TIME", "es_ES.UTF-8")
# Librerias ----
library(tidyverse)
library(DBI)
library(lubridate)
library(bizdays)
library(openxlsx2)
library(blastula)
library(shiny)
library(bslib)
library(shinybusy)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)

# Modulos ----
source("modules/filtros.R", encoding = "UTF-8")
source("modules/values.R", encoding = "UTF-8")

# Tabs ----
source("modules/tabs/maker_tab.R", encoding = "UTF-8")
source("modules/tabs/checker_tab.R", encoding = "UTF-8")








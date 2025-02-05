Sys.setlocale("LC_TIME", "es_ES.UTF-8")
## Librerias ----
library(shiny)
library(bslib)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinybusy)
library(tidyverse)
library(scales)
library(lubridate)
library(shinytoastr)
library(phosphoricons)
library(rhandsontable)
library(DBI)
library(DT)

## Valores ----
fecha <- as.Date(PrimerDia(Sys.Date())-months(1))

## Modulos ----
source("modules/modules.R", encoding = "UTF-8")
source("misc/values.R", encoding = "UTF-8")
source("misc/body_tab.R", encoding = "UTF-8")



## Datos ------
cont2 <- connectapi::connect(
  server  = "http://172.16.19.39:3939",
  api_key = "HayDGkCmpQqmZB1rSkj2300JMDNpA2el"
)

Users <- do.call("bind_rows", 
                 lapply(connectapi::get_groups(cont2)$guid, function(x){
                   connectapi::get_group_members(cont2,x) %>% 
                     mutate(Idgrupo = x)
                 })) %>% 
  select(Idgrupo, Idusr = guid, username) %>% 
  left_join(connectapi::get_groups(cont2) %>% 
              select(Idgrupo = guid, NomGrupo = name), by = join_by(Idgrupo))















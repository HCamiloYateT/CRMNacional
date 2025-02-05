Sys.setlocale("LC_TIME", "es_ES.UTF-8")
options(dplyr.summarise.inform = F)
# Librerias ----
library(shiny)
library(bslib)
library(shinyBS)
library(shinyjs)
library(shinytoastr)
library(shinyWidgets)
library(shinybusy)
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)
library(ggTimeSeries)

# Funciones ----
# Datos ----

grps <- connectapi::get_groups(cont2) %>% 
  select(guid, Grupo = name)

usrs <- do.call("bind_rows", 
                lapply(grps$guid, function(x){
                  connectapi::get_group_members(cont2,x) %>% 
                    mutate(Idgrupo = x)
                })) %>% 
  mutate(Nombre = paste(first_name, last_name),
         active_time = with_tz(as_datetime(active_time), tzone = "America/Bogota")) %>%  
  select(Nombre, user_guid = guid, Idgrupo, UltimaActividad = active_time) %>% 
  left_join(grps, by=c("Idgrupo"="guid"))

apps <- connectwidgets::connect(
  server  = "http://172.16.19.39:3939",
  api_key = "HayDGkCmpQqmZB1rSkj2300JMDNpA2el") %>%
  connectwidgets::content() %>% 
  select(content_guid = guid, App = title, created_time )

data <- read_rds("data/data.rds") %>% 
  bind_rows(
    connectapi::get_usage_shiny(cont2, limit = Inf, from = Sys.Date()) %>% 
      mutate_at(c("started", "ended"), list(~with_tz(as_datetime(.), tzone = "America/Bogota")))
    ) %>% 
  left_join(usrs, by = join_by(user_guid)) %>% 
  left_join(apps, by = join_by(content_guid)) %>% 
  filter(!is.na(App), !(App %in% c("Landing", "Portal de Aplicaciones")), !is.na(user_guid))

rm(cont2, grps, usrs, apps)

# Sources -----

source("misc/tabs/general_tab.R", encoding = "UTF-8")

# Valores ----
lang <- list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/es-ES.json')







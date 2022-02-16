# librerias ----

library(apputils)
library(shiny)
library(bslib)
library(shinydashboard)
library(shinyjs)
library(shinyauthr)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(plotly)
library(scales)
library(writexl)
library(rgdal)
library(RColorBrewer)
library(leaflet)
library(lubridate)
library(prophet)
library(writexl)

# Datos ----

data <- readRDS("data/data.rds")

## Funciones ----

vline <- function(x = 0, color = "red") {
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
hline <- function(y = 0, color = "#ff3a21") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color)
  )
}
col_kpi <- function(x, proporcional = T){
  
  if(proporcional){
    col = case_when(x == 0 ~ "#000000", # Negro
                        x < 0 ~ "#943126",  # Rojo
                        x > 0 ~ "#0B5345"   # Verde
    )} else {
    col = case_when(x == 0 ~ "#000000",  # Negro
                        x < 0 ~  "#0B5345",  # Verde
                        x > 0 ~  "#943126",  # Rojo
    )}
  return(col)
  }
chr_kpi <- function(x){
  case_when(x == 0 ~ "▬",
            x < 0 ~ "▼",
            x > 0 ~ "▲")
  }

RecodificarTop <- function(data, var_recode, var_top, fun_Top, n=10, lab_recodificar = "Otros"){
  
  # data = base de datos a recodificar
  # var_recode = Variable que se quiere recodificar
  # var_top = variable por la cual se hace el top
  # fun_Top = funcion de agregacion: sum, mean, n_distinct, median, n
  # n = Numero de categorias a recodificar
  # lab_recodificar = Etiqueta de los elementos fuera del top
  
  require(dplyr)
  require(lazyeval)
  datos = data
  print(fun_Top)
  
  if (fun_Top == "n"){
    aux1 <- datos %>% 
      group_by_at(var_recode) %>% 
      summarise(Var= n())
  } else{
    aux1 <- datos %>% 
      group_by_at(var_recode) %>% 
      summarise_(Var= interp(paste0(fun_Top, "(var, na.rm = T)"), var = as.name(var_top)))
  }
  
  aux1 <- aux1 %>% 
    arrange(desc(Var)) %>% 
    top_n(n) %>% 
    select(1) %>% 
    unlist() %>% 
    as.character()
  
  print(aux1)
  
  data <- datos %>% 
    mutate(!!var_recode := !!parse_expr(interp(paste0("ifelse(var %in% aux1, var,","'", 
                                                      lab_recodificar, "')" ), var = as.name(var_recode))
    )
    )
  
  return(data)
}

# Iconos ----

# Valores 
fecha_min <- max(data[data$Item == "Compras realizadas" & data$Valor !=0,]$Fecha, na.rm = T)
fecha_max <- ceiling_date(fecha_min, unit = "month") - days(1)
anho = year(fecha_min)
anho_ant = year(fecha_min) - 1

# Margenes -----

m <- list(
  l = 50,
  r = 50,
  b = 5,
  t = 20
)

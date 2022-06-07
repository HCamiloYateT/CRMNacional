library(shiny)
library(shiny.semantic)
library(modules)
library(shinyjs)
library(shinyauthr)
library(shinyWidgets)
library(tidyverse)
library(bslib)

### Funciones ----

Mult3 <- function(x){
  tmp <- x %% 3
  num = case_when (tmp == 0 ~ 0,
                   tmp == 1 ~ 2,
                   tmp == 2 ~ 1
  )
  return(num)
}
cuadro <- function(id, hex, ns) {
  
  img <- apps[apps$AppId == hex, ]$Img
  link <- apps[apps$AppId == hex, ]$Url
  texto <- apps[apps$AppId == hex, ]$AppTexto
  
  div(
    style = "border: none !important;",
    tags$a(
      href=link, target="_blank",
      tags$img(src = img, width ="98%", height="380")
    )
  )
}
board_ui <- function(id="board", data) {
  
  ns <- NS(id)
  size = data %>% nrow
  ncol <- 3
  nrow <- ceiling(size/ncol)
  list_apps <- c(data$AppId, rep(99999, Mult3(size)))

  areas <- cross2(1:ncol, 1:nrow) %>% map(~paste0("card_", .x[[1]], "_", .x[[2]]))

  grid_template <- shiny.semantic::grid_template(
    default = list(
      areas = areas %>% matrix(ncol = ncol),
      cols_width = rep(paste0(100/ncol, "%"), ncol),
      rows_height = rep("400px", times = nrow)
    )
  )

  cards <- purrr::map2(areas, list_apps, ~cuadro(ns(.x), .y, ns))
  args <- c(grid_template = list(grid_template), cards)
  args <- set_names(args, c("grid_template", areas))

  do.call(shiny.semantic::grid, args)
}

### Data ----

users <- readRDS("data/users.rds")
apps <- readRDS("data/apps.rds")
roles <- readRDS("data/roles.rds")

### Tabs ----
#### Tab de Login----
login_tab <- tabPanel(
  title = icon("lock"), 
  value = "login", 
  loginUI("login", title = "Ingrese sus Credenciales", user_title = "Usuario", 
          pass_title = "Contraseña", login_title = "Ingresar", error_message = "Credenciales Inválidas")
)

#### Listado de aplicaciones ----
tab_apps <- tabPanel("",
                     h3("Seleccione el Reporte de Interés"),
                     shiny.semantic::grid(
                       grid_template(
                         default = list(
                           areas = cbind("board"),
                           cols_width = c("4fr")
                           )
                         ),
                       board = uiOutput("board")
                       )
                     )
# librerias ----

library(shiny)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyauthr)
library(shinyWidgets)
library(apputils)
library(googlesheets4)
library(tidyverse)
library(DT)
library(plotly)
library(scales)

# Funciones ----

saveData <- function(data) {
  data <- data %>% as.list() %>% data.frame()
  sheet_append(googledrive::drive_get("Contacto"), data)
}

loadData <- function() {
  read_sheet(googledrive::drive_get("Contacto"))
}

labelObligatorio <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Datos ----

users <- readRDS("data/users.rds")

clientes <- loadData() %>% 
  select(cliente = Cliente) %>% 
  distinct() %>% 
  bind_rows(readRDS("data/clientes.rds"))


prods <- loadData() %>% 
  select(Tipo, Calidad) %>% 
  distinct() %>% 
  bind_rows(readRDS("data/prods.rds"))


# Definiciones ----

## Iconos
ico_cafe <- apputils::icon(list(src = "ico_granos.png", width = "15px"), lib = "local")

## Campos Obligatorios ----

obligatorios <- c("Cliente", "Tipo", "Calidad")
appCSS <- ".mandatory_star { color: red; }"

## Valores Iniciales de los Inputs

customers <- c("", sort(unique(clientes$cliente)))
tipos <- c("", sort(unique(prods$Tipo)))

## Tabs reactivas segun logins ----

### Tab de Log In ----
login_tab <- tabPanel(
  title = icon("lock"), 
  value = "login", 
  shinyauthr::loginUI("login", title = "Ingrese sus Credenciales", user_title = "Usuario", 
                      pass_title = "Contraseña", login_title = "Ingresar", error_message = "Credenciales Inválidas"),
  HTML('<center><img src="logo2.png" width="90" height="90"></center>')
)

### Tab de Registro ----

tab_registro <- tabPanel(paste("Registro de Contacto"), 
                         div(id = "Formulario", style = "width: 95%; max-width: 100%; margin: 0 auto; padding: 20px;",
                             wellPanel(
                               tags$h2("Ingrese el Contacto", class = "text-center", style = "padding-top: 0;font-weight:600;"),
                               br(),
                               fluidRow(
                                 column(4, selectizeInput("Cliente", h6(labelObligatorio("Seleccione el Cliente")),  
                                                          choices = customers, selected ="", options=list(create=TRUE))),
                                 column(4, selectizeInput("Tipo", h6(labelObligatorio("Seleccione el Tipo de Producto")), 
                                                          choices=tipos, selected ="", options=list(create=TRUE))),
                                 column(3, selectizeInput("Calidad", h6(labelObligatorio("Seleccione el Producto")), 
                                                          choices="", selected ="", options=list(create=TRUE))),
                                 column(1, br(),br(),br(),
                                        prettyToggle(inputId = "Venta", label_on = "Venta", label_off = "No", outline = TRUE, bigger = T,
                                                        plain = TRUE, icon_on = icon("thumbs-up"), icon_off = icon("thumbs-down")))
                                 ),
                               br(),
                               fluidRow(
                                 column(4),
                                 column(4, actionButton("Registrar", "Guardar", width = "100%", icon = icon("save"))),
                                 column(4)
                                 ),
                               br()
                               ),
                             br(),
                             HTML('<center><img src="logo2.png" width="90" height="90"></center>')
                             )
                         )


### Tab de Analisis ----

tab_analisis <- navbarMenu("Análisis de Contacto",
                           tabPanel("Análisis por Cliente", icon = icon("user-alt"),
                                    fluidRow(
                                      column(6, selectizeInput("ClienteConsulta", h4("Seleccione el Cliente"),  
                                                               choices = customers, selected ="", options=list(create=TRUE), width = "100%"))
                                      ),
                                    br(),
                                    fluidRow(
                                      column(3, infoBoxOutput("VB_ContactoUsuario", width = 12)),
                                      column(3, infoBoxOutput("VB_UltimoContacto", width = 12)),
                                      column(3, infoBoxOutput("VB_ContactoTipo", width = 12)),
                                      column(3, infoBoxOutput("VB_ContactoProducto", width = 12))
                                    ),
                                    br(),
                                    fluidRow(
                                      column(3, infoBoxOutput("VB_VentaUsuario", width = 12)),
                                      column(3, infoBoxOutput("VB_UltimoVenta", width = 12)),
                                      column(3, infoBoxOutput("VB_VentaTipo", width = 12)),
                                      column(3, infoBoxOutput("VB_VentaProducto", width = 12))
                                    ),
                                    fluidRow(
                                      column(6, box(title = "Serie Histórica de Contacto", width = 12,
                                                    solidHeader=TRUE,
                                                    status="primary",
                                                    plotlyOutput("SerieContacto",height = "500px")
                                                    )
                                             ),
                                      column(6, box(title = "Productos Ofrecidos", width = 12,
                                                    solidHeader=TRUE,
                                                    status="primary",
                                                    plotlyOutput("Productos",height = "500px")
                                                    )
                                             )
                                    ),
                                    dataTableOutput("test")
                                    ),
                           tabPanel("Análisis General", icon = icon("users"),
                                    "General")
                           )


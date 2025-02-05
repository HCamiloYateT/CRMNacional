### Header ----
header <- dashboardHeader(title = "Mezcla Óptima", titleWidth = 300, disable = T)

### Sidebar ----
sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(id="sbmenu",
                                        useShinyjs(),
                                        HTML('<center><img src="logo.png" width="250" height="90"></center>'),
                                        br(),br(), 
                                        menuItem("Calculadora", tabName = "calc", icon = icon("calculator")),
                                        tags$hr(style="border-color: gainsboro;"),
                                        menuItem("Filtros", tabName = "fil", icon = icon("filter"),
                                                 selectInput("Prep", "Seleccione el Producto a Preparar", 
                                                             choices = unique(espec$Nombre))),
                                        br(),br(),
                                        HTML('<center><img src="logo2.png" width="70" height="70"></center>')
                                        )
                            )

### Body -----
body <- dashboardBody(
  ### Opciones CSS ----
  shinyDashboardThemes(theme = "poor_mans_flatly"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tags$style("#Nombre {font-size:20px;
               color:white;
               display:block; 
               font-weight: bold}"),
  tags$style("#Especs {font-size:20px;
               color:white;
               display:block; 
               font-weight: bold}"),
  tags$style("#Faltas {font-size:20px;
               color:white;
               display:block; 
               font-weight: bold}"),
  tabItems(
    ### Resultados ----
    tabItem("calc", 
            tags$hr(style="border-color: gainsboro;"),
            fluidRow(
              column(4, 
                     box(title = "Preparación", textOutput("Nombre"), solidHeader = T, width = 12)
                     ),
              column(4, 
                     box(title = "Especificación", textOutput("Especs"), solidHeader = T, width = 12)
                     ),
              column(4, 
                     box(title = "Faltas", textOutput("Faltas"), solidHeader = T, width = 12)
                     )
              ),
            tags$hr(style="border-color: gainsboro;"),
            fluidRow(
              column(4, 
                     box(title = "Número de kilos a Preparar",
                         status = "info",
                         solidHeader = T,
                         width = 12,
                         numericInput("Kilos", "Kilos", value = 500)),
                     box(title = "Costos por Malla",
                         status = "info",
                         solidHeader = T,
                         width = 12,
                         numericInput("C_Malla13", "Malla 13", value = 19),
                         numericInput("C_Malla14", "Malla 14", value = 21),
                         numericInput("C_Malla15", "Malla 15", value = 22),
                         numericInput("C_Malla16", "Malla 16", value = 25),
                         numericInput("C_Malla17", "Malla 17", value = 25),
                         numericInput("C_Malla18", "Malla 18", value = 28)
                         )
                     ),
              column(4,
                     box(title = "Inventarios por Malla en Kg",
                         status = "info",
                         solidHeader = T,
                         width = 12,
                         numericInput("I_Malla13", "Kilos Malla 13", value = 5000),
                         numericInput("I_Malla14", "Kilos Malla 14", value = 5000),
                         numericInput("I_Malla15", "Kilos Malla 15", value = 5000),
                         numericInput("I_Malla16", "Kilos Malla 16", value = 5000),
                         numericInput("I_Malla17", "Kilos Malla 17", value = 5000),
                         numericInput("I_Malla18", "Kilos Malla 18", value = 5000)
                     ),
                     box(title = "Faltas por Malla",
                         status = "info",
                         solidHeader = T,
                         width = 12,
                         numericInput("F_Malla13", "Faltas por Kilo Malla 13", value = 56),
                         numericInput("F_Malla14", "Faltas por Kilo Malla 14", value = 48),
                         numericInput("F_Malla15", "Faltas por Kilo Malla 15", value = 42.6),
                         numericInput("F_Malla16", "Faltas por Kilo Malla 16", value = 41),
                         numericInput("F_Malla17", "Faltas por Kilo Malla 17", value = 36),
                         numericInput("F_Malla18", "Faltas por Kilo Malla 18", value = 30)
                         )
                     ),
              column(4, 
                     box(title = "Distribución de Mezcla",
                            status = "info",
                            solidHeader = T,
                            width = 12,
                            DT::dataTableOutput("resultados")
                            ),
                     br(),
                     box(title = "Faltas Estimadas",
                         status = "info",
                         solidHeader = T,
                         width = 12,
                         DT::dataTableOutput("faltas"),
                         br(),
                         valueBoxOutput("vb_faltastotal", width = "100%"),
                         valueBoxOutput("vb_faltasKg", width = "100%"),
                         valueBoxOutput("vb_faltas300", width = "100%")
                         ),
                     )
              )
            )
    )
  )

### Definicion ----
dashboardPage(
  header,
  sidebar,
  body
)
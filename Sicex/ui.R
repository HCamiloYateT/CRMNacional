### Header ----
header <- dashboardHeader(title = "Exportaciones de Café", titleWidth = 300, disable = T)

### Sidebar ----
sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(id="sbmenu",
                                        useShinyjs(),
                                        HTML('<center><img src="logo.png" width="250" height="90"></center>'),
                                        br(),br(), 
                                        menuItem("Instrucciones", tabName = "ins", icon = icon("book-open")),
                                        menuItem("Carga del Archivo", tabName = "res", icon = icon("upload")),
                                        menuItem("Exportaciones", tabName = "exp", icon = icon("chart-bar")),
                                        tags$hr(style="border-color: gainsboro;"),
                                        menuItem("Filtros", tabName = "fil", icon = icon("filter"),
                                                 dateRangeInput("Fecha", h6("Periodo de Análisis"), separator = "a", language = "es", weekstart = 1,
                                                                start=as.Date("1990-01-01"),end=as.Date("2019-12-31"))
                                                 ),
                                        br(),br(),
                                        HTML('<center><img src="logo2.png" width="70" height="70"></center>')
                                        )
                            )

### Body -----
body <- dashboardBody(
  ### Opciones CSS ----
  shinyDashboardThemes(theme = "grey_light"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tabItems(
    ### Instrucciones ----
    tabItem("ins", h3("Instrucciones"),
            fluidRow(
              column(12,
                     br(),br(),
                     h4("Para usar esta aplicacion, por favor cargue un archivo de Excel con las siguientes variables"),
                     br(),
                     DT::dataTableOutput("Variables"),
                     br(),
                     h4("La siguiente animacion muestra la forma de uso de la aplicacion"),
                     br(),
                     HTML('<center><img src="manual.gif"></center>')
                     )
              )
            ),
    ### Cargue del Archivo ----
    tabItem("res",
            br(),
            fluidRow(
              column(4,
                     box(width = "100%", title= "Cargue del archivo a analizar", status="info",
                         fileInput("sicex", label= h6("Cargue la base de datos a analizar (xlsx)"), multiple = FALSE, 
                                   placeholder="Explore el archivo", buttonLabel="Buscar",
                                   accept = c(".xlsx")
                                   )
                         )
                     ),
              column(8,
                     box(title = "Generalidades del Archivo Cargado",
                         status = "info",
                         solidHeader = FALSE,
                         width = 12,
                         fluidRow(
                           valueBoxOutput("vb_registros", width = 4),
                           valueBoxOutput("vb_fecha", width = 4),
                           valueBoxOutput("vb_sacos70", width = 4)
                           )
                         )
                     )
              ),
            fluidRow(
              column(12,
                     box(width = "100%", title= "Homologación de Importadores", status="info",
                         DT::dataTableOutput("Cambios")
                         )
                     )
              )
            ),
    ### Resultados ----
    tabItem("exp",
            tabsetPanel(
              tabPanel(" Magnitudes por Mes",
                       h2("Exportaciones Totales"),
                       selectizeInput("Medida1", h3("Medida"), choices=list("Número de Sacos de 70kg"="Sacos70", "Kilos"="Kilos"), selected="Sacos70"),
                       fluidRow(
                         column(12,
                                box(width = "100%", title= "", status="info",
                                    dygraphOutput("Serie1")
                                    )
                                )
                         ),
                       br(),br(),
                       fluidRow(
                         column(12,
                                box(width = "100%", title= "", status="info",
                                    plotlyOutput("BarrasSerie")
                                )
                         )
                       )
                       ),
              tabPanel("Exportadores",
                       numericInput("top1", label = h5("Top N"), value = 10),
                       br(),
                       fluidRow(
                         column(12, box(width = "100%", title= "", status="info", plotlyOutput("Exportador1", height = "500px"))
                                )
                         ),
                       fluidRow(
                         column(12, box(width = "100%", title= "", status="info", DT::dataTableOutput("Exportador2"))
                                )
                         )
                       ),
              tabPanel("Importadores",
                       numericInput("top2", label = h5("Top N"), value = 10),
                       br(),
                       fluidRow(
                         column(12, box(width = "100%", title= "", status="info", plotlyOutput("Importador1", height = "500px"))
                                )
                         ),
                       fluidRow(
                         column(12, box(width = "100%", title= "", status="info", DT::dataTableOutput("Importador2"))
                                )
                         )
                       ),
              tabPanel("Distribución de las Exportaciones", br(),
                       shiny::plotOutput("Sankey1", height = "900px")),
              tabPanel("Exportaciones Por País",
                       numericInput("top3", label = h5("Top N"), value = 10),
                       br(),
                       fluidRow(
                         column(12, box(width = "100%", title= "", status="info", plotlyOutput("Pais1", height = "500px"))
                         )
                       ),
                       fluidRow(
                         column(12, box(width = "100%", title= "", status="info", DT::dataTableOutput("Pais2"))
                         )
                       )),
              tabPanel("Distribución de exportacion Por País",
                       br(),
                       shiny::plotOutput("Sankey2", height = "900px"))
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
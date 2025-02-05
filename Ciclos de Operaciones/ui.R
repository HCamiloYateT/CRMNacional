### Header ----
header <- dashboardHeader(title = "Ciclos de Operaciones", titleWidth = 350)

### Sidebar ----
sidebar <- dashboardSidebar(width="350px",
                            sidebarMenu(id="sbmenu",
                                        useShinyjs(),
                                        br(),
                                        HTML('<center><img src="logo.png" width="250" height="90"></center>'),
                                        br(),br(), 
                                        menuItem("Tiempo entre Despacho y Zarpe", tabName = "ciclo1", icon = icon("truck")),
                                        menuItem("Tiempo entre Zarpe y Confirmación", tabName = "ciclo2", icon = icon("ship")),
                                        menuItem("Tiempo entre Confirmación y Documentos", tabName ="ciclo3", icon = icon("check")),
                                        menuItem("Tiempo entre Documentos y Recepcion", tabName = "ciclo4", icon = icon("file")),
                                        menuItem("Tiempo entre Recepción y Pago", tabName = "ciclo5", icon = icon("hand-holding-usd")),
                                        # menuItem("Tiempos Totales", tabName = "total", icon = icon("calendar-alt")),
                                        tags$hr(style="border-color: gainsboro;"),
                                        radioGroupButtons(inputId = "dias",label = "Medición en días:",
                                                          choices = list("Hábiles"="Habiles", "Calendario"="Calendario"),
                                                          justified = TRUE),
                                        dateRangeInput("FechaFactura", "Ingrese las fechas de Facturación", min = min(data$fechafct), 
                                                       max = max(data$fechafct), start = min(data$fechafct), end = max(data$fechafct), 
                                                       format = "yyyy-mm-dd", language = "es", separator = "a"),
                                        tags$hr(style="border-color: gainsboro;"),
                                        HTML('<center><img src="logo2.png" width="70" height="70"></center>'),
                                        tags$hr(style="border-color: gainsboro;")
                                        )
                            )

### Body -----
body <- dashboardBody(
  ### Opciones CSS ----
  shinyDashboardThemes(theme = "grey_light"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tags$style(".small-box.bg-yellow { background-color: #696969 !important; color: #696969 !important; }"),
  tabItems(
    ### Ciclo 1 ----
    tabItem("ciclo1",
            h2("Tiempo entre Despacho y Zarpe"),
            br(),
            fluidRow(
              column(3, valueBoxOutput("C1_transacciones", width = "100%")),
              column(3, valueBoxOutput("C1_SacosDespachados", width = "100%")),
              column(3, valueBoxOutput("C1_dias_promedio", width = "100%")),
              column(3, valueBoxOutput("C1_promedio_ponderado", width = "100%"))
              ),
            br(),
            h3("Duracion del cliclo en días (promedio simple)"),
            numericInput("C1_MetaDias", label = h4("Ingrese la Meta"), value = 7, step = 0.1),
            fluidRow(
              column(6,
                     box(width = "100%", title= "", status="info",
                         plotlyOutput("C1_BarrasTrilladoraDIa")
                         )
                     ),
              column(6,
                     box(width = "100%", title= "", status="info",
                         DT::dataTableOutput("C1_TablaDias")
                         )
                     )
              ),
            br(),
            h3("Duracion del cliclo en días (promedio ponderado por sacos despachados)"),
            numericInput("C1_MetaPromedio", label = h4("Ingrese la Meta"), value = 6, step = 0.1),
            fluidRow(
              column(6,
                     box(width = "100%", title= "", status="info",
                         plotlyOutput("C1_BarrasTrilladoraPromedio")
                         )
                     ),
              column(6,
                     box(width = "100%", title= "", status="info",
                         DT::dataTableOutput("C1_Tablapromedio", width = "100%", height = "100%")
                         )
                     )
              )
            ),
    ### Ciclo 2 ----
    tabItem("ciclo2",
            h2("Tiempo entre Zarpe y Confirmación"),
            br(),
            tabsetPanel(
              tabPanel("Medicion por Naviera",
                       br(),
                       fluidRow(
                         column(3, valueBoxOutput("C2_transacciones", width = "100%")),
                         column(3, valueBoxOutput("C2_SacosDespachados", width = "100%")),
                         column(3, valueBoxOutput("C2_dias_promedio", width = "100%")),
                         column(3, valueBoxOutput("C2_promedio_ponderado", width = "100%"))
                         ),
                       br(),
                       h3("Duracion del cliclo en días (promedio simple)"),
                       numericInput("C2_MetaDias", label = h4("Ingrese la Meta"), value = 1, step = 0.1),
                       fluidRow(
                         column(6,
                                box(width = "100%", title= "", status="info",
                                    plotlyOutput("C2_BarrasTrilladoraDIa")
                                    )
                                ),
                         column(6,
                                box(width = "100%", title= "", status="info",
                                    DT::dataTableOutput("C2_TablaDias")
                                    )
                                )
                         ),
                       br(),
                       h3("Duracion del cliclo en días (promedio ponderado por sacos despachados)"),
                       numericInput("C2_MetaPromedio", label = h4("Ingrese la Meta"), value = 1, step = 0.1),
                       fluidRow(
                         column(6,
                                box(width = "100%", title= "", status="info",
                                    plotlyOutput("C2_BarrasPromedio")
                                    )
                                ),
                         column(6,
                                box(width = "100%", title= "", status="info",
                                    DT::dataTableOutput("C2_Tablapromedio", width = "100%", height = "100%")
                                    )
                                )
                         )
                       ),
              tabPanel("Medicion por Destino",
                       fluidRow(
                         column(12, box(width = "100%", title= "", status="info", plotlyOutput("MapaMundial", height = "800px"))
                         )
                       ),
                       h6("Seleccione un pais para ver el detalle"),
                       fluidRow(
                         column(12, box(width = "100%", title= "", status="info", DT::dataTableOutput("DetalleMapa"))
                         )
                       )
                       )
              )
            ),
    ### Ciclo 3 ----
    tabItem("ciclo3",
            h2("Tiempo entre Confirmación y Documentos"),
            fluidRow(
              column(3, valueBoxOutput("C3_transacciones", width = "100%")),
              column(3, valueBoxOutput("C3_SacosDespachados", width = "100%")),
              column(3, valueBoxOutput("C3_dias_promedio", width = "100%")),
              column(3, valueBoxOutput("C3_promedio_ponderado", width = "100%"))
              ),
            fluidRow(
              column(6,
                     box(width = "100%", title= "", status="info",
                         plotlyOutput("C3_BarrasTiempos"),
                         verbatimTextOutput("test")
                         )
                     ),
              column(6,
                     box(width = "100%", title= "", status="info",
                         plotlyOutput("Mapa2")
                         )
                     )
              ),
            fluidRow(
              column(12,
                     box(width = "100%", title= "", status="info",
                         DT::dataTableOutput("C3_Detalle")
                     )
              )
            )
            ),
    ### Ciclo 4 ----
    tabItem("ciclo4",
            h2("Tiempo entre Documentos y Recepción del Cliente"),
            br(),
            fluidRow(
              column(3, valueBoxOutput("C4_transacciones", width = "100%")),
              column(3, valueBoxOutput("C4_SacosDespachados", width = "100%")),
              column(3, valueBoxOutput("C4_dias_promedio", width = "100%")),
              column(3, valueBoxOutput("C4_promedio_ponderado", width = "100%"))
              ),
            br(),
            h3("Duracion del cliclo en días (promedio simple)"),
            numericInput("C4_Top", label = h4("N Top"), value = 10, step = 1),
            fluidRow(
              column(6,
                     box(width = "100%", title= "", status="info",
                         plotlyOutput("C4_BarrasTrilladoraDIa", height = "700px")
                         )
                     ),
              column(6,
                     box(width = "100%", title= "", status="info",
                         DT::dataTableOutput("C4_TablaDias")
                         )
                     )
              ),
            br(),
            h3("Duracion del cliclo en días (promedio ponderado por sacos despachados)"),
            fluidRow(
              column(6,
                     box(width = "100%", title= "", status="info",
                         plotlyOutput("C4_BarrasTrilladoraPromedio", height = "700px")
                         )
                     ),
              column(6,
                     box(width = "100%", title= "", status="info",
                         DT::dataTableOutput("C4_Tablapromedio", width = "100%", height = "100%")
                         )
                     )
              )
            ),
    ### Ciclo 5 ----
    tabItem("ciclo5",
            h2("Tiempo entre Recepción del Cliente y Pago"),
            fluidRow(
              column(3, valueBoxOutput("C5_transacciones", width = "100%")),
              column(3, valueBoxOutput("C5_SacosDespachados", width = "100%")),
              column(3, valueBoxOutput("C5_dias_promedio", width = "100%")),
              column(3, valueBoxOutput("C5_promedio_ponderado", width = "100%"))
            ),
            br(),
            h3("Duracion del cliclo en días (promedio simple)"),
            numericInput("C5_Top", label = h4("N Top"), value = 10, step = 1),
            fluidRow(
              column(6,
                     box(width = "100%", title= "", status="info",
                         plotlyOutput("C5_BarrasTrilladoraDIa", height = "700px")
                     )
              ),
              column(6,
                     box(width = "100%", title= "", status="info",
                         DT::dataTableOutput("C5_TablaDias")
                     )
              )
            ),
            br(),
            h3("Duracion del cliclo en días (promedio ponderado por sacos despachados)"),
            fluidRow(
              column(6,
                     box(width = "100%", title= "", status="info",
                         plotlyOutput("C5_BarrasTrilladoraPromedio", height = "700px")
                     )
              ),
              column(6,
                     box(width = "100%", title= "", status="info",
                         DT::dataTableOutput("C5_Tablapromedio", width = "100%", height = "100%")
                     )
              )
            )
            ),
    ### Ciclo Total ----
    tabItem("total",
            h2("Medicion de Tiempos")
            )
    ### ----
    )
  )


### Definicion ----
dashboardPage(
  header,
  sidebar,
  body
)
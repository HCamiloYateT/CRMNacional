### Header ----
header <- dashboardHeader(title = "Calificación de Navieras", titleWidth = 250)

### Sidebar ----
sidebar <- dashboardSidebar(width="250px",
                            sidebarMenu(id="sbmenu",
                                        useShinyjs(),
                                        br(),
                                        HTML('<center><img src="logo.png" width="200" height="70"></center>'),
                                        br(),br(), 
                                        menuItem("Embarques", tabName = "emb", icon = icon("ship")),
                                        menuItem("Gastos", tabName = "item2", icon = icon("money-bill-alt")),
                                        tags$hr(style="border-color: gainsboro;"),
                                        HTML('<center><img src="logo2.png" width="80" height="80"></center>'),
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
  tabItems(
    ### Embarques ----
    tabItem("emb",
            fluidRow(
              ##### Filtros ----
              column(3,
                     pickerInput(inputId = "Medida", label = h4("Seleccione la Medida"), multiple = F, width = "100%",
                                 choices = list("Número de Embarques"="Embarques",
                                                "Número de Reservas"="Reservas",
                                                "Sacos"="Sacos",
                                                "Kilos"="Kilos",
                                                "Total Días de Almacenamiento"="DiasPuertoZarpeTot",
                                                "Promedio Días de Almacenamiento"="DiasPuertoZarpeProm",
                                                "Promedio Ponderado Dias de Almacenamiento"="DiasPuertoZarpePromPond"
                                                )
                                 )
                     ),
              column(3,
                     pickerInput("Puerto",h4("Seleccione el Puerto de Origen"),choices=sort(unique(embarques$puertoorigen)), width = "100%",
                                 multiple =T, selected = sort(unique(embarques$puertoorigen)),
                                 options = pickerOptions(`actions-box` = TRUE, `selected-text-format` = "count > 2", `live-search` = TRUE,
                                                         selectAllText = "Seleccionar Todo",
                                                         deselectAllText = "Limpiar Todo",
                                                         noneSelectedText = "Sin Seleccion",
                                                         virtualScroll = T)
                                 )
                     ),
              column(3,
                     pickerInput("Terminal",h4("Seleccione el Terminal Maritimo"),choices=sort(unique(embarques$terminalmaritimo)), width = "100%",
                                 multiple =T, selected = sort(unique(embarques$terminalmaritimo)),
                                 options = pickerOptions(`actions-box` = TRUE, `selected-text-format` = "count > 2", `live-search` = TRUE,
                                                         selectAllText = "Seleccionar Todo",
                                                         deselectAllText = "Limpiar Todo",
                                                         noneSelectedText = "Sin Seleccion",
                                                         virtualScroll = T)
                                 )
                     ),
              column(3,
                     pickerInput("Linea",h4("Seleccione la Linea"),choices=sort(unique(embarques$linea)), width = "100%",
                                 multiple =T, selected = sort(unique(embarques$linea)),
                                 options = pickerOptions(`actions-box` = TRUE, `selected-text-format` = "count > 2", `live-search` = TRUE,
                                                         selectAllText = "Seleccionar Todo",
                                                         deselectAllText = "Limpiar Todo",
                                                         noneSelectedText = "Sin Seleccion",
                                                         virtualScroll = T)
                                 )
                     )
              ),
            ##### Paneles -----
            tabsetPanel(
              ###### Generalidades -----
              tabPanel("Generalidades",
                       br(),
                       br(),
                       fluidRow(
                         column(4, valueBoxOutput("VB_EmbarquesEmbarques", width = 12)),
                         column(4, valueBoxOutput("VB_EmbarquesReservas", width = 12)),
                         column(4, valueBoxOutput("VB_EmbarquesSacos", width = 12))
                         ), 
                       fluidRow(
                         column(4, valueBoxOutput("VB_EmbarquesKilos", width = 12)),
                         column(4, valueBoxOutput("VB_EmbarquesDiasProm", width = 12)),
                         column(4, valueBoxOutput("VB_EmbarquesDiasPromPond", width = 12))
                         ),
                       br(),
                       br(),
                       fluidRow(
                         column(6,
                                plotlyOutput("EmbarquesSucursal", height = "500px", width = "100%")
                                ),
                         column(6,
                                plotlyOutput("EmbarquesPuertoOrigen", height = "500px", width = "100%")
                                )
                         ), 
                       br(),
                       br(),
                       fluidRow(
                         column(6,
                                plotlyOutput("EmbarquesTerminal", height = "500px", width = "100%")
                                ),
                         column(6,
                                plotlyOutput("EmbarquesNaviera", height = "500px", width = "100%")
                                )
                         ),
                       br(),
                       br(),
                       fluidRow(
                         column(6,
                                plotlyOutput("EmbarquesPaisDestino", height = "500px", width = "100%")
                                ),
                         column(6,
                                plotlyOutput("EmbarquesCliente", height = "500px", width = "100%")
                                )
                         )
                       ),
              ###### Flujos de Embarques -----
              tabPanel("Flujos de Embarques",
                       br(),
                       fluidRow(
                         column(3,
                                pickerInput(inputId = "NodoInicial", label = h4("Seleccione el nodo de Inicio"), multiple = F, width = "100%",
                                            choices = list("Sucursal"="sucursal",
                                                           "Puerto de origen"="puertoorigen",
                                                           "Terminal Maritimo"="terminalmaritimo",
                                                           "Naviera"="linea",
                                                           "País de Destino"="paisdestino",
                                                           "Puerto de Destino"="puertodestino"
                                                           )
                                            )
                                ),
                         column(3,numericInput("NumEntradas", label = h4("Número de Elementos de Entrada"), value = 10, width = "100%")),
                         column(3,
                                pickerInput(inputId = "NodoFinal", label = h4("Seleccione el nodo Final"), multiple = F, width = "100%",
                                            choices = list("Sucursal"="sucursal",
                                                           "Puerto de origen"="puertoorigen",
                                                           "Terminal Maritimo"="terminalmaritimo",
                                                           "Naviera"="linea",
                                                           "País de Destino"="paisdestino",
                                                           "Puerto de Destino"="puertodestino"
                                                           ),
                                            selected = "puertoorigen"
                                            )
                                ),
                         column(3,numericInput("NumSalidas", label = h4("Número de Elementos de Salida"), value = 10, width = "100%")),
                         ),
                       fluidRow(
                         column(12,plotlyOutput("EmbarquesSankey", height = "800px"))
                         ),
                       h6("Seleccione algunos de los nodos de origen y destino para ver el detalle"),
                       br(),
                       h4("Detalle según origen y destino"),
                       fluidRow(
                         column(12, box(width = "100%", title= "", status="info", DT::dataTableOutput("EmbarqueSankeyDetalle"))
                                )
                         )
                       )
              )
            ),
    ### Gastos ----
    tabItem("item2",
            fluidRow(
              column(4,
                     pickerInput("Proveedor",h4("Seleccione el Proveedor"),choices=sort(unique(gastos$proveedor)), width = "100%",
                                 multiple =T, selected = sort(unique(gastos$proveedor)),
                                 options = pickerOptions(`actions-box` = TRUE, `selected-text-format` = "count > 2", `live-search` = TRUE,
                                                         selectAllText = "Seleccionar Todo",
                                                         deselectAllText = "Limpiar Todo",
                                                         noneSelectedText = "Sin Seleccion",
                                                         virtualScroll = T)
                     )
              ),
              column(4,
                     pickerInput("Concepto",h4("Seleccione el Concepto"),choices=sort(unique(gastos$concepto)), width = "100%",
                                 multiple =T, selected = sort(unique(gastos$concepto)),
                                 options = pickerOptions(`actions-box` = TRUE, `selected-text-format` = "count > 2", `live-search` = TRUE,
                                                         selectAllText = "Seleccionar Todo",
                                                         deselectAllText = "Limpiar Todo",
                                                         noneSelectedText = "Sin Seleccion",
                                                         virtualScroll = T)
                     )
              ),
              column(4,
                     daterangepicker(inputId = "Fecha", label = h4("Seleccione Fecha de Facturacion"), start = min(gastos$fechafct),
                                     end = max(gastos$fechafct), min =  min(gastos$fechafct), max = max(gastos$fechafct), 
                                     style = "width:150%;border-radius:4px;", language = "es"
                     )
              )
            ),
            tabsetPanel(
              tabPanel("Generalidades",
                       br(),
                       fluidRow(
                         column(3, valueBoxOutput("IB_Registros", width = 12)),
                         column(3, valueBoxOutput("IB_Lotes", width = 12)),
                         column(3, valueBoxOutput("IB_Sacos", width = 12)),
                         column(3, valueBoxOutput("IB_Gasto", width = 12))
                         ),
                       br(),
                       box(width = "100%", title= "Detalle del Gasto", status="info", solidHeader = T, 
                           collapsible = T, collapsed = T,
                           h3("Distribucion del Gasto"),
                           plotlyOutput("Concepto1", height = "500px", width = "100%"),
                           h3("Tendencia del gasto"),
                           plotlyOutput("Serie1", height = "500px", width = "100%")
                           ),
                       box(width = "100%", title= "Gastos de Almacenaje", status="info", solidHeader = T, 
                           collapsible = T, collapsed = T,
                           h3("Tiempos de Almacenaje"),
                           plotlyOutput("Tiempos1", height = "500px", width = "100%"),
                           h3("Distribucion del Gasto de Almacenaje"),
                           plotlyOutput("Almacenamiento1", height = "500px", width = "100%")
                           ),
                       box(width = "100%", title= "Otros Gastos", status="info", solidHeader = T, 
                           collapsible = T, collapsed = T,
                           h3("Otros Gastos"),
                           plotlyOutput("Otros1", height = "800px", width = "100%")
                           )
                       )
              # tabPanel("Otros"
              #          )
              )
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
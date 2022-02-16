tagList(
  ## CSS y HTML ----
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo2.png")),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  # tagList(tags$head(tags$style(type = 'text/css','.navbar-brand{display:none;}'))),
  tags$style(type = 'text/css', '.bg-aqua {background-color: #707B7C!important; }'),
  tags$style(type = 'text/css', '.bg-teal {background-color: #121110!important; }'),
  tags$style(type = 'text/css', '.bg-olive {background-color: #317A5A!important; }'),
  ## Herencia de Funciones ----
  useShinyjs(),
  useShinydashboard(),
  ## Filtros Flotantes ----
  fixedPanel(top = "20", left = "auto", right = 10, bottom = "auto",
             shinyWidgets::dropdown(
               airDatepickerInput(inputId = "Fecha", label = h6("Rango de Fecha de Análisis"),
                                  value = fecha_max, range = T, separator = " a ", 
                                  minDate = min(data$Fecha, na.rm = T), maxDate = fecha_max, 
                                  view = "months", minView = "months", language = "es",
                                  dateFormat = "M yyyy", autoClose = F, addon ="left"),
               pickerInput("Sucursal",h6("Seleccione la Sucursal"), width = "100%", multiple =F,
                           choices=sort(unique(data$Sucursal)), selected = "Bachue",
                           options = pickerOptions(`actions-box` = T, `selected-text-format` = "count > 1",
                                                   selectAllText = "Seleccionar Todo",
                                                   deselectAllText = "Limpiar Todo",
                                                   noneSelectedText = "Sin Seleccion",
                                                   virtualScroll = T)),
               actionButton("AplicarFiltros", "Aplicar Filtros", icon = shiny::icon("check"), width = "100%"),
               style = "pill", icon = icon("filter"), size = "lg",
               status = "danger", width = "300px",right = T, height = "600px",
               animate = animateOptions(
                 enter = animations$fading_entrances$fadeInRight,
                 exit = animations$fading_exits$fadeOutRightBig)
               ),
             style = "z-index: 10;"
             ),
  ### Navegacion ----
  navbarPage( 
    title = "Informe de Gestión",
    windowTitle="Gestion de Trilladoras",
    theme = bs_theme(bootswatch = "lux"),
    collapsible = T,
    tabPanel("Reporte de Gestión",
             uiOutput("MesAnalisis"),
             br(),
             #### Costo Estándar -----
             box(width = "100%", title= "Costo Estándar", status="danger",
                 solidHeader = F, collapsible = T, collapsed = T,
                 fluidRow(
                   column(8, plotlyOutput("BarrasCostoEstandar", height = "300px")),
                   column(4, br(),tableOutput('TablaCostoEstandar'))
                 )
             ),
             #### Compras -----
             box(width = "100%", title= "Compras", status="danger",
                 solidHeader = F, collapsible = T, collapsed = T,
                 fluidRow(
                   column(4, br(),
                          valueBoxOutput("ResumenCompras", width = 12),
                          uiOutput("DetallesCompras")
                   ),
                   column(3, 
                          h6("Cumplimiento de Compras"),
                          plotlyOutput("GaugeCumpCompras", width = "100%", height = "170px", inline = F),
                          br(),
                          uiOutput("DetallGaugeCompras")
                   ),
                   column(5, h6("Comparativo de Compras Acumuladas"),
                          plotlyOutput("AcumCompras", height = "300px"))
                 ),
                 fluidRow(
                   column(12, h6("Histórico de Compras (Kg)"),
                          plotlyOutput("SerieCompras", height = "450px")),
                 )
             ),
             #### Entradas ----
             box(width = "100%", title= "Entradas", status="danger",
                 solidHeader = F, collapsible = T, collapsed = T,
                 fluidRow(
                   column(4, br(),
                          valueBoxOutput("ResumenEntradas", width = 12),
                          uiOutput("DetallesEntradas")
                   ),
                   column(8, h6("Histórico de Entradas (Kg)"),
                          plotlyOutput("SerieEntradas", height = "450px"))
                 )
             ),
             #### Producción -----
             box(width = "100%", title= "Producción", status="danger",
                 solidHeader = F, collapsible = T, collapsed = T,
                 fluidRow(
                   column(4, br(),
                          valueBoxOutput("ResumenProduccion", width = 12),
                          uiOutput("DetallesProduccion")
                   ),
                   column(3, 
                          h6("Distribución por Producto"),
                          plotlyOutput("SB_Produccion", height = "300px")
                   ),
                   column(5, 
                          h6("Comparativo de Producción Acumulada"),
                          plotlyOutput("AcumProduccion", height = "300px")
                   )
                 ),
                 fluidRow(
                   column(12, h6("Histórico de Produccion (Sacos)"),
                          plotlyOutput("SerieProduccion", height = "450px")),
                 )
             ), 
             #### Despachos ----
             box(width = "100%", title= "Despachos", status="danger",
                 solidHeader = F, collapsible = T, collapsed = T,
                 fluidRow(
                   column(4, br(),
                          valueBoxOutput("ResumenDespachos", width = 12),
                          uiOutput("DetallesDespachos")
                   ),
                   column(3, 
                          h6("Distribución por Producto"),
                          plotlyOutput("SB_Despachos", height = "300px")
                   ),
                   column(5, 
                          h6("Comparativo de Acumulados"),
                          plotlyOutput("AcumDespachos", height = "300px")
                   )
                 ),
                 fluidRow(
                   column(12, h6("Histórico de Despachos (Sacos)"),
                          plotlyOutput("SerieDespachos", height = "450px"))
                 )
             ),
             #### Rechazos ----
             box(width = "100%", title= "Rechazos", status="danger",
                 solidHeader = F, collapsible = T, collapsed = T,
                 fluidRow(
                   column(4, br(),
                          valueBoxOutput("ResumenRechazos", width = 12),
                          uiOutput("DetallesRechazos")
                   ),
                   column(3, 
                          h6("Distribución por Producto"),
                          plotlyOutput("SB_Rechazos", height = "300px")
                   ),
                   column(5, 
                          h6("Comparativo de Acumulados"),
                          plotlyOutput("AcumRechazos", height = "300px")
                   )
                 ),
                 fluidRow(
                   column(12, h6("Histórico de Rechazos (Sacos)"),
                          plotlyOutput("SerieRechazos", height = "450px"))
                 )
             ),
             #### PyG de Gestion ----
             box(width = "100%", title= "PyG", status="danger", 
                 solidHeader = F, collapsible = T, collapsed = T,
                 ##### Ingresos ----
                 box(width = "100%", title= "Ingresos", status="danger", 
                     solidHeader = F, collapsible = T, collapsed = T,
                     fluidRow(
                       column(4, br(),
                              valueBoxOutput("ResumenIngresos", width = 12),
                              uiOutput("DetallesIngresos"),
                              br(),
                              valueBoxOutput("ResumenOtrosIngresos", width = 12),
                              uiOutput("DetallesOtrosIngresos")
                       ),
                       column(3, 
                              h6("Distribución de Ingresos"),
                              br(),
                              plotlyOutput("SB_Ingresos", height = "450px")
                       ),
                       column(5, 
                              h6("Comparativo de Ingresos Acumulados"),
                              plotlyOutput("AcumIngresos", height = "250px"),
                              plotlyOutput("AcumOtrosIngresos", height = "250px")
                              
                       )
                       
                     ),
                     br(),
                     fluidRow(
                       column(1, radioGroupButtons(inputId = "TipoSerieIngresos", label = "Nivel de Detalle",
                                                   choices = c("Total", "Detalle 1", "Detalle 2", "Detalle 3"), 
                                                   direction = "vertical")
                              ),
                       column(11, 
                              conditionalPanel(condition = "input.TipoSerieIngresos == 'Total'",
                                               plotlyOutput("SerieIngresosTotal", height = "500px")),
                              conditionalPanel(condition = "input.TipoSerieIngresos == 'Detalle 1'",
                                               plotlyOutput("SerieIngresosDet1", height = "500px")),
                              conditionalPanel(condition = "input.TipoSerieIngresos == 'Detalle 2'",
                                               plotlyOutput("SerieIngresosDet2", height = "500px")),
                              conditionalPanel(condition = "input.TipoSerieIngresos == 'Detalle 3'",
                                               plotlyOutput("SerieIngresosDet3", height = "500px"))
                              )
                       )
                     ),
                 ##### Costos ----
                 box(width = "100%", title= "Costos", status="danger", 
                     solidHeader = F, collapsible = T, collapsed = T,
                     fluidRow(
                       column(4, br(),
                              valueBoxOutput("ResumenCostos", width = 12),
                              uiOutput("DetallesCostos"),
                              br(),
                              valueBoxOutput("ResumenOtrosCostos", width = 12),
                              uiOutput("DetallesOtrosCostos")
                       ),
                       column(3,
                              h6("Distribución de Costos"),
                              br(),
                              plotlyOutput("SB_Costos", height = "450px")
                       ),
                       column(5, 
                              h6("Comparativo de Costos Acumulados"),
                              plotlyOutput("AcumCostos", height = "250px"),
                              plotlyOutput("AcumOtrosCostos", height = "250px")
                       )
                       
                     ),
                     fluidRow(
                       column(1, radioGroupButtons(inputId = "TipoSerieCostos", label = "Nivel de Detalle",
                                                   choices = c("Total", "Detalle 1", "Detalle 2", "Detalle 3"), 
                                                   direction = "vertical")
                              ),
                       column(11, 
                              conditionalPanel(condition = "input.TipoSerieCostos == 'Total'",
                                               plotlyOutput("SerieCostosTotal", height = "500px")),
                              conditionalPanel(condition = "input.TipoSerieCostos == 'Detalle 1'",
                                               plotlyOutput("SerieCostosDet1", height = "500px")),
                              conditionalPanel(condition = "input.TipoSerieCostos == 'Detalle 2'",
                                               plotlyOutput("SerieCostosDet2", height = "500px")),
                              conditionalPanel(condition = "input.TipoSerieCostos == 'Detalle 3'",
                                               plotlyOutput("SerieCostosDet3", height = "500px")),
                       )
                     )
                 ),
                 ##### Utilidades ----
                 box(width = "100%", title= "Utilidades", status="danger", 
                     solidHeader = F, collapsible = T, collapsed = T,
                     fluidRow(
                       column(4, br(),
                              valueBoxOutput("ResumenUtOperacional", width = 12),
                              uiOutput("DetallesUtOperacional"), br(),
                              valueBoxOutput("ResumenUtCorte", width = 12),
                              uiOutput("DetallesUtCorte"), br(),
                              valueBoxOutput("ResumenUtNeta", width = 12),
                              uiOutput("DetallesUtNeta"), br()
                              
                       ),
                       column(8, 
                              h6("Series Históricas"),
                              plotlyOutput("SerieUtilidades", height = "550px")
                       )
                     )
                 )
             ),
             #### Indicadores -----
             box(width = "100%", title= "Indicadores", status="danger", 
                 solidHeader = F, collapsible = T, collapsed = T,
                 ### Indicadores de Proveedores -----
                 box(width = "100%", title= "Proveedores", status="danger", 
                     solidHeader = F, collapsible = T, collapsed = T,
                     fluidRow(
                       column(8, plotlyOutput("BarrasProveedores", height = "400px")),
                       column(4, 
                              h6("Proveedor de Mayor Concentración"),
                              plotlyOutput("GaugeProveedoresPeriodo", width = "100%", height = "230px", inline = F),
                              uiOutput("DetalleGaugeProveedoresPeriodo")
                              )
                       )
                     ),
                 ### Indicadores de Proceso -----
                 box(width = "100%", title= "Proceso", status="danger", 
                     solidHeader = F, collapsible = T, collapsed = T,
                     fluidRow(
                       column(3, 
                              h6("Cafés Diferenciados y a la medida sobre total de despachos"),
                              plotlyOutput("GaugeDifSobreDespachos", width = "100%", height = "300px", inline = F),
                              uiOutput("DetalleGaugeDifSobreDespachos")),
                       column(6, style='border-left: 1px dotted gray',
                              h6("Histórico de Kw por Saco"),
                              plotlyOutput("SerieKWXSaco", height = "400px")
                              ),
                       column(3,h6("KW por saco"),
                              plotlyOutput("GaugeKWXSaco", width = "100%", height = "300px", inline = F),
                              uiOutput("DetalleGaugeKWXSaco")),
                       
                     )
                     ),
                 ### Indicadores de Calidad -----
                 box(width = "100%", title= "Calidad", status="danger", 
                     solidHeader = F, collapsible = T, collapsed = T,
                     fluidRow(
                       column(3,
                              h6("Supremos Sobre el Total de Excelsos"),
                              plotlyOutput("GaugeSupExcelsos", width = "100%", height = "300px", inline = F),
                              uiOutput("DetalleGaugeSupExcelsos")),
                       column(9, 
                              plotlyOutput("SerieSupExcelsos", height = "450px")
                              )
                       )
                     ),
                 ### Indicadores de Operación Logística -----
                 box(width = "100%", title= "Operación Logística", status="danger", 
                     solidHeader = F, collapsible = T, collapsed = T,
                     fluidRow(
                       column(3,
                              h6("Eficiencia en Flete"),
                              plotlyOutput("GaugeEficienciaFlete", width = "100%", height = "300px", inline = F),
                              uiOutput("DetalleGaugeEficienciaFlete")),
                       column(9,
                              plotlyOutput("SerieEficienciaFlete", height = "450px"))
                       ),
                     br(),
                     fluidRow(
                       column(3,
                              h6("Oportunidad Logística"),
                              plotlyOutput("GaugeOpLogistica", width = "100%", height = "300px", inline = F),
                              uiOutput("DetalleGaugeOpLogistica")),
                       column(9,
                              plotlyOutput("SerieOpLogistica", height = "450px"))
                       )
                     ),
                 ### Indicadores de Talento Humano -----
                 box(width = "100%", title= "Indicadores de Talento Humano", status="danger", 
                     solidHeader = F, collapsible = T, collapsed = T,
                     fluidRow(
                       column(3,
                              h6("Costo de Movilización por Saco"),
                              plotlyOutput("GaugeMovXSaco", width = "100%", height = "300px", inline = F),
                              uiOutput("DetalleGaugeMovXSaco")),
                       column(9,
                              plotlyOutput("SerieMovXSaco", height = "450px"))
                       ),
                     br(),
                     fluidRow(
                       column(3,
                              h6("Personal"),
                              br(),
                              plotlyOutput("BarrasPersonal", width = "100%", height = "400px", inline = F)
                              ),
                       column(6, style='border-left: 1px dotted gray',
                              h6("Horas Extras"),
                              plotlyOutput("SerieHorasExtras", height = "450px")),
                       column(3,
                              valueBoxOutput("VB_HorasExtras", width = 12),
                              valueBoxOutput("VB_HorasExtrasCostos", width = 12)
                              )
                       ),
                     br(),
                     fluidRow(
                       column(3,
                              h6("Accidentes"),
                              valueBoxOutput("VB_NumAccidentes", width = 12),
                              valueBoxOutput("VB_DiasAusencia", width = 12)
                              ),
                       column(9,
                              plotlyOutput("SerieAccidentes", height = "450px"))
                       )
                     ),
             )
             #### ====
        )
      ),
  ### Footer ----
  br(),br(),br(),
  HTML('<center><img src="logo2.png" width="90" height="90"></center>'),
  tags$footer(HTML("                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <div class='footer-copyright text-center py-3'>© 2022 Copyright:
                           <a href='https://racafe.com.co/es/'> Racafé</a>
                           </div>
                           <!-- Copyright -->

                           </footer>
                           <!-- Footer -->"))
  )
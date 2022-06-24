tagList(
  ## CSS y HTML ----
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo2.png")),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tags$style(type="text/css", "body {padding-top: 120px;}"),
  # tagList(tags$head(tags$style(type = 'text/css','.navbar-brand{display:none;}'))),
  tags$style(type = 'text/css', '.bg-aqua {background-color: #707B7C!important; }'),
  tags$style(type = 'text/css', '.bg-teal {background-color: #121110!important; }'),
  ## Herencia de Funciones ----
  useShinyjs(),
  useShinydashboard(),
  ### Navegacion ----
  navbarPage( 
    title = HTML("<center><img src='logo.png' width='150' height='60'></center>"),
    windowTitle = "Informe Comercial",
    theme = bs_theme(bootswatch = "lux"),
    position = "fixed-top",
    collapsible = T, lang = "es",
    ##
    
    tabPanel("Informe Comercial",
             #### Filtros ----
             fixedPanel(top = "130", left = "auto", right = 10, bottom = "auto",
                        shinyWidgets::dropdown(
                          h5("Filtros"),
                          fluidRow(
                            column(12,
                                   dateRangeInput(inputId = "Fecha_Factura", label = h6("Fecha Factura"),
                                                  min = min(tabla_informe$Fecha_Factura, na.rm = T),
                                                  max = max(tabla_informe$Fecha_Factura, na.rm = T),
                                                  start = fecha_min,
                                                  end = fecha_max,
                                                  format = "dd/mm/yyyy", separator = "a", language = "es",
                                                  width = "100%"),
                                   
                                   pickerInput("Sucursal",h6("Sucursal"), width = "100%", multiple = T,
                                               choices= sort(unique(tabla_informe$Sucursal)),
                                               selected = unique(tabla_informe$Sucursal),
                                               options = pickerOptions(`live-search`=TRUE,
                                                                       `actions-box` = T, 
                                                                       `selected-text-format` = paste("count > ", length(unique(tabla_informe$Sucursal))-2),
                                                                       `count-selected-text` = "TODOS",
                                                                       selectAllText = "Seleccionar Todo",
                                                                       deselectAllText = "Limpiar Todo",
                                                                       noneSelectedText = "Sin Seleccion",
                                                                       virtualScroll = T))
                                   
                                   )),
                          style = "pill", icon = icon("filter"), size = "lg",
                          status = "danger", width = "400px",right = T, height = "300px",
                          tooltip = tooltipOptions(title = "Click para modificar los filtros"),
                          animate = animateOptions(
                            enter = animations$fading_entrances$fadeInRight,
                            exit = animations$fading_exits$fadeOutRightBig)
                          ),
                        style = "z-index: 10;"),
             
             uiOutput("TextoEntradas"),
             
             h4("Calidades"),
             br(),
             fluidRow(
               column(4,
                      plotlyOutput("pie_Calidades", height = "500px")),
               column(8,
                      dataTableOutput("tb_Calidades"))
                      
               ),
             
             
             br(),br(),
             
             h4("Proveedor"),
             fluidRow(
               column(4,
                      plotlyOutput("pie_Proveedor", height = "500px")),
               column(8,
                      dataTableOutput("tb_Proveedor"))
             ),
             br(), br(),
             
             h4("Ubicación"),
             fluidRow(
               column(4,
                      plotlyOutput("gra_Ubicacion", height = "100px")),
               column(4,
                      h6("Detalle por Ubicación"),
                      br(),
                      dataTableOutput("tb_Ubicacion")),
               column(4,
                      h6("Detalle por Proveedor"),
                      br(),
                      dataTableOutput("tb_Proveedor2"))
             )
             
          )
  ),
  
  ### Footer ----
  br(),br(),br(),
  tags$footer(HTML("<footer class='page-footer font-large indigo'>
                    <center><img src='logo2.png' width='90' height='90'></center>
                    <div class='footer-copyright text-center py-3'>Año 2022 Copyright:
                    <a href='https://racafe.com.co/es/'> Racafé</a>
                    </div>
                    </footer>"))
  
)
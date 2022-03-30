# Define UI for application
tagList(
  ## CSS y HTML ----
  tags$head(tags$link(rel ="icon", type = "image/png", sizes= "32x32", 
                      href="logo2.png")),
  tags$style(type = "text/css",
             ".shiny-output-error {visibility:hidden; }" ,
             ".shiny-output-error:before {visibility:hidden; }"),
  tags$style(type = 'text/css', '.bg-aqua {background-color: #707B7C!important; }'),
  tags$style(type = 'text/css', '.bg-teal {background-color: #121110!important; }'),
  tags$style(type = 'text/css', '.bg-olive {background-color: #317A5A!important; }'),
  tagList(tags$head(tags$style(type ='text/css', 
  '.navbar-brand {display:none;}'))),
  tags$style(HTML(" .my_table .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td,
                  .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th { padding: 8px;
                  line-height: 1.5; vertical-align: top; border-top: 1px solid #000000;
                  border-bottom : 1px solid #000000; }")),
  
##Herencia de Funciones----
useShinyjs(),
useShinydashboard(),

#Navegación ----

navbarPage(
  title = HTML("<center><img src='logo.png' width = '200' height='70'<>/center>"),
  windowTitle = "Posición Dólares y Café",
  theme = bs_theme(bootswatch = "lux"),
  collapsible = T,
  
  tabPanel("Indicadores",
           uiOutput("FechaAnalisis"),
           ###Sacos por fijar ----
           
           fluidRow(
             column(6),
             column(6)),
           
           br(),
           
           fluidRow(
             column(6,
                    h3("Sacos Comprados por Fijar"),
                    br(),
                    
                    fluidRow(
                      column(2),
                      column(8, valueBoxOutput("VB_SacosXFijar", width=12)),
                      column(2)),
                    br(),
                    
                    fluidRow(
                      column(12, tableOutput("DetalleSacosXFijar")))
                    ),
             column(1),
             column(5, style='border-left: 1px dotted #85929E',
                    fluidRow(
                      column(12,
                             h3("Cuenta Café")),
                             br()),
                      fluidRow(
                        column(2),
                        column(8, 
                             valueBoxOutput("VB_VerificacionCafe", width = 12)),
                        column(2)),
                            br(),
                    fluidRow(
                      column(6,
                             tableOutput("Cuenta_Cafe")),
                      column(1),
                      column(5,
                             tableOutput("Ajustes_Cafe"))
                             
                    ),
                    br()
                        
                    )),
  
           
           
           ### Dólares por fijar ----   
           
           
           fluidRow(
             column(6,
                      h3("Dólares por Asginar"),
                      br(),
                      fluidRow(
                        column(12, tableOutput("DetalleDolaresXAsignar"))
                              ),
                      br(),
                      fluidRow(
                        column(12, tableOutput("FijadoxComprar")))
                    ),
             column(1),
             column(5, style='border-left: 1px dotted #85929E',
                    fluidRow(
                      column(12,
                             h3("Cuenta Dólares"))),
                    br(),
                    fluidRow(column(2),
                             column(8,
                                    valueBoxOutput("VB_VerificacionDolares", width = 12)),
                             column(2)
                             ),
                    br(),
                    fluidRow(column(6,
                                    tableOutput("Cuenta_Dolares")),
                             column(1),
                             column(5,
                                    tableOutput("Ajustes_Dolares"))
                             )
                    
                    )
           ),
           

           
           br(),
           
           
           fluidRow(
             column(1),
             column(5, 
                    valueBoxOutput("VB_SaldoDLRS", width = 8)),
             column(1), 
             column(5,
                    valueBoxOutput("VB_TasaPromedio", width = 8))
           ),
           
           br(),
           
           
           ### Contratos x Fijar ----
           
           h3("Contratos por Asignar"),
           br(),
           fluidRow(column(8, 
                           DT::dataTableOutput("DetalleContratos")),
                    column(4,
                           valueBoxOutput("VB_ContratosXFijar", width = 12),
                           plotlyOutput("BarrasContratos", height = "500px", width = "100%"))
           ),
           br(),
           
           h3("Resumen Contratos por mes"),
           
           br(),
           fluidRow(
             column(6,
                    tableOutput("DetalleCMes")),
             column(1),
             column(2,
                    tableOutput("ResumenContratos"),
                    ),
             column(3,
                    valueBoxOutput("VB_Corto3m", width = "100%"),
                    br(),
                    valueBoxOutput("VB_pCorto3m", width = "100%"))
             
           ),
           
           br(),
           
        ## Saldo Diferencial +KKMM ----
        
        h3("Saldo Diferencial + KKMM"),
        br(),
        fluidRow(column(6,
                        tableOutput("SaldoDiferencial")),
                 column(6,
                        tableOutput("SaldoDiferencial1")
                        )
                 ),
        br(),
        
       fluidRow(column(3,
                       valueBoxOutput("VB_SubtotalSldoD", width = "80%")),
                column(3,
                        valueBoxOutput("VB_SubtotalSldoDTRM", width = "80%")),
                column(3,
                       valueBoxOutput("VB_SubtotalSldoDPe", width = "80%")),
                column(3,
                       valueBoxOutput("VB_TotalSldoD", width = "80%"))
                ),
                
                 
        br(),
       
       ## Capital Cuenta de Futuros
      
       h3("Capital Cuenta de Futuros"),
       br(),
       
       fluidRow(column(4,
                       tableOutput("Capital_CtaF")),
 
                column(8,
                         fluidRow(
                                  column(2),
                                  column(4,
                                         valueBoxOutput("VB_saldoCAdm", width = "80%")),
                                  column(4,
                                         valueBoxOutput("VB_SaldoCStonex", width = "80%")),
                                  column(2)
                                  ),
                       
                       
                       br(),
                       
                         
                         fluidRow(
                                column(4,
                                       valueBoxOutput("VB_SubCapitalCtaF", width = "80%")),
                                br(),
                                column(4,
                                       valueBoxOutput("VB_SubCapitalCtaFPesos", width = "80%")),
                                br(),
                                column(4,
                                       valueBoxOutput("VB_SubCapitalCtaFTRM", width = "80%"))
                                )
                       )
                ),
       
       br(),
       
       
       ## Saldo en CUenta de Compensación
       
       
       h3("Saldo en Cuenta de Compensación"),
       br(),
       
       
       fluidRow(column(6,
                       tableOutput("Cuenta_Compensacion")),
                column(6,
                       fluidRow(column(6,
                                       valueBoxOutput("VB_SldoDisPref", width = "100%")),
                                column(6,
                                       valueBoxOutput("VB_Total_CtaCompensacion", width = "100%"))
                                )
                       )
                

               ),
       
       br(),
       
       
           
  
  
  tags$footer(HTML(" <footer class='page-footer font-large indigo'>
                             <center><img src='logo2.png' width='90' height='90'></center>
                             <div class='footer-copyright text-center py-3'>Año 2022 Copyright:
                             <a href='https://racafe.com.co/es/'> Racafé</a>
                             </div>
                             </footer>")))
  
  
  ))

           

  
 
  

  
  
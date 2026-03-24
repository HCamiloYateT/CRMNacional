body <- bs4DashBody(
  includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
  tags$script(HTML("// Solo aplicar a dropdowns con clase 'custom-dropdown-menu'
                   $(document).on('click', '.custom-dropdown-menu', function (e) {    
                      e.stopPropagation();
                   });")),
  tags$script(HTML("// Botones de la tabla Leads'
                   $(document).on('click', '.btn-action', function() {
                      var action = $(this).data('action');
                      var row = $(this).data('row');
                      var cliente = $(this).data('cliente');
                   Shiny.setInputValue('button_clicked', {
                      action: action,
                      row: row,
                      cliente: cliente
                      }, {priority: 'event'});
                      });")),
  use_waiter(),
  shinyjs::useShinyjs(),
  bs4TabItems(
    # Hoja de Trabajo.
    bs4TabItem(tabName = "HT_Reportes", ResumenTotalUI("ResumenTotal")),
    bs4TabItem(tabName = "HT_Indicadores", ComparacionIndicadoresUI("CompIndicadores")),
    bs4TabItem(tabName = "HT_Calculadoras", CalculadoraUI("Calculadoras")),
    bs4TabItem(tabName = "HT_Presupuesto", PresupuestoUI("PresupuestoTotal")),
    bs4TabItem(tabName = "HT_Pendientes", PendientesUI("Pendientes")),
    bs4TabItem(tabName = "HT_Tareas", noteDisplayUI("NotasTareas")),
    # Oportunidades de Negocio ----
    bs4TabItem(tabName = "OP_Registro", FormularioOportunidadUI("Formulario")),
    bs4TabItem(tabName = "OP_Listado", TablaOportunidadesUI("Listado")),
    bs4TabItem(tabName = "OP_Seguimiento", DashboardOportunidadesUI("Oportunidades")),
    # Clientes.
    bs4TabItem(tabName = "CL_Resumen", DetalleClienteUI("ResumenClientes")),
    bs4TabItem(tabName = "CL_Presupuesto", PresupuestoUI("Presupuesto")),
    bs4TabItem(tabName = "CL_RFM", 
               bs4TabCard(id = "rfm_tabs",title = NULL, width = 12,
                 side = "left", collapsible = FALSE,
                 tabPanel(title = tagList(icon("cubes"), "Volúmen"),
                          RFMUI("RFMClientesSacos")),
                 tabPanel(title = tagList(icon("dollar-sign"), "Márgen"),
                          RFMUI("RFMClientesMargen"))
                 )
               ),
    # Clientes a Recuperar.
    bs4TabItem(tabName = "CR_Resumen", DetalleClienteRecuperarUI("ResumenClientesRecuperar")),
    bs4TabItem(tabName = "CR_Presupuesto",
               DTOutput("ClientesRecuperarConPPto"),
               ),
    bs4TabItem(tabName = "CR_Embudo", SankeyTablaUI("ClienteRecuperar")),
    bs4TabItem(tabName = "CR_RFM", 
               bs4TabCard(id = "rfm_tabs",title = NULL, width = 12,
                          side = "left", collapsible = FALSE, 
                          tabPanel(title = tagList(icon("cubes"), "Volúmen"),
                                   RFMUI("RFMCliRecSacos")),
                          tabPanel(title = tagList(icon("dollar-sign"), "Márgen"),
                                   RFMUI("RFMCliRecMargen"))
                          )
               ),
    # Leads.
    bs4TabItem(tabName = "LE_Listado", TablaModalCeldaUI("ResumenLeads")),
    bs4TabItem(tabName = "LE_Embudo", SankeyTablaUI("Leads")),
    # Consulta Individual
    bs4TabItem(tabName = "IN_Consulta",
               fluidRow(
                 column(4,
                        pickerInput("IND_Cliente",label = h6("Cliente"), width = "100%", 
                                    choices = c("", Unicos(data$PerRazSoc)),  selected = NULL, multiple = F,
                                    options = pick_opt(c("", Unicos(data$PerRazSoc))))
                        ),
                 column(4,
                        pickerInput("IND_LinNeg",label = h6("Línea de Negocio"), width = "100%", 
                                    choices = c("",Unicos(data$CLLinNegNo)),  selected = NULL, multiple = F,
                                    options = pick_opt(c("",Unicos(data$CLLinNegNo))))
                 )
               ),
               fluidRow(
                 column(12, 
                        IndividualUI("ConsultaIndivual"))
               )
               
               )
    )
  )
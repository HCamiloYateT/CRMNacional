# Cliente por pedido: CLCliNit combinado con su razón social
choices_clcli_nit <- data %>%
  distinct(CLCliNit, RazonSocialCliNit) %>%
  filter(!is.na(CLCliNit), !is.na(RazonSocialCliNit)) %>%
  arrange(RazonSocialCliNit) %>%
  {setNames(as.character(.$CLCliNit), paste0(.$CLCliNit, " - ", .$RazonSocialCliNit))}

# Cliente principal: CliNitPpal combinado con razón social y línea de negocio
choices_clinit_ppal <- data %>%
  distinct(CliNitPpal, PerRazSoc) %>%
  filter(!is.na(CliNitPpal), !is.na(PerRazSoc)) %>%
  arrange(PerRazSoc) %>%
  {setNames(as.character(.$CliNitPpal),
            paste0(.$CliNitPpal, " - ", .$PerRazSoc))}

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
    # Embudo Comercial (Contacto → Lead → Cliente).
    bs4TabItem(tabName = "EC_Kanban", KanbanEmbudoUI("EmbudoKanban")),
    bs4TabItem(tabName = "EC_Contactos", TablaContactosUI("EmbudoContactos")),
    bs4TabItem(tabName = "EC_Prospectos", TablaProspectosUI("EmbudoProspectos")),
    bs4TabItem(tabName = "EC_Leads", TablaLeadsUI("EmbudoLeads")),
    bs4TabItem(tabName = "EC_Descartados", TablaDescartadosUI("EmbudoDescartados")),
    bs4TabItem(tabName = "EC_Embudo", EmbudoConversionUI("EmbudoConversion")),
    # Consulta Individual
    bs4TabItem(tabName = "IN_Consulta",
               fluidRow(style = "display: flex; align-items: flex-end;",
                        column(4,
                               div(style = "min-height: 40px;", h6("Cliente Padre")),
                               racafe::ListaDesplegable("IND_CliNitPpal", label = NULL,
                                                        choices = choices_clinit_ppal, selected = NULL,
                                                        multiple = FALSE)
                               ),
                        column(4,
                               div(style = "min-height: 40px;", h6("Cliente Hijo")),
                               racafe::ListaDesplegable("IND_CLCliNit", label = NULL,
                                                        choices = choices_clcli_nit,
                                                        multiple = TRUE)
                               ),
                        column(3,
                               div(style = "min-height: 40px;", h6("L\u00ednea de Negocio")),
                               racafe::ListaDesplegable("IND_LinNeg", label = NULL,
                                                        choices = Unicos(data$CLLinNegNo),
                                                        multiple = TRUE)
                               ),
                        column(1,
                               div(style = "min-height: 40px;", h6("Limpiar")),
                               actionButton("IND_Limpiar", label = NULL, icon = icon("eraser"),
                                            class = "btn-danger", width = "100%")
                               )
                        ),
               fluidRow(
                 column(12, IndividualUI("ConsultaIndivual"))
                 )
               )
    )
  )

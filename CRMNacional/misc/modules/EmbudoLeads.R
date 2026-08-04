# Funciones ----

# Este archivo depende de TablaContactos.R: MostrarModalConClase,
# derivar_etapa_actual, derivar_fecha_entrada_etapa, registrar_transicion_etapa,
# descartar_generico, .coldef_accion, .kpi_card, .rangos_antiguedad,
# EditarContacto/UI, GestionContacto/UI

# Registra (upsert) el vínculo manual de NIT de un lead con el NIT que factura
registrar_vinculo_nit <- function(cod_contacto, nit_vinculado, usr, observacion = NA_character_) {
  fila <- data.frame(CodContacto = cod_contacto, NitVinculado = nit_vinculado, UsuarioVinculo = usr,
                     FechaHoraVinculo = Sys.time(), Observacion = observacion, stringsAsFactors = FALSE)
  ReemplazarDatos(fila, "CRMNALVINCULONIT", llaves = list(CodContacto = cod_contacto))
}

# Carga el vínculo de NIT de un contacto, si existe
cargar_vinculo_nit <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALVINCULONIT") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame(NitVinculado = character()))
}

# NIT efectivo de un contacto/lead: el vinculado manualmente si existe, si
# no, su propio NIT de identificación (PerCod)
obtener_nit_efectivo <- function(cod_contacto, nit_propio) {
  vinculo <- cargar_vinculo_nit(cod_contacto)
  if (nrow(vinculo) > 0) vinculo$NitVinculado[[1]] else nit_propio
}

# Verifica, contra el objeto `data` global de facturación, qué Leads activos
# ya facturaron (por su NIT propio o por su NIT vinculado) y los marca como
# convertidos en CRMNALLEADCLIENTE. FechaConversion usa la fecha real de la
# primera factura, no el momento de la detección.
detectar_conversion_leads <- function(usr = "SISTEMA") {
  leads_activos <- derivar_etapa_actual() %>% filter(Etapa == "LEAD") %>% select(CodContacto, PerCod)
  
  ya_convertidos <- tryCatch(CargarDatos("CRMNALLEADCLIENTE")$CodContacto, error = function(e) character(0))
  leads_activos <- leads_activos %>% filter(!CodContacto %in% ya_convertidos)
  if (nrow(leads_activos) == 0) return(invisible(0))
  
  vinculos <- tryCatch(CargarDatos("CRMNALVINCULONIT"), error = function(e) data.frame(CodContacto = character(), NitVinculado = character()))
  
  leads_activos <- leads_activos %>%
    left_join(vinculos %>% select(CodContacto, NitVinculado), by = "CodContacto") %>%
    mutate(NitEfectivo = suppressWarnings(as.numeric(ifelse(!is.na(NitVinculado), NitVinculado, PerCod)))) %>%
    filter(!is.na(NitEfectivo))
  
  primera_factura_por_nit <- data %>%
    filter(!is.na(CLCliNit), !is.na(FecFact)) %>%
    group_by(CLCliNit) %>%
    summarise(FechaConversion = min(FecFact, na.rm = TRUE), .groups = "drop")
  
  convertidos <- leads_activos %>% inner_join(primera_factura_por_nit, by = c("NitEfectivo" = "CLCliNit"))
  
  for (i in seq_len(nrow(convertidos))) {
    fila <- data.frame(CodContacto = convertidos$CodContacto[i], NitFacturacion = convertidos$NitEfectivo[i],
                       FechaConversion = convertidos$FechaConversion[i], stringsAsFactors = FALSE)
    AgregarDatos(fila, "CRMNALLEADCLIENTE")
    registrar_transicion_etapa(convertidos$CodContacto[i], "LEAD", "CLIENTE", usr,
                               motivo = paste("Factura detectada NIT", convertidos$NitEfectivo[i]))
  }
  invisible(nrow(convertidos))
}

.RAZONES_DESCARTE_LEAD <- c(
  "NO AUTORIZA TRATAMIENTO DE DATOS", "SIN INTERÉS COMERCIAL",
  "DATOS INCOMPLETOS O ERRÓNEOS", "DUPLICADO", "OTRAS"
)

# Modulos Auxiliares ----

## FormularioVincularNit ----

FormularioVincularNitUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    box(title = "Vincular NIT de Facturación", width = 12, collapsible = FALSE,
        FormatearTexto("Úsalo cuando el lead entró con un NIT distinto al que finalmente factura.",
                       tamano_pct = 0.8, color = "#64748B"),
        textInput(ns("VIN_Nit"), label = Obligatorio("NIT que Factura"), width = "100%"),
        textAreaInput(ns("VIN_Observacion"), label = h6("Observación"), value = "", width = "100%", height = "60px")),
    div(style = "text-align: right; margin-top: 10px;",
        actionBttn(ns("VIN_Guardar"), label = "Guardar Vínculo",
                   style = "unite", color = "danger", size = "xs", icon = icon("link")))
  )
}

FormularioVincularNit <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ret <- reactiveVal(0)
    
    data_contacto <- reactive({
      req(cod_contacto())
      CargarDatos("CRMNALCONTACTO") %>% filter(CodContacto == cod_contacto())
    })
    
    observeEvent(cod_contacto(), {
      req(cod_contacto())
      v <- cargar_vinculo_nit(cod_contacto())
      if (nrow(v) > 0) updateTextInput(session, "VIN_Nit", value = v$NitVinculado[[1]])
    })
    
    output$Titulo <- renderUI({
      req(nrow(data_contacto()) > 0)
      identificador <- data_contacto()$PerRazSoc %||% data_contacto()$PerCod
      h4(paste0(data_contacto()$CodContacto, " - ", identificador))
    })
    
    observeEvent(input$VIN_Guardar, {
      if (EsVacio(input$VIN_Nit) || !EsEnteroPositivo(input$VIN_Nit)) {
        showNotification("Ingrese un NIT válido", type = "error", duration = 4)
        return(invisible(NULL))
      }
      tryCatch({
        registrar_vinculo_nit(cod_contacto(), input$VIN_Nit, usr(), input$VIN_Observacion)
        showNotification("NIT vinculado exitosamente", duration = 3, type = "message")
        ret(ret() + 1)
      }, error = function(e) showNotification(paste("Error al vincular:", conditionMessage(e)), duration = 5, type = "error"))
    })
    
    list(n = reactive(ret()))
  })
}

## FormularioDescartarLead ----

FormularioDescartarLeadUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    box(title = "Descartar Lead", width = 12, collapsible = FALSE,
        ListaDesplegable(ns("DES_Razon"), label = Obligatorio("Razón de Descarte"),
                         choices = .RAZONES_DESCARTE_LEAD, selected = NULL, multiple = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("DES_Razon"), "'] == 'OTRAS'"),
          textAreaInput(ns("DES_RazonOtra"), label = Obligatorio("Especifique la razón"),
                        value = "", width = "100%", height = "80px"))),
    div(style = "text-align: right; margin-top: 10px;",
        actionBttn(ns("DES_Solicitar"), label = "Descartar Lead",
                   style = "unite", color = "danger", size = "xs", icon = icon("ban")))
  )
}

FormularioDescartarLead <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    data_contacto <- reactive({
      req(cod_contacto())
      CargarDatos("CRMNALCONTACTO") %>% filter(CodContacto == cod_contacto())
    })
    
    output$Titulo <- renderUI({
      req(nrow(data_contacto()) > 0)
      identificador <- data_contacto()$PerRazSoc %||% data_contacto()$PerCod
      h4(paste0(data_contacto()$CodContacto, " - ", identificador))
    })
    
    motivo_final <- reactive({
      if (identical(input$DES_Razon, "OTRAS")) trimws(input$DES_RazonOtra %||% "") else input$DES_Razon
    })
    
    observeEvent(input$DES_Solicitar, {
      cond <- c(
        "El campo Razón de Descarte es obligatorio" = EsVacio(input$DES_Razon),
        "Debe especificar la razón cuando selecciona OTRAS" =
          identical(input$DES_Razon, "OTRAS") && EsVacio(input$DES_RazonOtra)
      )
      if (any(cond)) {
        sapply(names(cond), function(x) if (cond[[x]]) showNotification(x, duration = 4, type = "error"))
        return(invisible(NULL))
      }
      MostrarModalConClase(
        modalDialog(
          title = "Confirmar descarte", easyClose = FALSE,
          footer = tagList(
            racafeShiny::Boton(ns("DES_Cancelar"), label = "Cancelar", icono = "xmark",
                               color_fondo = "transparent", color_fuente = "#6c757d"),
            racafeShiny::Boton(ns("DES_Confirmar"), label = "Descartar Lead",
                               icono = "ban", color_fondo = "#C11007")
          ),
          tags$p(paste0("¿Deseas descartar este lead? Motivo: ", motivo_final()))
        ), "aviso"
      )
    })
    
    observeEvent(input$DES_Cancelar, { removeModal() })
    
    observeEvent(input$DES_Confirmar, {
      tryCatch({
        descartar_generico(cod_contacto(), "LEAD", motivo_final(), usr())
        removeModal()
        showNotification("Lead descartado exitosamente", duration = 4, type = "message")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al descartar:", conditionMessage(e)), duration = 6, type = "error")
      })
    })
    
    list(n = reactive(ret()))
  })
}

# Modulo Principal ----

## TablaLeads ----

TablaLeadsUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(3, offset = 9, ListaDesplegable(ns("filtro_estado"), label = h6("Sub Estado"),
                                             choices = c("ACTIVO", "DESCARTADO", "TODOS"), selected = "ACTIVO", multiple = FALSE))
    ),
    br(),
    fluidRow(
      column(6, box(title = "Antigüedad en Lead", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_antiguedad"), height = "220px"))),
      column(6, box(title = "Leads por Asesor", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_asesor"), height = "220px")))
    ),
    br(),
    TablaReactableUI(ns("tabla_leads"), titulo = "Leads",
                     footer = "La conversión a Cliente es automática al detectar factura (NIT propio o vinculado).", footer_tipo = "info")
  )
}

TablaLeads <- function(id, usr) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    
    observeEvent(refresh_trigger(), { detectar_conversion_leads(usr()) }, ignoreNULL = FALSE)
    
    # Etapa derivada del historial; los datos propios de Lead (Asesor,
    # Segmento, LinNegocio) se traen aparte desde CONTACTOLEAD
    leads_raw <- reactive({
      refresh_trigger()
      lead_data <- CargarDatos("CONTACTOLEAD") %>% select(CodContacto, Asesor, Segmento, LinNegocio)
      
      derivar_etapa_actual() %>%
        filter(Etapa == "LEAD") %>%
        derivar_fecha_entrada_etapa() %>%
        select(-any_of(c("Asesor", "Segmento", "LinNegocio"))) %>%
        left_join(lead_data, by = "CodContacto") %>%
        mutate(DiasEnLead = as.numeric(difftime(Sys.time(), FechaEntradaEtapa, units = "days")),
               RangoAntiguedad = .rangos_antiguedad(DiasEnLead))
    })
    
    leads_filtrados <- reactive({
      dat <- leads_raw()
      if (input$filtro_estado != "TODOS") dat <- dat %>% filter(Estado == input$filtro_estado)
      dat
    })
    
    output$kpi_antiguedad <- plotly::renderPlotly({
      dat <- leads_filtrados() %>% count(RangoAntiguedad, .drop = FALSE, name = "n")
      .grafico_barras_horizontal(dat, "RangoAntiguedad", "n", color = "#1C398E", titulo_x = "Leads")
    })
    
    output$kpi_asesor <- plotly::renderPlotly({
      dat <- leads_filtrados() %>% mutate(Asesor = ifelse(is.na(Asesor), "SIN ASIGNAR", Asesor)) %>% count(Asesor, sort = TRUE, name = "n")
      .grafico_barras_horizontal(dat, "Asesor", "n", color = "#0F6E56", titulo_x = "Leads")
    })
    
    data_tabla <- reactive({
      leads_filtrados() %>%
        mutate(Acciones = CodContacto) %>%
        arrange(desc(FechaEntradaEtapa)) %>%
        select(Acciones, CodContacto, PerRazSoc, PerCod, Asesor, Segmento, LinNegocio, DiasEnLead, Estado)
    })
    
    mod_tabla <- TablaReactable(
      id = "tabla_leads", data = data_tabla, columnas = NULL,
      col_specs = list(
        Acciones = .coldef_dropdown_acciones(ns, c(editar = "Editar", comentar = "Comentar",
                                                   vincular = "Vincular NIT", descartar = "Descartar")),
        CodContacto = reactable::colDef(show = FALSE),
        PerRazSoc   = reactable::colDef(name = "Razón Social", minWidth = 170),
        PerCod      = reactable::colDef(name = "NIT", minWidth = 100),
        Asesor      = reactable::colDef(name = "Asesor", minWidth = 100),
        Segmento    = reactable::colDef(name = "Segmento", minWidth = 100),
        LinNegocio  = reactable::colDef(name = "Línea de Negocio", minWidth = 130),
        DiasEnLead  = reactable::colDef(name = "Días en Lead", minWidth = 100, cell = function(v) round(v, 0)),
        Estado      = reactable::colDef(name = "Sub Estado", minWidth = 100)
      ),
      modo_seleccion = "ninguno", id_col = "CodContacto",
      sortable = TRUE, searchable = TRUE, page_size = 20, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = TRUE
    )
    
    editar_cod_rv <- reactiveVal(NULL)
    EditarContacto(id = "mod_editar", usr = usr, cod_contacto = reactive(editar_cod_rv()))
    
    gestion_cod_rv <- reactiveVal(NULL)
    GestionContacto(id = "mod_gestion", usr = usr, cod_contacto = reactive(gestion_cod_rv()))
    
    vincular_cod_rv <- reactiveVal(NULL)
    vincular_mod <- FormularioVincularNit(id = "mod_vincular", usr = usr, cod_contacto = reactive(vincular_cod_rv()))
    
    descartar_cod_rv <- reactiveVal(NULL)
    descartar_mod <- FormularioDescartarLead(id = "mod_descartar", usr = usr, cod_contacto = reactive(descartar_cod_rv()))
    
    observeEvent(input$accion_tabla, {
      acc <- input$accion_tabla
      req(acc$cod_contacto, acc$accion)
      cod <- acc$cod_contacto
      
      if (acc$accion == "editar") {
        editar_cod_rv(cod)
        showModal(modalDialog(title = "Editar Lead", size = "xl", easyClose = TRUE, footer = modalButton("Cerrar"),
                              EditarContactoUI(ns("mod_editar"))))
      } else if (acc$accion == "comentar") {
        gestion_cod_rv(cod)
        showModal(modalDialog(title = "Gestión Comercial", size = "l", easyClose = TRUE, footer = modalButton("Cerrar"),
                              GestionContactoUI(ns("mod_gestion"))))
      } else if (acc$accion == "vincular") {
        vincular_cod_rv(cod)
        showModal(modalDialog(title = "Vincular NIT", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioVincularNitUI(ns("mod_vincular"))))
      } else if (acc$accion == "descartar") {
        descartar_cod_rv(cod)
        showModal(modalDialog(title = "Descartar Lead", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioDescartarLeadUI(ns("mod_descartar"))))
      }
    })
    
    observeEvent(vincular_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(descartar_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
  })
}

# App de prueba ----
# Requiere haber cargado antes TablaContactos.R y TablaProspectos.R

ui <- bs4DashPage(
  title = "Prueba Tab Leads",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(), controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    TablaLeadsUI("tab_leads")
  )
)

server <- function(input, output, session) {
  TablaLeads("tab_leads", usr = reactive("CMEDINA"))
}

shinyApp(ui, server)
# Funciones ----

# Este archivo depende de TablaContactos.R: MostrarModalConClase,
# derivar_etapa_actual, registrar_transicion_etapa, descartar_generico,
# determinar_linea_negocio, .coldef_accion, .kpi_card, .rangos_antiguedad,
# EditarContacto/UI, GestionContacto/UI

# Catálogo de motivos de descarte de Prospecto — propio de esta etapa,
# distinto al de Contacto/Lead (contexto de alianzas, no de venta directa)
.RAZONES_DESCARTE_PROSPECTO <- c(
  "SIN CLIENTE ALIADO DISPONIBLE EN LA ZONA", "VOLUMEN INSUFICIENTE INCLUSO PARA ALIANZA",
  "CLIENTE ALIADO NO ACEPTÓ LA RELACIÓN", "SIN INTERÉS EN EL MODELO DE ALIANZA", "OTRAS"
)

# Genera un identificador único para una alianza de prospecto
.generar_id_alianza <- function() paste0("AL-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))

# Registra una alianza (Prospecto vinculado a un Cliente existente)
registrar_alianza_prospecto <- function(cod_contacto, cod_cliente_aliado, usr, observacion = NA_character_) {
  fila <- data.frame(
    IdAlianza = .generar_id_alianza(), CodContacto = cod_contacto, CodClienteAliado = cod_cliente_aliado,
    UsuarioCrea = usr, FechaHoraCrea = Sys.time(), Observacion = observacion, stringsAsFactors = FALSE
  )
  AgregarDatos(fila, "CRMNALPROSPECTOALIANZA")
  invisible(fila)
}

# Lista las alianzas registradas para un prospecto, con la razón social del
# cliente aliado ya resuelta
listar_alianzas_prospecto <- function(cod_contacto) {
  alianzas <- tryCatch({
    CargarDatos("CRMNALPROSPECTOALIANZA") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame(IdAlianza = character(), CodContacto = character(),
                                    CodClienteAliado = character(), Observacion = character()))
  if (nrow(alianzas) == 0) return(alianzas %>% mutate(ClienteAliado = character()))
  
  clientes <- CargarDatos("CRMNALCONTACTO") %>% select(CodContacto, PerRazSoc, PerCod)
  alianzas %>%
    left_join(clientes, by = c("CodClienteAliado" = "CodContacto")) %>%
    mutate(ClienteAliado = coalesce(PerRazSoc, PerCod, CodClienteAliado)) %>%
    select(-PerRazSoc, -PerCod)
}

# Elimina una alianza (DELETE vía ReemplazarDatos con 0 filas)
# Trae todas las alianzas activas, con razón social del Prospecto y del
# Cliente aliado ya resueltas — base para el KPI "Prospectos por Alianza"
listar_todas_las_alianzas <- function() {
  alianzas <- tryCatch(CargarDatos("CRMNALPROSPECTOALIANZA"), error = function(e) data.frame(CodContacto = character(), CodClienteAliado = character()))
  if (nrow(alianzas) == 0) return(alianzas %>% mutate(ClienteAliado = character()))
  
  clientes <- CargarDatos("CRMNALCONTACTO") %>% select(CodContacto, PerRazSoc, PerCod)
  alianzas %>%
    left_join(clientes, by = c("CodClienteAliado" = "CodContacto")) %>%
    mutate(ClienteAliado = coalesce(PerRazSoc, PerCod, CodClienteAliado)) %>%
    select(-PerRazSoc, -PerCod)
}

# Badges HTML con la razón social de cada Cliente aliado de un Prospecto —
# reemplaza mostrar solo el conteo numérico de alianzas
.badges_alianzas <- function(cod_contacto) {
  alianzas <- listar_alianzas_prospecto(cod_contacto)
  if (nrow(alianzas) == 0) return("<span style='color:#94A3B8; font-size:11px;'>Sin alianzas</span>")
  paste(sapply(alianzas$ClienteAliado, function(nombre) {
    as.character(tags$span(
      style = "display:inline-block; margin:2px; padding:2px 8px; border-radius:10px; background:#F0E6D2; color:#C8862A; font-size:11px;",
      nombre
    ))
  }), collapse = " ")
}

eliminar_alianza_prospecto <- function(id_alianza) {
  vacio <- CargarDatos("CRMNALPROSPECTOALIANZA") %>% filter(FALSE)
  ReemplazarDatos(vacio, "CRMNALPROSPECTOALIANZA", llaves = list(IdAlianza = id_alianza))
}

# Catálogo de Clientes disponibles para vincular como aliados — Etapa
# derivada, no presencia directa en una tabla
catalogo_clientes_aliados <- function() {
  derivar_etapa_actual() %>%
    filter(Etapa == "CLIENTE") %>%
    mutate(Etiqueta = paste0(CodContacto, " - ", coalesce(PerRazSoc, PerCod))) %>%
    select(CodContacto, Etiqueta)
}

# Convierte un Contacto en Prospecto: registra 1+ alianzas y el historial de
# la transición. No requiere Asesor/Segmento/Línea de Negocio.
convertir_contacto_a_prospecto <- function(cod_contacto, cods_cliente_aliado, usr, observacion = NA_character_) {
  if (length(cods_cliente_aliado) == 0) stop("Debe vincular al menos una alianza")
  for (cod_cliente in cods_cliente_aliado) registrar_alianza_prospecto(cod_contacto, cod_cliente, usr, observacion)
  registrar_transicion_etapa(cod_contacto, "CONTACTO", "PROSPECTO", usr)
  invisible(NULL)
}

# Inserta la conversión a Lead en CONTACTOLEAD y registra el historial —
# compartida por convertir_contacto_a_lead() (en TablaContactos.R, vía este
# mismo helper) y convertir_prospecto_a_lead()
.insertar_conversion_lead <- function(cod_contacto, asesor, segmento, linea_negocio_input, etapa_anterior, usr) {
  linea <- determinar_linea_negocio(linea_negocio_input)
  fila_lead <- data.frame(
    CodContacto = cod_contacto, FechaConversion = Sys.time(), CodLinNegocio = linea$cod,
    LinNegocio = linea$nombre, Asesor = asesor, Segmento = segmento, stringsAsFactors = FALSE
  )
  AgregarDatos(fila_lead, "CONTACTOLEAD")
  registrar_transicion_etapa(cod_contacto, etapa_anterior, "LEAD", usr)
  invisible(fila_lead)
}

# Convierte un Contacto en Lead (redefine la versión que pudiera existir en
# TablaContactos.R — esta es la vigente, ya que centraliza la lógica
# compartida con Prospecto vía .insertar_conversion_lead)
convertir_contacto_a_lead <- function(cod_contacto, asesor, segmento, linea_negocio_input, usr) {
  .insertar_conversion_lead(cod_contacto, asesor, segmento, linea_negocio_input, etapa_anterior = "CONTACTO", usr = usr)
}

# Reclasifica un Prospecto a Lead — las alianzas existentes NO se eliminan,
# quedan como historial de que ese Lead alguna vez fue Prospecto
convertir_prospecto_a_lead <- function(cod_contacto, asesor, segmento, linea_negocio_input, usr) {
  .insertar_conversion_lead(cod_contacto, asesor, segmento, linea_negocio_input, etapa_anterior = "PROSPECTO", usr = usr)
}

# Modulos Auxiliares ----

## FormularioReclasificarLead ----

FormularioReclasificarLeadUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    box(title = "Reclasificar a Lead", width = 12, collapsible = FALSE,
        FormatearTexto("El volumen de este Prospecto ahora sí interesa para venta directa. Las alianzas ya registradas quedan como historial.",
                       tamano_pct = 0.8, color = "#64748B"),
        ListaDesplegable(ns("RLE_Asesor"), label = Obligatorio("Asesor"),
                         choices = Choices()$personas, selected = NULL, multiple = FALSE),
        ListaDesplegable(ns("RLE_Segmento"), label = Obligatorio("Segmento"),
                         choices = Choices()$segmento, selected = NULL, multiple = FALSE),
        ListaDesplegable(ns("RLE_LinNeg"), label = Obligatorio("Línea de Negocio"),
                         choices = Choices()$linneg, selected = NULL, multiple = FALSE)),
    div(style = "text-align: right; margin-top: 10px;",
        actionBttn(ns("RLE_Solicitar"), label = "Reclasificar a Lead",
                   style = "unite", color = "success", size = "xs", icon = icon("arrow-up")))
  )
}

FormularioReclasificarLead <- function(id, usr, cod_contacto) {
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
    
    validar_campos_reclasificacion <- function() {
      c(
        "El campo Asesor es obligatorio" = EsVacio(input$RLE_Asesor),
        "El campo Segmento es obligatorio" = EsVacio(input$RLE_Segmento),
        "El campo Línea de Negocio es obligatorio" = EsVacio(input$RLE_LinNeg)
      )
    }
    
    observeEvent(input$RLE_Solicitar, {
      cond <- validar_campos_reclasificacion()
      if (any(cond)) {
        sapply(names(cond), function(x) if (cond[[x]]) showNotification(x, duration = 4, type = "error"))
        return(invisible(NULL))
      }
      MostrarModalConClase(
        modalDialog(
          title = "Confirmar reclasificación a Lead", easyClose = FALSE,
          footer = tagList(
            racafeShiny::Boton(ns("RLE_Cancelar"), label = "Cancelar", icono = "xmark",
                               color_fondo = "transparent", color_fuente = "#6c757d"),
            racafeShiny::Boton(ns("RLE_Confirmar"), label = "Reclasificar a Lead",
                               icono = "arrow-up", color_fondo = "#198754")
          ),
          tags$p(paste0("¿Deseas reclasificar este Prospecto a Lead con asesor ", input$RLE_Asesor,
                        ", segmento ", input$RLE_Segmento, " y línea de negocio ", input$RLE_LinNeg, "?"))
        ), "aviso"
      )
    })
    
    observeEvent(input$RLE_Cancelar, { removeModal() })
    
    observeEvent(input$RLE_Confirmar, {
      tryCatch({
        convertir_prospecto_a_lead(cod_contacto(), input$RLE_Asesor, input$RLE_Segmento, input$RLE_LinNeg, usr())
        removeModal()
        showNotification("Prospecto reclasificado a Lead exitosamente", duration = 4, type = "message")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al reclasificar:", conditionMessage(e)), duration = 6, type = "error")
      })
    })
    
    list(n = reactive(ret()))
  })
}

## FormularioDescartarProspecto ----

FormularioDescartarProspectoUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    box(title = "Descartar Prospecto", width = 12, collapsible = FALSE,
        ListaDesplegable(ns("DES_Razon"), label = Obligatorio("Razón de Descarte"),
                         choices = .RAZONES_DESCARTE_PROSPECTO, selected = NULL, multiple = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("DES_Razon"), "'] == 'OTRAS'"),
          textAreaInput(ns("DES_RazonOtra"), label = Obligatorio("Especifique la razón"),
                        value = "", width = "100%", height = "80px"))),
    div(style = "text-align: right; margin-top: 10px;",
        actionBttn(ns("DES_Solicitar"), label = "Descartar Prospecto",
                   style = "unite", color = "danger", size = "xs", icon = icon("ban")))
  )
}

FormularioDescartarProspecto <- function(id, usr, cod_contacto) {
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
            racafeShiny::Boton(ns("DES_Confirmar"), label = "Descartar Prospecto",
                               icono = "ban", color_fondo = "#C11007")
          ),
          tags$p(paste0("¿Deseas descartar este prospecto? Motivo: ", motivo_final()))
        ), "aviso"
      )
    })
    
    observeEvent(input$DES_Cancelar, { removeModal() })
    
    observeEvent(input$DES_Confirmar, {
      tryCatch({
        descartar_generico(cod_contacto(), "PROSPECTO", motivo_final(), usr())
        removeModal()
        showNotification("Prospecto descartado exitosamente", duration = 4, type = "message")
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

## TablaProspectos ----

TablaProspectosUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(3, offset = 9, ListaDesplegable(ns("filtro_estado"), label = h6("Sub Estado"),
                                             choices = c("ACTIVO", "DESCARTADO", "TODOS"), selected = "ACTIVO", multiple = FALSE))
    ),
    br(),
    fluidRow(
      column(6, box(title = "Antigüedad en Prospecto", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_antiguedad"), height = "220px"))),
      column(6, box(title = "Prospectos por Alianza", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_alianzas"), height = "220px")))
    ),
    br(),
    TablaReactableUI(ns("tabla_prospectos"), titulo = "Prospectos",
                     footer = "Un Prospecto se gestiona vía alianzas con Clientes existentes, no con venta directa.", footer_tipo = "info")
  )
}

TablaProspectos <- function(id, usr) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    
    prospectos_raw <- reactive({
      refresh_trigger()
      alianzas <- tryCatch(CargarDatos("CRMNALPROSPECTOALIANZA"), error = function(e) data.frame(CodContacto = character()))
      n_alianzas <- alianzas %>% count(CodContacto, name = "NumAlianzas")
      
      derivar_etapa_actual() %>%
        filter(Etapa == "PROSPECTO") %>%
        derivar_fecha_entrada_etapa() %>%
        left_join(n_alianzas, by = "CodContacto") %>%
        mutate(
          NumAlianzas = coalesce(NumAlianzas, 0),
          DiasEnEtapa = as.numeric(difftime(Sys.time(), FechaEntradaEtapa, units = "days")),
          RangoAntiguedad = .rangos_antiguedad(DiasEnEtapa)
        )
    })
    
    prospectos_filtrados <- reactive({
      dat <- prospectos_raw()
      if (input$filtro_estado != "TODOS") dat <- dat %>% filter(Estado == input$filtro_estado)
      dat
    })
    
    output$kpi_antiguedad <- plotly::renderPlotly({
      dat <- prospectos_filtrados() %>% count(RangoAntiguedad, .drop = FALSE, name = "n")
      .grafico_barras_horizontal(dat, "RangoAntiguedad", "n", color = "#1C398E", titulo_x = "Prospectos")
    })
    
    output$kpi_alianzas <- plotly::renderPlotly({
      cods_activos <- prospectos_filtrados()$CodContacto
      dat <- listar_todas_las_alianzas() %>%
        filter(CodContacto %in% cods_activos) %>%
        distinct(CodContacto, ClienteAliado) %>%
        count(ClienteAliado, sort = TRUE, name = "n")
      .grafico_barras_horizontal(dat, "ClienteAliado", "n", color = "#C8862A", titulo_x = "Prospectos vinculados")
    })
    
    data_tabla <- reactive({
      prospectos_filtrados() %>%
        rowwise() %>%
        mutate(AlianzasBadges = .badges_alianzas(CodContacto)) %>%
        ungroup() %>%
        mutate(Acciones = CodContacto) %>%
        arrange(desc(FechaEntradaEtapa)) %>%
        select(Acciones, CodContacto, PerRazSoc, PerCod, AlianzasBadges, DiasEnEtapa, Estado)
    })
    
    mod_tabla <- TablaReactable(
      id = "tabla_prospectos", data = data_tabla, columnas = NULL,
      col_specs = list(
        Acciones = .coldef_dropdown_acciones(ns, c(editar = "Editar", comentar = "Comentar",
                                                   reclasificar = "Reclasificar a Lead", descartar = "Descartar")),
        CodContacto = reactable::colDef(show = FALSE),
        PerRazSoc   = reactable::colDef(name = "Razón Social", minWidth = 180),
        PerCod      = reactable::colDef(name = "NIT", minWidth = 100),
        AlianzasBadges = reactable::colDef(name = "Alianzas", html = TRUE, minWidth = 220),
        DiasEnEtapa = reactable::colDef(name = "Días en Prospecto", minWidth = 130, cell = function(v) round(v, 0)),
        Estado      = reactable::colDef(name = "Sub Estado", minWidth = 100)
      ),
      modo_seleccion = "ninguno", id_col = "CodContacto",
      sortable = TRUE, searchable = TRUE, page_size = 20, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = TRUE
    )
    
    editar_cod_rv <- reactiveVal(NULL)
    EditarContacto(id = "mod_editar", usr = usr, cod_contacto = reactive(editar_cod_rv()))
    
    gestion_cod_rv <- reactiveVal(NULL)
    GestionContacto(id = "mod_gestion", usr = usr, cod_contacto = reactive(gestion_cod_rv()))
    
    reclasificar_cod_rv <- reactiveVal(NULL)
    reclasificar_mod <- FormularioReclasificarLead(id = "mod_reclasificar", usr = usr, cod_contacto = reactive(reclasificar_cod_rv()))
    
    descartar_cod_rv <- reactiveVal(NULL)
    descartar_mod <- FormularioDescartarProspecto(id = "mod_descartar", usr = usr, cod_contacto = reactive(descartar_cod_rv()))
    
    observeEvent(input$accion_tabla, {
      acc <- input$accion_tabla
      req(acc$cod_contacto, acc$accion)
      cod <- acc$cod_contacto
      
      if (acc$accion == "editar") {
        editar_cod_rv(cod)
        showModal(modalDialog(title = "Editar Prospecto", size = "xl", easyClose = TRUE, footer = modalButton("Cerrar"),
                              EditarContactoUI(ns("mod_editar"))))
      } else if (acc$accion == "comentar") {
        gestion_cod_rv(cod)
        showModal(modalDialog(title = "Gestión Comercial", size = "l", easyClose = TRUE, footer = modalButton("Cerrar"),
                              GestionContactoUI(ns("mod_gestion"))))
      } else if (acc$accion == "reclasificar") {
        reclasificar_cod_rv(cod)
        showModal(modalDialog(title = "Reclasificar a Lead", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioReclasificarLeadUI(ns("mod_reclasificar"))))
      } else if (acc$accion == "descartar") {
        descartar_cod_rv(cod)
        showModal(modalDialog(title = "Descartar Prospecto", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioDescartarProspectoUI(ns("mod_descartar"))))
      }
    })
    
    observeEvent(reclasificar_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(descartar_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
  })
}

# App de prueba ----
# Requiere haber cargado antes TablaContactos.R

ui <- bs4DashPage(
  title = "Prueba Tab Prospectos",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(), controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    TablaProspectosUI("tab_prospectos")
  )
)

server <- function(input, output, session) {
  TablaProspectos("tab_prospectos", usr = reactive("CMEDINA"))
}

shinyApp(ui, server)
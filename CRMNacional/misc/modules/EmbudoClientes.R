# Funciones ----

# Este archivo depende de TablaContactos.R (MostrarModalConClase,
# registrar_transicion_etapa, descartar_generico, .coldef_dropdown_acciones,
# GestionContacto/UI) y de TablaDescartados.R (.RAZONES_DESCARTE_CONTACTO,
# .RAZONES_DESCARTE_LEAD, .RAZONES_DESCARTE_PROSPECTO). Requiere además
# CajaModal/CajaModalUI (racafeModulos) e IndFormulario/UI (Individual.R,
# ya existente en tu CRM) para la edición por Unidad Comercial.

# Catálogo de motivos de descarte de Cliente — propio de esta etapa
.RAZONES_DESCARTE_CLIENTE <- c(
  "CAMBIO DE PROVEEDOR", "CIERRE DE OPERACIONES", "SIN RESPUESTA A GESTIÓN DE RECUPERACIÓN",
  "INCONFORMIDAD CON PRODUCTO O SERVICIO", "CONDICIONES COMERCIALES NO COMPETITIVAS", "OTRAS"
)

# Amplía la categorización de motivos (usada en TablaDescartados y Embudo)
# para que también reconozca el catálogo de Cliente — sobreescribe la
# versión definida en TablaDescartados.R con una que incluye los 4 catálogos
.categoria_motivo_descarte <- function(motivo) {
  catalogos <- unique(c(.RAZONES_DESCARTE_CONTACTO, .RAZONES_DESCARTE_LEAD,
                        .RAZONES_DESCARTE_PROSPECTO, .RAZONES_DESCARTE_CLIENTE))
  ifelse(motivo %in% catalogos, motivo, "OTRAS")
}

# Encuentra el CodContacto del embudo que corresponde a una Unidad Comercial
# (CliNitPpal + LinNegCod). Devuelve NA si esa UC nunca pasó por el embudo
# (cliente histórico, anterior a este sistema) — en ese caso no hay nada
# que descartar desde aquí.
buscar_cod_contacto_por_uc <- function(cli_nit_ppal, lin_neg_cod) {
  clientes <- tryCatch(CargarDatos("CRMNALLEADCLIENTE"), error = function(e) data.frame(CodContacto = character(), NitFacturacion = character()))
  leads    <- tryCatch(CargarDatos("CONTACTOLEAD"), error = function(e) data.frame(CodContacto = character(), CodLinNegocio = character()))
  
  match <- clientes %>%
    filter(as.character(NitFacturacion) == as.character(cli_nit_ppal)) %>%
    inner_join(leads %>% filter(as.character(CodLinNegocio) == as.character(lin_neg_cod)), by = "CodContacto")
  
  if (nrow(match) == 0) return(NA_character_)
  match$CodContacto[[1]]
}

# Descarta la Unidad Comercial (Cliente + Línea de Negocio) en el embudo —
# NO toca CRMNALCLIENTE/Excluir, son conceptos independientes
descartar_cliente_uc <- function(cli_nit_ppal, lin_neg_cod, motivo, usr) {
  cod <- buscar_cod_contacto_por_uc(cli_nit_ppal, lin_neg_cod)
  if (is.na(cod)) stop("Esta Unidad Comercial no tiene registro en el embudo comercial; no se puede descartar desde aquí.")
  descartar_generico(cod, "CLIENTE", motivo, usr)
  invisible(cod)
}

# Trae TODAS las Unidades Comerciales (CliNitPpal + LinNegCod) con su
# estado vigente en CRMNALSEGR y contexto de CRMNALCLIENTE — a escala de
# toda la base, no de un solo cliente
todas_unidades_comerciales <- function() {
  segr <- tryCatch(CargarDatos("CRMNALSEGR"), error = function(e) data.frame())
  if (nrow(segr) == 0 || !"CliNitPpal" %in% names(segr)) return(data.frame())
  
  ultimo_segr <- segr %>%
    mutate(FecProceso = as_datetime(FecProceso)) %>%
    group_by(LinNegCod, CliNitPpal) %>%
    filter(FecProceso == max(FecProceso)) %>%
    slice(1) %>%
    ungroup()
  
  clientes <- tryCatch(CargarDatos("CRMNALCLIENTE"), error = function(e) data.frame())
  ctx <- if (nrow(clientes) > 0) {
    clientes %>%
      mutate(FecProceso = as_datetime(FecProceso)) %>%
      group_by(LinNegCod, CliNitPpal) %>%
      filter(FecProceso == max(FecProceso)) %>%
      slice(1) %>%
      ungroup() %>%
      select(LinNegCod, CliNitPpal, Segmento, Asesor, Excluir) %>%
      mutate(LinNegCod = as.character(LinNegCod), CliNitPpal = as.character(CliNitPpal))
  } else {
    data.frame(LinNegCod = character(), CliNitPpal = character(), Segmento = character(), Asesor = character(), Excluir = character())
  }
  
  razsoc <- tryCatch(CargarDatos("CRMNALCONTACTO") %>% select(PerCod, PerRazSoc) %>% distinct() %>%
                       mutate(PerCod = as.character(PerCod)),
                     error = function(e) data.frame(PerCod = character(), PerRazSoc = character()))
  
  ultimo_segr %>%
    mutate(LinNegCod = as.character(LinNegCod), CliNitPpal = as.character(CliNitPpal)) %>%
    left_join(ctx, by = c("LinNegCod", "CliNitPpal")) %>%
    left_join(razsoc, by = c("CliNitPpal" = "PerCod"))
}

# Modulos Auxiliares ----

## FormularioDescartarClienteUC ----

FormularioDescartarClienteUCUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    box(title = "Descartar Cliente (Unidad Comercial)", width = 12, collapsible = FALSE,
        FormatearTexto("Esto descarta únicamente esta línea de negocio del cliente en el embudo comercial. No modifica su exclusión en CRMNALCLIENTE.",
                       tamano_pct = 0.8, color = "#64748B"),
        ListaDesplegable(ns("DES_Razon"), label = Obligatorio("Razón de Descarte"),
                         choices = .RAZONES_DESCARTE_CLIENTE, selected = NULL, multiple = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("DES_Razon"), "'] == 'OTRAS'"),
          textAreaInput(ns("DES_RazonOtra"), label = Obligatorio("Especifique la razón"),
                        value = "", width = "100%", height = "80px"))),
    div(style = "text-align: right; margin-top: 10px;",
        actionBttn(ns("DES_Solicitar"), label = "Descartar Cliente",
                   style = "unite", color = "danger", size = "xs", icon = icon("ban")))
  )
}

FormularioDescartarClienteUC <- function(id, usr, uc) {
  moduleServer(id, function(input, output, session) {
    
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    output$Titulo <- renderUI({
      req(uc())
      h4(paste0(uc()$cli_nit_ppal, " — Línea ", uc()$lin_neg_cod))
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
          title = "Confirmar descarte de Cliente", easyClose = FALSE,
          footer = tagList(
            racafeShiny::Boton(ns("DES_Cancelar"), label = "Cancelar", icono = "xmark",
                               color_fondo = "transparent", color_fuente = "#6c757d"),
            racafeShiny::Boton(ns("DES_Confirmar"), label = "Descartar Cliente",
                               icono = "ban", color_fondo = "#C11007")
          ),
          tags$p(paste0("¿Deseas descartar esta Unidad Comercial? Motivo: ", motivo_final()))
        ), "aviso"
      )
    })
    
    observeEvent(input$DES_Cancelar, { removeModal() })
    
    observeEvent(input$DES_Confirmar, {
      req(uc())
      tryCatch({
        descartar_cliente_uc(uc()$cli_nit_ppal, uc()$lin_neg_cod, motivo_final(), usr())
        removeModal()
        showNotification("Cliente descartado exitosamente en esta línea de negocio", duration = 4, type = "message")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al descartar:", conditionMessage(e)), duration = 6, type = "error")
      })
    })
    
    list(n = reactive(ret()))
  })
}

## TablaUnidadesComerciales ----
# Sub-módulo genérico: tabla de UC + Editar/Comentar (+Descartar opcional),
# usado como contenido de cada una de las 3 CajaModal

TablaUnidadesComercialesUI <- function(id) {
  ns <- NS(id)
  reactable::reactableOutput(ns("tabla"))
}

TablaUnidadesComerciales <- function(id, usr, dat, mostrar_descartar = FALSE, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$tabla <- reactable::renderReactable({
      d <- dat()
      if (nrow(d) == 0) return(reactable::reactable(data.frame(Mensaje = "Sin Unidades Comerciales en esta categoría")))
      d <- d %>% mutate(Acciones = paste(LinNegCod, CliNitPpal, sep = "||"))
      
      opciones <- c(editar = "Editar", comentar = "Comentar", oportunidad = "Crear Oportunidad")
      if (mostrar_descartar) opciones <- c(opciones, descartar = "Descartar")
      
      reactable::reactable(
        d, sortable = TRUE, compact = TRUE, searchable = TRUE, defaultPageSize = 10,
        columns = list(
          Acciones = .coldef_dropdown_acciones(ns, opciones),
          PerRazSoc = reactable::colDef(name = "Razón Social", minWidth = 160),
          CliNitPpal = reactable::colDef(name = "NIT", minWidth = 100),
          LinNegCod = reactable::colDef(show = FALSE),
          SegmentoRacafe = reactable::colDef(name = "Estado", minWidth = 130),
          Segmento = reactable::colDef(name = "Segmento", minWidth = 100),
          Asesor = reactable::colDef(name = "Asesor", minWidth = 100),
          Excluir = reactable::colDef(name = "Excluido (CRMNALCLIENTE)", minWidth = 130)
        )
      )
    })
    
    identidad_indformulario <- reactiveVal(NULL)
    uc_seleccionada <- reactiveVal(NULL)
    gestion_cod_rv <- reactiveVal(NULL)
    
    IndFormulario(paste0(id, "_editar"), identidad = reactive(identidad_indformulario()),
                  dat = reactive(dat() %>% filter(LinNegCod == (uc_seleccionada()$lin_neg_cod %||% "___"))),
                  usr = usr)
    GestionContacto(id = paste0(id, "_gestion"), usr = usr, cod_contacto = reactive(gestion_cod_rv()))
    descartar_mod <- FormularioDescartarClienteUC(id = paste0(id, "_descartar"), usr = usr, uc = reactive(uc_seleccionada()))
    
    # Crea Oportunidad reutilizando el módulo compartido — mismo patrón de Contactos/Leads/Prospectos
    dd_oportunidad <- reactiveVal(NULL)
    oportunidad_trigger <- reactiveVal(0)
    FormularioOportunidad(paste0(id, "_oportunidad"), dd_data = reactive(dd_oportunidad()),
                          dat = reactive(data), usr = usr, trigger_update = oportunidad_trigger,
                          tipo_cliente_default = reactive("CLIENTE"))
    
    observeEvent(input$accion_tabla, {
      acc <- input$accion_tabla
      req(acc$cod_contacto, acc$accion)
      partes <- strsplit(acc$cod_contacto, "\\|\\|")[[1]]
      lin_neg_cod <- partes[1]; cli_nit_ppal <- partes[2]
      uc_seleccionada(list(lin_neg_cod = lin_neg_cod, cli_nit_ppal = cli_nit_ppal))
      
      # Busca el CodContacto del embudo para esta UC (necesario para Comentar)
      cod_embudo <- buscar_cod_contacto_por_uc(cli_nit_ppal, lin_neg_cod)
      
      if (acc$accion == "editar") {
        identidad_indformulario(list(nit = suppressWarnings(as.numeric(cli_nit_ppal)),
                                     linneg_cod = suppressWarnings(as.numeric(lin_neg_cod))))
        showModal(modalDialog(title = "Editar Unidad Comercial", size = "l", easyClose = TRUE, footer = modalButton("Cerrar"),
                              IndFormularioUI(ns(paste0(id, "_editar")))))
      } else if (acc$accion == "comentar") {
        if (is.na(cod_embudo)) {
          showNotification("Esta Unidad Comercial no tiene registro en el embudo comercial; no hay gestión que mostrar aquí.",
                           type = "warning", duration = 5)
          return(invisible(NULL))
        }
        gestion_cod_rv(cod_embudo)
        showModal(modalDialog(title = "Gestión Comercial", size = "l", easyClose = TRUE, footer = modalButton("Cerrar"),
                              GestionContactoUI(ns(paste0(id, "_gestion")))))
      } else if (acc$accion == "descartar") {
        showModal(modalDialog(title = "Descartar Cliente", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioDescartarClienteUCUI(ns(paste0(id, "_descartar")))))
      } else if (acc$accion == "oportunidad") {
        fila <- dat() %>% filter(LinNegCod == lin_neg_cod, CliNitPpal == cli_nit_ppal)
        req(nrow(fila) > 0)
        dd_oportunidad(list(fila_completa = list(PerRazSoc = fila$PerRazSoc[[1]])))
        showModal(modalDialog(title = paste0("Nueva Oportunidad — ", fila$PerRazSoc[[1]]), size = "l",
                              easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioOportunidadUI(ns(paste0(id, "_oportunidad")))))
      }
    })
    
    observeEvent(oportunidad_trigger(), { removeModal() }, ignoreInit = TRUE)
    
    observeEvent(descartar_mod$n(), {
      removeModal()
      refresh_trigger(isolate(refresh_trigger()) + 1)
    })
  })
}

# Modulo Principal ----

## TablaClientes ----

TablaClientesUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tabsetPanel(
      tabPanel("Clientes Activos", br(), TablaUnidadesComercialesUI(ns("tabla_activos"))),
      tabPanel("Clientes Inactivos (A Recuperar)", br(), TablaUnidadesComercialesUI(ns("tabla_inactivos"))),
      tabPanel("Clientes Nuevos", br(), TablaUnidadesComercialesUI(ns("tabla_nuevos")))
    )
  )
}

TablaClientes <- function(id, usr) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    
    unidades <- reactive({
      refresh_trigger()
      todas_unidades_comerciales()
    })
    
    dat_activos   <- reactive({ unidades() %>% filter(SegmentoRacafe == "CLIENTE") })
    dat_inactivos <- reactive({ unidades() %>% filter(SegmentoRacafe == "CLIENTE A RECUPERAR") })
    dat_nuevos    <- reactive({ unidades() %>% filter(!SegmentoRacafe %in% c("CLIENTE", "CLIENTE A RECUPERAR")) })
    
    TablaUnidadesComerciales("tabla_activos", usr = usr, dat = dat_activos, mostrar_descartar = FALSE, refresh_trigger = refresh_trigger)
    TablaUnidadesComerciales("tabla_inactivos", usr = usr, dat = dat_inactivos, mostrar_descartar = TRUE, refresh_trigger = refresh_trigger)
    TablaUnidadesComerciales("tabla_nuevos", usr = usr, dat = dat_nuevos, mostrar_descartar = FALSE, refresh_trigger = refresh_trigger)
  })
}

# App de prueba ----
# Requiere haber cargado antes TablaContactos.R, TablaDescartados.R, e
# IndFormulario (Individual.R de tu CRM)

ui <- bs4DashPage(
  title = "Prueba Tab Clientes",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(), controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    TablaClientesUI("EmbudoClientes")
  )
)

server <- function(input, output, session) {
  TablaClientes("EmbudoClientes", usr = reactive("CMEDINA"))
}

shinyApp(ui, server)
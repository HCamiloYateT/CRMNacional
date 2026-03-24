DetalleLeadsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        12,
        div(style = "margin-top: 10px;",
            GTBotonesUI(ns("TablaLeads")))
      )
    )
  )
}
DetalleLeads <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Datos procesados ----
    data_leads <- reactive({
      req(dat())
      
      dat() %>%
        select(
          PerRazSoc, LinNegocio, Asesor, EstadoCuenta) %>%
        mutate(
          pct_missing = rowSums(is.na(.)) / ncol(.)
        )

    })
    
    ## Tabla GT ----
    mi_tabla_gt <- reactive({
      df <- data_leads()
      
      if (nrow(df) == 0) {
        return(
          gt_mensaje_vacio("No hay leads disponibles")
        )
      }
      
      df %>%
        crear_link_cliente(
          col_razsoc = "PerRazSoc",
          col_linneg = "LinNegocio"
        ) %>%
        gt() %>%
        tab_header(
          title    = "Detalle de Leads",
          subtitle = paste("Total registros:", nrow(df))
        ) %>%
        cols_label(
          PerRazSoc   = "Razón Social",
          LinNegocio  = "Línea de Negocio",
          Asesor      = "Asesor",
          EstadoCuenta = "Estado de Cuenta",
          pct_missing = "% Información Ausente"
        ) %>%
        fmt_markdown(columns = PerRazSoc) %>%
        fmt_percent(columns = pct_missing, decimals = 2) %>%
        gt_minimal_style() %>%
        gt::opt_interactive(
          use_pagination = FALSE,
          use_filters    = FALSE,
          use_resizers   = TRUE
        )
    })
    
    ## Configuración de botones ----
    botones_config <- crear_config_botones(
      oportunidad = list(
        titulo        = "Crear Oportunidad",
        tit_modal     = "Crear Oportunidad -",
        module_ui     = "FormularioOportunidadUI",
        module_server = "FormularioOportunidad",
        module_id     = "mod_formulario",
        icono         = "hand-holding-dollar",
        extra_params  = list(
          dat            = dat,
          usr            = usr,
          trigger_update = trigger_update,
          tipo_cliente_default = reactive("LEAD")
        )
      )
    )
    
    ## Reactivos de drilldown ----
    dd <- setNames(
      lapply(names(botones_config), function(x) reactiveVal(NULL)),
      paste0(names(botones_config), "_dd")
    )
    
    ## UI de sub-módulos ----
    modulos_ui <- setNames(
      lapply(botones_config, function(cfg) {
        ui_fun <- get(cfg$module_ui)
        function(ns_parent) ui_fun(ns_parent(cfg$module_id))
      }),
      paste0(names(botones_config), "_module")
    )
    
    ## Tabla con botones ----
    GTBotones("TablaLeads", gt_table = mi_tabla_gt, data = data_leads,
              nombre_col = "PerRazSoc", nombre_col_sec = NULL,
              botones_config = botones_config,
              modulos_ui     = modulos_ui,
              lado_botones   = "inicio")
  })
}

### App de prueba ----
ui <- bs4DashPage(
  title = "Prueba ResumenTotal",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(useShinyjs(),
                     DetalleLeadsUI("resumen")
  )
)

server <- function(input, output, session) {
  DetalleLeads(
    "resumen",
    dat = reactive(CargarDatos("CRMNALLEAD")),
    usr = reactive("HCYATE"),
    trigger_update = reactive(0)
  )
}

shinyApp(ui, server)
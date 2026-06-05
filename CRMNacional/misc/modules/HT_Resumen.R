# Control de Vista ----
.spec_cajas_resumen <- list(
  list(seccion   = "Presupuesto",
       icono_sec = "file-invoice-dollar",
       cajas     = list(list(id = "kpi_cumpl_sacos",  columnas = 3L),
                        list(id = "kpi_cumpl_margen", columnas = 3L))
  ),
  list(seccion   = "Unidades Comerciales",
       icono_sec = "users",
       cajas     = list(list(id = "kpi_activas",   columnas = 4L),
                        list(id = "kpi_recuperar", columnas = 4L),
                        list(id = "kpi_nuevas",    columnas = 4L))
  ),
  list(seccion   = "Actividad Comercial",
       icono_sec = "bullseye",
       cajas     = list(list(id = "kpi_leads",         columnas = 3L),
                        list(id = "kpi_oportunidades", columnas = 3L),
                        list(id = "kpi_cohorte",       columnas = 3L),
                        list(id = "kpi_competencia",   columnas = 3L))
  )
)

# Módulo ----
ResumenTotalUI <- function(id) {
  ns <- NS(id)
  .seccion_ui <- function(titulo, icono) {
    tags$div(
      style = paste0(
        "display:flex; align-items:center; gap:8px; ",
        "margin:18px 0 6px 0; padding-bottom:6px; ",
        "border-bottom:2px solid #E2E8F0;"
      ),
      tags$span(style = "color:#64748B; font-size:13px;", icon(icono)),
      tags$span(titulo, style = paste0(
        "font-size:13px; font-weight:700; color:#374151; ",
        "letter-spacing:0.03em; text-transform:uppercase;"
      ))
    )
  }
  bloques <- purrr::map(.spec_cajas_resumen, function(sec) {
    tagList(
      .seccion_ui(sec$seccion, sec$icono_sec),
      fluidRow(purrr::map(sec$cajas, ~ column(.x$columnas, CajaModalUI(ns(.x$id)))))
    )
  })
  tagList(!!!bloques)
}
ResumenTotal <- function(id, dat, dat_t, dat_c, dat_leads, dat_oportunidades, dat_competencia,
                         clientes_raw, data_cohortes, segmentos_raw, fact_r, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observe({
      ids_plotly <- c(
        paste0(ns("detalle_activas-factmes")),
        paste0(ns("detalle_activas-conppto")),
        paste0(ns("detalle_activas-meses")),
        paste0(ns("detalle_recuperar-segmento")),
        paste0(ns("detalle_recuperar-conppto")),
        paste0(ns("detalle_recuperar-meses")),
        paste0(ns("detalle_nuevas-segmento")),
        paste0(ns("detalle_nuevas-linneg")),
        paste0(ns("detalle_nuevas-bucketsacos"))
      )
      session$userData$.plotlyShinyEventIDs <- unique(c(
        session$userData$.plotlyShinyEventIDs, ids_plotly
      ))
    }, priority = 1000L)
    
    # Helpers ----
    
    .mes_es <- function(fecha) {
      meses <- c(
        "enero", "febrero", "marzo", "abril", "mayo", "junio",
        "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
      )
      paste(meses[month(fecha)], year(fecha))
    }
    
    .fondo_cumpl <- function(v) {
      dplyr::case_when(
        is.na(v)  ~ "white",
        v >= 1.00 ~ "#EFF6FF",
        v >= 0.80 ~ "#F0FDF4",
        v >= 0.50 ~ "#FFFBEB",
        TRUE      ~ "#FEF2F2"
      )
    }
    
    # Registra CajaModal con defaults comunes para cajas de conteo entero
    .caja_std <- function(id, valor_r, texto, icono, footer_r, ui_modal_fn,
                          titulo_modal, icono_modal = icono) {
      CajaModal(
        id              = id,
        valor           = reactive(html_valor(valor_r(), formato = "entero")),
        formato         = "entero",
        texto           = texto,
        icono           = icono,
        colores         = c(fondo = "white"),
        mostrar_boton   = TRUE,
        titulo_modal    = titulo_modal,
        icono_modal     = icono_modal,
        contenido_modal = ui_modal_fn,
        footer          = footer_r,
        footer_class    = "caja-modal-footer"
      )
    }
    
    # Reactivos base ----
    
    corte_mes <- reactive({ PrimerDia(Sys.Date()) })
    
    # Universo dimensional activo — fuente única de verdad para todos los filtros.
    # universo_nits : para UC (tienen CliNitPpal en los datos transaccionales)
    # universo_razsoc: para actividad comercial (leads/oportunidades usan PerRazSoc)
    universo_nits <- reactive({
      dat_t() %>% select(LinNegCod, CliNitPpal) %>% distinct()
    })
    universo_razsoc <- reactive({
      dat_t() %>% select(LinNegCod, PerRazSoc) %>% distinct()
    })
    
    # Snapshot CRMNALSEGR al corte, cruzado contra universo_nits.
    # Aplica Asesor + Segmento + LinNeg heredados de dat_t().
    segr_corte <- reactive({
      req(nrow(universo_nits()) > 0)
      segmentos_raw() %>%
        filter(FecProceso == corte_mes()) %>%
        semi_join(universo_nits(), by = c("LinNegCod", "CliNitPpal"))
    })
    
    # Referencia histórica global del corte — sin cruzar con universo (para nuevos)
    nits_segr_corte <- reactive({
      segmentos_raw() %>%
        filter(FecProceso == corte_mes()) %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
    })
    
    # Historial de facturación anterior al mes vigente — usa FACT global (DataPrep).
    # CORRECCIÓN: fact_r() = fact_cache está colapsado por CLLotCod, no por FctNit.
    # FACT tiene MinFecFact y FctNit agrupados por cliente.
    nits_fact_antes_mes <- reactive({
      FACT %>%
        filter(as.Date(MinFecFact) < corte_mes()) %>%
        select(CliNitPpal = FctNit) %>%
        distinct()
    })
    
    # UCs con transacción en el mes corrido — dat() hereda filtros de dat_t()
    nits_mes_corrido <- reactive({
      dat() %>%
        filter(PrimerDia(FecFact) == corte_mes()) %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
    })
    
    # Dataframes de UC ----
    # Fuente única de verdad para conteo Y drill-down; sin re-filtro en módulos hijos.
    
    df_activas <- reactive({
      nits <- segr_corte() %>%
        filter(SegmentoRacafe == "CLIENTE") %>%
        select(LinNegCod, CliNitPpal)
      dat_c() %>% semi_join(nits, by = c("LinNegCod", "CliNitPpal"))
    })
    
    df_recuperar <- reactive({
      nits <- segr_corte() %>%
        filter(SegmentoRacafe == "CLIENTE A RECUPERAR") %>%
        select(LinNegCod, CliNitPpal)
      dat_c() %>% semi_join(nits, by = c("LinNegCod", "CliNitPpal"))
    })
    
    df_nuevas <- reactive({
      nits <- nits_mes_corrido() %>%
        anti_join(nits_segr_corte(),     by = c("LinNegCod", "CliNitPpal")) %>%
        anti_join(nits_fact_antes_mes(), by = "CliNitPpal")
      dat_c() %>% semi_join(nits, by = c("LinNegCod", "CliNitPpal"))
    })
    
    # Dataframes de actividad comercial ----
    # Leads: dat_leads() ya filtró por LinNegCod/Segmento/Asesor en server.R.
    # El semi_join aquí garantiza consistencia si el universo cambia en sesión.
    df_leads <- reactive({
      dat_leads() %>%
        semi_join(universo_razsoc(), by = c("PerRazSoc", "LinNegCod"))
    })
    
    # Oportunidades: dat_oportunidades() ya hizo inner_join con dims en server.R.
    # El semi_join refuerza el cruce por PerRazSoc + LinNegCod.
    df_oportunidades <- reactive({
      dat_oportunidades() %>%
        semi_join(universo_razsoc(), by = c("PerRazSoc", "LinNegCod"))
    })
    
    # Competencia: CRMNALCOMPETENCIA solo tiene PerRazSoc; filtro por %in%.
    df_competencia <- reactive({ dat_competencia() })
    
    # Cohorte: panel_full filtrado al mes vigente y al universo activo.
    # El conteo del KPI usa solo cliente_id distintos en el mes vigente.
    df_cohorte <- reactive({
      d <- data_cohortes()
      req(!is.null(d), !is.null(d$panel_full), !is.null(d$mes_vigente))
      nits_activos <- universo_nits() %>% pull(CliNitPpal) %>% unique()
      d$panel_full %>%
        filter(ym == d$mes_vigente, CliNitPpal %in% nits_activos) %>%
        distinct(cliente_id)
    })
    
    # Conteos — única fuente numérica para KPI y drill-down ----
    val_activas   <- reactive({ n_distinct(df_activas()$LinNegCod,   df_activas()$CliNitPpal) })
    val_recuperar <- reactive({ n_distinct(df_recuperar()$LinNegCod, df_recuperar()$CliNitPpal) })
    val_nuevas    <- reactive({ n_distinct(df_nuevas()$LinNegCod,    df_nuevas()$CliNitPpal) })
    val_leads     <- reactive({ n_distinct(df_leads()$PerRazSoc) })
    
    val_oportunidades <- reactive({
      n_distinct(
        df_oportunidades()$LinNegCod, df_oportunidades()$PerRazSoc,
        df_oportunidades()$Categoria, df_oportunidades()$Producto
      )
    })
    
    val_competencia <- reactive({ n_distinct(df_competencia()$Competencia) })
    val_cohorte     <- reactive({ nrow(df_cohorte()) })
    
    # KPIs de presupuesto YTD ----
    
    periodo_r <- reactive({ year(max(dat()$FecFact, na.rm = TRUE)) })
    
    ejec_ytd_r <- reactive({
      mes <- month(Sys.Date())
      dat() %>%
        filter(!is.na(FecFact), year(FecFact) == periodo_r(), month(FecFact) <= mes) %>%
        mutate(
          KILOS     = if_else(LinNegCod == 10000L, SacLote * 62.5, SacLote * 70),
          KilosFact = if_else(is.na(KilosFact), KILOS, KilosFact),
          Margen    = if_else(is.infinite(Margen), NA_real_, Margen)
        ) %>%
        summarise(
          sacos  = sum(KilosFact / 70, na.rm = TRUE),
          margen = sum(Margen,          na.rm = TRUE)
        )
    })
    
    ppto_ytd_r <- reactive({
      mes <- month(Sys.Date())
      clientes_raw() %>%
        filter(year(FecProceso) == year(Sys.Date())) %>%
        group_by(LinNegCod, CliNitPpal) %>%
        filter(FecProceso == max(FecProceso)) %>%
        slice(1L) %>%
        ungroup() %>%
        inner_join(
          dat_t() %>% select(LinNegCod, Segmento, Asesor) %>% distinct(),
          by = c("LinNegCod", "Segmento", "Asesor")
        ) %>%
        summarise(
          ppto_sacos  = sum(SSPpto,    na.rm = TRUE) / 12 * mes,
          ppto_margen = sum(MNFCCPpto, na.rm = TRUE) / 12 * mes
        )
    })
    
    kpi_cumpl_sacos_r <- reactive({
      e <- ejec_ytd_r(); p <- ppto_ytd_r()
      list(
        cumpl   = SiError_0(e$sacos  / p$ppto_sacos),
        ejec    = e$sacos,
        ppto    = p$ppto_sacos,
        periodo = periodo_r(),
        mes     = month.name[month(Sys.Date())]
      )
    })
    
    kpi_cumpl_margen_r <- reactive({
      e <- ejec_ytd_r(); p <- ppto_ytd_r()
      list(
        cumpl   = SiError_0(e$margen / p$ppto_margen),
        ejec    = e$margen,
        ppto    = p$ppto_margen,
        periodo = periodo_r(),
        mes     = month.name[month(Sys.Date())]
      )
    })
    
    # Registro de módulos hijos ----
    # Todos reciben df_* para que KPI y drill-down sean exactamente el mismo universo.
    DetalleCliente(         "detalle_activas",      df_activas,      usr, trigger_update)
    DetalleClienteRecuperar("detalle_recuperar",    df_recuperar,    usr, trigger_update)
    DetalleClienteNuevo(    "detalle_nuevas",        df_nuevas,       usr, trigger_update)
    DashboardLeads(         "detalle_leads",         df_leads,        usr)
    DashboardOportunidades( "detalle_oportunidades", df_oportunidades, usr)
    Cohortes(               "detalle_cohortes",      data_cohortes)
    DetalleCompetencia(     "detalle_competencia")
    Presupuesto(            "detalle_presupuesto",   dat_t, clientes_raw)
    
    # Cajas KPI Presupuesto ----
    CajaModal("kpi_cumpl_sacos",
              valor           = reactive(kpi_cumpl_sacos_r()$cumpl),
              formato         = "porcentaje",
              texto           = reactive(paste0(
                "Cumplimiento Sacos \u2014 ",
                kpi_cumpl_sacos_r()$mes, " ", kpi_cumpl_sacos_r()$periodo
              )),
              icono           = "check-double",
              colores         = reactive(c(fondo = "white")),
              color_fondo_hex = reactive(.fondo_cumpl(kpi_cumpl_sacos_r()$cumpl)),
              mostrar_boton   = TRUE,
              titulo_modal    = "Detalle \u2014 Seguimiento de Presupuesto",
              icono_modal     = "chart-line",
              contenido_modal = function() PresupuestoUI(ns("detalle_presupuesto")),
              footer = reactive(paste0(
                FormatearNumero(kpi_cumpl_sacos_r()$ejec, "coma"), " sacos facturados de ",
                FormatearNumero(kpi_cumpl_sacos_r()$ppto, "coma"),
                " presupuestados acumulados al mes."
              ) %>% HTML),
              footer_class = "caja-modal-footer"
    )
    
    CajaModal("kpi_cumpl_margen",
              valor           = reactive(kpi_cumpl_margen_r()$cumpl),
              formato         = "porcentaje",
              texto           = reactive(paste0(
                "Cumplimiento Margen \u2014 ",
                kpi_cumpl_margen_r()$mes, " ", kpi_cumpl_margen_r()$periodo
              )),
              icono           = "dollar-sign",
              colores         = reactive(c(fondo = "white")),
              color_fondo_hex = reactive(.fondo_cumpl(kpi_cumpl_margen_r()$cumpl)),
              mostrar_boton   = TRUE,
              titulo_modal    = "Detalle \u2014 Seguimiento de Presupuesto",
              icono_modal     = "chart-line",
              contenido_modal = function() PresupuestoUI(ns("detalle_presupuesto")),
              footer = reactive(paste0(
                FormatearNumero(kpi_cumpl_margen_r()$ejec, "dinero"), " facturado de ",
                FormatearNumero(kpi_cumpl_margen_r()$ppto, "dinero"),
                " presupuestado acumulado al mes."
              ) %>% HTML),
              footer_class = "caja-modal-footer"
    )
    
    # Cajas KPI UC ----
    .caja_std(
      id           = "kpi_activas",
      valor_r      = val_activas,
      texto        = "Clientes Activos",
      icono        = "users",
      titulo_modal = "Detalle \u2014 Clientes Activos",
      ui_modal_fn  = function() DetalleClienteUI(ns("detalle_activas")),
      footer_r     = reactive(paste0(
        "UC clasificadas como CLIENTE en la segmentaci\u00f3n del ",
        format(corte_mes(), "%d/%m/%Y"), "."
      ))
    )
    
    .caja_std(
      id           = "kpi_recuperar",
      valor_r      = val_recuperar,
      texto        = "Clientes a Recuperar",
      icono        = "user-clock",
      titulo_modal = "Detalle \u2014 Clientes a Recuperar",
      ui_modal_fn  = function() DetalleClienteRecuperarUI(ns("detalle_recuperar")),
      footer_r     = reactive(paste0(
        "UC clasificadas como CLIENTE A RECUPERAR en la segmentaci\u00f3n del ",
        format(corte_mes(), "%d/%m/%Y"), "."
      ))
    )
    
    .caja_std(
      id           = "kpi_nuevas",
      valor_r      = val_nuevas,
      texto        = "Clientes Nuevos",
      icono        = "user-plus",
      titulo_modal = "Detalle \u2014 Clientes Nuevos",
      ui_modal_fn  = function() DetalleClienteNuevoUI(ns("detalle_nuevas")),
      footer_r     = reactive(paste0(
        "UC con factura en ", .mes_es(Sys.Date()),
        " sin registro previo en CRMNALSEGR ni en el historial de facturaci\u00f3n."
      ))
    )
    
    # Cajas KPI Comercial ----
    .caja_std(
      id           = "kpi_leads",
      valor_r      = val_leads,
      texto        = "Leads Creados",
      icono        = "address-card",
      titulo_modal = "Detalle \u2014 Leads",
      ui_modal_fn  = function() DashboardLeadsUI(ns("detalle_leads")),
      footer_r     = reactive(
        "Prospectos registrados en el embudo comercial para el periodo activo."
      )
    )
    
    .caja_std(
      id           = "kpi_oportunidades",
      valor_r      = val_oportunidades,
      texto        = "Oportunidades Creadas",
      icono        = "handshake",
      titulo_modal = "Detalle \u2014 Oportunidades",
      ui_modal_fn  = function() DashboardOportunidadesUI(ns("detalle_oportunidades")),
      footer_r     = reactive(paste0(
        "Combinaciones \u00fanicas de cliente, l\u00ednea y producto ",
        "con oportunidad de venta registrada en el periodo."
      ))
    )
    
    .caja_std(
      id           = "kpi_cohorte",
      valor_r      = val_cohorte,
      texto        = "Clientes en Cohorte",
      icono        = "arrows-to-eye",
      titulo_modal = "Detalle \u2014 Cohorte",
      ui_modal_fn  = function() CohortesUI(ns("detalle_cohortes")),
      footer_r     = reactive({
        d <- data_cohortes()
        paste0(
          "Poblaci\u00f3n base al ", format(d$mes_inicio, "%d/%m/%Y"),
          " m\u00e1s altas del periodo. Total mes ",
          format(d$mes_vigente, "%B %Y"), "."
        )
      })
    )
    
    .caja_std(
      id           = "kpi_competencia",
      valor_r      = val_competencia,
      texto        = "Competidor(es) Registrado(s)",
      icono        = "building-flag",
      titulo_modal = "Detalle \u2014 Competencia",
      ui_modal_fn  = function() DetalleCompetenciaUI(ns("detalle_competencia")),
      footer_r     = reactive(
        "Marcas competidoras con registros activos para el universo de clientes del filtro."
      )
    )
    
  })
}
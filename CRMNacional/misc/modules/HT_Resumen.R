# Control de Vista ----
.spec_cajas_resumen <- list(list(seccion   = "Presupuesto",
                                 icono_sec = "file-invoice-dollar",
                                 cajas     = list(list(id = "kpi_cumpl_sacos",   columnas = 3L),
                                                  list(id = "kpi_cumpl_margen",  columnas = 3L))
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
                                                  list(id = "kpi_competencia",   columnas = 3L)
                                                  )
                                 )
                            )

# Modulo ----
ResumenTotalUI <- function(id) {
  ns <- NS(id)
  # Separador visual de sección
  .seccion_ui <- function(titulo, icono) {
    tags$div(
      style = paste0(
        "display:flex; align-items:center; gap:8px; ",
        "margin:18px 0 6px 0; padding-bottom:6px; ",
        "border-bottom:2px solid #E2E8F0;"
      ),
      tags$span(style = "color:#64748B; font-size:13px;", icon(icono)),
      tags$span(
        titulo,
        style = paste0(
          "font-size:13px; font-weight:700; color:#374151; ",
          "letter-spacing:0.03em; text-transform:uppercase;"
        )
      )
    )
  }
  # UI generada desde la especificación — no hardcodear cajas aquí
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
    
    # Helpers ----
    
    # Nombre del mes en español
    .mes_es <- function(fecha) {
      meses <- c(
        "enero", "febrero", "marzo", "abril", "mayo", "junio",
        "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
      )
      paste(meses[month(fecha)], year(fecha))
    }
    
    # Semáforo de fondo para KPIs de cumplimiento porcentual
    .fondo_cumpl <- function(v) {
      dplyr::case_when(
        is.na(v)  ~ "white",
        v >= 1.00 ~ "#EFF6FF",
        v >= 0.80 ~ "#F0FDF4",
        v >= 0.50 ~ "#FFFBEB",
        TRUE      ~ "#FEF2F2"
      )
    }
    
    # Helper: registra CajaModal con defaults comunes del landing (UC y comercial)
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
    # Corte: primer día del mes en curso
    corte_mes <- reactive({ PrimerDia(Sys.Date()) })
    
    # Snapshot de segmentos en el corte, enriquecido con dimensiones del filtro.
    # segmentos_raw viene del server (segmentos_raw_cache) — sin carga adicional a BD.
    segr_corte <- reactive({
      linneg_activos <- tryCatch(
        dat_t() %>% pull(LinNegCod) %>% unique(),
        error = function(e) NULL
      )
      req(!is.null(linneg_activos), length(linneg_activos) > 0)
      segmentos_raw() %>%
        filter(FecProceso == corte_mes(), LinNegCod %in% linneg_activos)
    })
    
    # NITs con registro en CRMNALSEGR en el corte — para detección de nuevos
    nits_segr_corte <- reactive({
      segmentos_raw() %>%
        filter(FecProceso == corte_mes()) %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
    })
    
    # NITs con factura antes del mes vigente — usa fact_r (reactiveVal del server)
    nits_fact_antes_mes <- reactive({
      fact_r() %>%
        filter(as.Date(FecPrimerFact) < corte_mes()) %>%
        select(CliNitPpal = FctNit) %>%
        distinct()
    })
    
    # UCs con transacción en el mes corrido
    nits_mes_corrido <- reactive({
      dat() %>%
        filter(PrimerDia(FecFact) == corte_mes()) %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
    })
    
    # Dataframes de UC — fuente única de verdad para conteo y drill-down ----
    
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
        anti_join(nits_segr_corte(),    by = c("LinNegCod", "CliNitPpal")) %>%
        anti_join(nits_fact_antes_mes(), by = "CliNitPpal")
      dat_c() %>% semi_join(nits, by = c("LinNegCod", "CliNitPpal"))
    })
    
    # Conteos derivados — única fuente de verdad numérica ----
    val_activas   <- reactive({ n_distinct(df_activas()$LinNegCod,   df_activas()$CliNitPpal) })
    val_recuperar <- reactive({ n_distinct(df_recuperar()$LinNegCod, df_recuperar()$CliNitPpal) })
    val_nuevas    <- reactive({ n_distinct(df_nuevas()$LinNegCod,    df_nuevas()$CliNitPpal) })
    
    val_leads <- reactive({ n_distinct(dat_leads()$PerRazSoc) })
    
    val_oportunidades <- reactive({
      n_distinct(
        dat_oportunidades()$LinNegCod, dat_oportunidades()$CliNitPpal,
        dat_oportunidades()$Categoria, dat_oportunidades()$Producto
      )
    })
    
    val_competencia <- reactive({ n_distinct(dat_competencia()$Competencia) })
    
    val_cohorte <- reactive({
      dat <- data_cohortes()
      req(!is.null(dat), !is.null(dat$panel_full), !is.null(dat$mes_vigente))
      dat$panel_full %>%
        filter(ym == dat$mes_vigente) %>%
        distinct(cliente_id) %>%
        nrow()
    })
    
    # KPIs de presupuesto — agregación YTD sobre reactivos ya en memoria ----
    # dat()         = data_f  (facturas filtradas)
    # dat_t()       = data_t  (universo dimensional, sin filtro de fecha)
    # clientes_raw()= clientes_raw_cache$get()
    # No se hace ninguna carga a BD aquí.
    
    periodo_r <- reactive({ year(max(dat()$FecFact, na.rm = TRUE)) })
    
    # Ejecución YTD: sacos y margen acumulados hasta el mes actual desde dat()
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
    
    # Presupuesto YTD: desde clientes_raw ya en memoria — sin CargarDatos adicional
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
    
    # KPIs de cumplimiento compuestos ----
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
    DetalleCliente(         "detalle_activas",        df_activas,   usr, trigger_update)
    DetalleClienteRecuperar("detalle_recuperar",      df_recuperar, usr, trigger_update)
    DetalleClienteNuevo(    "detalle_nuevas",         df_nuevas,    usr, trigger_update)
    DashboardLeads(         "detalle_leads",          dat_leads)
    DashboardOportunidades( "detalle_oportunidades",  dat_oportunidades, usr)
    Cohortes(               "detalle_cohortes",       data_cohortes)
    DetalleCompetencia(     "detalle_competencia")
    Presupuesto(            "detalle_presupuesto",    dat_t, clientes_raw)
    
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
                FormatearNumero(kpi_cumpl_sacos_r()$ppto, "coma"), " presupuestados acumulados al mes."
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
                FormatearNumero(kpi_cumpl_margen_r()$ppto, "dinero"), " presupuestado acumulado al mes."
              ) %>% HTML),
              footer_class = "caja-modal-footer"
    )
    
    # Cajas KPI UC — usando helper .caja_std ----
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
      footer_r     = reactive("Prospectos registrados en el embudo comercial para el periodo activo.")
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
        dat <- data_cohortes()
        paste0(
          "Poblaci\u00f3n base al ", format(dat$mes_inicio, "%d/%m/%Y"),
          " m\u00e1s altas del periodo. Total mes ",
          format(dat$mes_vigente, "%B %Y"), "."
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
      footer_r     = reactive(paste0(
        "Marcas competidoras con registros activos para el universo de clientes del filtro."
      ))
    )
    
  })
}
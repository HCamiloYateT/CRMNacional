# Control de Vista ----
.spec_cajas_resumen <- list(
  list(seccion   = "Presupuesto",
       icono_sec = "file-invoice-dollar",
       cajas = list(list(id = "kpi_cumpl_sacos",  columnas = 3L),
                    list(id = "kpi_cumpl_margen", columnas = 3L))),
  list(seccion   = "Unidades Comerciales",
       icono_sec = "users",
       cajas = list(list(id = "kpi_activas",   columnas = 4L),
                    list(id = "kpi_recuperar", columnas = 4L),
                    list(id = "kpi_nuevas",    columnas = 4L))),
  list(seccion   = "Actividad Comercial",
       icono_sec = "bullseye",
       cajas = list(list(id = "kpi_leads",        columnas = 3L),
                    list(id = "kpi_oportunidades", columnas = 3L),
                    list(id = "kpi_cohorte",       columnas = 3L),
                    list(id = "kpi_competencia",   columnas = 3L)))
)

# Módulo UI ----
ResumenTotalUI <- function(id) {
  ns <- NS(id)
  .seccion_ui <- function(titulo, icono) {
    tags$div(
      style = paste0("display:flex; align-items:center; gap:8px; ",
                     "margin:18px 0 6px 0; padding-bottom:6px; ",
                     "border-bottom:2px solid #E2E8F0;"),
      tags$span(style = "color:#64748B; font-size:13px;", icon(icono)),
      tags$span(titulo,
                style = paste0("font-size:13px; font-weight:700; color:#374151; ",
                               "letter-spacing:0.03em; text-transform:uppercase;"))
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

# Módulo Server ----
ResumenTotal <- function(id, dat, dat_t, dat_c, dat_leads, dat_oportunidades,
                         dat_competencia, clientes_raw, data_cohortes,
                         segmentos_raw, fact_r, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helpers ----
    .mes_es <- function(fecha) {
      meses <- c("enero","febrero","marzo","abril","mayo","junio",
                 "julio","agosto","septiembre","octubre","noviembre","diciembre")
      paste(meses[month(fecha)], year(fecha))
    }
    .fondo_cumpl <- function(v) {
      dplyr::case_when(is.na(v) ~ "white", v >= 1.00 ~ "#EFF6FF",
                       v >= 0.80 ~ "#F0FDF4", v >= 0.50 ~ "#FFFBEB", TRUE ~ "#FEF2F2")
    }
    
    # Wrapper UC
    .caja_uc <- function(id, valor_r, texto, icono, titulo_modal, ui_modal_fn, footer_r) {
      CajaModal(
        id = id,
        valor = reactive(html_valor(valor_r(), formato = "entero")),
        formato = "entero", texto = texto, icono = icono,
        colores = c(fondo = "white"), mostrar_boton = TRUE,
        titulo_modal = titulo_modal, icono_modal = icono,
        contenido_modal = ui_modal_fn,
        footer = reactive({
          if (valor_r() == 0)
            HTML("<span style='color:#B91C1C;'>Sin unidades comerciales para los filtros activos.</span>")
          else footer_r()
        }),
        footer_class = "caja-modal-footer"
      )
    }
    
    .caja_std <- function(id, valor_r, texto, icono, footer_r, ui_modal_fn,
                          titulo_modal, icono_modal = icono) {
      CajaModal(
        id = id,
        valor = reactive(html_valor(valor_r(), formato = "entero")),
        formato = "entero", texto = texto, icono = icono,
        colores = c(fondo = "white"), mostrar_boton = TRUE,
        titulo_modal = titulo_modal, icono_modal = icono_modal,
        contenido_modal = ui_modal_fn,
        footer = footer_r, footer_class = "caja-modal-footer"
      )
    }
    
    # Reactivos base ----
    corte_mes <- reactive({ PrimerDia(Sys.Date()) })
    
    universo_nits <- reactive({
      dat_t() %>% select(LinNegCod, CliNitPpal) %>% distinct()
    })
    universo_razsoc <- reactive({
      dat_t() %>% select(LinNegCod, PerRazSoc) %>% distinct()
    })
    
    # Snapshot CRMNALSEGR del mes de corte acotado al universo del filtro.
    # FUENTE UNICA DE VERDAD para conteos y poblacion de modulos UC.
    segr_corte <- reactive({
      req(nrow(universo_nits()) > 0)
      segmentos_raw() %>%
        filter(FecProceso == corte_mes()) %>%
        semi_join(universo_nits(), by = c("LinNegCod", "CliNitPpal"))
    })
    
    # NITs por segmento: CRMNALSEGR solo tiene LinNegCod, CliNitPpal, Meses,
    # SegmentoRacafe, FecProceso. Columnas como Segmento, Asesor, Presupuestado,
    # UltFecFact llegan de dat_c() via join en data_base() de cada modulo.
    nits_activas <- reactive({
      segr_corte() %>%
        filter(SegmentoRacafe == "CLIENTE") %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
    })
    nits_recuperar <- reactive({
      segr_corte() %>%
        filter(SegmentoRacafe == "CLIENTE A RECUPERAR") %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
    })
    # NUEVOS: CLIENTE en corte actual sin presencia en el mes anterior
    # NUEVO = facturó en dat_c() (período activo) pero NO existe en CRMNALSEGR
    # en el mes de corte. Si ya aparece en segr_corte(), ya fue clasificado
    # (CLIENTE o CLIENTE A RECUPERAR) y no es nuevo.
    nits_nuevas <- reactive({
      nits_en_segr <- segr_corte() %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
      dat_c() %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct() %>%
        semi_join(universo_nits(), by = c("LinNegCod", "CliNitPpal")) %>%
        anti_join(nits_en_segr,   by = c("LinNegCod", "CliNitPpal"))
    })
    
    # dat_c() filtrado por semi_join con NITs del segmento
    df_activas   <- reactive({ dat_c() %>%
        semi_join(nits_activas(),   by = c("LinNegCod", "CliNitPpal")) })
    df_recuperar <- reactive({ dat_c() %>%
        semi_join(nits_recuperar(), by = c("LinNegCod", "CliNitPpal")) })
    df_nuevas    <- reactive({ dat_c() %>%
        semi_join(nits_nuevas(),    by = c("LinNegCod", "CliNitPpal")) })
    
    # Conteos KPI: mismo universo que el detalle
    val_activas   <- reactive({ n_distinct(nits_activas()$LinNegCod,
                                           nits_activas()$CliNitPpal) })
    val_recuperar <- reactive({ n_distinct(nits_recuperar()$LinNegCod,
                                           nits_recuperar()$CliNitPpal) })
    val_nuevas    <- reactive({ n_distinct(nits_nuevas()$LinNegCod,
                                           nits_nuevas()$CliNitPpal) })
    
    # Actividad Comercial ----
    df_leads <- reactive({
      dat_leads() %>% semi_join(universo_razsoc(), by = c("PerRazSoc", "LinNegCod"))
    })
    df_oportunidades <- reactive({
      dat_oportunidades() %>% semi_join(universo_razsoc(), by = c("PerRazSoc", "LinNegCod"))
    })
    df_competencia <- reactive({ dat_competencia() })
    df_cohorte <- reactive({
      mes_vig <- max(data_cohortes()$FecProceso, na.rm = TRUE)
      data_cohortes() %>%
        semi_join(universo_nits(), by = c("LinNegCod", "CliNitPpal")) %>%
        filter(FecProceso == mes_vig)
    })
    
    val_leads         <- reactive({ n_distinct(df_leads()$PerRazSoc) })
    val_oportunidades <- reactive({
      n_distinct(df_oportunidades()$LinNegCod, df_oportunidades()$PerRazSoc,
                 df_oportunidades()$Categoria, df_oportunidades()$Producto)
    })
    val_competencia <- reactive({ n_distinct(df_competencia()$Competencia) })
    val_cohorte     <- reactive({ nrow(df_cohorte()) })
    
    # KPIs Presupuesto YTD ----
    periodo_r <- reactive({ year(max(dat()$FecFact, na.rm = TRUE)) })
    
    ejec_ytd_r <- reactive({
      mes <- month(Sys.Date())
      dat() %>%
        filter(!is.na(FecFact), year(FecFact) == periodo_r(), month(FecFact) <= mes) %>%
        mutate(KILOS = if_else(LinNegCod == 10000L, SacLote * 62.5, SacLote * 70),
               KilosFact = if_else(is.na(KilosFact), KILOS, KilosFact),
               Margen    = if_else(is.infinite(Margen), NA_real_, Margen)) %>%
        summarise(sacos  = sum(KilosFact / 70, na.rm = TRUE),
                  margen = sum(Margen,          na.rm = TRUE))
    })
    ppto_ytd_r <- reactive({
      mes <- month(Sys.Date())
      clientes_raw() %>%
        filter(year(FecProceso) == year(Sys.Date())) %>%
        group_by(LinNegCod, CliNitPpal) %>%
        filter(FecProceso == max(FecProceso)) %>%
        slice(1L) %>% ungroup() %>%
        inner_join(dat_t() %>% select(LinNegCod, Segmento, Asesor) %>% distinct(),
                   by = c("LinNegCod", "Segmento", "Asesor")) %>%
        summarise(ppto_sacos  = sum(SSPpto,    na.rm = TRUE) / 12 * mes,
                  ppto_margen = sum(MNFCCPpto, na.rm = TRUE) / 12 * mes)
    })
    kpi_cumpl_sacos_r <- reactive({
      e <- ejec_ytd_r(); p <- ppto_ytd_r()
      list(cumpl = SiError_0(e$sacos / p$ppto_sacos), ejec = e$sacos, ppto = p$ppto_sacos,
           periodo = periodo_r(), mes = month.name[month(Sys.Date())])
    })
    kpi_cumpl_margen_r <- reactive({
      e <- ejec_ytd_r(); p <- ppto_ytd_r()
      list(cumpl = SiError_0(e$margen / p$ppto_margen), ejec = e$margen, ppto = p$ppto_margen,
           periodo = periodo_r(), mes = month.name[month(Sys.Date())])
    })
    
    # Contexto de clientes: perfil y presupuesto desde clientes_raw() + PerRazSoc de dat_t().
    # clientes_raw() es CRMNALCLIENTE — contiene Asesor, Segmento, SSPpto, MNFCCPpto.
    # dat_t() aporta PerRazSoc (razón social) que CRMNALCLIENTE no tiene directamente.
    # Se usa el snapshot más reciente por cliente/linea para el año vigente.
    ctx_clientes <- reactive({
      snap <- clientes_raw() %>%
        filter(year(FecProceso) == year(Sys.Date())) %>%
        group_by(LinNegCod, CliNitPpal) %>%
        filter(FecProceso == max(FecProceso)) %>%
        slice(1L) %>%
        ungroup() %>%
        mutate(
          Presupuestado = ifelse(SSPpto > 0, "PRESUPUESTADO", "NO PRESUPUESTADO"),
          PptoSacos     = SSPpto,
          PptoMargen    = MNFCCPpto
        ) %>%
        select(LinNegCod, CliNitPpal, Segmento, Asesor,
               Presupuestado, PptoSacos, PptoMargen)
      razsoc <- dat_t() %>%
        select(LinNegCod, CliNitPpal, PerRazSoc) %>%
        distinct()
      snap %>% left_join(razsoc, by = c("LinNegCod", "CliNitPpal"))
    })
    
    # Registro modulos hijos ----
    # Modulos UC reciben:
    #   dat   = dat_c() filtrado por NITs del segmento (ejecucion con filtro de fechas)
    #   nits  = NITs del segmento desde CRMNALSEGR
    #   ctx   = contexto completo sin filtro de fechas desde dat_t()
    DetalleCliente(
      "detalle_activas",
      dat = df_activas, nits = nits_activas, ctx = ctx_clientes,
      usr = usr, trigger_update = trigger_update
    )
    DetalleClienteRecuperar(
      "detalle_recuperar",
      dat = df_recuperar, nits = nits_recuperar, ctx = ctx_clientes,
      usr = usr, trigger_update = trigger_update
    )
    DetalleClienteNuevo(
      "detalle_nuevas",
      dat = df_nuevas, nits = nits_nuevas, ctx = ctx_clientes,
      usr = usr, trigger_update = trigger_update
    )
    DashboardLeads("detalle_leads",                  df_leads,         usr)
    DashboardOportunidades("detalle_oportunidades",  df_oportunidades, usr)
    Cohortes("detalle_cohortes",                     data_cohortes,    dat_t)
    DetalleCompetencia("detalle_competencia")
    Presupuesto("detalle_presupuesto",               dat_t,            clientes_raw)
    
    # Outputs Presupuesto ----
    CajaModal(
      "kpi_cumpl_sacos",
      valor   = reactive(kpi_cumpl_sacos_r()$cumpl),
      formato = "porcentaje",
      texto   = reactive(paste0("Cumplimiento Sacos \u2014 ",
                                kpi_cumpl_sacos_r()$mes, " ", kpi_cumpl_sacos_r()$periodo)),
      icono           = "check-double",
      colores         = reactive(c(fondo = "white")),
      color_fondo_hex = reactive(.fondo_cumpl(kpi_cumpl_sacos_r()$cumpl)),
      mostrar_boton   = TRUE,
      titulo_modal    = "Detalle \u2014 Seguimiento de Presupuesto",
      icono_modal     = "chart-line",
      contenido_modal = function() PresupuestoUI(ns("detalle_presupuesto")),
      footer = reactive({
        paste0(FormatearNumero(kpi_cumpl_sacos_r()$ejec,  "coma"), " sacos facturados de ",
               FormatearNumero(kpi_cumpl_sacos_r()$ppto,  "coma"),
               " presupuestados acumulados al mes.") %>% HTML
      }),
      footer_class = "caja-modal-footer"
    )
    CajaModal(
      "kpi_cumpl_margen",
      valor   = reactive(kpi_cumpl_margen_r()$cumpl),
      formato = "porcentaje",
      texto   = reactive(paste0("Cumplimiento Margen \u2014 ",
                                kpi_cumpl_margen_r()$mes, " ", kpi_cumpl_margen_r()$periodo)),
      icono           = "dollar-sign",
      colores         = reactive(c(fondo = "white")),
      color_fondo_hex = reactive(.fondo_cumpl(kpi_cumpl_margen_r()$cumpl)),
      mostrar_boton   = TRUE,
      titulo_modal    = "Detalle \u2014 Seguimiento de Presupuesto",
      icono_modal     = "chart-line",
      contenido_modal = function() PresupuestoUI(ns("detalle_presupuesto")),
      footer = reactive({
        paste0(FormatearNumero(kpi_cumpl_margen_r()$ejec, "dinero"), " facturado de ",
               FormatearNumero(kpi_cumpl_margen_r()$ppto, "dinero"),
               " presupuestado acumulado al mes.") %>% HTML
      }),
      footer_class = "caja-modal-footer"
    )
    
    # Outputs UC ----
    .caja_uc("kpi_activas",   val_activas,   "Clientes Activos",   "users",
             "Detalle \u2014 Clientes Activos",
             function() DetalleClienteUI(ns("detalle_activas")),
             reactive(paste0("UC clasificadas como CLIENTE en CRMNALSEGR al ",
                             format(corte_mes(), "%d/%m/%Y"), ".")))
    .caja_uc("kpi_recuperar", val_recuperar, "Clientes a Recuperar", "user-clock",
             "Detalle \u2014 Clientes a Recuperar",
             function() DetalleClienteRecuperarUI(ns("detalle_recuperar")),
             reactive(paste0("UC clasificadas como CLIENTE A RECUPERAR en CRMNALSEGR al ",
                             format(corte_mes(), "%d/%m/%Y"), ".")))
    .caja_uc("kpi_nuevas",    val_nuevas,    "Clientes Nuevos",    "user-plus",
             "Detalle \u2014 Clientes Nuevos",
             function() DetalleClienteNuevoUI(ns("detalle_nuevas")),
             reactive(paste0("UC con facturación en el período activo ",
                             "que no aparecen en CRMNALSEGR al ",
                             format(corte_mes(), "%d/%m/%Y"), ".")))
    
    # Outputs Actividad Comercial ----
    .caja_std("kpi_leads", val_leads, "Leads Creados", "address-card",
              reactive("Prospectos registrados en el embudo comercial para el periodo activo."),
              function() DashboardLeadsUI(ns("detalle_leads")),
              "Detalle \u2014 Leads")
    .caja_std("kpi_oportunidades", val_oportunidades, "Oportunidades Creadas", "handshake",
              reactive(paste0("Combinaciones \u00fanicas de cliente, l\u00ednea y producto ",
                              "con oportunidad de venta registrada en el periodo.")),
              function() DashboardOportunidadesUI(ns("detalle_oportunidades")),
              "Detalle \u2014 Oportunidades")
    .caja_std("kpi_cohorte", val_cohorte, "Clientes en Cohorte", "arrows-to-eye",
              reactive(paste0("Poblaci\u00f3n base al ",
                              format(min(df_cohorte()$FecProceso), "%d/%m/%Y"),
                              " m\u00e1s altas del periodo. Total mes ",
                              format(max(df_cohorte()$FecProceso), "%B %Y"), ".")),
              function() CohortesUI(ns("detalle_cohortes")),
              "Detalle \u2014 Cohorte")
    .caja_std("kpi_competencia", val_competencia, "Competidor(es) Registrado(s)", "building-flag",
              reactive("Marcas competidoras con registros activos para el universo del filtro."),
              function() DetalleCompetenciaUI(ns("detalle_competencia")),
              "Detalle \u2014 Competencia")
  })
}
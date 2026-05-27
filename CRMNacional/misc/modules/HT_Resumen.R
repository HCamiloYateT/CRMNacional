ResumenTotalUI <- function(id) {
  ns <- NS(id)
  
  # Helper: titulo de seccion con separador visual
  .seccion <- function(titulo, icono) {
    tags$div(
      style = paste0(
        "display:flex; align-items:center; gap:8px; ",
        "margin:18px 0 6px 0; padding-bottom:6px; ",
        "border-bottom:2px solid #E2E8F0;"
      ),
      tags$span(
        style = "color:#64748B; font-size:13px;",
        icon(icono)
      ),
      tags$span(
        titulo,
        style = paste0(
          "font-size:13px; font-weight:700; ",
          "color:#374151; letter-spacing:0.03em; text-transform:uppercase;"
        )
      )
    )
  }
  
  tagList(
    # Seccion presupuesto
    .seccion("Presupuesto", "file-invoice-dollar"),
    fluidRow(
      column(3, CajaModalUI(ns("kpi_cumpl_sacos"))),
      column(3, CajaModalUI(ns("kpi_cumpl_margen")))
    ),
    # Seccion unidades comerciales
    .seccion("Unidades Comerciales", "users"),
    fluidRow(
      column(4, CajaModalUI(ns("kpi_activas"))),
      column(4, CajaModalUI(ns("kpi_recuperar"))),
      column(4, CajaModalUI(ns("kpi_nuevas")))
    ),
    # Seccion actividad comercial
    .seccion("Actividad Comercial", "bullseye"),
    fluidRow(
      column(3, CajaModalUI(ns("kpi_leads"))),
      column(3, CajaModalUI(ns("kpi_oportunidades"))),
      column(3, CajaModalUI(ns("kpi_cohorte"))),
      column(3, CajaModalUI(ns("kpi_competencia")))
    )
  )
}
ResumenTotal <- function(id, dat, dat_t, dat_c, dat_leads, dat_oportunidades, dat_competencia, clientes_raw, fec,
                         usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helpers de fecha ----
    .mes_es <- function(fecha) {
      meses <- c(
        "enero", "febrero", "marzo", "abril", "mayo", "junio",
        "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
      )
      paste(meses[month(fecha)], year(fecha))
    }
    .mes_info <- function(fecha_proceso) { .mes_es(fecha_proceso - months(1)) }
    
    # Helper semaforo de fondo para KPIs de porcentaje
    .fondo_cumpl <- function(v) {
      dplyr::case_when(
        is.na(v)    ~ "white",
        v >= 1.00   ~ "#EFF6FF",
        v >= 0.80   ~ "#F0FDF4",
        v >= 0.50   ~ "#FFFBEB",
        TRUE        ~ "#FEF2F2"
      )
    }
    
    # Reactivos base ----
    
    # Referencia de corte: 1ro del mes en curso
    corte_mes <- reactive({ PrimerDia(Sys.Date()) })
    
    # Pares LinNegCod/CliNitPpal presentes en dat() — universo filtrado activo
    nits_dat <- reactive({
      dat() %>% select(LinNegCod, CliNitPpal) %>% distinct()
    })
    
    # Carga unica de CRMNALSEGR — base para snapshot del mes e historial de nuevos
    segr_completo <- reactive({
      t1 <- CargarDatos("CRMNALSEGR") %>%
        mutate(FecProceso = as.Date(FecProceso))
    })
    
    # Se derivan de dat_t() que ya aplica esos tres filtros sin restriccion de fecha ni NIT
    filtros_dim <- reactive({
      dat_t() %>%
        select(LinNegCod, Asesor, Segmento) %>%
        distinct()
    })
    
    # Snapshot CRMNALSEGR al 1ro del mes, filtrado por dimensiones (no por NIT)
    segr_corte <- reactive({
      d <- filtros_dim()
      segr_completo() %>%
        filter(FecProceso == corte_mes()) %>%
        left_join(
          dat_c() %>% select(LinNegCod, CliNitPpal, Asesor, Segmento) %>% distinct(),
          by = join_by(LinNegCod, CliNitPpal)
        ) %>%
        filter(
          LinNegCod %in% d$LinNegCod,
          Asesor    %in% d$Asesor,
          Segmento  %in% d$Segmento
        )
    })
    
    # NITs con presencia en CRMNALSEGR antes del mes vigente — para deteccion de nuevos
    nits_segr_antes_mes <- reactive({
      segr_completo() %>%
        filter(FecProceso == corte_mes()) %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
    })
    
    # NITs con al menos una factura antes del mes vigente — para deteccion de nuevos
    nits_fact_antes_mes <- reactive({
      FACT %>%
        filter(as.Date(UltFecFact) < corte_mes()) %>%
        select(CliNitPpal = FctNit) %>%
        distinct()
    })
    
    # UC de dat_f() con transacciones en lo corrido del mes en curso
    nits_mes_corrido <- reactive({
      dat() %>%
        filter(PrimerDia(FecFact) == corte_mes()) %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
    })
    # Reactivos de UC — dataframes base compartidos entre conteo y drill-down ----
    
    # Activas: dat filtrado a NITs clasificados como CLIENTE en el snapshot del corte
    df_activas <- reactive({
      nits <- segr_corte() %>%
        filter(SegmentoRacafe == "CLIENTE") %>%
        select(LinNegCod, CliNitPpal)
      dat_c() %>% semi_join(nits, by = c("LinNegCod", "CliNitPpal"))
    })
    
    # A recuperar: dat filtrado a NITs clasificados como CLIENTE A RECUPERAR en el snapshot
    df_recuperar <- reactive({
      nits <- segr_corte() %>%
        filter(SegmentoRacafe == "CLIENTE A RECUPERAR") %>%
        select(LinNegCod, CliNitPpal)
      dat_c() %>% semi_join(nits, by = c("LinNegCod", "CliNitPpal"))
    })
    
    # Nuevas: dat filtrado a NITs del mes corrido sin presencia previa en segr ni en FACT
    df_nuevas <- reactive({
      nits <- nits_mes_corrido() %>%
        anti_join(nits_segr_antes_mes(), by = c("LinNegCod", "CliNitPpal")) %>%
        anti_join(nits_fact_antes_mes(), by = "CliNitPpal")
      dat_c() %>% semi_join(nits, by = c("LinNegCod", "CliNitPpal"))
    })
    
    # Conteos derivados directamente de los dataframes — unica fuente de verdad
    val_activas  <- reactive({ n_distinct(df_activas()$LinNegCod,  df_activas()$CliNitPpal) })
    val_recuperar <- reactive({ n_distinct(df_recuperar()$LinNegCod, df_recuperar()$CliNitPpal) })
    val_nuevas   <- reactive({ n_distinct(df_nuevas()$LinNegCod,   df_nuevas()$CliNitPpal) })
    
    # Reactivos de conteo comercial ----
    val_leads <- reactive({
      n_distinct(dat_leads()$PerRazSoc)
    })
    val_oportunidades <- reactive({
      n_distinct(
        dat_oportunidades()$LinNegCod, dat_oportunidades()$CliNitPpal,
        dat_oportunidades()$Categoria, dat_oportunidades()$Producto
      )
    })
    val_competencia <- reactive({
      n_distinct(dat_competencia()$Competencia)
    })
    val_cohorte <- reactive({
      n_distinct(segr_corte()$CliNitPpal, segr_corte()$LinNegCod)
    })
    
    # Reactivos KPI presupuesto — derivados de datos_grafico via Presupuesto internals ----
    # Se calculan directamente desde dat() para no duplicar logica del modulo Presupuesto.
    # Comparten la misma ETL de obtener_presupuesto / procesar_datos_base.
    
    .normalizar_kilos_rt <- function(df) {
      df %>% mutate(
        Margen    = ifelse(is.infinite(Margen), NA, Margen),
        KILOS     = ifelse(LinNegCod == 10000, SacLote * 62.5, SacLote * 70),
        KilosFact = ifelse(is.na(KilosFact), KILOS, KilosFact)
      )
    }
    
    periodo_r <- reactive({ year(max(dat()$FecFact, na.rm = TRUE)) })
    
    # Ejecucion YTD desde dat() — sacos y margen acumulados hasta el mes actual
    ejec_ytd_r <- reactive({
      mes <- month(Sys.Date())
      dat() %>%
        filter(!is.na(FecFact), year(FecFact) == periodo_r(), month(FecFact) <= mes) %>%
        .normalizar_kilos_rt() %>%
        summarise(
          sacos  = sum(KilosFact / 70, na.rm = TRUE),
          margen = sum(Margen,          na.rm = TRUE)
        )
    })
    
    # Presupuesto YTD: ultimo proceso del anio por cliente/linea cruzado con dat_t(), acumulado al mes
    # dat_t() = data_t sin filtro de fecha — universo dimensional correcto para presupuesto
    ppto_ytd_r <- reactive({
      mes <- month(Sys.Date())
      CargarDatos("CRMNALCLIENTE") %>%
        filter(year(FecProceso) == year(Sys.Date())) %>%
        group_by(LinNegCod, CliNitPpal) %>%
        filter(FecProceso == max(FecProceso)) %>%
        ungroup() %>%
        inner_join(dat_t() %>% select(LinNegCod, Segmento, Asesor) %>% distinct(),
                   by = join_by(LinNegCod, Segmento, Asesor)) %>%
        summarise(ppto_sacos  = sum(SSPpto,    na.rm = TRUE) / 12 * mes,
                  ppto_margen = sum(MNFCCPpto, na.rm = TRUE) / 12 * mes
                  )
    })
    
    # Cumplimiento acumulado hasta el mes en curso
    kpi_cumpl_sacos_r <- reactive({
      e <- ejec_ytd_r(); p <- ppto_ytd_r()
      list(
        cumpl  = SiError_0(e$sacos  / p$ppto_sacos),
        ejec   = e$sacos,
        ppto   = p$ppto_sacos,
        periodo = periodo_r(),
        mes    = month.name[month(Sys.Date())]
      )
    })
    kpi_cumpl_margen_r <- reactive({
      e <- ejec_ytd_r(); p <- ppto_ytd_r()
      list(
        cumpl  = SiError_0(e$margen / p$ppto_margen),
        ejec   = e$margen,
        ppto   = p$ppto_margen,
        periodo = periodo_r(),
        mes    = month.name[month(Sys.Date())]
      )
    })
    
    # Registro de modulos de detalle con dataframes pre-filtrados ----
    DetalleCliente         (id = "detalle_activas",   dat = df_activas,   usr, trigger_update)
    DetalleClienteRecuperar(id = "detalle_recuperar", dat = df_recuperar, usr, trigger_update)
    DetalleClienteNuevo    (id = "detalle_nuevas",    dat = df_nuevas,    usr, trigger_update)
    DashboardLeads         (id = "detalle_leads", dat_leads)
    DashboardOportunidades (id = "detalle_oportunidades",  dat_oportunidades, usr)
    Cohortes               (id = "detalle_cohortes", data_tx = dat_t)
    DetalleCompetencia     (id = "detalle_competencia")
    Presupuesto            (id = "detalle_presupuesto", dat_t, clientes_raw)
    
    # Cajas KPI presupuesto ----
    CajaModal("kpi_cumpl_sacos",
              valor   = reactive(kpi_cumpl_sacos_r()$cumpl),
              formato = "porcentaje",
              texto   = reactive(paste0(
                "Cumplimiento Sacos \u2014 ", kpi_cumpl_sacos_r()$mes, " ", kpi_cumpl_sacos_r()$periodo
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
              valor   = reactive(kpi_cumpl_margen_r()$cumpl),
              formato = "porcentaje",
              texto   = reactive(paste0(
                "Cumplimiento Margen \u2014 ", kpi_cumpl_margen_r()$mes, " ", kpi_cumpl_margen_r()$periodo
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
    
    # Cajas KPI UC ----
    
    CajaModal("kpi_activas",
              valor           = reactive(html_valor(val_activas(), formato = "entero")),
              formato         = "entero",
              texto           = "Clientes Activos",
              icono           = "users",
              colores         = c(fondo = "white"),
              mostrar_boton   = TRUE,
              titulo_modal    = "Detalle \u2014 Clientes Activos",
              icono_modal     = "users",
              contenido_modal = function() DetalleClienteUI(ns("detalle_activas")),
              footer = reactive(paste0(
                "UC clasificadas como CLIENTE en la segmentaci\u00f3n del ",
                format(corte_mes(), "%d/%m/%Y"), "."
              )),
              footer_class = "caja-modal-footer"
    )
    
    CajaModal("kpi_recuperar",
              valor           = reactive(html_valor(val_recuperar(), formato = "entero")),
              formato         = "entero",
              texto           = "Clientes a Recuperar",
              icono           = "user-clock",
              colores         = c(fondo = "white"),
              mostrar_boton   = TRUE,
              titulo_modal    = "Detalle \u2014 Clientes a Recuperar",
              icono_modal     = "user-clock",
              contenido_modal = function() DetalleClienteRecuperarUI(ns("detalle_recuperar")),
              footer = reactive(paste0(
                "UC clasificadas como CLIENTE A RECUPERAR en la segmentaci\u00f3n del ",
                format(corte_mes(), "%d/%m/%Y"), "."
              )),
              footer_class = "caja-modal-footer"
    )
    
    CajaModal("kpi_nuevas",
              valor           = reactive(html_valor(val_nuevas(), formato = "entero")),
              formato         = "entero",
              texto           = "Clientes Nuevos",
              icono           = "user-plus",
              colores         = c(fondo = "white"),
              mostrar_boton   = TRUE,
              titulo_modal    = "Detalle \u2014 Clientes Nuevos",
              icono_modal     = "user-plus",
              contenido_modal = function() DetalleClienteNuevoUI(ns("detalle_nuevas")),
              footer = reactive(paste0(
                "UC con factura en ", .mes_es(Sys.Date()),
                " sin registro previo en CRMNALSEGR ni en el historial de facturaci\u00f3n."
              )),
              footer_class = "caja-modal-footer"
    )
    # Cajas KPI comercial ----
    CajaModal("kpi_leads",
              valor         = reactive(html_valor(val_leads(), formato = "entero")),
              formato       = "entero",
              texto         = "Leads Creados",
              icono         = "address-card",
              colores       = c(fondo = "white"),
              mostrar_boton = TRUE,
              titulo_modal  = "Detalle \u2014 Leads",
              icono_modal   = "address-card",
              contenido_modal = function() DashboardLeadsUI(ns("detalle_leads")),
              footer = reactive(paste0(
                "Prospectos registrados en el embudo comercial ",
                "para el periodo activo del filtro."
              )),
              footer_class = "caja-modal-footer"
    )
    
    CajaModal("kpi_oportunidades",
              valor         = reactive(html_valor(val_oportunidades(), formato = "entero")),
              formato       = "entero",
              texto         = "Oportunidades Creadas",
              icono         = "handshake",
              colores       = c(fondo = "white"),
              mostrar_boton = TRUE,
              titulo_modal  = "Detalle \u2014 Oportunidades",
              icono_modal   = "handshake",
              contenido_modal = function() DashboardOportunidadesUI(ns("detalle_oportunidades")),
              footer = reactive(paste0(
                "Combinaciones \u00fanicas de cliente, l\u00ednea y producto con oportunidad ",
                "de venta registrada en el periodo."
              )),
              footer_class = "caja-modal-footer"
    )
    
    CajaModal("kpi_cohorte",
              valor         = reactive(html_valor(val_cohorte(), formato = "entero")),
              formato       = "entero",
              texto         = "Clientes en Cohorte",
              icono         = "arrows-to-eye",
              colores       = c(fondo = "white"),
              mostrar_boton = TRUE,
              titulo_modal  = "Detalle \u2014 Cohorte",
              icono_modal   = "arrows-to-eye",
              contenido_modal = function() CohortesUI(ns("detalle_cohortes")),
              footer = reactive(paste0(
                "UC presentes en la segmentaci\u00f3n Racafe al ",
                format(corte_mes(), "%d/%m/%Y"), "."
              )),
              footer_class = "caja-modal-footer"
    )
    
    CajaModal("kpi_competencia",
              valor         = reactive(html_valor(val_competencia(), formato = "entero")),
              formato       = "entero",
              texto         = "Competidor(es) Registrado(s)",
              icono         = "building-flag",
              colores       = c(fondo = "white"),
              mostrar_boton = TRUE,
              titulo_modal  = "Detalle \u2014 Competencia",
              icono_modal   = "building-flag",
              contenido_modal = function() DetalleCompetenciaUI(ns("detalle_competencia")),
              footer = reactive(paste0(
                "Marcas competidoras con registros activos para el universo ",
                "de clientes del filtro."
              )),
              footer_class = "caja-modal-footer"
    )
    
  })
}
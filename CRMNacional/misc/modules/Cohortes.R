TablaDetalleClientesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("sin_datos")),
    racafeModulos::TablaReactable2UI(ns("tbl")),
    shiny::uiOutput(ns("ui_descarga"))
    )
}
TablaDetalleClientes <- function(id, data, mes_vig, titulo, subtitulo = NULL, 
                                 footer = NULL, footer_tipo = "info") {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Formatos compartidos — idénticos a los del módulo Cohortes
    .fmt_sacos  <- racafe::DefinirFormato("coma")
    .fmt_margen <- racafe::DefinirFormato("dinero")
    .fmt_pct    <- racafe::DefinirFormato("porcentaje")
    
    # Semáforo de cumplimiento: >= 100% verde, >= 85% naranja, rojo
    .style_cumpl <- function(v) {
      if (is.null(v) || is.na(v) || is.infinite(v)) return(NULL)
      list(
        color = if (v >= 1) "#1E8449" else if (v >= 0.85) "#D4780A" else "#C0392B",
        fontWeight = "600"
      )
    }
    
    # Celda de cumplimiento: porcentaje con 2 decimales o guión si ausente
    .cell_cumpl <- function(v) {
      if (is.null(v) || is.na(v) || is.infinite(v)) return("\u2014")
      .fmt_pct(v)
    }
    
    # Definición centralizada de columnas — fuente única de verdad
    .coldefs <- list(LinNegCod = reactable::colDef(name = "Lin. Negocio", sticky = "left"),
                     CliNitPpal = reactable::colDef(name = "NIT Ppal", sticky = "left"),
                     PerRazSoc = reactable::colDef(name = "Cliente", minWidth = 250, sticky = "left"),
                     Segmento = reactable::colDef(name = "Segmento", minWidth = 100),
                     Asesor = reactable::colDef(name = "Asesor", minWidth = 120),
                     Presupuestado = reactable::colDef(name = "Presupuesto", minWidth = 130),
                     UltFecFact = reactable::colDef(name = "\u00dalt. Factura", minWidth = 110,
                                                    cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d/%m/%Y")),
                     # Mes vigente — sacos
                     Ej_SacosVig = reactable::colDef(name = "Sacos Mes", minWidth = 100,
                                                     cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                     Pt_SacosVig = reactable::colDef(name = "Ppto Sacos Mes", minWidth = 110,
                                                     cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                     Cump_SacosVig = reactable::colDef(name = "% Cumpl Sacos Mes", minWidth = 130,
                                                       cell = function(v) .cell_cumpl(v),
                                                       style = function(v) .style_cumpl(v)),
                     # Mes vigente — margen
                     Ej_MargenVig = reactable::colDef(name = "Margen Mes", minWidth = 130,
                                                      cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                     Pt_MargenVig = reactable::colDef(name = "Ppto Margen Mes", minWidth = 140,
                                                      cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                     Cump_MargenVig = reactable::colDef(name = "% Cumpl Margen Mes", minWidth = 140,
                                                        cell = function(v) .cell_cumpl(v),
                                                        style = function(v) .style_cumpl(v)),
                     # YTD — sacos
                     Ej_SacosYTD = reactable::colDef(name = "Sacos Año Corrido", minWidth = 100,
                                                     cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                     Pt_SacosYTD = reactable::colDef(name = "Ppto Sacos Año Corrido", minWidth = 110,
                                                     cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                     Cump_SacosYTD = reactable::colDef(name = "% Cumpl Sacos Año Corrido", minWidth = 130,
                                                       cell = function(v) .cell_cumpl(v),
                                                       style = function(v) .style_cumpl(v)),
                     # YTD — margen
                     Ej_MargenYTD = reactable::colDef(name = "Margen Año Corrido", minWidth = 130,
                                                      cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                     Pt_MargenYTD = reactable::colDef(name = "Ppto Margen Año Corrido", minWidth = 140,
                                                      cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                     Cump_MargenYTD = reactable::colDef(name = "% Cumpl Margen Año Corrido", minWidth = 150,
                                                        cell = function(v) .cell_cumpl(v),
                                                        style = function(v) .style_cumpl(v))
      
    )
    
    # Transformación interna: agrega métricas mes vigente + YTD
    .preparar_data <- function(dat, mes_v) {
      dat %>%
        dplyr::group_by(LinNegCod, CliNitPpal, PerRazSoc, Asesor, Segmento,
                        Presupuestado, UltFecFact) %>%
        dplyr::summarise(
          Ej_SacosVig = sum(ifelse(FecProceso == mes_v, Sacos, 0), na.rm = TRUE),
          Pt_SacosVig = sum(ifelse(FecProceso == mes_v, PPtoSacos, 0), na.rm = TRUE),
          Ej_MargenVig = sum(ifelse(FecProceso == mes_v, Margen, 0), na.rm = TRUE),
          Pt_MargenVig = sum(ifelse(FecProceso == mes_v, PPtoMargen, 0), na.rm = TRUE),
          Ej_SacosYTD = sum(Sacos, na.rm = TRUE),
          Pt_SacosYTD = sum(PPtoSacos, na.rm = TRUE),
          Ej_MargenYTD = sum(Margen, na.rm = TRUE),
          Pt_MargenYTD = sum(PPtoMargen, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          Cump_SacosVig = dplyr::if_else(Pt_SacosVig  > 0, Ej_SacosVig  / Pt_SacosVig, NA_real_),
          Cump_MargenVig = dplyr::if_else(Pt_MargenVig > 0, Ej_MargenVig / Pt_MargenVig, NA_real_),
          Cump_SacosYTD = dplyr::if_else(Pt_SacosYTD  > 0, Ej_SacosYTD  / Pt_SacosYTD, NA_real_),
          Cump_MargenYTD = dplyr::if_else(Pt_MargenYTD > 0, Ej_MargenYTD / Pt_MargenYTD, NA_real_)
        ) %>%
        dplyr::select(
          LinNegCod, CliNitPpal, PerRazSoc, Segmento, Asesor,
          Presupuestado, UltFecFact,
          Ej_SacosVig, Pt_SacosVig, Cump_SacosVig,
          Ej_MargenVig, Pt_MargenVig, Cump_MargenVig,
          Ej_SacosYTD, Pt_SacosYTD, Cump_SacosYTD,
          Ej_MargenYTD, Pt_MargenYTD, Cump_MargenYTD
        )
    }
    
    # Reactive de datos transformados — se recalcula solo si data o mes_vig cambian
    data_prep <- shiny::reactive({
      shiny::req(data(), mes_vig())
      dat <- data()
      if (nrow(dat) == 0) return(NULL)
      .preparar_data(dat, mes_vig())
    })
    output$sin_datos <- shiny::renderUI({
      shiny::req(is.null(data_prep()))
      shiny::tags$div(
        style = paste0(
          "display:flex; align-items:center; gap:8px;",
          "padding:16px 12px; color:#6B7280;",
          "font-size:0.85rem; font-style:italic;"
        ),
        shiny::icon("circle-info"),
        "No hay información disponible."
      )
    })
    
    # Tabla — solo se registra cuando hay datos
    shiny::observe({
      shiny::req(!is.null(data_prep()))
      racafeModulos::TablaReactable2(
        id = "tbl",
        data = data_prep,
        titulo = titulo,
        subtitulo = subtitulo,
        footer = footer,
        footer_tipo = footer_tipo,
        col_specs = .coldefs,
        id_col = "CliNitPpal",
        modo_seleccion = "ninguno",
        searchable = TRUE,
        page_size = 20L
      )
    })
    
    # UI del botón — solo visible cuando hay datos
    output$ui_descarga <- shiny::renderUI({
      shiny::req(!is.null(data_prep()))
      shiny::tags$div(
        style = "display:flex; justify-content:flex-end; margin-bottom:6px;",
        shiny::downloadButton(
          outputId = session$ns("btn_descarga"),
          label = "Descargar",
          icon = shiny::icon("download"),
          class = "btn btn-sm btn-outline-secondary"
        )
      )
    })
    
    # Handler de descarga — exporta data_prep como CSV con separador ;
    output$btn_descarga <- shiny::downloadHandler(
      filename = function() {
        tit <- if (shiny::is.reactive(titulo)) titulo() else titulo
        tit_limpio <- gsub("[^A-Za-z0-9_\\-]", "_", tit)
        paste0(tit_limpio, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(data_prep(), file)
      }
    )
    
  })
}

CohortesUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Bloque KPIs de periodo: inicio vs mes vigente ----
    bs4Dash::bs4Card(title = "Resumen de Periodo", width = 12, solidHeader = TRUE, 
                     status = "white", collapsible = TRUE,
                     # Fila — Mes Inicial
                     uiOutput(ns("lbl_mes_inicial")),
                     fluidRow(column(3, racafeModulos::CajaModalUI(ns("kpi_ini_total"))),
                              column(3, racafeModulos::CajaModalUI(ns("kpi_ini_activo"))),
                              column(3, racafeModulos::CajaModalUI(ns("kpi_ini_inactivo"))),
                              column(3, racafeModulos::CajaModalUI(ns("kpi_ini_nuevo")))
                              ),
                     tags$hr(style = "margin: 8px 0;"),
                     # Fila — Mes Vigente
                     uiOutput(ns("lbl_mes_vigente")),
                     fluidRow(column(3, racafeModulos::CajaModalUI(ns("kpi_vig_total"))),
                              column(3, racafeModulos::CajaModalUI(ns("kpi_vig_activo"))),
                              column(3, racafeModulos::CajaModalUI(ns("kpi_vig_inactivo"))),
                              column(3, racafeModulos::CajaModalUI(ns("kpi_vig_nuevo")))
                              ),
                     tags$hr(style = "margin:10px 0;"),
                     tags$p(tags$strong("Alertas del Mes Vigente"),
                            style = "font-weight:700; color:#374151; margin-bottom:4px;"),
                     fluidRow(
                       column(6, racafeModulos::CajaModalUI(ns("kpi_vig_en_riesgo"))),
                       column(6, racafeModulos::CajaModalUI(ns("kpi_vig_por_recuperar")))
                       )
                     ),
    # Bloque Indicadores de Transición ----
    bs4Dash::bs4Card(
      title = "Indicadores de Transici\u00f3n", width = 12,
      solidHeader = TRUE, status = "white", collapsible = TRUE,
      fluidRow(
        # Columna izquierda — matrices de transición
        column(4,
               racafeModulos::TablaReactable2UI(ns("tbl_matriz_trans_abs")),
               shiny::selectInput(
                 inputId = ns("sel_tipo_matriz"),
                 label = NULL,
                 choices = c("% fila (inicio)" = "pct_fila",
                              "% columna (vigente)" = "pct_col",
                              "% total general" = "pct_total"),
                 selected = "pct_fila",
                 width = "100%"
               ),
               racafeModulos::TablaReactable2UI(ns("tbl_matriz_trans"))),
        # Columna derecha — KPIs por grupo
        column(8,
               # Grupo 1 — tasas de transición
               tags$p(tags$strong("Tasas de Transici\u00f3n"),
                      style = "margin:0 0 4px 0; color:#6B7280; font-size:0.8rem;
                      text-transform:uppercase; letter-spacing:0.04em;"),
               fluidRow(
                 column(4, racafeModulos::CajaModalUI(ns("kpi_retencion"))),
                 column(4, racafeModulos::CajaModalUI(ns("kpi_churn"))),
                 column(4, racafeModulos::CajaModalUI(ns("kpi_reactivacion")))
               ),
               tags$hr(style = "margin:10px 0;"),
               # Grupo 2 — volumen y movimiento
               tags$p(tags$strong("Volumen y Movimiento"),
                      style = "margin:0 0 4px 0; color:#6B7280; font-size:0.8rem;
                      text-transform:uppercase; letter-spacing:0.04em;"),
               fluidRow(
                 column(3, racafeModulos::CajaModalUI(ns("kpi_activos_ini"))),
                 column(3, racafeModulos::CajaModalUI(ns("kpi_activos_fin"))),
                 column(3, racafeModulos::CajaModalUI(ns("kpi_cambio_neto"))),
                 column(3, racafeModulos::CajaModalUI(ns("kpi_estabilidad")))
               ),
               tags$hr(style = "margin:10px 0;"),
               # Grupo 3 — duración y dinámica
               tags$p(tags$strong("Duraci\u00f3n y Din\u00e1mica"),
                      style = "margin:0 0 4px 0; color:#6B7280; font-size:0.8rem;
                      text-transform:uppercase; letter-spacing:0.04em;"),
               fluidRow(
                 column(4, racafeModulos::CajaModalUI(ns("kpi_vida_activo"))),
                 column(4, racafeModulos::CajaModalUI(ns("kpi_t_reactiv"))),
                 column(4, racafeModulos::CajaModalUI(ns("kpi_balance_neto")))
               ),
               tags$hr(style = "margin:10px 0;"),
               # Grupo 4 — largo plazo y valor en riesgo
               tags$p(tags$strong("Largo Plazo y Valor en Riesgo"),
                      style = "margin:0 0 4px 0; color:#6B7280; font-size:0.8rem;
                      text-transform:uppercase; letter-spacing:0.04em;"),
               fluidRow(
                 column(3, racafeModulos::CajaModalUI(ns("kpi_pi_activo"))),
                 column(3, racafeModulos::CajaModalUI(ns("kpi_pi_inactivo"))),
                 column(3, racafeModulos::CajaModalUI(ns("kpi_sacos_riesgo"))),
                 column(3, racafeModulos::CajaModalUI(ns("kpi_rev_riesgo")))
               )
        )
      )
    ),
    # Resumen mensual por estado ----
    bs4Dash::bs4Card("Resumen Mensual por Estado", width = 12, solidHeader = TRUE,
                     status = "white", collapsible = TRUE,
                     racafeModulos::TablaReactable2UI(ns("tbl_resumen_mensual"))
                     ),
    # ----
    )
}
Cohortes <- function(id, bd, data_t) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Constantes ----
    # col_specs compartidos para TablaDetalleCliente
    
    .fmt_sacos  <- racafe::DefinirFormato("coma")
    .fmt_margen <- racafe::DefinirFormato("dinero")
    .fmt_pct    <- racafe::DefinirFormato("porcentaje")
    .niveles_trans   <- c("ACTIVO", "INACTIVO")
    .niveles_resumen <- c("ACTIVO", "INACTIVO", "NUEVO")
    
    # Semáforo de color de letra para columnas de cumplimiento: >= 100% verde, >= 85% naranja, rojo
    .style_cumpl <- function(v) {
      if (is.null(v) || is.na(v) || is.infinite(v)) return(NULL)
      list(
        color = if (v >= 1) "#1E8449" else if (v >= 0.85) "#D4780A" else "#C0392B",
        fontWeight = "600"
      )
    }
    # Celda de cumplimiento: porcentaje con 2 decimales o guión si ausente
    .cell_cumpl <- function(v) {
      if (is.null(v) || is.na(v) || is.infinite(v)) return("\u2014")
      .fmt_pct(v)
    }
    
    .coldefs_detalle <- list(LinNegCod = reactable::colDef(show = TRUE, sticky = "left"),
                             CliNitPpal = reactable::colDef(show = TRUE, sticky = "left"),
                             PerRazSoc = reactable::colDef(name = "Cliente", minWidth = 250, sticky = "left"),
                             Segmento = reactable::colDef(name = "Segmento", minWidth = 100),
                             Asesor = reactable::colDef(name = "Asesor", minWidth = 120),
                             Presupuestado = reactable::colDef(name = "Presupuesto", minWidth = 130),
                             UltFecFact = reactable::colDef(name = "\u00dalt. Factura", minWidth = 110,
                                                               cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d/%m/%Y")
                                                               ),
                             # Mes vigente — sacos
                             Ej_SacosVig = reactable::colDef(name = "Sacos Mes", minWidth = 100,
                                                               cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                             Pt_SacosVig = reactable::colDef(name = "Ppto Sacos Mes", minWidth = 110,
                                                               cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                             Cump_SacosVig = reactable::colDef(name = "% Cumpl Sacos Mes", minWidth = 130,
                                                               cell = function(v) .cell_cumpl(v), style = function(v) .style_cumpl(v)),
                             # Mes vigente — margen
                             Ej_MargenVig = reactable::colDef(name = "Margen Mes", minWidth = 130,
                                                                cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                             Pt_MargenVig = reactable::colDef(name = "Ppto Margen Mes", minWidth = 140, 
                                                                cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                             Cump_MargenVig = reactable::colDef(name = "% Cumpl Margen Mes", minWidth = 140,
                                                                cell = function(v) .cell_cumpl(v), style = function(v) .style_cumpl(v)),
                             # YTD — sacos
                             Ej_SacosYTD = reactable::colDef(name = "Sacos Año Corrido", minWidth = 100,
                                                               cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                             Pt_SacosYTD = reactable::colDef(name = "Ppto Sacos Año Corrido", minWidth = 110,
                                                               cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                             Cump_SacosYTD = reactable::colDef(name = "% Cumpl Sacos Año Corrido", minWidth = 130,
                                                               cell = function(v) .cell_cumpl(v), style = function(v) .style_cumpl(v)),
                             # YTD — margen
                             Ej_MargenYTD = reactable::colDef(name = "Margen Año Corrido", minWidth = 130, 
                                                                cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                             Pt_MargenYTD = reactable::colDef(name = "Ppto Margen Año Corrido", minWidth = 140,
                                                                cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                             Cump_MargenYTD = reactable::colDef(name = "% Cumpl Margen Año Corrido", minWidth = 150,
                                                                cell = function(v) .cell_cumpl(v), style = function(v) .style_cumpl(v))
                             )
    
    
    # Helpers ----
    # Semáforo de color para KPIs de tasa — umbrales de negocio Racafé
    .col_kpi <- function(v, tipo) {
      if (is.null(v) || is.na(v)) return("#999999")
      switch(tipo,
             retencion = if (v >= 0.80) "#27AE60" else if (v >= 0.60) "#F4A820" else "#E74C3C",
             churn = if (v <= 0.10) "#27AE60" else if (v <= 0.20) "#F4A820" else "#E74C3C",
             reactivacion = if (v >= 0.30) "#27AE60" else if (v >= 0.15) "#F4A820" else "#E74C3C",
             tasa_fact = if (v >= 0.70) "#27AE60" else if (v >= 0.50) "#F4A820" else "#E74C3C",
             "#999999"
      )
    }
    # Función pura: tabla de detalle cliente con métricas mes vigente + YTD
    .tabla_detalle <- function(dat, mes_vig) {
      dat %>%
        group_by(LinNegCod, CliNitPpal, PerRazSoc, Asesor, Segmento,
                 Presupuestado, UltFecFact) %>%
        summarise(Ej_SacosVig = sum(ifelse(FecProceso == mes_vig, Sacos, 0), na.rm = TRUE),
                  Pt_SacosVig = sum(ifelse(FecProceso == mes_vig, PPtoSacos, 0), na.rm = TRUE),
                  Ej_MargenVig = sum(ifelse(FecProceso == mes_vig, Margen, 0), na.rm = TRUE),
                  Pt_MargenVig = sum(ifelse(FecProceso == mes_vig, PPtoMargen, 0), na.rm = TRUE),
                  Ej_SacosYTD = sum(Sacos, na.rm = TRUE),
                  Pt_SacosYTD = sum(PPtoSacos, na.rm = TRUE),
                  Ej_MargenYTD = sum(Margen, na.rm = TRUE),
                  Pt_MargenYTD = sum(PPtoMargen, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(Cump_SacosVig = dplyr::if_else(Pt_SacosVig  > 0, Ej_SacosVig  / Pt_SacosVig, NA_real_),
               Cump_MargenVig = dplyr::if_else(Pt_MargenVig > 0, Ej_MargenVig / Pt_MargenVig, NA_real_),
               Cump_SacosYTD = dplyr::if_else(Pt_SacosYTD  > 0, Ej_SacosYTD  / Pt_SacosYTD, NA_real_),
               Cump_MargenYTD = dplyr::if_else(Pt_MargenYTD > 0, Ej_MargenYTD / Pt_MargenYTD, NA_real_)) %>%
        select(LinNegCod, CliNitPpal, PerRazSoc, Segmento, Asesor, Presupuestado, UltFecFact,
               Ej_SacosVig, Pt_SacosVig, Cump_SacosVig,
               Ej_MargenVig, Pt_MargenVig, Cump_MargenVig,
               Ej_SacosYTD, Pt_SacosYTD, Cump_SacosYTD,
               Ej_MargenYTD, Pt_MargenYTD, Cump_MargenYTD)
    }
    # KPIs de periodo — conteos por corte e Estado
    .n_estado <- function(dat_corte, estado) {
      n_distinct(paste(
        dat_corte$LinNegCod[dat_corte$EstadoPanel == estado],
        dat_corte$CliNitPpal[dat_corte$EstadoPanel == estado]
      ))
    }
    .n_total <- function(dat_corte) {
      n_distinct(paste(dat_corte$LinNegCod, dat_corte$CliNitPpal))
    }


    # Datos reactivos ----
    
    # Fechas.
    mes_vigente <- reactive({
      req(bd())
      max(bd()$FecProceso, na.rm = TRUE)
    })
    mes_inicio <- reactive({
      req(bd())
      min(bd()$FecProceso, na.rm = TRUE)
    })
    # Universo de UCs activas según filtros dimensionales de data_t
    ucs_activas <- reactive({
      req(data_t())
      data_t() %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
    })
    # bd filtrado al universo dimensional de data_t
    bd_f <- reactive({
      req(bd(), ucs_activas())
      bd() %>%
        semi_join(ucs_activas(), by = join_by(LinNegCod, CliNitPpal))
    })
    # Corte vigente: solo el último mes de bd_f
    bd_vig <- reactive({
      req(bd_f(), mes_vigente())
      bd_f() %>% filter(FecProceso == mes_vigente())
    })
    # Corte inicial: solo el primer mes de bd_f
    bd_ini <- reactive({
      req(bd_f(), mes_inicio())
      bd_f() %>% filter(FecProceso == mes_inicio())
    })
    # Estado inicial/final por UC 
    trans_ucs <- reactive({
      req(bd_f(), mes_inicio(), mes_vigente())
      
      bd_f() %>%
        group_by(LinNegCod, CliNitPpal) %>%
        filter(FecProceso %in% c(mes_inicio(), mes_vigente()),
               EstadoPanel != "NUEVO") %>%
        arrange(FecProceso) %>%
        summarise(EstadoInicial = dplyr::first(EstadoPanel),
                  EstadoFinal = dplyr::last(EstadoPanel),
                  .groups = "drop") %>%
        mutate(EstadoInicial = factor(EstadoInicial, levels = .niveles_trans),
               EstadoFinal = factor(EstadoFinal, levels = .niveles_trans))
    })
    # Matriz de transición: estado inicial vs estado final por UC, excluyendo NUEVO
    matriz_trans <- reactive({
      req(trans_ucs())
      trans_ucs() %>%
        count(EstadoInicial, EstadoFinal, name = "Clientes", .drop = FALSE) %>%
        tidyr::pivot_wider(names_from = EstadoFinal, values_from = Clientes, values_fill = 0) %>%
        mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
        bind_rows(
          (.) %>% summarise(EstadoInicial = "TOTAL", across(where(is.numeric), sum))
        )
    })
    # Fila seleccionada en la matriz absoluta
    sel_matriz_estado <- reactiveVal(NULL)
    
    # Indicadores escalares derivados de la matriz de transición
    indicadores_trans <- reactive({
      req(bd_f())
      
      # Fechas ordenadas del panel
      fechas <- sort(unique(bd_f()$FecProceso))
      
      # Si solo hay un mes no hay transiciones posibles
      if (length(fechas) < 2) {
        return(list(aa = 0L, ai = 0L, ia = 0L, ii = 0L,
                    retencion = NA_real_, churn = NA_real_,
                    reactivacion = NA_real_, activos_ini = 0L,
                    activos_fin = 0L, cambio_neto = 0L,
                    n_transiciones = 0L))
      }
      
      # Construir pares de meses consecutivos: t → t+1
      pares <- purrr::map_dfr(seq_len(length(fechas) - 1), function(i) {
        t0 <- fechas[i]
        t1 <- fechas[i + 1]
        
        # Estado de cada UC en t0 y t1 — solo ACTIVO e INACTIVO
        snap_t0 <- bd_f() %>%
          dplyr::filter(FecProceso == t0, EstadoPanel %in% c("ACTIVO", "INACTIVO")) %>%
          dplyr::distinct(LinNegCod, CliNitPpal, EstadoPanel) %>%
          dplyr::rename(Estado_t0 = EstadoPanel)
        
        snap_t1 <- bd_f() %>%
          dplyr::filter(FecProceso == t1, EstadoPanel %in% c("ACTIVO", "INACTIVO")) %>%
          dplyr::distinct(LinNegCod, CliNitPpal, EstadoPanel) %>%
          dplyr::rename(Estado_t1 = EstadoPanel)
        
        # Solo UCs presentes en ambos meses
        snap_t0 %>%
          dplyr::inner_join(snap_t1, by = c("LinNegCod", "CliNitPpal")) %>%
          dplyr::mutate(t0 = t0, t1 = t1)
      })
      
      if (nrow(pares) == 0) {
        return(list(aa = 0L, ai = 0L, ia = 0L, ii = 0L,
                    retencion = NA_real_, churn = NA_real_,
                    reactivacion = NA_real_, activos_ini = 0L,
                    activos_fin = 0L, cambio_neto = 0L,
                    n_transiciones = 0L))
      }
      
      # Conteo total de cada tipo de transición acumulado en todo el panel
      conteos <- pares %>%
        dplyr::summarise(
          aa = sum(Estado_t0 == "ACTIVO"   & Estado_t1 == "ACTIVO",   na.rm = TRUE),
          ai = sum(Estado_t0 == "ACTIVO"   & Estado_t1 == "INACTIVO", na.rm = TRUE),
          ia = sum(Estado_t0 == "INACTIVO" & Estado_t1 == "ACTIVO",   na.rm = TRUE),
          ii = sum(Estado_t0 == "INACTIVO" & Estado_t1 == "INACTIVO", na.rm = TRUE)
        )
      
      aa <- as.integer(conteos$aa)
      ai <- as.integer(conteos$ai)
      ia <- as.integer(conteos$ia)
      ii <- as.integer(conteos$ii)
      
      # Probabilidades promedio mes a mes
      list(
        aa = aa,
        ai = ai,
        ia = ia,
        ii = ii,
        retencion = dplyr::if_else((aa + ai) > 0, aa / (aa + ai), NA_real_),
        churn = dplyr::if_else((aa + ai) > 0, ai / (aa + ai), NA_real_),
        reactivacion = dplyr::if_else((ia + ii) > 0, ia / (ia + ii), NA_real_),
        # Activos y cambio neto se calculan sobre los cortes extremos
        activos_ini = .n_estado(bd_ini(), "ACTIVO"),
        activos_fin = .n_estado(bd_vig(), "ACTIVO"),
        cambio_neto = .n_estado(bd_vig(), "ACTIVO") - .n_estado(bd_ini(), "ACTIVO"),
        n_transiciones = nrow(pares)
      )
    })
    indicadores_duracion <- reactive({
      req(indicadores_trans())
      ind <- indicadores_trans()
      
      # Guardia: probabilidades válidas y mayores a 0 para evitar división por cero
      t_vida_activo    <- if (!is.na(ind$churn)        && ind$churn        > 0) {
        round(1 / ind$churn,        1)
      } else NA_real_
      
      t_reactiv        <- if (!is.na(ind$reactivacion)  && ind$reactivacion > 0) {
        round(1 / ind$reactivacion, 1)
      } else NA_real_
      
      # Velocidad de deterioro: períodos para reducir base activa a la mitad
      # sin reactivación: n = log(0.5) / log(P(A→A))
      v_deterioro      <- if (!is.na(ind$retencion) && ind$retencion > 0 && ind$retencion < 1) {
        round(log(0.5) / log(ind$retencion), 1)
      } else NA_real_
      
      list(t_vida_activo = t_vida_activo,
           t_reactiv = t_reactiv,
           v_deterioro = v_deterioro)
    })
    estado_estacionario <- reactive({
      req(indicadores_trans())
      ind   <- indicadores_trans()
      denom <- (ind$churn %||% 0) + (ind$reactivacion %||% 0)
      
      pi_a <- if (!is.na(denom) && denom > 0) ind$reactivacion / denom else NA_real_
      pi_i <- if (!is.na(denom) && denom > 0) ind$churn        / denom else NA_real_
      
      # Índice de estabilidad: qué tan cerca está la distribución actual del estacionario
      # Distancia = |%activos_actual - pi_A|
      total_actual <- ind$activos_ini + (ind$ii + ind$ia)
      pct_actual_a <- if (total_actual > 0) ind$activos_fin / total_actual else NA_real_
      
      distancia <- if (!is.na(pi_a) && !is.na(pct_actual_a)) {
        abs(pct_actual_a - pi_a)
      } else NA_real_
      
      list(pi_a = pi_a,
           pi_i = pi_i,
           distancia = distancia)
    })
    
    # Indicadores de valor — revenue en riesgo y recuperable
    # Requiere ticket promedio por UC desde bd_f (usa Margen como proxy de valor)
    indicadores_valor <- reactive({
      req(bd_f(), indicadores_trans())
      ind <- indicadores_trans()
      
      n_activos_act <- ind$activos_fin
      n_inactivos   <- ind$ii + ind$ia
      
      # Ticket promedio de margen por UC activa en el mes vigente
      ticket_prom <- bd_f() %>%
        dplyr::filter(FecProceso == mes_vigente(), Margen > 0) %>%
        dplyr::group_by(LinNegCod, CliNitPpal) %>%
        dplyr::summarise(margen_uc = sum(Margen, na.rm = TRUE), .groups = "drop") %>%
        dplyr::summarise(ticket = mean(margen_uc, na.rm = TRUE)) %>%
        dplyr::pull(ticket)
      ticket_prom <- if (length(ticket_prom) == 0 || is.na(ticket_prom)) 0 else ticket_prom
      
      # Sacos promedio por UC activa en el mes vigente
      sacos_prom <- bd_f() %>%
        dplyr::filter(FecProceso == mes_vigente(), Sacos > 0) %>%
        dplyr::group_by(LinNegCod, CliNitPpal) %>%
        dplyr::summarise(sacos_uc = sum(Sacos, na.rm = TRUE), .groups = "drop") %>%
        dplyr::summarise(sacos = mean(sacos_uc, na.rm = TRUE)) %>%
        dplyr::pull(sacos)
      sacos_prom <- if (length(sacos_prom) == 0 || is.na(sacos_prom)) 0 else sacos_prom
      
      # Estimaciones de riesgo y balance
      rev_riesgo   <- if (!is.na(ind$churn))        n_activos_act * ind$churn        * ticket_prom else NA_real_
      sacos_riesgo <- if (!is.na(ind$churn))        n_activos_act * ind$churn        * sacos_prom  else NA_real_
      balance_neto <- (ind$reactivacion %||% 0) * n_inactivos -
        (ind$churn        %||% 0) * n_activos_act
      
      list(ticket_prom = ticket_prom,
           sacos_prom = sacos_prom,
           rev_riesgo = rev_riesgo,
           sacos_riesgo = sacos_riesgo,
           balance_neto = balance_neto,
           n_inactivos = n_inactivos)
    })
    # Tasa de facturación global: % de meses en panel con Sacos > 0 por UC
    tasa_fact_global <- reactive({
      req(bd_f())
      bd_f() %>%
        mutate(facturo = ifelse(Sacos > 0, 1L, 0L)) %>%
        summarise(tasa = mean(facturo, na.rm = TRUE)) %>%
        pull(tasa)
    })
    # Resumen mensual por EstadoPanel (pivot: estados en filas, meses en cols)
    resumen_mensual <- reactive({
      req(bd_f())
      bd_f() %>%
        mutate(EstadoPanel = factor(EstadoPanel, levels = .niveles_resumen)) %>%
        group_by(FecProceso, EstadoPanel, .drop = FALSE) %>%
        summarise(Clientes = n_distinct(paste(LinNegCod, CliNitPpal)),
                  .groups = "drop") %>%
        mutate(Mes = format(FecProceso, "%b-%y")) %>%
        select(-FecProceso) %>%
        tidyr::pivot_wider(names_from = Mes, values_from = Clientes, values_fill = 0L) %>%
        rename(Estado = EstadoPanel) %>%
        mutate(Estado = as.character(Estado)) %>%
        janitor::adorn_totals("row", name = "TOTAL")
    })
    # Alertas: UCs que se inactivarían este mes (activos cuya UltFecFact + NumMeses == mes_vig)
    alertas_inactivar <- reactive({
      req(bd_vig(), mes_vigente())
      ucs <- bd_vig() %>%
        filter(EstadoPanel == "ACTIVO") %>%
        mutate(meses_rec = ifelse(is.na(NumMesesRecuperar), 4L, NumMesesRecuperar),
               fec_lim = UltFecFact %m+% months(meses_rec)) %>%
        filter(!is.na(fec_lim), month(fec_lim) == month(mes_vigente())) %>%
        select(LinNegCod, CliNitPpal)
      bd_f() %>%
        semi_join(ucs, by = join_by(LinNegCod, CliNitPpal))
    })
    # Alertas: inactivos que facturaron en el mes vigente (recuperados parciales)
    alertas_recuperados <- reactive({
      req(bd_vig())
      ucs <- bd_vig() %>%
        filter(EstadoPanel == "INACTIVO", Sacos != 0) %>%
        select(LinNegCod, CliNitPpal)
      bd_f() %>%
        semi_join(ucs, by = join_by(LinNegCod, CliNitPpal))
    })
    # Alertas: clientes nuevos del periodo
    alertas_nuevos <- reactive({
      req(bd_vig())
      ucs <- bd_vig() %>%
        filter(Estado == "NUEVO") %>%
        select(LinNegCod, CliNitPpal)
      bd_f() %>%
        semi_join(ucs, by = join_by(LinNegCod, CliNitPpal))
    })
    
    # Outputs ----
    ## Resumen de Periodo ---- 
    
    output$lbl_mes_inicial <- renderUI({
      tags$h6(paste0("Mes Inicial \u2014 ", format(mes_inicio(), "%B %Y")),
              style = "font-weight:700; color:#374151;")
    })
    
    # KPI total inicio
    racafeModulos::CajaModal("kpi_ini_total",
                             valor = reactive(.n_total(bd_ini())),
                             formato = "coma",
                             texto = "Total UCs",
                             icono = "users",
                             colores = c(fondo = "white"),
                             mostrar_boton = TRUE,
                             titulo_modal = reactive(paste0("Detalle \u2014 Inicio \u2014 ", format(mes_inicio(), "%B %Y"))),
                             icono_modal = NULL,
                             tamano_modal = "xl",
                             contenido_modal = function() TablaDetalleClientesUI(ns("tbl_mod_ini_total")),
                             footer = reactive(format(mes_inicio(), "Corte %B %Y")),
                             footer_class = "caja-modal-footer")
    
    TablaDetalleClientes(id = "tbl_mod_ini_total",
                         data = bd_ini,
                         mes_vig = mes_vigente,
                         titulo = reactive(paste0("Detalle \u2014 Inicio de Periodo \u2014 ", format(mes_inicio(), "%B %Y"))),
                         subtitulo = "Todos los estados en el corte inicial",
                         footer = reactive(paste0(.n_total(bd_ini()), " unidades comerciales en el corte inicial")),
                         footer_tipo = "info")
    
    # KPI activos inicio
    racafeModulos::CajaModal("kpi_ini_activo",
                             valor = reactive(.n_estado(bd_ini(), "ACTIVO")),
                             formato = "coma",
                             texto = "Activos",
                             icono = "check-circle",
                             colores = c(fondo = "white"),
                             mostrar_boton = TRUE,
                             titulo_modal = reactive(paste0("Activos \u2014 Inicio \u2014 ", format(mes_inicio(), "%B %Y"))),
                             icono_modal = NULL,
                             tamano_modal = "xl",
                             contenido_modal = function() TablaDetalleClientesUI(ns("tbl_mod_ini_activo")),
                             footer_class = "caja-modal-footer")
    
    TablaDetalleClientes(id = "tbl_mod_ini_activo",
                         data = reactive(bd_ini() %>% dplyr::filter(EstadoPanel == "ACTIVO")),
                         mes_vig = mes_vigente,
                         titulo = reactive(paste0("Activos \u2014 Inicio de Periodo \u2014 ", format(mes_inicio(), "%B %Y"))),
                         footer = reactive({
                           n <- .n_estado(bd_ini(), "ACTIVO")
                           paste0(n, " UC(s) activas en el corte inicial")
                           }),
                         footer_tipo = "info")
    
    # KPI inactivos inicio
    racafeModulos::CajaModal("kpi_ini_inactivo",
                             valor = reactive(.n_estado(bd_ini(), "INACTIVO")),
                             formato = "coma",
                             texto = "A Recuperar",
                             icono = "exclamation",
                             colores = c(fondo = "white"),
                             mostrar_boton = TRUE,
                             titulo_modal = reactive(paste0("A Recuperar \u2014 Inicio \u2014 ", format(mes_inicio(), "%B %Y"))),
                             icono_modal = NULL,
                             tamano_modal = "xl",
                             contenido_modal = function() TablaDetalleClientesUI(ns("tbl_mod_ini_inactivo")),
                             footer_class = "caja-modal-footer")
    
    TablaDetalleClientes(id = "tbl_mod_ini_inactivo",
                         data = reactive(bd_ini() %>% dplyr::filter(EstadoPanel == "INACTIVO")),
                         mes_vig = mes_vigente,
                         titulo = reactive(paste0("A Recuperar \u2014 Inicio de Periodo \u2014 ", format(mes_inicio(), "%B %Y"))),
                         footer = reactive({
                           n <- .n_estado(bd_ini(), "INACTIVO")
                           paste0(n, " UC(s) inactivas en el corte inicial")
                           }),
                         footer_tipo = "warning")
    
    # KPI nuevos inicio
    racafeModulos::CajaModal("kpi_ini_nuevo",
                             valor = reactive(.n_estado(bd_ini(), "NUEVO")),
                             formato = "coma",
                             texto = "Nuevos",
                             icono = "star",
                             colores = c(fondo = "white"),
                             mostrar_boton = TRUE,
                             titulo_modal = reactive({paste0(
                               "Nuevos \u2014 Inicio \u2014 ", format(mes_inicio(), "%B %Y")
                             )}),
                             icono_modal = NULL,
                             tamano_modal = "xl",
                             contenido_modal = function() TablaDetalleClientesUI(ns("tbl_mod_ini_nuevo")),
                             footer_class = "caja-modal-footer")
    
    TablaDetalleClientes(id = "tbl_mod_ini_nuevo",
                         data = reactive(bd_ini() %>% dplyr::filter(EstadoPanel == "NUEVO")),
                         mes_vig = mes_vigente,
                         titulo = reactive(paste0("Nuevos \u2014 Inicio de Periodo \u2014 ", format(mes_inicio(), "%B %Y"))),
                         footer = reactive({
                           n <- .n_estado(bd_ini(), "NUEVO")
                           paste0(n, " UC(s) nuevas en el corte inicial")
                           }),
                         footer_tipo = "info")
    
    output$lbl_mes_vigente <- renderUI({
      tags$h6(paste0("Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y")),
              style = "font-weight:700; color:#374151;")
    })
    
    # KPI total vigente
    racafeModulos::CajaModal("kpi_vig_total",
                             valor = reactive(.n_total(bd_vig())),
                             formato = "coma",
                             texto = "Total UCs",
                             icono = "users",
                             colores = c(fondo = "white"),
                             mostrar_boton = TRUE,
                             titulo_modal = reactive(paste0("Detalle \u2014 Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y"))),
                             icono_modal = NULL,
                             tamano_modal = "xl",
                             contenido_modal = function() TablaDetalleClientesUI(ns("tbl_mod_vig_total")),
                             footer = reactive(format(mes_vigente(), "Corte %B %Y")),
                             footer_class = "caja-modal-footer")
    
    TablaDetalleClientes(id = "tbl_mod_vig_total",
                         data = bd_vig,
                         mes_vig = mes_vigente,
                         titulo = reactive(paste0("Detalle \u2014 Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y"))),
                         subtitulo = "Todos los estados en el mes vigente",
                         footer = reactive(paste0(.n_total(bd_vig()), " unidades comerciales en el mes vigente")),
                         footer_tipo = "info")
    
    # KPI activos vigente
    racafeModulos::CajaModal("kpi_vig_activo",
                             valor = reactive(.n_estado(bd_vig(), "ACTIVO")),
                             formato = "coma",
                             texto = "Activos",
                             icono = "check-circle",
                             colores = c(fondo = "white"),
                             mostrar_boton = TRUE,
                             titulo_modal = reactive(paste0("Activos \u2014 Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y"))),
                             icono_modal = NULL,
                             tamano_modal = "xl",
                             contenido_modal = function() TablaDetalleClientesUI(ns("tbl_mod_vig_activo")),
                             footer_class = "caja-modal-footer")
    
    TablaDetalleClientes(id = "tbl_mod_vig_activo",
                         data = reactive(bd_vig() %>% dplyr::filter(EstadoPanel == "ACTIVO")),
                         mes_vig = mes_vigente,
                         titulo = reactive(paste0("Activos \u2014 Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y"))),
                         footer = reactive({
                           n <- .n_estado(bd_vig(), "ACTIVO")
                           paste0(n, " UC(s) activas en el mes vigente")
                         }),
                         footer_tipo = "info")
    
    # KPI inactivos vigente
    racafeModulos::CajaModal("kpi_vig_inactivo",
                             valor = reactive(.n_estado(bd_vig(), "INACTIVO")),
                             formato = "coma",
                             texto = "A Recuperar",
                             icono = "exclamation",
                             colores = c(fondo = "white"),
                             mostrar_boton = TRUE,
                             titulo_modal = reactive(paste0("A Recuperar \u2014 Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y"))),
                             icono_modal = NULL,
                             tamano_modal = "xl",
                             contenido_modal = function() TablaDetalleClientesUI(ns("tbl_mod_vig_inactivo")),
                             footer_class = "caja-modal-footer")
    
    TablaDetalleClientes(id = "tbl_mod_vig_inactivo",
                         data = reactive(bd_vig() %>% dplyr::filter(EstadoPanel == "INACTIVO")),
                         mes_vig = mes_vigente,
                         titulo = reactive(paste0("A Recuperar \u2014 Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y"))),
                         footer = reactive({
                           n <- .n_estado(bd_vig(), "INACTIVO")
                           paste0(n, " UC(s) inactivas en el mes vigente")
                         }),
                         footer_tipo = "warning")
    
    # KPI nuevos vigente
    racafeModulos::CajaModal("kpi_vig_nuevo",
                             valor = reactive(.n_estado(bd_vig(), "NUEVO")),
                             formato = "coma",
                             texto = "Nuevos",
                             icono = "star",
                             colores = c(fondo = "white"),
                             mostrar_boton = TRUE,
                             titulo_modal = reactive({paste0(
                               "Nuevos \u2014 Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y")
                             )}),
                             icono_modal = NULL,
                             tamano_modal = "xl",
                             contenido_modal = function() TablaDetalleClientesUI(ns("tbl_mod_vig_nuevo")),
                             footer_class = "caja-modal-footer")
    
    TablaDetalleClientes(id = "tbl_mod_vig_nuevo",
                         data = reactive(bd_vig() %>% dplyr::filter(EstadoPanel == "NUEVO")),
                         mes_vig = mes_vigente,
                         titulo = reactive(paste0("Nuevos \u2014 Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y"))),
                         footer = reactive({
                           n <- .n_estado(bd_vig(), "NUEVO")
                           paste0(n, " UC(s) nuevas en el mes vigente")
                           }),
                         footer_tipo = "info")
    
    # KPI — clientes en riesgo de inactivarse este mes
    racafeModulos::CajaModal("kpi_vig_en_riesgo",
                             valor = reactive({
                               n_distinct(paste(
                                 alertas_inactivar()$LinNegCod,
                                 alertas_inactivar()$CliNitPpal
                               ))
                             }),
                             formato = "coma",
                             texto = "En Riesgo",
                             icono = "exclamation-triangle",
                             colores = c(fondo = "white"),
                             mostrar_boton = TRUE,
                             titulo_modal = reactive({paste0(
                               "En Riesgo de Inactivarse \u2014 ", format(mes_vigente(), "%B %Y")
                             )}),
                             icono_modal = NULL,
                             tamano_modal = "xl",
                             contenido_modal = function() {
                               TablaDetalleClientesUI(ns("tbl_mod_vig_en_riesgo"))
                             },
                             footer = reactive({
                               n <- n_distinct(paste(
                                 alertas_inactivar()$LinNegCod,
                                 alertas_inactivar()$CliNitPpal
                               ))
                               paste0(n, " activos llegan al l\u00edmite de inactivaci\u00f3n este mes")
                             }),
                             footer_class = "caja-modal-footer")
    
    TablaDetalleClientes(id = "tbl_mod_vig_en_riesgo",
                         data = alertas_inactivar,
                         mes_vig = mes_vigente,
                         titulo = reactive({paste0(
                           "En Riesgo de Inactivarse \u2014 ", format(mes_vigente(), "%B %Y")
                         )}),
                         subtitulo   = "Activos cuya \u00faltima factura alcanza el umbral de inactivaci\u00f3n este mes",
                         footer      = reactive({
                           n <- dplyr::n_distinct(paste(
                             alertas_inactivar()$LinNegCod, alertas_inactivar()$CliNitPpal
                           ))
                           paste0(n, " UC(s) en riesgo de pasar a 'A Recuperar' este mes")
                         }),
                         footer_tipo = "warning")
    
    # KPI — inactivos que facturaron este mes (se recuperan)
    racafeModulos::CajaModal("kpi_vig_por_recuperar",
                             valor = reactive({
                               n_distinct(paste(
                                 alertas_recuperados()$LinNegCod,
                                 alertas_recuperados()$CliNitPpal
                               ))
                             }),
                             formato = "coma",
                             texto = "Recuper\u00e1ndose",
                             icono = "undo",
                             colores = c(fondo = "white"),
                             mostrar_boton = TRUE,
                             titulo_modal = reactive({paste0(
                               "Inactivos que Facturaron \u2014 ", format(mes_vigente(), "%B %Y")
                             )}),
                             icono_modal = NULL,
                             tamano_modal = "xl",
                             contenido_modal = function() {
                               TablaDetalleClientesUI(ns("tbl_mod_vig_por_recuperar"))
                             },
                             footer = reactive({
                               n <- n_distinct(paste(
                                 alertas_recuperados()$LinNegCod,
                                 alertas_recuperados()$CliNitPpal
                               ))
                               paste0(n, " inactivos con factura en el mes \u2014 próximos a reactivarse")
                             }),
                             footer_class = "caja-modal-footer")
    
    TablaDetalleClientes(id = "tbl_mod_vig_por_recuperar",
                         data = alertas_recuperados,
                         mes_vig = mes_vigente,
                         titulo = reactive({paste0(
                           "Inactivos que Facturaron \u2014 ", format(mes_vigente(), "%B %Y")
                         )}),
                         subtitulo   = "Inactivos con compra en el mes vigente \u2014 candidatos a reactivaci\u00f3n",
                         footer      = reactive({
                           n <- dplyr::n_distinct(paste(
                             alertas_recuperados()$LinNegCod, alertas_recuperados()$CliNitPpal
                           ))
                           paste0(n, " UC(s) inactivas con facturaci\u00f3n en el mes vigente")
                         }),
                         footer_tipo = "info")
    
    
    ## Indicadores de Transición ----
    
    # Registro eager — detalle longitudinal para modal de matriz absoluta
    TablaDetalleClientes(
      id = "tbl_mod_matriz_total",
      data = reactive({
        req(sel_matriz_estado(), trans_ucs())
        ucs_sel <- trans_ucs() %>%
          dplyr::filter(EstadoInicial == sel_matriz_estado()) %>%
          dplyr::select(LinNegCod, CliNitPpal)
        bd_f() %>%
          dplyr::semi_join(ucs_sel, by = dplyr::join_by(LinNegCod, CliNitPpal))
      }),
      mes_vig = mes_vigente,
      titulo = reactive(paste0(
        "Detalle \u2014 Estado Inicial: ", sel_matriz_estado() %||% "", " \u2014 ",
        format(mes_inicio(), "%b %Y"), " \u2192 ", format(mes_vigente(), "%b %Y")
      )),
      subtitulo = "Clientes cuyo estado inicial corresponde a la fila seleccionada",
      footer = reactive({
        req(sel_matriz_estado(), trans_ucs())
        n <- trans_ucs() %>%
          dplyr::filter(EstadoInicial == sel_matriz_estado()) %>%
          nrow()
        paste0(n, " unidades comerciales con estado inicial '", sel_matriz_estado(), "'")
      }),
      footer_tipo = "info"
    )
    
    # Matriz absoluta — modo fila para habilitar modal de detalle
    racafeModulos::TablaReactable2(
      id = "tbl_matriz_trans_abs",
      data = reactive({
        req(matriz_trans())
        matriz_trans() %>% dplyr::rename(Estado = EstadoInicial)
      }),
      titulo = "Transiciones",
      subtitulo = reactive({paste0(
        format(mes_inicio(), "%b %Y"), " \u2192 ", format(mes_vigente(), "%b %Y")
      )}),
      footer = reactive({
        ind <- indicadores_trans()
        paste0(
          "Retuvieron: ",       ind$aa, "<br/>",
          "Se perdieron: ",     ind$ai, "<br/>",
          "Se recuperaron: ",   ind$ia, "<br/>",
          "Siguen inactivos: ", ind$ii, "<br/>",
          tags$small(style = "color:#9CA3AF;",
                     "Conteos acumulados sobre ", ind$n_transiciones, " transiciones mensuales"
          ) %>% as.character()
        ) %>% HTML
      }),
      id_col = "Estado",
      col_specs = list(
        Estado = reactable::colDef(name = "Estado Inicial", minWidth = 120, sticky = "left"),
        ACTIVO = reactable::colDef(name = "Activo",   minWidth = 80),
        INACTIVO = reactable::colDef(name = "Inactivo", minWidth = 80),
        TOTAL = reactable::colDef(name = "Total",    minWidth = 80,
                                  style = list(fontWeight = "bold"))
      ),
      modo_seleccion = "celda",
      cols_activos = c("Estado", "ACTIVO", "INACTIVO"),
      filas_bloqueadas = as.character(length(.niveles_trans)),
      searchable = FALSE,
      sortable = FALSE,
      page_size = 10L,
      modal_titulo_fn = function(sel) paste0("Detalle \u2014 ", sel$id),
      modal_pre_fn = function(sel) sel_matriz_estado(as.character(sel$id)),
      modal_contenido_fn = function(sel) TablaDetalleClientesUI(session$ns("tbl_mod_matriz_total")),
      modal_size = "xl"
    )
    
    # Matriz porcentual — sin selección
    racafeModulos::TablaReactable2(
      id = "tbl_matriz_trans",
      data = reactive({
        req(matriz_trans(), input$sel_tipo_matriz)
        m     <- matriz_trans()
        datos <- m %>% dplyr::filter(EstadoInicial != "TOTAL")
        tot_g <- sum(datos$ACTIVO, na.rm = TRUE) + sum(datos$INACTIVO, na.rm = TRUE)
        tot_a <- sum(datos$ACTIVO,   na.rm = TRUE)
        tot_i <- sum(datos$INACTIVO, na.rm = TRUE)
        
        result <- switch(input$sel_tipo_matriz,
                         pct_fila = m %>%
                           dplyr::mutate(
                             ACTIVO = ifelse(TOTAL > 0, ACTIVO   / TOTAL, NA_real_),
                             INACTIVO = ifelse(TOTAL > 0, INACTIVO / TOTAL, NA_real_),
                             TOTAL = ifelse(TOTAL > 0, 1,                NA_real_)
                           ),
                         pct_col = m %>%
                           dplyr::mutate(
                             ACTIVO = if (tot_a > 0) ACTIVO   / tot_a else NA_real_,
                             INACTIVO = if (tot_i > 0) INACTIVO / tot_i else NA_real_,
                             TOTAL = if (tot_g > 0) TOTAL    / tot_g else NA_real_
                           ),
                         pct_total = m %>%
                           dplyr::mutate(
                             ACTIVO = if (tot_g > 0) ACTIVO   / tot_g else NA_real_,
                             INACTIVO = if (tot_g > 0) INACTIVO / tot_g else NA_real_,
                             TOTAL = if (tot_g > 0) TOTAL    / tot_g else NA_real_
                           )
        )
        result %>% dplyr::rename(Estado = EstadoInicial)
      }),
      titulo = reactive({
        switch(input$sel_tipo_matriz,
               pct_fila = "Transiciones \u2014 % Fila (origen)",
               pct_col = "Transiciones \u2014 % Columna (destino)",
               pct_total = "Transiciones \u2014 % Total general"
        )
      }),
      subtitulo = reactive(paste0(
        format(mes_inicio(), "%b %Y"), " \u2192 ", format(mes_vigente(), "%b %Y")
      )),
      footer = reactive({
        ind <- indicadores_trans()
        switch(input$sel_tipo_matriz,
               pct_fila = paste0(
                 "De cada 100 activos al inicio: ",
                 round((ind$retencion %||% 0) * 100, 1), " siguen activos, ",
                 round((ind$churn     %||% 0) * 100, 1), " se perdieron"
               ) %>% HTML,
               pct_col = paste0(
                 "De cada 100 activos al cierre: ",
                 round((ind$retencion %||% 0) * 100, 1), " ya eran activos antes"
               ) %>% HTML,
               pct_total = {
                 total <- ind$aa + ind$ai + ind$ia + ind$ii
                 v     <- if (total > 0) (ind$aa + ind$ii) / total else NA_real_
                 paste0(round((v %||% 0) * 100, 1),
                        "% del panel no cambi\u00f3 de estado en el periodo") %>% HTML
               }
        )
      }),
      id_col = "Estado",
      col_specs = list(
        Estado = reactable::colDef(name = "Estado Inicial", minWidth = 120, sticky = "left"),
        ACTIVO = reactable::colDef(name = "Activo",   minWidth = 80,
                                     cell = function(v) if (is.na(v)) "\u2014" else .fmt_pct(v)),
        INACTIVO = reactable::colDef(name = "Inactivo", minWidth = 80,
                                     cell = function(v) if (is.na(v)) "\u2014" else .fmt_pct(v)),
        TOTAL = reactable::colDef(name = "Total",    minWidth = 80,
                                     cell = function(v) if (is.na(v)) "\u2014" else .fmt_pct(v),
                                     style = list(fontWeight = "bold"))
      ),
      modo_seleccion = "ninguno",
      searchable = FALSE,
      sortable = FALSE,
      page_size = 10L
    )
    
    # KPIs de tasa
    racafeModulos::CajaModal("kpi_retencion",
                             valor = reactive({
                               v <- indicadores_trans()$retencion
                               racafeModulos::html_valor(
                                 v %||% 0, formato = "porcentaje",
                                 color = .col_kpi(v, "retencion")
                               )
                             }),
                             texto = "Retenci\u00f3n Activos",
                             icono = "check-double",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               ind <- indicadores_trans()
                               paste0(
                                 ind$aa, " clientes activos se mantuvieron<br/>",
                                 "Activos al inicio: ", ind$activos_ini,
                                 " \u2014 al cierre: ", ind$activos_fin
                               ) %>% HTML
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_churn",
                             valor = reactive({
                               v <- indicadores_trans()$churn
                               racafeModulos::html_valor(
                                 v %||% 0, formato = "porcentaje",
                                 color = .col_kpi(v, "churn")
                               )
                             }),
                             texto = "P\u00e9rdida (Churn)",
                             icono = "arrow-down",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               ind     <- indicadores_trans()
                               neto_txt <- if (ind$cambio_neto >= 0) {
                                 paste0("+", ind$cambio_neto)
                               } else {
                                 as.character(ind$cambio_neto)
                               }
                               paste0(
                                 ind$ai, " clientes dejaron de comprar<br/>",
                                 "Cambio neto de la base: ", neto_txt, " clientes"
                               ) %>% HTML
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_reactivacion",
                             valor = reactive({
                               v <- indicadores_trans()$reactivacion
                               racafeModulos::html_valor(
                                 v %||% 0, formato = "porcentaje",
                                 color = .col_kpi(v, "reactivacion")
                               )
                             }),
                             texto = "Reactivaci\u00f3n",
                             icono = "sync-alt",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               ind <- indicadores_trans()
                               paste0(
                                 ind$ia, " inactivos volvieron a comprar<br/>",
                                 ind$ii, " permanecen sin facturar"
                               ) %>% HTML
                             }),
                             footer_class = "caja-modal-footer")
    
    # KPIs de volumen
    racafeModulos::CajaModal("kpi_activos_ini",
                             valor = reactive(indicadores_trans()$activos_ini),
                             formato = "coma",
                             texto = "Activos Inicio",
                             icono = "flag",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               ind <- indicadores_trans()
                               paste0(
                                 ind$aa, " se mantuvieron \u2014 ",
                                 ind$ai, " dejaron de comprar"
                               )
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_activos_fin",
                             valor = reactive(indicadores_trans()$activos_fin),
                             formato = "coma",
                             texto = "Activos Cierre",
                             icono = "flag-checkered",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               ind <- indicadores_trans()
                               paste0(
                                 ind$aa, " ya eran activos \u2014 ",
                                 ind$ia, " se recuperaron"
                               )
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_cambio_neto",
                             valor = reactive({
                               v     <- indicadores_trans()$cambio_neto
                               color <- if (is.na(v)) "#999999" else if (v >= 0) "#27AE60" else "#E74C3C"
                               racafeModulos::html_valor(v, formato = "coma", color = color)
                             }),
                             texto = "Cambio Neto",
                             icono = "exchange-alt",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               ind <- indicadores_trans()
                               tendencia <- if (ind$cambio_neto > 0) {
                                 paste0("La base creci\u00f3 en ", ind$cambio_neto, " clientes")
                               } else if (ind$cambio_neto < 0) {
                                 paste0("La base se redujo en ", abs(ind$cambio_neto), " clientes")
                               } else {
                                 "La base no cambi\u00f3 de tama\u00f1o"
                               }
                               paste0(
                                 ind$ia, " recuperados vs ", ind$ai, " perdidos<br/>",
                                 tendencia
                               ) %>% HTML
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_estabilidad",
                             valor = reactive({
                               ind   <- indicadores_trans()
                               total <- ind$aa + ind$ai + ind$ia + ind$ii
                               v     <- dplyr::if_else(total > 0, (ind$aa + ind$ii) / total, NA_real_)
                               racafeModulos::html_valor(
                                 v %||% 0, formato = "porcentaje",
                                 color = if (is.na(v))       "#999999"
                                 else if (v >= 0.75) "#27AE60"
                                 else if (v >= 0.55) "#F4A820"
                                 else                "#E74C3C"
                               )
                             }),
                             texto = "Estabilidad",
                             icono = "anchor",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               ind   <- indicadores_trans()
                               total <- ind$aa + ind$ai + ind$ia + ind$ii
                               paste0(
                                 ind$aa + ind$ii, " de ", total,
                                 " clientes no cambiaron de estado<br/>",
                                 "<strong>Sesgo:</strong> no distingue si la estabilidad",
                                 " es en activo o en inactivo."
                               ) %>% HTML
                             }),
                             footer_class = "caja-modal-footer")
    
    # KPIs de duración
    racafeModulos::CajaModal("kpi_vida_activo",
                             valor = reactive({
                               v <- indicadores_duracion()$t_vida_activo
                               racafeModulos::html_valor(
                                 v %||% 0, formato = "coma",
                                 color = if (is.na(v))     "#999999"
                                 else if (v >= 6)  "#27AE60"
                                 else if (v >= 3)  "#F4A820"
                                 else              "#E74C3C"
                               )
                             }),
                             texto = "Vida Media Activo (meses)",
                             icono = "hourglass-half",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               v   <- indicadores_duracion()$t_vida_activo
                               ind <- indicadores_trans()
                               if (is.na(v)) {
                                 "Sin suficientes transiciones para calcular."
                               } else {
                                 paste0(
                                   "Meses promedio que un cliente permanece activo",
                                   " antes de inactivarse.<br/>",
                                   "<strong>Sesgo:</strong> clientes con historial",
                                   " corto en el panel reducen este promedio."
                                 ) %>% HTML
                               }
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_t_reactiv",
                             valor = reactive({
                               v <- indicadores_duracion()$t_reactiv
                               racafeModulos::html_valor(
                                 v %||% 0, formato = "coma",
                                 color = if (is.na(v))     "#999999"
                                 else if (v <= 3)  "#27AE60"
                                 else if (v <= 6)  "#F4A820"
                                 else              "#E74C3C"
                               )
                             }),
                             texto = "Tiempo de Reactivaci\u00f3n (meses)",
                             icono = "clock",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               v <- indicadores_duracion()$t_reactiv
                               if (is.na(v)) {
                                 "Sin suficientes transiciones para calcular."
                               } else {
                                 paste0(
                                   "Meses promedio que tarda un inactivo en volver a comprar.<br/>",
                                   "<strong>Sesgo:</strong> los que nunca regresan no se cuentan,",
                                   " lo que reduce el promedio observado."
                                 ) %>% HTML
                               }
                             }),
                             footer_class = "caja-modal-footer")
    
    # KPIs de largo plazo
    racafeModulos::CajaModal("kpi_balance_neto",
                             valor = reactive({
                               v     <- indicadores_valor()$balance_neto
                               color <- if (is.na(v))   "#999999"
                               else if (v > 0)  "#27AE60"
                               else if (v == 0) "#F4A820"
                               else             "#E74C3C"
                               racafeModulos::html_valor(
                                 round(v %||% 0, 1), formato = "coma", color = color
                               )
                             }),
                             texto = "Balance Neto de Base",
                             icono = "balance-scale",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               val      <- indicadores_valor()
                               ind      <- indicadores_trans()
                               tendencia <- if (val$balance_neto > 0) {
                                 "<strong>La base est\u00e1 creciendo.</strong>"
                               } else if (val$balance_neto < 0) {
                                 "<strong>La base est\u00e1 decreciendo.</strong>"
                               } else {
                                 "<strong>La base est\u00e1 estable.</strong>"
                               }
                               paste0(
                                 tendencia, "<br/>",
                                 "Recuperaciones esperadas vs p\u00e9rdidas esperadas por mes.<br/>",
                                 "<strong>Sesgo:</strong> asume que las tasas del periodo",
                                 " se mantienen constantes el pr\u00f3ximo mes."
                               ) %>% HTML
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_pi_activo",
                             valor = reactive({
                               v <- estado_estacionario()$pi_a
                               racafeModulos::html_valor(
                                 v %||% 0, formato = "porcentaje",
                                 color = if (is.na(v))       "#999999"
                                 else if (v >= 0.60) "#27AE60"
                                 else if (v >= 0.40) "#F4A820"
                                 else                "#E74C3C"
                               )
                             }),
                             texto = "Activos en Largo Plazo",
                             icono = "infinity",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               ee <- estado_estacionario()
                               if (is.na(ee$pi_a)) {
                                 "Sin suficientes transiciones para proyectar."
                               } else {
                                 paste0(
                                   "Proporci\u00f3n a la que converge la base activa",
                                   " si las tasas no cambian.<br/>",
                                   "Distancia al equilibrio hoy: ",
                                   .fmt_pct(ee$distancia %||% 0), "<br/>",
                                   "<strong>Sesgo:</strong> asume tasas constantes.",
                                   " Estacionalidad o campa\u00f1as alteran este rumbo."
                                 ) %>% HTML
                               }
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_pi_inactivo",
                             valor = reactive({
                               v <- estado_estacionario()$pi_i
                               racafeModulos::html_valor(
                                 v %||% 0, formato = "porcentaje",
                                 color = if (is.na(v))       "#999999"
                                 else if (v <= 0.30) "#27AE60"
                                 else if (v <= 0.50) "#F4A820"
                                 else                "#E74C3C"
                               )
                             }),
                             texto = "Inactivos en Largo Plazo",
                             icono = "moon",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               ee <- estado_estacionario()$pi_i
                               if (is.na(ee)) {
                                 "Sin suficientes transiciones para proyectar."
                               } else {
                                 paste0(
                                   "Proporci\u00f3n estructural de inactivos",
                                   " si las tasas no cambian.<br/>",
                                   "<strong>Sesgo:</strong> mejora la tasa de reactivaci\u00f3n",
                                   " y este valor baja."
                                 ) %>% HTML
                               }
                             }),
                             footer_class = "caja-modal-footer")
    
    # KPI de valor — sacos en riesgo
    racafeModulos::CajaModal("kpi_sacos_riesgo",
                             valor = reactive({
                               v <- indicadores_valor()$sacos_riesgo
                               racafeModulos::html_valor(
                                 v %||% 0, formato = "coma",
                                 color = if (is.na(v)) "#999999" else "#E74C3C"
                               )
                             }),
                             texto = "Sacos en Riesgo",
                             icono = "exclamation-triangle",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               val <- indicadores_valor()
                               ind <- indicadores_trans()
                               paste0(
                                 "Sacos mensuales en riesgo si el churn se repite.<br/>",
                                 ind$activos_fin, " activos \u00d7 ",
                                 .fmt_pct(ind$churn %||% 0), " churn \u00d7 ",
                                 .fmt_sacos(val$sacos_prom), " sacos promedio.<br/>",
                                 "<strong>Sesgo:</strong> usa sacos promedio del mes vigente."
                               ) %>% HTML
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_rev_riesgo",
                             valor = reactive({
                               v <- indicadores_valor()$rev_riesgo
                               racafeModulos::html_valor(
                                 v %||% 0, formato = "dinero",
                                 color = if (is.na(v)) "#999999" else "#E74C3C"
                               )
                             }),
                             texto = "Margen en Riesgo",
                             icono = "dollar-sign",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive({
                               val <- indicadores_valor()
                               ind <- indicadores_trans()
                               paste0(
                                 "Margen mensual en riesgo si el churn se repite.<br/>",
                                 ind$activos_fin, " activos \u00d7 ",
                                 .fmt_pct(ind$churn %||% 0), " churn \u00d7 ",
                                 .fmt_margen(val$ticket_prom), " margen promedio.<br/>",
                                 "<strong>Sesgo:</strong> clientes grandes que hacen churn",
                                 " elevan este valor por encima del promedio real."
                               ) %>% HTML
                             }),
                             footer_class = "caja-modal-footer")
    
    ## Tablas Detalle y Alertas ----
    
    # Tabla completa de clientes — todos los estados
    racafeModulos::TablaReactable2(id = "tbl_detalle",
                                   data = reactive(.tabla_detalle(bd_f(), mes_vigente())),
                                   titulo = reactive({paste0(
                                     "Detalle de Clientes \u2014 ", format(mes_vigente(), "%B %Y")
                                   )}),
                                   subtitulo = "Ejecutado vs presupuesto — mes vigente y Año Corrido",
                                   footer = reactive({paste0(
                                     .n_total(bd_vig()), " unidades comerciales en el panel"
                                   )}),
                                   col_specs = .coldefs_detalle,
                                   id_col = "CliNitPpal",
                                   modo_seleccion = "ninguno",
                                   searchable = TRUE,
                                   page_size = 20L
                                   )
    
    # Alerta — riesgo de inactivación
    racafeModulos::TablaReactable2(id = "tbl_alerta_inactivar",
                                   data = reactive(.tabla_detalle(alertas_inactivar(), mes_vigente())),
                                   titulo = "Clientes en Riesgo de Inactivarse",
                                   subtitulo = reactive({paste0(
                                     "Con \u00faltima factura fuera del umbral de recuperaci\u00f3n este mes \u2014 ",
                                     format(mes_vigente(), "%B %Y")
                                   )}),
                                   footer = reactive({
                                     n <- n_distinct(paste(
                                       alertas_inactivar()$LinNegCod, alertas_inactivar()$CliNitPpal
                                     ))
                                     paste0(n, " UC(s) en riesgo de pasar a 'A Recuperar' este mes")
                                   }),
                                   footer_tipo = "warning",
                                   col_specs = .coldefs_detalle,
                                   id_col = "CliNitPpal",
                                   modo_seleccion = "ninguno",
                                   searchable = TRUE,
                                   page_size = 20L)
    
    # Alerta — inactivos que facturaron (recuperados parciales)
    racafeModulos::TablaReactable2(id = "tbl_alerta_recuperados",
                                   data = reactive(.tabla_detalle(alertas_recuperados(), mes_vigente())),
                                   titulo = "Inactivos con Facturaci\u00f3n Este Mes",
                                   subtitulo = reactive({paste0(
                                     "Clientes 'A Recuperar' que facturaron en ",
                                     format(mes_vigente(), "%B %Y")
                                   )}),
                                   footer = reactive({{
                                     n <- n_distinct(paste(
                                       alertas_recuperados()$LinNegCod, alertas_recuperados()$CliNitPpal
                                     ))
                                     paste0(n, " UC(s) inactivas con compra registrada en el mes vigente")
                                   }}),
                                   footer_tipo = "info",
                                   col_specs = .coldefs_detalle,
                                   id_col = "CliNitPpal",
                                   modo_seleccion = "ninguno",
                                   searchable = TRUE,
                                   page_size = 20L
                                   )
    
    # Alerta — clientes nuevos del periodo
    racafeModulos::TablaReactable2(id = "tbl_alerta_nuevos",
                                   data = reactive(.tabla_detalle(alertas_nuevos(), mes_vigente())),
                                   titulo = "Clientes Nuevos del Periodo",
                                   subtitulo = reactive({paste0(
                                     "Sin historial previo al ", format(mes_inicio(), "%B %Y"),
                                     " que facturaron en el a\u00f1o"
                                   )}),
                                   footer = reactive({
                                     n <- n_distinct(paste(
                                       alertas_nuevos()$LinNegCod, alertas_nuevos()$CliNitPpal
                                     ))
                                     paste0(n, " UC(s) nuevas incorporadas al panel en el periodo")
                                   }),
                                   footer_tipo = "dark",
                                   col_specs = .coldefs_detalle,
                                   id_col = "CliNitPpal",
                                   modo_seleccion = "ninguno",
                                   searchable = TRUE,
                                   page_size = 20L
                                   )
      
    
    ## Resumen mensual por estado ----
    mapa_mes_fecha <- reactive({
      req(bd_f())
      fechas <- sort(unique(bd_f()$FecProceso))
      stats::setNames(fechas, format(fechas, "%b-%y"))
    })
    sel_resumen <- reactiveValues(estado = NULL, mes = NULL)
    
    racafeModulos::TablaReactable2(id = "tbl_resumen_mensual",
                                   data = resumen_mensual,
                                   titulo = "Clientes por Estado y Mes",
                                   subtitulo = "Conteo de unidades comerciales \u00fanicas \u2014 clic en una celda para ver el detalle",
                                   id_col = "Estado",
                                   col_specs = list(Estado = reactable::colDef(name = "Estado", minWidth = 140, sticky = "left")),
                                   modo_seleccion = "celda",
                                   filas_bloqueadas = as.character(length(.niveles_resumen)),
                                   searchable = FALSE,
                                   sortable = FALSE,
                                   page_size = 10L,
                                   modal_titulo_fn = function(sel) paste0(sel$id, " \u2014 ", sel$col),
                                   modal_contenido_fn = function(sel) {
                                     sel_resumen$estado <- sel$id
                                     sel_resumen$mes    <- sel$col
                                     TablaDetalleClientesUI(ns("tbl_mod_resumen_celda"))
                                   },
                                   modal_size = "xl")
    
    # Detalle filtrado por la celda seleccionada — fuente del modal
    TablaDetalleClientes(id = "tbl_mod_resumen_celda",
                         data = reactive({
                           req(sel_resumen$estado, sel_resumen$mes, mapa_mes_fecha())
                           fecha_sel <- mapa_mes_fecha()[[sel_resumen$mes]]
                           req(!is.null(fecha_sel))
                           bd_f() %>%
                             dplyr::filter(FecProceso == fecha_sel, EstadoPanel == sel_resumen$estado)
                           }),
                         mes_vig = reactive({
                           req(sel_resumen$mes, mapa_mes_fecha())
                           mapa_mes_fecha()[[sel_resumen$mes]]
                           }),
                         titulo = reactive({
                           req(sel_resumen$estado, sel_resumen$mes)
                           paste0(sel_resumen$estado, " \u2014 ", sel_resumen$mes)
                           }),
                         subtitulo = "Detalle de unidades comerciales para la celda seleccionada",
                         footer = reactive({
                           req(sel_resumen$estado, sel_resumen$mes, mapa_mes_fecha())
                           fecha_sel <- mapa_mes_fecha()[[sel_resumen$mes]]
                           n <- bd_f() %>%
                             dplyr::filter(FecProceso == fecha_sel, EstadoPanel == sel_resumen$estado) %>%
                             dplyr::summarise(n = dplyr::n_distinct(paste(LinNegCod, CliNitPpal))) %>%
                             dplyr::pull(n)
                           paste0(n, " UC(s) en estado ", sel_resumen$estado, " \u2014 ", sel_resumen$mes)
                           }),
                         footer_tipo = "info"
                         )
    
  })
}

# App de prueba ----
options(OutDec = ",")
ui <- bs4DashPage(
  title = "Cohortes",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  body = bs4DashBody(
    includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
    useShinyjs(), 
    CohortesUI("cohortes"))
)
server <- function(input, output, session) {
  Cohortes("cohortes", bd = reactive(BaseCohortes), data_t = reactive(BaseDatos_t))
}
shinyApp(ui, server)
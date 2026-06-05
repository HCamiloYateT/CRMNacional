CohortesUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Bloque KPIs globales de transición
    bs4Dash::bs4Card(title= "Indicadores de Transición", width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE,
                     fluidRow(column(3, racafeModulos::CajaModalUI(ns("kpi_retencion"))),
                              column(3, racafeModulos::CajaModalUI(ns("kpi_churn"))),
                              column(3, racafeModulos::CajaModalUI(ns("kpi_reactivacion"))),
                              column(3, racafeModulos::CajaModalUI(ns("kpi_tasa_fact")))),
                     fluidRow(column(3, tags$small(class = "text-muted",
                                                   "Activos t \u2192 Activos t+1 / Total Activos en t")),
                              column(3, tags$small(class = "text-muted",
                                                   "Activos t \u2192 Inactivos t+1 / Total Activos en t")),
                              column(3, tags$small(class = "text-muted",
                                                   "Inactivos t \u2192 Activos t+1 / Total Inactivos en t")),
                              column(3, tags$small(class = "text-muted",
                                                   "Meses con facturación / meses en panel YTD"))
                              )
                     ),
    # Bloque KPIs de periodo: inicio vs mes vigente
    bs4Dash::bs4Card(title = "Resumen de Periodo", width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE, 
                     fluidRow(column(6,
                                     uiOutput(ns("lbl_mes_inicial")),
                                     fluidRow(column(4, racafeModulos::CajaModalUI(ns("kpi_ini_total"))),
                                              column(4, racafeModulos::CajaModalUI(ns("kpi_ini_activo"))),
                                              column(4, racafeModulos::CajaModalUI(ns("kpi_ini_inactivo")))
                                              )
                                     ),
                              column(6,
                                     uiOutput(ns("lbl_mes_vigente")),
                                     fluidRow(column(4, racafeModulos::CajaModalUI(ns("kpi_vig_total"))),
                                              column(4, racafeModulos::CajaModalUI(ns("kpi_vig_activo"))),
                                              column(4, racafeModulos::CajaModalUI(ns("kpi_vig_inactivo")))
                                              )
                                     )
                              )
                     ),
    # Detalle completo de clientes
    bs4Dash::bs4Card("Detalle de Clientes", width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE, 
                     racafeModulos::TablaReactable2UI(ns("tbl_detalle"))
                     ),
    # Alerta — riesgo de inactivación
    bs4Dash::bs4Card(title = "Riesgo de Inactivación", width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE, collapsed = TRUE,
                     racafeModulos::TablaReactable2UI(ns("tbl_alerta_inactivar"))
                     ),
    # Alerta — inactivos que facturaron
    bs4Dash::bs4Card("Inactivos con Facturación", width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE, collapsed = TRUE,
                     racafeModulos::TablaReactable2UI(ns("tbl_alerta_recuperados"))
                     ),
    # Alerta — clientes nuevos del periodo
    bs4Dash::bs4Card("Clientes Nuevos del Periodo", width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE, collapsed = TRUE,
                     racafeModulos::TablaReactable2UI(ns("tbl_alerta_nuevos"))
                     ),
    # Resumen mensual por estado + matriz de transición
    fluidRow(
      column(8,
             bs4Dash::bs4Card("Resumen Mensual por Estado", width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE, 
                              racafeModulos::TablaReactable2UI(ns("tbl_resumen_mensual"))
                              )
             ),
      column(4,
        bs4Dash::bs4Card("Matriz de Transición", width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE, 
                         racafeModulos::TablaReactable2UI(ns("tbl_matriz"))
                         )
        )
    ),
    # Gráfico de evolución mensual
    bs4Dash::bs4Card("Evolución Mensual por Estado", width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE, 
                     plotly::plotlyOutput(ns("graf_evolucion"), height = "320px")
                     )
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
    
    # Semáforo de color de letra para columnas de cumplimiento: >= 100% verde, >= 85% naranja, rojo
    .style_cumpl <- function(v) {
      if (is.null(v) || is.na(v) || is.infinite(v)) return(NULL)
      list(
        color      = if (v >= 1) "#1E8449" else if (v >= 0.85) "#D4780A" else "#C0392B",
        fontWeight = "600"
      )
    }
    # Celda de cumplimiento: porcentaje con 2 decimales o guión si ausente
    .cell_cumpl <- function(v) {
      if (is.null(v) || is.na(v) || is.infinite(v)) return("\u2014")
      .fmt_pct(v)
    }
    
    .coldefs_detalle <- list(LinNegCod     = reactable::colDef(show = TRUE, sticky = "left"),
                             CliNitPpal    = reactable::colDef(show = TRUE, sticky = "left"),
                             PerRazSoc     = reactable::colDef(name = "Cliente", minWidth = 250, sticky = "left"),
                             Segmento      = reactable::colDef(name = "Segmento", minWidth = 100),
                             Asesor        = reactable::colDef(name = "Asesor", minWidth = 120),
                             Presupuestado = reactable::colDef(name = "Presupuesto", minWidth = 130),
                             UltFecFact    = reactable::colDef(name = "\u00dalt. Factura", minWidth = 110,
                                                               cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d/%m/%Y")
                                                               ),
                             # Mes vigente — sacos
                             Ej_SacosVig   = reactable::colDef(name = "Sacos Mes", minWidth = 100,
                                                               cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                             Pt_SacosVig   = reactable::colDef(name = "Ppto Sacos Mes", minWidth = 110,
                                                               cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                             Cump_SacosVig = reactable::colDef(name  = "% Cumpl Sacos Mes", minWidth = 130,
                                                               cell  = function(v) .cell_cumpl(v), style = function(v) .style_cumpl(v)),
                             # Mes vigente — margen
                             Ej_MargenVig   = reactable::colDef(name = "Margen Mes", minWidth = 130,
                                                                cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                             Pt_MargenVig   = reactable::colDef(name = "Ppto Margen Mes", minWidth = 140, 
                                                                cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                             Cump_MargenVig = reactable::colDef(name  = "% Cumpl Margen Mes", minWidth = 140,
                                                                cell  = function(v) .cell_cumpl(v), style = function(v) .style_cumpl(v)),
                             # YTD — sacos
                             Ej_SacosYTD   = reactable::colDef(name = "Sacos YTD",      minWidth = 100,
                                                               cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                             Pt_SacosYTD   = reactable::colDef(name = "Ppto Sacos YTD", minWidth = 110,
                                                               cell = function(v) if (is.na(v)) "\u2014" else .fmt_sacos(v)),
                             Cump_SacosYTD = reactable::colDef(name  = "% Cumpl Sacos YTD", minWidth = 130,
                                                               cell  = function(v) .cell_cumpl(v), style = function(v) .style_cumpl(v)),
                             # YTD — margen
                             Ej_MargenYTD   = reactable::colDef(name = "Margen YTD",      minWidth = 130, 
                                                                cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                             Pt_MargenYTD   = reactable::colDef(name = "Ppto Margen YTD", minWidth = 140,
                                                                cell = function(v) if (is.na(v)) "\u2014" else .fmt_margen(v)),
                             Cump_MargenYTD = reactable::colDef(name  = "% Cumpl Margen YTD", minWidth = 150,
                                                                cell  = function(v) .cell_cumpl(v), style = function(v) .style_cumpl(v))
                             )
    
    
    # Helpers ----
    # Semáforo de color para KPIs de tasa — umbrales de negocio Racafé
    .col_kpi <- function(v, tipo) {
      if (is.null(v) || is.na(v)) return("#999999")
      switch(tipo,
             retencion    = if (v >= 0.80) "#27AE60" else if (v >= 0.60) "#F4A820" else "#E74C3C",
             churn        = if (v <= 0.10) "#27AE60" else if (v <= 0.20) "#F4A820" else "#E74C3C",
             reactivacion = if (v >= 0.30) "#27AE60" else if (v >= 0.15) "#F4A820" else "#E74C3C",
             tasa_fact    = if (v >= 0.70) "#27AE60" else if (v >= 0.50) "#F4A820" else "#E74C3C",
             "#999999"
      )
    }
    # Función pura: tabla de detalle cliente con métricas mes vigente + YTD
    .tabla_detalle <- function(dat, mes_vig) {
      dat %>%
        group_by(LinNegCod, CliNitPpal, PerRazSoc, Asesor, Segmento,
                 Presupuestado, UltFecFact) %>%
        summarise(Ej_SacosVig    = sum(ifelse(FecProceso == mes_vig, Sacos, 0),    na.rm = TRUE),
                  Pt_SacosVig    = sum(ifelse(FecProceso == mes_vig, PPtoSacos, 0), na.rm = TRUE),
                  Ej_MargenVig   = sum(ifelse(FecProceso == mes_vig, Margen, 0),   na.rm = TRUE),
                  Pt_MargenVig   = sum(ifelse(FecProceso == mes_vig, PPtoMargen, 0), na.rm = TRUE),
                  Ej_SacosYTD    = sum(Sacos,      na.rm = TRUE),
                  Pt_SacosYTD    = sum(PPtoSacos,  na.rm = TRUE),
                  Ej_MargenYTD   = sum(Margen,     na.rm = TRUE),
                  Pt_MargenYTD   = sum(PPtoMargen, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(Cump_SacosVig  = dplyr::if_else(Pt_SacosVig  > 0, Ej_SacosVig  / Pt_SacosVig,  NA_real_),
               Cump_MargenVig = dplyr::if_else(Pt_MargenVig > 0, Ej_MargenVig / Pt_MargenVig, NA_real_),
               Cump_SacosYTD  = dplyr::if_else(Pt_SacosYTD  > 0, Ej_SacosYTD  / Pt_SacosYTD,  NA_real_),
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
    # Matriz de transición: estado inicial vs estado final por UC, excluyendo NUEVO
    matriz_trans <- reactive({
      req(bd_f(), mes_inicio(), mes_vigente())
      
      niveles_estado <- c("ACTIVO", "INACTIVO")
      
      bd_f() %>%
        group_by(LinNegCod, CliNitPpal) %>%
        filter(FecProceso %in% c(mes_inicio(), mes_vigente()),
               EstadoPanel != "NUEVO") %>%
        arrange(FecProceso) %>%
        summarise(EstadoInicial = dplyr::first(EstadoPanel),
                  EstadoFinal   = dplyr::last(EstadoPanel),
                  .groups = "drop") %>%
        mutate(EstadoInicial = factor(EstadoInicial, levels = niveles_estado),
               EstadoFinal   = factor(EstadoFinal,   levels = niveles_estado)) %>% 
        count(EstadoInicial, EstadoFinal, name = "Clientes", .drop = FALSE) %>%
        tidyr::pivot_wider(names_from  = EstadoFinal, values_from = Clientes, values_fill = 0) %>%
        mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
        bind_rows(
          (.) %>% summarise(EstadoInicial = "TOTAL", across(where(is.numeric), sum))
        )
    })
    # Indicadores escalares derivados de la matriz de transición
    indicadores_trans <- reactive({
      req(matriz_trans())
      m <- matriz_trans()
      
      .v <- function(estado_ini, col) {
        fila <- m[m$EstadoInicial == estado_ini, ]
        if (nrow(fila) == 0 || !col %in% names(fila)) return(0L)
        as.integer(fila[[col]][[1]])
      }
      
      aa <- .v("ACTIVO",   "ACTIVO")
      ai <- .v("ACTIVO",   "INACTIVO")
      ia <- .v("INACTIVO", "ACTIVO")
      ii <- .v("INACTIVO", "INACTIVO")
      
      list(aa = aa,
           ai = ai,
           ia = ia,
           ii = ii,
           retencion    = dplyr::if_else((aa + ai) > 0, aa / (aa + ai), NA_real_),
           churn        = dplyr::if_else((aa + ai) > 0, ai / (aa + ai), NA_real_),
           reactivacion = dplyr::if_else((ia + ii) > 0, ia / (ia + ii), NA_real_),
           activos_ini  = aa + ai,
           activos_fin  = aa + ia,
           cambio_neto  = ia - ai)
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
        group_by(FecProceso, EstadoPanel) %>%
        summarise(Clientes = n_distinct(paste(LinNegCod, CliNitPpal)),
                  .groups = "drop") %>%
        mutate(Mes = format(FecProceso, "%b-%y")) %>%
        select(-FecProceso) %>%
        tidyr::pivot_wider(names_from  = Mes, values_from = Clientes, values_fill = 0L) %>%
        rename(Estado = EstadoPanel) %>% 
        janitor::adorn_totals("row", name = "TOTAL")
    })
    # Alertas: UCs que se inactivarían este mes (activos cuya UltFecFact + NumMeses == mes_vig)
    alertas_inactivar <- reactive({
      req(bd_vig(), mes_vigente())
      ucs <- bd_vig() %>%
        filter(EstadoPanel == "ACTIVO") %>%
        mutate(meses_rec = ifelse(is.na(NumMesesRecuperar), 4L, NumMesesRecuperar),
               fec_lim   = UltFecFact %m+% months(meses_rec)) %>%
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
    ## Indicadores de Transición ----
    racafeModulos::CajaModal("kpi_retencion",
                             valor = reactive({
                               v <- indicadores_trans()$retencion
                               racafeModulos::html_valor(v %||% 0, formato = "porcentaje",
                                                         color = .col_kpi(v, "retencion"))
                             }),
                             texto   = "Retenci\u00f3n Activos",
                             icono   = "check-double",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer  = reactive({
                               ind <- indicadores_trans()
                               paste0("Activos iniciales: ", ind$activos_ini,
                                      " \u2014 Activos finales: ", ind$activos_fin)
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_churn",
                             valor = reactive({
                               v <- indicadores_trans()$churn
                               racafeModulos::html_valor(v %||% 0, formato = "porcentaje",
                                                         color = .col_kpi(v, "churn"))
                             }),
                             texto   = "P\u00e9rdida (Churn)",
                             icono   = "arrow-down",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer  = reactive({
                               ind <- indicadores_trans()
                               paste0("Activos \u2192 Inactivos: ", ind$ai,
                                      " \u2014 Cambio neto: ", ind$cambio_neto)
                             }),
                             footer_class = "caja-modal-footer")
    
    racafeModulos::CajaModal("kpi_reactivacion",
                             valor = reactive({
                               v <- indicadores_trans()$reactivacion
                               racafeModulos::html_valor(v %||% 0, formato = "porcentaje",
                                                         color = .col_kpi(v, "reactivacion"))
                             }),
                             texto   = "Reactivaci\u00f3n",
                             icono   = "sync-alt",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer  = reactive({
                               ind <- indicadores_trans()
                               paste0("Inactivos \u2192 Activos: ", ind$ia,
                                      " \u2014 Permanecen inactivos: ", ind$ii)
                             }),
                             footer_class = "caja-modal-footer")
      
    
    racafeModulos::CajaModal("kpi_tasa_fact",
                             valor = reactive({
                               v <- tasa_fact_global()
                               racafeModulos::html_valor(v %||% 0, formato = "porcentaje",
                                                         color = .col_kpi(v, "tasa_fact"))
                             }),
                             texto   = "Tasa Facturaci\u00f3n Global",
                             icono   = "percentage",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer  = "Promedio de meses con facturación / meses en panel año corrido",
                             footer_class = "caja-modal-footer")
      
    
    ## Resumen de Periodo ----
    
    output$lbl_mes_inicial <- renderUI({
      tags$h6(
        paste0("Mes Incial \u2014 ", format(mes_inicio(), "%B %Y")),
        style = "font-weight:700; color:#374151;"
      )
    })
    output$lbl_mes_vigente <- renderUI({
      tags$h6(
        paste0("Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y")),
        style = "font-weight:700; color:#374151;"
      )
    })
    
    # Macro helper: registra CajaModal de conteo con modal de tabla detalle
    .kpi_conteo <- function(id_kpi, dat_r, filtro_estado, texto_kpi, icono_kpi,
                            titulo_modal, tabla_id) {
      racafeModulos::CajaModal(id_kpi,
                               valor   = reactive(n_distinct(paste(
                                 dat_r()$LinNegCod[dat_r()$EstadoPanel == filtro_estado],
                                 dat_r()$CliNitPpal[dat_r()$EstadoPanel == filtro_estado]
                                 ))),
                               formato = "numero",
                               texto   = texto_kpi,
                               icono   = icono_kpi,
                               colores = c(fondo = "white"),
                               mostrar_boton   = TRUE,
                               titulo_modal    = titulo_modal,
                               icono_modal     = icono_kpi,
                               tamano_modal    = "xl",
                               contenido_modal = function() racafeModulos::TablaReactable2UI(ns(tabla_id))
                               )
      }
    
    # KPI totales — inicio
    racafeModulos::CajaModal("kpi_ini_total",
                             valor   = reactive(.n_total(bd_ini())),
                             formato = "numero",
                             texto   = "Total UCs",
                             icono   = "users",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer  = reactive(format(mes_inicio(), "Corte %B %Y")),
                             footer_class = "caja-modal-footer")
    
    .kpi_conteo("kpi_ini_activo",   bd_ini, "ACTIVO",   "Activos",
                "check-circle",  "Activos — Inicio de Periodo",   "tbl_mod_ini_activo")
    .kpi_conteo("kpi_ini_inactivo", bd_ini, "INACTIVO", "A Recuperar",
                "exclamation",   "A Recuperar — Inicio de Periodo", "tbl_mod_ini_inactivo")
    
    # KPI totales — vigente
    racafeModulos::CajaModal("kpi_vig_total",
                             valor   = reactive(.n_total(bd_vig())),
                             formato = "numero",
                             texto   = "Total UCs",
                             icono   = "users",
                             colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer  = reactive(format(mes_vigente(), "Corte %B %Y")),
                             footer_class = "caja-modal-footer")
    
    .kpi_conteo("kpi_vig_activo",   bd_vig, "ACTIVO",   "Activos",
                "check-circle",  "Activos — Mes Vigente",   "tbl_mod_vig_activo")
    .kpi_conteo("kpi_vig_inactivo", bd_vig, "INACTIVO", "A Recuperar",
                "exclamation",   "A Recuperar — Mes Vigente", "tbl_mod_vig_inactivo")
    
    # Tablas de modal para KPIs de conteo — registradas en startup (patrón eager)
    .registrar_modal_detalle <- function(tabla_id, dat_r, estado) {
      racafeModulos::TablaReactable2(id = ns(tabla_id),
                                     data = reactive({.tabla_detalle(
                                       dat_r() %>% filter(EstadoPanel == estado),
                                       mes_vigente()
                                     )}),
                                     titulo = reactive(paste0("Detalle \u2014 ", estado)),
                                     col_specs = .coldefs_detalle,
                                     id_col = "CliNitPpal",
                                     modo_seleccion = "ninguno",
                                     searchable = TRUE,
                                     page_size = 20L
                                     )
    }
    .registrar_modal_detalle("tbl_mod_ini_activo",   bd_ini, "ACTIVO")
    .registrar_modal_detalle("tbl_mod_ini_inactivo", bd_ini, "INACTIVO")
    .registrar_modal_detalle("tbl_mod_vig_activo",   bd_vig, "ACTIVO")
    .registrar_modal_detalle("tbl_mod_vig_inactivo", bd_vig, "INACTIVO")
    
    ## Tablas Detalle y Alertas ----
    
    # Tabla completa de clientes — todos los estados
    racafeModulos::TablaReactable2(id = "tbl_detalle",
                                   data = reactive(.tabla_detalle(bd_f(), mes_vigente())),
                                   titulo = reactive({paste0(
                                     "Detalle de Clientes \u2014 ", format(mes_vigente(), "%B %Y")
                                   )}),
                                   subtitulo = "Ejecutado vs presupuesto — mes vigente y YTD",
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
                                   footer    = reactive({
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
      
    
    ## Output: Resumen Mensual por Estado ----
    racafeModulos::TablaReactable2(id = "tbl_resumen_mensual",
                                   data = resumen_mensual,
                                   titulo = "Clientes por Estado y Mes",
                                   subtitulo = "Conteo de unidades comerciales únicas",
                                   id_col = "Estado",
                                   col_specs = list(
                                     Estado = reactable::colDef(name = "Estado", minWidth = 140, sticky = "left")
                                   ),
                                   modo_seleccion = "ninguno",
                                   searchable = FALSE,
                                   sortable = FALSE,
                                   page_size = 10L)
    
    ## Output: Matriz de Transición ----
    racafeModulos::TablaReactable2(id = "tbl_matriz",
                                   data = reactive({
                                     req(matriz_trans())
                                     matriz_trans() %>%
                                       rename(Estado = EstadoInicial)
                                   }),
                                   titulo = "Transiciones de Estado",
                                   subtitulo = reactive({paste0(
                                     format(mes_inicio(), "%b %Y"), " \u2192 ",
                                     format(mes_vigente(), "%b %Y")
                                   )}),
                                   id_col         = "Estado",
                                   col_specs      = list(Estado   = reactable::colDef(name = "Estado Inicial", minWidth = 130, sticky = "left"),
                                                         ACTIVO   = reactable::colDef(name = "Activo",   minWidth = 80),
                                                         INACTIVO = reactable::colDef(name = "Inactivo", minWidth = 80),
                                                         TOTAL    = reactable::colDef(name = "Total",    minWidth = 80,
                                                                                      style = list(fontWeight = "bold"))),
                                   modo_seleccion = "ninguno",
                                   searchable     = FALSE,
                                   sortable       = FALSE,
                                   page_size      = 10L)
    
    ## Output: Gráfico de Evolución Mensual ----
    output$graf_evolucion <- plotly::renderPlotly({
      req(bd_f())
      
      # Serie mensual por EstadoPanel
      serie <- bd_f() %>%
        group_by(FecProceso, EstadoPanel) %>%
        summarise(Clientes = n_distinct(paste(LinNegCod, CliNitPpal)),
                  .groups  = "drop") %>%
        mutate(Mes = format(FecProceso, "%b-%y"))
      
      colores_estado <- c("ACTIVO" = "#27AE60",
                          "INACTIVO" = "#E74C3C",
                          "NUEVO" = "#3498DB")
      
      plotly::plot_ly(data = serie, x = ~Mes, y = ~Clientes, color = ~EstadoPanel, colors = colores_estado,
                      type = "scatter", mode = "lines+markers", marker = list(size = 7), 
                      text = ~paste0(EstadoPanel, ": ", Clientes, " UCs"), 
                      hoverinfo = "text") %>%
        plotly::layout(xaxis  = list(title = ""),
                       yaxis  = list(title = "Unidades Comerciales"),
                       legend = list(orientation = "h", x = 0, y = -0.15),
                       margin = list(l = 10, r = 10, t = 10, b = 40)) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
  })
}

# App de prueba ----
options(OutDec = ",")
ui <- bs4DashPage(
  title   = "Cohortes",
  header  = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  body    = bs4DashBody(useShinyjs(), CohortesUI("cohortes"))
)
server <- function(input, output, session) {
  Cohortes("cohortes", bd = reactive(BaseCohortes), data_t = reactive(BaseDatos_t))
}
shinyApp(ui, server)
# Helpers UI ----

# Nota explicativa bajo KPI dinámico
.te_nota <- function(texto) {
  tags$p(
    texto,
    style = paste0(
      "font-size:10px; color:#999; text-align:center; ",
      "margin-top:2px; margin-bottom:0; font-style:italic;"
    )
  )
}

# Separador de sección con icono
.te_seccion <- function(titulo, icono) {
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


# UI principal ----
CohortesUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # [1] Bloque de resumen general del universo completo ----
    # Conteos totales sin filtro de población: activos, a recuperar, nuevos
    bs4Dash::bs4Card(
      title = tagList(icon("users"), " Resumen General del Universo"),
      width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE,
      fluidRow(
        column(4, CajaModalUI(ns("kpi_total_activos"))),
        column(4, CajaModalUI(ns("kpi_total_recuperar"))),
        column(4, CajaModalUI(ns("kpi_total_nuevos")))
      )
    ),
    
    # [2] Bloque de alertas del mes vigente ----
    bs4Dash::bs4Card(
      title = tagList(icon("exchange-alt"), " Alertas del Mes Vigente"),
      width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE,
      fluidRow(
        column(3, CajaModalUI(ns("kpi_a_inactivo"))),
        column(3, CajaModalUI(ns("kpi_a_activo"))),
        column(3, CajaModalUI(ns("kpi_nuevo"))),
        column(3, CajaModalUI(ns("kpi_reactivado")))
      )
    ),
    
    # [3] Bloque de análisis por población ----
    # Se eliminó bloque "Indicadores Dinámicos YTD" global (redundante con el por-población)
    bs4Dash::bs4Card(
      title = tagList(icon("users"), " An\u00e1lisis por Poblaci\u00f3n"),
      width = 12, solidHeader = TRUE, status = "white", collapsible = TRUE,
      
      # Control de población
      fluidRow(
        column(12,
               tags$div(
                 style = paste0(
                   "display:flex; gap:24px; align-items:center; padding:8px 12px; ",
                   "background:#F8FAFC; border-radius:6px; margin-bottom:12px;"
                 ),
                 shinyWidgets::prettyCheckbox(
                   ns("pob_total"),      "Total",           value = TRUE,
                   icon = icon("check"), shape = "round"
                 ),
                 shinyWidgets::prettyCheckbox(
                   ns("pob_presup"),     "Presupuestados",  value = FALSE,
                   icon = icon("check"), shape = "round"
                 ),
                 shinyWidgets::prettyCheckbox(
                   ns("pob_sin_presup"), "Sin Presupuesto", value = FALSE,
                   icon = icon("check"), shape = "round"
                 )
               )
        )
      ), 
      
      .te_seccion("Estado Enero \u2014 Inicio de A\u00f1o", "calendar-alt"),
      h6("Inicio de A\u00f1o \u2014 Enero", style = "font-weight:600; color:#000; margin-bottom:4px;"),
      uiOutput(ns("cajas_enero")),
      
      .te_seccion("Estado Actual \u2014 Mes Vigente", "calendar-check"),
      h6(uiOutput(ns("lbl_mes_vigente")), style = "font-weight:600; color:#000; margin-bottom:4px;"),
      uiOutput(ns("cajas_vigente")),
      
      # Indicadores dinámicos por población (único bloque — no hay global redundante)
      .te_seccion("Indicadores Din\u00e1micos Enero \u2192 Mes Vigente", "arrows-alt-h"),
      uiOutput(ns("cajas_dinamicos")),
      
      hr(),
      
      # Tabla resumen mensual
      .te_seccion("Resumen Mensual por Estado", "table"),
      reactable::reactableOutput(ns("tabla_resumen_mensual")),
      p(icon("hand-pointer"),
        " Haga clic en una celda para ver el detalle de clientes",
        style = "font-size:11px; color:#888; margin-bottom:8px;"),
      
      hr(),
      
    # [4] Ejecución vs Presupuesto ----
      .te_seccion("Ejecuci\u00f3n vs Presupuesto", "bullseye"),
      uiOutput(ns("panel_ejecucion")),
      
      hr(),
      
    # [5] Evolución y Permanencia — ampliado ----
      .te_seccion("Evoluci\u00f3n y Permanencia", "chart-bar"),
      
      fluidRow(
        column(8,
               h6("Evoluci\u00f3n Mensual por Estado",
                  style = "font-weight:600; margin-bottom:4px;"),
               plotly::plotlyOutput(ns("graf_evolucion"), height = "300px")
        ),
        column(4,
               h6("Permanencia por Estado",
                  style = "font-weight:600; margin-bottom:4px;"),
               plotly::plotlyOutput(ns("graf_permanencia"), height = "300px")
        )
      ),
      
      fluidRow(
        column(6,
               h6("Tasa de Facturaci\u00f3n Mensual Promedio",
                  style = "font-weight:600; margin-bottom:4px; margin-top:16px;"),
               plotly::plotlyOutput(ns("graf_tasa_mensual"), height = "260px")
        ),
        column(6,
               h6("Indicadores de Transici\u00f3n Mensuales",
                  style = "font-weight:600; margin-bottom:4px; margin-top:16px;"),
               plotly::plotlyOutput(ns("graf_transicion_mensual"), height = "260px")
        )
      ),
      
      fluidRow(
        column(6,
               h6("Distribuci\u00f3n: D\u00edas hasta Inactivaci\u00f3n",
                  style = "font-weight:600; margin-bottom:4px; margin-top:16px;"),
               plotly::plotlyOutput(ns("graf_dias_venc"), height = "240px")
        ),
        column(6,
               h6("Cohorte de Altas Acumuladas",
                  style = "font-weight:600; margin-bottom:4px; margin-top:16px;"),
               plotly::plotlyOutput(ns("graf_cohorte_altas"), height = "240px")
        )
      )
    )
  )
}


# Server ----
Cohortes <- function(id, data_cohortes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helpers internos ----
    
    # Colores semafóricos para KPIs de tasa
    .color_kpi <- function(v, tipo) {
      if (is.null(v) || is.na(v)) return("#999999")
      switch(tipo,
             retencion    = if (v >= 0.80) "#27AE60" else if (v >= 0.60) "#F4A820" else "#E74C3C",
             perdida      = if (v <= 0.10) "#27AE60" else if (v <= 0.20) "#F4A820" else "#E74C3C",
             reactivacion = if (v >= 0.30) "#27AE60" else if (v >= 0.15) "#F4A820" else "#E74C3C",
             tasa_fact    = if (v >= 0.70) "#27AE60" else if (v >= 0.50) "#F4A820" else "#E74C3C",
             "#999999"
      )
    }
    
    # Reactable simple para modales de alerta (sin TablaReactable2 — contexto de alerta)
    .reactable_uc <- function(data, click_id) {
      reactable::reactable(
        data %>%
          select(any_of(c(
            "PerRazSoc", "Asesor", "Segmento", "estado", "presupuestada", "cliente_id"
          ))) %>%
          distinct(),
        onClick = reactable::JS(sprintf(
          "function(rowInfo) { Shiny.setInputValue('%s',
            {cliente_id: rowInfo.values['cliente_id'], nonce: Math.random()},
            {priority: 'event'}); }",
          click_id
        )),
        columns = list(
          cliente_id    = reactable::colDef(show = FALSE),
          PerRazSoc     = reactable::colDef(name = "Cliente",     minWidth = 200),
          Asesor        = reactable::colDef(name = "Asesor",      minWidth = 120),
          Segmento      = reactable::colDef(name = "Segmento",    minWidth = 100),
          estado        = reactable::colDef(name = "Estado",      minWidth = 150),
          presupuestada = reactable::colDef(name = "Presupuesto", minWidth = 120)
        ),
        highlight = TRUE, compact = TRUE, bordered = TRUE,
        pagination = FALSE, searchable = TRUE
      )
    }
    
    # Handler de descarga Excel
    .dl_handler <- function(datos_fn, prefijo) {
      downloadHandler(
        filename = function() paste0(prefijo, "_", Sys.Date(), ".xlsx"),
        content  = function(file) writexl::write_xlsx(datos_fn(), path = file)
      )
    }
    
    # Convierte etiqueta de población a sufijo de ID válido
    .pob_id <- function(p) gsub("[^A-Za-z0-9]", "_", p)
    
    # Alias reactivos de data_cohortes() ----
    panel_full_r   <- reactive({ data_cohortes()$panel_full })
    panel_p_r      <- reactive({ panel_full_r() %>% filter(presupuestada == "PRESUPUESTADA") })
    panel_np_r     <- reactive({ panel_full_r() %>% filter(presupuestada == "NO PRESUPUESTADA") })
    panel_cumpl    <- reactive({ data_cohortes()$panel_cumpl })
    meses_con_real <- reactive({ data_cohortes()$meses_con_real })
    
    # Indicadores de transición ----
    
    # Helper: calcula retención, pérdida y reactivación para un subpanel
    .calcular_ind <- function(pan) {
      meses <- sort(unique(pan$ym))
      if (length(meses) < 2) return(list(retencion = NA, perdida = NA, reactivacion = NA))
      pares <- tibble(ym_t = head(meses, -1), ym_t1 = tail(meses, -1))
      trans <- pares %>%
        left_join(
          pan %>% select(cliente_id, ym, estado) %>% rename(ym_t = ym, est_t = estado),
          by = "ym_t"
        ) %>%
        left_join(
          pan %>% select(cliente_id, ym, estado) %>% rename(ym_t1 = ym, est_t1 = estado),
          by = c("cliente_id", "ym_t1")
        )
      activos_t   <- trans %>% filter(est_t == "CLIENTE ACTIVO") %>% nrow()
      recuperar_t <- trans %>% filter(est_t == "CLIENTE A RECUPERAR") %>% nrow()
      list(
        retencion = SiError_0(
          trans %>% filter(est_t == "CLIENTE ACTIVO", est_t1 == "CLIENTE ACTIVO") %>%
            nrow() / activos_t
        ),
        perdida = SiError_0(
          trans %>% filter(est_t == "CLIENTE ACTIVO", est_t1 == "CLIENTE A RECUPERAR") %>%
            nrow() / activos_t
        ),
        reactivacion = SiError_0(
          trans %>% filter(est_t == "CLIENTE A RECUPERAR", est_t1 == "CLIENTE ACTIVO") %>%
            nrow() / recuperar_t
        )
      )
    }
    
    # Indicadores por población
    ind_full <- reactive({ req(data_cohortes()); .calcular_ind(panel_full_r()) })
    ind_p    <- reactive({ req(data_cohortes()); .calcular_ind(panel_p_r())    })
    ind_np   <- reactive({ req(data_cohortes()); .calcular_ind(panel_np_r())   })
    
    tasa_fact_global <- reactive({
      req(data_cohortes())
      mean(data_cohortes()$tasa_fact_uc$tasa_facturacion, na.rm = TRUE)
    })
    
    # Tabla resumen mensual ----
    .tabla_resumen <- function(pan) {
      req(pan, data_cohortes())
      meses   <- data_cohortes()$meses_periodo
      conteos <- pan %>%
        group_by(estado, ym) %>%
        summarise(n = n_distinct(cliente_id), .groups = "drop")
      totales <- conteos %>%
        group_by(ym) %>%
        summarise(n = sum(n), .groups = "drop") %>%
        mutate(estado = "TOTAL")
      bind_rows(conteos, totales) %>%
        mutate(
          estado = factor(estado, levels = c(
            "CLIENTE ACTIVO", "CLIENTE A RECUPERAR", "NUEVO DEL PERIODO", "TOTAL"
          )),
          mes_lbl = format(ym, "%b-%y")
        ) %>%
        select(estado, mes_lbl, n) %>%
        tidyr::pivot_wider(names_from = mes_lbl, values_from = n, values_fill = 0L) %>%
        arrange(estado)
    }
    
    # Población activa ----
    poblacion_activa <- reactive({
      pobs <- c()
      if (isTRUE(input$pob_total))      pobs <- c(pobs, "TOTAL")
      if (isTRUE(input$pob_presup))     pobs <- c(pobs, "PRESUPUESTADA")
      if (isTRUE(input$pob_sin_presup)) pobs <- c(pobs, "NO PRESUPUESTADA")
      if (length(pobs) == 0) pobs <- "TOTAL"
      pobs
    })
    
    panel_activo <- reactive({
      req(data_cohortes())
      pf <- panel_full_r()
      if ("TOTAL" %in% poblacion_activa()) return(pf)
      pf %>% filter(presupuestada %in% poblacion_activa())
    })
    
    # Pre-registro de IDs Plotly
    observe({
      ids_plotly <- c(
        ns("graf_evolucion"), ns("graf_permanencia"),
        ns("graf_tasa_mensual"), ns("graf_transicion_mensual"),
        ns("graf_dias_venc"), ns("graf_cohorte_altas")
      )
      session$userData$.plotlyShinyEventIDs <- union(
        session$userData$.plotlyShinyEventIDs %||% character(0),
        ids_plotly
      )
    })
    
    # Outputs ----
    
    ## [1] Resumen general: conteos del universo completo ----
    # Corte en el mes vigente sobre panel_full (sin filtro de población)
    .conteo_full_vigente <- reactive({
      req(data_cohortes())
      dat <- data_cohortes()
      corte <- panel_full_r() %>% filter(ym == max(ym))
      list(
        activos   = n_distinct(corte$cliente_id[corte$estado == "CLIENTE ACTIVO"]),
        recuperar = n_distinct(corte$cliente_id[corte$estado == "CLIENTE A RECUPERAR"]),
        nuevos    = n_distinct(
          panel_full_r()$cliente_id[panel_full_r()$tipo_cohorte == "ALTA EN COHORTE"]
        )
      )
    })
    
    # Tablas para modales de resumen general
    output$tbl_resumen_activos <- reactable::renderReactable({
      req(data_cohortes())
      corte <- panel_full_r() %>% filter(ym == max(ym), estado == "CLIENTE ACTIVO")
      .reactable_uc(corte %>% mutate(estado = "ACTIVO"), ns("click_gen_activo"))
    })
    output$tbl_resumen_recuperar <- reactable::renderReactable({
      req(data_cohortes())
      corte <- panel_full_r() %>%
        filter(ym == max(ym), estado == "CLIENTE A RECUPERAR")
      .reactable_uc(corte %>% mutate(estado = "A RECUPERAR"), ns("click_gen_recuperar"))
    })
    output$tbl_resumen_nuevos <- reactable::renderReactable({
      req(data_cohortes())
      nuevos <- panel_full_r() %>%
        filter(tipo_cohorte == "ALTA EN COHORTE") %>%
        distinct(cliente_id, .keep_all = TRUE)
      .reactable_uc(nuevos %>% mutate(estado = "NUEVO"), ns("click_gen_nuevo"))
    })
    
    CajaModal(
      "kpi_total_activos",
      valor = reactive(html_valor(.conteo_full_vigente()$activos, "numero")),
      texto = "Activos (Universo)",
      icono = "user-check", colores = c(fondo = "white"),
      mostrar_boton = TRUE,
      titulo_modal    = "Clientes Activos \u2014 Universo Completo",
      icono_modal     = "user-check",
      contenido_modal = function() reactable::reactableOutput(ns("tbl_resumen_activos")),
      footer          = reactive("Total activos en el mes vigente — todas las poblaciones"),
      footer_class    = "caja-modal-footer"
    )
    CajaModal(
      "kpi_total_recuperar",
      valor = reactive(html_valor(.conteo_full_vigente()$recuperar, "numero")),
      texto = "A Recuperar (Universo)",
      icono = "user-clock", colores = c(fondo = "white"),
      mostrar_boton = TRUE,
      titulo_modal    = "Clientes A Recuperar \u2014 Universo Completo",
      icono_modal     = "user-clock",
      contenido_modal = function() reactable::reactableOutput(ns("tbl_resumen_recuperar")),
      footer          = reactive("Total inactivos en el mes vigente — todas las poblaciones"),
      footer_class    = "caja-modal-footer"
    )
    CajaModal(
      "kpi_total_nuevos",
      valor = reactive(html_valor(.conteo_full_vigente()$nuevos, "numero")),
      texto = "Nuevos YTD (Universo)",
      icono = "user-plus", colores = c(fondo = "white"),
      mostrar_boton = TRUE,
      titulo_modal    = "Altas en Cohorte YTD \u2014 Universo Completo",
      icono_modal     = "user-plus",
      contenido_modal = function() reactable::reactableOutput(ns("tbl_resumen_nuevos")),
      footer          = reactive("Clientes que ingresaron al panel en el año en curso"),
      footer_class    = "caja-modal-footer"
    )
    
    ## [2] Alertas del mes vigente ----
    
    # [1-fix] ACTIVO_A_INACTIVO: clientes activos que se inactivarían si no compran este mes
    # Lógica: SegmentoRacafe == "CLIENTE" && EstadoProyectado == "CLIENTE A RECUPERAR"
    # Esto ya existe en data_cohortes()$transiciones con Transicion == "ACTIVO_A_INACTIVO"
    output$tbl_a_inactivo <- reactable::renderReactable({
      req(data_cohortes())
      .reactable_uc(
        data_cohortes()$transiciones %>%
          filter(Transicion == "ACTIVO_A_INACTIVO") %>%
          mutate(estado = Transicion),
        ns("click_alerta_inactivo")
      )
    })
    CajaModal(
      "kpi_a_inactivo",
      valor = reactive(html_valor(
        data_cohortes()$transiciones %>% filter(Transicion == "ACTIVO_A_INACTIVO") %>% nrow(),
        formato = "numero", color = "#E74C3C"
      )),
      texto           = html_texto("Se Inactivar\u00edan Este Mes", color = "#E74C3C"),
      icono           = "user-times",
      colores         = c(fondo = "white"),
      color_fondo_hex = "#FADBD8",
      mostrar_boton   = TRUE,
      titulo_modal    = "Detalle \u2014 Clientes en Riesgo de Inactivarse",
      icono_modal     = "user-times",
      contenido_modal = function() reactable::reactableOutput(ns("tbl_a_inactivo")),
      footer          = reactive(
        "Activos clasificados como CLIENTE que dejar\u00edan de serlo si no facturan este mes"
      ),
      footer_class    = "caja-modal-footer"
    )
    
    output$tbl_a_activo <- reactable::renderReactable({
      req(data_cohortes())
      .reactable_uc(
        data_cohortes()$transiciones %>%
          filter(Transicion == "INACTIVO_A_ACTIVO") %>%
          mutate(estado = Transicion),
        ns("click_alerta_activo")
      )
    })
    CajaModal(
      "kpi_a_activo",
      valor = reactive(html_valor(
        data_cohortes()$transiciones %>% filter(Transicion == "INACTIVO_A_ACTIVO") %>% nrow(),
        formato = "numero", color = "#27AE60"
      )),
      texto           = html_texto("Se Van a Recuperar", color = "#27AE60"),
      icono           = "user-check",
      colores         = c(fondo = "white"),
      color_fondo_hex = "#D5F5E3",
      mostrar_boton   = TRUE,
      titulo_modal    = "Detalle \u2014 Clientes en Recuperaci\u00f3n",
      icono_modal     = "user-check",
      contenido_modal = function() reactable::reactableOutput(ns("tbl_a_activo")),
      footer          = reactive("Inactivos que facturaron dentro de su ventana este mes"),
      footer_class    = "caja-modal-footer"
    )
    
    output$tbl_nuevo <- reactable::renderReactable({
      req(data_cohortes())
      .reactable_uc(
        data_cohortes()$transiciones %>%
          filter(Transicion == "NUEVO_ABSOLUTO") %>%
          mutate(estado = Transicion),
        ns("click_alerta_nuevo")
      )
    })
    CajaModal(
      "kpi_nuevo",
      valor = reactive(html_valor(
        data_cohortes()$transiciones %>% filter(Transicion == "NUEVO_ABSOLUTO") %>% nrow(),
        formato = "numero", color = "#2C7BB6"
      )),
      texto           = html_texto("Clientes Nuevos", color = "#2C7BB6"),
      icono           = "user-plus",
      colores         = c(fondo = "white"),
      color_fondo_hex = "#D6EAF8",
      mostrar_boton   = TRUE,
      titulo_modal    = "Detalle \u2014 Clientes Nuevos Absolutos",
      icono_modal     = "user-plus",
      contenido_modal = function() reactable::reactableOutput(ns("tbl_nuevo")),
      footer          = reactive(
        "Creados en NCLIENTE en los \u00faltimos 2 meses, primera factura este mes"
      ),
      footer_class    = "caja-modal-footer"
    )
    
    output$tbl_reactivado <- reactable::renderReactable({
      req(data_cohortes())
      .reactable_uc(
        data_cohortes()$transiciones %>%
          filter(Transicion == "REACTIVADO_SIN_CRM") %>%
          mutate(estado = Transicion),
        ns("click_alerta_reactivado")
      )
    })
    CajaModal(
      "kpi_reactivado",
      valor = reactive(html_valor(
        data_cohortes()$transiciones %>%
          filter(Transicion == "REACTIVADO_SIN_CRM") %>% nrow(),
        formato = "numero", color = "#F4A820"
      )),
      texto           = html_texto("Reactivados sin CRM", color = "#F4A820"),
      icono           = "user-clock",
      colores         = c(fondo = "white"),
      color_fondo_hex = "#FEF9E7",
      mostrar_boton   = TRUE,
      titulo_modal    = "Detalle \u2014 Reactivados sin Historial CRM",
      icono_modal     = "user-clock",
      contenido_modal = function() reactable::reactableOutput(ns("tbl_reactivado")),
      footer          = reactive(
        "Facturaron este mes; existen en maestro pero sin clasificaci\u00f3n CRM"
      ),
      footer_class    = "caja-modal-footer"
    )
    
    ## [3] Cajas por población ----
    # Bloque global de indicadores YTD eliminado — reemplazado por las cajas por-población
    
    output$lbl_mes_vigente <- renderUI({
      req(data_cohortes())
      paste0("Mes Vigente \u2014 ", format(data_cohortes()$mes_vigente, "%B %Y"))
    })
    
    # Fuente de valores para CajaModal por población
    .vals_poblacion <- function(pan_r, ind_r) {
      list(
        enero = reactive({
          req(pan_r(), data_cohortes())
          dat   <- data_cohortes()
          corte <- pan_r() %>% filter(ym == dat$mes_inicio)
          list(
            activo    = n_distinct(corte$cliente_id[corte$estado == "CLIENTE ACTIVO"]),
            recuperar = n_distinct(corte$cliente_id[corte$estado == "CLIENTE A RECUPERAR"]),
            nuevos    = n_distinct(corte$cliente_id[corte$tipo_cohorte == "ALTA EN COHORTE"]),
            tasa      = mean(
              dat$tasa_fact_uc %>%
                semi_join(corte, by = "cliente_id") %>%
                pull(tasa_facturacion),
              na.rm = TRUE
            )
          )
        }),
        vigente = reactive({
          req(pan_r(), data_cohortes())
          dat   <- data_cohortes()
          corte <- pan_r() %>% filter(ym == max(pan_r()$ym))
          list(
            activo    = n_distinct(corte$cliente_id[corte$estado == "CLIENTE ACTIVO"]),
            recuperar = n_distinct(corte$cliente_id[corte$estado == "CLIENTE A RECUPERAR"]),
            nuevos    = n_distinct(corte$cliente_id[corte$tipo_cohorte == "ALTA EN COHORTE"]),
            tasa      = mean(
              dat$tasa_fact_uc %>%
                semi_join(corte, by = "cliente_id") %>%
                pull(tasa_facturacion),
              na.rm = TRUE
            )
          )
        }),
        ind = ind_r
      )
    }
    
    vals_pob <- list(
      TOTAL            = .vals_poblacion(panel_full_r, ind_full),
      PRESUPUESTADA    = .vals_poblacion(panel_p_r,    ind_p),
      NO_PRESUPUESTADA = .vals_poblacion(panel_np_r,   ind_np)
    )
    
    # Pre-registro de CajaModal para las tres poblaciones
    for (pop_lbl in c("TOTAL", "PRESUPUESTADA", "NO PRESUPUESTADA")) {
      local({
        p   <- pop_lbl
        pid <- .pob_id(p)
        vp  <- vals_pob[[pid]]
        
        CajaModal(paste0("kpi_enero_activo_",    pid),
                  valor = reactive(html_valor(vp$enero()$activo,    "numero")),
                  texto = "Activos Enero",        icono = "user-check",
                  colores = c(fondo = "white"),
                  mostrar_boton   = TRUE,
                  titulo_modal    = paste0("Activos Enero — ", p),
                  icono_modal     = "user-check",
                  contenido_modal = function() reactable::reactableOutput(
                    ns(paste0("tbl_enero_activo_", pid))
                  ))
        CajaModal(paste0("kpi_enero_recuperar_", pid),
                  valor = reactive(html_valor(vp$enero()$recuperar, "numero")),
                  texto = "A Recuperar Enero",    icono = "user-clock",
                  colores = c(fondo = "white"),
                  mostrar_boton   = TRUE,
                  titulo_modal    = paste0("A Recuperar Enero — ", p),
                  icono_modal     = "user-clock",
                  contenido_modal = function() reactable::reactableOutput(
                    ns(paste0("tbl_enero_recuperar_", pid))
                  ))
        CajaModal(paste0("kpi_enero_nuevos_",    pid),
                  valor = reactive(html_valor(vp$enero()$nuevos,    "numero")),
                  texto = "Nuevos Enero",         icono = "user-plus",
                  colores = c(fondo = "white"),
                  mostrar_boton   = TRUE,
                  titulo_modal    = paste0("Nuevos Enero — ", p),
                  icono_modal     = "user-plus",
                  contenido_modal = function() reactable::reactableOutput(
                    ns(paste0("tbl_enero_nuevos_", pid))
                  ))
        CajaModal(paste0("kpi_enero_tasa_",      pid),
                  valor = reactive(html_valor(
                    vp$enero()$tasa %||% 0, "porcentaje",
                    color = .color_kpi(vp$enero()$tasa, "tasa_fact")
                  )),
                  texto = "Tasa Facturaci\u00f3n", icono = "percentage",
                  colores = c(fondo = "white"),    mostrar_boton = FALSE)
        
        CajaModal(paste0("kpi_vig_activo_",    pid),
                  valor = reactive(html_valor(vp$vigente()$activo,    "numero")),
                  texto = "Activos",              icono = "user-check",
                  colores = c(fondo = "white"),   mostrar_boton = FALSE)
        CajaModal(paste0("kpi_vig_recuperar_", pid),
                  valor = reactive(html_valor(vp$vigente()$recuperar, "numero")),
                  texto = "A Recuperar",          icono = "user-clock",
                  colores = c(fondo = "white"),   mostrar_boton = FALSE)
        CajaModal(paste0("kpi_vig_nuevos_",    pid),
                  valor = reactive(html_valor(vp$vigente()$nuevos,    "numero")),
                  texto = "Nuevos",               icono = "user-plus",
                  colores = c(fondo = "white"),   mostrar_boton = FALSE)
        CajaModal(paste0("kpi_vig_tasa_",      pid),
                  valor = reactive(html_valor(
                    vp$vigente()$tasa %||% 0, "porcentaje",
                    color = .color_kpi(vp$vigente()$tasa, "tasa_fact")
                  )),
                  texto = "Tasa Facturaci\u00f3n", icono = "percentage",
                  colores = c(fondo = "white"),    mostrar_boton = FALSE)
        
        CajaModal(paste0("kpi_din_ret_", pid),
                  valor = reactive(html_valor(
                    vp$ind()$retencion %||% 0, "porcentaje",
                    color = .color_kpi(vp$ind()$retencion, "retencion")
                  )),
                  texto = "Retenci\u00f3n", icono = "shield-alt",
                  colores = c(fondo = "white"), mostrar_boton = FALSE)
        CajaModal(paste0("kpi_din_per_", pid),
                  valor = reactive(html_valor(
                    vp$ind()$perdida %||% 0, "porcentaje",
                    color = .color_kpi(vp$ind()$perdida, "perdida")
                  )),
                  texto = "P\u00e9rdida", icono = "user-minus",
                  colores = c(fondo = "white"), mostrar_boton = FALSE)
        CajaModal(paste0("kpi_din_rea_", pid),
                  valor = reactive(html_valor(
                    vp$ind()$reactivacion %||% 0, "porcentaje",
                    color = .color_kpi(vp$ind()$reactivacion, "reactivacion")
                  )),
                  texto = "Reactivaci\u00f3n", icono = "sync-alt",
                  colores = c(fondo = "white"), mostrar_boton = FALSE)
      })
    }
    
    # Tablas de detalle para modales de Estado Enero — una por población × estado
    # Se registran fuera del loop for para evitar problemas de scope con local()
    for (pop_lbl in c("TOTAL", "PRESUPUESTADA", "NO PRESUPUESTADA")) {
      local({
        p   <- pop_lbl
        pid <- .pob_id(p)
        
        # Datos del corte enero para esta población
        pan_enero_r <- reactive({
          req(data_cohortes())
          dat   <- data_cohortes()
          pan_r <- switch(pid,
                          TOTAL            = panel_full_r(),
                          PRESUPUESTADA    = panel_p_r(),
                          NO_PRESUPUESTADA = panel_np_r()
          )
          pan_r %>% filter(ym == dat$mes_inicio)
        })
        
        output[[paste0("tbl_enero_activo_",    pid)]] <- reactable::renderReactable({
          req(pan_enero_r())
          .reactable_uc(
            pan_enero_r() %>% filter(estado == "CLIENTE ACTIVO"),
            ns(paste0("click_enero_activo_", pid))
          )
        })
        output[[paste0("tbl_enero_recuperar_", pid)]] <- reactable::renderReactable({
          req(pan_enero_r())
          .reactable_uc(
            pan_enero_r() %>% filter(estado == "CLIENTE A RECUPERAR"),
            ns(paste0("click_enero_recuperar_", pid))
          )
        })
        output[[paste0("tbl_enero_nuevos_",    pid)]] <- reactable::renderReactable({
          req(pan_enero_r())
          .reactable_uc(
            pan_enero_r() %>% filter(tipo_cohorte == "ALTA EN COHORTE"),
            ns(paste0("click_enero_nuevos_", pid))
          )
        })
      })
    }
    
    # Renderiza filas de cajas según checkboxes activos
    output$cajas_enero <- renderUI({
      pobs <- poblacion_activa()
      cols <- lapply(pobs, function(p) {
        pid <- .pob_id(p)
        column(
          12 / length(pobs),
          h6(p, style = "font-weight:700; color:#000; text-align:center;"),
          fluidRow(
            column(3, CajaModalUI(ns(paste0("kpi_enero_activo_",    pid)))),
            column(3, CajaModalUI(ns(paste0("kpi_enero_recuperar_", pid)))),
            column(3, CajaModalUI(ns(paste0("kpi_enero_nuevos_",    pid)))),
            column(3, CajaModalUI(ns(paste0("kpi_enero_tasa_",      pid))))
          )
        )
      })
      do.call(fluidRow, cols)
    })
    
    output$cajas_vigente <- renderUI({
      pobs <- poblacion_activa()
      cols <- lapply(pobs, function(p) {
        pid <- .pob_id(p)
        column(
          12 / length(pobs),
          h6(p, style = "font-weight:700; color:#000; text-align:center;"),
          fluidRow(
            column(3, CajaModalUI(ns(paste0("kpi_vig_activo_",    pid)))),
            column(3, CajaModalUI(ns(paste0("kpi_vig_recuperar_", pid)))),
            column(3, CajaModalUI(ns(paste0("kpi_vig_nuevos_",    pid)))),
            column(3, CajaModalUI(ns(paste0("kpi_vig_tasa_",      pid))))
          )
        )
      })
      do.call(fluidRow, cols)
    })
    
    output$cajas_dinamicos <- renderUI({
      pobs <- poblacion_activa()
      cols <- lapply(pobs, function(p) {
        pid <- .pob_id(p)
        column(
          12 / length(pobs),
          h6(p, style = "font-weight:700; color:#555; text-align:center;"),
          fluidRow(
            column(4, CajaModalUI(ns(paste0("kpi_din_ret_", pid)))),
            column(4, CajaModalUI(ns(paste0("kpi_din_per_", pid)))),
            column(4, CajaModalUI(ns(paste0("kpi_din_rea_", pid))))
          ),
          fluidRow(
            column(4, .te_nota("Activos t \u2192 Activos t+1 / Total Activos en t")),
            column(4, .te_nota("Activos t \u2192 A Recuperar t+1 / Total Activos en t")),
            column(4, .te_nota(
              "A Recuperar t \u2192 Activos t+1 / Total A Recuperar en t"
            ))
          )
        )
      })
      do.call(fluidRow, cols)
    })
    
    ## Tabla resumen mensual ----
    # renderReactable directo: columnas dinámicas por mes + onclick JS custom.
    # TablaReactable2 no se usa aquí porque requeriría re-registrar el módulo
    # server cada vez que cambia panel_activo() — patrón incorrecto que genera
    # múltiples instancias acumuladas y re-renders en cascada.
    output$tabla_resumen_mensual <- reactable::renderReactable({
      req(panel_activo())
      tab      <- .tabla_resumen(panel_activo())
      click_id <- ns("click_celda_resumen")
      cols_mes <- setdiff(names(tab), "estado")
      col_mes  <- lapply(cols_mes, function(m) {
        reactable::colDef(
          name = m, minWidth = 70,
          cell = function(value, index) {
            estado_fila <- tab$estado[[index]]
            tags$span(
              style   = "cursor:pointer; font-weight:600;",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {estado: '%s', mes: '%s',
                 nonce: Math.random()}, {priority: 'event'})",
                click_id, estado_fila, m
              ),
              format(value, big.mark = ".")
            )
          }
        )
      })
      names(col_mes) <- cols_mes
      reactable::reactable(
        tab,
        columns = c(
          list(estado = reactable::colDef(
            name = "Estado", minWidth = 180, sticky = "left"
          )),
          col_mes
        ),
        highlight = TRUE, compact = TRUE, bordered = TRUE, pagination = FALSE
      )
    })
    
    # [6] Modal de detalle de clientes — TablaReactable2 con campos completos ----
    # Construye el dataframe enriquecido para drill-down desde tabla resumen.
    # Fuente de presupuesto: marca_presupuesto (data_cohortes()$panel_cumpl puede
    # tener estructuras variables según versión del módulo empaquetador).
    # El ppto mensual se deriva siempre como ppto_anual/12 aquí para garantizar
    # consistencia, y el YTD como ppto_anual/12 * n_meses_ytd.
    .data_drill_enriquecida <- function(clientes_mes, fecha_sel) {
      dat <- data_cohortes()
      
      # Número de meses transcurridos desde inicio de año hasta fecha_sel (inclusive)
      # Aritmética directa: evita dependencia de lubridate::interval enmascarado
      n_meses_ytd <- (
        as.integer(format(fecha_sel,      "%Y")) * 12L +
          as.integer(format(fecha_sel,      "%m")) -
          as.integer(format(dat$mes_inicio, "%Y")) * 12L -
          as.integer(format(dat$mes_inicio, "%m"))
      ) + 1L
      
      # Métricas reales del mes seleccionado
      real_mes <- dat$real_mensual %>%
        filter(ym == fecha_sel) %>%
        semi_join(clientes_mes, by = "cliente_id") %>%
        select(cliente_id, sacos_mes = real_sacos, margen_mes = real_margen)
      
      # Métricas reales YTD: acumulado desde inicio de año hasta mes seleccionado
      real_ytd <- dat$real_mensual %>%
        filter(ym >= dat$mes_inicio, ym <= fecha_sel) %>%
        semi_join(clientes_mes, by = "cliente_id") %>%
        group_by(cliente_id) %>%
        summarise(
          sacos_ytd  = sum(real_sacos,  na.rm = TRUE),
          margen_ytd = sum(real_margen, na.rm = TRUE),
          .groups    = "drop"
        )
      
      # Presupuesto: fuente directa CRMNALCLIENTE — una fila por (CliNitPpal, LinNegCod).
      # Se evita panel_cumpl y marca_presupuesto porque ambos agregan SSPpto/MNFCCPpto
      # sobre tx_limpia (múltiples filas por cliente en enero) duplicando el valor anual.
      # SSPpto y MNFCCPpto en CRMNALCLIENTE son valores anuales → se divide por 12.
      ppto <- tryCatch({
        CargarDatos("CRMNALCLIENTE") %>%
          mutate(
            FecProceso = as.Date(FecProceso),
            cliente_id = paste(CliNitPpal, LinNegCod, sep = "_")
          ) %>%
          semi_join(clientes_mes, by = "cliente_id") %>%
          group_by(cliente_id) %>%
          filter(FecProceso == max(FecProceso)) %>%
          slice(1L) %>%
          ungroup() %>%
          transmute(
            cliente_id,
            ppto_sacos_mes  = coalesce(SSPpto,    0) / 12,
            ppto_margen_mes = coalesce(MNFCCPpto, 0) / 12,
            ppto_sacos_ytd  = ppto_sacos_mes  * n_meses_ytd,
            ppto_margen_ytd = ppto_margen_mes * n_meses_ytd
          )
      }, error = function(e) {
        tibble(
          cliente_id      = character(0),
          ppto_sacos_mes  = numeric(0), ppto_margen_mes  = numeric(0),
          ppto_sacos_ytd  = numeric(0), ppto_margen_ytd  = numeric(0)
        )
      })
      
      # Última facturación: fecha exacta desde tx_limpia (FecFact real, no ym).
      # real_mensual agrega a primer día del mes → siempre devuelve día 1; incorrecto.
      ultima_fact <- dat$tx_limpia %>%
        filter(!is.na(FecFact), ym >= dat$mes_inicio, ym <= fecha_sel) %>%
        semi_join(clientes_mes, by = "cliente_id") %>%
        group_by(cliente_id) %>%
        summarise(ultima_fec_fact = max(FecFact, na.rm = TRUE), .groups = "drop")
      
      # Join final — NA en ppto = sin presupuesto asignado (cliente no presupuestado)
      clientes_mes %>%
        left_join(real_mes,    by = "cliente_id") %>%
        left_join(real_ytd,    by = "cliente_id") %>%
        left_join(ppto,        by = "cliente_id") %>%
        left_join(ultima_fact, by = "cliente_id") %>%
        mutate(
          sacos_mes  = coalesce(sacos_mes,  0),
          margen_mes = coalesce(margen_mes, 0),
          sacos_ytd  = coalesce(sacos_ytd,  0),
          margen_ytd = coalesce(margen_ytd, 0)
        ) %>%
        select(
          LinNegCod, CliNitPpal, PerRazSoc, Segmento, Asesor, estado,
          ultima_fec_fact,
          sacos_mes,      ppto_sacos_mes,
          sacos_ytd,      ppto_sacos_ytd,
          margen_mes,     ppto_margen_mes,
          margen_ytd,     ppto_margen_ytd
        )
    }
    
    # Versión contador para recrear TablaReactable2 por cada apertura de modal
    .drill_ver <- reactiveVal(0L)
    
    observeEvent(input$click_celda_resumen, {
      req(!is.null(input$click_celda_resumen$estado))
      estado_sel <- input$click_celda_resumen$estado
      mes_sel    <- input$click_celda_resumen$mes
      fecha_sel  <- tryCatch(
        as.Date(paste0("01-", mes_sel), format = "%d-%b-%y"),
        error = function(e) NULL
      )
      req(!is.null(fecha_sel))
      
      clientes_mes <- panel_activo() %>%
        filter(ym == fecha_sel) %>%
        { if (estado_sel == "TOTAL") . else filter(., estado == estado_sel) } %>%
        select(cliente_id, CliNitPpal, LinNegCod, PerRazSoc, Asesor, Segmento, estado)
      
      req(nrow(clientes_mes) > 0)
      
      # Incrementar versión para obtener ID único de TablaReactable2
      .drill_ver(.drill_ver() + 1L)
      ver        <- isolate(.drill_ver())
      tbl_id     <- paste0("drill_tbl_", ver)
      data_drill <- .data_drill_enriquecida(clientes_mes, fecha_sel)
      
      # Montar server de TablaReactable2 antes de mostrar el modal
      racafeModulos::TablaReactable2(
        id             = tbl_id,
        data           = reactive(data_drill),
        titulo         = NULL,
        subtitulo      = NULL,
        footer         = paste0(
          "Corte: ", mes_sel, " \u2014 Estado: ", estado_sel,
          " \u2014 ", nrow(data_drill), " unidades comerciales"
        ),
        footer_tipo    = "info",
        col_labels     = c(
          LinNegCod       = "L\u00ednea",
          CliNitPpal      = "NIT",
          PerRazSoc       = "Raz\u00f3n Social",
          Segmento        = "Segmento",
          Asesor          = "Asesor",
          estado          = "Estado",
          ultima_fec_fact = "\u00dalt. Facturaci\u00f3n",
          sacos_mes       = "Sacos Mes",
          ppto_sacos_mes  = "Ppto Sacos Mes",
          sacos_ytd       = "Sacos YTD",
          ppto_sacos_ytd  = "Ppto Sacos YTD",
          margen_mes      = "Margen Mes",
          ppto_margen_mes = "Ppto Margen Mes",
          margen_ytd      = "Margen YTD",
          ppto_margen_ytd = "Ppto Margen YTD"
        ),
        col_specs      = list(
          LinNegCod       = reactable::colDef(minWidth = 75),
          CliNitPpal      = reactable::colDef(minWidth = 110),
          PerRazSoc       = reactable::colDef(minWidth = 200, sticky = "left"),
          Segmento        = reactable::colDef(minWidth = 90),
          Asesor          = reactable::colDef(minWidth = 120),
          estado          = reactable::colDef(minWidth = 140),
          ultima_fec_fact = reactable::colDef(
            minWidth = 120,
            format   = reactable::colFormat(date = TRUE, locales = "es-CO")
          ),
          sacos_mes       = reactable::colDef(
            minWidth = 90,
            format   = reactable::colFormat(separators = TRUE, digits = 1)
          ),
          ppto_sacos_mes  = reactable::colDef(
            minWidth = 90,
            format   = reactable::colFormat(separators = TRUE, digits = 1)
          ),
          sacos_ytd       = reactable::colDef(
            minWidth = 90,
            format   = reactable::colFormat(separators = TRUE, digits = 1)
          ),
          ppto_sacos_ytd  = reactable::colDef(
            minWidth = 90,
            format   = reactable::colFormat(separators = TRUE, digits = 1)
          ),
          margen_mes      = reactable::colDef(
            minWidth = 110,
            format   = reactable::colFormat(separators = TRUE, digits = 0, prefix = "$")
          ),
          ppto_margen_mes = reactable::colDef(
            minWidth = 110,
            format   = reactable::colFormat(separators = TRUE, digits = 0, prefix = "$")
          ),
          margen_ytd      = reactable::colDef(
            minWidth = 110,
            format   = reactable::colFormat(separators = TRUE, digits = 0, prefix = "$")
          ),
          ppto_margen_ytd = reactable::colDef(
            minWidth = 110,
            format   = reactable::colFormat(separators = TRUE, digits = 0, prefix = "$")
          )
        ),
        modo_seleccion = "ninguno",
        id_col         = NULL,
        col_header_n   = 3L,
        sortable       = TRUE,
        searchable     = TRUE,
        page_size      = 999L,    # Sin paginación efectiva
        compact        = TRUE,
        mostrar_badge  = FALSE,
        mostrar_nota   = FALSE,
        cols_heatmap   = c("sacos_mes", "sacos_ytd"),
        cols_valor_color   = c("margen_mes", "margen_ytd"),
        umbral_valor_color = 0
      )
      
      showModal(modalDialog(
        title     = tagList(icon("users"), " ", estado_sel, " \u2014 ", mes_sel),
        size      = "xl", easyClose = TRUE, footer = modalButton("Cerrar"),
        racafeModulos::TablaReactable2UI(ns(tbl_id), estilo = "minimal")
      ))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    ## [4] Ejecución vs Presupuesto ----
    # Agrega real y presupuesto por estado (activo / a recuperar / nuevos)
    # para el corte del mes vigente y YTD; resalta brecha de presupuesto no ejecutado
    # por inactividad (clientes A Recuperar con presupuesto asignado)
    ejecucion_estado_r <- reactive({
      req(data_cohortes())
      dat     <- data_cohortes()
      mes_vig <- max(panel_full_r()$ym)
      
      # Estado vigente por cliente — de panel_full con garantía de columna existente
      estado_vig <- panel_full_r() %>%
        filter(ym == mes_vig) %>%
        select(cliente_id, estado) %>%
        distinct(cliente_id, .keep_all = TRUE)
      
      # Real del mes vigente desde real_mensual (fuente limpia, sin duplicados)
      real_vig <- dat$real_mensual %>%
        filter(ym == mes_vig) %>%
        select(cliente_id, real_sacos, real_margen)
      
      # Presupuesto: fuente directa CRMNALCLIENTE — snapshot más reciente por UC.
      # SSPpto y MNFCCPpto son anuales → se divide por 12 para el mes.
      # Se filtran solo los clientes presupuestados del panel vigente.
      ids_presup <- panel_full_r() %>%
        filter(ym == mes_vig, presupuestada == "PRESUPUESTADA") %>%
        distinct(cliente_id)
      
      ppto_vig <- tryCatch({
        CargarDatos("CRMNALCLIENTE") %>%
          mutate(
            FecProceso = as.Date(FecProceso),
            cliente_id = paste(CliNitPpal, LinNegCod, sep = "_")
          ) %>%
          semi_join(ids_presup, by = "cliente_id") %>%
          group_by(cliente_id) %>%
          filter(FecProceso == max(FecProceso)) %>%
          slice(1L) %>%
          ungroup() %>%
          transmute(
            cliente_id,
            ppto_sacos_mes  = coalesce(SSPpto,    0) / 12,
            ppto_margen_mes = coalesce(MNFCCPpto, 0) / 12
          )
      }, error = function(e) {
        tibble(cliente_id = character(0),
               ppto_sacos_mes = numeric(0), ppto_margen_mes = numeric(0))
      })
      
      # Join: todos los clientes presupuestados del panel vigente + real + estado
      ids_presup %>%
        left_join(estado_vig, by = "cliente_id") %>%
        left_join(real_vig,   by = "cliente_id") %>%
        left_join(ppto_vig,   by = "cliente_id") %>%
        mutate(
          real_sacos      = coalesce(real_sacos,  0),
          real_margen     = coalesce(real_margen, 0),
          ppto_sacos_mes  = coalesce(ppto_sacos_mes,  0),
          ppto_margen_mes = coalesce(ppto_margen_mes, 0),
          estado          = coalesce(estado, "Sin Estado"),
          estado_grp      = case_when(
            estado == "CLIENTE ACTIVO"       ~ "Activo",
            estado == "CLIENTE A RECUPERAR"  ~ "A Recuperar",
            estado == "NUEVO DEL PERIODO"    ~ "Nuevo",
            TRUE                             ~ "Otro"
          )
        )
    })
    
    # Resumen agregado por estado para visualización
    resumen_ejec_r <- reactive({
      req(ejecucion_estado_r())
      ejecucion_estado_r() %>%
        group_by(estado_grp) %>%
        summarise(
          ppto_sacos   = sum(ppto_sacos_mes,  na.rm = TRUE),
          real_sacos   = sum(real_sacos,       na.rm = TRUE),
          ppto_margen  = sum(ppto_margen_mes,  na.rm = TRUE),
          real_margen  = sum(real_margen,      na.rm = TRUE),
          n_uc         = n_distinct(cliente_id),
          .groups      = "drop"
        ) %>%
        mutate(
          brecha_sacos  = ppto_sacos  - real_sacos,
          brecha_margen = ppto_margen - real_margen,
          cumpl_sacos   = SiError_0(real_sacos  / ppto_sacos),
          cumpl_margen  = SiError_0(real_margen / ppto_margen)
        ) %>%
        arrange(factor(
          estado_grp, levels = c("Activo", "A Recuperar", "Nuevo", "Otro")
        ))
    })
    
    output$panel_ejecucion <- renderUI({
      req(resumen_ejec_r())
      df <- resumen_ejec_r()
      tagList(
        fluidRow(
          # Cajas de KPI de cumplimiento por estado — sacos
          lapply(seq_len(nrow(df)), function(i) {
            row  <- df[i, ]
            clr  <- .color_kpi(row$cumpl_sacos, "retencion")
            column(
              max(3L, 12L %/% nrow(df)),
              tags$div(
                style = paste0(
                  "background:#F8FAFC; border-radius:8px; padding:12px; ",
                  "border-left:4px solid ", clr, "; margin-bottom:8px;"
                ),
                tags$p(
                  style = "font-size:11px; font-weight:700; color:#374151; margin:0 0 4px 0;",
                  toupper(row$estado_grp)
                ),
                tags$div(
                  style = "display:flex; gap:16px; flex-wrap:wrap;",
                  tags$span(
                    style = "font-size:11px; color:#555;",
                    icon("box"), " Sacos: ",
                    tags$strong(
                      style = paste0("color:", clr, ";"),
                      FormatearNumero(row$real_sacos, "coma"), " / ",
                      FormatearNumero(row$ppto_sacos, "coma")
                    )
                  ),
                  tags$span(
                    style = "font-size:11px; color:#555;",
                    icon("dollar-sign"), " Margen: ",
                    tags$strong(
                      style = paste0("color:", .color_kpi(row$cumpl_margen, "retencion"), ";"),
                      FormatearNumero(row$real_margen, "dinero"), " / ",
                      FormatearNumero(row$ppto_margen, "dinero")
                    )
                  )
                ),
                # Brecha de presupuesto no ejecutado — crítico para "A Recuperar"
                if (row$estado_grp == "A Recuperar" && row$brecha_sacos > 0) {
                  tags$p(
                    style = "font-size:10px; color:#E74C3C; margin:6px 0 0 0; font-style:italic;",
                    icon("exclamation-triangle"), " Presupuesto en riesgo por inactividad: ",
                    FormatearNumero(row$brecha_sacos,  "coma"),   " sacos / ",
                    FormatearNumero(row$brecha_margen, "dinero")
                  )
                }
              )
            )
          })
        ),
        # Gráfico de barras ppto vs real por estado
        fluidRow(
          column(6,
                 h6("Sacos: Ejec. vs Ppto por Estado",
                    style = "font-weight:600; margin:12px 0 4px;"),
                 plotly::plotlyOutput(ns("graf_ejec_sacos"),  height = "240px")
          ),
          column(6,
                 h6("Margen: Ejec. vs Ppto por Estado",
                    style = "font-weight:600; margin:12px 0 4px;"),
                 plotly::plotlyOutput(ns("graf_ejec_margen"), height = "240px")
          )
        )
      )
    })
    
    output$graf_ejec_sacos <- renderPlotly({
      req(resumen_ejec_r())
      df <- resumen_ejec_r() %>%
        tidyr::pivot_longer(
          cols = c(ppto_sacos, real_sacos),
          names_to = "tipo", values_to = "valor"
        ) %>%
        mutate(tipo = if_else(tipo == "ppto_sacos", "Presupuesto", "Ejecutado"))
      plotly::plot_ly(
        df, x = ~estado_grp, y = ~valor, color = ~tipo,
        colors = c("Presupuesto" = "#BDC3C7", "Ejecutado" = "#2C7BB6"),
        type = "bar"
      ) %>%
        plotly::layout(
          barmode = "group",
          xaxis   = list(title = ""),
          yaxis   = list(title = "Sacos"),
          legend  = list(orientation = "h", y = -0.2),
          margin  = list(l = 10, r = 10, t = 20, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$graf_ejec_margen <- renderPlotly({
      req(resumen_ejec_r())
      df <- resumen_ejec_r() %>%
        tidyr::pivot_longer(
          cols = c(ppto_margen, real_margen),
          names_to = "tipo", values_to = "valor"
        ) %>%
        mutate(tipo = if_else(tipo == "ppto_margen", "Presupuesto", "Ejecutado"))
      plotly::plot_ly(
        df, x = ~estado_grp, y = ~valor, color = ~tipo,
        colors = c("Presupuesto" = "#BDC3C7", "Ejecutado" = "#27AE60"),
        type = "bar"
      ) %>%
        plotly::layout(
          barmode = "group",
          xaxis   = list(title = ""),
          yaxis   = list(title = "Margen ($)", tickformat = "$.3s"),
          legend  = list(orientation = "h", y = -0.2),
          margin  = list(l = 10, r = 10, t = 20, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    ## [5] Gráficos de Evolución y Permanencia — ampliados ----
    
    # Evolución mensual por estado (existente)
    output$graf_evolucion <- renderPlotly({
      req(panel_activo())
      df <- panel_activo() %>%
        group_by(ym, estado) %>%
        summarise(n = n_distinct(cliente_id), .groups = "drop") %>%
        mutate(
          estado  = factor(estado, levels = c(
            "CLIENTE ACTIVO", "CLIENTE A RECUPERAR", "NUEVO DEL PERIODO"
          )),
          mes_lbl = format(ym, "%b-%y")
        )
      colores <- c(
        "CLIENTE ACTIVO"      = "#2C7BB6",
        "CLIENTE A RECUPERAR" = "#F4A820",
        "NUEVO DEL PERIODO"   = "#27AE60"
      )
      plotly::plot_ly(
        df, x = ~mes_lbl, y = ~n, color = ~estado,
        colors = colores, type = "bar"
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis   = list(title = "", tickangle = -45),
          yaxis   = list(title = "Clientes"),
          legend  = list(orientation = "h", y = -0.3),
          margin  = list(l = 10, r = 10, t = 30, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Permanencia: distribución de tasa de facturación (existente)
    output$graf_permanencia <- renderPlotly({
      req(data_cohortes(), panel_activo())
      df <- data_cohortes()$tasa_fact_uc %>%
        semi_join(panel_activo() %>% distinct(cliente_id), by = "cliente_id") %>%
        mutate(Rango = cut(
          tasa_facturacion,
          breaks = c(0, .25, .5, .75, 1.001),
          labels = c("0-25%", "25-50%", "50-75%", "75-100%"),
          right  = FALSE
        )) %>%
        count(Rango)
      plotly::plot_ly(
        df, x = ~Rango, y = ~n, type = "bar",
        marker = list(color = "#2C7BB6")
      ) %>%
        plotly::layout(
          xaxis  = list(title = "Tasa de Facturaci\u00f3n"),
          yaxis  = list(title = "Clientes"),
          margin = list(l = 10, r = 10, t = 30, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # [NUEVO] Tasa de facturación mensual promedio — línea temporal
    output$graf_tasa_mensual <- renderPlotly({
      req(data_cohortes(), panel_activo())
      dat <- data_cohortes()
      df  <- panel_activo() %>%
        left_join(dat$tasa_fact_uc, by = "cliente_id") %>%
        group_by(ym) %>%
        summarise(
          tasa_prom = mean(tasa_facturacion, na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        mutate(mes_lbl = format(ym, "%b-%y"))
      plotly::plot_ly(
        df, x = ~mes_lbl, y = ~tasa_prom, type = "scatter", mode = "lines+markers",
        line = list(color = "#2C7BB6", width = 2),
        marker = list(color = "#2C7BB6", size = 7)
      ) %>%
        plotly::layout(
          xaxis  = list(title = "", tickangle = -45),
          yaxis  = list(title = "Tasa promedio", tickformat = ".0%", range = c(0, 1)),
          margin = list(l = 10, r = 10, t = 20, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # [NUEVO] Indicadores de transición mensual: retención y pérdida mes a mes
    output$graf_transicion_mensual <- renderPlotly({
      req(panel_activo())
      pan   <- panel_activo()
      meses <- sort(unique(pan$ym))
      if (length(meses) < 2) {
        return(plotly::plotly_empty())
      }
      pares <- tibble(ym_t = head(meses, -1), ym_t1 = tail(meses, -1))
      ind_mes <- pares %>%
        left_join(
          pan %>% select(cliente_id, ym, estado) %>% rename(ym_t = ym, est_t = estado),
          by = "ym_t"
        ) %>%
        left_join(
          pan %>% select(cliente_id, ym, estado) %>% rename(ym_t1 = ym, est_t1 = estado),
          by = c("cliente_id", "ym_t1")
        ) %>%
        group_by(ym_t1) %>%
        summarise(
          activos_t    = sum(est_t == "CLIENTE ACTIVO", na.rm = TRUE),
          recuperar_t  = sum(est_t == "CLIENTE A RECUPERAR", na.rm = TRUE),
          retencion    = SiError_0(
            sum(est_t == "CLIENTE ACTIVO" & est_t1 == "CLIENTE ACTIVO", na.rm = TRUE) /
              pmax(activos_t, 1)
          ),
          perdida      = SiError_0(
            sum(est_t == "CLIENTE ACTIVO" & est_t1 == "CLIENTE A RECUPERAR", na.rm = TRUE) /
              pmax(activos_t, 1)
          ),
          reactivacion = SiError_0(
            sum(est_t == "CLIENTE A RECUPERAR" & est_t1 == "CLIENTE ACTIVO", na.rm = TRUE) /
              pmax(recuperar_t, 1)
          ),
          .groups = "drop"
        ) %>%
        mutate(mes_lbl = format(ym_t1, "%b-%y"))
      plotly::plot_ly(ind_mes, x = ~mes_lbl) %>%
        plotly::add_trace(
          y = ~retencion, name = "Retenci\u00f3n", type = "scatter", mode = "lines+markers",
          line = list(color = "#27AE60", width = 2), marker = list(size = 6)
        ) %>%
        plotly::add_trace(
          y = ~perdida, name = "P\u00e9rdida", type = "scatter", mode = "lines+markers",
          line = list(color = "#E74C3C", width = 2, dash = "dot"),
          marker = list(size = 6)
        ) %>%
        plotly::add_trace(
          y = ~reactivacion, name = "Reactivaci\u00f3n", type = "scatter",
          mode = "lines+markers",
          line = list(color = "#F4A820", width = 2), marker = list(size = 6)
        ) %>%
        plotly::layout(
          xaxis  = list(title = "", tickangle = -45),
          yaxis  = list(title = "%", tickformat = ".0%", range = c(0, 1)),
          legend = list(orientation = "h", y = -0.3),
          margin = list(l = 10, r = 10, t = 20, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # [NUEVO] Distribución de días hasta vencimiento de ventana (activos en riesgo)
    output$graf_dias_venc <- renderPlotly({
      req(data_cohortes())
      trans <- data_cohortes()$transiciones
      req("DiasHastaVencimiento" %in% names(trans))
      df <- trans %>%
        filter(
          Transicion %in% c("ACTIVO_A_INACTIVO", "MANTIENE_ACTIVO"),
          !is.na(DiasHastaVencimiento)
        ) %>%
        mutate(
          Rango = cut(
            DiasHastaVencimiento,
            breaks = c(-Inf, -30, -15, 0, 15, 30, Inf),
            labels = c(
              "<-30d (vencido)",  "-30 a -15d", "-15 a 0d",
              "0 a 15d",           "15 a 30d",  ">30d (holgura)"
            ),
            right = TRUE
          )
        ) %>%
        count(Rango) %>%
        mutate(
          color = case_when(
            grepl("vencido", Rango) ~ "#E74C3C",
            grepl("-15 a 0", Rango) ~ "#F4A820",
            TRUE                    ~ "#27AE60"
          )
        )
      plotly::plot_ly(
        df, x = ~Rango, y = ~n, type = "bar",
        marker = list(color = ~color)
      ) %>%
        plotly::layout(
          xaxis  = list(title = "D\u00edas hasta vencimiento"),
          yaxis  = list(title = "Clientes"),
          margin = list(l = 10, r = 10, t = 20, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # [NUEVO] Cohorte de altas acumuladas: nuevos UC que ingresan al panel por mes
    output$graf_cohorte_altas <- renderPlotly({
      req(panel_activo())
      # panel_full ya tiene columna mes_entrada para ALTA EN COHORTE (del panel_altas).
      # Para POBLACION BASE mes_entrada = mes_inicio (asignado en bind_rows).
      # Se usa mes_entrada directamente; group_by(mes_entrada = ym) creaba conflicto.
      df <- panel_activo() %>%
        filter(tipo_cohorte == "ALTA EN COHORTE") %>%
        filter(!is.na(mes_entrada)) %>%
        group_by(mes_entrada) %>%
        summarise(n_altas = n_distinct(cliente_id), .groups = "drop") %>%
        arrange(mes_entrada) %>%
        mutate(
          mes_lbl = format(mes_entrada, "%b-%y"),
          n_acum  = cumsum(n_altas)
        )
      plotly::plot_ly(df, x = ~mes_lbl) %>%
        plotly::add_bars(
          y = ~n_altas, name = "Nuevas altas", marker = list(color = "#27AE60")
        ) %>%
        plotly::add_trace(
          y = ~n_acum, name = "Acumulado", type = "scatter", mode = "lines",
          line = list(color = "#2C7BB6", width = 2),
          yaxis = "y2"
        ) %>%
        plotly::layout(
          xaxis  = list(title = "", tickangle = -45),
          yaxis  = list(title = "Nuevas altas"),
          yaxis2 = list(
            title = "Acumulado", overlaying = "y", side = "right", showgrid = FALSE
          ),
          legend = list(orientation = "h", y = -0.3),
          margin = list(l = 10, r = 10, t = 20, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    ## Descargas ----
    output$dl_poblacion <- .dl_handler(
      function() {
        panel_activo() %>%
          left_join(data_cohortes()$tasa_fact_uc, by = "cliente_id") %>%
          arrange(cliente_id, ym)
      },
      "poblacion"
    )
    
    output$dl_altas <- .dl_handler(
      function() {
        data_cohortes()$panel_full %>%
          filter(tipo_cohorte == "ALTA EN COHORTE") %>%
          left_join(data_cohortes()$tasa_fact_uc, by = "cliente_id") %>%
          arrange(cliente_id, ym)
      },
      "altas_cohorte"
    )
  })
}
# Helpers ----

# Comentario de nota explicativa bajo KPI dinamico
.te_nota <- function(texto) {
  tags$p(
    texto,
    style = paste0(
      "font-size:10px; color:#999; text-align:center; ",
      "margin-top:2px; margin-bottom:0; font-style:italic;"
    )
  )
}
# Comentario de separador de seccion con icono
.te_seccion <- function(titulo, icono) {
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
        "font-size:13px; font-weight:700; color:#374151; ",
        "letter-spacing:0.03em; text-transform:uppercase;"
      )
    )
  )
}
# Comentario de subtitulo con color dinamico
.te_subtitulo <- function(output_id, color) {
  h6(
    uiOutput(output_id),
    style = sprintf(
      "font-weight:600; color:%s; margin-bottom:4px; margin-top:8px;",
      color
    )
  )
}


# Modulo principal ----
CohortesUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Comentario de bloque de alertas clickeables
    bs4Dash::bs4Card(
      title = tagList(
        icon("exchange-alt"),
        " Alertas del Mes Vigente"
      ),
      width = 12,
      solidHeader = TRUE,
      status = "white",
      collapsible = TRUE,
      
      fluidRow(
        column(3,
               CajaModalUI(ns("kpi_a_inactivo"))
        ),
        column(3,
               CajaModalUI(ns("kpi_a_activo"))
        ),
        column(3,
               CajaModalUI(ns("kpi_nuevo"))
        ),
        column(3,
               CajaModalUI(ns("kpi_reactivado"))
        )
      )
    ),
    
    # Comentario de bloque de indicadores dinamicos YTD
    bs4Dash::bs4Card(
      title = tagList(
        icon("chart-line"),
        " Indicadores Dinámicos — YTD"
      ),
      width = 12,
      solidHeader = TRUE,
      status = "white",
      collapsible = TRUE,
      
      fluidRow(
        column(3,
               CajaModalUI(ns("kpi_retencion"))
        ),
        column(3,
               CajaModalUI(ns("kpi_perdida"))
        ),
        column(3,
               CajaModalUI(ns("kpi_reactivacion"))
        ),
        column(3,
               CajaModalUI(ns("kpi_tasa_fact"))
        )
      ),
      
      fluidRow(
        column(
          3,
          .te_nota("Activos t → Activos t+1 / Total Activos en t")
        ),
        column(
          3,
          .te_nota("Activos t → A Recuperar t+1 / Total Activos en t")
        ),
        column(
          3,
          .te_nota(
            "A Recuperar t → Activos t+1 / Total A Recuperar en t"
          )
        ),
        column(
          3,
          .te_nota(
            "Promedio meses con facturación / meses en panel YTD"
          )
        )
      )
    ),
    
    # Comentario de bloque de analisis por poblacion
    bs4Dash::bs4Card(
      title = tagList(
        icon("users"),
        " Análisis por Población"
      ),
      width = 12,
      solidHeader = TRUE,
      status = "white",
      collapsible = TRUE,
      
      # Comentario de control de poblacion
      fluidRow(
        column(12,
               tags$div(
                 style = paste0(
                   "display:flex; gap:24px; align-items:center; ",
                   "padding:8px 12px; background:#F8FAFC; ",
                   "border-radius:6px; margin-bottom:12px;"
                 ),
                 
                 tags$span(
                   style = "font-size:12px; font-weight:700; color:#374151;",
                   icon("filter"),
                   " Población:"
                 ),
                 
                 shinyWidgets::prettyCheckbox(
                   inputId = ns("pob_total"),
                   label = "Total",
                   value = TRUE,
                   icon = icon("check"),
                   status = "primary",
                   shape = "round"
                 ),
                 
                 shinyWidgets::prettyCheckbox(
                   inputId = ns("pob_presup"),
                   label = "Presupuestados",
                   value = FALSE,
                   icon = icon("check"),
                   status = "info",
                   shape = "round"
                 ),
                 
                 shinyWidgets::prettyCheckbox(
                   inputId = ns("pob_sin_presup"),
                   label = "Sin Presupuesto",
                   value = FALSE,
                   icon = icon("check"),
                   status = "warning",
                   shape = "round"
                 )
               )
        )
      ),
      
      # Comentario de sub bloque estado enero
      .te_seccion(
        "Estado Enero — Inicio de Año",
        "calendar-alt"
      ),
      
      h6(
        "Inicio de Año — Enero",
        style = paste0(
          "font-weight:600; color:#2C7BB6; ",
          "margin-bottom:4px;"
        )
      ),
      
      uiOutput(ns("cajas_enero")),
      
      # Comentario de sub bloque estado mes vigente
      .te_seccion(
        "Estado Actual — Mes Vigente",
        "calendar-check"
      ),
      
      h6(
        uiOutput(ns("lbl_mes_vigente")),
        style = paste0(
          "font-weight:600; color:#F4A820; ",
          "margin-bottom:4px;"
        )
      ),
      
      uiOutput(ns("cajas_vigente")),
      
      # Comentario de sub bloque indicadores dinamicos
      .te_seccion(
        "Indicadores Dinámicos Enero → Mes Vigente",
        "arrows-alt-h"
      ),
      
      uiOutput(ns("cajas_dinamicos")),
      
      hr(),
      
      # Comentario de tabla resumen mensual
      .te_seccion(
        "Resumen Mensual por Estado",
        "table"
      ),
      
      p(
        icon("hand-pointer"),
        " Haga clic en una celda para ver el detalle de clientes",
        style = "font-size:11px; color:#888; margin-bottom:8px;"
      ),
      
      reactable::reactableOutput(ns("tabla_resumen_mensual")),
      
      hr(),
      
      # Comentario de panel ejecucion vs presupuesto
      .te_seccion(
        "Ejecución vs Presupuesto",
        "bullseye"
      ),
      
      uiOutput(ns("panel_ejecucion")),
      
      hr(),
      
      # Comentario de graficos de evolucion y permanencia
      .te_seccion(
        "Evolución y Permanencia",
        "chart-bar"
      ),
      
      fluidRow(
        
        # Comentario de grafico de evolucion
        column(7,
               h6(
                 "Evolución Mensual por Estado",
                 style = "font-weight:600; margin-bottom:4px;"
               ),
               
               plotly::plotlyOutput(
                 ns("graf_evolucion"),
                 height = "300px"
               )
        ),
        
        # Comentario de grafico de permanencia
        column(5,
               h6(
                 "Permanencia por Estado",
                 style = "font-weight:600; margin-bottom:4px;"
               ),
               
               plotly::plotlyOutput(
                 ns("graf_permanencia"),
                 height = "300px"
               )
        )
      )
    )
  )
}
Cohortes <- function(id, data_tx, fecha_rango = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helpers ----
    # Colores semafóricos para KPIs de tasa
    .color_kpi <- function(v, tipo) {
      if (is.null(v) || is.na(v)) return("#999999")
      switch(tipo,
             retencion   = if (v >= 0.80) "#27AE60" else if (v >= 0.60) "#F4A820" else "#E74C3C",
             perdida     = if (v <= 0.10) "#27AE60" else if (v <= 0.20) "#F4A820" else "#E74C3C",
             reactivacion = if (v >= 0.30) "#27AE60" else if (v >= 0.15) "#F4A820" else "#E74C3C",
             tasa_fact   = if (v >= 0.70) "#27AE60" else if (v >= 0.50) "#F4A820" else "#E74C3C",
             "#999999"
      )
    }
    # Reactable de UCs para modales — patrón Cohortes existente
    .reactable_uc <- function(data, click_id) {
      reactable::reactable(
        data %>%
          select(any_of(c("PerRazSoc", "Asesor", "Segmento",
                          "estado", "presupuestada", "cliente_id"))) %>%
          distinct(),
        onClick  = reactable::JS(sprintf(
          "function(rowInfo) { Shiny.setInputValue('%s',
            {cliente_id: rowInfo.values['cliente_id'],
             nonce: Math.random()}, {priority: 'event'}); }",
          click_id
        )),
        columns = list(
          cliente_id    = reactable::colDef(show = FALSE),
          PerRazSoc     = reactable::colDef(name = "Cliente",       minWidth = 200),
          Asesor        = reactable::colDef(name = "Asesor",        minWidth = 120),
          Segmento      = reactable::colDef(name = "Segmento",      minWidth = 100),
          estado        = reactable::colDef(name = "Estado",        minWidth = 150),
          presupuestada = reactable::colDef(name = "Presupuesto",   minWidth = 120)
        ),
        highlight = TRUE, compact = TRUE, bordered = TRUE,
        pagination = FALSE, searchable = TRUE
      )
    }
    # Handler de descarga Excel — patrón Cohortes existente
    .dl_handler <- function(datos_fn, prefijo) {
      downloadHandler(
        filename = function() paste0(prefijo, "_", Sys.Date(), ".xlsx"),
        content  = function(file) writexl::write_xlsx(datos_fn(), path = file)
      )
    }
    # Modal con tabla de clientes — se usa en tabla resumen mensual
    .show_modal <- function(titulo, icono_nm, data_clientes, modal_id) {
      click_id <- paste0("click_", modal_id)
      cumpl_id <- paste0("cumpl_", modal_id)
      
      showModal(modalDialog(
        title = tagList(icon(icono_nm), " ", titulo),
        size = "xl", easyClose = TRUE, footer = modalButton("Cerrar"),
        tagList(
          .reactable_uc(data_clientes, ns(click_id)),
          p(icon("hand-pointer"),
            " Haga clic en una fila para ver el detalle de cumplimiento",
            style = "font-size:11px; color:#888; margin-top:4px;"),
          uiOutput(ns(cumpl_id))
        )
      ))
      
      observeEvent(input[[click_id]], {
        req(!is.null(input[[click_id]]$cliente_id))
        cid      <- input[[click_id]]$cliente_id
        fila     <- data_clientes %>% filter(cliente_id == cid)
        req(nrow(fila) > 0)
        rsoc      <- fila$PerRazSoc[[1]]
        es_presup <- fila$presupuestada[[1]] == "PRESUPUESTADA"
        
        # panel_cumpl_grupal no está implementado — solo mostrar cumplimiento
        # para presupuestadas; resto muestra aviso.
        panel_c <- if (es_presup) panel_cumpl() else NULL
        
        output[[cumpl_id]] <<- renderUI({
          if (is.null(panel_c) || !(cid %in% panel_c$cliente_id)) {
            return(tagList(
              hr(),
              p(icon("info-circle"),
                " Sin presupuesto asignado en el periodo.",
                style = "color:#888; font-style:italic; margin-top:10px;")
            ))
          }
          gt_id <- paste0("gt_", gsub("[^A-Za-z0-9]", "_", cid), "_", modal_id)
          output[[gt_id]] <<- gt::render_gt({
            req(meses_con_real())
            .gt_cumpl_uc(panel_c, cid, rsoc, meses_con_real())
          })
          tagList(hr(), gt::gt_output(ns(gt_id)))
        })
      }, ignoreNULL = TRUE, ignoreInit = TRUE, once = FALSE)
    }
    # Convierte etiqueta de población a sufijo de ID válido para Shiny/JS
    .pob_id <- function(p) gsub("[^A-Za-z0-9]", "_", p)
    
    # Parametros ----
    
    fecha_rango_efectiva <- reactive({
      if (!is.null(fecha_rango) && !is.null(fecha_rango())) {
        fecha_rango()
      } else {
        c(
          as.Date(sprintf("%d-01-01", year(Sys.Date()))),
          PrimerDia(Sys.Date())
        )
      }
    })
    anio_vigente <- reactive({
      req(fecha_rango_efectiva())
      year(fecha_rango_efectiva()[2])
    })
    mes_inicio <- reactive({
      req(anio_vigente())
      as.Date(sprintf("%d-01-01", anio_vigente()))
    })
    mes_vigente <- reactive({ PrimerDia(Sys.Date()) })
    meses_periodo <- reactive({
      req(mes_inicio(), mes_vigente())
      seq.Date(mes_inicio(), mes_vigente(), by = "month")
    })
    n_meses <- reactive({ length(meses_periodo()) })
    
    # Datos ----
    
    # CRMNALSEGR con cliente_id compuesto
    crm_data <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      on.exit(waiter_hide())  
      CargarDatos("CRMNALSEGR") %>%
        mutate(FecProceso = as.Date(FecProceso),
               cliente_id = paste(CliNitPpal, LinNegCod, sep = "_"))
    }) %>% bindCache("CRMNALSEGR")
    # Último corte disponible en crm_data (primer día hábil = cierre del mes anterior)
    ultimo_corte <- reactive({
      req(crm_data())
      max(crm_data()$FecProceso)
    })
    # Transacciones limpias del año seleccionado
    tx_limpia <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      req(data_tx(), anio_vigente())
      data_tx() %>%
        filter(Excluir      == "NO",
               ProdExcluir  == "NO",
               !is.na(FecFact),
               year(FecFact) == anio_vigente()) %>%
        mutate(FecFact    = as.Date(FecFact),
               ym         = PrimerDia(FecFact),
               cliente_id = paste(CliNitPpal, LinNegCod, sep = "_")
        )
    })
    
    # Catálogo razón social / asesor / segmento
    catalogo_rs <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      req(tx_limpia(), crm_data())
      
      # Atributos dimensionales desde CRMNALCLIENTE (fuente correcta)
      # Snapshot vigente: último registro por cliente-línea
      attrs_crm <- CargarDatos("CRMNALCLIENTE") %>%
        mutate(FecProceso = as.Date(FecProceso)) %>%
        group_by(CliNitPpal, LinNegCod) %>%
        filter(FecProceso == max(FecProceso)) %>%
        slice(1L) %>%
        ungroup() %>%
        mutate(
          cliente_id = paste(CliNitPpal, LinNegCod, sep = "_"),
          Segmento   = if_else(Segmento == "GRANDES", "GRANDE", Segmento),
          Asesor     = str_squish(str_to_upper(Asesor))
        ) %>%
        select(cliente_id, CliNitPpal, LinNegCod, Asesor, Segmento)
      
      # Razón social desde NCLIENTE (fuente correcta)
      razon_social <- NCLIENTE %>%
        select(CliNitPpal = PerCod, PerRazSoc) %>%
        distinct()
      
      # Base: todos los clientes del panel CRM
      crm_data() %>%
        distinct(cliente_id, CliNitPpal, LinNegCod) %>%
        left_join(attrs_crm,    by = c("cliente_id", "CliNitPpal", "LinNegCod")) %>%
        left_join(razon_social, by = "CliNitPpal") %>%
        mutate(
          PerRazSoc = coalesce(PerRazSoc, "\u2014"),
          Asesor    = coalesce(Asesor,    "\u2014"),
          Segmento  = coalesce(Segmento,  "\u2014")
        ) %>%
        select(cliente_id, CliNitPpal, LinNegCod, PerRazSoc, Asesor, Segmento)
    })
    # Versión slim del catálogo: solo atributos dimensionales nuevos.
    # Usar en joins sobre bases que ya traen CliNitPpal y LinNegCod.
    catalogo_slim <- reactive({
      catalogo_rs() %>% select(cliente_id, PerRazSoc, Asesor, Segmento)
    })
    
    # Marca de presupuesto por UC desde el primer mes del año
    marca_presupuesto <- reactive({
      req(tx_limpia(), mes_inicio())
      tx_limpia() %>%
        filter(ym == mes_inicio()) %>%
        group_by(cliente_id, CliNitPpal, LinNegCod) %>%
        summarise(
          ppto_sacos_anual  = sum(coalesce(PptoSacos,  0), na.rm = TRUE),
          ppto_margen_anual = sum(coalesce(PptoMargen, 0), na.rm = TRUE),
          presupuestada     = if_else(
            sum(coalesce(PptoSacos, 0), na.rm = TRUE) > 0,
            "PRESUPUESTADA", "NO PRESUPUESTADA"
          ),
          .groups = "drop"
        )
    })
    
    # Ventas reales mensuales
    real_mensual <- reactive({
      req(tx_limpia())
      tx_limpia() %>%
        group_by(cliente_id, ym) %>%
        summarise(
          real_sacos  = sum(coalesce(SacFact70, 0), na.rm = TRUE),
          real_margen = sum(coalesce(Margen,    0), na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    # Meses con facturación real (para gt de cumplimiento)
    meses_con_real <- reactive({
      req(real_mensual())
      real_mensual() %>%
        filter(real_sacos > 0) %>%
        pull(ym) %>%
        unique() %>%
        sort()
    })
    
    # Panel Longitudinal ----
    
    # Actividad mensual desde CRMNALSEGR
    actividad_mensual <- reactive({
      req(crm_data(), mes_inicio(), mes_vigente())
      crm_data() %>%
        filter(FecProceso >= mes_inicio(), FecProceso <= mes_vigente()) %>%
        mutate(ym = PrimerDia(FecProceso)) %>%
        distinct(cliente_id, ym, SegmentoRacafe)
    })
    # Baseline: clientes existentes en enero del año seleccionado
    baseline <- reactive({
      req(crm_data(), marca_presupuesto(), mes_inicio())
      crm_data() %>%
        filter(
          FecProceso == mes_inicio(),
          SegmentoRacafe %in% c("CLIENTE", "CLIENTE A RECUPERAR")
        ) %>%
        distinct(CliNitPpal, LinNegCod, cliente_id) %>%
        left_join(
          marca_presupuesto() %>% select(cliente_id, presupuestada),
          by = "cliente_id"
        ) %>%
        mutate(
          presupuestada = replace_na(presupuestada, "NO PRESUPUESTADA"),
          tipo_cohorte  = "POBLACION BASE"
        )
    })
    # Altas en cohorte: nuevos que facturan en el año, acumulativas
    altas_cohorte <- reactive({
      req(tx_limpia(), baseline(), mes_inicio())
      nits_baseline          <- baseline() %>% distinct(CliNitPpal)
      nits_fact_pre_baseline <- FACT %>%
        filter(as.Date(MinFecFact) < mes_inicio()) %>%
        distinct(FctNit) %>%
        rename(CliNitPpal = FctNit)
      tx_limpia() %>%
        filter(ym >= mes_inicio(), ym <= mes_vigente()) %>%
        distinct(CliNitPpal, LinNegCod, cliente_id) %>%
        anti_join(nits_baseline,          by = "CliNitPpal") %>%
        anti_join(nits_fact_pre_baseline, by = "CliNitPpal") %>%
        left_join(
          marca_presupuesto() %>% select(cliente_id, presupuestada),
          by = "cliente_id"
        ) %>%
        mutate(
          presupuestada = replace_na(presupuestada, "NO PRESUPUESTADA"),
          tipo_cohorte  = "ALTA EN COHORTE"
        )
    })
    # Panel baseline: estado mes a mes desde CRMNALSEGR
    panel_baseline <- reactive({
      req(baseline(), actividad_mensual(), meses_periodo())
      baseline() %>%
        crossing(tibble(ym = meses_periodo())) %>%
        left_join(actividad_mensual(), by = c("cliente_id", "ym")) %>%
        mutate(estado = case_when(
          SegmentoRacafe == "CLIENTE"             ~ "CLIENTE ACTIVO",
          SegmentoRacafe == "CLIENTE A RECUPERAR" ~ "CLIENTE A RECUPERAR",
          TRUE                                    ~ "CLIENTE A RECUPERAR"
        )) %>%
        select(cliente_id, CliNitPpal, LinNegCod, presupuestada,
               tipo_cohorte, ym, estado)
    })
    # Panel altas: nuevos con estado fijo "NUEVO DEL PERIODO"
    panel_altas <- reactive({
      req(tx_limpia(), altas_cohorte(), meses_periodo())
      # Mes de primera factura por cliente nuevo
      primer_mes_nuevo <- tx_limpia() %>%
        semi_join(altas_cohorte(), by = "cliente_id") %>%
        group_by(cliente_id) %>%
        summarise(mes_entrada = min(ym), .groups = "drop")
      
      altas_cohorte() %>%
        left_join(primer_mes_nuevo, by = "cliente_id") %>%
        mutate(mes_entrada = coalesce(mes_entrada, mes_inicio())) %>%
        crossing(tibble(ym = meses_periodo())) %>%
        # Solo aparece desde su mes de entrada en adelante
        filter(ym >= mes_entrada) %>%
        mutate(estado = "NUEVO DEL PERIODO") %>%
        select(cliente_id, CliNitPpal, LinNegCod, presupuestada,
               tipo_cohorte, ym, estado, mes_entrada)
    })
    # Panel completo: baseline + altas
    panel_full <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      req(panel_baseline(), panel_altas())
      bind_rows(
        panel_baseline() %>% mutate(mes_entrada = mes_inicio()),
        panel_altas()
      ) %>%
        # Usar catalogo_slim: CliNitPpal y LinNegCod ya vienen de panel_baseline/altas
        left_join(catalogo_slim(), by = "cliente_id")
    })
    
    panel_p  <- reactive(panel_full() %>% filter(presupuestada == "PRESUPUESTADA"))
    panel_np <- reactive(panel_full() %>% filter(presupuestada == "NO PRESUPUESTADA"))
    
    # Panel enriquecido con cumplimiento individual (presupuestados)
    panel_cumpl <- reactive({
      req(marca_presupuesto(), real_mensual(), meses_periodo())
      marca_presupuesto() %>%
        filter(presupuestada == "PRESUPUESTADA") %>%
        select(cliente_id, LinNegCod, ppto_sacos_anual, ppto_margen_anual) %>%
        crossing(tibble(ym = meses_periodo())) %>%
        left_join(
          panel_baseline() %>% select(cliente_id, ym, estado),
          by = c("cliente_id", "ym")
        ) %>%
        left_join(real_mensual(), by = c("cliente_id", "ym")) %>%
        mutate(
          ppto_sacos_mes  = ppto_sacos_anual  / 12,
          ppto_margen_mes = ppto_margen_anual / 12,
          real_sacos      = coalesce(real_sacos,  0),
          real_margen     = coalesce(real_margen, 0),
          cumpl_sacos_pct = if_else(
            ppto_sacos_mes  > 0, round(real_sacos  / ppto_sacos_mes  * 100, 1), NA_real_
          ),
          cumpl_margen_pct = if_else(
            ppto_margen_mes > 0, round(real_margen / ppto_margen_mes * 100, 1), NA_real_
          )
        ) %>%
        select(cliente_id, LinNegCod, ym, estado,
               ppto_sacos_mes, real_sacos, cumpl_sacos_pct,
               ppto_margen_mes, real_margen, cumpl_margen_pct)
    })
    
    # Tasa de facturación por UC
    tasa_fact_uc <- reactive({
      req(panel_full(), meses_periodo())
      panel_full() %>%
        group_by(cliente_id) %>%
        summarise(
          meses_en_panel  = n_distinct(ym),
          meses_con_fact  = sum(estado == "CLIENTE ACTIVO", na.rm = TRUE),
          tasa_facturacion = meses_con_fact / pmax(meses_en_panel, 1),
          .groups = "drop"
        )
    })
    
    # Último corte disponible del panel para KPIs inicio/fin
    corte_ult <- reactive({
      req(panel_full())
      ult <- max(panel_full()$ym)
      panel_full() %>% filter(ym == ult)
    })
    
    # Transiciones ----
    # Última factura por cliente-línea sobre todo el histórico
    ultima_fact_r <- reactive({
      req(data_tx())
      data_tx() %>%
        filter(!is.na(FecFact)) %>%
        group_by(CliNitPpal, LinNegCod) %>%
        summarise(UltimaFact = max(FecFact, na.rm = TRUE), .groups = "drop") %>%
        mutate(cliente_id = paste(CliNitPpal, LinNegCod, sep = "_"))
    })
    
    # Parámetro de ventana individual por cliente desde CRMNALSEGR
    t1_corte <- reactive({
      req(crm_data(), ultimo_corte())
      crm_data() %>%
        filter(FecProceso == ultimo_corte()) %>%
        select(cliente_id, CliNitPpal, LinNegCod, SegmentoRacafe, Meses)
    })
    
    # Clientes con factura en el mes vigente (t3 en vivo)
    t3_mes_vigente_r <- reactive({
      req(data_tx(), mes_vigente())
      data_tx() %>%
        filter(!is.na(FecFact), PrimerDia(FecFact) == mes_vigente()) %>%
        distinct(CliNitPpal, LinNegCod) %>%
        mutate(cliente_id = paste(CliNitPpal, LinNegCod, sep = "_"))
    })
    
    # Clientes nuevos en t4: creados en los últimos 2 meses
    t4_nuevos_r <- reactive({
      NCLIENTE %>%
        mutate(
          FecCreacion = as.Date(FecCreacion),
          FecCreacion = if_else(
            FecCreacion < as.Date("1900-01-01"), NA_Date_, FecCreacion
          )
        ) %>%
        filter(!is.na(FecCreacion), FecCreacion >= mes_vigente() - months(2)) %>%
        distinct(CliNitPpal = PerCod)
    })
    
    # Proyección de transiciones para clientes conocidos
    proyeccion_conocidos <- reactive({
      req(t1_corte(), ultima_fact_r(), mes_vigente())
      t1_corte() %>%
        left_join(ultima_fact_r(), by = c("cliente_id", "CliNitPpal", "LinNegCod")) %>%
        # Usar catalogo_slim: CliNitPpal y LinNegCod ya vienen de t1_corte
        left_join(catalogo_slim(), by = "cliente_id") %>%
        left_join(
          marca_presupuesto() %>% select(cliente_id, presupuestada),
          by = "cliente_id"
        ) %>%
        mutate(
          presupuestada    = coalesce(presupuestada, "NO PRESUPUESTADA"),
          FecLimite        = mes_vigente() - months(Meses),
          EstadoProyectado = if_else(
            coalesce(UltimaFact, as.Date("2000-01-01")) >= FecLimite,
            "CLIENTE", "CLIENTE A RECUPERAR"
          ),
          Transicion = case_when(
            SegmentoRacafe == "CLIENTE" &
              EstadoProyectado == "CLIENTE A RECUPERAR" ~ "ACTIVO_A_INACTIVO",
            SegmentoRacafe == "CLIENTE A RECUPERAR" &
              EstadoProyectado == "CLIENTE"             ~ "INACTIVO_A_ACTIVO",
            SegmentoRacafe == "CLIENTE" &
              EstadoProyectado == "CLIENTE"             ~ "MANTIENE_ACTIVO",
            SegmentoRacafe == "CLIENTE A RECUPERAR" &
              EstadoProyectado == "CLIENTE A RECUPERAR" ~ "MANTIENE_INACTIVO",
            TRUE                                        ~ "OTRO"
          ),
          # Positivo: días dentro de ventana | Negativo: ya venció
          DiasHastaVencimiento = as.integer(
            coalesce(UltimaFact + months(Meses), FecLimite) - Sys.Date()
          )
        )
    })
    
    # Nuevos absolutos: facturaron en el mes vigente y creados en t4 últimos 2 meses
    nuevos_absolutos_r <- reactive({
      req(t3_mes_vigente_r(), t1_corte(), t4_nuevos_r())
      t3_mes_vigente_r() %>%
        anti_join(t1_corte(),    by = c("CliNitPpal", "LinNegCod")) %>%
        semi_join(t4_nuevos_r(), by = "CliNitPpal") %>%
        # Usar catalogo_slim: CliNitPpal y LinNegCod ya vienen de t3_mes_vigente_r
        left_join(catalogo_slim(), by = "cliente_id") %>%
        mutate(
          SegmentoRacafe       = NA_character_,
          Meses                = NA_integer_,
          presupuestada        = "NO PRESUPUESTADA",
          UltimaFact           = NA_Date_,
          FecLimite            = NA_Date_,
          EstadoProyectado     = "CLIENTE",
          Transicion           = "NUEVO_ABSOLUTO",
          DiasHastaVencimiento = NA_integer_
        )
    })
    
    # Reactivados sin CRM: facturaron, no en t1, no nuevos en t4
    reactivados_r <- reactive({
      req(t3_mes_vigente_r(), t1_corte(), t4_nuevos_r())
      t3_mes_vigente_r() %>%
        anti_join(t1_corte(),    by = c("CliNitPpal", "LinNegCod")) %>%
        anti_join(t4_nuevos_r(), by = "CliNitPpal") %>%
        # Usar catalogo_slim: CliNitPpal y LinNegCod ya vienen de t3_mes_vigente_r
        left_join(catalogo_slim(), by = "cliente_id") %>%
        mutate(
          SegmentoRacafe       = NA_character_,
          Meses                = NA_integer_,
          presupuestada        = "NO PRESUPUESTADA",
          UltimaFact           = NA_Date_,
          FecLimite            = NA_Date_,
          EstadoProyectado     = "CLIENTE",
          Transicion           = "REACTIVADO_SIN_CRM",
          DiasHastaVencimiento = NA_integer_
        )
    })
    
    # Consolidado de transiciones
    transiciones <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      req(proyeccion_conocidos(), nuevos_absolutos_r(), reactivados_r())
      bind_rows(
        proyeccion_conocidos() %>%
          select(cliente_id, CliNitPpal, LinNegCod, PerRazSoc, Asesor, Segmento,
                 SegmentoRacafe, Meses, presupuestada, UltimaFact, FecLimite,
                 EstadoProyectado, Transicion, DiasHastaVencimiento),
        nuevos_absolutos_r(),
        reactivados_r()
      )
    })
    
    # Indicadores — retención, pérdida, reactivación, tasa fact ----
    # Helper: calcula los tres indicadores para un subpanel dado
    .calcular_ind <- function(pan) {
      meses  <- sort(unique(pan$ym))
      if (length(meses) < 2) return(list(retencion = NA, perdida = NA, reactivacion = NA))
      pares <- tibble(ym_t = head(meses, -1), ym_t1 = tail(meses, -1))
      trans <- pares %>%
        left_join(pan %>% select(cliente_id, ym, estado) %>% rename(ym_t = ym, est_t = estado),
                  by = "ym_t") %>%
        left_join(pan %>% select(cliente_id, ym, estado) %>% rename(ym_t1 = ym, est_t1 = estado),
                  by = c("cliente_id", "ym_t1"))
      activos_t   <- trans %>% filter(est_t == "CLIENTE ACTIVO") %>% nrow()
      recuperar_t <- trans %>% filter(est_t == "CLIENTE A RECUPERAR") %>% nrow()
      list(
        retencion    = SiError_0(
          trans %>% filter(est_t == "CLIENTE ACTIVO", est_t1 == "CLIENTE ACTIVO") %>% nrow() /
            activos_t
        ),
        perdida      = SiError_0(
          trans %>%
            filter(est_t == "CLIENTE ACTIVO", est_t1 == "CLIENTE A RECUPERAR") %>%
            nrow() / activos_t
        ),
        reactivacion = SiError_0(
          trans %>%
            filter(est_t == "CLIENTE A RECUPERAR", est_t1 == "CLIENTE ACTIVO") %>%
            nrow() / recuperar_t
        )
      )
    }
    ind_full <- reactive({ req(panel_full()); .calcular_ind(panel_full()) })
    ind_p    <- reactive({ req(panel_p());    .calcular_ind(panel_p())    })
    ind_np   <- reactive({ req(panel_np());   .calcular_ind(panel_np())   })
    
    # Tasa de facturación global
    tasa_fact_global <- reactive({
      req(tasa_fact_uc())
      mean(tasa_fact_uc()$tasa_facturacion, na.rm = TRUE)
    })
    
    # Tabla resumen Mensual ----
    # Función que construye la tabla pivote para una población dada
    .tabla_resumen <- function(pan) {
      req(pan, meses_periodo())
      meses <- meses_periodo()
      # Conteo por estado y mes
      conteos <- pan %>%
        group_by(estado, ym) %>%
        summarise(n = n_distinct(cliente_id), .groups = "drop")
      # Totales por mes
      totales <- conteos %>%
        group_by(ym) %>%
        summarise(n = sum(n), .groups = "drop") %>%
        mutate(estado = "TOTAL")
      # Pivote: estados en filas, meses en columnas
      bind_rows(conteos, totales) %>%
        mutate(
          estado = factor(estado, levels = c(
            "CLIENTE ACTIVO", "CLIENTE A RECUPERAR",
            "NUEVO DEL PERIODO", "TOTAL"
          )),
          mes_lbl = format(ym, "%b-%y")
        ) %>%
        select(estado, mes_lbl, n) %>%
        tidyr::pivot_wider(names_from = mes_lbl, values_from = n, values_fill = 0L) %>%
        arrange(estado)
    }
    
    # Poblacion activa ----
    # Población seleccionada: combinación de checkboxes
    poblacion_activa <- reactive({
      req(panel_full())
      pobs <- c()
      if (isTRUE(input$pob_total))       pobs <- c(pobs, "TOTAL")
      if (isTRUE(input$pob_presup))      pobs <- c(pobs, "PRESUPUESTADA")
      if (isTRUE(input$pob_sin_presup))  pobs <- c(pobs, "NO PRESUPUESTADA")
      if (length(pobs) == 0) pobs <- "TOTAL"
      pobs
    })
    # Panel filtrado según población activa
    panel_activo <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      on.exit(waiter_hide())
      req(panel_full(), poblacion_activa())
      if ("TOTAL" %in% poblacion_activa()) return(panel_full())
      panel_full() %>%
        filter(presupuestada %in% poblacion_activa())
    })
    
    # Outputs ----
    observe({
      ids_plotly <- c(
        ns("graf_evolucion"),
        ns("graf_permanencia")
      )
      session$userData$.plotlyShinyEventIDs <- union(
        session$userData$.plotlyShinyEventIDs %||% character(0),
        ids_plotly
      )
    })
    ## Bloque 1: KPIs de alerta clickeables ----
    
    # Tabla de clientes en riesgo de inactivarse
    output$tbl_a_inactivo <- reactable::renderReactable({
      req(transiciones())
      .reactable_uc(
        transiciones() %>%
          filter(Transicion == "ACTIVO_A_INACTIVO") %>%
          mutate(estado = Transicion),
        ns("click_alerta_inactivo")
      )
    })
    CajaModal(
      "kpi_a_inactivo",
      valor   = reactive(html_valor(
        transiciones() %>% filter(Transicion == "ACTIVO_A_INACTIVO") %>% nrow(),
        formato = "numero", color = "#E74C3C"
      )),
      texto           = html_texto("Van a Inactivarse", color = "#E74C3C"),
      icono           = "user-times",
      colores         = c(fondo = "white"),
      color_fondo_hex = "#FADBD8",
      mostrar_boton   = TRUE,
      titulo_modal    = "Detalle \u2014 Clientes en Riesgo de Inactivarse",
      icono_modal     = "user-times",
      contenido_modal = function() reactable::reactableOutput(ns("tbl_a_inactivo")),
      footer          = reactive(paste0(
        "Clientes activos que superaron su ventana de inactividad al mes vigente"
      )),
      footer_class    = "caja-modal-footer"
    )
    
    # Tabla de clientes en recuperación
    output$tbl_a_activo <- reactable::renderReactable({
      req(transiciones())
      .reactable_uc(
        transiciones() %>%
          filter(Transicion == "INACTIVO_A_ACTIVO") %>%
          mutate(estado = Transicion),
        ns("click_alerta_activo")
      )
    })
    CajaModal(
      "kpi_a_activo",
      valor   = reactive(html_valor(
        transiciones() %>% filter(Transicion == "INACTIVO_A_ACTIVO") %>% nrow(),
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
    
    # Tabla de clientes nuevos absolutos
    output$tbl_nuevo <- reactable::renderReactable({
      req(transiciones())
      .reactable_uc(
        transiciones() %>%
          filter(Transicion == "NUEVO_ABSOLUTO") %>%
          mutate(estado = Transicion),
        ns("click_alerta_nuevo")
      )
    })
    CajaModal(
      "kpi_nuevo",
      valor   = reactive(html_valor(
        transiciones() %>% filter(Transicion == "NUEVO_ABSOLUTO") %>% nrow(),
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
        "Creados en t4 en los \u00faltimos 2 meses, primera factura este mes"
      ),
      footer_class    = "caja-modal-footer"
    )
    
    # Tabla de reactivados sin historial CRM
    output$tbl_reactivado <- reactable::renderReactable({
      req(transiciones())
      .reactable_uc(
        transiciones() %>%
          filter(Transicion == "REACTIVADO_SIN_CRM") %>%
          mutate(estado = Transicion),
        ns("click_alerta_reactivado")
      )
    })
    CajaModal(
      "kpi_reactivado",
      valor   = reactive(html_valor(
        transiciones() %>% filter(Transicion == "REACTIVADO_SIN_CRM") %>% nrow(),
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
    
    ## Bloque 2: Indicadores dinámicos globales ----
    
    CajaModal(
      "kpi_retencion",
      valor   = reactive(html_valor(
        ind_full()$retencion %||% 0, "porcentaje",
        color = .color_kpi(ind_full()$retencion, "retencion")
      )),
      texto = "Retenci\u00f3n", icono = "shield-alt",
      colores = c(fondo = "white"), mostrar_boton = FALSE
    )
    CajaModal(
      "kpi_perdida",
      valor   = reactive(html_valor(
        ind_full()$perdida %||% 0, "porcentaje",
        color = .color_kpi(ind_full()$perdida, "perdida")
      )),
      texto = "P\u00e9rdida", icono = "user-minus",
      colores = c(fondo = "white"), mostrar_boton = FALSE
    )
    CajaModal(
      "kpi_reactivacion",
      valor   = reactive(html_valor(
        ind_full()$reactivacion %||% 0, "porcentaje",
        color = .color_kpi(ind_full()$reactivacion, "reactivacion")
      )),
      texto = "Reactivaci\u00f3n", icono = "sync-alt",
      colores = c(fondo = "white"), mostrar_boton = FALSE
    )
    CajaModal(
      "kpi_tasa_fact",
      valor   = reactive(html_valor(
        tasa_fact_global() %||% 0, "porcentaje",
        color = .color_kpi(tasa_fact_global(), "tasa_fact")
      )),
      texto = "Tasa Facturaci\u00f3n", icono = "percentage",
      colores = c(fondo = "white"), mostrar_boton = FALSE
    )
    
    ## Bloque 3: Cajas por población (renderUI reactivo) ----
    
    output$lbl_mes_vigente <- renderUI({
      paste0("Mes Vigente \u2014 ", format(mes_vigente(), "%B %Y"))
    })
    
    # Fuente de valores para CajaModal por población — reactivos con cierre sobre pan_r/ind_r
    .vals_poblacion <- function(pan_r, ind_r) {
      list(
        enero = reactive({
          req(pan_r(), mes_inicio(), tasa_fact_uc())
          corte <- pan_r() %>% filter(ym == mes_inicio())
          list(
            activo    = n_distinct(corte$cliente_id[corte$estado == "CLIENTE ACTIVO"]),
            recuperar = n_distinct(
              corte$cliente_id[corte$estado == "CLIENTE A RECUPERAR"]
            ),
            nuevos    = n_distinct(
              corte$cliente_id[corte$tipo_cohorte == "ALTA EN COHORTE"]
            ),
            tasa      = mean(
              tasa_fact_uc() %>%
                semi_join(corte, by = "cliente_id") %>%
                pull(tasa_facturacion),
              na.rm = TRUE
            )
          )
        }),
        vigente = reactive({
          req(pan_r(), tasa_fact_uc())
          corte <- pan_r() %>% filter(ym == max(pan_r()$ym))
          list(
            activo    = n_distinct(corte$cliente_id[corte$estado == "CLIENTE ACTIVO"]),
            recuperar = n_distinct(
              corte$cliente_id[corte$estado == "CLIENTE A RECUPERAR"]
            ),
            nuevos    = n_distinct(
              corte$cliente_id[corte$tipo_cohorte == "ALTA EN COHORTE"]
            ),
            tasa      = mean(
              tasa_fact_uc() %>%
                semi_join(corte, by = "cliente_id") %>%
                pull(tasa_facturacion),
              na.rm = TRUE
            )
          )
        }),
        ind = ind_r
      )
    }
    
    # Mapa de valores para las tres poblaciones posibles — llaves sanitizadas
    vals_pob <- list(
      TOTAL            = .vals_poblacion(panel_full, ind_full),
      PRESUPUESTADA    = .vals_poblacion(panel_p,    ind_p),
      NO_PRESUPUESTADA = .vals_poblacion(panel_np,   ind_np)
    )
    
    n_distinct_uc <- function(df) dplyr::n_distinct(df$cliente_id)
    
    # Pre-registro de CajaModal para todas las poblaciones posibles.
    # Se registran las tres siempre; Shiny evalúa outputs lazy,
    # por lo que las cajas ocultas no generan cómputo hasta que se muestran.
    for (pop_lbl in c("TOTAL", "PRESUPUESTADA", "NO PRESUPUESTADA")) {
      local({
        p   <- pop_lbl
        pid <- .pob_id(p)
        vp  <- vals_pob[[pid]]
        
        # Cajas Enero
        CajaModal(
          paste0("kpi_enero_activo_", pid),
          valor        = reactive(
            html_valor(vp$enero()$activo, "numero", color = "#2C7BB6")
          ),
          texto = "Activos Enero", icono = "user-check",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
        CajaModal(
          paste0("kpi_enero_recuperar_", pid),
          valor        = reactive(
            html_valor(vp$enero()$recuperar, "numero", color = "#F4A820")
          ),
          texto = "A Recuperar Enero", icono = "user-clock",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
        CajaModal(
          paste0("kpi_enero_nuevos_", pid),
          valor        = reactive(
            html_valor(vp$enero()$nuevos, "numero", color = "#27AE60")
          ),
          texto = "Nuevos Enero", icono = "user-plus",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
        CajaModal(
          paste0("kpi_enero_tasa_", pid),
          valor        = reactive(html_valor(
            vp$enero()$tasa %||% 0, "porcentaje",
            color = .color_kpi(vp$enero()$tasa, "tasa_fact")
          )),
          texto = "Tasa Facturaci\u00f3n", icono = "percentage",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
        
        # Cajas Vigente
        CajaModal(
          paste0("kpi_vig_activo_", pid),
          valor        = reactive(
            html_valor(vp$vigente()$activo, "numero", color = "#2C7BB6")
          ),
          texto = "Activos", icono = "user-check",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
        CajaModal(
          paste0("kpi_vig_recuperar_", pid),
          valor        = reactive(
            html_valor(vp$vigente()$recuperar, "numero", color = "#F4A820")
          ),
          texto = "A Recuperar", icono = "user-clock",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
        CajaModal(
          paste0("kpi_vig_nuevos_", pid),
          valor        = reactive(
            html_valor(vp$vigente()$nuevos, "numero", color = "#27AE60")
          ),
          texto = "Nuevos", icono = "user-plus",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
        CajaModal(
          paste0("kpi_vig_tasa_", pid),
          valor        = reactive(html_valor(
            vp$vigente()$tasa %||% 0, "porcentaje",
            color = .color_kpi(vp$vigente()$tasa, "tasa_fact")
          )),
          texto = "Tasa Facturaci\u00f3n", icono = "percentage",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
        
        # Cajas Dinámicos
        CajaModal(
          paste0("kpi_din_ret_", pid),
          valor        = reactive(html_valor(
            vp$ind()$retencion %||% 0, "porcentaje",
            color = .color_kpi(vp$ind()$retencion, "retencion")
          )),
          texto = "Retenci\u00f3n", icono = "shield-alt",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
        CajaModal(
          paste0("kpi_din_per_", pid),
          valor        = reactive(html_valor(
            vp$ind()$perdida %||% 0, "porcentaje",
            color = .color_kpi(vp$ind()$perdida, "perdida")
          )),
          texto = "P\u00e9rdida", icono = "user-minus",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
        CajaModal(
          paste0("kpi_din_rea_", pid),
          valor        = reactive(html_valor(
            vp$ind()$reactivacion %||% 0, "porcentaje",
            color = .color_kpi(vp$ind()$reactivacion, "reactivacion")
          )),
          texto = "Reactivaci\u00f3n", icono = "sync-alt",
          colores = c(fondo = "white"), mostrar_boton = FALSE
        )
      })
    }
    
    # Renderiza filas de cajas según checkboxes activos — IDs sanitizados
    output$cajas_enero <- renderUI({
      pobs <- poblacion_activa()
      cols <- lapply(pobs, function(p) {
        pid <- .pob_id(p)
        column(
          12 / length(pobs),
          h6(p, style = "font-weight:700; color:#2C7BB6; text-align:center;"),
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
          h6(p, style = "font-weight:700; color:#F4A820; text-align:center;"),
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
    
    ## Tabla resumen mensual (estados en filas, meses en columnas) ----
    output$tabla_resumen_mensual <- reactable::renderReactable({
      req(panel_activo())
      tab <- .tabla_resumen(panel_activo())
      # Columnas dinámicas: una por mes
      col_mes <- lapply(
        setdiff(names(tab), "estado"),
        function(m) {
          reactable::colDef(
            name     = m,
            minWidth = 70,
            # Clic en celda → modal con detalle de clientes
            cell = function(value, index) {
              estado_fila <- tab$estado[[index]]
              tags$span(
                style   = "cursor:pointer; font-weight:600;",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', {estado: '%s', mes: '%s', nonce: Math.random()},
                   {priority: 'event'})",
                  ns("click_celda_resumen"), estado_fila, m
                ),
                format(value, big.mark = ".")
              )
            }
          )
        }
      )
      names(col_mes) <- setdiff(names(tab), "estado")
      reactable::reactable(
        tab,
        columns  = c(
          list(estado = reactable::colDef(name = "Estado", minWidth = 180, sticky = "left")),
          col_mes
        ),
        highlight = TRUE, compact = TRUE, bordered = TRUE,
        pagination = FALSE
      )
    })
    
    # Modal al hacer clic en celda de tabla resumen
    observeEvent(input$click_celda_resumen, {
      req(!is.null(input$click_celda_resumen$estado))
      estado_sel <- input$click_celda_resumen$estado
      mes_sel    <- input$click_celda_resumen$mes
      # Reconstruir fecha desde etiqueta "Ene-25"
      fecha_sel <- tryCatch(
        as.Date(paste0("01-", mes_sel), format = "%d-%b-%y"),
        error = function(e) NULL
      )
      req(!is.null(fecha_sel))
      
      # Clientes del panel en el corte seleccionado (panel ya trae atributos slim)
      clientes_mes <- panel_activo() %>%
        filter(ym == fecha_sel) %>%
        {
          if (estado_sel == "TOTAL") .
          else filter(., estado == estado_sel)
        } %>%
        select(cliente_id, CliNitPpal, LinNegCod, PerRazSoc, Asesor, Segmento, estado)
      
      # Transacciones del mes: última factura, sacos y margen acumulados del mes
      tx_mes <- tx_limpia() %>%
        filter(ym == fecha_sel) %>%
        semi_join(clientes_mes, by = "cliente_id") %>%
        group_by(cliente_id, CliNitPpal, LinNegCod) %>%
        summarise(
          ultima_fec_fact = max(FecFact, na.rm = TRUE),
          sacos           = sum(coalesce(SacFact70, 0), na.rm = TRUE),
          margen          = sum(coalesce(Margen,    0), na.rm = TRUE),
          .groups = "drop"
        )
      
      # Última fecha de pedido: se toma de data_tx completo si la columna existe
      tiene_pedido <- "FecPedido" %in% names(data_tx())
      if (tiene_pedido) {
        pedido_mes <- data_tx() %>%
          filter(!is.na(FecPedido), PrimerDia(FecFact) == fecha_sel) %>%
          semi_join(clientes_mes, by = "cliente_id") %>%
          group_by(cliente_id) %>%
          summarise(ultima_fec_pedido = max(FecPedido, na.rm = TRUE), .groups = "drop")
        tx_mes <- left_join(tx_mes, pedido_mes, by = "cliente_id")
      } else {
        tx_mes <- mutate(tx_mes, ultima_fec_pedido = NA_Date_)
      }
      
      # Panel enriquecido: atributos del cliente + métricas del mes
      data_drill <- clientes_mes %>%
        left_join(
          tx_mes %>% select(cliente_id, ultima_fec_pedido, ultima_fec_fact,
                            sacos, margen),
          by = "cliente_id"
        )
      
      # Reactable de drill-down con columnas extendidas
      showModal(modalDialog(
        title     = tagList(
          icon("users"), " ", estado_sel, " \u2014 ", mes_sel
        ),
        size      = "xl", easyClose = TRUE, footer = modalButton("Cerrar"),
        reactable::reactable(
          data_drill,
          columns = list(
            cliente_id        = reactable::colDef(show = FALSE),
            PerRazSoc         = reactable::colDef(name = "Raz\u00f3n Social",
                                                  minWidth = 200, sticky = "left"),
            CliNitPpal        = reactable::colDef(name = "NIT",          minWidth = 110),
            LinNegCod         = reactable::colDef(name = "L\u00ednea",   minWidth = 80),
            Segmento          = reactable::colDef(name = "Segmento",     minWidth = 100),
            Asesor            = reactable::colDef(name = "Asesor",       minWidth = 120),
            estado            = reactable::colDef(name = "Estado",       minWidth = 140),
            ultima_fec_pedido = reactable::colDef(
              name     = "\u00dalt. Pedido",    minWidth = 110,
              format   = reactable::colFormat(date = TRUE, locales = "es-CO")
            ),
            ultima_fec_fact   = reactable::colDef(
              name     = "\u00dalt. Facturaci\u00f3n", minWidth = 120,
              format   = reactable::colFormat(date = TRUE, locales = "es-CO")
            ),
            sacos             = reactable::colDef(
              name   = "Sacos",      minWidth = 80,
              format = reactable::colFormat(separators = TRUE, digits = 1)
            ),
            margen            = reactable::colDef(
              name   = "Margen ($)", minWidth = 110,
              format = reactable::colFormat(separators = TRUE, digits = 0, prefix = "$")
            )
          ),
          highlight  = TRUE, compact = TRUE, bordered = TRUE,
          pagination = FALSE, searchable = TRUE, filterable = TRUE
        )
      ))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    ## Gráficos evolución y permanencia ----
    
    output$graf_evolucion <- renderPlotly({
      req(panel_activo())
      df <- panel_activo() %>%
        group_by(ym, estado) %>%
        summarise(n = n_distinct(cliente_id), .groups = "drop") %>%
        mutate(
          estado = factor(estado, levels = c(
            "CLIENTE ACTIVO", "CLIENTE A RECUPERAR", "NUEVO DEL PERIODO"
          )),
          mes_lbl = format(ym, "%b-%y")
        )
      colores <- c(
        "CLIENTE ACTIVO"      = "#2C7BB6",
        "CLIENTE A RECUPERAR" = "#F4A820",
        "NUEVO DEL PERIODO"   = "#27AE60"
      )
      plotly::plot_ly(df,
                      x     = ~mes_lbl, y = ~n, color = ~estado,
                      colors = colores, type = "bar"
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis   = list(title = "", tickangle = -45),
          yaxis   = list(title = "Clientes"),
          legend  = list(orientation = "h", y = -0.25),
          margin  = list(l = 10, r = 10, t = 30, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$graf_permanencia <- renderPlotly({
      req(tasa_fact_uc(), panel_activo())
      df <- tasa_fact_uc() %>%
        semi_join(panel_activo() %>% distinct(cliente_id), by = "cliente_id") %>%
        mutate(
          Rango = cut(
            tasa_facturacion,
            breaks = c(0, .25, .5, .75, 1.001),
            labels = c("0-25%", "25-50%", "50-75%", "75-100%"),
            right  = FALSE
          )
        ) %>%
        count(Rango)
      plotly::plot_ly(df,
                      x     = ~Rango, y = ~n, type = "bar",
                      marker = list(color = "#2C7BB6")
      ) %>%
        plotly::layout(
          xaxis  = list(title = "Tasa de Facturaci\u00f3n"),
          yaxis  = list(title = "Clientes"),
          margin = list(l = 10, r = 10, t = 30, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Descargas ----
    
    output$dl_poblacion <- .dl_handler(
      function() panel_activo() %>%
        left_join(tasa_fact_uc(), by = "cliente_id") %>%
        arrange(cliente_id, ym),
      "poblacion"
    )
    
    output$dl_altas <- .dl_handler(
      function() panel_altas() %>%
        left_join(catalogo_slim(), by = "cliente_id") %>%
        left_join(tasa_fact_uc(), by = "cliente_id") %>%
        arrange(cliente_id, ym),
      "altas_cohorte"
    )
    
  })
}

# App de prueba ----
options(OutDec = ",") 
ui <- bs4DashPage(
  title = "Presupuesto", header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(), controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body   = bs4DashBody(useShinyjs(), 
                       includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
                       CohortesUI("Prueba"))
)
server <- function(input, output, session) {
  Cohortes("Prueba", reactive({BaseDatos_c}), reactive({c(as.Date("2026-01-01"), as.Date("2026-05-16"))}))
}
shinyApp(ui, server)
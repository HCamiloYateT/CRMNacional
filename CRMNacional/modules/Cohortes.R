# ==============================================================================
# MODULO: CohortesUnificado
# Refactorizado con Racafe + racafeModulos
# Cambios principales:
#   - CajaModal / CajaModalUI reemplazan renderbs4ValueBox + observeEvent manual
#   - html_valor / html_texto reemplazan .caja_kpi con formato manual
#   - BotonDescarga reemplaza downloadButton con clases manuales
#   - .caja_con_boton y .caja_kpi eliminados
#   - .show_modal conservado solo para modales desde tabla resumen y rangos
#     (son dinamicos: el contenido varia segun el click, no se puede declarar
#     estaticamente como contenido_modal; CajaModal no aplica ahi)
# ==============================================================================

library(shiny)
library(bs4Dash)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(gt)
library(reactable)
library(writexl)
library(scales)
library(stringr)
library(racafeModulos)

# ------------------------------------------------------------------------------
# Helpers estaticos
# ------------------------------------------------------------------------------

# .to_ym eliminado — reemplazado por PrimerDia() del paquete Racafe
# %||%   eliminado — se usa el exportado por el paquete Racafe

.ESTADOS <- c(
  "CLIENTE ACTIVO"      = "#2C7BB6",
  "CLIENTE A RECUPERAR" = "#F4A820",
  "NUEVO DEL PERIODO"   = "#27AE60"
)

n_distinct_uc <- function(df) dplyr::n_distinct(df$cliente_id)
# ------------------------------------------------------------------------------
# Paleta semafórica — devuelve hex segun tipo de indicador y valor
# Conservada: la usan html_valor (color), .color_cumpl_bg y .color_tasa_bg
# ------------------------------------------------------------------------------

.color_kpi <- function(v, tipo = c("retencion", "perdida", "reactivacion", "tasa")) {
  tipo <- match.arg(tipo)
  if (is.null(v) || is.na(v)) return("#AAAAAA")
  switch(tipo,
         retencion    = if (v >= 80) "#27AE60" else if (v >= 60) "#F4A820" else "#E74C3C",
         perdida      = if (v <= 10) "#27AE60" else if (v <= 25) "#F4A820" else "#E74C3C",
         reactivacion = if (v >= 30) "#27AE60" else if (v >= 15) "#F4A820" else "#E74C3C",
         tasa         = if (v >= 75) "#27AE60" else if (v >= 50) "#F4A820" else "#E74C3C"
  )
}

# ------------------------------------------------------------------------------
# Helpers de formato — para reactable y gt
# ------------------------------------------------------------------------------

.fmt_pct <- function(x) ifelse(is.na(x), "\u2014", paste0(round(x, 1), "%"))
.fmt_num <- function(x) format(round(x, 0), big.mark = ",", scientific = FALSE)

.color_cumpl_bg <- function(pct) {
  dplyr::case_when(
    is.na(pct) ~ "#F8F9FA",
    pct >= 90  ~ "#D5F5E3",
    pct >= 70  ~ "#FCF3CF",
    TRUE       ~ "#FADBD8"
  )
}

.color_tasa_bg <- function(pct) {
  dplyr::case_when(
    is.na(pct) ~ "#F8F9FA",
    pct >= 75  ~ "#D5F5E3",
    pct >= 50  ~ "#FCF3CF",
    TRUE       ~ "#FADBD8"
  )
}

.kpi_nota <- function(texto) {
  p(texto,
    style = "font-size:10px; color:#999; margin-top:-6px; margin-bottom:6px; font-style:italic;")
}

# ------------------------------------------------------------------------------
# Reactable de unidades comerciales
# ------------------------------------------------------------------------------

.reactable_uc <- function(data, input_id) {
  
  tiene_ppto <- all(c("ppto_sacos", "ppto_margen",
                      "cumpl_sacos_pct", "cumpl_margen_pct") %in% names(data))
  
  js_click <- paste0(
    "function(rowInfo) {",
    "  Shiny.setInputValue('", input_id, "', {",
    "    cliente_id: rowInfo.values['cliente_id'],",
    "    nonce: Math.random()",
    "  }, {priority: 'event'});",
    "}"
  )
  
  cols_base <- list(
    cliente_id    = reactable::colDef(show = FALSE),
    PerRazSoc     = reactable::colDef(name = "Razon Social",      minWidth = 180),
    Asesor        = reactable::colDef(name = "Asesor",             minWidth = 130),
    Segmento      = reactable::colDef(name = "Segmento",           minWidth = 110),
    CliNitPpal    = reactable::colDef(name = "NIT Principal",      minWidth = 110),
    LinNegCod     = reactable::colDef(name = "Linea de Negocio",   minWidth = 100),
    presupuestada = reactable::colDef(name = "Presupuestada",      minWidth = 110),
    tipo_cohorte  = reactable::colDef(name = "Cohorte",            minWidth = 110),
    estado        = reactable::colDef(
      name     = "Estado",
      minWidth = 140,
      style    = function(value) {
        bg <- switch(value,
                     "CLIENTE ACTIVO"      = "#EFF6FF",
                     "CLIENTE A RECUPERAR" = "#FFF8EC",
                     "NUEVO DEL PERIODO"   = "#EDFBF2",
                     "white"
        )
        list(background = bg, fontWeight = "500")
      }
    ),
    fecha_ref     = reactable::colDef(
      name     = "Fecha ref.",
      minWidth = 105,
      cell     = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%b %Y")
    ),
    sacos_total   = reactable::colDef(
      name = "Sacos reales", minWidth = 100,
      cell = function(v) .fmt_num(v)
    ),
    margen_total  = reactable::colDef(
      name = "Margen real", minWidth = 110,
      cell = function(v) .fmt_num(v)
    ),
    tasa_fact_pct = reactable::colDef(
      name  = "Tasa facturacion", minWidth = 110,
      cell  = function(v) .fmt_pct(v),
      style = function(v) list(background = .color_tasa_bg(v))
    )
  )
  
  cols_ppto <- if (tiene_ppto) list(
    ppto_sacos       = reactable::colDef(
      name = "Ppto sacos",  minWidth = 100,
      cell = function(v) .fmt_num(v)
    ),
    ppto_margen      = reactable::colDef(
      name = "Ppto margen", minWidth = 110,
      cell = function(v) .fmt_num(v)
    ),
    cumpl_sacos_pct  = reactable::colDef(
      name  = "% Sacos",  minWidth = 80,
      cell  = function(v) .fmt_pct(v),
      style = function(v) list(background = .color_cumpl_bg(v))
    ),
    cumpl_margen_pct = reactable::colDef(
      name  = "% Margen", minWidth = 80,
      cell  = function(v) .fmt_pct(v),
      style = function(v) list(background = .color_cumpl_bg(v))
    )
  ) else list()
  
  reactable::reactable(
    data            = data,
    columns         = c(cols_base, cols_ppto),
    onClick         = JS(js_click),
    highlight       = TRUE,
    striped         = FALSE,
    bordered        = TRUE,
    compact         = TRUE,
    searchable      = TRUE,
    defaultPageSize = 15,
    theme           = reactable::reactableTheme(
      headerStyle = list(fontWeight = "600", fontSize = "12px"),
      cellStyle   = list(fontSize = "12px", cursor = "pointer")
    )
  )
}

# ------------------------------------------------------------------------------
# GT cumplimiento mensual por unidad comercial
# ------------------------------------------------------------------------------

.gt_cumpl_uc <- function(panel_c, cliente, razon_social, meses_disp) {
  data <- panel_c %>%
    filter(cliente_id == cliente, ym %in% meses_disp) %>%
    arrange(ym) %>%
    transmute(
      Mes           = format(ym, "%B %Y"),
      `Ppto Sacos`  = round(ppto_sacos_mes,  0),
      `Real Sacos`  = round(real_sacos,       0),
      `% Sacos`     = cumpl_sacos_pct,
      `Ppto Margen` = round(ppto_margen_mes,  0),
      `Real Margen` = round(real_margen,      0),
      `% Margen`    = cumpl_margen_pct
    )
  
  total <- data %>%
    summarise(
      Mes           = "Total periodo",
      `Ppto Sacos`  = sum(`Ppto Sacos`,  na.rm = TRUE),
      `Real Sacos`  = sum(`Real Sacos`,  na.rm = TRUE),
      `% Sacos`     = round(
        sum(`Real Sacos`,  na.rm = TRUE) /
          pmax(sum(`Ppto Sacos`,  na.rm = TRUE), 1) * 100, 1
      ),
      `Ppto Margen` = sum(`Ppto Margen`, na.rm = TRUE),
      `Real Margen` = sum(`Real Margen`, na.rm = TRUE),
      `% Margen`    = round(
        sum(`Real Margen`, na.rm = TRUE) /
          pmax(sum(`Ppto Margen`, na.rm = TRUE), 1) * 100, 1
      )
    )
  
  bind_rows(data, total) %>%
    gt(rowname_col = "Mes") %>%
    tab_header(
      title    = razon_social,
      subtitle = paste0("Unidad Comercial: ", cliente)
    ) %>%
    tab_spanner(label = "Sacos",  columns = c(`Ppto Sacos`,  `Real Sacos`,  `% Sacos`)) %>%
    tab_spanner(label = "Margen", columns = c(`Ppto Margen`, `Real Margen`, `% Margen`)) %>%
    fmt_number(
      columns  = c(`Ppto Sacos`, `Real Sacos`, `Ppto Margen`, `Real Margen`),
      decimals = 0, sep_mark = ",", dec_mark = "."
    ) %>%
    fmt_number(columns = c(`% Sacos`, `% Margen`), decimals = 1, dec_mark = ".") %>%
    data_color(
      columns = `% Sacos`,
      fn      = scales::col_bin(
        palette = c("#FADBD8", "#FCF3CF", "#D5F5E3"),
        bins = c(-Inf, 70, 90, Inf), na.color = "#F8F9FA"
      )
    ) %>%
    data_color(
      columns = `% Margen`,
      fn      = scales::col_bin(
        palette = c("#FADBD8", "#FCF3CF", "#D5F5E3"),
        bins = c(-Inf, 70, 90, Inf), na.color = "#F8F9FA"
      )
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(rows = Mes == "Total periodo")
    ) %>%
    tab_style(cell_text(weight = "bold"), cells_stub()) %>%
    tab_options(
      table.font.size              = px(12),
      column_labels.font.weight    = "bold",
      heading.title.font.size      = px(13),
      heading.title.font.weight    = "bold",
      table.width                  = pct(100)
    )
}

# ==============================================================================
# UI
# ==============================================================================

CohortesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # ── KPIs — Poblacion Base ─────────────────────────────────────────────────
    # Usa CajaModalUI de racafeModulos en lugar de bs4ValueBoxOutput
    bs4Dash::bs4Card(
      title        = tagList(icon("users"), " Poblacion Base"),
      width        = 12, solidHeader = TRUE, status = "white", collapsible = TRUE,
      fluidRow(
        column(3, CajaModalUI(ns("kpi_baseline"))),
        column(3, CajaModalUI(ns("kpi_presup"))),
        column(3, CajaModalUI(ns("kpi_no_presup"))),
        column(3, CajaModalUI(ns("kpi_nuevos")))
      )
    ),
    
    # ── KPIs — Indicadores Dinamicos Globales ─────────────────────────────────
    # KPIs semafóricos sin boton: CajaModalUI con mostrar_boton = FALSE
    bs4Dash::bs4Card(
      title        = tagList(icon("chart-line"),
                             " Indicadores Dinamicos de Cohorte — ultimo mes"),
      width        = 12, solidHeader = TRUE, status = "white", collapsible = TRUE,
      fluidRow(
        column(3, CajaModalUI(ns("kpi_retencion"))),
        column(3, CajaModalUI(ns("kpi_perdida"))),
        column(3, CajaModalUI(ns("kpi_reactivacion"))),
        column(3, CajaModalUI(ns("kpi_tasa_fact")))
      ),
      fluidRow(
        column(3, .kpi_nota("Activos t que permanecen Activos en t+1 / Total Activos en t")),
        column(3, .kpi_nota("Activos t que pasan a A Recuperar en t+1 / Total Activos en t")),
        column(3, .kpi_nota("A Recuperar en t que pasan a Activos en t+1 / Total A Recuperar en t")),
        column(3, .kpi_nota("Promedio de (meses con facturacion / meses en cohorte) por UC"))
      )
    ),
    
    # ── PRESUPUESTADOS ────────────────────────────────────────────────────────
    bs4Dash::bs4Card(
      title        = tagList(icon("bullseye"), " Unidades Comerciales Presupuestadas"),
      width        = 12, solidHeader = TRUE, status = "white", collapsible = TRUE,
      
      h6(uiOutput(ns("lbl_inicio_p")),
         style = "font-weight:600; color:#2C7BB6; margin-bottom:4px;"),
      fluidRow(
        column(4, CajaModalUI(ns("kpi_p_activo_ini"))),
        column(4, CajaModalUI(ns("kpi_p_recuperar_ini"))),
        column(4, CajaModalUI(ns("kpi_p_tasa_fact_ini")))
      ),
      h6(uiOutput(ns("lbl_fin_p")),
         style = "font-weight:600; color:#F4A820; margin-bottom:4px; margin-top:8px;"),
      fluidRow(
        column(4, CajaModalUI(ns("kpi_p_activo_fin"))),
        column(4, CajaModalUI(ns("kpi_p_recuperar_fin"))),
        column(4, CajaModalUI(ns("kpi_p_tasa_fact_fin")))
      ),
      
      h6("Distribucion por Tasa de Facturacion — Presupuestados",
         style = "font-weight:600; color:#555; margin-top:10px; margin-bottom:4px;"),
      reactable::reactableOutput(ns("tabla_p_rangos_fact")),
      p(icon("hand-pointer"),
        " Haga clic en una fila para ver el detalle de las Unidades Comerciales",
        style = "font-size:11px; color:#888; margin-top:4px; margin-bottom:8px;"),
      
      h6("Indicadores Dinamicos — Presupuestados",
         style = "font-weight:600; color:#555; margin-top:6px;"),
      fluidRow(
        column(4, CajaModalUI(ns("kpi_p_retencion"))),
        column(4, CajaModalUI(ns("kpi_p_perdida"))),
        column(4, CajaModalUI(ns("kpi_p_reactivacion")))
      ),
      fluidRow(
        column(4, .kpi_nota("Activos t → Activos t+1 / Total Activos en t")),
        column(4, .kpi_nota("Activos t → A Recuperar t+1 / Total Activos en t")),
        column(4, .kpi_nota("A Recuperar t → Activos t+1 / Total A Recuperar en t"))
      ),
      
      fluidRow(
        column(7, plotly::plotlyOutput(ns("graf_p_evolucion"),   height = "300px")),
        column(5, plotly::plotlyOutput(ns("graf_p_permanencia"), height = "300px"))
      ),
      hr(),
      h5("Resumen mensual — Estado de la Poblacion Presupuestada",
         style = "font-weight:600; margin-bottom:4px;"),
      p(icon("hand-pointer"),
        " Haga clic en una celda para ver el detalle de las Unidades Comerciales",
        style = "font-size:11px; color:#888; margin-bottom:8px;"),
      reactable::reactableOutput(ns("tabla_p_resumen")),
      hr(),
      h5("Matriz de Transicion de Estado — Presupuestados",
         style = "font-weight:600; margin-bottom:4px;"),
      uiOutput(ns("lbl_transicion_p")),
      fluidRow(
        column(6, gt::gt_output(ns("tabla_p_transicion"))),
        column(6, plotly::plotlyOutput(ns("sankey_p"), height = "400px"))
      ),
      br(),
      # BotonDescarga de Racafe reemplaza downloadButton manual
      BotonDescarga("dl_presup", color = "#2C7BB6", ns = ns,
                    title = "Descargar Unidades Presupuestadas")
    ),
    
    # ── NO PRESUPUESTADOS ─────────────────────────────────────────────────────
    bs4Dash::bs4Card(
      title        = tagList(icon("user-clock"), " Unidades Comerciales No Presupuestadas"),
      width        = 12, solidHeader = TRUE, status = "white", collapsible = TRUE,
      
      h6(uiOutput(ns("lbl_inicio_np")),
         style = "font-weight:600; color:#2C7BB6; margin-bottom:4px;"),
      fluidRow(
        column(3, CajaModalUI(ns("kpi_np_activo_ini"))),
        column(3, CajaModalUI(ns("kpi_np_recuperar_ini"))),
        column(3, CajaModalUI(ns("kpi_np_nuevos_ini"))),
        column(3, CajaModalUI(ns("kpi_np_tasa_fact")))
      ),
      h6(uiOutput(ns("lbl_fin_np")),
         style = "font-weight:600; color:#F4A820; margin-bottom:4px; margin-top:8px;"),
      fluidRow(
        column(3, CajaModalUI(ns("kpi_np_activo_fin"))),
        column(3, CajaModalUI(ns("kpi_np_recuperar_fin"))),
        column(3, CajaModalUI(ns("kpi_np_nuevos_fin"))),
        # kpi_np_tasa_fact es el mismo ID en inicio y fin (sin boton, solo lectura)
        column(3, CajaModalUI(ns("kpi_np_tasa_fact_fin")))
      ),
      
      h6("Distribucion por Tasa de Facturacion — No Presupuestados",
         style = "font-weight:600; color:#555; margin-top:10px; margin-bottom:4px;"),
      reactable::reactableOutput(ns("tabla_np_rangos_fact")),
      p(icon("hand-pointer"),
        " Haga clic en una fila para ver el detalle de las Unidades Comerciales",
        style = "font-size:11px; color:#888; margin-top:4px; margin-bottom:8px;"),
      
      h6("Indicadores Dinamicos — No Presupuestados",
         style = "font-weight:600; color:#555; margin-top:6px;"),
      fluidRow(
        column(4, CajaModalUI(ns("kpi_np_retencion"))),
        column(4, CajaModalUI(ns("kpi_np_perdida"))),
        column(4, CajaModalUI(ns("kpi_np_reactivacion")))
      ),
      fluidRow(
        column(4, .kpi_nota("Activos t → Activos t+1 / Total Activos en t")),
        column(4, .kpi_nota("Activos t → A Recuperar t+1 / Total Activos en t")),
        column(4, .kpi_nota("A Recuperar t → Activos t+1 / Total A Recuperar en t"))
      ),
      
      fluidRow(
        column(7, plotly::plotlyOutput(ns("graf_np_evolucion"),   height = "300px")),
        column(5, plotly::plotlyOutput(ns("graf_np_permanencia"), height = "300px"))
      ),
      hr(),
      h5("Resumen mensual — Estado de la Poblacion No Presupuestada",
         style = "font-weight:600; margin-bottom:4px;"),
      p(icon("hand-pointer"),
        " Haga clic en una celda para ver el detalle de las Unidades Comerciales",
        style = "font-size:11px; color:#888; margin-bottom:8px;"),
      reactable::reactableOutput(ns("tabla_np_resumen")),
      hr(),
      h5("Matriz de Transicion de Estado — No Presupuestados",
         style = "font-weight:600; margin-bottom:4px;"),
      uiOutput(ns("lbl_transicion_np")),
      fluidRow(
        column(6, gt::gt_output(ns("tabla_np_transicion"))),
        column(6, plotly::plotlyOutput(ns("sankey_np"), height = "400px"))
      ),
      br(),
      fluidRow(
        column(4, BotonDescarga("dl_np_todos",     color = "#F4A820", ns = ns,
                                title = "Descargar No Presupuestados")),
        column(4, BotonDescarga("dl_np_nuevos",    color = "#27AE60", ns = ns,
                                title = "Descargar Altas en Cohorte")),
        column(4, BotonDescarga("dl_np_recuperar", color = "#E74C3C", ns = ns,
                                title = "Descargar Clientes a Recuperar"))
      )
    )
  )
}

# ==============================================================================
# Configuracion de presupuestos grupales
# ==============================================================================

.PPTO_GRUPO <- tibble::tribble(
  ~LinNegCod, ~grupo,                  ~ppto_sacos_anual, ~ppto_margen_anual,
  1,          "NUEVO DEL PERIODO",      120000,            15000000,
  2,          "NUEVO DEL PERIODO",       80000,            10000000,
  1,          "CLIENTE A RECUPERAR",    200000,            25000000,
  2,          "CLIENTE A RECUPERAR",    150000,            18000000
) %>% mutate(LinNegCod = as.double(LinNegCod))

# ==============================================================================
# SERVER
# ==============================================================================

Cohortes <- function(id, data_tx, fecha_rango) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Carga y preparacion de CRMNALSEGR ────────────────────────────────────
    
    crm_data <- reactive({
      CargarDatos("CRMNALSEGR") %>%
        mutate(
          FecProceso = as.Date(FecProceso),
          cliente_id = paste(CliNitPpal, LinNegCod, sep = "_")
        )
    }) %>% bindCache("CRMNALSEGR")
    
    meses_crm_disponibles <- reactive({
      req(crm_data())
      crm_data() %>%
        distinct(FecProceso) %>%
        pull(FecProceso) %>%
        PrimerDia(uni = "month") %>%
        unique() %>%
        sort()
    })
    
    # ── Periodo: gobernado por el input global FT_Fecha ───────────────────────
    
    mes_inicio    <- reactive({
      req(fecha_rango())
      PrimerDia(fecha_rango()[1], uni = "month")
    })
    mes_fin       <- reactive({
      req(fecha_rango())
      PrimerDia(fecha_rango()[2], uni = "month")
    })
    meses_periodo <- reactive({
      req(mes_inicio(), mes_fin())
      seq.Date(mes_inicio(), mes_fin(), by = "month")
    })
    n_meses       <- reactive({ length(meses_periodo()) })
    
    # ── Transacciones limpias ─────────────────────────────────────────────────
    
    tx_limpia <- reactive({
      req(data_tx())
      data_tx() %>%
        filter(Excluir == "NO", ProdExcluir == "NO", !is.na(FecFact)) %>%
        mutate(
          FecFact    = as.Date(FecFact),
          ym         = PrimerDia(FecFact, uni = "month"),
          cliente_id = paste(CliNitPpal, LinNegCod, sep = "_")
        )
    })
    
    # ── Catalogo razon social / asesor / segmento ─────────────────────────────
    
    catalogo_rs <- reactive({
      req(tx_limpia())
      attrs_tx <- tx_limpia() %>%
        filter(!is.na(PerRazSoc) | !is.na(Asesor) | !is.na(Segmento)) %>%
        group_by(cliente_id, CliNitPpal, LinNegCod) %>%
        summarise(
          PerRazSoc = first(PerRazSoc[!is.na(PerRazSoc)]),
          Asesor    = first(Asesor[!is.na(Asesor)]),
          Segmento  = first(Segmento[!is.na(Segmento)]),
          .groups   = "drop"
        )
      
      rs_ncliente <- NCLIENTE %>%
        select(PerCod, PerRazSoc) %>%
        distinct() %>%
        rename(CliNitPpal = PerCod, PerRazSoc_nc = PerRazSoc)
      
      crm_data() %>%
        distinct(cliente_id, CliNitPpal, LinNegCod) %>%
        left_join(attrs_tx,    by = c("cliente_id", "CliNitPpal", "LinNegCod")) %>%
        left_join(rs_ncliente, by = "CliNitPpal") %>%
        mutate(
          PerRazSoc = coalesce(PerRazSoc, PerRazSoc_nc, "\u2014"),
          Asesor    = replace_na(Asesor,   "\u2014"),
          Segmento  = replace_na(Segmento, "\u2014")
        ) %>%
        select(cliente_id, PerRazSoc, Asesor, Segmento)
    })
    
    # ── Marca de presupuesto por UC ───────────────────────────────────────────
    
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
    
    # ── Ventas reales mensuales y acumuladas ──────────────────────────────────
    
    real_mensual <- reactive({
      req(tx_limpia(), mes_inicio(), mes_fin())
      tx_limpia() %>%
        filter(ym >= mes_inicio(), ym <= mes_fin()) %>%
        group_by(cliente_id, ym) %>%
        summarise(
          real_sacos  = sum(coalesce(SacFact70, 0), na.rm = TRUE),
          real_margen = sum(coalesce(Margen,    0), na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    fechas_fact <- reactive({
      req(real_mensual())
      real_mensual() %>%
        filter(real_sacos > 0) %>%
        group_by(cliente_id) %>%
        summarise(
          primera_fact = min(ym),
          ultima_fact  = max(ym),
          .groups = "drop"
        )
    })
    
    real_acum <- reactive({
      req(real_mensual())
      real_mensual() %>%
        group_by(cliente_id) %>%
        summarise(
          sacos_total  = sum(real_sacos,  na.rm = TRUE),
          margen_total = sum(real_margen, na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    # ── Panel de cumplimiento individual (presupuestadas) ─────────────────────
    
    panel_cumpl <- reactive({
      req(marca_presupuesto(), real_mensual(), meses_periodo())
      marca_presupuesto() %>%
        filter(presupuestada == "PRESUPUESTADA") %>%
        select(cliente_id, ppto_sacos_anual, ppto_margen_anual) %>%
        crossing(tibble(ym = meses_periodo())) %>%
        mutate(
          ppto_sacos_mes  = ppto_sacos_anual  / n_meses(),
          ppto_margen_mes = ppto_margen_anual / n_meses()
        ) %>%
        left_join(real_mensual(), by = c("cliente_id", "ym")) %>%
        mutate(
          real_sacos       = replace_na(real_sacos,  0),
          real_margen      = replace_na(real_margen, 0),
          cumpl_sacos_pct  = if_else(
            ppto_sacos_mes  > 0,
            round(real_sacos  / ppto_sacos_mes  * 100, 1), NA_real_
          ),
          cumpl_margen_pct = if_else(
            ppto_margen_mes > 0,
            round(real_margen / ppto_margen_mes * 100, 1), NA_real_
          )
        )
    })
    
    meses_con_real <- reactive({
      req(real_mensual())
      real_mensual() %>%
        filter(real_sacos > 0 | real_margen > 0) %>%
        pull(ym) %>%
        unique() %>%
        sort()
    })
    
    # ── Panel de cumplimiento grupal (NP con estado especial) ─────────────────
    
    panel_cumpl_grupal <- reactive({
      req(panel_full(), real_mensual(), meses_periodo())
      ucs_grupo <- panel_full() %>%
        filter(
          presupuestada == "NO PRESUPUESTADA",
          estado %in% c("NUEVO DEL PERIODO", "CLIENTE A RECUPERAR")
        ) %>%
        select(cliente_id, LinNegCod, ym, estado)
      
      n_ucs_mes <- ucs_grupo %>%
        group_by(LinNegCod, estado, ym) %>%
        summarise(n_ucs = n_distinct(cliente_id), .groups = "drop")
      
      ucs_grupo %>%
        left_join(
          .PPTO_GRUPO %>%
            mutate(LinNegCod = as.double(LinNegCod)) %>%
            rename(estado = grupo),
          by = c("LinNegCod", "estado")
        ) %>%
        left_join(n_ucs_mes, by = c("LinNegCod", "estado", "ym")) %>%
        mutate(
          ppto_sacos_mes  = if_else(
            !is.na(ppto_sacos_anual)  & n_ucs > 0,
            ppto_sacos_anual  / 12 / n_ucs, NA_real_
          ),
          ppto_margen_mes = if_else(
            !is.na(ppto_margen_anual) & n_ucs > 0,
            ppto_margen_anual / 12 / n_ucs, NA_real_
          )
        ) %>%
        left_join(real_mensual(), by = c("cliente_id", "ym")) %>%
        mutate(
          real_sacos       = replace_na(real_sacos,  0),
          real_margen      = replace_na(real_margen, 0),
          cumpl_sacos_pct  = if_else(
            !is.na(ppto_sacos_mes)  & ppto_sacos_mes  > 0,
            round(real_sacos  / ppto_sacos_mes  * 100, 1), NA_real_
          ),
          cumpl_margen_pct = if_else(
            !is.na(ppto_margen_mes) & ppto_margen_mes > 0,
            round(real_margen / ppto_margen_mes * 100, 1), NA_real_
          )
        ) %>%
        select(
          cliente_id, LinNegCod, ym, estado,
          ppto_sacos_mes, real_sacos, cumpl_sacos_pct,
          ppto_margen_mes, real_margen, cumpl_margen_pct
        )
    })
    
    # ── Actividad mensual desde CRM ───────────────────────────────────────────
    
    actividad_mensual <- reactive({
      req(crm_data(), mes_inicio(), mes_fin())
      crm_data() %>%
        filter(FecProceso >= mes_inicio(), FecProceso <= mes_fin()) %>%
        mutate(ym = PrimerDia(FecProceso, uni = "month")) %>%
        distinct(cliente_id, ym, SegmentoRacafe)
    })
    
    # ── Poblacion baseline (existentes en mes_inicio) ─────────────────────────
    
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
    
    # ── Altas en cohorte (nuevos que facturan en el periodo) ──────────────────
    
    altas_cohorte <- reactive({
      req(tx_limpia(), baseline(), mes_inicio(), mes_fin())
      nits_baseline          <- baseline() %>% distinct(CliNitPpal)
      nits_fact_pre_baseline <- FACT %>%
        filter(as.Date(MinFecFact) < mes_inicio()) %>%
        distinct(FctNit) %>%
        rename(CliNitPpal = FctNit)
      
      tx_limpia() %>%
        filter(ym >= mes_inicio(), ym <= mes_fin()) %>%
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
    
    # ── Panel baseline: estado CRM mes a mes ─────────────────────────────────
    
    panel_baseline <- reactive({
      # Panel cerrado: cada UC de baseline aparece en TODOS los meses del periodo.
      # Estado = lo que CRM diga ese mes; si no aparece → "CLIENTE A RECUPERAR"
      req(baseline(), actividad_mensual(), meses_periodo())
      baseline() %>%
        select(cliente_id, CliNitPpal, LinNegCod, presupuestada, tipo_cohorte) %>%
        crossing(tibble(ym = meses_periodo())) %>%
        left_join(
          actividad_mensual() %>% select(cliente_id, ym, SegmentoRacafe),
          by = c("cliente_id", "ym")
        ) %>%
        mutate(estado = case_when(
          SegmentoRacafe == "CLIENTE"             ~ "CLIENTE ACTIVO",
          SegmentoRacafe == "CLIENTE A RECUPERAR" ~ "CLIENTE A RECUPERAR",
          TRUE                                    ~ "CLIENTE A RECUPERAR"
        )) %>%
        select(cliente_id, CliNitPpal, LinNegCod, presupuestada, tipo_cohorte, ym, estado)
    })
    
    # ── Panel altas: NP fijo, presupuestadas evolucionan con CRM ─────────────
    
    panel_altas <- reactive({
      # A) NP: estado fijo "NUEVO DEL PERIODO" desde mes_entrada hasta mes_fin
      # B) Presupuestadas: estado evoluciona con CRM igual que baseline
      req(altas_cohorte(), actividad_mensual(), real_mensual(), meses_periodo())
      
      primer_mes_crm <- actividad_mensual() %>%
        semi_join(altas_cohorte(), by = "cliente_id") %>%
        group_by(cliente_id) %>%
        summarise(mes_alta_crm = min(ym), .groups = "drop")
      
      primer_mes_tx <- real_mensual() %>%
        semi_join(altas_cohorte(), by = "cliente_id") %>%
        filter(real_sacos > 0) %>%
        group_by(cliente_id) %>%
        summarise(mes_alta_tx = min(ym), .groups = "drop")
      
      altas_base <- altas_cohorte() %>%
        select(cliente_id, CliNitPpal, LinNegCod, presupuestada, tipo_cohorte) %>%
        left_join(primer_mes_crm, by = "cliente_id") %>%
        left_join(primer_mes_tx,  by = "cliente_id") %>%
        mutate(
          mes_entrada = pmin(
            coalesce(mes_alta_crm, as.Date("9999-01-01")),
            coalesce(mes_alta_tx,  as.Date("9999-01-01"))
          ),
          mes_entrada = if_else(
            mes_entrada == as.Date("9999-01-01"),
            mes_inicio(), mes_entrada
          ),
          mes_entrada = pmax(mes_entrada, mes_inicio())
        )
      
      # Altas NP: estado fijo
      altas_np <- altas_base %>%
        filter(presupuestada == "NO PRESUPUESTADA") %>%
        crossing(tibble(ym = meses_periodo())) %>%
        filter(ym >= mes_entrada) %>%
        mutate(estado = "NUEVO DEL PERIODO") %>%
        select(cliente_id, CliNitPpal, LinNegCod, presupuestada, tipo_cohorte,
               ym, estado, mes_entrada)
      
      # Altas presupuestadas: estado evoluciona con CRM
      altas_p <- altas_base %>%
        filter(presupuestada == "PRESUPUESTADA") %>%
        crossing(tibble(ym = meses_periodo())) %>%
        filter(ym >= mes_entrada) %>%
        left_join(
          actividad_mensual() %>% select(cliente_id, ym, SegmentoRacafe),
          by = c("cliente_id", "ym")
        ) %>%
        mutate(estado = case_when(
          SegmentoRacafe == "CLIENTE"             ~ "CLIENTE ACTIVO",
          SegmentoRacafe == "CLIENTE A RECUPERAR" ~ "CLIENTE A RECUPERAR",
          TRUE                                    ~ "CLIENTE A RECUPERAR"
        )) %>%
        select(cliente_id, CliNitPpal, LinNegCod, presupuestada, tipo_cohorte,
               ym, estado, mes_entrada)
      
      bind_rows(altas_np, altas_p)
    })
    
    # ── Panel completo y subpaneles ───────────────────────────────────────────
    
    panel_full <- reactive({
      req(panel_baseline(), panel_altas())
      bind_rows(
        panel_baseline() %>% mutate(mes_entrada = mes_inicio()),
        panel_altas()
      )
    })
    
    panel_p    <- reactive(panel_full() %>% filter(presupuestada == "PRESUPUESTADA"))
    panel_np   <- reactive(panel_full() %>% filter(presupuestada == "NO PRESUPUESTADA"))
    ultimo_mes <- reactive(max(panel_full()$ym))
    
    # ── Tasa de facturacion por UC ────────────────────────────────────────────
    
    tasa_fact_uc <- reactive({
      req(panel_full(), real_mensual())
      meses_en_cohorte <- panel_full() %>%
        group_by(cliente_id, mes_entrada) %>%
        summarise(meses_cohorte = n_distinct(ym), .groups = "drop")
      
      meses_activo <- real_mensual() %>%
        filter(real_sacos > 0) %>%
        semi_join(panel_full() %>% distinct(cliente_id), by = "cliente_id") %>%
        left_join(
          panel_full() %>% distinct(cliente_id, mes_entrada),
          by = "cliente_id"
        ) %>%
        filter(ym >= mes_entrada, ym >= mes_inicio(), ym <= mes_fin()) %>%
        group_by(cliente_id) %>%
        summarise(meses_con_fact = n_distinct(ym), .groups = "drop")
      
      meses_en_cohorte %>%
        left_join(meses_activo, by = "cliente_id") %>%
        mutate(
          meses_con_fact = replace_na(meses_con_fact, 0),
          tasa_fact_pct  = round(meses_con_fact / pmax(meses_cohorte, 1) * 100, 1)
        ) %>%
        select(cliente_id, meses_cohorte, meses_con_fact, tasa_fact_pct)
    })
    
    # ── Indicadores de transicion de cohorte ──────────────────────────────────
    
    .indicadores_cohorte <- function(panel) {
      trans <- panel %>%
        filter(tipo_cohorte == "POBLACION BASE") %>%
        arrange(cliente_id, ym) %>%
        group_by(cliente_id) %>%
        mutate(estado_sig = lead(estado)) %>%
        ungroup() %>%
        filter(!is.na(estado_sig))
      
      n_activo_t    <- sum(trans$estado == "CLIENTE ACTIVO")
      n_recuperar_t <- sum(trans$estado == "CLIENTE A RECUPERAR")
      n_ret         <- sum(trans$estado == "CLIENTE ACTIVO"      & trans$estado_sig == "CLIENTE ACTIVO")
      n_perd        <- sum(trans$estado == "CLIENTE ACTIVO"      & trans$estado_sig == "CLIENTE A RECUPERAR")
      n_react       <- sum(trans$estado == "CLIENTE A RECUPERAR" & trans$estado_sig == "CLIENTE ACTIVO")
      
      list(
        retencion    = if (n_activo_t    > 0) round(n_ret   / n_activo_t    * 100, 1) else NA_real_,
        perdida      = if (n_activo_t    > 0) round(n_perd  / n_activo_t    * 100, 1) else NA_real_,
        reactivacion = if (n_recuperar_t > 0) round(n_react / n_recuperar_t * 100, 1) else NA_real_
      )
    }
    
    ind_global <- reactive({ req(panel_full()); .indicadores_cohorte(panel_full()) })
    ind_p      <- reactive({ req(panel_p());    .indicadores_cohorte(panel_p())    })
    ind_np     <- reactive({ req(panel_np());   .indicadores_cohorte(panel_np())   })
    
    # ── Tasa de facturacion media por grupo ───────────────────────────────────
    
    .tasa_media <- function(ids) {
      vals <- tasa_fact_uc() %>%
        filter(cliente_id %in% ids) %>%
        pull(tasa_fact_pct)
      round(mean(vals, na.rm = TRUE), 1)
    }
    
    tasa_fact_global <- reactive({
      req(tasa_fact_uc())
      .tasa_media(tasa_fact_uc()$cliente_id)
    })
    tasa_fact_p      <- reactive({
      req(tasa_fact_uc(), panel_p())
      .tasa_media(panel_p() %>% distinct(cliente_id) %>% pull())
    })
    tasa_fact_np     <- reactive({
      req(tasa_fact_uc(), panel_np())
      .tasa_media(panel_np() %>% distinct(cliente_id) %>% pull())
    })
    
    # ── Corte en ultimo mes: tabla enriquecida para modales ───────────────────
    
    corte_ult <- reactive({
      req(panel_full(), catalogo_rs(), real_acum(), tasa_fact_uc(),
          fechas_fact(), panel_cumpl_grupal())
      n_m <- n_meses()
      
      ppto_indiv <- marca_presupuesto() %>%
        filter(presupuestada == "PRESUPUESTADA") %>%
        transmute(
          cliente_id,
          ppto_sacos  = ppto_sacos_anual  / 12 * n_m,
          ppto_margen = ppto_margen_anual / 12 * n_m
        )
      
      ppto_grupal_acum <- panel_cumpl_grupal() %>%
        group_by(cliente_id) %>%
        summarise(
          ppto_sacos  = sum(ppto_sacos_mes,  na.rm = TRUE),
          ppto_margen = sum(ppto_margen_mes, na.rm = TRUE),
          .groups = "drop"
        )
      
      ppto_total <- bind_rows(ppto_indiv, ppto_grupal_acum) %>%
        group_by(cliente_id) %>%
        summarise(
          ppto_sacos  = sum(ppto_sacos,  na.rm = TRUE),
          ppto_margen = sum(ppto_margen, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(ppto_sacos > 0 | ppto_margen > 0)
      
      panel_full() %>%
        filter(ym == ultimo_mes()) %>%
        left_join(catalogo_rs(),  by = "cliente_id") %>%
        left_join(real_acum(),    by = "cliente_id") %>%
        left_join(tasa_fact_uc() %>% select(cliente_id, tasa_fact_pct), by = "cliente_id") %>%
        left_join(fechas_fact(),  by = "cliente_id") %>%
        left_join(ppto_total,     by = "cliente_id") %>%
        mutate(
          PerRazSoc        = replace_na(PerRazSoc,   "\u2014"),
          Asesor           = replace_na(Asesor,       "\u2014"),
          Segmento         = replace_na(Segmento,     "\u2014"),
          sacos_total      = replace_na(sacos_total,  0),
          margen_total     = replace_na(margen_total, 0),
          fecha_ref        = case_when(
            estado == "NUEVO DEL PERIODO"   ~ primera_fact,
            estado == "CLIENTE A RECUPERAR" ~ ultima_fact,
            TRUE                            ~ NA_Date_
          ),
          cumpl_sacos_pct  = if_else(
            !is.na(ppto_sacos)  & ppto_sacos  > 0,
            round(sacos_total  / ppto_sacos  * 100, 1), NA_real_
          ),
          cumpl_margen_pct = if_else(
            !is.na(ppto_margen) & ppto_margen > 0,
            round(margen_total / ppto_margen * 100, 1), NA_real_
          )
        ) %>%
        select(
          cliente_id, PerRazSoc, Asesor, Segmento,
          CliNitPpal, LinNegCod,
          presupuestada, tipo_cohorte, estado, fecha_ref,
          sacos_total, margen_total, tasa_fact_pct,
          ppto_sacos, ppto_margen, cumpl_sacos_pct, cumpl_margen_pct
        ) %>%
        arrange(CliNitPpal, LinNegCod)
    })
    
    # ==========================================================================
    # HELPER MODAL DINAMICO
    # Nota: .show_modal se conserva para modales cuyo contenido depende
    # del click (tabla resumen y rangos de tasa).  No aplica CajaModal porque
    # la UC/estado/mes a mostrar se determina en tiempo de ejecucion.
    # ==========================================================================
    
    .show_modal <- function(titulo, icono_nm, data_clientes, modal_id) {
      click_id <- paste0("click_", modal_id)
      cumpl_id <- paste0("cumpl_", modal_id)
      
      showModal(modalDialog(
        title     = tagList(icon(icono_nm), " ", titulo),
        size      = "xl", easyClose = TRUE, footer = modalButton("Cerrar"),
        tagList(
          .reactable_uc(data_clientes, ns(click_id)),
          p(icon("hand-pointer"),
            " Haga clic en una fila para ver el detalle de cumplimiento",
            style = "font-size:11px; color:#888; margin-top:4px;"),
          uiOutput(ns(cumpl_id))
        )
      ))
      
      # Observer de click en fila de la tabla del modal
      # Se registra una vez por invocacion de .show_modal; el modal se destruye
      # al cerrarse por lo que el observer queda inactivo hasta el siguiente click
      observeEvent(input[[click_id]], {
        req(!is.null(input[[click_id]]$cliente_id))
        cid       <- input[[click_id]]$cliente_id
        fila      <- data_clientes %>% filter(cliente_id == cid)
        req(nrow(fila) > 0)
        
        rsoc       <- fila$PerRazSoc[[1]]
        es_presup  <- fila$presupuestada[[1]] == "PRESUPUESTADA"
        panel_c    <- if (es_presup) panel_cumpl() else panel_cumpl_grupal()
        tiene_ppto <- cid %in% panel_c$cliente_id
        
        output[[cumpl_id]] <<- renderUI({
          if (!tiene_ppto) {
            return(tagList(
              hr(),
              p(icon("info-circle"),
                " Esta Unidad Comercial no tiene presupuesto asignado en el periodo.",
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
    
    # ==========================================================================
    # KPIs — POBLACION BASE
    # Patron CajaModal: registro previo obligatorio antes del primer click.
    # Los outputs de la tabla del modal se declaran estaticamente aqui;
    # CajaModal los presenta dentro de contenido_modal = function() { ... }
    # ==========================================================================
    
    # Tablas estaticas para los modales de poblacion base
    output$tbl_baseline   <- reactable::renderReactable({
      req(corte_ult()); .reactable_uc(corte_ult(), ns("click_modal_baseline"))
    })
    output$tbl_presup_pop <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(presupuestada == "PRESUPUESTADA"),
        ns("click_modal_presup_pop")
      )
    })
    output$tbl_no_presup_pop <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(presupuestada == "NO PRESUPUESTADA",
                               tipo_cohorte  == "POBLACION BASE"),
        ns("click_modal_no_presup_pop")
      )
    })
    output$tbl_nuevos_pop <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(tipo_cohorte == "ALTA EN COHORTE"),
        ns("click_modal_nuevos_pop")
      )
    })
    
    # Helper para renderizar cumplimiento desde click en tabla de modal estatico
    .bind_cumpl_modal <- function(click_id, cumpl_id) {
      observeEvent(input[[click_id]], {
        req(!is.null(input[[click_id]]$cliente_id))
        cid       <- input[[click_id]]$cliente_id
        fila      <- corte_ult() %>% filter(cliente_id == cid)
        req(nrow(fila) > 0)
        rsoc      <- fila$PerRazSoc[[1]]
        es_presup <- fila$presupuestada[[1]] == "PRESUPUESTADA"
        panel_c   <- if (es_presup) panel_cumpl() else panel_cumpl_grupal()
        tiene_ppto <- cid %in% panel_c$cliente_id
        output[[cumpl_id]] <<- renderUI({
          if (!tiene_ppto) {
            return(tagList(
              hr(),
              p(icon("info-circle"),
                " Esta Unidad Comercial no tiene presupuesto asignado en el periodo.",
                style = "color:#888; font-style:italic; margin-top:10px;")
            ))
          }
          gt_id <- paste0("gt_cumpl_", gsub("[^A-Za-z0-9]", "_", cid))
          output[[gt_id]] <<- gt::render_gt({
            req(meses_con_real())
            .gt_cumpl_uc(panel_c, cid, rsoc, meses_con_real())
          })
          tagList(hr(), gt::gt_output(ns(gt_id)))
        })
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
    }
    
    .bind_cumpl_modal("click_modal_baseline",     "cumpl_modal_baseline")
    .bind_cumpl_modal("click_modal_presup_pop",   "cumpl_modal_presup_pop")
    .bind_cumpl_modal("click_modal_no_presup_pop","cumpl_modal_no_presup_pop")
    .bind_cumpl_modal("click_modal_nuevos_pop",   "cumpl_modal_nuevos_pop")
    
    # Registro de CajaModal para KPIs de poblacion base
    CajaModal(
      id            = "kpi_baseline",
      valor         = reactive(nrow(baseline())),
      formato       = "numero",
      texto         = reactive(paste("Poblacion Base a", format(mes_inicio(), "%b %Y"))),
      icono         = "users",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(paste0("Poblacion Base — ", format(mes_inicio(), "%b %Y"))),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_baseline")),
          p(icon("hand-pointer"),
            " Haga clic en una fila para ver el detalle de cumplimiento",
            style = "font-size:11px; color:#888; margin-top:4px;"),
          uiOutput(ns("cumpl_modal_baseline"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_presup",
      valor         = reactive(
        baseline() %>% filter(presupuestada == "PRESUPUESTADA") %>% nrow()
      ),
      formato       = "numero",
      texto         = reactive(paste("Presupuestadas a", format(mes_inicio(), "%b %Y"))),
      icono         = "bullseye",
      mostrar_boton = TRUE,
      titulo_modal  = reactive("Unidades Presupuestadas — ultimo mes"),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_presup_pop")),
          p(icon("hand-pointer"),
            " Haga clic en una fila para ver el detalle de cumplimiento",
            style = "font-size:11px; color:#888; margin-top:4px;"),
          uiOutput(ns("cumpl_modal_presup_pop"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_no_presup",
      valor         = reactive(
        baseline() %>% filter(presupuestada == "NO PRESUPUESTADA") %>% nrow()
      ),
      formato       = "numero",
      texto         = reactive(paste("No Presupuestadas a", format(mes_inicio(), "%b %Y"))),
      icono         = "user-clock",
      mostrar_boton = TRUE,
      titulo_modal  = reactive("No Presupuestadas — ultimo mes"),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_no_presup_pop")),
          p(icon("hand-pointer"),
            " Haga clic en una fila para ver el detalle de cumplimiento",
            style = "font-size:11px; color:#888; margin-top:4px;"),
          uiOutput(ns("cumpl_modal_no_presup_pop"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_nuevos",
      valor         = reactive(nrow(altas_cohorte())),
      formato       = "numero",
      texto         = reactive(paste("Altas en Cohorte a", format(mes_inicio(), "%b %Y"))),
      icono         = "user-plus",
      mostrar_boton = TRUE,
      titulo_modal  = reactive("Altas en Cohorte — ultimo mes"),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_nuevos_pop")),
          p(icon("hand-pointer"),
            " Haga clic en una fila para ver el detalle de cumplimiento",
            style = "font-size:11px; color:#888; margin-top:4px;"),
          uiOutput(ns("cumpl_modal_nuevos_pop"))
        )
      }
    )
    
    # ==========================================================================
    # KPIs — INDICADORES DINAMICOS GLOBALES
    # Sin boton (mostrar_boton = FALSE); valor coloreado con html_valor de
    # racafeModulos usando .color_kpi para el semaforo
    # ==========================================================================
    
    CajaModal(
      id            = "kpi_retencion",
      valor         = reactive(
        html_valor(ind_global()$retencion %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(ind_global()$retencion, "retencion"))
      ),
      texto         = "Retencion (Activo → Activo)",
      icono         = "check-double",
      mostrar_boton = FALSE
    )
    
    CajaModal(
      id            = "kpi_perdida",
      valor         = reactive(
        html_valor(ind_global()$perdida %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(ind_global()$perdida, "perdida"))
      ),
      texto         = "Perdida Dinamica (Activo → A Recuperar)",
      icono         = "arrow-down",
      mostrar_boton = FALSE
    )
    
    CajaModal(
      id            = "kpi_reactivacion",
      valor         = reactive(
        html_valor(ind_global()$reactivacion %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(ind_global()$reactivacion, "reactivacion"))
      ),
      texto         = "Reactivacion (A Recuperar → Activo)",
      icono         = "sync-alt",
      mostrar_boton = FALSE
    )
    
    CajaModal(
      id            = "kpi_tasa_fact",
      valor         = reactive(
        html_valor(tasa_fact_global() %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(tasa_fact_global(), "tasa"))
      ),
      texto         = "Tasa de Facturacion Promedio",
      icono         = "calendar-check",
      mostrar_boton = FALSE
    )
    
    # ==========================================================================
    # HELPERS CONTEO Y LABELS
    # ==========================================================================
    
    .n_uc <- function(df) dplyr::n_distinct(paste(df$CliNitPpal, df$LinNegCod))
    
    corte_ini_p     <- reactive({ req(panel_p());  panel_p()  %>% filter(ym == mes_inicio()) })
    corte_fin_p     <- reactive({ req(panel_p());  panel_p()  %>% filter(ym == ultimo_mes()) })
    corte_ini_np    <- reactive({ req(panel_np()); panel_np() %>% filter(ym == mes_inicio()) })
    corte_fin_np    <- reactive({ req(panel_np()); panel_np() %>% filter(ym == ultimo_mes()) })
    
    corte_ini_altas <- reactive({
      req(panel_altas())
      panel_altas() %>%
        filter(presupuestada == "NO PRESUPUESTADA", ym == mes_inicio())
    })
    corte_fin_altas <- reactive({
      req(panel_altas())
      panel_altas() %>%
        filter(presupuestada == "NO PRESUPUESTADA", ym == ultimo_mes())
    })
    
    output$lbl_inicio_p  <- renderUI({ strong(paste("Inicio —", format(mes_inicio(), "%b %Y"))) })
    output$lbl_fin_p     <- renderUI({ strong(paste("Fin —",    format(ultimo_mes(), "%b %Y"))) })
    output$lbl_inicio_np <- renderUI({ strong(paste("Inicio —", format(mes_inicio(), "%b %Y"))) })
    output$lbl_fin_np    <- renderUI({ strong(paste("Fin —",    format(ultimo_mes(), "%b %Y"))) })
    
    .lbl_transicion <- function() {
      renderUI({
        p(icon("info-circle"),
          paste0(" Periodo: ", format(mes_inicio(), "%B %Y"),
                 " → ", format(ultimo_mes(), "%B %Y"),
                 "  |  Transiciones t → t+1 · Poblacion Base"),
          style = "font-size:11px; color:#666; margin-bottom:8px;")
      })
    }
    output$lbl_transicion_p  <- .lbl_transicion()
    output$lbl_transicion_np <- .lbl_transicion()
    
    # ==========================================================================
    # KPIs — PRESUPUESTADOS inicio/fin
    # Con boton: tabla filtrada declarada estaticamente antes del CajaModal
    # ==========================================================================
    
    # Tablas para modales de presupuestados inicio/fin
    output$tbl_p_activo_ini <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(presupuestada == "PRESUPUESTADA", estado == "CLIENTE ACTIVO"),
        ns("click_p_activo_ini")
      )
    })
    output$tbl_p_recuperar_ini <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(presupuestada == "PRESUPUESTADA", estado == "CLIENTE A RECUPERAR"),
        ns("click_p_recuperar_ini")
      )
    })
    output$tbl_p_activo_fin <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(presupuestada == "PRESUPUESTADA", estado == "CLIENTE ACTIVO"),
        ns("click_p_activo_fin")
      )
    })
    output$tbl_p_recuperar_fin <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(presupuestada == "PRESUPUESTADA", estado == "CLIENTE A RECUPERAR"),
        ns("click_p_recuperar_fin")
      )
    })
    
    .bind_cumpl_modal("click_p_activo_ini",    "cumpl_p_activo_ini")
    .bind_cumpl_modal("click_p_recuperar_ini", "cumpl_p_recuperar_ini")
    .bind_cumpl_modal("click_p_activo_fin",    "cumpl_p_activo_fin")
    .bind_cumpl_modal("click_p_recuperar_fin", "cumpl_p_recuperar_fin")
    
    CajaModal(
      id            = "kpi_p_activo_ini",
      valor         = reactive(corte_ini_p() %>% filter(estado == "CLIENTE ACTIVO") %>% .n_uc()),
      formato       = "numero",
      texto         = "Clientes Activos",
      icono         = "check-circle",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(
        paste0("Presupuestados Activos — Inicio (", format(mes_inicio(), "%b %Y"), ")")
      ),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_p_activo_ini")),
          uiOutput(ns("cumpl_p_activo_ini"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_p_recuperar_ini",
      valor         = reactive(
        corte_ini_p() %>% filter(estado == "CLIENTE A RECUPERAR") %>% .n_uc()
      ),
      formato       = "numero",
      texto         = "Clientes a Recuperar",
      icono         = "exclamation-triangle",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(
        paste0("Presupuestados a Recuperar — Inicio (", format(mes_inicio(), "%b %Y"), ")")
      ),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_p_recuperar_ini")),
          uiOutput(ns("cumpl_p_recuperar_ini"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_p_tasa_fact_ini",
      valor         = reactive(
        html_valor(tasa_fact_p() %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(tasa_fact_p(), "tasa"))
      ),
      texto         = "Tasa Facturacion Promedio",
      icono         = "calendar-check",
      mostrar_boton = FALSE
    )
    
    CajaModal(
      id            = "kpi_p_activo_fin",
      valor         = reactive(corte_fin_p() %>% filter(estado == "CLIENTE ACTIVO") %>% .n_uc()),
      formato       = "numero",
      texto         = "Clientes Activos",
      icono         = "check-circle",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(
        paste0("Presupuestados Activos — Fin (", format(ultimo_mes(), "%b %Y"), ")")
      ),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_p_activo_fin")),
          uiOutput(ns("cumpl_p_activo_fin"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_p_recuperar_fin",
      valor         = reactive(
        corte_fin_p() %>% filter(estado == "CLIENTE A RECUPERAR") %>% .n_uc()
      ),
      formato       = "numero",
      texto         = "Clientes a Recuperar",
      icono         = "exclamation-triangle",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(
        paste0("Presupuestados a Recuperar — Fin (", format(ultimo_mes(), "%b %Y"), ")")
      ),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_p_recuperar_fin")),
          uiOutput(ns("cumpl_p_recuperar_fin"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_p_tasa_fact_fin",
      valor         = reactive(
        html_valor(tasa_fact_p() %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(tasa_fact_p(), "tasa"))
      ),
      texto         = "Tasa Facturacion Promedio",
      icono         = "calendar-check",
      mostrar_boton = FALSE
    )
    
    # KPIs dinamicos presupuestados — sin boton
    CajaModal(
      id            = "kpi_p_retencion",
      valor         = reactive(
        html_valor(ind_p()$retencion %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(ind_p()$retencion, "retencion"))
      ),
      texto         = "Retencion",
      icono         = "check-double",
      mostrar_boton = FALSE
    )
    CajaModal(
      id            = "kpi_p_perdida",
      valor         = reactive(
        html_valor(ind_p()$perdida %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(ind_p()$perdida, "perdida"))
      ),
      texto         = "Perdida Dinamica",
      icono         = "arrow-down",
      mostrar_boton = FALSE
    )
    CajaModal(
      id            = "kpi_p_reactivacion",
      valor         = reactive(
        html_valor(ind_p()$reactivacion %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(ind_p()$reactivacion, "reactivacion"))
      ),
      texto         = "Reactivacion",
      icono         = "sync-alt",
      mostrar_boton = FALSE
    )
    
    # ==========================================================================
    # KPIs — NO PRESUPUESTADOS inicio/fin
    # ==========================================================================
    
    # Tablas para modales de no presupuestados inicio/fin
    output$tbl_np_activo_ini <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(presupuestada == "NO PRESUPUESTADA", estado == "CLIENTE ACTIVO"),
        ns("click_np_activo_ini")
      )
    })
    output$tbl_np_recuperar_ini <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(
          presupuestada == "NO PRESUPUESTADA", estado == "CLIENTE A RECUPERAR"
        ),
        ns("click_np_recuperar_ini")
      )
    })
    output$tbl_np_nuevos_ini <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(presupuestada == "NO PRESUPUESTADA",
                               tipo_cohorte  == "ALTA EN COHORTE"),
        ns("click_np_nuevos_ini")
      )
    })
    output$tbl_np_activo_fin <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(presupuestada == "NO PRESUPUESTADA", estado == "CLIENTE ACTIVO"),
        ns("click_np_activo_fin")
      )
    })
    output$tbl_np_recuperar_fin <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(
          presupuestada == "NO PRESUPUESTADA", estado == "CLIENTE A RECUPERAR"
        ),
        ns("click_np_recuperar_fin")
      )
    })
    output$tbl_np_nuevos_fin <- reactable::renderReactable({
      req(corte_ult())
      .reactable_uc(
        corte_ult() %>% filter(presupuestada == "NO PRESUPUESTADA",
                               tipo_cohorte  == "ALTA EN COHORTE"),
        ns("click_np_nuevos_fin")
      )
    })
    
    .bind_cumpl_modal("click_np_activo_ini",    "cumpl_np_activo_ini")
    .bind_cumpl_modal("click_np_recuperar_ini", "cumpl_np_recuperar_ini")
    .bind_cumpl_modal("click_np_nuevos_ini",    "cumpl_np_nuevos_ini")
    .bind_cumpl_modal("click_np_activo_fin",    "cumpl_np_activo_fin")
    .bind_cumpl_modal("click_np_recuperar_fin", "cumpl_np_recuperar_fin")
    .bind_cumpl_modal("click_np_nuevos_fin",    "cumpl_np_nuevos_fin")
    
    CajaModal(
      id            = "kpi_np_activo_ini",
      valor         = reactive(corte_ini_np() %>% filter(estado == "CLIENTE ACTIVO") %>% .n_uc()),
      formato       = "numero",
      texto         = "Clientes Activos",
      icono         = "check-circle",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(
        paste0("No Presupuestados Activos — Inicio (", format(mes_inicio(), "%b %Y"), ")")
      ),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_np_activo_ini")),
          uiOutput(ns("cumpl_np_activo_ini"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_np_recuperar_ini",
      valor         = reactive(
        corte_ini_np() %>% filter(estado == "CLIENTE A RECUPERAR") %>% .n_uc()
      ),
      formato       = "numero",
      texto         = "Clientes a Recuperar",
      icono         = "exclamation-triangle",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(
        paste0("No Presupuestados a Recuperar — Inicio (", format(mes_inicio(), "%b %Y"), ")")
      ),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_np_recuperar_ini")),
          uiOutput(ns("cumpl_np_recuperar_ini"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_np_nuevos_ini",
      valor         = reactive(corte_ini_altas() %>% .n_uc()),
      formato       = "numero",
      texto         = "Altas en Cohorte",
      icono         = "user-plus",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(
        paste0("Altas en Cohorte — Inicio (", format(mes_inicio(), "%b %Y"), ")")
      ),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_np_nuevos_ini")),
          uiOutput(ns("cumpl_np_nuevos_ini"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_np_tasa_fact",
      valor         = reactive(
        html_valor(tasa_fact_np() %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(tasa_fact_np(), "tasa"))
      ),
      texto         = "Tasa Facturacion Promedio",
      icono         = "calendar-check",
      mostrar_boton = FALSE
    )
    
    CajaModal(
      id            = "kpi_np_activo_fin",
      valor         = reactive(corte_fin_np() %>% filter(estado == "CLIENTE ACTIVO") %>% .n_uc()),
      formato       = "numero",
      texto         = "Clientes Activos",
      icono         = "check-circle",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(
        paste0("No Presupuestados Activos — Fin (", format(ultimo_mes(), "%b %Y"), ")")
      ),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_np_activo_fin")),
          uiOutput(ns("cumpl_np_activo_fin"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_np_recuperar_fin",
      valor         = reactive(
        corte_fin_np() %>% filter(estado == "CLIENTE A RECUPERAR") %>% .n_uc()
      ),
      formato       = "numero",
      texto         = "Clientes a Recuperar",
      icono         = "exclamation-triangle",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(
        paste0("No Presupuestados a Recuperar — Fin (", format(ultimo_mes(), "%b %Y"), ")")
      ),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_np_recuperar_fin")),
          uiOutput(ns("cumpl_np_recuperar_fin"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_np_nuevos_fin",
      valor         = reactive(corte_fin_altas() %>% .n_uc()),
      formato       = "numero",
      texto         = "Altas en Cohorte",
      icono         = "user-plus",
      mostrar_boton = TRUE,
      titulo_modal  = reactive(
        paste0("Altas en Cohorte — Fin (", format(ultimo_mes(), "%b %Y"), ")")
      ),
      tamano_modal  = "xl",
      contenido_modal = function() {
        tagList(
          reactable::reactableOutput(ns("tbl_np_nuevos_fin")),
          uiOutput(ns("cumpl_np_nuevos_fin"))
        )
      }
    )
    
    CajaModal(
      id            = "kpi_np_tasa_fact_fin",
      valor         = reactive(
        html_valor(tasa_fact_np() %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(tasa_fact_np(), "tasa"))
      ),
      texto         = "Tasa Facturacion Promedio",
      icono         = "calendar-check",
      mostrar_boton = FALSE
    )
    
    # KPIs dinamicos no presupuestados — sin boton
    CajaModal(
      id            = "kpi_np_retencion",
      valor         = reactive(
        html_valor(ind_np()$retencion %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(ind_np()$retencion, "retencion"))
      ),
      texto         = "Retencion",
      icono         = "check-double",
      mostrar_boton = FALSE
    )
    CajaModal(
      id            = "kpi_np_perdida",
      valor         = reactive(
        html_valor(ind_np()$perdida %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(ind_np()$perdida, "perdida"))
      ),
      texto         = "Perdida Dinamica",
      icono         = "arrow-down",
      mostrar_boton = FALSE
    )
    CajaModal(
      id            = "kpi_np_reactivacion",
      valor         = reactive(
        html_valor(ind_np()$reactivacion %||% 0,
                   formato = "porcentaje",
                   color   = .color_kpi(ind_np()$reactivacion, "reactivacion"))
      ),
      texto         = "Reactivacion",
      icono         = "sync-alt",
      mostrar_boton = FALSE
    )
    
    # ==========================================================================
    # GRAFICOS — EVOLUCION
    # ==========================================================================
    
    .graf_evolucion <- function(panel) {
      df <- panel %>%
        group_by(ym, estado) %>%
        summarise(n = n_distinct(cliente_id), .groups = "drop") %>%
        mutate(estado = factor(estado, levels = names(.ESTADOS)))
      
      plot_ly(
        df, x = ~ym, y = ~n, color = ~estado, colors = .ESTADOS,
        type = "scatter", mode = "lines+markers",
        line   = list(width = 2),
        marker = list(size = 6),
        hovertemplate = ~paste0(
          "<b>", estado, "</b><br>",
          format(ym, "%B %Y"), "<br>",
          "Unidades Comerciales: <b>", n, "</b>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          xaxis      = list(title = "", type = "date", tickformat = "%b %Y", dtick = "M1"),
          yaxis      = list(title = "Unidades Comerciales"),
          legend     = list(orientation = "h", y = -0.25),
          hovermode  = "x unified",
          hoverlabel = list(
            bgcolor    = "rgba(255,255,255,1)", bordercolor = "#cccccc",
            font       = list(size = 12, color = "#000000"),
            align      = "left", namelength = -1
          )
        )
    }
    
    output$graf_p_evolucion  <- renderPlotly({ req(panel_p());  .graf_evolucion(panel_p())  })
    output$graf_np_evolucion <- renderPlotly({ req(panel_np()); .graf_evolucion(panel_np()) })
    
    # ==========================================================================
    # GRAFICOS — PERMANENCIA
    # ==========================================================================
    
    .graf_permanencia <- function(panel) {
      df_hist   <- panel %>%
        group_by(cliente_id) %>%
        summarise(meses_activo = sum(estado == "CLIENTE ACTIVO"), .groups = "drop")
      total_ucs <- nrow(df_hist)
      freq      <- df_hist %>%
        count(meses_activo, name = "n_ucs") %>%
        mutate(pct = round(100 * n_ucs / total_ucs, 1))
      
      plot_ly(
        x    = freq$meses_activo,
        y    = freq$n_ucs,
        type = "bar",
        marker        = list(color = "#2C7BB6", line = list(color = "white", width = 1)),
        customdata    = freq$pct,
        hovertemplate = paste0(
          "<b>%{x} mes(es) como Cliente Activo</b><br>",
          "Unidades Comerciales: <b>%{y}</b><br>",
          "Participacion en el grupo: <b>%{customdata}%</b>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          xaxis      = list(
            title    = "Meses como Cliente Activo en el periodo",
            dtick    = 1, tickmode = "linear"
          ),
          yaxis      = list(title = "Unidades Comerciales"),
          bargap     = 0.15,
          hoverlabel = list(
            bgcolor     = "rgba(255,255,255,1)", bordercolor = "#2C7BB6",
            font        = list(size = 12, color = "#000000"),
            align       = "left"
          )
        )
    }
    
    output$graf_p_permanencia  <- renderPlotly({ req(panel_p());  .graf_permanencia(panel_p())  })
    output$graf_np_permanencia <- renderPlotly({ req(panel_np()); .graf_permanencia(panel_np()) })
    
    # ==========================================================================
    # TABLA RESUMEN MENSUAL
    # ==========================================================================
    
    .tabla_resumen_reactable <- function(panel, input_id_click) {
      meses        <- meses_periodo()
      col_keys_ord <- format(meses, "%Y-%m")
      col_labels   <- format(meses, "%b %y")
      
      conteos <- panel %>%
        group_by(estado, ym) %>%
        summarise(n = n_distinct(cliente_id), .groups = "drop") %>%
        mutate(col_key = format(ym, "%Y-%m")) %>%
        pivot_wider(
          id_cols    = estado,
          names_from = col_key,
          values_from = n, values_fill = 0L
        ) %>%
        mutate(estado = factor(
          estado,
          levels = names(.ESTADOS)[names(.ESTADOS) %in% unique(panel$estado)]
        )) %>%
        arrange(estado) %>%
        mutate(estado = as.character(estado))
      
      cols_presentes <- intersect(col_keys_ord, names(conteos))
      
      fila_total <- conteos %>%
        summarise(across(all_of(cols_presentes), ~ sum(.x, na.rm = TRUE))) %>%
        mutate(estado = "Total poblacion", .before = 1)
      
      df <- bind_rows(conteos, fila_total)
      
      max_val <- max(unlist(conteos[, cols_presentes, drop = FALSE]), na.rm = TRUE)
      max_val <- if (is.finite(max_val) && max_val > 0) max_val else 1
      
      pal_fn <- scales::col_numeric(
        palette = c("#F0F6FF", "#B8D4F0", "#7BAFD4"),
        domain  = c(0, max_val)
      )
      
      js_onclick <- paste0(
        "function(rowInfo, colInfo) {",
        "  var col = colInfo.id;",
        "  if (!/^\\d{4}-\\d{2}$/.test(col)) return;",
        "  if (rowInfo.values['estado'] === 'Total poblacion') return;",
        "  Shiny.setInputValue('", input_id_click, "', {",
        "    estado: rowInfo.values['estado'],",
        "    ym: col,",
        "    nonce: Math.random()",
        "  }, {priority: 'event'});",
        "}"
      )
      
      cols_meses <- setNames(
        lapply(seq_along(col_keys_ord), function(i) {
          local({
            lbl_local <- col_labels[[i]]
            n_estados <- nrow(conteos)
            reactable::colDef(
              name     = lbl_local,
              minWidth = 72,
              style    = function(value, index) {
                es_total <- index > n_estados
                if (es_total) {
                  return(list(background = "#EEF2FF", fontWeight = "700", color = "#1a1a1a"))
                }
                bg <- if (is.na(value) || value == 0) "#F8F9FA"
                else pal_fn(min(value, max_val))
                list(background = bg, cursor = "pointer",
                     fontWeight = "500", color = "#1a1a1a")
              }
            )
          })
        }),
        col_keys_ord
      )
      
      col_estado <- list(
        estado = reactable::colDef(
          name     = "Estado en Cohorte",
          minWidth = 170,
          style    = function(value) {
            if (value == "Total poblacion") {
              return(list(fontWeight = "700", color = "#333", background = "#EEF2FF"))
            }
            list(
              fontWeight = "600",
              color = if (!is.null(.ESTADOS[value]) && !is.na(.ESTADOS[value]))
                .ESTADOS[value] else "#333"
            )
          }
        )
      )
      
      reactable::reactable(
        data      = df,
        columns   = c(col_estado, cols_meses),
        onClick   = JS(js_onclick),
        bordered  = TRUE,
        compact   = TRUE,
        highlight = TRUE,
        sortable  = FALSE,
        theme     = reactable::reactableTheme(
          headerStyle = list(fontWeight = "600", fontSize = "12px"),
          cellStyle   = list(fontSize = "12px")
        )
      )
    }
    
    output$tabla_p_resumen  <- reactable::renderReactable({
      req(panel_p())
      .tabla_resumen_reactable(panel_p(),  ns("resumen_click_p"))
    })
    output$tabla_np_resumen <- reactable::renderReactable({
      req(panel_np())
      .tabla_resumen_reactable(panel_np(), ns("resumen_click_np"))
    })
    
    # Click en celda de tabla resumen → modal dinamico con .show_modal
    .obs_resumen_click <- function(input_id_click, panel_fn, grupo_label) {
      observeEvent(input[[input_id_click]], {
        click <- input[[input_id_click]]
        req(!is.null(click$estado), !is.null(click$ym))
        ym_sel     <- as.Date(paste0(click$ym, "-01"))
        ids_sel    <- panel_fn() %>%
          filter(ym == ym_sel, estado == click$estado) %>%
          distinct(cliente_id) %>%
          pull()
        data_modal <- corte_ult() %>% filter(cliente_id %in% ids_sel)
        titulo     <- paste0(grupo_label, " — ", click$estado,
                             " · ", format(ym_sel, "%b %Y"),
                             " (", nrow(data_modal), " UCs)")
        modal_id   <- paste0(
          "res_",
          gsub("[^A-Za-z0-9]", "_", paste(grupo_label, click$estado, click$ym))
        )
        .show_modal(titulo, "table", data_modal, modal_id)
      }, ignoreNULL = TRUE)
    }
    
    .obs_resumen_click("resumen_click_p",  panel_p,  "Presupuestados")
    .obs_resumen_click("resumen_click_np", panel_np, "No Presupuestados")
    
    # ==========================================================================
    # TABLA RANGOS DE TASA DE FACTURACION
    # ==========================================================================
    
    .RANGOS_BREAKS <- c(0, 10, 30, 60, 80, 100)
    .RANGOS_LABELS <- c("0% - 10%", "10% - 30%", "30% - 60%", "60% - 80%", "80% - 100%")
    
    .rango_tasa <- function(pct) {
      as.character(cut(pct,
                       breaks = .RANGOS_BREAKS, labels = .RANGOS_LABELS,
                       include.lowest = TRUE, right = TRUE
      ))
    }
    
    .tabla_rangos_reactable <- function(df_rangos, click_id_ns) {
      js_click <- paste0(
        "function(rowInfo) {",
        "  Shiny.setInputValue('", click_id_ns, "', {",
        "    rango: rowInfo.values['rango'],",
        "    nonce: Math.random()",
        "  }, {priority: 'event'});",
        "}"
      )
      reactable::reactable(
        data            = df_rangos,
        onClick         = JS(js_click),
        highlight       = TRUE,
        bordered        = TRUE,
        compact         = TRUE,
        defaultPageSize = 6,
        columns = list(
          rango            = reactable::colDef(name = "Rango Tasa Facturacion", minWidth = 180),
          n_ucs            = reactable::colDef(name = "UCs",         minWidth = 80,
                                               cell = function(v) format(v, big.mark = ",")),
          pct_ucs          = reactable::colDef(name = "% Grupo",     minWidth = 80,
                                               cell = function(v) paste0(v, "%")),
          sacos_total      = reactable::colDef(name = "Sacos",       minWidth = 100,
                                               cell = function(v) .fmt_num(v)),
          margen_total     = reactable::colDef(name = "Margen",      minWidth = 110,
                                               cell = function(v) .fmt_num(v)),
          ppto_sacos       = reactable::colDef(
            name = "Ppto Sacos",  minWidth = 100,
            cell = function(v) if (is.na(v)) "\u2014" else .fmt_num(v)
          ),
          ppto_margen      = reactable::colDef(
            name = "Ppto Margen", minWidth = 110,
            cell = function(v) if (is.na(v)) "\u2014" else .fmt_num(v)
          ),
          cumpl_sacos_pct  = reactable::colDef(
            name  = "% Sacos",  minWidth = 80,
            cell  = function(v) .fmt_pct(v),
            style = function(v) list(background = .color_cumpl_bg(v))
          ),
          cumpl_margen_pct = reactable::colDef(
            name  = "% Margen", minWidth = 80,
            cell  = function(v) .fmt_pct(v),
            style = function(v) list(background = .color_cumpl_bg(v))
          )
        ),
        theme = reactable::reactableTheme(
          headerStyle = list(fontWeight = "600", fontSize = "12px"),
          cellStyle   = list(fontSize = "12px", cursor = "pointer")
        )
      )
    }
    
    .build_rangos_df <- function(panel) {
      ids <- panel %>% distinct(cliente_id) %>% pull()
      n_m <- n_meses()
      
      tasa_ids <- tasa_fact_uc() %>%
        filter(cliente_id %in% ids) %>%
        mutate(rango = .rango_tasa(tasa_fact_pct))
      
      real_ids <- real_acum() %>% filter(cliente_id %in% ids)
      
      ppto_indiv <- marca_presupuesto() %>%
        filter(presupuestada == "PRESUPUESTADA", cliente_id %in% ids) %>%
        transmute(
          cliente_id,
          ppto_sacos_uc  = ppto_sacos_anual  / 12 * n_m,
          ppto_margen_uc = ppto_margen_anual / 12 * n_m
        )
      
      ppto_grupal_uc <- panel_cumpl_grupal() %>%
        filter(cliente_id %in% ids) %>%
        group_by(cliente_id) %>%
        summarise(
          ppto_sacos_uc  = sum(ppto_sacos_mes,  na.rm = TRUE),
          ppto_margen_uc = sum(ppto_margen_mes, na.rm = TRUE),
          .groups = "drop"
        )
      
      ppto_all <- bind_rows(ppto_indiv, ppto_grupal_uc) %>%
        group_by(cliente_id) %>%
        summarise(
          ppto_sacos_uc  = sum(ppto_sacos_uc,  na.rm = TRUE),
          ppto_margen_uc = sum(ppto_margen_uc, na.rm = TRUE),
          .groups = "drop"
        )
      
      tasa_ids %>%
        left_join(real_ids, by = "cliente_id") %>%
        left_join(ppto_all, by = "cliente_id") %>%
        mutate(
          sacos_total  = replace_na(sacos_total,  0),
          margen_total = replace_na(margen_total, 0)
        ) %>%
        group_by(rango) %>%
        summarise(
          n_ucs        = n(),
          sacos_total  = sum(sacos_total,  na.rm = TRUE),
          margen_total = sum(margen_total, na.rm = TRUE),
          ppto_sacos   = if (any(!is.na(ppto_sacos_uc)))
            sum(ppto_sacos_uc,  na.rm = TRUE) else NA_real_,
          ppto_margen  = if (any(!is.na(ppto_margen_uc)))
            sum(ppto_margen_uc, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) %>%
        right_join(tibble(rango = .RANGOS_LABELS), by = "rango") %>%
        mutate(
          rango        = factor(rango, levels = .RANGOS_LABELS),
          n_ucs        = replace_na(n_ucs, 0L),
          sacos_total  = replace_na(sacos_total, 0),
          margen_total = replace_na(margen_total, 0),
          pct_ucs      = round(n_ucs / pmax(sum(n_ucs), 1) * 100, 1),
          cumpl_sacos_pct  = if_else(
            !is.na(ppto_sacos)  & ppto_sacos  > 0,
            round(sacos_total  / ppto_sacos  * 100, 1), NA_real_
          ),
          cumpl_margen_pct = if_else(
            !is.na(ppto_margen) & ppto_margen > 0,
            round(margen_total / ppto_margen * 100, 1), NA_real_
          )
        ) %>%
        arrange(rango) %>%
        mutate(rango = as.character(rango)) %>%
        select(rango, n_ucs, pct_ucs, sacos_total, margen_total,
               ppto_sacos, ppto_margen, cumpl_sacos_pct, cumpl_margen_pct)
    }
    
    output$tabla_p_rangos_fact  <- reactable::renderReactable({
      req(tasa_fact_uc(), panel_p(), real_acum())
      .tabla_rangos_reactable(.build_rangos_df(panel_p()), ns("click_rangos_p"))
    })
    output$tabla_np_rangos_fact <- reactable::renderReactable({
      req(tasa_fact_uc(), panel_np(), real_acum())
      .tabla_rangos_reactable(.build_rangos_df(panel_np()), ns("click_rangos_np"))
    })
    
    # Click en rango de tasa → modal dinamico con .show_modal
    .show_modal_rango <- function(click_input, panel_fn, grupo_label) {
      observeEvent(input[[click_input]], {
        req(!is.null(input[[click_input]]$rango))
        rango_sel  <- input[[click_input]]$rango
        ids_rango  <- tasa_fact_uc() %>%
          filter(
            cliente_id %in% (panel_fn() %>% distinct(cliente_id) %>% pull()),
            .rango_tasa(tasa_fact_pct) == rango_sel
          ) %>%
          pull(cliente_id)
        data_modal <- corte_ult() %>% filter(cliente_id %in% ids_rango)
        modal_id   <- paste0(
          "rango_",
          gsub("[^A-Za-z0-9]", "_", paste(grupo_label, rango_sel))
        )
        .show_modal(paste0(grupo_label, " — Tasa ", rango_sel),
                    "percentage", data_modal, modal_id)
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
    }
    
    .show_modal_rango("click_rangos_p",  panel_p,  "Presupuestados")
    .show_modal_rango("click_rangos_np", panel_np, "No Presupuestados")
    
    # ==========================================================================
    # MATRIZ DE TRANSICION
    # ==========================================================================
    
    .tabla_transicion_gt <- function(panel, lbl_ini, lbl_fin) {
      estados_ord <- c("CLIENTE ACTIVO", "CLIENTE A RECUPERAR", "NUEVO DEL PERIODO")
      estados_ord <- estados_ord[estados_ord %in% unique(panel$estado)]
      
      trans_raw <- panel %>%
        arrange(cliente_id, ym) %>%
        group_by(cliente_id) %>%
        mutate(estado_sig = lead(estado)) %>%
        ungroup() %>%
        filter(!is.na(estado_sig))
      
      mat <- trans_raw %>%
        count(estado, estado_sig, name = "n") %>%
        complete(estado = estados_ord, estado_sig = estados_ord, fill = list(n = 0L))
      
      tot_fila <- mat %>%
        group_by(estado) %>%
        summarise(n_total_fila = sum(n), .groups = "drop")
      
      mat_pct <- mat %>%
        left_join(tot_fila, by = "estado") %>%
        mutate(
          pct   = if_else(n_total_fila > 0, round(100 * n / n_total_fila, 1), NA_real_),
          celda = paste0(n, " (", ifelse(is.na(pct), "\u2014", paste0(pct, "%")), ")")
        )
      
      wide <- mat_pct %>%
        select(estado, estado_sig, celda) %>%
        pivot_wider(names_from = estado_sig, values_from = celda, values_fill = "\u2014") %>%
        left_join(tot_fila %>% rename(`Total fila` = n_total_fila), by = "estado") %>%
        mutate(estado = factor(estado, levels = estados_ord)) %>%
        arrange(estado) %>%
        mutate(estado = as.character(estado))
      
      tot_col_vals <- mat %>%
        group_by(estado_sig) %>%
        summarise(n_total = sum(n), .groups = "drop")
      
      tot_col_wide  <- tot_col_vals %>%
        pivot_wider(names_from = estado_sig, values_from = n_total)
      gran_total    <- sum(unlist(tot_col_wide), na.rm = TRUE)
      
      fila_tot <- tibble(estado = "Total columna", `Total fila` = as.character(gran_total))
      for (est in estados_ord) {
        val           <- if (est %in% names(tot_col_wide)) tot_col_wide[[est]] else 0L
        fila_tot[[est]] <- as.character(replace_na(val, 0L))
      }
      fila_tot <- fila_tot %>% select(all_of(names(wide)))
      
      wide     <- wide %>% mutate(`Total fila` = as.character(`Total fila`))
      df_final <- bind_rows(wide, fila_tot)
      
      cols_estado  <- estados_ord[estados_ord %in% names(df_final)]
      n_ucs_panel  <- n_distinct(panel$cliente_id)
      n_meses_real <- n_distinct(panel$ym)
      nota_pie     <- paste0(
        "Transiciones registradas: ", gran_total, " = ",
        n_ucs_panel, " UCs x ", n_meses_real - 1L, " transiciones posibles.",
        " El total por fila representa cuantas veces una UC estuvo en ese estado",
        " entre t y t+1 a lo largo del periodo."
      )
      
      df_final %>%
        gt(rowname_col = "estado") %>%
        tab_header(
          title    = "Matriz de Transicion de Estado",
          subtitle = paste0("Periodo: ", lbl_ini, " → ", lbl_fin,
                            "  |  Toda la poblacion del panel")
        ) %>%
        tab_spanner(label = "→ Estado t+1", columns = all_of(cols_estado)) %>%
        tab_source_note(source_note = nota_pie) %>%
        tab_style(cell_text(weight = "bold"), cells_stub()) %>%
        tab_style(cell_text(weight = "bold"), cells_column_labels()) %>%
        tab_style(
          style     = list(cell_fill(color = "#EEF2FF"), cell_text(weight = "bold")),
          locations = cells_body(columns = `Total fila`)
        ) %>%
        tab_style(
          style     = list(cell_fill(color = "#EEF2FF"), cell_text(weight = "bold")),
          locations = cells_body(rows = estado == "Total columna")
        ) %>%
        tab_style(
          style     = list(cell_fill(color = "#EEF2FF"), cell_text(weight = "bold")),
          locations = cells_stub(rows = estado == "Total columna")
        ) %>%
        tab_options(
          table.font.size             = px(12),
          heading.title.font.size     = px(13),
          heading.title.font.weight   = "bold",
          heading.subtitle.font.size  = px(11),
          source_notes.font.size      = px(10),
          table.width                 = pct(100)
        )
    }
    
    output$tabla_p_transicion  <- gt::render_gt({
      req(panel_p())
      .tabla_transicion_gt(
        panel_p(),
        format(mes_inicio(), "%B %Y"),
        format(ultimo_mes(), "%B %Y")
      )
    })
    output$tabla_np_transicion <- gt::render_gt({
      req(panel_np())
      .tabla_transicion_gt(
        panel_np(),
        format(mes_inicio(), "%B %Y"),
        format(ultimo_mes(), "%B %Y")
      )
    })
    
    # ==========================================================================
    # SANKEY MES A MES
    # ==========================================================================
    
    .build_sankey_data <- function(panel) {
      meses <- meses_periodo()
      if (length(meses) < 2) return(NULL)
      
      trans <- panel %>%
        arrange(cliente_id, ym) %>%
        group_by(cliente_id) %>%
        mutate(estado_sig = lead(estado), ym_sig = lead(ym)) %>%
        ungroup() %>%
        filter(!is.na(estado_sig))
      
      .lbl <- function(e) dplyr::recode(e,
                                        "CLIENTE ACTIVO"      = "Activo",
                                        "CLIENTE A RECUPERAR" = "A Recuperar",
                                        "NUEVO DEL PERIODO"   = "Nuevo",
                                        .default              = e
      )
      
      nodos_orig <- trans %>%
        mutate(nodo = paste0(.lbl(estado),     "\n", format(ym,     "%b %y")))
      nodos_dest <- trans %>%
        mutate(nodo = paste0(.lbl(estado_sig), "\n", format(ym_sig, "%b %y")))
      
      all_nodos <- unique(c(nodos_orig$nodo, nodos_dest$nodo))
      nodo_idx  <- tibble(nodo = all_nodos) %>%
        mutate(
          mes_part    = str_extract(nodo, "(?<=\n).+"),
          estado_part = str_extract(nodo, "^[^\n]+")
        ) %>%
        arrange(mes_part, estado_part) %>%
        mutate(idx = row_number() - 1L)
      
      flujos <- bind_cols(
        nodos_orig %>% select(nodo_orig = nodo),
        nodos_dest %>% select(nodo_dest = nodo)
      ) %>%
        count(nodo_orig, nodo_dest, name = "n") %>%
        left_join(
          nodo_idx %>% select(nodo, src = idx) %>% rename(nodo_orig = nodo),
          by = "nodo_orig"
        ) %>%
        left_join(
          nodo_idx %>% select(nodo, tgt = idx) %>% rename(nodo_dest = nodo),
          by = "nodo_dest"
        )
      
      color_nodo <- nodo_idx %>%
        arrange(idx) %>%
        mutate(color = dplyr::recode(estado_part,
                                     "Activo"      = "#2C7BB6",
                                     "A Recuperar" = "#F4A820",
                                     "Nuevo"       = "#27AE60",
                                     .default      = "#AAAAAA"
        )) %>%
        pull(color)
      
      list(nodos = nodo_idx, flujos = flujos, color_nodo = color_nodo)
    }
    
    .graf_sankey <- function(panel) {
      d <- .build_sankey_data(panel)
      if (is.null(d)) {
        return(plot_ly() %>%
                 layout(title = list(text = "Se requieren al menos 2 meses en el periodo")))
      }
      
      plot_ly(
        type        = "sankey",
        orientation = "h",
        arrangement = "snap",
        node = list(
          label     = d$nodos$nodo,
          color     = d$color_nodo,
          pad       = 12,
          thickness = 18,
          line      = list(color = "white", width = 0.5),
          hovertemplate = paste0(
            "<b>%{label}</b><br>",
            "Flujo total: <b>%{value}</b> UCs",
            "<extra></extra>"
          )
        ),
        link = list(
          source        = d$flujos$src,
          target        = d$flujos$tgt,
          value         = d$flujos$n,
          color         = "rgba(180,180,180,0.30)",
          hovertemplate = paste0(
            "<b>%{source.label}</b>",
            " → <b>%{target.label}</b><br>",
            "Unidades Comerciales: <b>%{value}</b>",
            "<extra></extra>"
          )
        )
      ) %>%
        layout(
          title = list(
            text    = "Flujo de Transiciones mes a mes — Poblacion Base",
            font    = list(size = 13, color = "#333"),
            x       = 0,
            xanchor = "left"
          ),
          font       = list(size = 10),
          hoverlabel = list(
            bgcolor     = "rgba(255,255,255,1)",
            bordercolor = "#cccccc",
            font        = list(size = 11, color = "#000000"),
            align       = "left",
            namelength  = -1
          ),
          margin = list(l = 10, r = 10, t = 40, b = 10)
        ) %>%
        config(displayModeBar = FALSE)
    }
    
    output$sankey_p  <- renderPlotly({ req(panel_p(),  n_meses() >= 2); .graf_sankey(panel_p())  })
    output$sankey_np <- renderPlotly({ req(panel_np(), n_meses() >= 2); .graf_sankey(panel_np()) })
    
    # ==========================================================================
    # DESCARGABLES — BotonDescarga de Racafe maneja el boton; solo el handler
    # ==========================================================================
    
    .dl_handler <- function(datos_fn, prefijo) {
      downloadHandler(
        filename = function() paste0(prefijo, "_", Sys.Date(), ".xlsx"),
        content  = function(file) writexl::write_xlsx(datos_fn(), path = file)
      )
    }
    
    output$dl_presup <- .dl_handler(
      function() panel_p() %>%
        left_join(
          panel_cumpl() %>%
            select(cliente_id, ym, ppto_sacos_mes, real_sacos,
                   cumpl_sacos_pct, ppto_margen_mes, real_margen, cumpl_margen_pct),
          by = c("cliente_id", "ym")
        ) %>%
        left_join(catalogo_rs(),  by = "cliente_id") %>%
        left_join(tasa_fact_uc(), by = "cliente_id") %>%
        arrange(cliente_id, ym),
      "presupuestados"
    )
    
    output$dl_np_todos <- .dl_handler(
      function() panel_np() %>%
        left_join(catalogo_rs(),  by = "cliente_id") %>%
        left_join(tasa_fact_uc(), by = "cliente_id") %>%
        arrange(cliente_id, ym),
      "no_presupuestados"
    )
    
    output$dl_np_nuevos <- .dl_handler(
      function() panel_altas() %>%
        filter(presupuestada == "NO PRESUPUESTADA") %>%
        left_join(catalogo_rs(),  by = "cliente_id") %>%
        left_join(tasa_fact_uc(), by = "cliente_id") %>%
        arrange(cliente_id, ym),
      "altas_cohorte"
    )
    
    output$dl_np_recuperar <- .dl_handler(
      function() panel_np() %>%
        filter(estado == "CLIENTE A RECUPERAR") %>%
        left_join(catalogo_rs(),  by = "cliente_id") %>%
        left_join(tasa_fact_uc(), by = "cliente_id") %>%
        arrange(cliente_id, ym),
      "clientes_a_recuperar"
    )
    
  })
}

# ==============================================================================
# APP DE PRUEBA
# ==============================================================================

ui <- bs4Dash::bs4DashPage(
  title   = "Analisis de Cohortes Downstream",
  header  = bs4Dash::bs4DashNavbar(skin = "dark"),
  sidebar = bs4Dash::bs4DashSidebar(
    skin = "dark",
    bs4Dash::bs4SidebarMenu(
      bs4Dash::bs4SidebarMenuItem(
        "Cohorte Operativa", tabName = "cohortes",
        icon = shiny::icon("layer-group")
      )
    )
  ),
  body = bs4Dash::bs4DashBody(
    bs4Dash::bs4TabItems(
      bs4Dash::bs4TabItem(
        "cohortes",
        fluidRow(
          column(4,
                 dateRangeInput("FT_Fecha", "",
                                start     = PrimerDia(Sys.Date(), uni = "year"),
                                min       = min(data$FecFact, na.rm = TRUE),
                                end       = max(data$FecFact, na.rm = TRUE),
                                max       = max(data$FecFact, na.rm = TRUE),
                                language  = "es",
                                separator = " - ",
                                format    = "dd/mm/yyyy"
                 )
          )
        ),
        CohortesUI("cohortes")
      )
    )
  ),
  footer = bs4Dash::bs4DashFooter(
    left  = "Racafe Analytics — Analisis Downstream",
    right = format(Sys.Date(), "%Y")
  )
)

server <- function(input, output, session) {
  Cohortes(
    "cohortes",
    data_tx     = reactive(BaseDatos),
    fecha_rango = reactive(input$FT_Fecha)
  )
}

shiny::shinyApp(ui, server)
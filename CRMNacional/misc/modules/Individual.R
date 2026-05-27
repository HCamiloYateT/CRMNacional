

# Global — Catálogos y helpers ------------------------------------------------

.IND_CHO <- Choices()

.IND_NEG_CONTACTADO <- c(
  "", grep("OPORTUNIDAD|DESCARTADO", .IND_CHO$estadonegocio, value = TRUE)
)
.IND_NEG_MUERTO <- c(
  "", grep("OPORTUNIDAD|DESCARTADO", .IND_CHO$estadonegocio,
           value = TRUE, invert = TRUE)
)

con_vacio <- function(x) unique(c("", x))

ind_picker <- function(inputId, label, choices, selected = NULL, ns) {
  pickerInput(ns(inputId), label = h6(label), width = "100%",
              choices = choices, multiple = FALSE, options = pick_opt(choices),
              selected = selected %||% choices[1])
}

# Formato numérico colombiano — no depende de options(OutDec)
fmt_co <- function(x, dec = 0) {
  formatC(as.numeric(x), digits = dec, format = "f",
          big.mark = ".", decimal.mark = ",")
}


# ==============================================================================
# Submódulo 1: IndContacto — solo tabla de contacto
# Los KPIs están en el orquestador mediante CajaModal.
# ==============================================================================

IndContactoUI <- function(id) {
  ns <- NS(id)
  box(title = "Información de Contacto", width = 12,
      status = "white", collapsible = TRUE, collapsed = FALSE,
      gt_output(ns("tbl_contacto")))
}
IndContacto <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$tbl_contacto <- render_gt({
      dat() %>%
        summarise(
          `NIT Principal`      = Unicos(as.character(CliNitPpal)),
          `Asesor`             = paste(unique(na.omit(Asesor)),         collapse = " / "),
          `Segmento`           = paste(unique(na.omit(Segmento)),       collapse = " / "),
          `Tipo Cliente`       = paste(unique(na.omit(SegmentoRacafe)), collapse = " / "),
          `Contacto`           = paste(unique(na.omit(CliCont)),        collapse = " / "),
          `Dirección`          = paste(unique(na.omit(CliDir)),         collapse = " / "),
          `Ciudad`             = paste(unique(na.omit(CiuExtNom)),      collapse = " / "),
          `Departamento`       = paste(unique(na.omit(Depto)),          collapse = " / "),
          `Teléfono`           = paste(unique(na.omit(CliTel)),         collapse = " / "),
          `Contacto Comercial` = paste(unique(na.omit(CliConCom)),      collapse = " / "),
          `Tel. Comercial`     = paste(unique(na.omit(CliTelCom)),      collapse = " / "),
          `Email Comercial`    = paste(unique(na.omit(CliEmlCom)),      collapse = " / ")
        ) %>%
        pivot_longer(everything(), names_to = "Campo", values_to = "Valor") %>%
        gt() %>% gt_minimal_style() %>%
        cols_label(Campo = "", Valor = "") %>%
        cols_width(Campo ~ px(200)) %>%
        tab_style(style = cell_text(weight = "bold", color = "#2c3e50"),
                  locations = cells_body(columns = Campo)) %>%
        tab_options(table.width = pct(100))
    })
  })
}


# ==============================================================================
# Submódulo 2: IndHistorico — serie mensual de facturación
# ==============================================================================

IndHistoricoUI <- function(id) {
  ns <- NS(id)
  box(title = "Histórico de Facturación", width = 12,
      status = "white", collapsible = TRUE, collapsed = FALSE,
      fluidRow(
        column(6,
               BotonesRadiales("IND_VarHist", NULL, c("Sacos", "Margen"), "Sacos", ns = ns)),
        column(6, style = "text-align: right;",
               BotonesRadiales("IND_TipoHist", NULL,
                               c("Mensual" = "mensual", "Acumulado" = "acumulado"),
                               "mensual", ns = ns))
      ),
      plotlyOutput(ns("plt_hist"), height = "340px"))
}
IndHistorico <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ts_base <- reactive({
      dat() %>%
        filter(!is.na(FecFact)) %>%
        mutate(Mes = PrimerDia(FecFact)) %>%
        group_by(Mes) %>%
        summarise(Sacos = sum(SacFact70, na.rm=TRUE), Margen = sum(Margen, na.rm=TRUE),
                  .groups = "drop") %>%
        arrange(Mes)
    })
    
    ts_acum <- reactive({
      ts_base() %>% mutate(Sacos = cumsum(Sacos), Margen = cumsum(Margen))
    })
    
    output$plt_hist <- renderPlotly({
      req(input$IND_VarHist, input$IND_TipoHist)
      df        <- if (input$IND_TipoHist == "mensual") ts_base() else ts_acum()
      var       <- input$IND_VarHist
      es_margen <- var == "Margen"
      media     <- mean(df[[var]], na.rm = TRUE)
      colores   <- ifelse(df[[var]] >= media, "#1a5276", "#c0392b")
      txt       <- if (es_margen) paste0("$", fmt_co(df[[var]]/1e6, 1), " MM") else fmt_co(df[[var]])
      hover_tmpl <- paste0("<b>%{x|%b %Y}</b><br>", var, ": ", txt, "<extra></extra>")
      
      p_base <- if (input$IND_TipoHist == "mensual") {
        plot_ly(df, x = ~Mes, y = ~.data[[var]], type = "bar",
                marker = list(color = colores), text = txt,
                textposition = "outside", hovertemplate = hover_tmpl)
      } else {
        plot_ly(df, x = ~Mes, y = ~.data[[var]], type = "scatter",
                mode = "lines+markers", line = list(color = "#1a5276", width = 2),
                marker = list(color = "#1a5276", size = 6), text = txt,
                hovertemplate = hover_tmpl)
      }
      p_base %>%
        layout(xaxis = list(title = "", tickformat = "%b %Y"),
               yaxis = list(title = var, tickformat = if (es_margen) "$,.0f" else ",.0f"),
               plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
               showlegend = FALSE,
               shapes = list(list(type = "line", x0 = min(df$Mes), x1 = max(df$Mes),
                                  y0 = media, y1 = media,
                                  line = list(color = "#e67e22", dash = "dash", width = 1.5))))
    })
  })
}


# ==============================================================================
# Submódulo 3: IndParticipacion — treemap jerárquico Categoría > Producto
# Un solo toggle: Sacos / Margen. Sin botón de dimensión.
# ids únicos para productos evitan colisiones de nombres entre categorías.
# ==============================================================================

IndParticipacionUI <- function(id) {
  ns <- NS(id)
  box(title = "Distribución por Categoría y Producto", width = 12,
      status = "white", collapsible = TRUE, collapsed = FALSE,
      fluidRow(
        column(6,
               BotonesRadiales("IND_VarPart", NULL, c("Sacos", "Margen"), "Sacos", ns = ns))
      ),
      plotlyOutput(ns("plt_part"), height = "380px"))
}
IndParticipacion <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$plt_part <- renderPlotly({
      req(input$IND_VarPart)
      med_var <- input$IND_VarPart
      
      df_prod <- dat() %>%
        filter(!is.na(Categoria), !is.na(Producto)) %>%
        group_by(Categoria, Producto) %>%
        summarise(Sacos = sum(SacFact70, na.rm=TRUE), Margen = sum(Margen, na.rm=TRUE),
                  .groups = "drop") %>%
        filter(!!sym(med_var) > 0)
      
      df_cat <- df_prod %>%
        group_by(Categoria) %>%
        summarise(Sacos = sum(Sacos, na.rm=TRUE), Margen = sum(Margen, na.rm=TRUE),
                  .groups = "drop")
      
      # ids únicos para productos: evita colisión de nombres entre categorías
      prod_ids <- paste0(df_prod$Categoria, " | ", df_prod$Producto)
      n_cat    <- nrow(df_cat)
      pal_cat  <- colorRampPalette(c("#1a5276", "#2980b9", "#85c1e9", "#d6eaf8"))(n_cat)
      cat_map  <- setNames(pal_cat, df_cat$Categoria)
      
      plot_ly(
        type         = "treemap",
        ids          = c(df_cat$Categoria, prod_ids),
        labels       = c(df_cat$Categoria, df_prod$Producto),
        parents      = c(rep("", n_cat), df_prod$Categoria),
        values       = c(df_cat[[med_var]], df_prod[[med_var]]),
        branchvalues = "total",
        textinfo     = "label+percent parent",
        marker       = list(
          colors = c(pal_cat, cat_map[df_prod$Categoria]),
          pad    = list(t = 1, l = 1, r = 1, b = 1)
        ),
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          med_var, ": %{value:,.0f}<br>",
          "% en categoría: %{percentParent:.1%}<br>",
          "% global: %{percentRoot:.1%}<extra></extra>"
        )
      ) %>%
        layout(
          plot_bgcolor  = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          margin        = list(t = 0, l = 0, r = 0, b = 0)
        )
    })
  })
}


# ==============================================================================
# Submódulo 4: IndPendientes — lotes pendientes con asignación de OC
# KPIs internos via CajaModal (status white).
# ==============================================================================

IndPendientesUI <- function(id) {
  ns <- NS(id)
  box(title = tagList(icon("clock"), " Lotes Pendientes"), width = 12,
      status = "white", collapsible = TRUE, collapsed = FALSE,
      fluidRow(
        column(4, racafeModulos::CajaModalUI(ns("kpi_pend_prod"))),
        column(4, racafeModulos::CajaModalUI(ns("kpi_pend_desp"))),
        column(4, racafeModulos::CajaModalUI(ns("kpi_pend_fact")))
      ),
      fluidRow(
        column(4, createSwitch("PEN_PendProducir",     "Pend. por Producir",         ns = ns)),
        column(4, createSwitch("PEN_PendDespachar",    "Pend. por Despachar",        ns = ns)),
        column(4, createSwitch("PEN_DespPendFacturar", "Desp. Pend. Facturar", TRUE, ns = ns))
      ),
      br(),
      uiOutput(ns("bloque_pend")))
}
IndPendientes <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dd_oc_rv <- reactiveVal(NULL)
    AsignarOrdenCompra("mod_oc", dd_data = reactive(dd_oc_rv()))
    
    racafeModulos::CajaModal(
      "kpi_pend_prod",
      valor = reactive(sum(dat()$PendProducir, na.rm = TRUE)),
      formato = "coma", texto = "Sacos Pend. Producir", icono = "industry",
      colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer  = reactive(paste0(sum(dat()$PendProducir > 0.1, na.rm=TRUE), " lotes"))
    )
    racafeModulos::CajaModal(
      "kpi_pend_desp",
      valor = reactive(sum(dat()$PendDespachar, na.rm = TRUE)),
      formato = "coma", texto = "Sacos Pend. Despachar", icono = "truck",
      colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer  = reactive(paste0(sum(dat()$PendDespachar > 0.1, na.rm=TRUE), " lotes"))
    )
    racafeModulos::CajaModal(
      "kpi_pend_fact",
      valor = reactive(sum(dat()$PendFacturar, na.rm = TRUE)),
      formato = "coma", texto = "Sacos Pend. Facturar", icono = "money-bill",
      colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer  = reactive(paste0(sum(dat()$PendFacturar > 0.1, na.rm=TRUE), " lotes"))
    )
    
    data_pend_r <- reactive({
      pend_prod <- isTRUE(input$PEN_PendProducir)
      pend_desp <- isTRUE(input$PEN_PendDespachar)
      pend_fact <- isTRUE(input$PEN_DespPendFacturar)
      df <- dat()
      if (any(c(pend_prod, pend_desp, pend_fact))) {
        df <- df %>%
          filter((pend_prod & PendProducir  > 0.1) |
                   (pend_desp & PendDespachar > 0.1) |
                   (pend_fact & PendFacturar  > 0.1))
      }
      df %>%
        select(Sucursal, CLLotCod, PdcRefCli, SacLote, FecAsignLote,
               OrdenCompra, CLLinNegNo, Categoria, Producto,
               PendProducir, PendDespachar, PendFacturar) %>%
        arrange(desc(FecAsignLote)) %>%
        mutate(Asignar = "btn", CLLotCod = as.character(CLLotCod)) %>%
        select(Asignar, everything())
    })
    
    output$bloque_pend <- renderUI({
      if (nrow(data_pend_r()) == 0) {
        return(div(style = "padding:20px;text-align:center;color:#6c757d;font-style:italic;",
                   icon("check-circle", style = "font-size:20px;display:block;margin-bottom:6px;"),
                   "Sin lotes pendientes para los filtros seleccionados."))
      }
      TablaReactableUI(ns("tbl_pend"), titulo = "Lotes Pendientes",
                       footer = "Clic en el bot\u00f3n para asignar una orden de compra.",
                       footer_tipo = "info")
    })
    
    TablaReactable(
      id = "tbl_pend", data = data_pend_r, modo_seleccion = "celda",
      id_col = NULL, col_header_n = 1L, cols_activos = "Asignar",
      sortable = FALSE, searchable = TRUE, page_size = 99999L,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE,
      modal_icon = "arrow-right", modal_size = "xl",
      modal_titulo_fn = function(sel) {
        paste0("Asignar OC \u2014 Lote ", sel$fila$CLLotCod[[1]])
      },
      modal_pre_fn = function(sel) {
        if (isTRUE(sel$fila$Asignar == "")) return(invisible(NULL))
        dd_oc_rv(list(fila_completa = sel$fila))
      },
      modal_contenido_fn = function(sel) AsignarOrdenCompraUI(ns("mod_oc")),
      columnas = list(
        Asignar = reactable::colDef(name = "", minWidth = 50, html = TRUE,
                                    cell = function(v) {
                                      if (v == "") return("")
                                      as.character(tags$span(
                                        style = paste("display:inline-flex;align-items:center;justify-content:center;",
                                                      "width:26px;height:26px;border-radius:6px;",
                                                      "background:#C11007;color:white;font-size:12px;cursor:pointer;"),
                                        icon("arrow-right")))
                                    }),
        Sucursal      = reactable::colDef(name = "Sucursal",      minWidth = 90),
        CLLotCod      = reactable::colDef(name = "Lote",          minWidth = 80),
        PdcRefCli     = reactable::colDef(name = "Pedido",        minWidth = 90),
        OrdenCompra   = reactable::colDef(name = "Orden Compra",  minWidth = 120),
        FecAsignLote  = reactable::colDef(name = "Fecha Asig.",   minWidth = 110),
        CLLinNegNo    = reactable::colDef(name = "L\u00ednea Negocio", minWidth = 110),
        Categoria     = reactable::colDef(name = "Categor\u00eda",     minWidth = 100),
        Producto      = reactable::colDef(name = "Producto",      minWidth = 120),
        SacLote       = reactable::colDef(name = "Sacos",         minWidth = 80,
                                          cell = function(v) if (is.na(v)) "\u2014" else fmt_co(v)),
        PendProducir  = reactable::colDef(name = "Pend. Producir",  minWidth = 120,
                                          cell = function(v) if (is.na(v)) "\u2014" else fmt_co(v)),
        PendDespachar = reactable::colDef(name = "Pend. Despachar", minWidth = 130,
                                          cell = function(v) if (is.na(v)) "\u2014" else fmt_co(v)),
        PendFacturar  = reactable::colDef(name = "Pend. Facturar",  minWidth = 120,
                                          cell = function(v) if (is.na(v)) "\u2014" else fmt_co(v))
      )
    )
  })
}


# ==============================================================================
# Submódulo 5: IndRFM — RFM, CLV y Churn (bases Sacos S / Margen M)
# KPIs via CajaModal todos blancos.
# ==============================================================================

IndRFMUI <- function(id) {
  ns <- NS(id)
  box(title = tagList(icon("star"), " RFM, CLV & Churn"), width = 12,
      status = "white", collapsible = TRUE, collapsed = TRUE,
      fluidRow(
        column(4,
               BotonesRadiales("IND_RFM_base", NULL,
                               c("Sacos" = "S", "Margen" = "M"), "S", ns = ns))
      ),
      fluidRow(
        column(3, racafeModulos::CajaModalUI(ns("kpi_churn"))),
        column(3, racafeModulos::CajaModalUI(ns("kpi_clv"))),
        column(3, racafeModulos::CajaModalUI(ns("kpi_pred_sac"))),
        column(3, racafeModulos::CajaModalUI(ns("kpi_rec_days")))
      ),
      fluidRow(
        column(6,
               box(title = "Radar RFM (1-5)", width = 12, status = "white",
                   collapsible = TRUE, collapsed = FALSE,
                   plotlyOutput(ns("plt_rfm"), height = "300px"))),
        column(6,
               box(title = "Segmentaci\u00f3n Anal\u00edtica", width = 12, status = "white",
                   collapsible = TRUE, collapsed = FALSE,
                   gt_output(ns("tbl_segmento"))))
      ))
}
IndRFM <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rfm_snap <- reactive({ dat()[1, ] })
    
    racafeModulos::CajaModal(
      "kpi_churn",
      valor = reactive({
        ch <- as.numeric(rfm_snap()$Churn %||% NA)
        if (is.na(ch)) 0 else ch * 100
      }),
      formato = "porcentaje", texto = "Prob. P\u00e9rdida (Churn %)",
      icono = "user-slash", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive({
        ch <- as.numeric(rfm_snap()$Churn %||% NA)
        if (is.na(ch)) "N/D"
        else if (ch < 0.10) "Riesgo bajo"
        else if (ch < 0.30) "Riesgo medio" else "Riesgo alto"
      })
    )
    racafeModulos::CajaModal(
      "kpi_clv",
      valor = reactive({
        marg <- sum(dat()$Margen, na.rm = TRUE)
        anos <- max(as.numeric(difftime(max(dat()$FecFact, na.rm=TRUE),
                                        min(dat()$FecFact, na.rm=TRUE),
                                        units = "days")) / 365.25, 1)
        ch   <- as.numeric(rfm_snap()$Churn %||% 0.20)
        ch   <- if (is.na(ch) || ch <= 0) 0.01 else ch
        (marg / anos / ch) / 1e6
      }),
      formato = "dinero", texto = "CLV Estimado (MM)",
      icono = "gem", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive("Margen anualizado / Churn")
    )
    racafeModulos::CajaModal(
      "kpi_pred_sac",
      valor = reactive(as.numeric(rfm_snap()$SacosPred %||% 0)),
      formato = "coma", texto = "Sacos Predichos",
      icono = "chart-line", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive("Pr\u00f3ximo per\u00edodo (modelo ML)")
    )
    racafeModulos::CajaModal(
      "kpi_rec_days",
      valor = reactive({
        req(input$IND_RFM_base)
        col <- paste0("recency_days", input$IND_RFM_base)
        as.numeric(rfm_snap()[[col]] %||% 0)
      }),
      formato = "entero", texto = "D\u00edas sin Comprar",
      icono = "clock", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive({
        req(input$IND_RFM_base)
        dias <- as.numeric(rfm_snap()[[paste0("recency_days", input$IND_RFM_base)]] %||% NA)
        if (is.na(dias)) "N/D"
        else if (dias <= 30) "Activo reciente"
        else if (dias <= 90) "En seguimiento" else "Recuperaci\u00f3n requerida"
      })
    )
    
    output$plt_rfm <- renderPlotly({
      req(input$IND_RFM_base)
      r    <- rfm_snap(); base <- input$IND_RFM_base
      r_sc <- as.numeric(r[[paste0("recency_score",   base)]] %||% 0)
      f_sc <- as.numeric(r[[paste0("frequency_score", base)]] %||% 0)
      m_sc <- as.numeric(r[[paste0("monetary_score",  base)]] %||% 0)
      
      plot_ly(type = "scatterpolar", r = c(r_sc, f_sc, m_sc, r_sc),
              theta = c("Recencia", "Frecuencia", "Monetario", "Recencia"),
              fill = "toself",
              marker = list(color = "#1a5276", size = 8),
              line   = list(color = "#1a5276", width = 2),
              fillcolor = "rgba(26,82,118,0.20)",
              hovertemplate = "<b>%{theta}</b><br>Score: %{r} / 5<extra></extra>") %>%
        layout(polar = list(
          radialaxis  = list(visible = TRUE, range = c(0,5),
                             tickvals = 0:5, gridcolor = "#dee2e6"),
          angularaxis = list(gridcolor = "#dee2e6")),
          showlegend = FALSE,
          plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
    })
    
    output$tbl_segmento <- render_gt({
      req(input$IND_RFM_base)
      r <- rfm_snap(); base <- input$IND_RFM_base
      data.frame(
        Campo = c("Segmento Anal\u00edtico", "Score RFM", "Score Recencia",
                  "Score Frecuencia", "Score Monetario",
                  "N\u00ba Transacciones", "Tipo Cliente Racaf\u00e9"),
        Valor = c(
          as.character(r[[paste0("SegmentoAnalitica",  base)]] %||% "N/D"),
          as.character(r[[paste0("rfm_score",          base)]] %||% "N/D"),
          as.character(r[[paste0("recency_score",      base)]] %||% "N/D"),
          as.character(r[[paste0("frequency_score",    base)]] %||% "N/D"),
          as.character(r[[paste0("monetary_score",     base)]] %||% "N/D"),
          as.character(r[[paste0("transaction_count",  base)]] %||% "N/D"),
          as.character(r$SegmentoRacafe %||% "N/D")
        ),
        stringsAsFactors = FALSE
      ) %>%
        gt() %>% gt_minimal_style() %>%
        cols_label(Campo = "", Valor = "") %>%
        cols_width(Campo ~ px(200)) %>%
        tab_style(style = cell_text(weight = "bold", color = "#2c3e50"),
                  locations = cells_body(columns = Campo)) %>%
        tab_options(table.width = pct(100))
    })
  })
}


# ==============================================================================
# Submódulo 6: IndFormulario
# Schema CRMNALCLIENTE (11 cols): FecProceso, Usr, LinNegCod, CLCliNit,
# CliNitPpal, Segmento, SSPpto, MNFCCPpto, Asesor, NumMesesRecuperar, Excluir
# CliNitPpal es editable con validación numérica. CLCliNit se preserva.
# SSPpto y MNFCCPpto se preservan del último registro.
# ==============================================================================

IndFormularioUI <- function(id) {
  ns <- NS(id)
  box(title = "Datos Comerciales del Cliente", width = 12,
      status = "white", collapsible = TRUE, collapsed = FALSE,
      fluidRow(
        column(12,
               tags$div(h6("NIT Principal"),
                        textInput(ns("frm_CliNitPpal"), label = NULL,
                                  placeholder = "Sin d\u00edgito de verificaci\u00f3n",
                                  width = "100%"))
        ),
        column(12, ind_picker("frm_Asesor",   "Asesor",   con_vacio(.IND_CHO$personas), ns = ns)),
        column(12, ind_picker("frm_Segmento", "Segmento", con_vacio(.IND_CHO$segmento), ns = ns)),
        column(12, ind_picker("frm_Excluir",  "Excluir",  c("NO", "SI"),                ns = ns))
      ),
      fluidRow(
        column(12,
               autonumericInput(ns("frm_NumMesesRecuperar"), h6("Meses para Recuperar"),
                                value = 3L, width = "100%", decimalPlaces = 0, minimumValue = 1))
      ),
      tags$hr(),
      div(style = "text-align: right;",
          actionButton(ns("btn_guardar"), "Guardar cambios",
                       icon = icon("save"), class = "btn-danger")))
}
IndFormulario <- function(id, identidad, dat, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    refresh <- reactiveVal(0L)
    
    data_crm <- reactive({
      id_val <- identidad()
      req(!is.null(id_val), length(id_val$nit) == 1L, !is.na(id_val$nit))
      refresh()
      CargarDatos("CRMNALCLIENTE") %>%
        mutate(FecProceso = as.Date(FecProceso)) %>%
        filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod) %>%
        arrange(desc(FecProceso)) %>%
        slice(1)
    })
    
    # Poblar: prioridad registro CRM → BaseDatos → default
    observe({
      crm <- data_crm(); d <- dat()
      nit_val   <- if (nrow(crm) > 0) crm$CliNitPpal[1]       else d$CliNitPpal[1]
      asesor_val <- if (nrow(crm) > 0) crm$Asesor[1]    %||% "" else d$Asesor[1]    %||% ""
      seg_val    <- if (nrow(crm) > 0) crm$Segmento[1]  %||% "" else d$Segmento[1]  %||% ""
      exc_val    <- if (nrow(crm) > 0) crm$Excluir[1]   %||% "NO" else "NO"
      meses_val  <- if (nrow(crm) > 0) crm$NumMesesRecuperar[1] %||% 3L else 3L
      
      updateTextInput(session,        "frm_CliNitPpal",        value    = as.character(nit_val %||% ""))
      updatePickerInput(session,      "frm_Asesor",            selected = asesor_val)
      updatePickerInput(session,      "frm_Segmento",          selected = seg_val)
      updatePickerInput(session,      "frm_Excluir",           selected = exc_val)
      updateAutonumericInput(session, "frm_NumMesesRecuperar",  value   = meses_val)
    })
    
    # Payload exacto a las 11 columnas del schema
    construir_payload <- function() {
      id_val   <- identidad()
      crm      <- data_crm()
      ssppto   <- if (nrow(crm) > 0) as.numeric(crm$SSPpto[1]    %||% 0) else 0
      mnfccpto <- if (nrow(crm) > 0) as.numeric(crm$MNFCCPpto[1] %||% 0) else 0
      
      nit_input <- trimws(input$frm_CliNitPpal %||% "")
      nit_num   <- suppressWarnings(as.numeric(nit_input))
      nit_final <- if (!is.na(nit_num) && nit_num > 0) nit_num else id_val$nit
      
      data.frame(
        FecProceso        = as.character(Sys.Date()),
        Usr               = usr(),
        LinNegCod         = id_val$linneg_cod,
        CLCliNit          = id_val$nit,    # NIT pedido — no editable
        CliNitPpal        = nit_final,
        Segmento          = input$frm_Segmento          %||% NA_character_,
        SSPpto            = ssppto,
        MNFCCPpto         = mnfccpto,
        Asesor            = input$frm_Asesor             %||% NA_character_,
        NumMesesRecuperar = input$frm_NumMesesRecuperar  %||% 3L,
        Excluir           = input$frm_Excluir            %||% "NO",
        stringsAsFactors  = FALSE
      )
    }
    
    # Validación de NIT antes de pedir confirmación
    observeEvent(input$btn_guardar, {
      nit_input <- trimws(input$frm_CliNitPpal %||% "")
      if (nzchar(nit_input)) {
        nit_num <- suppressWarnings(as.numeric(nit_input))
        if (is.na(nit_num) || nit_num == 0) {
          showNotification("El NIT Principal debe ser un entero distinto de cero.", type = "error")
          return()
        }
      }
      confirmSweetAlert(session = session, inputId = ns("confirm_guardar"),
                        title = "Confirmar guardado",
                        text  = "\u00bfDesea guardar los datos comerciales del cliente?",
                        type  = "warning", btn_labels = c("Cancelar", "Guardar"),
                        btn_colors = c("#E7180B", "#1F7A55"), html = TRUE, width = "400px")
    })
    
    observeEvent(input$confirm_guardar, {
      req(isTRUE(input$confirm_guardar))
      tryCatch({
        racafe::AgregarDatos(construir_payload(), "CRMNALCLIENTE")
        showNotification("Datos guardados exitosamente.", type = "message")
        refresh(refresh() + 1L)
      }, error = function(e) {
        showNotification(paste("Error al guardar:", e$message), type = "error", duration = NULL)
      })
    })
    
    return(list(refresh = refresh))
  })
}


# ==============================================================================
# Submódulo 7: IndNotas — historial de notas CRM del cliente
# Tabla: CRMNALNOTAS (FechaHoraCrea, Usuario, LinNegCod, CliNitPpal, Nota)
# ==============================================================================

IndNotasUI <- function(id) {
  ns <- NS(id)
  box(title = tagList(icon("sticky-note"), " Notas CRM"), width = 12,
      status = "white", collapsible = TRUE, collapsed = TRUE,
      fluidRow(
        column(10,
               textAreaInput(ns("nota_texto"), label = NULL, width = "100%", rows = 2,
                             placeholder = "Ingrese nota de seguimiento...")),
        column(2,
               div(style = "padding-top: 0px;",
                   actionButton(ns("btn_nota"), "Agregar", icon = icon("plus"),
                                class = "btn-danger", width = "100%")))
      ),
      br(),
      reactableOutput(ns("tbl_notas")))
}
IndNotas <- function(id, identidad, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_notas <- reactiveVal(0L)
    
    data_notas <- reactive({
      id_val <- identidad(); refresh_notas()
      tryCatch({
        CargarDatos("CRMNALNOTAS") %>%
          mutate(FechaHoraCrea = as.POSIXct(FechaHoraCrea)) %>%
          filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod) %>%
          arrange(desc(FechaHoraCrea))
      }, error = function(e) {
        tibble::tibble(FechaHoraCrea = as.POSIXct(character()),
                       Usuario = character(), Nota = character())
      })
    })
    
    observeEvent(input$btn_nota, {
      req(nzchar(trimws(input$nota_texto %||% "")))
      id_val <- identidad()
      nuevo  <- data.frame(FechaHoraCrea = as.character(Sys.time()),
                           Usuario = usr(), LinNegCod = id_val$linneg_cod,
                           CliNitPpal = id_val$nit,
                           Nota = trimws(input$nota_texto), stringsAsFactors = FALSE)
      tryCatch({
        racafe::AgregarDatos(nuevo, "CRMNALNOTAS")
        updateTextAreaInput(session, "nota_texto", value = "")
        refresh_notas(refresh_notas() + 1L)
        showNotification("Nota registrada.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    output$tbl_notas <- renderReactable({
      df <- data_notas()
      if (nrow(df) == 0) return(reactable::reactable(
        data.frame(Mensaje = "Sin notas registradas."), compact = TRUE))
      reactable::reactable(df, sortable = TRUE, searchable = TRUE, compact = TRUE,
                           bordered = TRUE, highlight = TRUE, defaultPageSize = 10,
                           columns = list(
                             FechaHoraCrea = reactable::colDef(name = "Fecha/Hora", minWidth = 140,
                                                               cell = function(v) format(as.POSIXct(v), "%d/%m/%Y %H:%M")),
                             Usuario    = reactable::colDef(name = "Usuario",   minWidth = 100),
                             LinNegCod  = reactable::colDef(show = FALSE),
                             CliNitPpal = reactable::colDef(show = FALSE),
                             Nota       = reactable::colDef(name = "Nota",      minWidth = 300)
                           ))
    })
  })
}


# ==============================================================================
# Submódulo 8: IndOportunidades — pipeline de oportunidades y leads
# Tabla: CRMNALCLOPT
# ==============================================================================

IndOportunidadesUI <- function(id) {
  ns <- NS(id)
  box(title = tagList(icon("handshake"), " Oportunidades"), width = 12,
      status = "white", collapsible = TRUE, collapsed = TRUE,
      reactableOutput(ns("tbl_oport")))
}
IndOportunidades <- function(id, identidad) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$tbl_oport <- renderReactable({
      id_val <- identidad()
      df <- tryCatch({
        CargarDatos("CRMNALCLOPT") %>%
          mutate(FecProceso = as.Date(FecProceso)) %>%
          filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod) %>%
          arrange(desc(FecProceso))
      }, error = function(e) {
        data.frame(Mensaje = "Sin oportunidades registradas o tabla no disponible.")
      })
      reactable::reactable(df, sortable = TRUE, compact = TRUE,
                           highlight = TRUE, bordered = TRUE, defaultPageSize = 10)
    })
  })
}


# ==============================================================================
# Submódulo 9: IndBenchmark — comparativo vs pares del mismo segmento
# Requiere dat_global (BaseDatos completo, no filtrado por cliente).
# Muestra percentil del cliente en sacos y margen vs su segmento.
# ==============================================================================

IndBenchmarkUI <- function(id) {
  ns <- NS(id)
  box(title = tagList(icon("chart-bar"), " Benchmark vs Segmento"), width = 12,
      status = "white", collapsible = TRUE, collapsed = TRUE,
      gt_output(ns("tbl_bench")))
}
IndBenchmark <- function(id, dat, dat_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$tbl_bench <- render_gt({
      df_cli   <- dat(); df_all <- dat_global()
      if (is.null(df_all) || nrow(df_all) == 0) {
        return(gt(data.frame(Mensaje = "dat_global no disponible.")))
      }
      segmento <- df_cli$Segmento[1] %||% "SIN DATO"
      linneg   <- df_cli$LinNegCod[1]
      cli_nit  <- df_cli$CliNitPpal[1]
      
      peers <- df_all %>%
        filter(Segmento == segmento, LinNegCod == linneg, CliNitPpal != cli_nit) %>%
        group_by(CliNitPpal) %>%
        summarise(Sacos  = sum(SacFact70, na.rm=TRUE),
                  Margen = sum(Margen,    na.rm=TRUE), .groups = "drop")
      
      cli_sacos  <- sum(df_cli$SacFact70, na.rm=TRUE)
      cli_margen <- sum(df_cli$Margen,    na.rm=TRUE)
      n_pares    <- nrow(peers)
      
      pct_s <- if (n_pares > 0) mean(cli_sacos  > peers$Sacos,  na.rm=TRUE) * 100 else NA
      pct_m <- if (n_pares > 0) mean(cli_margen > peers$Margen, na.rm=TRUE) * 100 else NA
      
      data.frame(
        Indicador = c("Sacos Hist\u00f3rico (70Kg)", "Margen Hist\u00f3rico"),
        Cliente   = c(fmt_co(cli_sacos),
                      paste0("$", fmt_co(cli_margen/1e6, 1), " MM")),
        Mediana   = c(
          if (n_pares > 0) fmt_co(median(peers$Sacos,  na.rm=TRUE)) else "N/D",
          if (n_pares > 0) paste0("$", fmt_co(median(peers$Margen, na.rm=TRUE)/1e6, 1), " MM")
          else "N/D"),
        Percentil = c(
          if (!is.na(pct_s)) paste0(fmt_co(pct_s, 1), "%") else "N/D",
          if (!is.na(pct_m)) paste0(fmt_co(pct_m, 1), "%") else "N/D"),
        N_Pares   = c(n_pares, n_pares),
        check.names = FALSE
      ) %>%
        gt() %>% gt_minimal_style() %>%
        cols_label(N_Pares = "N Pares") %>%
        tab_header(title = paste0("Benchmark \u2014 ", segmento),
                   subtitle = paste0("L\u00ednea: ", df_cli$CLLinNegNo[1])) %>%
        tab_source_note("Percentil: % de pares con menor valor que este cliente") %>%
        tab_options(table.width = pct(100))
    })
  })
}


# ==============================================================================
# Submódulo 10: IndEstacionalidad — heatmap mes × año de compras
# ==============================================================================

IndEstacionalidadUI <- function(id) {
  ns <- NS(id)
  box(title = tagList(icon("calendar-alt"), " Estacionalidad de Compras"),
      width = 12, status = "white", collapsible = TRUE, collapsed = TRUE,
      fluidRow(
        column(4,
               BotonesRadiales("IND_EST_var", NULL, c("Sacos", "Margen"), "Sacos", ns = ns))
      ),
      plotlyOutput(ns("plt_estacion"), height = "320px"))
}
IndEstacionalidad <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$plt_estacion <- renderPlotly({
      req(input$IND_EST_var)
      var   <- input$IND_EST_var
      meses <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
      
      df <- dat() %>%
        filter(!is.na(FecFact)) %>%
        mutate(Anho = year(FecFact), Mes = month(FecFact)) %>%
        group_by(Anho, Mes) %>%
        summarise(Sacos = sum(SacFact70, na.rm=TRUE), Margen = sum(Margen, na.rm=TRUE),
                  .groups = "drop") %>%
        tidyr::complete(Anho, Mes = 1:12,
                        fill = list(Sacos = NA_real_, Margen = NA_real_)) %>%
        mutate(MesNom = factor(meses[Mes], levels = meses))
      
      plot_ly(df, x = ~MesNom, y = ~as.character(Anho), z = ~.data[[var]],
              type = "heatmap",
              colorscale = list(list(0, "#f7fbff"), list(0.35, "#4292c6"),
                                list(1, "#08306b")),
              hovertemplate = paste0(
                "<b>%{y} \u2014 %{x}</b><br>",
                var, ": %{z:,.0f}<extra></extra>"),
              zmin = 0) %>%
        layout(xaxis = list(title = ""),
               yaxis = list(title = "", autorange = "reversed"),
               plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
    })
  })
}


# ==============================================================================
# Módulo principal: Individual
# Orquesta 10 submódulos + [Presupuesto] externo.
# Parámetros:
#   dat          → reactive df filtrado por cliente y línea
#   usr          → reactive character con usuario activo
#   clientes_raw → reactive df CRMNALCLIENTE enriquecido (para Presupuesto)
#   dat_global   → reactive df BaseDatos completo (para Benchmark)
# ==============================================================================

IndividualUI <- function(id) {
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(), uiOutput(ns("wrapper")))
}
Individual <- function(id, dat, usr,
                       clientes_raw = reactive(NULL),
                       dat_global   = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    identidad <- reactive({
      req(nrow(dat()) > 0)
      list(nit        = dat()$CliNitPpal[1],
           linneg_cod = dat()$LinNegCod[1],
           razon_soc  = Unicos(dat()$PerRazSoc),
           linneg_nom = Unicos(dat()$CLLinNegNo))
    })
    
    # Reactivos compartidos para KPIs ─────────────────────────────────────────
    
    kpi_hist_r <- reactive({
      df <- dat()
      list(sacos   = sum(df$SacFact70, na.rm = TRUE),
           margen  = sum(df$Margen,    na.rm = TRUE),
           kilos   = sum(df$Kilos,     na.rm = TRUE),
           d_min   = min(df$FecFact,   na.rm = TRUE),
           d_max   = max(df$FecFact,   na.rm = TRUE),
           n_lotes = n_distinct(df$CLLotCod))
    })
    
    kpi_ytd_r <- reactive({
      ano <- year(Sys.Date())
      df  <- dat() %>% filter(!is.na(FecFact), year(FecFact) == ano)
      list(sacos = sum(df$SacFact70, na.rm=TRUE), margen = sum(df$Margen, na.rm=TRUE))
    })
    
    kpi_mtd_r <- reactive({
      ano <- year(Sys.Date()); mes <- month(Sys.Date())
      df  <- dat() %>%
        filter(!is.na(FecFact), year(FecFact) == ano, month(FecFact) == mes)
      list(sacos = sum(df$SacFact70, na.rm=TRUE), margen = sum(df$Margen, na.rm=TRUE))
    })
    
    # F1: KPIs históricos globales (CajaModal × 4) ───────────────────────────
    
    racafeModulos::CajaModal(
      "kpi_sac_hist",
      valor = reactive(kpi_hist_r()$sacos),
      formato = "coma", texto = "Sacos Facturados (Hist\u00f3rico)",
      icono = "box-open", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive(paste0(
        "Desde ", format(kpi_hist_r()$d_min, "%Y"),
        " \u00b7 ", kpi_hist_r()$n_lotes, " lotes"))
    )
    racafeModulos::CajaModal(
      "kpi_mar_hist",
      valor = reactive(kpi_hist_r()$margen),
      formato = "dinero", texto = "Margen Hist\u00f3rico",
      icono = "dollar-sign", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive(paste0(
        "Mar/Kg: $", fmt_co(kpi_hist_r()$margen / pmax(kpi_hist_r()$kilos, 1))))
    )
    racafeModulos::CajaModal(
      "kpi_anos_cli",
      valor = reactive({
        anos <- as.numeric(difftime(kpi_hist_r()$d_max,
                                    kpi_hist_r()$d_min, units = "days")) / 365.25
        round(max(anos, 0), 1)
      }),
      formato = "numero", texto = "A\u00f1os como Cliente",
      icono = "calendar", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive(paste0(format(kpi_hist_r()$d_min, "%b %Y"),
                               " \u2014 ", format(kpi_hist_r()$d_max, "%b %Y")))
    )
    racafeModulos::CajaModal(
      "kpi_mar_kilo",
      valor = reactive(kpi_hist_r()$margen / pmax(kpi_hist_r()$kilos, 1)),
      formato = "dinero", texto = "Margen / Kilo Promedio",
      icono = "weight", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive(paste0(fmt_co(kpi_hist_r()$kilos), " kg facturados"))
    )
    
    # F2: KPIs YTD y MTD (CajaModal × 4) ────────────────────────────────────
    
    racafeModulos::CajaModal(
      "kpi_sac_ytd",
      valor = reactive(kpi_ytd_r()$sacos),
      formato = "coma", texto = reactive(paste0("Sacos YTD ", year(Sys.Date()))),
      icono = "chart-line", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive(paste0("Acum. a ", format(Sys.Date(), "%B %Y")))
    )
    racafeModulos::CajaModal(
      "kpi_mar_ytd",
      valor = reactive(kpi_ytd_r()$margen),
      formato = "dinero", texto = reactive(paste0("Margen YTD ", year(Sys.Date()))),
      icono = "coins", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive(paste0("Acum. a ", format(Sys.Date(), "%B %Y")))
    )
    racafeModulos::CajaModal(
      "kpi_sac_mtd",
      valor = reactive(kpi_mtd_r()$sacos),
      formato = "coma",
      texto = reactive(paste0("Sacos MTD \u2014 ", format(Sys.Date(), "%B"))),
      icono = "boxes", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer = reactive(format(Sys.Date(), "Mes %m de %Y"))
    )
    racafeModulos::CajaModal(
      "kpi_mar_mtd",
      valor = reactive(kpi_mtd_r()$margen),
      formato = "dinero",
      texto = reactive(paste0("Margen MTD \u2014 ", format(Sys.Date(), "%B"))),
      icono = "hand-holding-dollar", colores = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer = reactive(format(Sys.Date(), "Mes %m de %Y"))
    )
    
    # Wrapper UI ──────────────────────────────────────────────────────────────
    
    output$wrapper <- renderUI({
      if (nrow(dat()) == 0) {
        return(div(class = "alert alert-warning", style = "margin:20px;",
                   icon("exclamation-triangle"), " ",
                   "No existe informaci\u00f3n para el cliente seleccionado."))
      }
      id_val <- identidad()
      
      tagList(
        Saltos(),
        fluidRow(column(12,
                        h4(icon("user-circle"), sprintf(" %s", id_val$razon_soc),
                           tags$small(class = "text-muted", style = "font-size:.75em;margin-left:10px;",
                                      paste0("L\u00ednea: ", id_val$linneg_nom,
                                             " \u00b7 NIT: ", fmt_co(id_val$nit))))
        )),
        Saltos(1),
        
        # Fila 1: KPIs históricos globales
        fluidRow(
          column(3, racafeModulos::CajaModalUI(ns("kpi_sac_hist"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_mar_hist"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_anos_cli"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_mar_kilo")))
        ),
        Saltos(1),
        
        # Fila 2: YTD + MTD
        fluidRow(
          column(3, racafeModulos::CajaModalUI(ns("kpi_sac_ytd"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_mar_ytd"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_sac_mtd"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_mar_mtd")))
        ),
        Saltos(1),
        
        # Fila 3: col-4 [Contacto + Formulario] · col-8 [Pendientes]
        fluidRow(
          column(4,
                 IndContactoUI(ns("contacto")),
                 Saltos(1),
                 IndFormularioUI(ns("formulario"))
          ),
          column(8, IndPendientesUI(ns("pendientes")))
        ),
        Saltos(1),
        
        # Fila 4: col-8 [Histórico] · col-4 [Treemap jerárquico]
        fluidRow(
          column(7, IndHistoricoUI(ns("historico"))),
          column(5, IndParticipacionUI(ns("participacion")))
        ),
        Saltos(1),
        
        # Fila 5: Presupuesto vs ejecución (full, colapsado)
        box(title = tagList(icon("bullseye"), " Presupuesto vs Ejecuci\u00f3n"),
            width = 12, status = "white", collapsible = TRUE, collapsed = TRUE,
            PresupuestoUI(ns("presupuesto"))),
        Saltos(1),
        
        # Fila 6: RFM & CLV (full, colapsado)
        IndRFMUI(ns("rfm")),
        Saltos(1),
        
        # Módulos adicionales — todos colapsados por defecto
        IndNotasUI(ns("notas")),
        Saltos(1),
        IndOportunidadesUI(ns("oportunidades")),
        Saltos(1),
        IndBenchmarkUI(ns("benchmark")),
        Saltos(1),
        IndEstacionalidadUI(ns("estacionalidad"))
      )
    })
    
    # Instanciación de submódulos ─────────────────────────────────────────────
    
    IndContacto(      "contacto",       dat)
    IndHistorico(     "historico",      dat)
    IndParticipacion( "participacion",  dat)
    IndPendientes(    "pendientes",     dat)
    IndRFM(           "rfm",            dat)
    IndFormulario(    "formulario",     identidad, dat, usr)
    IndNotas(         "notas",          identidad, usr)
    IndOportunidades( "oportunidades",  identidad)
    IndBenchmark(     "benchmark",      dat, dat_global)
    IndEstacionalidad("estacionalidad", dat)
    # Presupuesto("presupuesto", dat, clientes_raw) 
  })
}


# ==============================================================================
# App de prueba — remover en producción
# ==============================================================================

if (TRUE) {
  ui <- bs4DashPage(
    title   = "Prueba \u2014 M\u00f3dulo Individual",
    header  = bs4DashNavbar(),
    sidebar = bs4DashSidebar(disable = TRUE),
    body    = bs4DashBody(useShinyjs(), IndividualUI("ind"))
  )
  
  server <- function(input, output, session) {
    clientes_raw_r <- reactive({
      local_geo <- CargarDatos("CRMNALLOCAL") %>%
        mutate(FecProceso = as.Date(FecProceso)) %>%
        group_by(CliNitPpal) %>%
        filter(FecProceso == max(FecProceso)) %>%
        ungroup()
      CargarDatos("CRMNALCLIENTE") %>%
        mutate(
          FecProceso = as.Date(FecProceso),
          across(where(is.numeric),   ~ ifelse(is.na(.), 0, .)),
          across(where(is.character), ~ ifelse(is.na(.) | . == "N/A", "", .))
        ) %>%
        left_join(local_geo, by = join_by(FecProceso, Usr, CliNitPpal)) %>%
        mutate(LinNegocio = ifelse(LinNegCod == 10000, "CONVENCIONALES", "A LA MEDIDA"))
    })
    
    Individual(
      "ind",
      dat          = reactive({ BaseDatos_i }),
      usr          = reactive("HCYATE"),
      clientes_raw = clientes_raw_r,
      dat_global   = reactive({ BaseDatos_c })
    )
  }
  
  shinyApp(ui, server)
}
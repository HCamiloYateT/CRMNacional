# Global ----------------------------------------------------------------

.IND_CHO <- Choices()
.IND_NEG_CONTACTADO <- c("", grep("OPORTUNIDAD|DESCARTADO", .IND_CHO$estadonegocio, value = TRUE))
.IND_NEG_MUERTO <- c("", grep("OPORTUNIDAD|DESCARTADO", .IND_CHO$estadonegocio,
                              value = TRUE, invert = TRUE))

con_vacio <- function(x) unique(c("", x))

ind_picker <- function(inputId, label, choices, selected = NULL, ns) {
  pickerInput(ns(inputId), label = h6(label), width = "100%", choices = choices,
              multiple = FALSE, options = pick_opt(choices), selected = selected %||% choices[1])
}

fmt_co <- function(x, dec = 0) {
  formatC(as.numeric(x), digits = dec, format = "f", big.mark = ".", decimal.mark = ",")
}

# Paleta Racafe compartida por todo el módulo Individual ------------------
# .RACAFE_ROOT: :root del proyecto. .RACAFE_WEB: paleta oficial racafe.com.co.

.RACAFE_ROOT <- list(
  red_primary = "#d90429", red_secondary = "#dc3545", red_dark = "#b3001b",
  red_muted = "#c0392b", red_hover = "#b02a37",
  gray_100 = "#f5f5f5", gray_200 = "#f0f0f0", gray_300 = "#e8e8e8",
  gray_400 = "#d6d6d6", gray_500 = "#c8c8c8", gray_600 = "#a1a1a1",
  gray_700 = "#999999", gray_800 = "#666666", gray_850 = "#6c757d",
  gray_900 = "#555555", gray_950 = "#1a1a1a"
)

.RACAFE_WEB <- list(
  cafe_corporativo = "#7A5C45", dorado_suave = "#C9A66B", azul_tecnico = "#0073A8",
  blanco = "#FFFFFF", gris_calido_claro = "#F5F5F4", texto_principal = "#292524",
  texto_secundario = "#57534D", borde_beige = "#D6D3D1"
)

racafe_escala_secuencial <- function() {
  list(list(0, .RACAFE_WEB$gris_calido_claro), list(0.5, .RACAFE_WEB$cafe_corporativo),
       list(1, .RACAFE_ROOT$red_dark))
}

# Paleta categórica AMPLIADA para el treemap: cada entrada es un color base
# distinto para cada categoría. Se agregaron más tonos (rojos, café, dorado,
# azul técnico, grises) para dar mayor variedad antes de recurrir a
# interpolación cuando hay muchas categorías.
racafe_paleta_categorica <- function(n) {
  base <- c(
    .RACAFE_WEB$cafe_corporativo, .RACAFE_ROOT$red_primary, .RACAFE_WEB$dorado_suave,
    .RACAFE_WEB$azul_tecnico, .RACAFE_ROOT$red_dark, .RACAFE_ROOT$gray_850,
    .RACAFE_ROOT$red_muted, .RACAFE_ROOT$gray_700, .RACAFE_WEB$borde_beige,
    .RACAFE_ROOT$red_secondary, .RACAFE_ROOT$gray_600, .RACAFE_ROOT$red_hover,
    .RACAFE_ROOT$gray_900, .RACAFE_ROOT$gray_500
  )
  if (n <= length(base)) return(base[seq_len(n)])
  colorRampPalette(base)(n)
}

sin_modebar <- function(p) config(p, displayModeBar = FALSE)

# Conversión hex -> rgba con alpha (jerarquía visual en treemaps)
hex_a_rgba <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], alpha)
}

# Genera n variaciones de transparencia de un mismo color base, para que
# cada producto DENTRO de una categoría tenga un tono distinguible del
# mismo color de familia (no todos con el mismo alpha plano).
# El primer producto es el más opaco; decrece hacia productos siguientes.
variaciones_alpha <- function(hex, n, alpha_max = 0.85, alpha_min = 0.30) {
  if (n <= 1) return(hex_a_rgba(hex, alpha_max))
  alphas <- seq(alpha_max, alpha_min, length.out = n)
  vapply(alphas, function(a) hex_a_rgba(hex, round(a, 2)), character(1))
}

# Mezcla un color base con blanco en distintas proporciones (tinte real de
# RGB, NO transparencia) — más robusto que alpha porque el resultado no
# depende del color de fondo detrás del plot. El primer producto es el más
# saturado (cerca del color base); los siguientes se aclaran hacia blanco.
variaciones_tinte <- function(hex, n, tinte_max = 0.05, tinte_min = 0.65) {
  rgb_base <- grDevices::col2rgb(hex) / 255
  if (n <= 1) return(hex)
  tintes <- seq(tinte_max, tinte_min, length.out = n)
  vapply(tintes, function(t) {
    rgb_mix <- rgb_base * (1 - t) + 1 * t
    grDevices::rgb(rgb_mix[1], rgb_mix[2], rgb_mix[3])
  }, character(1))
}

# IndContacto -------------------------------------------------------------
# Tabla transpuesta: columnas = cuentas (hijas + principal), filas = campo

IndContactoUI <- function(id) {
  ns <- NS(id)
  box(title = "Información de Contacto", width = 12, status = "white",
      collapsible = TRUE, collapsed = FALSE, gt_output(ns("tbl_contacto_hijos")))
}
IndContacto <- function(id, dat, ncliente) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$tbl_contacto_hijos <- render_gt({
      principal_nit <- dat()$CliNitPpal[1]
      hijos_nit <- dat() %>% distinct(CLCliNit) %>% filter(!is.na(CLCliNit)) %>% pull(CLCliNit)
      
      base <- ncliente() %>%
        filter(PerCod %in% hijos_nit) %>%
        transmute(PerCod, PerRazSoc, Contacto = CliCont, `Dirección` = CliDir,
                  Ciudad = CiuExtNom, `Teléfono` = CliTel, `Contacto Comercial` = CliConCom,
                  `Tel. Comercial` = CliTelCom, `Email Comercial` = CliEmlCom) %>%
        arrange(desc(PerCod == principal_nit), PerRazSoc) %>%
        mutate(col_id = paste0(PerRazSoc, " (NIT ", fmt_co(PerCod), ")"))
      
      col_principal <- base %>% filter(PerCod == principal_nit) %>% pull(col_id)
      n_hijas <- dplyr::n_distinct(base$col_id) - length(col_principal)
      
      base %>%
        select(-PerCod, -PerRazSoc) %>%
        pivot_longer(-col_id, names_to = "Campo", values_to = "Valor") %>%
        pivot_wider(names_from = col_id, values_from = Valor) %>%
        gt(rowname_col = "Campo") %>%
        gt_minimal_style() %>%
        sub_missing(columns = everything(), missing_text = "\u2014") %>%
        tab_style(
          style = list(cell_fill(color = .RACAFE_ROOT$gray_200),
                       cell_text(weight = "bold", color = .RACAFE_ROOT$red_primary)),
          locations = list(cells_body(columns = all_of(col_principal)),
                           cells_column_labels(columns = all_of(col_principal)))
        ) %>%
        tab_style(style = cell_text(weight = "bold", color = .RACAFE_WEB$texto_principal),
                  locations = cells_stub()) %>%
        tab_source_note(md("**Nota:** El NIT principal se encuentra resaltado en rojo.")) %>%
        tab_source_note(md(paste0("**Cuentas hijas asociadas:** ", n_hijas))) %>%
        tab_options(table.width = pct(100))
    })
  })
}

# IndHistorico --------------------------------------------------------------
# Serie mensual/acumulada y comparativo interanual (semana ISO o mes).

IndHistoricoUI <- function(id) {
  ns <- NS(id)
  box(title = "Histórico de Facturación", width = 12, status = "white",
      collapsible = TRUE, collapsed = FALSE,
      fluidRow(
        column(4, racafe::ListaDesplegable(ns("IND_VarHist"), label = h6("Variable"),
                                           choices = c("Sacos", "Margen"), selected = "Sacos",
                                           multiple = FALSE)),
        column(4, racafe::ListaDesplegable(ns("IND_TipoHist"), label = h6("Vista"),
                                           choices = c("Mensual" = "mensual",
                                                       "Acumulado" = "acumulado",
                                                       "Comparativo Interanual" = "comparativo"),
                                           selected = "mensual", multiple = FALSE)),
        column(4, uiOutput(ns("ui_comparativo_tipo")))
      ),
      plotlyOutput(ns("plt_hist"), height = "380px"))
}
IndHistorico <- function(id, dat, identidad = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
                  "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
    
    mes_actual <- reactive(PrimerDia(Sys.Date()))
    
    ts_base <- reactive({
      df <- dat() %>%
        filter(!is.na(FecFact)) %>%
        mutate(Mes = PrimerDia(FecFact)) %>%
        group_by(Mes) %>%
        summarise(Sacos = sum(SacFact70, na.rm = TRUE), Margen = sum(Margen, na.rm = TRUE),
                  .groups = "drop")
      req(nrow(df) > 0)
      
      fecha_min <- min(df$Mes)
      fecha_max <- min(max(df$Mes), mes_actual())
      todos_los_meses <- seq(fecha_min, fecha_max, by = "month")
      
      tibble::tibble(Mes = todos_los_meses) %>%
        left_join(df, by = "Mes") %>%
        mutate(Sacos = coalesce(Sacos, 0), Margen = coalesce(Margen, 0)) %>%
        arrange(Mes)
    })
    
    ts_comparativo <- reactive({
      req(input$IND_ComparativoTipo)
      df_raw <- dat() %>% filter(!is.na(FecFact)) %>% mutate(Anho = year(FecFact))
      req(nrow(df_raw) > 0)
      
      anho_actual <- year(Sys.Date())
      
      if (input$IND_ComparativoTipo == "semana") {
        df <- df_raw %>%
          mutate(Periodo = lubridate::isoweek(FecFact)) %>%
          group_by(Anho, Periodo) %>%
          summarise(Sacos = sum(SacFact70, na.rm = TRUE), Margen = sum(Margen, na.rm = TRUE),
                    .groups = "drop")
        periodo_max_actual <- lubridate::isoweek(Sys.Date())
        rango_completo <- 1:53
      } else {
        df <- df_raw %>%
          mutate(Periodo = month(FecFact)) %>%
          group_by(Anho, Periodo) %>%
          summarise(Sacos = sum(SacFact70, na.rm = TRUE), Margen = sum(Margen, na.rm = TRUE),
                    .groups = "drop")
        periodo_max_actual <- month(Sys.Date())
        rango_completo <- 1:12
      }
      
      anios <- unique(df$Anho)
      grid <- purrr::map_dfr(anios, function(a) {
        rango_a <- if (a < anho_actual) rango_completo
        else if (a == anho_actual) seq_len(periodo_max_actual)
        else integer(0)
        tibble::tibble(Anho = a, Periodo = rango_a)
      })
      
      grid %>%
        left_join(df, by = c("Anho", "Periodo")) %>%
        mutate(Sacos = coalesce(Sacos, 0), Margen = coalesce(Margen, 0)) %>%
        arrange(Anho, Periodo)
    })
    
    output$ui_comparativo_tipo <- renderUI({
      req(input$IND_TipoHist == "comparativo")
      racafe::ListaDesplegable(ns("IND_ComparativoTipo"), label = h6("Agrupar por"),
                               choices = c("Semana ISO" = "semana", "Mes" = "mes"),
                               selected = "mes", multiple = FALSE)
    })
    
    output$plt_hist <- renderPlotly({
      req(input$IND_VarHist, input$IND_TipoHist)
      var <- input$IND_VarHist
      es_margen <- var == "Margen"
      
      if (input$IND_TipoHist == "comparativo") {
        req(input$IND_ComparativoTipo)
        df <- ts_comparativo()
        n_anios <- dplyr::n_distinct(df$Anho)
        paleta <- racafe_paleta_categorica(n_anios)
        
        if (input$IND_ComparativoTipo == "mes") {
          df <- df %>% mutate(PeriodoNom = factor(meses_es[Periodo], levels = meses_es))
          eje_x_titulo <- "Mes"
          eje_x_var <- ~PeriodoNom
          customdata_vals <- as.character(df$PeriodoNom)
        } else {
          anho_base <- min(df$Anho, na.rm = TRUE)
          df <- df %>%
            mutate(FechaAprox = as.Date(paste0(anho_base, "-01-01")) +
                     lubridate::weeks(Periodo - 1))
          eje_x_titulo <- "Semana ISO"
          eje_x_var <- ~Periodo
          customdata_vals <- paste0("~ ", format(df$FechaAprox, "%d %b"))
        }
        
        txt <- if (es_margen) paste0("$", fmt_co(df[[var]] / 1e6, 1), " MM") else fmt_co(df[[var]])
        
        p <- plot_ly(df, x = eje_x_var, y = ~.data[[var]], color = ~as.character(Anho),
                     colors = paleta, customdata = customdata_vals,
                     type = "scatter", mode = "lines+markers", text = txt,
                     hovertemplate = paste0(
                       "<b>Año %{fullData.name}</b><br>",
                       eje_x_titulo, " %{x} (%{customdata})<br>",
                       var, ": %{text}<extra></extra>"
                     )) %>%
          layout(xaxis = list(title = eje_x_titulo),
                 yaxis = list(title = var, tickformat = if (es_margen) "$,.0f" else ",.0f"),
                 plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                 legend = list(title = list(text = "Año")))
        sin_modebar(p)
        
      } else {
        df <- ts_base() %>% mutate(MesNom = paste0(meses_es[month(Mes)], " ", year(Mes)))
        if (input$IND_TipoHist == "acumulado") {
          df <- df %>% mutate(across(c(Sacos, Margen), cumsum))
        }
        
        txt <- if (es_margen) paste0("$", fmt_co(df[[var]] / 1e6, 1), " MM") else fmt_co(df[[var]])
        hover_real <- paste0("<b>%{customdata[0]}</b><br>", var, ": %{text}<extra></extra>")
        
        p <- if (input$IND_TipoHist == "mensual") {
          plot_ly(df, x = ~Mes, y = ~.data[[var]], type = "bar",
                  marker = list(color = .RACAFE_WEB$cafe_corporativo),
                  text = txt, customdata = list(df$MesNom), textposition = "outside",
                  hovertemplate = hover_real, name = "Real")
        } else {
          plot_ly(df, x = ~Mes, y = ~.data[[var]], type = "scatter", mode = "lines+markers",
                  line = list(color = .RACAFE_ROOT$red_primary, width = 2),
                  marker = list(color = .RACAFE_ROOT$red_primary, size = 6), text = txt,
                  customdata = list(df$MesNom), hovertemplate = hover_real, name = "Real")
        }
        
        p <- p %>%
          layout(xaxis = list(title = "", tickformat = "%b %Y"),
                 yaxis = list(title = var, tickformat = if (es_margen) "$,.0f" else ",.0f"),
                 plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                 showlegend = FALSE)
        sin_modebar(p)
      }
    })
  })
}

# IndParticipacion ----------------------------------------------------------
# Treemap jerárquico Categoría > Producto
IndParticipacionUI <- function(id) {
  ns <- NS(id)
  box(title = "Distribución por Categoría y Producto", width = 12, status = "white",
      collapsible = TRUE, collapsed = FALSE,
      fluidRow(column(6, racafe::ListaDesplegable(ns("IND_VarPart"), label = h6("Variable"),
                                                  choices = c("Sacos", "Margen"),
                                                  selected = "Sacos", multiple = FALSE))),
      Saltos(),
      plotlyOutput(ns("plt_part"), height = "380px"))
}
IndParticipacion <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$plt_part <- renderPlotly({
      req(input$IND_VarPart)
      med_var <- input$IND_VarPart
      
      # Agregación por Categoría + Producto -------------------------------
      df_prod <- dat() %>%
        filter(!is.na(Categoria), !is.na(Producto)) %>%
        group_by(Categoria, Producto) %>%
        summarise(Sacos = sum(SacFact70, na.rm = TRUE), Margen = sum(Margen, na.rm = TRUE),
                  .groups = "drop") %>%
        filter(!!sym(med_var) > 0) %>%
        arrange(Categoria, desc(!!sym(med_var)))
      
      # Agregación por Categoría (nivel padre del treemap) ----------------
      df_cat <- df_prod %>%
        group_by(Categoria) %>%
        summarise(Sacos = sum(Sacos, na.rm = TRUE), Margen = sum(Margen, na.rm = TRUE),
                  .groups = "drop")
      
      prod_ids <- paste0(df_prod$Categoria, " | ", df_prod$Producto)
      n_cat <- nrow(df_cat)
      pal_cat <- racafe_paleta_categorica(n_cat)
      cat_map <- setNames(pal_cat, df_cat$Categoria)
      
      # Colores de producto: variaciones de tinte del color base de su
      # categoría (más saturado el de mayor valor, más claro el de menor)
      colores_prod <- df_prod %>%
        group_by(Categoria) %>%
        mutate(.color = variaciones_tinte(cat_map[[Categoria[1]]], n())) %>%
        ungroup() %>%
        pull(.color)
      
      colores_cat <- pal_cat
      
      # Texto de valor formateado según la medida: Sacos -> "N sacos",
      # Margen -> "$ N COP". Se pre-formatea en R porque el formato
      # depende de una variable reactiva y no puede resolverse con el
      # formato estático de %{value:...} de Plotly.
      fmt_valor <- function(x) {
        if (med_var == "Sacos") paste0(fmt_co(x), " sacos") else paste0("$ ", fmt_co(x), " COP")
      }
      
      valor_cat <- fmt_valor(df_cat[[med_var]])
      valor_prod <- fmt_valor(df_prod[[med_var]])
      
      labels_cat <- df_cat$Categoria
      labels_prod <- df_prod$Producto
      customdata_cat <- paste0(df_cat$Categoria, "|", valor_cat)
      customdata_prod <- paste0(df_prod$Categoria, "<br>Producto: ", df_prod$Producto,
                                "|", valor_prod)
      
      # Treemap jerárquico Categoría > Producto ---------------------------
      p <- plot_ly(
        type = "treemap", ids = c(df_cat$Categoria, prod_ids),
        labels = c(labels_cat, labels_prod),
        parents = c(rep("", n_cat), df_prod$Categoria),
        values = c(df_cat[[med_var]], df_prod[[med_var]]), branchvalues = "total",
        text = c(valor_cat, valor_prod),
        texttemplate = paste0("<b>%{label}</b><br>%{text}<br>",
                              "%{percentParent:.1%} de la categoría<br>",
                              "%{percentRoot:.1%} del total"),
        customdata = c(customdata_cat, customdata_prod),
        marker = list(colors = unname(c(colores_cat, colores_prod)),
                      pad = list(t = 25, l = 2, r = 2, b = 2),
                      line = list(width = 1, color = "rgba(255,255,255,0.6)")),
        hovertemplate = paste0("<b>%{customdata[0]}</b><br>%{customdata[1]}<br>",
                               "%% en categoría: %{percentParent:.1%}<br>",
                               "%% global: %{percentRoot:.1%}<extra></extra>")
      ) %>%
        layout(plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
               margin = list(t = 0, l = 0, r = 0, b = 0))
      sin_modebar(p)
    })
  })
}

# IndPendientes ---------------------------------------------------------
# Lotes pendientes de producción/despacho/facturación, con asignación de OC

IndPendientesUI <- function(id) {
  ns <- NS(id)
  box(title = tagList(icon("clock"), " Lotes Pendientes"), width = 12, status = "white",
      collapsible = TRUE, collapsed = FALSE,
      fluidRow(
        column(4, racafeModulos::CajaModalUI(ns("kpi_pend_prod"))),
        column(4, racafeModulos::CajaModalUI(ns("kpi_pend_desp"))),
        column(4, racafeModulos::CajaModalUI(ns("kpi_pend_fact")))
      ),
      fluidRow(
        column(4, createSwitch("PEN_PendProducir", "Pend. por Producir", ns = ns)),
        column(4, createSwitch("PEN_PendDespachar", "Pend. por Despachar", ns = ns)),
        column(4, createSwitch("PEN_DespPendFacturar", "Desp. Pend. Facturar", TRUE, ns = ns))
      ),
      br(), uiOutput(ns("bloque_pend")))
}
IndPendientes <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dd_oc_rv <- reactiveVal(NULL)
    AsignarOrdenCompra("mod_oc", dd_data = reactive(dd_oc_rv()))
    
    racafeModulos::CajaModal("kpi_pend_prod", valor = reactive(sum(dat()$PendProducir, na.rm = TRUE)),
                             formato = "coma", texto = "Sacos Pend. Producir", icono = "industry",
                             colores = c(fondo = "white"), mostrar_boton = FALSE,
                             footer = reactive(paste0(sum(dat()$PendProducir > 0.1, na.rm = TRUE), " lotes")))
    racafeModulos::CajaModal("kpi_pend_desp", valor = reactive(sum(dat()$PendDespachar, na.rm = TRUE)),
                             formato = "coma", texto = "Sacos Pend. Despachar", icono = "truck",
                             colores = c(fondo = "white"), mostrar_boton = FALSE,
                             footer = reactive(paste0(sum(dat()$PendDespachar > 0.1, na.rm = TRUE), " lotes")))
    racafeModulos::CajaModal("kpi_pend_fact", valor = reactive(sum(dat()$PendFacturar, na.rm = TRUE)),
                             formato = "coma", texto = "Sacos Pend. Facturar", icono = "money-bill",
                             colores = c(fondo = "white"), mostrar_boton = FALSE,
                             footer = reactive(paste0(sum(dat()$PendFacturar > 0.1, na.rm = TRUE), " lotes")))
    
    data_pend_r <- reactive({
      pend_prod <- isTRUE(input$PEN_PendProducir)
      pend_desp <- isTRUE(input$PEN_PendDespachar)
      pend_fact <- isTRUE(input$PEN_DespPendFacturar)
      df <- dat()
      if (any(c(pend_prod, pend_desp, pend_fact))) {
        df <- df %>%
          filter((pend_prod & PendProducir > 0.1) | (pend_desp & PendDespachar > 0.1) |
                   (pend_fact & PendFacturar > 0.1))
      }
      df %>%
        select(Sucursal, CLLotCod, PdcRefCli, SacLote, FecAsignLote, OrdenCompra, CLLinNegNo,
               Categoria, Producto, PendProducir, PendDespachar, PendFacturar) %>%
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
                       footer = "Clic en el botón para asignar una orden de compra.",
                       footer_tipo = "info")
    })
    
    TablaReactable(
      id = "tbl_pend", data = data_pend_r, modo_seleccion = "celda", id_col = NULL,
      col_header_n = 1L, cols_activos = "Asignar", sortable = FALSE, searchable = TRUE,
      page_size = 99999L, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE,
      modal_icon = "arrow-right", modal_size = "xl",
      modal_titulo_fn = function(sel) paste0("Asignar OC — Lote ", sel$fila$CLLotCod[[1]]),
      modal_pre_fn = function(sel) {
        if (isTRUE(sel$fila$Asignar == "")) return(invisible(NULL))
        dd_oc_rv(list(fila_completa = sel$fila))
      },
      modal_contenido_fn = function(sel) AsignarOrdenCompraUI(ns("mod_oc")),
      columnas = c("Asignar", "Sucursal", "CLLotCod", "PdcRefCli", "OrdenCompra",
                   "FecAsignLote", "CLLinNegNo", "Categoria", "Producto",
                   "PendProducir", "PendDespachar", "PendFacturar", "SacLote"),
      col_specs = list(
        Asignar = reactable::colDef(name = "", minWidth = 50, html = TRUE,
                                    cell = function(v) {
                                      if (v == "") return("")
                                      as.character(tags$span(
                                        style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                                                       "width:26px;height:26px;border-radius:6px;",
                                                       "background:", .RACAFE_ROOT$red_primary, ";",
                                                       "color:white;font-size:12px;cursor:pointer;"),
                                        icon("arrow-right")))
                                    }),
        Sucursal = reactable::colDef(name = "Sucursal", minWidth = 90),
        CLLotCod = reactable::colDef(name = "Lote", minWidth = 80),
        PdcRefCli = reactable::colDef(name = "Pedido", minWidth = 90),
        OrdenCompra = reactable::colDef(name = "Orden Compra", minWidth = 120),
        FecAsignLote = reactable::colDef(name = "Fecha Asig.", minWidth = 110),
        CLLinNegNo = reactable::colDef(name = "Línea Negocio", minWidth = 110),
        Categoria = reactable::colDef(name = "Categoría", minWidth = 100),
        Producto = reactable::colDef(name = "Producto", minWidth = 120),
        SacLote = reactable::colDef(name = "Sacos", minWidth = 80,
                                    cell = function(v) if (is.na(v)) "\u2014" else fmt_co(v)),
        PendProducir = reactable::colDef(name = "Pend. Producir", minWidth = 120,
                                         cell = function(v) if (is.na(v)) "\u2014" else fmt_co(v)),
        PendDespachar = reactable::colDef(name = "Pend. Despachar", minWidth = 130,
                                          cell = function(v) if (is.na(v)) "\u2014" else fmt_co(v)),
        PendFacturar = reactable::colDef(name = "Pend. Facturar", minWidth = 120,
                                         cell = function(v) if (is.na(v)) "\u2014" else fmt_co(v))
      )
    )
  })
}

# PresupuestoIndividual ----------------------------------------------------
PresupuestoIndividualUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, racafeModulos::CajaModalUI(ns("kpi_cumpl_sacos_ytd"))),
      column(3, racafeModulos::CajaModalUI(ns("kpi_cumpl_margen_ytd"))),
      column(3, racafeModulos::CajaModalUI(ns("kpi_ritmo_req_sacos"))),
      column(3, racafeModulos::CajaModalUI(ns("kpi_ritmo_req_margen")))
    ),
    fluidRow(
      bs4Dash::bs4Card(
        title = "Presupuesto", status = "white", solidHeader = TRUE,
        width = 12, collapsible = FALSE,
        div(style = "overflow-x: auto; width: 100%;", gt_output(ns("Presupuesto")))
      )
    ),
    fluidRow(
      column(4,
             div(style = "margin-left: 5px;",
                 shinyWidgets::materialSwitch(
                   inputId = ns("vista_acumulada"), label = "Vista Acumulada",
                   value = TRUE, status = "danger", inline = TRUE, width = "100%"
                 )))
    ),
    fluidRow(
      column(6,
             bs4Dash::bs4Card(
               title = "Presupuesto de Sacos (70 Kgs)", status = "white",
               solidHeader = TRUE, width = 12, collapsible = TRUE,
               plotlyOutput(ns("GraficoPresupuestoSacos"))
             )),
      column(6,
             bs4Dash::bs4Card(
               title = "Presupuesto de $MNFCC", status = "white",
               solidHeader = TRUE, width = 12, collapsible = TRUE,
               plotlyOutput(ns("GraficoPresupuestoMargen"))
             ))
    )
  )
}
PresupuestoIndividual <- function(id, dat, identidad) {
  moduleServer(id, function(input, output, session) {
    
    # Constantes ----
    
    .MESES <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
                "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
    
    .SEMAFORO <- list(
      umbral_exceso = 1.00, umbral_alto = 0.80, umbral_bajo = 0.50,
      col_azul = "#1D4ED8", col_verde = "#15803D", col_amarillo = "#B45309", col_rojo = "#B91C1C",
      fondo_azul = "#EFF6FF", fondo_verde = "#F0FDF4", fondo_amarillo = "#FFFBEB", fondo_rojo = "#FEF2F2"
    )
    
    # Helpers ----
    
    .semaforo <- function(v, tipo = "fondo_na") {
      s <- .SEMAFORO
      switch(tipo,
             fondo_na = dplyr::case_when(
               is.na(v)             ~ "white",
               v >= s$umbral_exceso ~ s$fondo_azul,
               v >= s$umbral_alto   ~ s$fondo_verde,
               v >= s$umbral_bajo   ~ s$fondo_amarillo,
               TRUE                 ~ s$fondo_rojo
             ))
    }
    
    gt_pct_style_semaforo <- function(gt_table, ...) {
      cols <- rlang::ensyms(...)
      s <- .SEMAFORO
      for (col in cols) {
        gt_table <- gt_table %>%
          gt::tab_style(style = gt::cell_text(color = s$col_rojo, weight = "bold"),
                        locations = gt::cells_body(columns = !!col, rows = !!col < s$umbral_bajo)) %>%
          gt::tab_style(style = gt::cell_text(color = s$col_amarillo, weight = "bold"),
                        locations = gt::cells_body(columns = !!col,
                                                   rows = !!col >= s$umbral_bajo & !!col < s$umbral_alto)) %>%
          gt::tab_style(style = gt::cell_text(color = s$col_verde, weight = "bold"),
                        locations = gt::cells_body(columns = !!col,
                                                   rows = !!col >= s$umbral_alto & !!col < s$umbral_exceso)) %>%
          gt::tab_style(style = gt::cell_text(color = s$col_azul, weight = "bold"),
                        locations = gt::cells_body(columns = !!col, rows = !!col >= s$umbral_exceso))
      }
      gt_table
    }
    
    .normalizar_kilos <- function(df) {
      df %>% mutate(
        Margen    = ifelse(is.infinite(Margen), NA, Margen),
        KILOS     = ifelse(LinNegCod == 10000, SacLote * 62.5, SacLote * 70),
        KilosFact = ifelse(is.na(KilosFact), KILOS, KilosFact)
      )
    }
    
    # Mes de corte según el periodo — año en curso corta en el mes actual;
    # años ya cerrados cortan en diciembre (los 12 meses son válidos).
    .mes_corte_periodo <- function(periodo) {
      if (periodo == year(Sys.Date())) month(Sys.Date()) else 12L
    }
    
    # ETL ----
    
    # Agrupa la facturación del cliente por año/mes. Fecha se indexa
    # directamente sobre .MESES (por número), NO con format(..., "%B"),
    # porque %B depende del locale del sistema y puede devolver el nombre
    # del mes en inglés si la sesión no llamó Sys.setlocale("LC_TIME", ...)
    # — eso rompía silenciosamente el join contra .MESES.
    procesar_datos_cliente <- function(data, periodo_desde) {
      data %>%
        filter(!is.na(FecFact), year(FecFact) >= periodo_desde) %>%
        mutate(Anho = year(FecFact), MesNum = month(FecFact)) %>%
        .normalizar_kilos() %>%
        group_by(Periodo = Anho, Fecha = .MESES[MesNum]) %>%
        summarise(Sacos70 = sum(SacosPYG, na.rm = TRUE), MargenFCC = sum(Margen, na.rm = TRUE),
                  .groups = "drop")
    }
    
    # Reactivos base ----
    
    calendario_r <- reactive({
      mes <- month(Sys.Date())
      list(mes_actual = mes, meses_rest = 13L - mes)
    })
    
    periodo_r <- reactive({
      req(nrow(dat()) > 0)
      year(max(dat()$FecFact, na.rm = TRUE))
    })
    
    datos_base_r <- reactive({ procesar_datos_cliente(dat(), periodo_r() - 1L) })
    datos_act_r  <- reactive({ procesar_datos_cliente(dat(), periodo_r()) })
    
    # Presupuesto del cliente: último snapshot de CRMNALCLIENTE para la
    # cuenta PRINCIPAL (identidad()$nit), no de las cuentas hijas — evita
    # depender del full-join multi-cliente del módulo de portafolio.
    ppto_anual_r <- reactive({
      id_val <- identidad(); req(!is.null(id_val))
      CargarDatos("CRMNALCLIENTE") %>%
        mutate(FecProceso = as_datetime(FecProceso)) %>%
        filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod) %>%
        arrange(desc(FecProceso)) %>%
        slice(1) %>%
        transmute(PptoSSAnual = SSPpto %||% 0, PptoMaAnual = MNFCCPpto %||% 0)
    })
    
    ppto_mensual_r <- reactive({
      ppto <- ppto_anual_r()
      tibble::tibble(Fecha = .MESES, PptoSS = ppto$PptoSSAnual / 12, PptoMa = ppto$PptoMaAnual / 12) %>%
        crossing(Periodo = periodo_r())
    })
    
    # Serie de gráfico: rellena con 0 los meses YA TRANSCURRIDOS sin
    # facturación (para que el acumulado sea correcto), pero deja en NA los
    # meses futuros (para no proyectar un acumulado que aún no puede existir).
    datos_grafico_r <- reactive({
      db <- datos_act_r() %>% filter(Periodo == periodo_r())
      ppto <- ppto_mensual_r()
      corte <- .mes_corte_periodo(periodo_r())
      
      tibble::tibble(Fecha = factor(.MESES, levels = .MESES, ordered = TRUE)) %>%
        left_join(db %>% mutate(Fecha = factor(Fecha, levels = .MESES, ordered = TRUE)),
                  by = "Fecha") %>%
        left_join(ppto %>% mutate(Fecha = factor(Fecha, levels = .MESES, ordered = TRUE)),
                  by = "Fecha") %>%
        mutate(
          Mes_Num     = match(as.character(Fecha), .MESES),
          Sacos_Real  = ifelse(Mes_Num <= corte, coalesce(Sacos70,   0), NA_real_),
          Margen_Real = ifelse(Mes_Num <= corte, coalesce(MargenFCC, 0), NA_real_),
          Sacos_Ppto  = coalesce(PptoSS, 0),
          Margen_Ppto = coalesce(PptoMa, 0)
        ) %>%
        arrange(Mes_Num)
    })
    
    # Gráfico de serie temporal ejecución vs presupuesto con meta futura ----
    
    construir_grafico_serie <- function(datos_grafico, vista_acumulada, col_real, col_ppto,
                                        titulo_acum, titulo_mensual, label_y_acum, label_y_mensual,
                                        formato_y = "numero") {
      mes_actual <- month(Sys.Date())
      
      datos_grafico <- datos_grafico %>%
        arrange(Mes_Num) %>%
        mutate(
          acum_real = cumsum(ifelse(is.na(.data[[col_real]]), 0, .data[[col_real]])),
          acum_ppto = cumsum(.data[[col_ppto]])
        )
      
      ultimo_mes <- suppressWarnings(
        max(datos_grafico$Mes_Num[!is.na(datos_grafico[[col_real]])], na.rm = TRUE)
      )
      if (is.infinite(ultimo_mes)) ultimo_mes <- 0
      
      datos_grafico <- datos_grafico %>%
        mutate(acum_real = ifelse(Mes_Num <= ultimo_mes, acum_real, NA_real_))
      
      ejec_acum <- datos_grafico %>%
        filter(Mes_Num <= ultimo_mes, !is.na(.data[[col_real]])) %>%
        summarise(s = sum(.data[[col_real]], na.rm = TRUE)) %>%
        pull(s)
      
      ppto_total <- sum(datos_grafico[[col_ppto]], na.rm = TRUE)
      meses_rest <- 13L - mes_actual
      meta_mens  <- SiError_0((ppto_total - ejec_acum) / pmax(meses_rest, 1))
      
      datos_grafico <- datos_grafico %>%
        mutate(
          meta_mensual = ifelse(Mes_Num >= mes_actual, meta_mens, NA_real_),
          meta_acum    = ifelse(
            Mes_Num >= mes_actual,
            ejec_acum + (Mes_Num - mes_actual + 1L) * meta_mens, NA_real_
          )
        )
      
      if (vista_acumulada) {
        datos_plot <- datos_grafico %>%
          select(Fecha, acum_ppto, acum_real, meta_acum) %>%
          pivot_longer(c(acum_ppto, acum_real, meta_acum), names_to = "Tipo", values_to = "Valor") %>%
          mutate(Tipo = dplyr::recode(Tipo, acum_ppto = "Presupuesto Acumulado",
                                      acum_real = "Ejecutado Acumulado",
                                      meta_acum = "Meta Acumulada para Ppto"),
                 Tipo = factor(Tipo, levels = c("Presupuesto Acumulado", "Ejecutado Acumulado",
                                                "Meta Acumulada para Ppto")))
        titulo_g <- titulo_acum; titulo_y <- label_y_acum
      } else {
        datos_plot <- datos_grafico %>%
          select(Fecha, ppto = all_of(col_ppto), real = all_of(col_real), meta_men = meta_mensual) %>%
          pivot_longer(c(ppto, real, meta_men), names_to = "Tipo", values_to = "Valor") %>%
          mutate(Tipo = dplyr::recode(Tipo, ppto = "Presupuesto Mensual", real = "Ejecutado Mensual",
                                      meta_men = "Meta Mensual para Ppto"),
                 Tipo = factor(Tipo, levels = c("Presupuesto Mensual", "Ejecutado Mensual",
                                                "Meta Mensual para Ppto")))
        titulo_g <- titulo_mensual; titulo_y <- label_y_mensual
      }
      
      trazas <- list(
        list(tipo = if (vista_acumulada) "Presupuesto Acumulado" else "Presupuesto Mensual",
             color = "#52525C", dash = "dash"),
        list(tipo = if (vista_acumulada) "Ejecutado Acumulado" else "Ejecutado Mensual",
             color = .RACAFE_ROOT$red_primary, dash = "solid"),
        list(tipo = if (vista_acumulada) "Meta Acumulada para Ppto" else "Meta Mensual para Ppto",
             color = .RACAFE_WEB$cafe_corporativo, dash = "dot")
      )
      
      p <- plot_ly()
      for (tr in trazas) {
        p <- p %>% add_trace(
          data = filter(datos_plot, Tipo == tr$tipo), x = ~Fecha, y = ~Valor,
          name = tr$tipo, type = "scatter", mode = "lines+markers",
          line = list(color = tr$color, dash = tr$dash, width = 2), marker = list(color = tr$color),
          hovertemplate = paste0("<b>", tr$tipo, "</b><br>Mes: %{x}<br>Valor: %{y:,.0f}<extra></extra>")
        )
      }
      
      yax <- list(title = titulo_y, tickformat = ",", rangemode = "tozero")
      if (formato_y == "dinero") yax$tickprefix <- "$"
      
      p %>%
        layout(title = titulo_g, xaxis = list(title = "Mes"), yaxis = yax,
               legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center", yanchor = "top"),
               hovermode = "x unified") %>%
        config(displayModeBar = FALSE, displaylogo = FALSE)
    }
    
    # KPIs ----
    
    kpi_sacos_r <- reactive({
      cal <- calendario_r()
      dg <- datos_grafico_r() %>% filter(Mes_Num <= cal$mes_actual)
      ejec <- sum(dg$Sacos_Real, na.rm = TRUE); ppto <- sum(dg$Sacos_Ppto, na.rm = TRUE)
      list(ejec = ejec, ppto = ppto, cumpl = SiError_0(ejec / ppto),
           periodo = periodo_r(), mes_actual = cal$mes_actual)
    })
    kpi_margen_r <- reactive({
      cal <- calendario_r()
      dg <- datos_grafico_r() %>% filter(Mes_Num <= cal$mes_actual)
      ejec <- sum(dg$Margen_Real, na.rm = TRUE); ppto <- sum(dg$Margen_Ppto, na.rm = TRUE)
      list(ejec = ejec, ppto = ppto, cumpl = SiError_0(ejec / ppto),
           periodo = periodo_r(), mes_actual = cal$mes_actual)
    })
    kpi_ritmo_sacos_r <- reactive({
      cal <- calendario_r(); dg <- datos_grafico_r()
      ejec_ytd <- dg %>% filter(Mes_Num <= cal$mes_actual) %>% summarise(s = sum(Sacos_Real, na.rm = TRUE)) %>% pull(s)
      ppto_anual <- dg %>% summarise(p = sum(Sacos_Ppto, na.rm = TRUE)) %>% pull(p)
      faltante <- ppto_anual - ejec_ytd
      list(ritmo = SiError_0(faltante / pmax(cal$meses_rest, 1)), faltante = faltante,
           meses_rest = cal$meses_rest)
    })
    kpi_ritmo_margen_r <- reactive({
      cal <- calendario_r(); dg <- datos_grafico_r()
      ejec_ytd <- dg %>% filter(Mes_Num <= cal$mes_actual) %>% summarise(m = sum(Margen_Real, na.rm = TRUE)) %>% pull(m)
      ppto_anual <- dg %>% summarise(p = sum(Margen_Ppto, na.rm = TRUE)) %>% pull(p)
      faltante <- ppto_anual - ejec_ytd
      list(ritmo = SiError_0(faltante / pmax(cal$meses_rest, 1)), faltante = faltante,
           meses_rest = cal$meses_rest)
    })
    
    # Outputs ----
    
    racafeModulos::CajaModal(
      id = "kpi_cumpl_sacos_ytd", valor = reactive(kpi_sacos_r()$cumpl), formato = "porcentaje",
      texto = reactive(paste0("Cumpl. Sacos — Acum. ", kpi_sacos_r()$periodo)),
      icono = "check-double", colores = reactive(c(fondo = "white")),
      color_fondo_hex = reactive(.semaforo(kpi_sacos_r()$cumpl, "fondo_na")), mostrar_boton = FALSE,
      footer = reactive(paste0(FormatearNumero(kpi_sacos_r()$ejec, "coma"), " sacos facturados de ",
                               FormatearNumero(kpi_sacos_r()$ppto, "coma"), " presupuestados") %>% HTML)
    )
    racafeModulos::CajaModal(
      id = "kpi_cumpl_margen_ytd", valor = reactive(kpi_margen_r()$cumpl), formato = "porcentaje",
      texto = reactive(paste0("Cumpl. Margen — Acum. ", kpi_margen_r()$periodo)),
      icono = "dollar-sign", colores = reactive(c(fondo = "white")),
      color_fondo_hex = reactive(.semaforo(kpi_margen_r()$cumpl, "fondo_na")), mostrar_boton = FALSE,
      footer = reactive(paste0(FormatearNumero(kpi_margen_r()$ejec, "dinero"), " facturado de ",
                               FormatearNumero(kpi_margen_r()$ppto, "dinero"), " presupuestado") %>% HTML)
    )
    racafeModulos::CajaModal(
      id = "kpi_ritmo_req_sacos", valor = reactive(kpi_ritmo_sacos_r()$ritmo), formato = "numero",
      texto = "Sacos / mes requeridos", icono = "arrow-trend-up", colores = reactive(c(fondo = "white")),
      color_fondo_hex = "#F8FAFC", mostrar_boton = FALSE,
      footer = reactive(paste0("Faltan ", FormatearNumero(kpi_ritmo_sacos_r()$faltante, "coma"),
                               " sacos en ", kpi_ritmo_sacos_r()$meses_rest,
                               " meses para cerrar el presupuesto") %>% HTML)
    )
    racafeModulos::CajaModal(
      id = "kpi_ritmo_req_margen", valor = reactive(kpi_ritmo_margen_r()$ritmo), formato = "dinero",
      texto = "Margen / mes requerido", icono = "money-bill-trend-up", colores = reactive(c(fondo = "white")),
      color_fondo_hex = "#F8FAFC", mostrar_boton = FALSE,
      footer = reactive(paste0("Faltan ", FormatearNumero(kpi_ritmo_margen_r()$faltante, "dinero"),
                               " en ", kpi_ritmo_margen_r()$meses_rest,
                               " meses para cerrar el presupuesto de margen") %>% HTML)
    )
    
    # Tabla GT mensual/acumulada ----
    output$Presupuesto <- render_gt({
      periodo     <- periodo_r()
      periodo_ant <- periodo - 1L
      ppto_mens   <- ppto_mensual_r()
      corte_act   <- .mes_corte_periodo(periodo)
      corte_ant   <- .mes_corte_periodo(periodo_ant)
      
      datos_ant <- datos_base_r() %>% filter(Periodo == periodo_ant)
      datos_act <- datos_act_r()  %>% filter(Periodo == periodo)
      
      t1 <- tibble::tibble(Fecha = factor(.MESES, levels = .MESES, ordered = TRUE)) %>%
        left_join(datos_ant %>% mutate(Fecha = factor(Fecha, levels = .MESES, ordered = TRUE)) %>%
                    select(Fecha, SacosAnt = Sacos70, MargenAnt = MargenFCC), by = "Fecha") %>%
        left_join(datos_act %>% mutate(Fecha = factor(Fecha, levels = .MESES, ordered = TRUE)) %>%
                    select(Fecha, SacosAct = Sacos70, MargenAct = MargenFCC), by = "Fecha") %>%
        left_join(ppto_mens %>% mutate(Fecha = factor(Fecha, levels = .MESES, ordered = TRUE)) %>%
                    select(Fecha, PptoSS, PptoMa), by = "Fecha") %>%
        arrange(Fecha) %>%
        mutate(
          Mes_Num   = match(as.character(Fecha), .MESES),
          # Meses ya transcurridos -> 0 si no hubo ventas; futuros -> NA
          SacosAnt  = ifelse(Mes_Num <= corte_ant, coalesce(SacosAnt,  0), NA_real_),
          MargenAnt = ifelse(Mes_Num <= corte_ant, coalesce(MargenAnt, 0), NA_real_),
          SacosAct  = ifelse(Mes_Num <= corte_act, coalesce(SacosAct,  0), NA_real_),
          MargenAct = ifelse(Mes_Num <= corte_act, coalesce(MargenAct, 0), NA_real_)
        ) %>%
        mutate(
          CumplSacos  = SiError_0(SacosAct  / PptoSS),
          CumplMargen = SiError_0(MargenAct / PptoMa),
          VarSacos    = Variacion(SacosAnt,  SacosAct),
          VarMargen   = Variacion(MargenAnt, MargenAct),
          # Acumulado: se suma el 0 (mes ya transcurrido sin ventas) y se
          # oculta (NA) solo a partir del mes futuro — un solo mes pasado
          # sin facturación ya no rompe el acumulado de meses siguientes.
          AcumSacosAnt  = ifelse(Mes_Num <= corte_ant, cumsum(coalesce(SacosAnt,  0)), NA_real_),
          AcumMargenAnt = ifelse(Mes_Num <= corte_ant, cumsum(coalesce(MargenAnt, 0)), NA_real_),
          AcumPptoSS    = cumsum(coalesce(PptoSS, 0)),
          AcumPptoMa    = cumsum(coalesce(PptoMa, 0)),
          AcumSacosAct  = ifelse(Mes_Num <= corte_act, cumsum(coalesce(SacosAct,  0)), NA_real_),
          AcumMargenAct = ifelse(Mes_Num <= corte_act, cumsum(coalesce(MargenAct, 0)), NA_real_),
          CumplSacosAcum  = SiError_0(AcumSacosAct  / AcumPptoSS),
          CumplMargenAcum = SiError_0(AcumMargenAct / AcumPptoMa),
          VarSacosAcum    = ifelse(!is.na(AcumSacosAnt) & !is.na(AcumSacosAct),
                                   Variacion(AcumSacosAnt, AcumSacosAct), NA),
          VarMargenAcum   = ifelse(!is.na(AcumMargenAnt) & !is.na(AcumMargenAct),
                                   Variacion(AcumMargenAnt, AcumMargenAct), NA)
        )
      
      fila_total <- t1 %>%
        summarise(SacosAnt = sum(SacosAnt, na.rm = TRUE), MargenAnt = sum(MargenAnt, na.rm = TRUE),
                  PptoSS = sum(PptoSS, na.rm = TRUE), SacosAct = sum(SacosAct, na.rm = TRUE),
                  PptoMa = sum(PptoMa, na.rm = TRUE), MargenAct = sum(MargenAct, na.rm = TRUE),
                  across(c(AcumSacosAnt, AcumMargenAnt, AcumPptoSS, AcumSacosAct, AcumPptoMa,
                           AcumMargenAct, CumplSacosAcum, VarSacosAcum, CumplMargenAcum, VarMargenAcum),
                         ~ NA_real_)) %>%
        mutate(Fecha = "TOTAL", CumplSacos = SiError_0(SacosAct / PptoSS),
               VarSacos = Variacion(SacosAnt, SacosAct), CumplMargen = SiError_0(MargenAct / PptoMa),
               VarMargen = Variacion(MargenAnt, MargenAct))
      
      bind_rows(t1 %>% mutate(Fecha = as.character(Fecha)), fila_total) %>%
        select(Fecha, SacosAnt, AcumSacosAnt, MargenAnt, AcumMargenAnt,
               PptoSS, SacosAct, CumplSacos, VarSacos, AcumPptoSS, AcumSacosAct, CumplSacosAcum, VarSacosAcum,
               PptoMa, MargenAct, CumplMargen, VarMargen, AcumPptoMa, AcumMargenAct, CumplMargenAcum, VarMargenAcum) %>%
        gt() %>%
        tab_header(title = md(paste0("**Seguimiento de Presupuesto ", periodo, "**")),
                   subtitle = md(paste0("Cliente actual — Periodo anterior: ", periodo_ant))) %>%
        tab_spanner(label = as.character(periodo_ant), columns = 2:5) %>%
        tab_spanner(label = as.character(periodo), columns = 6:21) %>%
        tab_spanner(label = "Sacos Mensual", columns = 6:9) %>%
        tab_spanner(label = "Sacos Acumulado", columns = 10:13) %>%
        tab_spanner(label = "$MNFCC Mensual", columns = 14:17) %>%
        tab_spanner(label = "$MNFCC Acumulado", columns = 18:21) %>%
        cols_label(Fecha = "", SacosAnt = "Sacos", AcumSacosAnt = "Sacos Acum.",
                   MargenAnt = "Margen", AcumMargenAnt = "Margen Acum.",
                   PptoSS = "Presupuesto", SacosAct = "Ejecutado", CumplSacos = "Cumplimiento",
                   VarSacos = "Comp. Per. Ant.", AcumPptoSS = "Presupuesto", AcumSacosAct = "Ejecutado",
                   CumplSacosAcum = "Cumplimiento", VarSacosAcum = "Comp. Per. Ant.",
                   PptoMa = "Presupuesto", MargenAct = "Ejecutado", CumplMargen = "Cumplimiento",
                   VarMargen = "Comp. Per. Ant.", AcumPptoMa = "Presupuesto", AcumMargenAct = "Ejecutado",
                   CumplMargenAcum = "Cumplimiento", VarMargenAcum = "Comp. Per. Ant.") %>%
        fmt_number(columns = c(2, 3, 6, 7, 10, 11), decimals = 0) %>%
        fmt_currency(columns = c(4, 5, 14, 15, 18, 19), currency = "COP", decimals = 0) %>%
        fmt_percent(columns = c(8, 9, 12, 13, 16, 17, 20, 21), decimals = 1) %>%
        gt_minimal_style() %>%
        sub_missing(columns = everything(), rows = everything(), missing_text = "") %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(rows = Fecha == "TOTAL")) %>%
        gt_pct_style_semaforo(CumplSacos) %>%
        gt_pct_style_semaforo(CumplSacosAcum) %>%
        gt_pct_style_semaforo(CumplMargen) %>%
        gt_pct_style_semaforo(CumplMargenAcum) %>%
        tab_options(table.width = pct(100))
    })
    
    output$GraficoPresupuestoSacos <- renderPlotly({
      construir_grafico_serie(datos_grafico_r(), input$vista_acumulada, "Sacos_Real", "Sacos_Ppto",
                              "Ejecución vs Presupuesto — Sacos (Acumulado)",
                              "Ejecución vs Presupuesto — Sacos (Mensual)",
                              "Sacos Acumulados", "Sacos Mensuales", "numero")
    })
    output$GraficoPresupuestoMargen <- renderPlotly({
      construir_grafico_serie(datos_grafico_r(), input$vista_acumulada, "Margen_Real", "Margen_Ppto",
                              "Ejecución vs Presupuesto — Margen (Acumulado)",
                              "Ejecución vs Presupuesto — Margen (Mensual)",
                              "Margen Acumulado", "Margen Mensual", "dinero")
    })
  })
}
# IndRFM -----------------------------------------------------------------
# RFM, CLV y Churn (bases Sacos S / Margen M). Radar incluye referencia de
# promedio general (punto medio de escala 1-5). Selector cambiado a
# lista desplegable.

IndRFMUI <- function(id) {
  ns <- NS(id)
  box(title = tagList(icon("star"), " RFM, CLV & Churn"), width = 12, status = "white",
      collapsible = TRUE, collapsed = TRUE,
      fluidRow(column(4, racafe::ListaDesplegable(ns("IND_RFM_base"), label = h6("Base"),
                                                  choices = c("Sacos" = "S", "Margen" = "M"),
                                                  selected = "S", multiple = FALSE))),
      fluidRow(
        column(3, racafeModulos::CajaModalUI(ns("kpi_churn"))),
        column(3, racafeModulos::CajaModalUI(ns("kpi_clv"))),
        column(3, racafeModulos::CajaModalUI(ns("kpi_pred_sac"))),
        column(3, racafeModulos::CajaModalUI(ns("kpi_rec_days")))
      ),
      fluidRow(
        column(6, box(title = "Radar RFM (1-5)", width = 12, status = "white",
                      collapsible = TRUE, collapsed = FALSE,
                      plotlyOutput(ns("plt_rfm"), height = "300px"))),
        column(6, box(title = "Segmentación Analítica", width = 12, status = "white",
                      collapsible = TRUE, collapsed = FALSE, gt_output(ns("tbl_segmento"))))
      ))
}
IndRFM <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rfm_snap <- reactive(dat()[1, ])
    
    racafeModulos::CajaModal("kpi_churn",
                             valor = reactive({ ch <- as.numeric(rfm_snap()$Churn %||% NA); if (is.na(ch)) 0 else ch * 100 }),
                             formato = "porcentaje", texto = "Prob. Pérdida (Churn %)", icono = "user-slash",
                             colores = c(fondo = "white"), mostrar_boton = FALSE,
                             footer = reactive({
                               ch <- as.numeric(rfm_snap()$Churn %||% NA)
                               if (is.na(ch)) "N/D" else if (ch < 0.10) "Riesgo bajo"
                               else if (ch < 0.30) "Riesgo medio" else "Riesgo alto"
                             }))
    
    racafeModulos::CajaModal("kpi_clv",
                             valor = reactive({
                               marg <- sum(dat()$Margen, na.rm = TRUE)
                               anos <- max(as.numeric(difftime(max(dat()$FecFact, na.rm = TRUE),
                                                               min(dat()$FecFact, na.rm = TRUE), units = "days")) / 365.25, 1)
                               ch <- as.numeric(rfm_snap()$Churn %||% 0.20)
                               ch <- if (is.na(ch) || ch <= 0) 0.01 else ch
                               (marg / anos / ch) / 1e6
                             }),
                             formato = "dinero", texto = "CLV Estimado (MM)", icono = "gem",
                             colores = c(fondo = "white"), mostrar_boton = FALSE,
                             footer = reactive("Margen anualizado / Churn"))
    
    racafeModulos::CajaModal("kpi_pred_sac",
                             valor = reactive(as.numeric(rfm_snap()$SacosPred %||% 0)),
                             formato = "coma", texto = "Sacos Predichos", icono = "chart-line",
                             colores = c(fondo = "white"), mostrar_boton = FALSE,
                             footer = reactive("Próximo período (modelo ML)"))
    
    racafeModulos::CajaModal("kpi_rec_days",
                             valor = reactive({
                               req(input$IND_RFM_base)
                               as.numeric(rfm_snap()[[paste0("recency_days", input$IND_RFM_base)]] %||% 0)
                             }),
                             formato = "entero", texto = "Días sin Comprar", icono = "clock",
                             colores = c(fondo = "white"), mostrar_boton = FALSE,
                             footer = reactive({
                               req(input$IND_RFM_base)
                               dias <- as.numeric(rfm_snap()[[paste0("recency_days", input$IND_RFM_base)]] %||% NA)
                               if (is.na(dias)) "N/D" else if (dias <= 30) "Activo reciente"
                               else if (dias <= 90) "En seguimiento" else "Recuperación requerida"
                             }))
    
    output$plt_rfm <- renderPlotly({
      req(input$IND_RFM_base)
      r <- rfm_snap(); base <- input$IND_RFM_base
      r_sc <- as.numeric(r[[paste0("recency_score", base)]] %||% 0)
      f_sc <- as.numeric(r[[paste0("frequency_score", base)]] %||% 0)
      m_sc <- as.numeric(r[[paste0("monetary_score", base)]] %||% 0)
      
      prom_r <- 3; prom_f <- 3; prom_m <- 3
      
      plot_ly(type = "scatterpolar") %>%
        add_trace(r = c(prom_r, prom_f, prom_m, prom_r),
                  theta = c("Recencia", "Frecuencia", "Monetario", "Recencia"),
                  fill = "toself", name = "Promedio general",
                  marker = list(color = .RACAFE_ROOT$gray_600, size = 6),
                  line = list(color = .RACAFE_ROOT$gray_600, width = 1, dash = "dot"),
                  fillcolor = "rgba(161,161,161,0.10)",
                  hovertemplate = "<b>%{theta}</b><br>Promedio: %{r} / 5<extra></extra>") %>%
        add_trace(r = c(r_sc, f_sc, m_sc, r_sc),
                  theta = c("Recencia", "Frecuencia", "Monetario", "Recencia"),
                  fill = "toself", name = "Cliente",
                  marker = list(color = .RACAFE_ROOT$red_primary, size = 8),
                  line = list(color = .RACAFE_ROOT$red_primary, width = 2),
                  fillcolor = "rgba(217,4,41,0.15)",
                  hovertemplate = "<b>%{theta}</b><br>Score: %{r} / 5<extra></extra>") %>%
        layout(polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 5), tickvals = 0:5,
                            gridcolor = .RACAFE_ROOT$gray_400),
          angularaxis = list(gridcolor = .RACAFE_ROOT$gray_400)),
          showlegend = TRUE, plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)") %>%
        config(displayModeBar = FALSE)
    })
    
    output$tbl_segmento <- render_gt({
      req(input$IND_RFM_base)
      r <- rfm_snap(); base <- input$IND_RFM_base
      data.frame(
        Campo = c("Segmento Analítico", "Score RFM", "Score Recencia", "Score Frecuencia",
                  "Score Monetario", "Nº Transacciones", "Tipo Cliente Racafé"),
        Valor = c(
          as.character(r[[paste0("SegmentoAnalitica", base)]] %||% "N/D"),
          as.character(r[[paste0("rfm_score", base)]] %||% "N/D"),
          as.character(r[[paste0("recency_score", base)]] %||% "N/D"),
          as.character(r[[paste0("frequency_score", base)]] %||% "N/D"),
          as.character(r[[paste0("monetary_score", base)]] %||% "N/D"),
          as.character(r[[paste0("transaction_count", base)]] %||% "N/D"),
          as.character(r$SegmentoRacafe %||% "N/D")
        ), stringsAsFactors = FALSE
      ) %>%
        gt() %>% gt_minimal_style() %>%
        cols_label(Campo = "", Valor = "") %>%
        cols_width(Campo ~ px(200)) %>%
        tab_style(style = cell_text(weight = "bold", color = .RACAFE_WEB$texto_principal),
                  locations = cells_body(columns = Campo)) %>%
        tab_options(table.width = pct(100))
    })
  })
}

# IndFormulario -----------------------------------------------------------
IndFormularioUI <- function(id) {
  ns <- NS(id)
  box(title = "Jerarquía y Datos Comerciales del Cliente", width = 12, status = "white",
      collapsible = TRUE, collapsed = TRUE,
      fluidRow(column(12, tags$div(h6("Cuenta (Hija o Principal)"),
                                   uiOutput(ns("ui_frm_cuenta_hija"))))),
      Saltos(1),
      fluidRow(column(12, tags$div(h6("Nueva Cuenta Padre"), uiOutput(ns("ui_frm_padre_nuevo"))))),
      Saltos(1),
      fluidRow(column(12, createSwitch("frm_ConvertirPrincipal",
                                       "Convertir en cuenta principal (independizar)",
                                       FALSE, ns = ns))),
      uiOutput(ns("aviso_independizar")),
      tags$hr(),
      div(style = "text-align: right;",
          actionButton(ns("btn_guardar_hijo"), "Guardar Jerarquía", icon = icon("sitemap"),
                       class = "btn-danger")),
      tags$hr(),
      fluidRow(
        column(12, ind_picker("frm_Asesor", "Asesor", con_vacio(.IND_CHO$personas), ns = ns)),
        column(12, ind_picker("frm_Segmento", "Segmento", con_vacio(.IND_CHO$segmento), ns = ns)),
        column(12, ind_picker("frm_Excluir", "Excluir", c("NO", "SI"), ns = ns))
      ),
      fluidRow(column(12, autonumericInput(ns("frm_NumMesesRecuperar"), h6("Meses para Recuperar"),
                                           value = 3L, width = "100%", decimalPlaces = 0,
                                           minimumValue = 1))),
      tags$hr(),
      div(style = "text-align: right;",
          actionButton(ns("btn_guardar"), "Guardar cambios", icon = icon("save"),
                       class = "btn-danger")),
      uiOutput(ns("aviso_sin_permiso"))
  )
}
IndFormulario <- function(id, identidad, dat, usr, ncliente) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0L)
    
    es_admin <- reactive(toupper(trimws(usr() %||% "")) == "JONATHAN CAÑON")
    
    observe({
      if (es_admin()) {
        shinyjs::enable(ns("btn_guardar_hijo"))
        shinyjs::enable(ns("btn_guardar"))
      } else {
        shinyjs::disable(ns("btn_guardar_hijo"))
        shinyjs::disable(ns("btn_guardar"))
      }
    })
    
    output$aviso_sin_permiso <- renderUI({
      if (es_admin()) return(NULL)
      tags$p(style = "font-size:12px; color:#64748B; margin-top:8px;",
             icon("lock"), " Solo el administrador (Jonathan Cañón) puede guardar cambios en esta sección.")
    })
    
    identidad_activa <- reactiveVal(NULL)
    observeEvent(identidad(), {
      id_val <- identidad(); req(!is.null(id_val))
      identidad_activa(list(nit = id_val$nit, linneg_cod = id_val$linneg_cod))
    })
    
    output$ui_frm_cuenta_hija <- renderUI({
      id_val <- identidad_activa(); req(!is.null(id_val))
      df <- dat()
      req(nrow(df) > 0)
      
      # Aseguramos escalares para evitar reciclado inconsistente en paste0()
      razon_soc <- Unicos(df$PerRazSoc)
      req(length(razon_soc) > 0)
      razon_soc <- razon_soc[1]
      
      nit_val <- as.character(id_val$nit)
      req(length(nit_val) == 1, nzchar(nit_val))
      
      # Cuentas hija (puede no existir ninguna)
      hijos <- df %>%
        dplyr::distinct(CLCliNit, RazonSocialCliNit) %>%
        dplyr::filter(!is.na(CLCliNit), CLCliNit != id_val$nit)
      
      # Choice de la cuenta principal, siempre presente
      choices_principal <- setNames(nit_val, paste0(nit_val, " - ", razon_soc, " (Principal)"))
      
      # Se agregan hijos solo si existen, evitando setNames con longitudes desalineadas
      if (nrow(hijos) > 0) {
        choices_hijas <- setNames(as.character(hijos$CLCliNit),
                                  paste0(hijos$CLCliNit, " - ", hijos$RazonSocialCliNit))
        choices_hija <- c(choices_principal, choices_hijas)
      } else {
        choices_hija <- choices_principal
      }
      
      racafe::ListaDesplegable(ns("frm_CuentaHija"), label = NULL, choices = choices_hija,
                               selected = NULL, multiple = FALSE)
    })
    
    output$ui_frm_padre_nuevo <- renderUI({
      req(nrow(ncliente()) > 0)
      choices_padre <- ncliente() %>%
        dplyr::filter(!is.na(PerCod), !is.na(PerRazSoc)) %>%
        dplyr::arrange(PerRazSoc) %>%
        {setNames(as.character(.$PerCod), paste0(.$PerCod, " - ", .$PerRazSoc))}
      racafe::ListaDesplegable(ns("frm_CuentaPadreNueva"), label = NULL,
                               choices = choices_padre, selected = NULL, multiple = FALSE)
    })
    
    observeEvent(input$frm_ConvertirPrincipal, {
      if (isTRUE(input$frm_ConvertirPrincipal)) {
        cuenta_hija <- input$frm_CuentaHija
        req(nzchar(cuenta_hija %||% ""))
        updatePickerInput(session, "frm_CuentaPadreNueva", selected = cuenta_hija)
      }
    })
    
    observeEvent(input$frm_CuentaHija, {
      if (isTRUE(input$frm_ConvertirPrincipal)) {
        updatePickerInput(session, "frm_CuentaPadreNueva", selected = input$frm_CuentaHija)
      }
    }, ignoreInit = TRUE)
    
    output$aviso_independizar <- renderUI({
      if (!isTRUE(input$frm_ConvertirPrincipal)) return(NULL)
      tags$p(style = "font-size:12px; color:#64748B; margin-top:6px;", icon("circle-info"), " ",
             "Al guardar, esta cuenta pasará a ser su propia cuenta principal.")
    })
    
    observeEvent(input$btn_guardar_hijo, {
      req(es_admin())
      cuenta_hija <- input$frm_CuentaHija
      req(nzchar(cuenta_hija %||% ""))
      convertir <- isTRUE(input$frm_ConvertirPrincipal)
      nuevo_padre <- if (convertir) cuenta_hija else trimws(input$frm_CuentaPadreNueva %||% "")
      
      if (!convertir && !nzchar(nuevo_padre)) {
        showNotification("Selecciona una cuenta padre o marca 'Convertir en cuenta principal'.",
                         type = "error")
        return()
      }
      
      snap <- CargarDatos("CRMNALCLIENTE") %>%
        dplyr::mutate(FecProceso = as_datetime(FecProceso)) %>%
        dplyr::group_by(CLCliNit) %>%
        dplyr::arrange(dplyr::desc(FecProceso)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
      
      dependencia_inversa <- snap %>%
        dplyr::filter(as.character(CLCliNit) == nuevo_padre, as.character(CliNitPpal) == cuenta_hija)
      if (nrow(dependencia_inversa) > 0) {
        showNotification(
          "No es posible: la cuenta padre depende actualmente de la cuenta que estás moviendo.",
          type = "error", duration = NULL)
        return()
      }
      
      if (!convertir) {
        propios <- snap %>%
          dplyr::filter(as.character(CliNitPpal) == cuenta_hija, as.character(CLCliNit) != cuenta_hija)
        if (nrow(propios) > 0) {
          showNotification(paste0("Esta cuenta tiene ", nrow(propios),
                                  " cuenta(s) hija(s) propia(s). Reasígnalas antes de mover esta cuenta."),
                           type = "error", duration = NULL)
          return()
        }
      }
      
      confirmSweetAlert(session = session, inputId = ns("confirm_hijo"),
                        title = "Confirmar cambio de jerarquía",
                        text = if (convertir) "¿Deseas independizar esta cuenta como principal?"
                        else "¿Deseas asignar esta cuenta a la nueva cuenta padre?",
                        type = "warning", btn_labels = c("Cancelar", "Confirmar"),
                        btn_colors = c(.RACAFE_ROOT$red_hover, "#1F7A55"), html = TRUE, width = "420px")
    })
    
    observeEvent(input$confirm_hijo, {
      req(isTRUE(input$confirm_hijo), es_admin())
      id_val <- identidad_activa()
      cuenta_hija <- input$frm_CuentaHija
      convertir <- isTRUE(input$frm_ConvertirPrincipal)
      nuevo_padre <- if (convertir) cuenta_hija else trimws(input$frm_CuentaPadreNueva)
      
      tryCatch({
        crm_padre_destino <- CargarDatos("CRMNALCLIENTE") %>%
          dplyr::mutate(FecProceso = as_datetime(FecProceso)) %>%
          dplyr::filter(CliNitPpal == as.numeric(nuevo_padre)) %>%
          dplyr::arrange(dplyr::desc(FecProceso)) %>%
          dplyr::slice(1)
        
        payload <- data.frame(
          FecProceso = Sys.time(), Usr = usr(), LinNegCod = id_val$linneg_cod,
          CLCliNit = as.numeric(cuenta_hija), CliNitPpal = as.numeric(nuevo_padre),
          Segmento = if (nrow(crm_padre_destino) > 0) crm_padre_destino$Segmento[1] else NA_character_,
          SSPpto = if (nrow(crm_padre_destino) > 0) crm_padre_destino$SSPpto[1] else 0,
          MNFCCPpto = if (nrow(crm_padre_destino) > 0) crm_padre_destino$MNFCCPpto[1] else 0,
          Asesor = if (nrow(crm_padre_destino) > 0) crm_padre_destino$Asesor[1] else NA_character_,
          NumMesesRecuperar = if (nrow(crm_padre_destino) > 0) {
            crm_padre_destino$NumMesesRecuperar[1]
          } else 3L,
          Excluir = if (nrow(crm_padre_destino) > 0) crm_padre_destino$Excluir[1] else "NO",
          stringsAsFactors = FALSE
        )
        racafe::AgregarDatos(payload, "CRMNALCLIENTE")
        showNotification("Jerarquía actualizada exitosamente.", type = "message")
        identidad_activa(list(nit = as.numeric(nuevo_padre), linneg_cod = id_val$linneg_cod))
        refresh(refresh() + 1L)
      }, error = function(e) {
        showNotification(paste("Error al guardar jerarquía:", e$message),
                         type = "error", duration = NULL)
      })
    })
    
    data_crm <- reactive({
      id_val <- identidad_activa()
      req(!is.null(id_val), length(id_val$nit) == 1L, !is.na(id_val$nit))
      refresh()
      CargarDatos("CRMNALCLIENTE") %>%
        mutate(FecProceso = as_datetime(FecProceso)) %>%
        filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod) %>%
        arrange(desc(FecProceso)) %>%
        slice(1)
    })
    
    observe({
      crm <- data_crm(); id_val <- identidad_activa(); req(!is.null(id_val))
      d <- dat()
      asesor_val <- if (nrow(crm) > 0) crm$Asesor[1] %||% "" else d$Asesor[1] %||% ""
      seg_val <- if (nrow(crm) > 0) crm$Segmento[1] %||% "" else d$Segmento[1] %||% ""
      exc_val <- if (nrow(crm) > 0) crm$Excluir[1] %||% "NO" else "NO"
      meses_val <- if (nrow(crm) > 0) crm$NumMesesRecuperar[1] %||% 3L else 3L
      
      updatePickerInput(session, "frm_Asesor", selected = asesor_val)
      updatePickerInput(session, "frm_Segmento", selected = seg_val)
      updatePickerInput(session, "frm_Excluir", selected = exc_val)
      updateAutonumericInput(session, "frm_NumMesesRecuperar", value = meses_val)
    })
    
    construir_payload <- function() {
      id_val <- identidad_activa()
      crm <- data_crm()
      ssppto <- if (nrow(crm) > 0) as.numeric(crm$SSPpto[1] %||% 0) else 0
      mnfccpto <- if (nrow(crm) > 0) as.numeric(crm$MNFCCPpto[1] %||% 0) else 0
      
      data.frame(
        FecProceso = Sys.time(), Usr = usr(), LinNegCod = id_val$linneg_cod,
        CLCliNit = id_val$nit, CliNitPpal = id_val$nit,
        Segmento = input$frm_Segmento %||% NA_character_, SSPpto = ssppto, MNFCCPpto = mnfccpto,
        Asesor = input$frm_Asesor %||% NA_character_,
        NumMesesRecuperar = input$frm_NumMesesRecuperar %||% 3L,
        Excluir = input$frm_Excluir %||% "NO", stringsAsFactors = FALSE
      )
    }
    
    observeEvent(input$btn_guardar, {
      req(es_admin())
      confirmSweetAlert(session = session, inputId = ns("confirm_guardar"),
                        title = "Confirmar guardado",
                        text = "¿Desea guardar los datos comerciales del cliente?",
                        type = "warning", btn_labels = c("Cancelar", "Guardar"),
                        btn_colors = c(.RACAFE_ROOT$red_hover, "#1F7A55"), html = TRUE, width = "400px")
    })
    
    observeEvent(input$confirm_guardar, {
      req(isTRUE(input$confirm_guardar), es_admin())
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

# IndNotas -----------------------------------------------------------------
# Historial de notas CRM del cliente (tabla CRMNALNOTAS)

IndNotasUI <- function(id) {
  ns <- NS(id)
  box(title = tagList(icon("sticky-note"), " Notas CRM"), width = 12, status = "white",
      collapsible = TRUE, collapsed = TRUE,
      fluidRow(
        column(10, textAreaInput(ns("nota_texto"), label = NULL, width = "100%", rows = 2,
                                 placeholder = "Ingrese nota de seguimiento...")),
        column(2, div(style = "padding-top: 0px;",
                      actionButton(ns("btn_nota"), "Agregar", icon = icon("plus"),
                                   class = "btn-danger", width = "100%")))
      ),
      br(), reactableOutput(ns("tbl_notas")))
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
        tibble::tibble(FechaHoraCrea = as.POSIXct(character()), Usuario = character(),
                       Nota = character())
      })
    })
    
    observeEvent(input$btn_nota, {
      req(nzchar(trimws(input$nota_texto %||% "")))
      id_val <- identidad()
      nuevo <- data.frame(FechaHoraCrea = as.character(Sys.time()), Usuario = usr(),
                          LinNegCod = id_val$linneg_cod, CliNitPpal = id_val$nit,
                          Nota = trimws(input$nota_texto), stringsAsFactors = FALSE)
      tryCatch({
        racafe::AgregarDatos(nuevo, "CRMNALNOTAS")
        updateTextAreaInput(session, "nota_texto", value = "")
        refresh_notas(refresh_notas() + 1L)
        showNotification("Nota registrada.", type = "message")
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    })
    
    output$tbl_notas <- renderReactable({
      df <- data_notas()
      if (nrow(df) == 0) {
        return(reactable::reactable(data.frame(Mensaje = "Sin notas registradas."), compact = TRUE))
      }
      reactable::reactable(df, sortable = TRUE, searchable = TRUE, compact = TRUE, bordered = TRUE,
                           highlight = TRUE, defaultPageSize = 10,
                           columns = list(
                             FechaHoraCrea = reactable::colDef(name = "Fecha/Hora", minWidth = 140,
                                                               cell = function(v) format(as.POSIXct(v), "%d/%m/%Y %H:%M")),
                             Usuario = reactable::colDef(name = "Usuario", minWidth = 100),
                             LinNegCod = reactable::colDef(show = FALSE),
                             CliNitPpal = reactable::colDef(show = FALSE),
                             Nota = reactable::colDef(name = "Nota", minWidth = 300)
                           ))
    })
  })
}

# IndOportunidades -----------------------------------------------------
# Pipeline de oportunidades y leads (tabla CRMNALCLOPT)

IndOportunidadesUI <- function(id) {
  ns <- NS(id)
  box(title = tagList(icon("handshake"), " Oportunidades"), width = 12, status = "white",
      collapsible = TRUE, collapsed = TRUE, reactableOutput(ns("tbl_oport")))
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
      }, error = function(e) data.frame(Mensaje = "Sin oportunidades registradas o tabla no disponible."))
      reactable::reactable(df, sortable = TRUE, compact = TRUE, highlight = TRUE, bordered = TRUE,
                           defaultPageSize = 10)
    })
  })
}

# Individual (orquestador) -----------------------------------------------
# Orquesta los submódulos activos. Usa PresupuestoIndividual (definido en
# PresupuestoIndividual.R) en vez del módulo de portafolio completo.
# Estacionalidad y Benchmark retirados por decisión de producto.

IndividualUI <- function(id) {
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(), uiOutput(ns("wrapper")))
}
Individual <- function(id, dat, usr, clientes_raw = reactive(NULL),
                       dat_global = reactive(NULL), ncliente = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    identidad <- reactive({
      req(nrow(dat()) > 0)
      df <- dat()
      hijos <- df %>% dplyr::distinct(CLCliNit, RazonSocialCliNit) %>% dplyr::filter(!is.na(CLCliNit))
      list(nit = df$CliNitPpal[1], linneg_cod = df$LinNegCod[1], razon_soc = Unicos(df$PerRazSoc),
           linneg_nom = Unicos(df$CLLinNegNo),
           asesor = paste(unique(na.omit(df$Asesor)), collapse = " / "),
           segmento = paste(unique(na.omit(df$Segmento)), collapse = " / "),
           tipo_cli = paste(unique(na.omit(df$SegmentoRacafe)), collapse = " / "), hijos = hijos)
    })
    
    color_tipo_cli <- function(tipo) {
      t <- toupper(tipo %||% "")
      if (grepl("RECUPERAR", t)) .RACAFE_ROOT$red_secondary
      else if (grepl("NUEVO", t)) .RACAFE_WEB$dorado_suave
      else if (grepl("CLIENTE", t)) .RACAFE_WEB$cafe_corporativo
      else .RACAFE_ROOT$gray_700
    }
    
    tarjeta_identidad <- function(id_val) {
      tags$div(
        style = paste0("background:var(--color-gray-100); border:1px solid var(--color-gray-400);",
                       "border-radius:6px; padding:10px 14px; margin-bottom:4px;"),
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:flex-start;
                    flex-wrap:wrap; gap:8px;",
          tags$div(
            style = "display:flex; align-items:baseline; gap:8px; flex-wrap:wrap;",
            tags$span(id_val$razon_soc,
                      style = "font-size:var(--font-size-lg); font-weight:700;
                                color:var(--color-gray-950);"),
            tags$span(paste0("NIT ", fmt_co(id_val$nit)),
                      style = "font-size:var(--font-size-xs); font-weight:600; color:white;
                                background:var(--color-red-primary); padding:1px 6px;
                                border-radius:4px;"),
            tags$span(paste0("Línea: ", id_val$linneg_nom),
                      style = "font-size:var(--font-size-sm); color:var(--color-gray-800);")
          ),
          tags$div(
            style = "display:flex; gap:6px; flex-wrap:wrap; justify-content:flex-end;",
            tags$span(id_val$asesor,
                      style = "font-size:var(--font-size-xs); font-weight:600;
                                color:var(--color-gray-920); background:white;
                                border:1px solid var(--color-gray-400); padding:2px 8px;
                                border-radius:10px;"),
            tags$span(id_val$segmento,
                      style = "font-size:var(--font-size-xs); font-weight:600;
                                color:var(--color-gray-920); background:white;
                                border:1px solid var(--color-gray-400); padding:2px 8px;
                                border-radius:10px;"),
            tags$span(id_val$tipo_cli,
                      style = paste0("font-size:var(--font-size-xs); font-weight:600; color:white;",
                                     "background:", color_tipo_cli(id_val$tipo_cli), ";",
                                     "padding:2px 8px; border-radius:10px;"))
          )
        ),
        if (nrow(id_val$hijos) > 0) {
          tags$div(
            style = "margin-top:6px; display:flex; flex-wrap:wrap; align-items:center; gap:6px;",
            tags$span("Cuentas:",
                      style = "font-size:var(--font-size-xs); font-weight:700;
                                color:var(--color-gray-700); text-transform:uppercase;
                                letter-spacing:.03em;"),
            lapply(seq_len(nrow(id_val$hijos)), function(i) {
              h <- id_val$hijos[i, ]
              tags$span(paste0(h$RazonSocialCliNit %||% "\u2014", " (", fmt_co(h$CLCliNit), ")"),
                        style = "font-size:var(--font-size-xs); color:var(--color-gray-920);
                                 background:white; border:1px solid var(--color-gray-400);
                                 padding:1px 7px; border-radius:10px;")
            })
          )
        }
      )
    }
    
    kpi_hist_r <- reactive({
      df <- dat()
      list(sacos = sum(df$SacFact70, na.rm = TRUE), margen = sum(df$Margen, na.rm = TRUE),
           kilos = sum(df$Kilos, na.rm = TRUE), d_min = min(df$FecFact, na.rm = TRUE),
           d_max = max(df$FecFact, na.rm = TRUE), n_lotes = n_distinct(df$CLLotCod))
    })
    kpi_ytd_r <- reactive({
      ano <- year(Sys.Date())
      df <- dat() %>% filter(!is.na(FecFact), year(FecFact) == ano)
      list(sacos = sum(df$SacFact70, na.rm = TRUE), margen = sum(df$Margen, na.rm = TRUE),
           n_lotes = n_distinct(df$CLLotCod))
    })
    kpi_mtd_r <- reactive({
      ano <- year(Sys.Date()); mes <- month(Sys.Date())
      df <- dat() %>% filter(!is.na(FecFact), year(FecFact) == ano, month(FecFact) == mes)
      list(sacos = sum(df$SacFact70, na.rm = TRUE), margen = sum(df$Margen, na.rm = TRUE),
           n_lotes = n_distinct(df$CLLotCod))
    })
    kpi_ult_fact_r <- reactive({
      d <- kpi_hist_r()$d_max
      list(fecha = d, dias = as.numeric(difftime(Sys.Date(), d, units = "days")))
    })
    
    detalle_lotes_r <- reactive({
      dat() %>%
        filter(!is.na(FecFact)) %>%
        group_by(CLLotCod, Categoria, Producto) %>%
        summarise(FecFact = max(FecFact, na.rm = TRUE), Sacos = sum(SacFact70, na.rm = TRUE),
                  Margen = sum(Margen, na.rm = TRUE), .groups = "drop")
    })
    
    cols_detalle <- list(
      CLLotCod = reactable::colDef(name = "Lote", minWidth = 90),
      Categoria = reactable::colDef(name = "Categoría", minWidth = 110),
      Producto = reactable::colDef(name = "Producto", minWidth = 130),
      FecFact = reactable::colDef(name = "Fecha Fact.", minWidth = 100,
                                  cell = function(v) format(v, "%d/%m/%Y")),
      Sacos = reactable::colDef(name = "Sacos", minWidth = 90, cell = function(v) fmt_co(v)),
      Margen = reactable::colDef(name = "Margen", minWidth = 110,
                                 cell = function(v) paste0("$", fmt_co(v)))
    )
    output$tbl_detalle_sac <- reactable::renderReactable({
      reactable::reactable(detalle_lotes_r() %>% arrange(desc(Sacos)), sortable = TRUE,
                           searchable = TRUE, compact = TRUE, bordered = TRUE, highlight = TRUE,
                           defaultPageSize = 10, columns = cols_detalle)
    })
    output$tbl_detalle_mar <- reactable::renderReactable({
      reactable::reactable(detalle_lotes_r() %>% arrange(desc(Margen)), sortable = TRUE,
                           searchable = TRUE, compact = TRUE, bordered = TRUE, highlight = TRUE,
                           defaultPageSize = 10, columns = cols_detalle)
    })
    
    output$dl_detalle_sac <- downloadHandler(
      filename = function() paste0("Detalle_Sacos_", identidad()$nit, "_", Sys.Date(), ".xlsx"),
      content = function(file) {
        tryCatch(openxlsx2::write_xlsx(detalle_lotes_r() %>% arrange(desc(Sacos)), file),
                 error = function(e) showNotification(paste("Error al generar descarga:", e$message),
                                                      type = "error"))
      })
    output$dl_detalle_mar <- downloadHandler(
      filename = function() paste0("Detalle_Margen_", identidad()$nit, "_", Sys.Date(), ".xlsx"),
      content = function(file) {
        tryCatch(openxlsx2::write_xlsx(detalle_lotes_r() %>% arrange(desc(Margen)), file),
                 error = function(e) showNotification(paste("Error al generar descarga:", e$message),
                                                      type = "error"))
      })
    
    contenido_modal_sac <- function() {
      tagList(div(style = "text-align:right; margin-bottom:8px;",
                  downloadButton(ns("dl_detalle_sac"), "Descargar Excel", class = "btn-danger")),
              reactable::reactableOutput(ns("tbl_detalle_sac")))
    }
    contenido_modal_mar <- function() {
      tagList(div(style = "text-align:right; margin-bottom:8px;",
                  downloadButton(ns("dl_detalle_mar"), "Descargar Excel", class = "btn-danger")),
              reactable::reactableOutput(ns("tbl_detalle_mar")))
    }
    
    racafeModulos::CajaModal("kpi_sac_hist", valor = reactive(kpi_hist_r()$sacos), formato = "coma",
                             texto = "Sacos Facturados (Histórico)", icono = "box-open", colores = c(fondo = "white"),
                             mostrar_boton = TRUE, contenido_modal = contenido_modal_sac,
                             titulo_modal = "Detalle de Lotes — Sacos Facturados", icono_modal = "box-open",
                             tamano_modal = "xl",
                             footer = reactive(paste0("Historial completo desde ", format(kpi_hist_r()$d_min, "%Y"),
                                                      " — ", kpi_hist_r()$n_lotes, " lotes facturados")))
    racafeModulos::CajaModal("kpi_mar_hist", valor = reactive(kpi_hist_r()$margen), formato = "dinero",
                             texto = "Margen Histórico", icono = "dollar-sign", colores = c(fondo = "white"),
                             mostrar_boton = TRUE, contenido_modal = contenido_modal_mar,
                             titulo_modal = "Detalle de Lotes — Margen", icono_modal = "dollar-sign", tamano_modal = "xl",
                             footer = reactive(paste0("Margen promedio de $",
                                                      fmt_co(kpi_hist_r()$margen / pmax(kpi_hist_r()$kilos, 1)),
                                                      " por kilo facturado")))
    racafeModulos::CajaModal("kpi_anos_cli",
                             valor = reactive({
                               anos <- as.numeric(difftime(kpi_hist_r()$d_max, kpi_hist_r()$d_min, units = "days")) / 365.25
                               round(max(anos, 0), 1)
                             }),
                             formato = "numero", texto = "Años como Cliente", icono = "calendar",
                             colores = c(fondo = "white"), mostrar_boton = FALSE,
                             footer = reactive(paste0("Cliente activo desde ", format(kpi_hist_r()$d_min, "%b %Y"),
                                                      " hasta ", format(kpi_hist_r()$d_max, "%b %Y"))))
    racafeModulos::CajaModal("kpi_ult_fact", valor = reactive(kpi_ult_fact_r()$dias),
                             formato = "entero", texto = "Días desde Última Factura", icono = "calendar-check",
                             colores = c(fondo = "white"), mostrar_boton = FALSE,
                             footer = reactive(paste0("Última factura: ", format(kpi_ult_fact_r()$fecha, "%d %b %Y"))))
    
    racafeModulos::CajaModal("kpi_sac_ytd", valor = reactive(kpi_ytd_r()$sacos), formato = "coma",
                             texto = "Sacos — Acumulado Anual", icono = "chart-line", colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive(paste0("Del 1 ene al ", format(Sys.Date(), "%d %b %Y"), " · ",
                                                      kpi_ytd_r()$n_lotes, " lotes")))
    racafeModulos::CajaModal("kpi_mar_ytd", valor = reactive(kpi_ytd_r()$margen), formato = "dinero",
                             texto = "Margen — Acumulado Anual", icono = "coins", colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive(paste0("Del 1 ene al ", format(Sys.Date(), "%d %b %Y"))))
    racafeModulos::CajaModal("kpi_sac_mtd", valor = reactive(kpi_mtd_r()$sacos), formato = "coma",
                             texto = "Sacos — Acumulado Mes Vigente", icono = "boxes", colores = c(fondo = "white"),
                             mostrar_boton = FALSE,
                             footer = reactive(paste0("Acumulado de ", format(Sys.Date(), "%B %Y"), " · ",
                                                      kpi_mtd_r()$n_lotes, " lotes")))
    racafeModulos::CajaModal("kpi_mar_mtd", valor = reactive(kpi_mtd_r()$margen), formato = "dinero",
                             texto = "Margen — Acumulado Mes Vigente", icono = "hand-holding-dollar",
                             colores = c(fondo = "white"), mostrar_boton = FALSE,
                             footer = reactive(paste0("Acumulado de ", format(Sys.Date(), "%B %Y"))))
    
    output$wrapper <- renderUI({
      if (nrow(dat()) == 0) {
        return(div(
          style = "margin:20px; padding:12px 16px; background:var(--color-gray-200);
                    color:var(--color-red-primary); border-radius:6px; font-weight:600;",
          icon("exclamation-triangle"), " ",
          "No existe información para el cliente seleccionado en la línea de negocio seleccionada."
        ))
      }
      id_val <- identidad()
      
      tagList(
        Saltos(),
        fluidRow(column(12, tarjeta_identidad(id_val))),
        Saltos(1),
        fluidRow(
          column(3, racafeModulos::CajaModalUI(ns("kpi_sac_hist"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_mar_hist"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_anos_cli"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_ult_fact")))
        ),
        Saltos(1),
        fluidRow(
          column(3, racafeModulos::CajaModalUI(ns("kpi_sac_ytd"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_mar_ytd"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_sac_mtd"))),
          column(3, racafeModulos::CajaModalUI(ns("kpi_mar_mtd")))
        ),
        Saltos(1),
        fluidRow(
          column(8, IndContactoUI(ns("contacto")), Saltos(1)),
          column(4, IndFormularioUI(ns("formulario")))
        ),
        Saltos(1),
        fluidRow(column(12, IndPendientesUI(ns("pendientes")))),
        Saltos(1),
        fluidRow(
          column(7, IndHistoricoUI(ns("historico"))),
          column(5, IndParticipacionUI(ns("participacion")))
        ),
        Saltos(1),
        box(title = tagList(icon("bullseye"), " Presupuesto vs Ejecución"), width = 12,
            status = "white", collapsible = TRUE, collapsed = TRUE,
            PresupuestoIndividualUI(ns("presupuesto"))),
        Saltos(1),
        IndRFMUI(ns("rfm")),
        Saltos(1),
        IndNotasUI(ns("notas")),
        Saltos(1),
        IndOportunidadesUI(ns("oportunidades"))
      )
    })
    
    IndContacto("contacto", dat, ncliente)
    IndHistorico("historico", dat, identidad)
    IndParticipacion("participacion", dat)
    IndPendientes("pendientes", dat)
    IndRFM("rfm", dat)
    IndFormulario("formulario", identidad, dat, usr, ncliente)
    IndNotas("notas", identidad, usr)
    IndOportunidades("oportunidades", identidad)
    PresupuestoIndividual("presupuesto", dat, identidad)
  })
}

# App de prueba -----------------------------------------------------------
# Remover en producción

if (TRUE) {
  ui <- bs4DashPage(
    title = "Prueba — Módulo Individual", header = bs4DashNavbar(),
    sidebar = bs4DashSidebar(disable = TRUE),
    body = bs4DashBody(useShinyjs(),
                       includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/",
                                         "Compartido/refs/heads/main/Styles/style.css")),
                       IndividualUI("ind"))
  )
  
  server <- function(input, output, session) {
    clientes_raw_r <- reactive({
      local_geo <- CargarDatos("CRMNALLOCAL") %>%
        mutate(FecProceso = as.Date(FecProceso)) %>%
        group_by(CliNitPpal) %>%
        filter(FecProceso == max(FecProceso)) %>%
        ungroup()
      CargarDatos("CRMNALCLIENTE") %>%
        mutate(FecProceso = as.Date(FecProceso),
               across(where(is.numeric), ~ ifelse(is.na(.), 0, .)),
               across(where(is.character), ~ ifelse(is.na(.) | . == "N/A", "", .))) %>%
        left_join(local_geo, by = join_by(FecProceso, Usr, CliNitPpal)) %>%
        mutate(LinNegocio = ifelse(LinNegCod == 10000, "CONVENCIONALES", "A LA MEDIDA"))
    })
    
    Individual("ind", dat = reactive(BaseDatos_i), usr = reactive("HCYATE"),
               clientes_raw = clientes_raw_r, dat_global = reactive(BaseDatos_c),
               ncliente = reactive(NCLIENTE))
  }
  
  shinyApp(ui, server)
}
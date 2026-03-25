# Helpers Oportunidades ----

# Peso en kilos por saco segun linea de negocio
peso_saco_linneg <- function(linneg) {
  dplyr::case_when(
    linneg == "CONVENCIONALES" ~ 62.5,
    linneg == "A LA MEDIDA"    ~ 70,
    TRUE                       ~ NA_real_
  )
}

# Calcula pendiente de regresion lineal del acumulado proyectada a 30 dias
calc_pendiente_30 <- function(fac, y_col) {
  df <- fac %>% dplyr::filter(!is.na(Dias), !is.na(.data[[y_col]]))
  if (nrow(df) < 2 || length(unique(df$Dias)) < 2) return(NA_real_)
  coef(stats::lm(df[[y_col]] ~ df$Dias))[2] * 30
}

# Factory: grafico de lineas acumuladas vs meta horizontal
.plot_lineas_op <- function(fac, y_col, meta_val, fmt, titulo, ytitle) {
  plotly::plot_ly() %>%
    plotly::add_lines(
      data = fac, x = ~FecFact, y = fac[[y_col]], mode = "lines+markers",
      name = "Facturado (acum)",
      hovertemplate = paste0("%{x|%b %Y}<br>%{y:", fmt, "}<extra></extra>")
    ) %>%
    plotly::add_lines(
      x = c(min(fac$FecFact), max(fac$FecFact)), y = c(meta_val, meta_val),
      name = "Meta",
      hovertemplate = paste0("%{x|%b %Y}<br>%{y:", fmt, "}<extra></extra>")
    ) %>%
    plotly::layout(
      title  = titulo,
      xaxis  = list(title = "Fecha de facturacion", tickformat = "%b %Y"),
      yaxis  = list(title = ytitle, tickformat = fmt),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
      hovermode = "x unified"
    ) %>%
    plotly::config(displayModeBar = FALSE, locale = "es")
}

# Factory: grafico de barras periodicas vs meta estandarizada a 30 dias
.plot_barras_op <- function(fac, y_col, plan_col, fmt, titulo, ytitle) {
  plan_val <- fac[[plan_col]][1]
  plotly::plot_ly() %>%
    plotly::add_bars(
      data = fac, x = ~FecFact, y = fac[[y_col]],
      name = "Equivalente 30 dias",
      hovertemplate = paste0("%{x|%b %Y}<br>%{y:", fmt, "}<extra></extra>")
    ) %>%
    plotly::add_lines(
      x = c(min(fac$FecFact), max(fac$FecFact)), y = c(plan_val, plan_val),
      name = "Meta 30 dias",
      hovertemplate = paste0("Meta 30 dias<br>%{y:", fmt, "}<extra></extra>")
    ) %>%
    plotly::layout(
      title  = titulo,
      xaxis  = list(title = "Mes de facturacion", tickformat = "%b %Y"),
      yaxis  = list(title = ytitle, tickformat = fmt),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
      hovermode = "x unified", barmode = "group"
    ) %>%
    plotly::config(displayModeBar = FALSE, locale = "es")
}

# Factory: tabla GT de resumen economico agrupada por dimension
.tabla_dim_op <- function(df, group_col) {
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) %>%
    dplyr::summarise(
      Sacos  = sum(SacosOP,            na.rm = TRUE),
      Margen = sum(SacosOP * MargenOP, na.rm = TRUE),
      Score  = mean(ScoreOP,           na.rm = TRUE),
      .groups = "drop"
    ) %>%
    gt::gt() %>%
    gt::fmt_currency(columns = Margen, currency = "COP", decimals = 0) %>%
    gt::fmt_number(columns = Score, decimals = 1) %>%
    gt_minimal_style() %>%
    gt::opt_interactive(use_pagination = FALSE, use_filters = FALSE, use_resizers = TRUE)
}

# Definicion de columnas reactable para tabla de facturacion
.cols_fact_op <- list(
  FecFact = reactable::colDef(
    name = "Fecha de facturacion", minWidth = 130
  ),
  SacFact = reactable::colDef(
    name = "Sacos facturados", minWidth = 110,
    format = reactable::colFormat(separators = TRUE, digits = 0)
  ),
  CumSac  = reactable::colDef(
    name = "Sacos acumulados", minWidth = 110,
    format = reactable::colFormat(separators = TRUE, digits = 0)
  ),
  Margen  = reactable::colDef(
    name = "Margen facturado", minWidth = 130,
    format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
  ),
  CumMarg = reactable::colDef(
    name = "Margen acumulado", minWidth = 130,
    format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
  )
)


# Modulo FormularioOportunidad ----
FormularioOportunidadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             box(
               title = "Identificacion", width = 12, status = "white",
               solidHeader = TRUE, collapsible = TRUE,
               fluidRow(
                 column(6,
                        ListaDesplegable(ns("OP_TipoCliente"), label = Obligatorio("Tipo de Cliente"),
                                         choices = c("", "CLIENTE", "CLIENTE A RECUPERAR", "LEAD"),
                                         selected = "", multiple = FALSE)
                 ),
                 column(6,
                        ListaDesplegable(ns("OP_Cliente"), label = Obligatorio("Cliente"), ns = ns,
                                         choices = c(""), multiple = FALSE, fem = FALSE)
                 )
               ),
               fluidRow(
                 column(6,
                        ListaDesplegable(ns("OP_LinNeg"), label = Obligatorio("Linea de Negocio"),
                                         ns = ns, choices = c("", "A LA MEDIDA", "CONVENCIONALES"),
                                         selected = "", multiple = FALSE)
                 ),
                 column(6,
                        ListaDesplegable(ns("OP_Segmento"), label = Obligatorio("Segmento"), ns = ns,
                                         choices = c("", "DETAL", "MEDIANO", "GRANDES"), selected = "", multiple = FALSE)
                 )
               )
             )
      )
    ),
    fluidRow(
      column(12,
             box(
               title = "Informacion del Producto", width = 12, status = "white",
               solidHeader = TRUE, collapsible = TRUE,
               fluidRow(
                 column(6,
                        ListaDesplegable(ns("OP_Categoria"), label = Obligatorio("Categoria"), ns = ns,
                                         choices = "", selected = NULL, multiple = FALSE)
                 ),
                 column(6,
                        ListaDesplegable(ns("OP_Producto"), label = Obligatorio("Producto"), ns = ns,
                                         choices = "", selected = NULL, multiple = FALSE)
                 )
               )
             )
      )
    ),
    fluidRow(
      column(12,
             box(
               title = "Detalles de la Oportunidad", width = 12, status = "white",
               solidHeader = TRUE, collapsible = TRUE,
               fluidRow(
                 column(6,
                        dateInput(ns("OP_Fecha"), label = Obligatorio("Fecha de Cumplimiento"),
                                  value = Sys.Date() + 7, min = Sys.Date(), language = "es", width = "100%")
                 ),
                 column(6,
                        uiOutput(ns("OP_Sacos_UI")),
                        InputNumerico(ns("OP_Frecuencia"), type = "numero",
                                      label = Obligatorio("Frecuencia (dias)"), value = NA, dec = 0),
                        InputNumerico(ns("OP_Margen"), type = "dinero",
                                      label = Obligatorio("Margen por Kilo"), value = NA, dec = 0),
                        br(),
                        uiOutput(ns("OP_Resumen"))
                 )
               )
             )
      )
    ),
    fluidRow(
      column(12,
             fluidRow(
               column(10),
               column(2,
                      actionBttn(inputId = ns("OP_Crear"), label = "Crear Oportunidad",
                                 style = "unite", color = "danger", size = "xs",
                                 icon = icon("save"), block = TRUE)
               )
             )
      )
    )
  )
}
FormularioOportunidad <- function(id, dd_data = reactive(NULL), dat, usr,
                                  trigger_update, tipo_cliente_default = reactive("CLIENTE")) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Inicializa tipo de cliente segun parametro externo ----
    observeEvent(dd_data(), {
      tipo_valido <- if (tipo_cliente_default() %in% c("CLIENTE", "CLIENTE A RECUPERAR", "LEAD")) {
        tipo_cliente_default()
      } else {
        ""
      }
      updatePickerInput(session, inputId = "OP_TipoCliente", selected = tipo_valido)
    })
    
    # Actualiza lista de clientes segun tipo seleccionado ----
    observeEvent(input$OP_TipoCliente, {
      req(input$OP_TipoCliente)
      
      # CLIENTE y CLIENTE A RECUPERAR siguen el mismo patron; LEAD carga tabla aparte
      clientes <- if (is.null(dd_data())) {
        if (input$OP_TipoCliente == "LEAD") {
          CargarDatos("CRMNALLEAD") %>%
            dplyr::filter(!is.na(PerRazSoc)) %>%
            dplyr::pull(PerRazSoc) %>%
            Unicos()
        } else {
          dat() %>%
            dplyr::filter(SegmentoRacafe == input$OP_TipoCliente) %>%
            dplyr::pull(PerRazSoc) %>%
            Unicos()
        }
      } else {
        extraer_razon_social(dd_data()$fila_completa$PerRazSoc)
      }
      
      updatePickerInput(session, "OP_Cliente",
                        choices  = c("", clientes),
                        selected = if (length(clientes) == 1) clientes else "")
    })
    
    # Label dinamico de sacos segun linea de negocio ----
    output$OP_Sacos_UI <- renderUI({
      label_saco <- if (!is.null(input$OP_LinNeg) && input$OP_LinNeg != "") {
        peso <- peso_saco_linneg(input$OP_LinNeg)
        if (!is.na(peso)) paste0("Sacos (", peso, " kgs)") else "Sacos"
      } else {
        "Sacos"
      }
      InputNumerico(ns("OP_Sacos"), type = "numero", label = Obligatorio(label_saco),
                    value = NA, dec = 2)
    })
    
    # Actualiza categorias segun linea de negocio ----
    observeEvent(input$OP_LinNeg, {
      req(input$OP_LinNeg)
      cho_cat <- c("",
                   dat() %>%
                     dplyr::filter(CLLinNegNo == input$OP_LinNeg) %>%
                     dplyr::pull(Categoria) %>%
                     Unicos())
      updatePickerInput(session, "OP_Categoria", choices = cho_cat)
    })
    
    # Actualiza productos segun categoria ----
    observeEvent(input$OP_Categoria, {
      req(input$OP_Categoria)
      cho_prod <- c("",
                    dat() %>%
                      dplyr::filter(Categoria == input$OP_Categoria) %>%
                      dplyr::pull(Producto) %>%
                      Unicos())
      updatePickerInput(session, "OP_Producto", choices = cho_prod)
    })
    
    # Actualiza segmento segun cliente y linea de negocio ----
    observeEvent(c(input$OP_Cliente, input$OP_LinNeg), {
      req(input$OP_Cliente, input$OP_LinNeg)
      seg <- dat() %>%
        dplyr::filter(PerRazSoc == input$OP_Cliente, CLLinNegNo == input$OP_LinNeg) %>%
        dplyr::pull(Segmento) %>%
        Unicos()
      updatePickerInput(session, "OP_Segmento", selected = seg)
    })
    
    # Resumen calculado de la oportunidad ----
    output$OP_Resumen <- renderUI({
      req(input$OP_Sacos, input$OP_LinNeg, input$OP_Frecuencia, input$OP_Margen)
      if (is.na(input$OP_Sacos) || is.na(input$OP_Frecuencia) || is.na(input$OP_Margen) ||
          input$OP_Sacos == "" || input$OP_Frecuencia == "" || input$OP_Margen == "") {
        return(NULL)
      }
      
      # Calculo centralizado de peso por saco
      peso_saco <- peso_saco_linneg(input$OP_LinNeg)
      if (is.na(peso_saco)) return(NULL)
      
      sacos         <- as.numeric(input$OP_Sacos)
      frecuencia    <- as.numeric(input$OP_Frecuencia)
      margen_kg     <- as.numeric(input$OP_Margen)
      sacos_mes     <- sacos / frecuencia * 30
      margen_saco   <- margen_kg * peso_saco
      margen_total  <- margen_saco * sacos
      margen_mes    <- margen_total / frecuencia * 30
      
      resumen_text <- paste0(
        "Oportunidad por ",
        FormatearNumero(sacos, formato = "coma", negrita = TRUE),
        " sacos de ",
        FormatearNumero(peso_saco, formato = "numero", negrita = TRUE),
        " kgs cada ",
        FormatearNumero(frecuencia, formato = "coma", negrita = TRUE),
        " dias, es decir, ",
        FormatearNumero(sacos_mes, formato = "numero", negrita = TRUE),
        " sacos mensuales, dejando un margen por kilo de ",
        FormatearNumero(margen_kg, formato = "dinero", negrita = TRUE),
        " lo que equivale a ",
        FormatearNumero(margen_saco, formato = "dinero", negrita = TRUE),
        " por saco y un margen total de oportunidad de ",
        FormatearNumero(margen_total, formato = "dinero", negrita = TRUE),
        " y un estimado mensual de ",
        FormatearNumero(margen_mes, formato = "dinero", negrita = TRUE)
      ) %>% HTML
      
      tags$div(
        class = "op-resumen-box",
        resumen_text
      )
    })
    
    # Crea oportunidad en BD ----
    observeEvent(input$OP_Crear, {
      cond <- c(
        "El campo Tipo de Cliente es obligatorio"  = EsVacio(input$OP_TipoCliente),
        "El campo Cliente es obligatorio"          = EsVacio(input$OP_Cliente),
        "El campo Linea de Negocio es obligatorio" = EsVacio(input$OP_LinNeg),
        "El campo Categoria es obligatorio"        = EsVacio(input$OP_Categoria),
        "El campo Segmento es obligatorio"         = EsVacio(input$OP_Segmento),
        "El campo Producto es obligatorio"         = EsVacio(input$OP_Producto),
        "El campo Sacos es obligatorio"            = EsVacio(input$OP_Sacos),
        "El campo Frecuencia es obligatorio"       = EsVacio(input$OP_Frecuencia),
        "El campo Margen por kilo es obligatorio"  = EsVacio(input$OP_Margen)
      )
      
      if (any(cond)) {
        sapply(names(cond[cond]), function(msg) {
          showNotification(msg, duration = 3, type = "error")
        })
        return()
      }
      
      # Tipo de cliente segun datos de facturacion activos
      tipo_cliente_bd <- dat() %>%
        dplyr::filter(PerRazSoc == input$OP_Cliente, CLLinNegNo == input$OP_LinNeg) %>%
        dplyr::pull(SegmentoRacafe) %>%
        Unicos()
      
      if (length(tipo_cliente_bd) == 0) tipo_cliente_bd <- NA
      
      aux1 <- data.frame(
        Usuario           = usr(),
        FechaHoraCrea     = Sys.time(),
        TipoClienteOP     = input$OP_TipoCliente,
        PerRazSoc         = input$OP_Cliente,
        LineaNegocio      = input$OP_LinNeg,
        TipoCliente       = tipo_cliente_bd,
        Segmento          = input$OP_Segmento,
        Categoria         = input$OP_Categoria,
        Producto          = input$OP_Producto,
        Oportunidad       = NA,
        FechaCumpOP       = input$OP_Fecha,
        SacosOP           = input$OP_Sacos,
        FrecuenciaDias    = input$OP_Frecuencia,
        MargenOP          = input$OP_Margen,
        Descartada        = FALSE,
        RazonDescartado   = NA,
        UsuarioDescarte   = NA,
        FechaHoraDescarte = NA
      )
      
      AgregarDatos(aux1, "CRMNALCLOPT")
      showNotification("Oportunidad registrada exitosamente", duration = 3, type = "message")
      
      inputs_reset <- c("OP_TipoCliente", "OP_Cliente", "OP_LinNeg", "OP_Categoria",
                        "OP_Segmento", "OP_Producto", "OP_Fecha", "OP_Sacos",
                        "OP_Frecuencia", "OP_Margen")
      lapply(inputs_reset, reset)
      
      # Restaura tipo de cliente por defecto tras resetear
      tipo_def <- tipo_cliente_default()
      updatePickerInput(session, inputId = "OP_TipoCliente",
                        selected = if (tipo_def %in% c("CLIENTE", "CLIENTE A RECUPERAR", "LEAD")) tipo_def else "")
      
      trigger_update(trigger_update() + 1)
    })
  })
}


# Funcion actualizar_descartado ----

# Actualiza el estado de descarte de una oportunidad en la BD
actualizar_descartado <- function(razon_social, linea_negocio, descartada, razon_descartado,
                                  usuario_descarte, descartado_precio = NA,
                                  descartado_calidad = NA, fecha_hora = Sys.time()) {
  razon_social      <- gsub("'", "''", razon_social)
  linea_negocio     <- gsub("'", "''", linea_negocio)
  razon_descartado  <- gsub("'", "''", razon_descartado)
  usuario_descarte  <- gsub("'", "''", usuario_descarte)
  descartado_calidad <- gsub("'", "''", descartado_calidad)
  
  con <- ConectarBD()
  on.exit(DBI::dbDisconnect(con))
  
  query <- glue::glue(
    "UPDATE CRMNALCLOPT
     SET  Descartada          = {descartada},
          RazonDescartado     = '{razon_descartado}',
          DescartadoPrecio    = {ifelse(is.na(descartado_precio), 'NULL', descartado_precio)},
          DescartadoCalidad   = {ifelse(is.na(descartado_calidad), 'NULL',
                                        glue::glue(\"'{descartado_calidad}'\"))},
          UsuarioDescarte     = '{usuario_descarte}',
          FechaHoraDescarte   = '{as.character(fecha_hora)}'
     WHERE PerRazSoc = '{razon_social}' AND LineaNegocio = '{linea_negocio}';"
  )
  
  DBI::dbExecute(con, query)
  invisible(TRUE)
}


# Modulo DescartarOportunidad ----
DescartarOportunidadUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "descartar-op-wrap",
        div(class = "alert alert-warning descartar-op-alerta",
            icon("exclamation-triangle"),
            strong("Advertencia: "),
            "Esta operacion no se puede deshacer."
        ),
        ListaDesplegable(
          inputId = ns("razon"), label = "Razon para descartar la oportunidad:",
          choices = Choices()$raz_descarte, selected = "", multiple = FALSE, ns = ns
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'PRECIO'", ns("razon")),
          InputNumerico(id = ns("precio"), label = "Precio ofrecido por kilo",
                        type = "dinero", value = NA, dec = 0)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'CALIDAD'", ns("razon")),
          ListaDesplegable(inputId = ns("calidad"), label = "Motivo asociado a calidad:",
                           choices = Choices()$producto, selected = NULL, multiple = FALSE, ns = ns)
        ),
        br(),
        actionButton(ns("confirmar"), "Confirmar Descarte",
                     icon = icon("ban"), class = "btn-danger", style = "width: 100%;")
    )
  )
}

DescartarOportunidad <- function(id, dd_data, usr, trigger) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$confirmar, {
      req(is.function(dd_data))
      sel <- dd_data()
      req(!is.null(sel), input$razon)
      
      # Validaciones de campos requeridos segun razon de descarte ----
      cond <- c(
        "La razon de descarte es obligatoria" =
          is.null(input$razon) || input$razon == "",
        "El precio es obligatorio cuando la razon es precio" =
          (input$razon == "PRECIO" && is.na(input$precio)),
        "El motivo de calidad es obligatorio cuando la razon es calidad" =
          (input$razon == "CALIDAD" && (is.null(input$calidad) || input$calidad == ""))
      )
      
      if (any(cond)) {
        sapply(names(cond[cond]), function(m) showNotification(m, duration = 3, type = "error"))
        return()
      }
      
      req(is.data.frame(sel$data), nrow(sel$data) >= 1)
      
      cliente <- extraer_info_cliente(sel$data$PerRazSoc %||% sel$data$Cliente)
      
      res <- try({
        actualizar_descartado(
          razon_social       = cliente$razon_social,
          linea_negocio      = cliente$linea_negocio,
          descartada         = 1,
          razon_descartado   = input$razon,
          usuario_descarte   = if (is.reactive(usr)) usr() else usr,
          descartado_precio  = if (input$razon == "PRECIO")  input$precio  else NA,
          descartado_calidad = if (input$razon == "CALIDAD") input$calidad else NA
        )
      }, silent = TRUE)
      
      if (inherits(res, "try-error")) {
        showNotification("Error al actualizar la oportunidad", type = "error")
        return()
      }
      
      if (is.reactive(trigger)) trigger(trigger() + 1)
      removeModal()
      showNotification("Oportunidad descartada exitosamente", type = "message", duration = 3)
    })
  })
}


# Helpers DetalleOportunidad ----

# Extrae fila resumen de la oportunidad (primera fila con campos clave)
extraer_oportunidad <- function(base) {
  base %>%
    dplyr::slice(1) %>%
    dplyr::select(Usuario, PerRazSoc, LineaNegocio, Segmento, Categoria, Producto,
                  FechaHoraCrea, FechaCumpOP, FrecuenciaDias, SacosOP, MargenTotalOP)
}

# Agrupa facturacion por fecha y calcula acumulados con normalizacion temporal
preparar_facturacion <- function(base, fecha_crea, frec_dias) {
  frec_dias <- ifelse(is.na(frec_dias) || frec_dias <= 0, NA_real_, frec_dias)
  
  base %>%
    dplyr::filter(!is.na(FecFact)) %>%
    dplyr::group_by(FecFact) %>%
    dplyr::summarise(
      SacFact = sum(as.numeric(SacFact), na.rm = TRUE),
      Margen  = sum(as.numeric(Margen),  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(FecFact) %>%
    dplyr::mutate(
      CumSac  = cumsum(SacFact),
      CumMarg = cumsum(Margen),
      Dias    = as.numeric(FecFact - fecha_crea),
      Tnorm   = Dias / frec_dias
    )
}

# Calcula cumplimiento total dentro del periodo de la oportunidad
calcular_cumplimiento <- function(fac, sacos_obj, margen_obj, frec_dias) {
  fac_h <- fac %>% dplyr::filter(Dias <= frec_dias)
  sac <- ifelse(nrow(fac_h) > 0, max(fac_h$CumSac,  na.rm = TRUE), 0)
  mar <- ifelse(nrow(fac_h) > 0, max(fac_h$CumMarg, na.rm = TRUE), 0)
  
  tibble::tibble(
    sacos_cumplidos = sac,
    margen_cumplido = mar,
    pct_sacos       = ifelse(sacos_obj  > 0, sac / sacos_obj,  NA_real_),
    pct_margen      = ifelse(margen_obj > 0, mar / margen_obj, NA_real_)
  )
}

# Agrega columnas de plan estandarizado a 30 dias a la tabla de facturacion
normalizar_30_dias <- function(fac, sacos_op, margen_op, frec_dias) {
  fac %>%
    dplyr::mutate(
      Dia30        = Tnorm * 30,
      PlanSacos30  = sacos_op  / frec_dias * 30,
      PlanMargen30 = margen_op / frec_dias * 30
    )
}

# Proyeccion lineal sobre grilla de puntos (usado para tendencias en graficos)
proyectar_lineal <- function(df, x, y, x_max, n = 60) {
  df <- df %>% dplyr::filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
  if (nrow(df) < 2 || length(unique(df[[x]])) < 2) return(NULL)
  
  fit  <- stats::lm(df[[y]] ~ df[[x]])
  grid <- data.frame(x = seq(min(df[[x]]), x_max, length.out = n))
  grid$y <- stats::predict(fit, newdata = grid)
  grid
}


# Modulo DetalleOportunidad ----
DetalleOportunidadUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Resumen textual de la oportunidad ----
    shiny::fluidRow(
      shiny::column(12,
                    bs4Dash::box(
                      title = "Resumen de la oportunidad",
                      width = 12, status = "white", solidHeader = TRUE,
                      shiny::uiOutput(ns("card_html"))
                    )
      )
    ),
    # Cumplimiento general acumulado ----
    shiny::fluidRow(
      shiny::column(12,
                    bs4Dash::box(
                      title = "Cumplimiento general de la oportunidad",
                      width = 12, status = "white", solidHeader = TRUE,
                      shiny::fluidRow(
                        shiny::column(6, plotly::plotlyOutput(ns("plot_sacos_op"),  height = "320px")),
                        shiny::column(6, plotly::plotlyOutput(ns("plot_margen_op"), height = "320px"))
                      )
                    )
      )
    ),
    # Estandarizacion a 30 dias ----
    shiny::fluidRow(
      shiny::column(12,
                    bs4Dash::box(
                      title = "Estandarizacion a 30 dias",
                      width = 12, status = "white", solidHeader = TRUE,
                      shiny::fluidRow(
                        shiny::column(6, plotly::plotlyOutput(ns("plot_sacos_30"),  height = "320px")),
                        shiny::column(6, plotly::plotlyOutput(ns("plot_margen_30"), height = "320px"))
                      )
                    )
      )
    ),
    # Tabla de facturacion (TablaReactable) ----
    shiny::fluidRow(
      shiny::column(12,
                    bs4Dash::box(
                      title = "Detalle de facturacion",
                      width = 12, status = "white", solidHeader = TRUE,
                      racafeModulos::TablaReactableUI(ns("tabla_fact"), titulo = NULL, sortable = TRUE)
                    )
      )
    )
  )
}

DetalleOportunidad <- function(id, dd_data, data_op_completa) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive principal: procesa datos de la oportunidad seleccionada ----
    op_data <- shiny::reactive({
      req(is.function(data_op_completa))
      dd <- dd_data()
      req(!is.null(dd), nrow(dd$fila_gt) == 1)
      
      f    <- dd$fila_gt
      base <- data_op_completa()
      
      base <- base %>%
        dplyr::filter(Categoria == f$Categoria, Producto == f$Producto) %>%
        dplyr::mutate(
          FechaHoraCrea = as.POSIXct(FechaHoraCrea),
          FechaCumpOP   = as.Date(FechaCumpOP),
          FecFact       = as.Date(FecFact)
        )
      
      req(nrow(base) > 0)
      
      op         <- extraer_oportunidad(base)
      fecha_crea <- as.Date(op$FechaHoraCrea)
      fecha_cump <- as.Date(op$FechaCumpOP)
      frec_dias  <- as.numeric(op$FrecuenciaDias)
      frec_dias  <- ifelse(is.na(frec_dias) || frec_dias <= 0,
                           as.numeric(fecha_cump - fecha_crea), frec_dias)
      
      fac <- preparar_facturacion(base, fecha_crea, frec_dias)
      
      # Pendientes de regresion calculadas una vez y reutilizadas en card y plots
      sacos_30  <- calc_pendiente_30(fac, "CumSac")
      margen_30 <- calc_pendiente_30(fac, "CumMarg")
      
      list(
        op           = op,
        fac          = fac,
        fac_30       = normalizar_30_dias(fac, op$SacosOP, op$MargenTotalOP, frec_dias),
        cumplimiento = calcular_cumplimiento(fac, op$SacosOP, op$MargenTotalOP, frec_dias),
        fecha_crea   = fecha_crea,
        fecha_cump   = fecha_cump,
        frec_dias    = frec_dias,
        sacos_30     = sacos_30,
        margen_30    = margen_30
      )
    })
    
    # Resumen HTML de la oportunidad ----
    output$card_html <- shiny::renderUI({
      x   <- op_data()
      op  <- x$op
      fac <- x$fac
      cum <- x$cumplimiento
      
      sacos_real  <- ifelse(nrow(fac) > 0, max(fac$CumSac,  na.rm = TRUE), 0)
      margen_real <- ifelse(nrow(fac) > 0, max(fac$CumMarg, na.rm = TRUE), 0)
      
      frec_dias         <- as.numeric(op$FrecuenciaDias)
      fecha_creacion    <- as.Date(op$FechaHoraCrea)
      fecha_cumplimiento <- as.Date(op$FechaCumpOP)
      dias_cumplimiento <- as.numeric(fecha_cumplimiento - fecha_creacion)
      
      # Usa pendientes ya calculadas en op_data (sin recomputer lm)
      sacos_30  <- x$sacos_30
      margen_30 <- x$margen_30
      
      sacos_mes_plan  <- as.numeric(op$SacosOP)       / frec_dias * 30
      margen_mes_plan <- as.numeric(op$MargenTotalOP) / frec_dias * 30
      pct_sacos_30    <- sacos_30  / sacos_mes_plan  * 100
      pct_margen_30   <- margen_30 / margen_mes_plan * 100
      
      shiny::HTML(paste0(
        "<div style='display:flex;gap:24px;flex-wrap:wrap;'>",
        "<div>",
        "<b>Cliente:</b> ", op$PerRazSoc, "<br>",
        "<b>Linea:</b> ", op$LineaNegocio, "<br>",
        "<b>Categoria:</b> ", op$Categoria, "<br>",
        "<b>Producto:</b> ", op$Producto, "<br>",
        "<b>Fecha de creacion:</b> ", as.character(fecha_creacion), "<br>",
        "<b>Fecha de cumplimiento:</b> ", as.character(fecha_cumplimiento), "<br>",
        "<b>Frecuencia comercial (dias):</b> ",
        FormatearNumero(frec_dias, formato = "numero"), "<br>",
        "<b>Dias para cumplir la oferta:</b> ",
        FormatearNumero(dias_cumplimiento, formato = "numero"),
        "</div>",
        "<div>",
        "<b>Meta sacos:</b> ",
        FormatearNumero(op$SacosOP, formato = "numero"), "<br>",
        "<b>Meta margen:</b> ",
        FormatearNumero(op$MargenTotalOP, formato = "dinero"), "<br><br>",
        "<b>Cumplimiento sacos (total):</b> ",
        FormatearNumero(cum$pct_sacos * 100, formato = "porcentaje"), "%<br>",
        "<b>Cumplimiento margen (total):</b> ",
        FormatearNumero(cum$pct_margen * 100, formato = "porcentaje"), "%<br><br>",
        "<b>Sacos estandarizados / 30 dias:</b> ",
        FormatearNumero(sacos_30, formato = "numero"), " (",
        FormatearNumero(pct_sacos_30, formato = "porcentaje"), "%)<br>",
        "<b>Margen estandarizado / 30 dias:</b> ",
        FormatearNumero(margen_30, formato = "dinero"), " (",
        FormatearNumero(pct_margen_30, formato = "porcentaje"), "%)",
        "</div>",
        "</div>"
      ))
    })
    
    # Plots cumplimiento total (usa factory .plot_lineas_op) ----
    output$plot_sacos_op <- plotly::renderPlotly({
      x <- op_data()
      .plot_lineas_op(x$fac, "CumSac", x$op$SacosOP, FormatoD3("numero"),
                      "Cumplimiento de sacos", "Sacos")
    })
    output$plot_margen_op <- plotly::renderPlotly({
      x <- op_data()
      .plot_lineas_op(x$fac, "CumMarg", x$op$MargenTotalOP, FormatoD3("dinero"),
                      "Cumplimiento de margen ($)", "Margen ($)")
    })
    
    # Plots estandarizados 30 dias (usa factory .plot_barras_op) ----
    output$plot_sacos_30 <- plotly::renderPlotly({
      .plot_barras_op(op_data()$fac_30, "CumSac", "PlanSacos30", FormatoD3("numero"),
                      "Estandarizacion a 30 dias - Sacos", "Sacos")
    })
    output$plot_margen_30 <- plotly::renderPlotly({
      .plot_barras_op(op_data()$fac_30, "CumMarg", "PlanMargen30", FormatoD3("dinero"),
                      "Estandarizacion a 30 dias - Margen ($)", "Margen ($)")
    })
    
    # Tabla de facturacion con TablaReactable ----
    racafeModulos::TablaReactable(
      id = "tabla_fact",
      data = shiny::reactive({
        fac <- op_data()$fac
        req(nrow(fac) > 0)
        fac %>%
          dplyr::select(FecFact, SacFact, CumSac, Margen, CumMarg) %>%
          dplyr::mutate(FecFact = as.character(as.Date(FecFact)))
      }),
      id_col         = "FecFact",
      modo_seleccion = "ninguno",
      columnas       = .cols_fact_op,
      mostrar_badge  = FALSE,
      compact        = TRUE,
      sortable       = FALSE,
      searchable     = FALSE
    )
    
    invisible(NULL)
  })
}


# Modulo TablaOportunidades ----
TablaOportunidadesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             box(
               title = "Filtros de Cumplimiento", width = 12, status = "white",
               solidHeader = TRUE, collapsible = TRUE,
               fluidRow(
                 tags$div(style = "width:20%; float:left;",
                          materialSwitch(inputId = ns("OP_CumplidoSacosTOT"),
                                         label = FormatearTexto("Cumplidas Sacos (Total)", 0.9),
                                         value = FALSE, status = "danger", width = "100%")),
                 tags$div(style = "width:20%; float:left;",
                          materialSwitch(inputId = ns("OP_CumplidoMargenTOT"),
                                         label = FormatearTexto("Cumplidas Margen (Total)", 0.9),
                                         value = FALSE, status = "danger", width = "100%")),
                 tags$div(style = "width:20%; float:left;",
                          materialSwitch(inputId = ns("OP_CumplidoSacosPER"),
                                         label = FormatearTexto("Cumplidas Sacos (Periodica)", 0.9),
                                         value = FALSE, status = "danger", width = "100%")),
                 tags$div(style = "width:20%; float:left;",
                          materialSwitch(inputId = ns("OP_CumplidoMargenPER"),
                                         label = FormatearTexto("Cumplidas Margen (Periodica)", 0.9),
                                         value = FALSE, status = "danger", width = "100%"))
               )
             )
      )
    ),
    fluidRow(
      column(12,
             box(
               title = "Oportunidades Activas", width = 12, status = "white",
               solidHeader = TRUE, collapsible = TRUE,
               GTBotonesUI(ns("tabla_activas"))
             )
      )
    ),
    fluidRow(
      column(12,
             box(
               title = "Oportunidades Descartadas", width = 12, status = "white",
               solidHeader = TRUE, collapsible = TRUE,
               gt_output(ns("tabla_descartadas"))
             )
      )
    )
  )
}

TablaOportunidades <- function(id, data_op, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    
    # Resumen completo de oportunidades con metricas de cumplimiento ----
    data_op_completa <- reactive({
      trigger_update()
      req(data_op())
      
      data_op() %>%
        dplyr::group_by(
          Usuario, FechaHoraCrea, TipoClienteOP, PerRazSoc, LineaNegocio,
          TipoCliente, Segmento, Categoria, Producto, FechaCumpOP, Descartada, Cliente
        ) %>%
        dplyr::summarise(
          SacosOP        = max(SacosOP),
          Sacos70        = max(Sacos70),
          Kilos          = max(Kilos),
          MargenTotalOP  = max(MargenTotalOP),
          MargenKiloOP   = max(MargenOP),
          FrecuenciaOP   = max(FrecuenciaDias),
          SacosTOT       = sum(dplyr::if_else(
            is.na(FecFact) | (FecFact >= FechaHoraCrea & FecFact <= FechaCumpOP),
            SacFact, 0), na.rm = TRUE),
          CumpSacosTOT   = SiError_0(SacosTOT / SacosOP),
          PendSacosTOT   = CumpSacosTOT < 1,
          MargenTOT      = sum(dplyr::if_else(
            is.na(FecFact) | (FecFact >= FechaHoraCrea & FecFact <= FechaCumpOP),
            Margen, 0), na.rm = TRUE),
          CumpMargenTOT  = SiError_0(MargenTOT / MargenTotalOP),
          PendMargenTOT  = CumpMargenTOT < 1,
          MargenKiloTOT  = SiError_0(MargenTOT / (SacosTOT * Kilos)),
          CumpMargenKiloTOT = SiError_0(MargenKiloTOT / MargenKiloOP),
          SacosMes       = max(SacosMes),
          MargenMes      = max(MargenMes),
          MargenKiloMes  = SiError_0(MargenMes / (SacosMes * Kilos)),
          MesMin = suppressWarnings(min(
            dplyr::if_else(is.na(FecFact) | FecFact >= FechaHoraCrea, FecFact, as.Date(NA)),
            na.rm = TRUE)),
          MesMax = suppressWarnings(max(
            dplyr::if_else(is.na(FecFact) | FecFact >= FechaHoraCrea, FecFact, as.Date(NA)),
            na.rm = TRUE)),
          NumMeses      = SiError_0(
            round(as.numeric(difftime(MesMax, MesMin, units = "days")) / 30)),
          SacosPER      = SiError_0(SacosTOT / NumMeses),
          CumpSacosPER  = SiError_0(SacosPER / SacosMes),
          PendSacosPER  = CumpSacosPER < 1,
          MargenPER     = SiError_0(MargenTOT / NumMeses),
          CumpMargenPER = SiError_0(MargenPER / MargenMes),
          PendMargenPER = CumpMargenPER < 1,
          MargenKiloPER = SiError_0(MargenPER / (SacosMes * Kilos)),
          CumpMargenKiloPER = SiError_0(MargenKiloPER / MargenKiloMes),
          .groups = "drop"
        ) %>%
        dplyr::mutate(Descartada = dplyr::if_else(is.na(Descartada), FALSE, Descartada))
    })
    
    data_activas    <- reactive({ data_op_completa() %>% dplyr::filter(Descartada == FALSE) })
    data_descartadas <- reactive({ data_op_completa() %>% dplyr::filter(Descartada == TRUE) })
    
    # Construye tabla GT con formato y estilos de cumplimiento ----
    make_gt <- function(df) {
      if (nrow(df) == 0)
        return(gt_mensaje_vacio("No hay oportunidades para los filtros seleccionados"))
      
      aux <- df %>%
        dplyr::select(
          Cliente, LineaNegocio, Categoria, Producto, Descartada, FechaCumpOP,
          SacosOP, Sacos70, MargenTotalOP, FrecuenciaOP,
          CumpSacosTOT, CumpMargenTOT, CumpMargenKiloTOT,
          CumpSacosPER, CumpMargenPER, CumpMargenKiloPER
        ) %>%
        dplyr::mutate(Descartada = ifelse(Descartada == 1, "SI", "NO"))
      
      gt::gt(aux) %>%
        gt::tab_spanner(label = "Oportunidad de Negocio", columns = 1:9) %>%
        gt::tab_spanner(label = "Cumplimiento Total",     columns = 10:12) %>%
        gt::tab_spanner(label = "Cumplimiento Periodico", columns = 13:15) %>%
        gt::tab_header(
          title    = "Oportunidades de Negocio",
          subtitle = paste("Total Oportunidades:", nrow(aux))
        ) %>%
        gt::cols_label(
          Cliente            = "Cliente",
          LineaNegocio       = "Linea de Negocio",
          Categoria          = "Categoria",
          Producto           = "Producto",
          FechaCumpOP        = "Fecha Cump. Oportunidad",
          SacosOP            = "Sacos Oportunidad",
          Sacos70            = "Sacos (70Kgs)",
          MargenTotalOP      = "Margen Total Oportunidad",
          FrecuenciaOP       = "Frecuencia (dias)",
          CumpSacosTOT       = "Cumpl. Sacos (%)",
          CumpMargenTOT      = "Cumpl. Margen (%)",
          CumpMargenKiloTOT  = "Cumpl. Margen/Kilo (%)",
          CumpSacosPER       = "Cumpl. Sacos Mes (%)",
          CumpMargenPER      = "Cumpl. Margen Mes (%)",
          CumpMargenKiloPER  = "Cumpl. Margen/Kilo Mes (%)"
        ) %>%
        gt::fmt_number(columns = c(SacosOP, FrecuenciaOP), decimals = 0) %>%
        gt::fmt_number(columns = Sacos70, decimals = 2) %>%
        gt::fmt_currency(columns = MargenTotalOP, currency = "COP", decimals = 0) %>%
        gt::fmt_percent(
          columns  = c(CumpSacosTOT, CumpMargenTOT, CumpMargenKiloTOT,
                       CumpSacosPER, CumpMargenPER, CumpMargenKiloPER),
          decimals = 2
        ) %>%
        gt::tab_style(
          style     = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(columns = c(CumpSacosTOT, CumpMargenTOT))
        ) %>%
        gt::tab_style(
          style     = gt::cell_text(color = "red", weight = "bold"),
          locations = gt::cells_body(columns = FechaCumpOP, rows = FechaCumpOP < Sys.Date())
        ) %>%
        gt::fmt_markdown(columns = Cliente) %>%
        gt_minimal_style() %>%
        gt_pct_style(CumpSacosTOT, CumpMargenTOT, CumpMargenKiloTOT,
                     CumpSacosPER, CumpMargenPER, CumpMargenKiloPER) %>%
        gt::opt_interactive(use_pagination = FALSE, use_filters = FALSE)
    }
    
    # Configuracion de botones de accion por fila ----
    botones_config <- crear_config_botones(
      detalle = list(
        titulo        = "Ver Detalle",
        tit_modal     = "Detalle Oportunidad -",
        module_ui     = "DetalleOportunidadUI",
        module_server = "DetalleOportunidad",
        module_id     = "mod_detalle",
        posicion      = "inicio",
        extra_params  = list(data_op_completa = data_op)
      ),
      descartar = list(
        titulo        = "Descartar Oportunidad",
        tit_modal     = "Descartar Oportunidad -",
        module_ui     = "DescartarOportunidadUI",
        module_server = "DescartarOportunidad",
        module_id     = "mod_descartar",
        posicion      = "inicio",
        extra_params  = list(usr = usr, trigger = trigger_update)
      )
    )
    
    # Tabla activas con GTBotones ----
    GTBotones(
      id             = "tabla_activas",
      gt_table       = reactive(make_gt(data_activas())),
      data           = data_activas,
      botones_config = botones_config,
      nombre_col     = c("Cliente")
    )
    
    # Tabla descartadas (solo lectura) ----
    output$tabla_descartadas <- render_gt({ make_gt(data_descartadas()) })
  })
}


# Modulo DashboardOportunidades ----
DashboardOportunidadesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # KPIs con CajaModal (reemplaza valueBoxOutput + renderbs4ValueBox) ----
    fluidRow(
      column(4, racafeModulos::CajaModalUI(ns("kpi_total"))),
      column(4, racafeModulos::CajaModalUI(ns("kpi_activas"))),
      column(4, racafeModulos::CajaModalUI(ns("kpi_descartadas")))
    ),
    # Cumplimiento total acumulado ----
    fluidRow(
      column(12,
             box(
               title = "Cumplimiento total (facturacion vs oportunidad)",
               status = "white", solidHeader = FALSE, width = 12,
               fluidRow(
                 column(6, plotlyOutput(ns("plot_sacos_tot"),  height = "320px")),
                 column(6, plotlyOutput(ns("plot_margen_tot"), height = "320px"))
               )
             )
      )
    ),
    # Estandarizacion 30 dias (total) ----
    fluidRow(
      column(12,
             box(
               title = "Estandarizacion a 30 dias (total)",
               status = "white", solidHeader = FALSE, width = 12,
               fluidRow(
                 column(6, plotlyOutput(ns("plot_sacos_30"),  height = "320px")),
                 column(6, plotlyOutput(ns("plot_margen_30"), height = "320px"))
               )
             )
      )
    ),
    # Oportunidades por asesor ----
    fluidRow(
      column(12,
             box(
               title = "Oportunidades por Asesor", status = "white", solidHeader = FALSE, width = 12,
               fluidRow(
                 column(6, plotlyOutput(ns("grafico_asesor"),      height = "300px")),
                 column(6, gt_output(ns("tabla_asesor")))
               )
             )
      )
    ),
    # Segmentacion de oportunidades ----
    fluidRow(
      column(12,
             box(
               title = "Segmentacion de Oportunidades",
               status = "white", solidHeader = FALSE, width = 12,
               fluidRow(
                 column(4, plotlyOutput(ns("grafico_tipo_cliente"), height = "260px")),
                 column(4, plotlyOutput(ns("grafico_linea"),        height = "260px")),
                 column(4, plotlyOutput(ns("grafico_segmento"),     height = "260px"))
               )
             )
      )
    ),
    # Resumen economico por dimension ----
    fluidRow(
      column(12,
             box(
               title = "Resumen Economico", status = "white", solidHeader = FALSE, width = 12,
               fluidRow(
                 column(6, gt_output(ns("tabla_margen"))),
                 column(6, gt_output(ns("tabla_sacos")))
               )
             )
      )
    ),
    # Oportunidades descartadas ----
    fluidRow(
      column(12,
             box(
               title = "Oportunidades Descartadas", status = "white", solidHeader = FALSE,
               width = 12, collapsible = TRUE, collapsed = TRUE,
               gt_output(ns("tabla_descartes"))
             )
      )
    )
  )
}

DashboardOportunidades <- function(id, datos_op, usr) {
  moduleServer(id, function(input, output, session) {
    
    # Paleta institucional ----
    pal_rg <- c("#7a1f1f", "#9e9e9e", "#bdbdbd", "#d32f2f", "#757575")
    
    # Datos base con scoring de urgencia ----
    datos <- reactive({
      req(datos_op())
      
      df <- datos_op() %>%
        dplyr::mutate(
          dplyr::across(where(is.character),
                        ~ ifelse(is.na(.) | . == "", "SIN DATO", trimws(.))),
          FechaCumpOP   = as.Date(FechaCumpOP),
          FechaHoraCrea = as.POSIXct(FechaHoraCrea),
          FecFact       = as.Date(FecFact),
          dias_urgencia   = as.numeric(FechaCumpOP - Sys.Date()),
          factor_urgencia = dplyr::case_when(
            is.na(dias_urgencia) ~ 1.0,
            dias_urgencia <= 7   ~ 1.5,
            dias_urgencia <= 30  ~ 1.2,
            TRUE                 ~ 1.0
          ),
          ScoreRaw = SacosOP * MargenOP * factor_urgencia
        )
      
      df$ScoreOP <- if (all(is.na(df$ScoreRaw)) || length(unique(na.omit(df$ScoreRaw))) <= 1) {
        0
      } else {
        scales::rescale(df$ScoreRaw, to = c(0, 100), na.rm = TRUE)
      }
      
      df %>% dplyr::select(-ScoreRaw)
    })
    
    # Cumplimiento agregado total (todas las oportunidades) ----
    cumplimiento_total <- reactive({
      df <- datos()
      req(nrow(df) > 0)
      
      keys   <- c("PerRazSoc", "LineaNegocio", "Categoria", "Producto")
      op_tot <- df %>%
        dplyr::distinct(dplyr::across(dplyr::all_of(keys)),
                        SacosOP, MargenTotalOP, FrecuenciaDias, FechaHoraCrea, FechaCumpOP) %>%
        dplyr::mutate(
          FechaHoraCrea = as.POSIXct(FechaHoraCrea),
          FechaCumpOP   = as.Date(FechaCumpOP)
        )
      
      sacos_obj  <- sum(as.numeric(op_tot$SacosOP),       na.rm = TRUE)
      margen_obj <- sum(as.numeric(op_tot$MargenTotalOP), na.rm = TRUE)
      fecha_crea <- suppressWarnings(min(as.Date(op_tot$FechaHoraCrea), na.rm = TRUE))
      fecha_cump <- suppressWarnings(max(as.Date(op_tot$FechaCumpOP),   na.rm = TRUE))
      
      frec_dias  <- suppressWarnings(
        stats::weighted.mean(as.numeric(op_tot$FrecuenciaDias),
                             as.numeric(op_tot$SacosOP), na.rm = TRUE))
      if (is.na(frec_dias) || frec_dias <= 0) frec_dias <- as.numeric(fecha_cump - fecha_crea)
      
      fac <- preparar_facturacion(df, fecha_crea, frec_dias)
      
      list(
        op         = tibble::tibble(SacosOP = sacos_obj, MargenTotalOP = margen_obj),
        fac        = fac,
        fac_30     = normalizar_30_dias(fac, sacos_obj, margen_obj, frec_dias),
        cumplimiento = calcular_cumplimiento(fac, sacos_obj, margen_obj, frec_dias),
        fecha_crea = fecha_crea,
        fecha_cump = fecha_cump,
        frec_dias  = frec_dias
      )
    })
    
    # KPIs con CajaModal (reemplaza renderbs4ValueBox + CajaValor) ----
    racafeModulos::CajaModal(
      id     = "kpi_total",
      valor  = reactive(dplyr::n_distinct(datos()$PerRazSoc, datos()$LineaNegocio)),
      texto  = "Total oportunidades", icono = "bullseye",
      colores = c(fondo = "white"), mostrar_boton = FALSE
    )
    racafeModulos::CajaModal(
      id    = "kpi_activas",
      valor = reactive(
        datos() %>% dplyr::filter(Descartada == 0) %>%
          dplyr::distinct(PerRazSoc, LineaNegocio) %>% nrow()
      ),
      texto = "Oportunidades activas", icono = "check-circle",
      colores = c(fondo = "white"), mostrar_boton = FALSE
    )
    racafeModulos::CajaModal(
      id    = "kpi_descartadas",
      valor = reactive(
        datos() %>% dplyr::filter(Descartada == 1) %>%
          dplyr::distinct(PerRazSoc, LineaNegocio) %>% nrow()
      ),
      texto = "Oportunidades descartadas", icono = "ban",
      colores = c(fondo = "white"), mostrar_boton = FALSE
    )
    
    # Plots cumplimiento total (usa factory .plot_lineas_op) ----
    output$plot_sacos_tot <- plotly::renderPlotly({
      x <- cumplimiento_total()
      req(nrow(x$fac) > 0)
      .plot_lineas_op(x$fac, "CumSac", x$op$SacosOP, FormatoD3("numero"),
                      "Cumplimiento total - Sacos", "Sacos")
    })
    output$plot_margen_tot <- plotly::renderPlotly({
      x <- cumplimiento_total()
      req(nrow(x$fac) > 0)
      .plot_lineas_op(x$fac, "CumMarg", x$op$MargenTotalOP, FormatoD3("dinero"),
                      "Cumplimiento total - Margen ($)", "Margen ($)")
    })
    
    # Plots estandarizacion 30 dias (usa factory .plot_barras_op) ----
    output$plot_sacos_30 <- plotly::renderPlotly({
      x <- cumplimiento_total()
      req(nrow(x$fac_30) > 0)
      .plot_barras_op(x$fac_30, "CumSac", "PlanSacos30", FormatoD3("numero"),
                      "Estandarizacion a 30 dias - Sacos (total)", "Sacos")
    })
    output$plot_margen_30 <- plotly::renderPlotly({
      x <- cumplimiento_total()
      req(nrow(x$fac_30) > 0)
      .plot_barras_op(x$fac_30, "CumMarg", "PlanMargen30", FormatoD3("dinero"),
                      "Estandarizacion a 30 dias - Margen ($) (total)", "Margen ($)")
    })
    
    # Grafico de barras por asesor ----
    output$grafico_asesor <- plotly::renderPlotly({
      df <- datos() %>% dplyr::count(Usuario, name = "Total") %>% dplyr::arrange(desc(Total))
      plotly::plot_ly(df, x = ~Usuario, y = ~Total, type = "bar",
                      marker = list(color = pal_rg[1]), text = ~Total, textposition = "outside") %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Graficos de distribucion (pie charts) ----
    .pie_op <- function(df, col) {
      plotly::plot_ly(df, labels = df[[col]], values = ~Total, type = "pie",
                      hole = 0.5, marker = list(colors = pal_rg)) %>%
        plotly::config(displayModeBar = FALSE)
    }
    output$grafico_tipo_cliente <- plotly::renderPlotly({
      .pie_op(datos() %>% dplyr::count(TipoClienteOP, name = "Total"), "TipoClienteOP")
    })
    output$grafico_linea <- plotly::renderPlotly({
      .pie_op(datos() %>% dplyr::count(LineaNegocio, name = "Total"), "LineaNegocio")
    })
    output$grafico_segmento <- plotly::renderPlotly({
      .pie_op(datos() %>% dplyr::count(Segmento, name = "Total"), "Segmento")
    })
    
    # Tablas de resumen economico (usa factory .tabla_dim_op) ----
    output$tabla_asesor  <- gt::render_gt({ .tabla_dim_op(datos(), "Usuario") })
    output$tabla_margen  <- gt::render_gt({ .tabla_dim_op(datos(), "LineaNegocio") })
    output$tabla_sacos   <- gt::render_gt({ .tabla_dim_op(datos(), "Categoria") })
    
    # Tabla de descartes por razon ----
    output$tabla_descartes <- gt::render_gt({
      datos() %>%
        dplyr::filter(Descartada == 1) %>%
        dplyr::group_by(RazonDescartado) %>%
        dplyr::summarise(
          Oportunidades = dplyr::n(),
          Sacos         = sum(SacosOP, na.rm = TRUE),
          Margen        = sum(SacosOP * MargenOP, na.rm = TRUE),
          .groups       = "drop"
        ) %>%
        gt::gt() %>%
        gt::fmt_currency(columns = Margen, currency = "COP", decimals = 0) %>%
        gt_minimal_style() %>%
        gt::opt_interactive(use_pagination = FALSE, use_filters = FALSE, use_resizers = TRUE)
    })
  })
}
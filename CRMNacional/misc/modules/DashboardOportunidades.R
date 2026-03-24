DashboardOportunidadesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # ---------------------
    # KPIS PRINCIPALES
    # ---------------------
    fluidRow(
      bs4Dash::bs4Card(
        title = "Indicadores Principales",
        width = 12, status = "primary", solidHeader = TRUE,
        fluidRow(
          bs4Dash::bs4ValueBoxOutput(ns("vb_total")),
          bs4Dash::bs4ValueBoxOutput(ns("vb_descartadas")),
          bs4Dash::bs4ValueBoxOutput(ns("vb_sacos")),
          bs4Dash::bs4ValueBoxOutput(ns("vb_margen"))
        )
      )
    ),
    
    # ---------------------
    # DISTRIBUCIONES
    # ---------------------
    fluidRow(
      bs4Dash::bs4Card(
        title = "Distribución por Variables Clave",
        width = 12, status = "info", solidHeader = TRUE,
        fluidRow(
          column(4, plotOutput(ns("plt_segmento"), height = "260px")),
          column(4, plotOutput(ns("plt_linea"), height = "260px")),
          column(4, plotOutput(ns("plt_categoria"), height = "260px"))
        )
      )
    ),
    
    # ---------------------
    # TABLA GT
    # ---------------------
    fluidRow(
      bs4Dash::bs4Card(
        title = "Tabla de Oportunidades",
        width = 12, status = "white", solidHeader = TRUE,
        GTBotonesUI(ns("tabla_op"))
      )
    ),
    
    # ---------------------
    # FECHAS
    # ---------------------
    fluidRow(
      bs4Dash::bs4Card(
        title = "Oportunidades por Fecha de Cumplimiento",
        width = 12, status = "secondary", solidHeader = TRUE,
        plotOutput(ns("plt_fechas"), height = "280px")
      )
    )
  )
}
DashboardOportunidades <- function(id, data_raw) {
  moduleServer(id, function(input, output, session) {
    
    # ============================================================
    # DATA BASE
    # ============================================================
    data_op <- reactive({
      req(data_raw())
      df <- data_raw()
      
      df %>%
        dplyr::mutate(
          FechaCumpOP = as.Date(FechaCumpOP),
          Descartada  = dplyr::if_else(is.na(Descartada), 0, Descartada),
          MargenTotal = SacosOP * MargenOP
        )
    })
    
    # ============================================================
    # RESUMEN KPI
    # ============================================================
    resumen <- reactive({
      df <- data_op()
      list(
        total        = nrow(df),
        descartadas  = sum(df$Descartada == 1, na.rm = TRUE),
        sacos        = sum(df$SacosOP, na.rm = TRUE),
        margen       = sum(df$MargenTotal, na.rm = TRUE)
      )
    })
    
    # -----------------------------
    # VALUE BOXES
    # -----------------------------
    output$vb_total <- bs4Dash::renderbs4ValueBox({
      r <- resumen()
      bs4Dash::bs4ValueBox(
        value = r$total,
        subtitle = "Total Oportunidades",
        icon = "fa-list"
      )
    })
    
    output$vb_descartadas <- bs4Dash::renderbs4ValueBox({
      r <- resumen()
      bs4Dash::bs4ValueBox(
        value = r$descartadas,
        subtitle = "Descartadas",
        icon = "fa-times"
      )
    })
    
    output$vb_sacos <- bs4Dash::renderbs4ValueBox({
      r <- resumen()
      bs4Dash::bs4ValueBox(
        value = format(r$sacos, big.mark = ","),
        subtitle = "Sacos Totales",
        icon = "fa-box-open"
      )
    })
    
    output$vb_margen <- bs4Dash::renderbs4ValueBox({
      r <- resumen()
      bs4Dash::bs4ValueBox(
        value = scales::comma(r$margen),
        subtitle = "Margen Total (COP)",
        icon = "fa-dollar-sign"
      )
    })
    
    
    # ============================================================
    # DISTRIBUCIONES
    # ============================================================
    output$plt_segmento <- renderPlot({
      df <- data_op()
      df %>%
        dplyr::count(Segmento) %>%
        ggplot2::ggplot(ggplot2::aes(x = Segmento, y = n)) +
        ggplot2::geom_col(fill = "#2C3E50") +
        ggplot2::labs(title = "Por Segmento", x = NULL, y = "Cantidad") +
        ggplot2::theme_minimal()
    })
    
    output$plt_linea <- renderPlot({
      df <- data_op()
      df %>%
        dplyr::count(LineaNegocio) %>%
        ggplot2::ggplot(ggplot2::aes(x = LineaNegocio, y = n)) +
        ggplot2::geom_col(fill = "#7F8C8D") +
        ggplot2::labs(title = "Por Línea de Negocio", x = NULL, y = "Cantidad") +
        ggplot2::theme_minimal()
    })
    
    output$plt_categoria <- renderPlot({
      df <- data_op()
      df %>%
        dplyr::count(Categoria) %>%
        ggplot2::ggplot(ggplot2::aes(x = Categoria, y = n)) +
        ggplot2::geom_col(fill = "#34495E") +
        ggplot2::labs(title = "Por Categoría", x = NULL, y = "Cantidad") +
        ggplot2::theme_minimal()
    })
    
    
    # ============================================================
    # TABLA GT
    # ============================================================
    tabla_gt <- reactive({
      df <- data_op()
      
      df %>%
        dplyr::select(
          Usuario, FechaHoraCrea, TipoClienteOP, PerRazSoc,
          LineaNegocio, Segmento, Categoria, Producto,
          Oportunidad, FechaCumpOP, SacosOP, MargenOP, MargenTotal,
          Descartada, RazonDescartado
        ) %>%
        gt::gt() %>%
        gt::fmt_currency(columns = c(MargenTotal), currency = "COP", decimals = 0)
    })
    
    GTBotones(
      id = "tabla_op",
      gt_table = tabla_gt,
      data = data_op,
      nombre_col = "PerRazSoc",
      botones_config = list(),
      modulos_ui = list(),
      lado_botones = "inicio"
    )
    
    
    # ============================================================
    # FECHAS
    # ============================================================
    output$plt_fechas <- renderPlot({
      df <- data_op()
      
      df %>%
        dplyr::count(FechaCumpOP) %>%
        ggplot2::ggplot(ggplot2::aes(x = FechaCumpOP, y = n)) +
        ggplot2::geom_line(size = 1, color = "#2C3E50") +
        ggplot2::geom_point(size = 2, color = "#2C3E50") +
        ggplot2::labs(
          title = "Oportunidades por Fecha de Cumplimiento",
          x = "Fecha",
          y = "Cantidad"
        ) +
        ggplot2::theme_minimal()
    })
  })
}

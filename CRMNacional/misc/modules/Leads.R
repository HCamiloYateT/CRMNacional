# Formulario Creacion de Leads -----


# Tablero de resumen -----
DashboardLeadsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4,
             valueBoxOutput(ns("total_leads"), width = 12)
      ),
      column(4,
             valueBoxOutput(ns("leads_contactados"), width = 12)
      ),
      column(4,
             valueBoxOutput(ns("leads_descartados"), width = 12)
      )
    ),
    
    fluidRow(
      column(12,
             box(title = "Análisis por Asesor", status = "white", solidHeader = FALSE,
                 width = 12,
                 fluidRow(
                   column(6,
                          plotlyOutput(ns("grafico_asesores"), height = "300px")
                   ),
                   column(6,
                          gt_output(ns("tabla_asesores"))
                   )
                 )
             )
      )
    ),
    
    fluidRow(
      column(12,
             box(title = "Estado y Calidad del Lead", status = "white", solidHeader = FALSE,
                 width = 12,
                 fluidRow(
                   column(4,
                          plotlyOutput(ns("grafico_estado_cuenta"), height = "260px")
                   ),
                   column(4,
                          plotlyOutput(ns("grafico_estado_negocio"), height = "260px")
                   ),
                   column(4,
                          plotlyOutput(ns("grafico_calidad"), height = "260px")
                   )
                 )
             )
      )
    ),
    
    fluidRow(
      column(12,
             box(title = "Origen y Distribución Geográfica", status = "white", solidHeader = FALSE,
                 width = 12,
                 fluidRow(
                   column(6,
                          plotlyOutput(ns("grafico_origen"), height = "300px")
                   ),
                   column(6,
                          plotlyOutput(ns("grafico_geografia"), height = "300px")
                   )
                 )
             )
      )
    ),
    
    fluidRow(
      column(12,
             box(title = "Segmentación del Lead", status = "white", solidHeader = FALSE,
                 width = 12,
                 fluidRow(
                   column(4,
                          gt_output(ns("tabla_segmento"))
                   ),
                   column(4,
                          plotlyOutput(ns("grafico_linea_negocio"), height = "260px")
                   ),
                   column(4,
                          gt_output(ns("tabla_tipo_negocio"))
                   )
                 )
             )
      )
    ),
    
    fluidRow(
      column(12,
             box(title = "Análisis de Leads Descartados", status = "white", solidHeader = FALSE,
                 width = 12, collapsible = TRUE, collapsed = TRUE,
                 gt_output(ns("tabla_descartados"))
             )
      )
    )
  )
}
DashboardLeads <- function(id, datos_leads, usr) {
  moduleServer(id, function(input, output, session) {
    
    # Datos reactivos ----
    datos <- reactive({
      req(datos_leads())
      datos_leads() %>%
        dplyr::mutate(
          dplyr::across(
            where(is.character),
            ~ ifelse(is.na(.) | . == "", "Sin dato", .)
          ),
          dplyr::across(
            where(is.factor),
            ~ forcats::fct_explicit_na(., na_level = "Sin dato")
          )
        )
    })
    
    # Paleta gráficos ----
    pal_rg <- c("#7a1f1f", "#9e9e9e", "#bdbdbd", "#d32f2f", "#757575")
    
    # KPIs ----
    output$total_leads <- renderbs4ValueBox({
      total <- nrow(datos())
      texto_completo <- paste("Total de Leads registrados")
      CajaValor(total, "coma", texto_completo, "users", mostrar_boton = FALSE)
    })
    
    output$leads_contactados <- renderbs4ValueBox({
      total <- nrow(datos())
      
      contactados <- datos() %>%
        dplyr::filter(EstadoCuenta == "CONTACTADO") %>%
        nrow()
      
      pct <- SiError_0(round(contactados / total * 100, 1))
      
      texto_completo <- paste(
        "Leads contactados",
        "<br>Porcentaje:", paste0(pct, "%")
      )
      
      CajaValor(contactados, "coma", texto_completo, "phone", mostrar_boton = FALSE)
    })
    
    output$leads_descartados <- renderbs4ValueBox({
      total <- nrow(datos())
      
      descartados <- datos() %>%
        dplyr::filter(Descartada == 1) %>%
        nrow()
      
      pct <- SiError_0(round(descartados / total * 100, 1))
      
      texto_completo <- paste(
        "Leads descartados",
        "<br>Porcentaje:", paste0(pct, "%")
      )
      
      CajaValor(descartados, "coma", texto_completo, "ban", mostrar_boton = FALSE)
    })
    
    # Gráficos ----
    output$grafico_asesores <- plotly::renderPlotly({
      df <- datos() %>%
        dplyr::count(Asesor, name = "Total") %>%
        dplyr::arrange(desc(Total))
      
      plotly::plot_ly(
        df,
        x = ~Asesor,
        y = ~Total,
        type = "bar",
        marker = list(color = pal_rg[1]),
        text = ~Total,
        textposition = "outside"
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$grafico_estado_cuenta <- plotly::renderPlotly({
      df <- datos() %>%
        dplyr::count(EstadoCuenta, name = "Total")
      
      plotly::plot_ly(
        df,
        labels = ~EstadoCuenta,
        values = ~Total,
        type = "pie",
        hole = 0.5,
        marker = list(colors = pal_rg)
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$grafico_estado_negocio <- plotly::renderPlotly({
      df <- datos() %>%
        dplyr::count(EstadoCuenta, name = "Total")
      
      plotly::plot_ly(
        df,
        labels = ~EstadoCuenta,
        values = ~Total,
        type = "pie",
        hole = 0.5,
        marker = list(colors = pal_rg)
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$grafico_calidad <- plotly::renderPlotly({
      df <- datos() %>%
        dplyr::count(Descartada, name = "Total")
      
      plotly::plot_ly(
        df,
        labels = ~Descartada,
        values = ~Total,
        type = "pie",
        hole = 0.5,
        marker = list(colors = pal_rg)
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$grafico_origen <- plotly::renderPlotly({
      df <- datos() %>%
        dplyr::count(Origen, name = "Total") %>%
        dplyr::arrange(desc(Total))
      
      plotly::plot_ly(
        df,
        x = ~reorder(Origen, Total),
        y = ~Total,
        type = "bar",
        marker = list(color = pal_rg[2]),
        text = ~Total,
        textposition = "outside"
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$grafico_geografia <- plotly::renderPlotly({
      df <- datos() %>%
        dplyr::count(Depto, name = "Total") %>%
        dplyr::arrange(desc(Total)) %>%
        head(10)
      
      plotly::plot_ly(
        df,
        y = ~reorder(Depto, Total),
        x = ~Total,
        type = "bar",
        orientation = "h",
        marker = list(color = pal_rg[3]),
        text = ~Total,
        textposition = "outside"
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$grafico_linea_negocio <- plotly::renderPlotly({
      df <- datos() %>%
        dplyr::count(LinNegocio, name = "Total")
      
      plotly::plot_ly(
        df,
        labels = ~LinNegocio,
        values = ~Total,
        type = "pie",
        hole = 0.5,
        marker = list(colors = pal_rg)
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Tablas ----
    output$tabla_asesores <- gt::render_gt({
      datos() %>%
        dplyr::group_by(Asesor) %>%
        dplyr::summarise(
          Total = n(),
          Contactados = sum(EstadoCuenta == "CONTACTADO"),
          .groups = "drop"
        ) %>%
        gt::gt() %>%
        gt_minimal_style()
    })
    
    output$tabla_segmento <- gt::render_gt({
      datos() %>%
        dplyr::group_by(Segmento) %>%
        dplyr::summarise(Total = n(), .groups = "drop") %>%
        gt::gt() %>%
        gt_minimal_style()
    })
    
    output$tabla_tipo_negocio <- gt::render_gt({
      datos() %>%
        dplyr::group_by(LinNegocio) %>%
        dplyr::summarise(Total = n(), .groups = "drop") %>%
        gt::gt() %>%
        gt_minimal_style()
    })
    
    output$tabla_descartados <- gt::render_gt({
      datos() %>%
        dplyr::filter(Descartada == 1) %>%
        dplyr::group_by(RazonDescartado) %>%
        dplyr::summarise(Total = n(), .groups = "drop") %>%
        gt::gt() %>%
        gt_minimal_style()
    })
    
  })
}




### App de prueba ----
ui <- bs4DashPage(
  title = "Prueba ResumenTotal",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    DashboardLeadsUI("resumen")
  )
)

server <- function(input, output, session) {
  DashboardLeads("resumen", reactive({CargarDatos("CRMNALLEAD")}), reactive("CMEDINA"))
}

shinyApp(ui, server)
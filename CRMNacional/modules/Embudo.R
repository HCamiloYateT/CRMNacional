## Embudo de Conversion ----
SankeyTablaUI <-  function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             plotlyOutput(ns("Sankey"), width = "100%", height = "650px"),
             modalDialogUI(modalId = ns("DetalleSankey"),
                           title = "Detalle de Clientes", easyClose = T, button = NULL,
                           footer = actionButton(ns("Cerrar_DetalleSankey"), "Cerrar"),
                           uiOutput(ns("TituloTabla")),
                           DTOutput(ns("detalleTabla"))
                           )
             )
      )
    )
}
SankeyTabla <-  function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Valor reactivo para almacenar el click
    selected_click <- reactiveVal(NULL)
    
    sankey <- reactive({
      req(data())
      
      aux1 <- data() %>%
        select(CliNitPpal, Niv1, Niv2, Niv3) %>%
        distinct()
      
      sankey <- ImprimeSankey2(aux1, vars = c("Niv1", "Niv2", "Niv3"),
                               fun ="n", colores = c("#1f618d", "#0e6655", "#b03a2e"),
                               source = "Sankey")
      
      return(sankey)
      
    })
    output$Sankey <- renderPlotly({
      req(sankey())
      sankey()$plot %>%
        event_register('plotly_click')
    })
    sankey_f <- reactive({
      sel <- selected_click()$customdata  # Usar el valor reactivo
      if (is.null(sel)) {
        aux2 <- data() %>%
          mutate(Origen = "TOTAL")
      } else {
        tipo <- ifelse(grepl("Origen", sel), "Arco", "Nodo")
        
        if (tipo == "Arco") {
          aux1 <- sankey()$arcos %>% 
            filter(texto == sel)
          
          var1 <- aux1$VarSource
          val1 <- aux1$Origen
          
          var2 <- aux1$VarTarget
          val2 <- aux1$Destino
          
          aux2 <- data() %>%
            mutate(Origen = "TOTAL") %>% 
            filter(!!sym(var1) == val1, !!sym(var2) == val2)
          
        } else {
          aux1 <- sankey()$nodos %>% 
            filter(texto == sel)
          
          var1 <- aux1$Var
          val1 <- aux1$label2
          
          aux2 <- data() %>%
            mutate(Origen = "TOTAL") %>% 
            filter(!!sym(var1) == val1)
        }
      }
      
      return(aux2)
    })
    
    # Observar clicks y almacenar el valor
    observeEvent(event_data("plotly_click", source = "Sankey"), {
      click_data <- event_data("plotly_click", source = "Sankey")
      
      if (!is.null(click_data)) {
        selected_click(click_data)  # Almacenar el click
        showModalUI("DetalleSankey")
      }
    }, ignoreNULL = FALSE)
    
    # Cerrar modal y limpiar selección
    observeEvent(input$Cerrar_DetalleSankey, {
      selected_click(NULL)  # Limpiar la selección
      hideModalUI("DetalleSankey")
    })
    
    output$TituloTabla <- renderUI({
      click_data <- selected_click()  # Usar el valor reactivo
      req(click_data)
      
      sel <- click_data$customdata
      tipo <- ifelse(grepl("Origen", sel), "Arco", "Nodo")
      seg <- data()$SegmentoRacafe[1]
      
      if (tipo=="Arco") {
        aux1 <- sankey()$arcos %>% 
          filter(texto == sel) 
        
        val1 <- aux1$Origen
        val2 <- aux1$Destino
        
        tit <- paste0(seg, " ", val1, " a ", val2)
        
      } else {
        aux1 <- sankey()$nodos %>% 
          filter(texto == sel)
        
        val1 <- aux1$label2
        tit <- paste0(seg, " ", val1)
      }
      
      h6(tit)
    })
    output$detalleTabla <- renderDataTable({
      req(selected_click())  # Asegurar que hay un click seleccionado
      
      aux1 <- sankey_f() %>% 
        group_by(PerRazSoc, LineaNegocio = CLLinNegNo, Segmento) |> 
        summarise(UltFcatura = max(FecFact, na.rm = T),
                  SacosMes = sum(if_else(PrimerDia(FecDesp) == PrimerDia(Sys.Date()), Kilos/70, 0), na.rm = T),
                  SacosAnho = sum(if_else(year(FecDesp) == year(Sys.Date()), Kilos/70, 0), na.rm = T),
                  MargenMes = sum(if_else(PrimerDia(FecDesp) == PrimerDia(Sys.Date()), Margen/70, 0), na.rm = T),
                  MargenAnho = sum(if_else(year(FecDesp) == year(Sys.Date()), Margen, 0), na.rm = T),
                  .groups = "drop") |> 
        janitor::adorn_totals("row", name = "TOTAL")
      
      ImprimirDTRAzSocLinNeg(aux1,
                             noms = c("Razón Social", "Línea de Negocio", "Segmento Racafé", "Última Factura",
                                      "Sacos Mes", "Sacos Año", "Márgen Mes", "Márgen Año"),
                             formatos = c(NA, NA, NA, NA, "sacos", "sacos", "dinero", "dinero"),
                             dom = "Bft", buscar = TRUE, alto = 500)
    })
    
  })
}
  
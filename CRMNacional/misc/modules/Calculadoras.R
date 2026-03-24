# Coproductos -----
CalculadoraCoproductosUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Selector de segmento centrado
    fluidRow(
      # Columna izquierda - Inputs
      column(6,
             fluidRow(
               column(12,
                      div(style = "width: 80%; margin: 0 auto 15px auto;",
                          radioGroupButtons(inputId = ns("Segmento"), label = NULL,  status = "danger",
                                            choices = c("MEDIANO", "DETAL"), selected = "MEDIANO",
                                            justified = TRUE)
                      )
               )
             ),
             # Card de Productos
             bs4Dash::bs4Card(title = "Productos", status = "white", solidHeader = TRUE, width = 12,  collapsible = FALSE,
                              fluidRow(
                                column(6, FormatearTexto("Costo 1D7")),
                                column(6, autonumericInput(ns("COSTO_1D7"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
                              ),
                              fluidRow(
                                column(6, FormatearTexto("Costo 5D5")),
                                column(6, autonumericInput(ns("COSTO_5D5"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
                              ),
                              fluidRow(
                                column(6, FormatearTexto("Costo 5D50")),
                                column(6, autonumericInput(ns("COSTO_5D50"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
                              ),
                              fluidRow(
                                column(6, FormatearTexto("Costo 9D1")),
                                column(6, autonumericInput(ns("COSTO_9D1"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
                              ),
                              fluidRow(
                                column(6, FormatearTexto("Costo 9D3")),
                                column(6, autonumericInput(ns("COSTO_9D3"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))                              
                              )
             ),
             # Card de Otros Costos
             bs4Dash::bs4Card(title = "Otros Costos", status = "white", solidHeader = TRUE, width = 12,  collapsible = FALSE,
                              fluidRow(
                                column(6, FormatearTexto("Cargos de Trilladora")),
                                column(6, autonumericInput(ns("CostosTrilladora"), label = NULL, value = 400, currencySymbol = "$", width = "100%"))
                              ),
                              fluidRow(
                                column(6, FormatearTexto("Gastos Fijos")),
                                column(6, autonumericInput(ns("GastosFijos"), label = NULL, value = 200, currencySymbol = "$", width = "100%"))
                                # $ 450 para detal
                              ),
                              fluidRow(
                                column(6, FormatearTexto("Fletes")),
                                column(6, autonumericInput(ns("Fletes"), label = NULL, value = 200, currencySymbol = "$", width = "100%"))
                              ),
                              fluidRow(
                                column(6, FormatearTexto("Márgen Esperado")),
                                column(6, autonumericInput(ns("MargenEsperado"), label = NULL, value = 200, currencySymbol = "$", width = "100%"))
                              )
             )
      ),
      # Columna derecha - Outputs
      column(6,
             # Card de Precios
             bs4Dash::bs4Card(title = "Precios Calculados", status = "white", solidHeader = TRUE,  width = 12, collapsible = FALSE,
                              fluidRow(
                                column(6, FormatearTexto("Precio 1D7")),
                                column(6, style = "text-align: right", uiOutput(ns("Precio_1d7")))
                              ),
                              fluidRow(
                                column(6, FormatearTexto("Precio 5D5")),
                                column(6, style = "text-align: right", uiOutput(ns("Precio_5d5")))
                              ),                                
                              fluidRow(
                                column(6, FormatearTexto("Precio 5D50")),
                                column(6, style = "text-align: right", uiOutput(ns("Precio_5d50")))
                              ),
                              fluidRow(
                                column(6, FormatearTexto("Precio 9D1")),
                                column(6, style = "text-align: right", uiOutput(ns("Precio_9d1")))
                              ),
                              fluidRow(
                                column(6, FormatearTexto("Precio 9D3")),
                                column(6, style = "text-align: right", uiOutput(ns("Precio_9d3")))
                              )
             ),
             bs4Dash::bs4Card(title = "Viabilidad de Negocios", status = "white", solidHeader = TRUE, width = 12, collapsible = FALSE,
                              # Encabezado de tabla
                              fluidRow(
                                column(4, FormatearTexto("Producto"), style = "font-weight: bold; text-align: left; font-size: 14pt; border-bottom: 1px solid #000;"),
                                column(2, FormatearTexto("Precio"), style = "font-weight: bold; text-align: center; font-size: 14pt; border-bottom: 1px solid #000;"),
                                column(2, FormatearTexto("Kilos"), style = "font-weight: bold; text-align: center; font-size: 14pt; border-bottom: 1px solid #000;"),
                                column(2, FormatearTexto("Margen"), style = "font-weight: bold; text-align: center; font-size: 14pt; border-bottom: 1px solid #000;"),
                                column(2, FormatearTexto("Utilidad"), style = "font-weight: bold; text-align: center; font-size: 14pt; border-bottom: 1px solid #000;")
                              ),
                              br(),
                              # Filas de productos
                              fluidRow(
                                column(4, FormatearTexto("1D7")),
                                column(2, autonumericInput(ns("Pre1D7"), label = NULL, value = 0, currencySymbol = "$", width = "100%")),
                                column(2, autonumericInput(ns("Kls1D7"), label = NULL, value = 0, width = "100%")),
                                column(2, style = "text-align: center", uiOutput(ns("Mar1D7"))),
                                column(2, style = "text-align: center", uiOutput(ns("Utl1D7")))
                              ),
                              fluidRow(
                                column(4, FormatearTexto("5D5")),
                                column(2, autonumericInput(ns("Pre5D5"), label = NULL, value = 0, currencySymbol = "$", width = "100%")),
                                column(2, autonumericInput(ns("Kls5D5"), label = NULL, value = 0, width = "100%")),
                                column(2, style = "text-align: center", uiOutput(ns("Mar5D5"))),
                                column(2, style = "text-align: center", uiOutput(ns("Utl5D5")))
                              ),
                              fluidRow(
                                column(4, FormatearTexto("5D50")),
                                column(2, autonumericInput(ns("Pre5D50"), label = NULL, value = 0, currencySymbol = "$", width = "100%")),
                                column(2, autonumericInput(ns("Kls5D50"), label = NULL, value = 0, width = "100%")),
                                column(2, style = "text-align: center", uiOutput(ns("Mar5D50"))),
                                column(2, style = "text-align: center", uiOutput(ns("Utl5D50")))
                              ),
                              fluidRow(
                                column(4, FormatearTexto("9D1")),
                                column(2, autonumericInput(ns("Pre9D1"), label = NULL, value = 0, currencySymbol = "$", width = "100%")),
                                column(2, autonumericInput(ns("Kls9D1"), label = NULL, value = 0, width = "100%")),
                                column(2, style = "text-align: center", uiOutput(ns("Mar9D1"))),
                                column(2, style = "text-align: center", uiOutput(ns("Utl9D1")))
                              ),
                              fluidRow(
                                column(4, FormatearTexto("9D3")),
                                column(2, autonumericInput(ns("Pre9D3"), label = NULL, value = 0, currencySymbol = "$", width = "100%")),
                                column(2, autonumericInput(ns("Kls9D3"), label = NULL, value = 0, width = "100%")),
                                column(2, style = "text-align: center", uiOutput(ns("Mar9D3"))),
                                column(2, style = "text-align: center", uiOutput(ns("Utl9D3")))
                              ),
                              br()
             ),
             # Botón centrado
             fluidRow(
               column(12, style = "text-align: center;",
                      div(style = "display: inline-block; width: 40%;",
                          actionBttn(inputId = ns("Calcular"), label = "Calcular", style = "unite", 
                                     color = "danger", size = "sm", icon = icon("calculator"), block = TRUE)
                      )
               )
             )
      )
    )
  )
}
CalculadoraCoproductos <- function(id, dat_ind, usr) {
  moduleServer(id, function(input, output, session) {
    
    # Actualizar valores según segmento
    observeEvent(input$Segmento, ignoreNULL = FALSE, {
      aux1 <- dat_ind()
      
      # Función auxiliar para obtener valores
      get_val <- function(item) {
        val <- aux1 %>% filter(Item == item) %>% pull(Valor)
        if (length(val) == 0) NA else val
      }
      
      # Obtener valores base
      val_9d1 <- get_val("Precio Soluble (Compras)")
      val_5d5 <- get_val("Precio Molidos (Compras)")
      val_1d7 <- get_val("Precio Consumo (Compras)")
      val_9d3 <- if (!is.na(val_9d1)) val_9d1 - 1000 else NA
      val_5d50 <- if (!any(is.na(c(val_9d1, val_5d5)))) mean(c(val_9d1, val_5d5)) else NA
      
      # Obtener margen esperado del presupuesto
      margen_esperado <- tryCatch({
        CargarDatos("CRMNALCLIENTE") %>% 
          filter(LinNegCod == 10000, Segmento == input$Segmento) %>% 
          group_by(LinNegCod, CliNitPpal) %>% 
          filter(FecProceso == max(FecProceso)) %>% 
          group_by(Segmento) %>% 
          summarise(MargenKilo = sum(MNFCCPpto, na.rm = TRUE) / (sum(SSPpto, na.rm = TRUE) * 70), .groups = 'drop') %>% 
          pull(MargenKilo)
      }, error = function(e) 200)
      
      gast_fijos <- ifelse(input$Segmento == "DETAL", 450, 200)
      
      
      # Actualizar inputs
      updateAutonumericInput(session, "COSTO_9D3", value = val_9d3)
      updateAutonumericInput(session, "COSTO_9D1", value = val_9d1)
      updateAutonumericInput(session, "COSTO_5D50", value = val_5d50)
      updateAutonumericInput(session, "COSTO_5D5", value = val_5d5)
      updateAutonumericInput(session, "COSTO_1D7", value = val_1d7)
      updateAutonumericInput(session, "MargenEsperado", value = margen_esperado)
      updateAutonumericInput(session, "GastosFijos", value = gast_fijos)
    })
    
    # Cambiar eventReactive por reactive para cálculo automático
    val_calculadora <- reactive({
      # Validar que todos los inputs existen
      req(input$COSTO_9D3, input$COSTO_9D1, input$COSTO_5D50, 
          input$COSTO_5D5, input$COSTO_1D7, input$CostosTrilladora,
          input$GastosFijos, input$Fletes, input$MargenEsperado)
      
      data.frame(
        val_9d3 = input$COSTO_9D3,
        val_9d1 = input$COSTO_9D1,
        val_5d50 = input$COSTO_5D50,
        val_5d5 = input$COSTO_5D5,
        val_1d7 = input$COSTO_1D7,
        Trilladora = input$CostosTrilladora,
        GastosFijos = input$GastosFijos,
        Fletes = input$Fletes,
        MargenEsperado = input$MargenEsperado,
        Pre9D3 = ifelse(is.null(input$Pre9D3), 0, input$Pre9D3),
        Pre9D1 = ifelse(is.null(input$Pre9D1), 0, input$Pre9D1),
        Pre5D50 = ifelse(is.null(input$Pre5D50), 0, input$Pre5D50),
        Pre5D5 = ifelse(is.null(input$Pre5D5), 0, input$Pre5D5),
        Pre1D7 = ifelse(is.null(input$Pre1D7), 0, input$Pre1D7),
        Kls9D3 = ifelse(is.null(input$Kls9D3), 0, input$Kls9D3),
        Kls9D1 = ifelse(is.null(input$Kls9D1), 0, input$Kls9D1),
        Kls5D50 = ifelse(is.null(input$Kls5D50), 0, input$Kls5D50),
        Kls5D5 = ifelse(is.null(input$Kls5D5), 0, input$Kls5D5),
        Kls1D7 = ifelse(is.null(input$Kls1D7), 0, input$Kls1D7)
      ) %>% 
        mutate(
          # Precios calculados
          Precio_9d3 = val_9d3 + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_9d1 = val_9d1 + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_5d50 = val_5d50 + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_5d5 = val_5d5 + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_1d7 = val_1d7 + Trilladora + GastosFijos + Fletes + MargenEsperado,
          # Márgenes
          Mar9D3 = Pre9D3 - Precio_9d3,
          Mar9D1 = Pre9D1 - Precio_9d1,
          Mar5D50 = Pre5D50 - Precio_5d50,
          Mar5D5 = Pre5D5 - Precio_5d5,
          Mar1D7 = Pre1D7 - Precio_1d7,
          # Utilidades
          Utl9D3 = Mar9D3 * Kls9D3,
          Utl9D1 = Mar9D1 * Kls9D1,
          Utl5D50 = Mar5D50 * Kls5D50,
          Utl5D5 = Mar5D5 * Kls5D5,
          Utl1D7 = Mar1D7 * Kls1D7
        )
    })
    
    # Configuración de outputs
    outputs_config <- list(
      "Precio_9d3" = "dollar", "Precio_9d1" = "dollar", "Precio_5d50" = "dollar",
      "Precio_5d5" = "dollar", "Precio_1d7" = "dollar", "Mar9D3" = "dollar",
      "Mar9D1" = "dollar", "Mar5D50" = "dollar", "Mar5D5" = "dollar",
      "Mar1D7" = "dollar", "Utl9D3" = "dollar", "Utl9D1" = "dollar",
      "Utl5D50" = "dollar", "Utl5D5" = "dollar", "Utl1D7" = "dollar"
    )
    
    # Generar outputs dinámicamente
    lapply(names(outputs_config), function(out) {
      output[[out]] <- renderUI({
        val <- val_calculadora() %>% pull(out)
        val_fmt <- eval(parse_expr(paste0(outputs_config[[out]], "(", val, ", accuracy = 0.01)")))
        FormatearTexto(val_fmt, alineacion = "right", negrita = FALSE) %>% HTML()
      })
    })
    
    # Opcional: Mantener el botón para forzar recálculo si es necesario
    observeEvent(input$Calcular, {
      # Forzar invalidación de val_calculadora si es necesario
      # O realizar alguna acción adicional
    })
  })
}

# Dolares Excelso ----
CalculadoraDolaresExcelsoUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6,
           bs4Dash::bs4Card(
             title = "Parámetros", status = "white",
             solidHeader = TRUE, width = 12, collapsible = FALSE,
             fluidRow(
               column(6, FormatearTexto("TRM")),
               column(6, autonumericInput(ns("TRM"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Contrato C NY")),
               column(6, autonumericInput(ns("NYC"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Diferencial de Compra")),
               column(6, autonumericInput(ns("PrimaUGQ"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Prima Mallas")),
               column(6, autonumericInput(ns("PrimaMallas"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Prima Especiales")),
               column(6, autonumericInput(ns("PrimaEspeciales"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Utilidad")),
               column(6, autonumericInput(ns("Utilidad"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Empaque")),
               column(6, autonumericInput(ns("Empaque"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Grainpro")),
               column(6, autonumericInput(ns("Grainpro"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Flete")),
               column(6, autonumericInput(ns("Flete"), label = NULL, value = 0, currencySymbol = "$", width = "100%"))
             )
           )
    ),
    column(6,
           bs4Dash::bs4Card(
             title = "Resultados", status = "white",
             solidHeader = TRUE, width = 12, collapsible = FALSE,
             fluidRow(
               column(6, FormatearTexto("Total")),
               column(6, style = "text-align: right", uiOutput(ns("Total")))
             ),
             fluidRow(
               column(6, FormatearTexto("Precio $USD")),
               column(6, style = "text-align: right", uiOutput(ns("PrecioUSD")))
             ),
             fluidRow(
               column(6, FormatearTexto("Precio $COP")),
               column(6, style = "text-align: right", uiOutput(ns("PrecioCOP")))
             ),
             fluidRow(
               column(6, FormatearTexto("Precio con Flete")),
               column(6, style = "text-align: right", uiOutput(ns("PrecioFlete")))
             )
           ),
           fluidRow(
             column(12, style = "text-align: center;",
                    div(style = "display: inline-block; width: 40%;",
                        actionBttn(inputId = ns("Calcular"), label = "Calcular",
                                   style = "unite", color = "danger", size = "sm",
                                   icon = icon("calculator"), block = TRUE)
                    )
             )
           )
    )
  )
}
CalculadoraDolaresExcelso <- function(id, dat_ind, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Valores por defecto
    actualizado <- reactiveVal(FALSE)
    observe({
      if (!actualizado()) {
        inputs_a_actualizar <- list(
          "TRM" = dat_ind() %>% filter(Item == "TRM (Hoja de trabajo)") %>% pull(Valor),
          "NYC" = dat_ind() %>% filter(Item == "Precio Bolsa") %>% pull(Valor),
          "PrimaUGQ" = get_system_data(uid, pwd)$precios_adicionales$Diferencial,
          "PrimaMallas" = 0,
          "PrimaEspeciales" = 0,
          "Utilidad" = 13.39,
          "Empaque" = 0,
          "Grainpro" = 0,
          "Flete" = 200
        )
        
        # Actualizar cada input
        for (id_input in names(inputs_a_actualizar)) {
          updateAutonumericInput(
            session,
            inputId = id_input,
            value = inputs_a_actualizar[[id_input]]
          )
        }
        
        actualizado(TRUE)
      }
    })
    
    # Cálculos y Outputs
    val_calculadora <- eventReactive(input$Calcular, ignoreNULL = FALSE, {
      data.frame(
        TRM = input$TRM,
        NYC = input$NYC,
        PrimaUGQ = input$PrimaUGQ,
        PrimaMallas = input$PrimaMallas,
        PrimaEspeciales = input$PrimaEspeciales,
        Utilidad = input$Utilidad,
        Empaque = input$Empaque,
        Grainpro = input$Grainpro,
        Flete = input$Flete
      ) %>% 
        mutate(
          Total = NYC + PrimaUGQ + PrimaMallas + PrimaEspeciales + Utilidad + Empaque + Grainpro,
          PrecioUSD = Total * 2.20462 / 100,
          PrecioCOP = PrecioUSD * TRM,
          PrecioFlete = PrecioCOP + Flete
        )
    })
    
    # Generar outputs dinámicamente
    outputs_config <- list(
      "Total" = "dollar",
      "PrecioUSD" = "dollar",
      "PrecioCOP" = "dollar",
      "PrecioFlete" = "dollar"
    )
    
    lapply(names(outputs_config), function(out){
      output[[out]] <- renderUI({
        val <- eval(parse_expr(paste0(outputs_config[[out]], "(", val_calculadora() %>% pull(out), ", accuracy = 0.01)")))
        FormatearTexto(val, alineacion = "right", negrita = FALSE) %>% HTML()
      })
    })
  })
}

# Pesos Excelso -----
CalculadoraPesosExcelsoUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6,
           bs4Dash::bs4Card(
             title = "Parámetros Generales", status = "white",
             solidHeader = TRUE, width = 12, collapsible = FALSE,
             fluidRow(
               column(6, FormatearTexto("TRM")),
               column(6, autonumericInput(ns("TRM"), label = NULL, value = 4403, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Costo Consumo")),
               column(6, autonumericInput(ns("CostoConsumo"), label = NULL, value = 28000, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Precio Carga")),
               column(6, autonumericInput(ns("PrecioCarga"), label = NULL, value = 3107000, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Prima Especial +83")),
               column(6, autonumericInput(ns("PrimaEspecial"), label = NULL, value = 2000, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Prima Supremo")),
               column(6, autonumericInput(ns("PrimaSupremo"), label = NULL, value = 1000, currencySymbol = "$", width = "100%"))
             )
           ),
           bs4Dash::bs4Card(
             title = "Resultados Generales", status = "white",
             solidHeader = TRUE, width = 12, collapsible = FALSE,
             fluidRow(
               column(6, FormatearTexto("Precio por Kilo")),
               column(6, style = "text-align: right", uiOutput(ns("PrecioKilo")))
             ),
             fluidRow(
               column(6, FormatearTexto("Rendimiento")),
               column(6, style = "text-align: right", uiOutput(ns("Rendimiento")))
             ),
             fluidRow(
               column(6, FormatearTexto("Costo por Kilo")),
               column(6, style = "text-align: right", uiOutput(ns("CostoKilo")))
             )
           ),
           bs4Dash::bs4Card(
             title = "Parámetros de Producto", status = "white",
             solidHeader = TRUE, width = 12, collapsible = FALSE,
             fluidRow(
               column(6, FormatearTexto("Producto"), style = "font-weight: bold; text-align: left; font-size: 14pt;border-bottom: 1px solid #000;"),
               column(2, FormatearTexto("Costo"), style = "font-weight: bold; text-align: center; font-size: 14pt;border-bottom: 1px solid #000;"),
               column(2, FormatearTexto("Márgen"), style = "font-weight: bold; text-align: center; font-size: 14pt;border-bottom: 1px solid #000;"),
               column(2, FormatearTexto("Precio"), style = "font-weight: bold; text-align: center; font-size: 14pt;border-bottom: 1px solid #000;")
             ),
             Saltos(),
             # Blend
             fluidRow(
               column(6, FormatearTexto("Blend")),
               column(2, style = "text-align: center", uiOutput(ns("CosBlend"))),
               column(2, autonumericInput(ns("MarBlend"), label = NULL, value = 0, currencySymbol = "$", width = "100%")),
               column(2, style = "text-align: center", uiOutput(ns("PreBlend")))
             ),
             # UGQ
             fluidRow(
               column(6, FormatearTexto("UGQ")),
               column(2, style = "text-align: center", uiOutput(ns("CosUGQ"))),
               column(2, autonumericInput(ns("MarUGQ"), label = NULL, value = 0, currencySymbol = "$", width = "100%")),
               column(2, style = "text-align: center", uiOutput(ns("PreUGQ")))
             ),
             # Supremo
             fluidRow(
               column(6, FormatearTexto("Supremo")),
               column(2, style = "text-align: center", uiOutput(ns("CosSup"))),
               column(2, autonumericInput(ns("MarSup"), label = NULL, value = 0, currencySymbol = "$", width = "100%")),
               column(2, style = "text-align: center", uiOutput(ns("PreSup")))
             ),
             # Especial 83+
             fluidRow(
               column(6, FormatearTexto("Especial 83+")),
               column(2, style = "text-align: center", uiOutput(ns("CosEsp"))),
               column(2, autonumericInput(ns("MarEsp"), label = NULL, value = 0, currencySymbol = "$", width = "100%")),
               column(2, style = "text-align: center", uiOutput(ns("PreEsp")))
             )
           )
    ),
    column(6,
           bs4Dash::bs4Card(
             title = "Costos y Utilidades", status = "white",
             solidHeader = TRUE, width = 12, collapsible = FALSE,
             fluidRow(
               column(6, FormatearTexto("Prima")),
               column(6, autonumericInput(ns("Prima"), label = NULL, value = 2000, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Flete Interno")),
               column(6, autonumericInput(ns("FleteInt"), label = NULL, value = 1000, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Flete a Cliente")),
               column(6, autonumericInput(ns("FleteExt"), label = NULL, value = 300, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Costos Fijos")),
               column(6, autonumericInput(ns("CostosFijos"), label = NULL, value = 500, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Financieros")),
               column(6, autonumericInput(ns("Financieros"), label = NULL, value = 1700, currencySymbol = "$", width = "100%"))
             ),
             br(),
             fluidRow(
               column(6, FormatearTexto("Costos")),
               column(6, style = "text-align: right", uiOutput(ns("Costos2")))
             ),
             fluidRow(
               column(6, FormatearTexto("Precio")),
               column(6, autonumericInput(ns("Precio"), label = NULL, value = 39420, currencySymbol = "$", width = "100%"))
             ),
             fluidRow(
               column(6, FormatearTexto("Utilidad")),
               column(6, style = "text-align: right", uiOutput(ns("Utilidad2")))
             ),
             fluidRow(
               column(6, FormatearTexto("Utilidad cts/lb")),
               column(6, style = "text-align: right", uiOutput(ns("UtilidadCtvs2")))
             )
           ),
           fluidRow(
             column(12, style = "text-align: center;",
                    div(style = "display: inline-block; width: 40%;",
                        actionBttn(inputId = ns("Calcular"), label = "Calcular",
                                   style = "unite", color = "danger", size = "sm",
                                   icon = icon("calculator"), block = TRUE)
                    )
             )
           )
    )
  )
}
CalculadoraPesosExcelso <- function(id, dat_ind, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Deshabilitar márgenes según usuario
    observe({
      if (!usr() %in% c("CMEDINA", "JGCANON")){
        shinyjs::disable("MarBlend")
        shinyjs::disable("MarUGQ")
        shinyjs::disable("MarSup")
        shinyjs::disable("MarEsp")
      } else{
        shinyjs::enable("MarBlend")
        shinyjs::enable("MarUGQ")
        shinyjs::enable("MarSup")
        shinyjs::enable("MarEsp")
      }
    }) 
    
    # Valores por defecto
    actualizado <- reactiveVal(FALSE)
    observe({
      if (!actualizado()) {
        inputs_a_actualizar <- list(
          "TRM" = dat_ind() %>% filter(Item == "TRM (Hoja de trabajo)") %>% pull(Valor),
          "CostoConsumo" = dat_ind() %>% filter(Item == "Precio Consumo (Compras)") %>% pull(Valor),
          "PrecioCarga" = dat_ind() %>% filter(Item == "Precio Carga (Promedio de últimas entradas del día)") %>% pull(Valor),
          "PrimaEspecial" = 2000,
          "Prima" = 2500,
          "FleteInt" = 500,
          "FleteExt" = 500,
          "CostosFijos" = 500,
          "Financieros" = 1700,
          "Precio" = 40000
        )
        
        # Actualizar cada input
        for (id_input in names(inputs_a_actualizar)) {
          updateAutonumericInput(
            session,
            inputId = id_input,
            value = inputs_a_actualizar[[id_input]]
          )
        }
        
        # Actualizar márgenes desde BD
        act_mar <- c("MarBlend", "MarUGQ", "MarSup", "MarEsp")
        aux1 <- CargarDatos("CRMNALCPMAR") %>% 
          filter(FechaHoraCrea == max(FechaHoraCrea))
        lapply(act_mar, function(inp){
          updateAutonumericInput(session, inputId = inp, value = aux1[,inp])
        })
        
        actualizado(TRUE)
      }
    })
    
    # Cambiar eventReactive por reactive para cálculo automático
    val_calculadora <- reactive({
      # Validar que todos los inputs principales existan
      req(input$TRM, input$CostoConsumo, input$PrecioCarga, 
          input$PrimaEspecial, input$Prima, input$FleteInt, 
          input$FleteExt, input$CostosFijos, input$Financieros, 
          input$Precio)
      
      data.frame(
        TRM = input$TRM,
        CostoConsumo = input$CostoConsumo,
        PrecioCarga = input$PrecioCarga,
        PrimaEspecial = input$PrimaEspecial,
        PrimaSupremo = ifelse(is.null(input$PrimaSupremo), 0, input$PrimaSupremo),
        MarBlend = ifelse(is.null(input$MarBlend), 0, input$MarBlend),
        MarUGQ = ifelse(is.null(input$MarUGQ), 0, input$MarUGQ),
        MarSup = ifelse(is.null(input$MarSup), 0, input$MarSup),
        MarEsp = ifelse(is.null(input$MarEsp), 0, input$MarEsp),
        Prima = input$Prima,
        FleteInt = input$FleteInt,
        FleteExt = input$FleteExt,
        CostosFijos = input$CostosFijos,
        Financieros = input$Financieros,
        Precio = input$Precio
      ) %>% 
        mutate(
          PrecioKilo = PrecioCarga / 125,
          Rendimiento = 70/(96.89),
          CostoKilo = PrecioKilo / Rendimiento,
          Costos = CostoKilo + PrimaEspecial + FleteInt + Financieros,
          CosUGQ = CostoKilo,
          PrecioUGQ = CosUGQ + MarUGQ,
          UtilidadKilo = PrecioUGQ - Costos,
          UtilidadCtvs = ((UtilidadKilo*70)/1.54322)/TRM,
          CosBlend = (CostoConsumo*0.4)+(CosUGQ*0.6),
          PreBlend = CosBlend + MarBlend,
          UtBlend = (((PreBlend - CosBlend)*70)/1.54322)/TRM,
          PreUGQ = CosUGQ + MarUGQ,
          UtUGQ = (((PreUGQ - CosUGQ)*70)/1.54322)/TRM,
          CosSup = CosUGQ + PrimaSupremo,
          PreSup = CosSup + MarSup,
          UtSup = (((PreSup - CosSup)*70)/1.54322)/TRM,
          CosEsp = CosUGQ + PrimaEspecial,
          PreEsp = CosEsp + MarEsp,
          UtEsp = (((PreEsp - CosEsp)*70)/1.54322)/TRM,
          Costos2 = CostoKilo + Prima + FleteInt + FleteExt + CostosFijos + Financieros,
          Utilidad2 = Precio - Costos2,
          UtilidadCtvs2 = ((Utilidad2*70)/1.54322)/TRM
        )
    })
    
    # Generar outputs dinámicamente
    outputs_config <- list(
      "PrecioKilo" = "dollar", "Rendimiento" = "percent", "CostoKilo" = "dollar",
      "Costos" = "dollar", "PrecioUGQ" = "dollar", "UtilidadKilo" = "dollar",
      "UtilidadCtvs" = "dollar", "CosBlend" = "dollar", "PreBlend" = "dollar",
      "UtBlend" = "dollar", "CosUGQ" = "dollar", "PreUGQ" = "dollar",
      "UtUGQ" = "dollar", "CosSup" = "dollar", "PreSup" = "dollar",
      "UtSup" = "dollar", "CosEsp" = "dollar", "PreEsp" = "dollar",
      "UtEsp" = "dollar", "Costos2" = "dollar", "Utilidad2" = "dollar",
      "UtilidadCtvs2" = "dollar"
    )
    
    lapply(names(outputs_config), function(out){
      output[[out]] <- renderUI({
        val <- val_calculadora() %>% pull(out)
        val_fmt <- eval(parse_expr(paste0(outputs_config[[out]], "(", val, ", accuracy = 0.01)")))
        FormatearTexto(val_fmt, alineacion = "right", negrita = FALSE) %>% HTML()
      })
    })
    
    # Guardar cambios en márgenes - mantener con botón
    observeEvent(input$Calcular, {
      if (usr() %in% c("CMEDINA", "JGCANON")){
        aux1 <- data.frame(
          UsuarioCrea = usr(),
          FechaHoraCrea = Sys.time(),
          MarBlend = input$MarBlend,
          MarUGQ = input$MarUGQ,
          MarSup = input$MarSup,
          MarEsp = input$MarEsp,
          stringsAsFactors = FALSE
        )
        SubirDatos(aux1, "CRMNALCPMAR")
      }
    })
  })
}



# Modulo Integrador ----
CalculadoraUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Pesos - Excelso", 
             Saltos(),
             CalculadoraPesosExcelsoUI(ns("pesos"))
             ),
    tabPanel("Dólares - Excelso", 
             Saltos(),
             CalculadoraDolaresExcelsoUI(ns("dolares"))
             ),
    tabPanel("Coproductos", 
             Saltos(),
             CalculadoraCoproductosUI(ns("coproductos"))
             )
    )
  }
Calculadora <- function(id, dat_ind, usr) {
  moduleServer(id, function(input, output, session) {
    CalculadoraPesosExcelso("pesos", dat_ind, usr)
    CalculadoraDolaresExcelso("dolares", dat_ind, usr)
    CalculadoraCoproductos("coproductos", dat_ind, usr)
  })
}
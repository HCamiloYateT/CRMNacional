# Helpers UI internos ----

# Fila estándar: label izquierda, input autonumeric derecha
.input_fila <- function(label, input_id, value = 0, symbol = "$") {
  fluidRow(
    column(6, FormatearTexto(label)),
    column(6, autonumericInput(input_id, label = NULL, value = value,
                               currencySymbol = symbol, width = "100%"))
  )
}

# Fila estándar: label izquierda, uiOutput alineado a la derecha
.output_fila <- function(label, output_id) {
  fluidRow(
    column(6, FormatearTexto(label)),
    column(6, class = "calc-output-right", uiOutput(output_id))
  )
}

# Encabezado de columna para tablas internas de calculadora
.header_col <- function(label, width, left = FALSE) {
  cls <- if (left) "calc-header-col calc-header-col--left" else "calc-header-col"
  column(width, FormatearTexto(label), class = cls)
}

# Botón calcular centrado, idéntico en las tres calculadoras
.boton_calcular <- function(ns) {
  fluidRow(
    column(12, class = "calc-btn-wrap",
           div(
             actionBttn(inputId = ns("Calcular"), label = "Calcular",
                        style = "unite", color = "danger", size = "sm",
                        icon = icon("calculator"), block = TRUE)
           )
    )
  )
}

# Card bs4 con solidHeader blanco sin colapso (shorthand)
.calc_card <- function(title, ...) {
  bs4Dash::bs4Card(title = title, status = "white", solidHeader = TRUE,
                   width = 12, collapsible = FALSE, ...)
}

# Helpers server internos ----

# Reemplaza ifelse(is.null(input$x), 0, input$x)
.inp0 <- function(x) if (is.null(x) || is.na(x)) 0 else x

# Patrón de inicialización única
.init_once <- function(fn_init) {
  done <- reactiveVal(FALSE)
  observe({
    if (!done()) { fn_init(); done(TRUE) }
  })
}

# Renderiza un output de texto formateado como dinero o porcentaje
.render_val <- function(output, id, reactive_fn, col, tipo = "dollar") {
  output[[id]] <- renderUI({
    val <- reactive_fn() %>% pull(!!col)
    val_fmt <- switch(tipo,
                      "dollar"  = scales::dollar(val, accuracy = 0.01),
                      "percent" = scales::percent(val, accuracy = 0.01),
                      scales::dollar(val, accuracy = 0.01)
    )
    FormatearTexto(val_fmt, alineacion = "right", negrita = FALSE) %>% HTML()
  })
}

# Registra múltiples outputs sobre un named list (nombre -> tipo)
.bind_outputs <- function(output, config, reactive_fn) {
  lapply(names(config), function(id) {
    .render_val(output, id, reactive_fn, id, config[[id]])
  })
}


# Coproductos ----
CalculadoraCoproductosUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Selector de segmento
    fluidRow(
      column(12,
             div(class = "calc-segmento-wrap",
                 radioGroupButtons(inputId = ns("Segmento"), label = NULL, status = "danger",
                                   choices = c("MEDIANO", "DETAL"), selected = "MEDIANO",
                                   justified = TRUE)
             )
      )
    ),
    fluidRow(
      # Columna izquierda - Inputs
      column(6,
             .calc_card("Costos por Producto",
                        .input_fila("Costo 1D7",  ns("COSTO_1D7")),
                        .input_fila("Costo 5D5",  ns("COSTO_5D5")),
                        .input_fila("Costo 5D50", ns("COSTO_5D50")),
                        .input_fila("Costo 9D1",  ns("COSTO_9D1")),
                        .input_fila("Costo 9D3",  ns("COSTO_9D3"))
             ),
             .calc_card("Otros Costos",
                        .input_fila("Cargos de Trilladora", ns("CostosTrilladora"), value = 400),
                        .input_fila("Gastos Fijos",          ns("GastosFijos"),      value = 200),
                        .input_fila("Fletes",                ns("Fletes"),           value = 200),
                        .input_fila("Margen Esperado",        ns("MargenEsperado"),  value = 200)
             )
      ),
      # Columna derecha - Outputs
      column(6,
             .calc_card("Precios Calculados",
                        .output_fila("Precio 1D7",  ns("Precio_1d7")),
                        .output_fila("Precio 5D5",  ns("Precio_5d5")),
                        .output_fila("Precio 5D50", ns("Precio_5d50")),
                        .output_fila("Precio 9D1",  ns("Precio_9d1")),
                        .output_fila("Precio 9D3",  ns("Precio_9d3"))
             ),
             .calc_card("Viabilidad de Negocios",
                        fluidRow(
                          .header_col("Producto", 4, left = TRUE),
                          .header_col("Precio",   2),
                          .header_col("Kilos",    2),
                          .header_col("Margen",   2),
                          .header_col("Utilidad", 2)
                        ),
                        br(),
                        fluidRow(class = "calc-producto-row",
                                 column(4, FormatearTexto("1D7")),
                                 column(2, autonumericInput(ns("Pre1D7"), NULL, 0, currencySymbol = "$", width = "100%")),
                                 column(2, autonumericInput(ns("Kls1D7"), NULL, 0, width = "100%")),
                                 column(2, class = "calc-output-center", uiOutput(ns("Mar1D7"))),
                                 column(2, class = "calc-output-center", uiOutput(ns("Utl1D7")))
                        ),
                        fluidRow(class = "calc-producto-row",
                                 column(4, FormatearTexto("5D5")),
                                 column(2, autonumericInput(ns("Pre5D5"), NULL, 0, currencySymbol = "$", width = "100%")),
                                 column(2, autonumericInput(ns("Kls5D5"), NULL, 0, width = "100%")),
                                 column(2, class = "calc-output-center", uiOutput(ns("Mar5D5"))),
                                 column(2, class = "calc-output-center", uiOutput(ns("Utl5D5")))
                        ),
                        fluidRow(class = "calc-producto-row",
                                 column(4, FormatearTexto("5D50")),
                                 column(2, autonumericInput(ns("Pre5D50"), NULL, 0, currencySymbol = "$", width = "100%")),
                                 column(2, autonumericInput(ns("Kls5D50"), NULL, 0, width = "100%")),
                                 column(2, class = "calc-output-center", uiOutput(ns("Mar5D50"))),
                                 column(2, class = "calc-output-center", uiOutput(ns("Utl5D50")))
                        ),
                        fluidRow(class = "calc-producto-row",
                                 column(4, FormatearTexto("9D1")),
                                 column(2, autonumericInput(ns("Pre9D1"), NULL, 0, currencySymbol = "$", width = "100%")),
                                 column(2, autonumericInput(ns("Kls9D1"), NULL, 0, width = "100%")),
                                 column(2, class = "calc-output-center", uiOutput(ns("Mar9D1"))),
                                 column(2, class = "calc-output-center", uiOutput(ns("Utl9D1")))
                        ),
                        fluidRow(class = "calc-producto-row",
                                 column(4, FormatearTexto("9D3")),
                                 column(2, autonumericInput(ns("Pre9D3"), NULL, 0, currencySymbol = "$", width = "100%")),
                                 column(2, autonumericInput(ns("Kls9D3"), NULL, 0, width = "100%")),
                                 column(2, class = "calc-output-center", uiOutput(ns("Mar9D3"))),
                                 column(2, class = "calc-output-center", uiOutput(ns("Utl9D3")))
                        ),
                        br()
             ),
             .boton_calcular(ns)
      )
    )
  )
}

CalculadoraCoproductos <- function(id, dat_ind, usr) {
  moduleServer(id, function(input, output, session) {
    
    # Cargar defaults al cambiar segmento
    observeEvent(input$Segmento, ignoreNULL = FALSE, {
      aux1 <- dat_ind()
      
      get_val <- function(item) {
        val <- aux1 %>% filter(Item == item) %>% pull(Valor)
        if (length(val) == 0) NA else val
      }
      
      val_9d1  <- get_val("Precio Soluble (Compras)")
      val_5d5  <- get_val("Precio Molidos (Compras)")
      val_1d7  <- get_val("Precio Consumo (Compras)")
      val_9d3  <- if (!is.na(val_9d1)) val_9d1 - 1000 else NA
      val_5d50 <- if (!any(is.na(c(val_9d1, val_5d5)))) mean(c(val_9d1, val_5d5)) else NA
      
      margen_esperado <- tryCatch({
        CargarDatos("CRMNALCLIENTE") %>%
          filter(LinNegCod == 10000, Segmento == input$Segmento) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          filter(FecProceso == max(FecProceso)) %>%
          group_by(Segmento) %>%
          summarise(
            MargenKilo = sum(MNFCCPpto, na.rm = TRUE) / (sum(SSPpto, na.rm = TRUE) * 70),
            .groups = "drop"
          ) %>%
          pull(MargenKilo)
      }, error = function(e) 200)
      
      gast_fijos <- ifelse(input$Segmento == "DETAL", 450, 200)
      
      updateAutonumericInput(session, "COSTO_9D3",      value = val_9d3)
      updateAutonumericInput(session, "COSTO_9D1",      value = val_9d1)
      updateAutonumericInput(session, "COSTO_5D50",     value = val_5d50)
      updateAutonumericInput(session, "COSTO_5D5",      value = val_5d5)
      updateAutonumericInput(session, "COSTO_1D7",      value = val_1d7)
      updateAutonumericInput(session, "MargenEsperado", value = margen_esperado)
      updateAutonumericInput(session, "GastosFijos",    value = gast_fijos)
    })
    
    # Cálculos disparados por botón
    val_calculadora <- eventReactive(input$Calcular, ignoreNULL = FALSE, {
      req(input$COSTO_9D3, input$COSTO_9D1, input$COSTO_5D50,
          input$COSTO_5D5, input$COSTO_1D7, input$CostosTrilladora,
          input$GastosFijos, input$Fletes, input$MargenEsperado)
      
      data.frame(
        val_9d3  = input$COSTO_9D3,  val_9d1  = input$COSTO_9D1,
        val_5d50 = input$COSTO_5D50, val_5d5  = input$COSTO_5D5,
        val_1d7  = input$COSTO_1D7,
        Trilladora     = input$CostosTrilladora,
        GastosFijos    = input$GastosFijos,
        Fletes         = input$Fletes,
        MargenEsperado = input$MargenEsperado,
        Pre9D3  = .inp0(input$Pre9D3),  Pre9D1  = .inp0(input$Pre9D1),
        Pre5D50 = .inp0(input$Pre5D50), Pre5D5  = .inp0(input$Pre5D5),
        Pre1D7  = .inp0(input$Pre1D7),
        Kls9D3  = .inp0(input$Kls9D3),  Kls9D1  = .inp0(input$Kls9D1),
        Kls5D50 = .inp0(input$Kls5D50), Kls5D5  = .inp0(input$Kls5D5),
        Kls1D7  = .inp0(input$Kls1D7)
      ) %>%
        mutate(
          Precio_9d3  = val_9d3  + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_9d1  = val_9d1  + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_5d50 = val_5d50 + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_5d5  = val_5d5  + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_1d7  = val_1d7  + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Mar9D3  = Pre9D3  - Precio_9d3,  Mar9D1  = Pre9D1  - Precio_9d1,
          Mar5D50 = Pre5D50 - Precio_5d50, Mar5D5  = Pre5D5  - Precio_5d5,
          Mar1D7  = Pre1D7  - Precio_1d7,
          Utl9D3  = Mar9D3  * Kls9D3,     Utl9D1  = Mar9D1  * Kls9D1,
          Utl5D50 = Mar5D50 * Kls5D50,    Utl5D5  = Mar5D5  * Kls5D5,
          Utl1D7  = Mar1D7  * Kls1D7
        )
    })
    
    .bind_outputs(output,
                  list(
                    Precio_9d3 = "dollar", Precio_9d1 = "dollar", Precio_5d50 = "dollar",
                    Precio_5d5 = "dollar", Precio_1d7 = "dollar",
                    Mar9D3  = "dollar", Mar9D1  = "dollar", Mar5D50 = "dollar",
                    Mar5D5  = "dollar", Mar1D7  = "dollar",
                    Utl9D3  = "dollar", Utl9D1  = "dollar", Utl5D50 = "dollar",
                    Utl5D5  = "dollar", Utl1D7  = "dollar"
                  ),
                  val_calculadora
    )
  })
}


# Dólares Excelso ----
CalculadoraDolaresExcelsoUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6,
           .calc_card("Parámetros",
                      .input_fila("TRM",                  ns("TRM")),
                      .input_fila("Precio NYC (HT)",       ns("NYC")),
                      .input_fila("Diferencial de Compra", ns("PrimaUGQ")),
                      .input_fila("Prima Mallas",           ns("PrimaMallas")),
                      .input_fila("Prima Especiales",       ns("PrimaEspeciales")),
                      .input_fila("Utilidad",               ns("Utilidad")),
                      .input_fila("Empaque",                ns("Empaque")),
                      .input_fila("Grainpro",               ns("Grainpro")),
                      .input_fila("Flete",                  ns("Flete"))
           )
    ),
    column(6,
           .calc_card("Resultados",
                      .output_fila("Total",            ns("Total")),
                      .output_fila("Precio (USD)",     ns("PrecioUSD")),
                      .output_fila("Precio (COP)",     ns("PrecioCOP")),
                      .output_fila("Precio con Flete", ns("PrecioFlete"))
           ),
           .boton_calcular(ns)
    )
  )
}

CalculadoraDolaresExcelso <- function(id, dat_ind, usr) {
  moduleServer(id, function(input, output, session) {
    
    # Inicialización única de defaults desde indicadores y sistema
    # Nota: get_system_data(uid, pwd) resuelve en scope global del server
    .init_once(function() {
      defaults <- list(
        TRM            = dat_ind() %>% filter(Item == "TRM (Hoja de trabajo)") %>% pull(Valor),
        NYC            = dat_ind() %>% filter(Item == "Precio NYC (HT)") %>% pull(Valor),
        PrimaUGQ       = get_system_data(uid, pwd)$precios_adicionales$Diferencial,
        PrimaMallas    = 0, PrimaEspeciales = 0,
        Utilidad       = 13.39,
        Empaque        = 0, Grainpro = 0, Flete = 200
      )
      for (id_input in names(defaults)) {
        updateAutonumericInput(session, inputId = id_input, value = defaults[[id_input]])
      }
    })
    
    # Cálculos disparados por botón
    val_calculadora <- eventReactive(input$Calcular, ignoreNULL = FALSE, {
      data.frame(
        TRM             = input$TRM,
        NYC             = input$NYC,
        PrimaUGQ        = input$PrimaUGQ,
        PrimaMallas     = input$PrimaMallas,
        PrimaEspeciales = input$PrimaEspeciales,
        Utilidad        = input$Utilidad,
        Empaque         = input$Empaque,
        Grainpro        = input$Grainpro,
        Flete           = input$Flete
      ) %>%
        mutate(
          Total       = NYC + PrimaUGQ + PrimaMallas + PrimaEspeciales + Utilidad + Empaque + Grainpro,
          PrecioUSD   = Total * 2.20462 / 100,
          PrecioCOP   = PrecioUSD * TRM,
          PrecioFlete = PrecioCOP + Flete
        )
    })
    
    .bind_outputs(output,
                  list(Total = "dollar", PrecioUSD = "dollar", PrecioCOP = "dollar", PrecioFlete = "dollar"),
                  val_calculadora
    )
  })
}


# Pesos Excelso ----
CalculadoraPesosExcelsoUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6,
           .calc_card("Parámetros Generales",
                      .input_fila("TRM",                ns("TRM"),           value = 4403),
                      .input_fila("Costo Consumo",      ns("CostoConsumo"),  value = 28000),
                      .input_fila("Precio Carga",       ns("PrecioCarga"),   value = 3107000),
                      .input_fila("Prima Especial +83", ns("PrimaEspecial"), value = 2000),
                      .input_fila("Prima Supremo",      ns("PrimaSupremo"),  value = 1000)
           ),
           .calc_card("Resultados Generales",
                      .output_fila("Precio por Kilo", ns("PrecioKilo")),
                      .output_fila("Rendimiento",     ns("Rendimiento")),
                      .output_fila("Costo por Kilo",  ns("CostoKilo"))
           ),
           .calc_card("Parámetros de Producto",
                      fluidRow(
                        .header_col("Producto", 6, left = TRUE),
                        .header_col("Costo",    2),
                        .header_col("Margen",   2),
                        .header_col("Precio",   2)
                      ),
                      Saltos(),
                      fluidRow(class = "calc-producto-row",
                               column(6, FormatearTexto("Blend")),
                               column(2, class = "calc-output-center", uiOutput(ns("CosBlend"))),
                               column(2, autonumericInput(ns("MarBlend"), NULL, 0, currencySymbol = "$", width = "100%")),
                               column(2, class = "calc-output-center", uiOutput(ns("PreBlend")))
                      ),
                      fluidRow(class = "calc-producto-row",
                               column(6, FormatearTexto("UGQ")),
                               column(2, class = "calc-output-center", uiOutput(ns("CosUGQ"))),
                               column(2, autonumericInput(ns("MarUGQ"), NULL, 0, currencySymbol = "$", width = "100%")),
                               column(2, class = "calc-output-center", uiOutput(ns("PreUGQ")))
                      ),
                      fluidRow(class = "calc-producto-row",
                               column(6, FormatearTexto("Supremo")),
                               column(2, class = "calc-output-center", uiOutput(ns("CosSup"))),
                               column(2, autonumericInput(ns("MarSup"), NULL, 0, currencySymbol = "$", width = "100%")),
                               column(2, class = "calc-output-center", uiOutput(ns("PreSup")))
                      ),
                      fluidRow(class = "calc-producto-row",
                               column(6, FormatearTexto("Especial 83+")),
                               column(2, class = "calc-output-center", uiOutput(ns("CosEsp"))),
                               column(2, autonumericInput(ns("MarEsp"), NULL, 0, currencySymbol = "$", width = "100%")),
                               column(2, class = "calc-output-center", uiOutput(ns("PreEsp")))
                      )
           )
    ),
    column(6,
           .calc_card("Costos y Utilidades",
                      .input_fila("Prima",           ns("Prima"),       value = 2000),
                      .input_fila("Flete Interno",   ns("FleteInt"),    value = 1000),
                      .input_fila("Flete a Cliente", ns("FleteExt"),    value = 300),
                      .input_fila("Costos Fijos",    ns("CostosFijos"), value = 500),
                      .input_fila("Financieros",     ns("Financieros"), value = 1700),
                      br(),
                      .output_fila("Costos",             ns("Costos2")),
                      .input_fila("Precio",              ns("Precio"),    value = 39420),
                      .output_fila("Utilidad",           ns("Utilidad2")),
                      .output_fila("Utilidad (cts/lb)",  ns("UtilidadCtvs2"))
           ),
           .boton_calcular(ns)
    )
  )
}

CalculadoraPesosExcelso <- function(id, dat_ind, usr) {
  moduleServer(id, function(input, output, session) {
    
    # Control de acceso a márgenes según usuario
    observe({
      permitido <- usr() %in% c("CMEDINA", "JGCANON")
      fn <- if (permitido) shinyjs::enable else shinyjs::disable
      lapply(c("MarBlend", "MarUGQ", "MarSup", "MarEsp"), fn)
    })
    
    # Inicialización única de defaults desde indicadores y BD de márgenes
    .init_once(function() {
      defaults <- list(
        TRM           = dat_ind() %>%
          filter(Item == "TRM (Hoja de trabajo)") %>% pull(Valor),
        CostoConsumo  = dat_ind() %>%
          filter(Item == "Precio Consumo (Compras)") %>% pull(Valor),
        PrecioCarga   = dat_ind() %>%
          filter(Item == "Precio Carga (Promedio de últimas entradas del día)") %>% pull(Valor),
        PrimaEspecial = 2000, Prima = 2500,
        FleteInt = 500, FleteExt = 500, CostosFijos = 500, Financieros = 1700, Precio = 40000
      )
      for (id_input in names(defaults)) {
        updateAutonumericInput(session, inputId = id_input, value = defaults[[id_input]])
      }
      
      # Márgenes persistidos en BD
      aux_mar <- CargarDatos("CRMNALCPMAR") %>% filter(FechaHoraCrea == max(FechaHoraCrea))
      lapply(c("MarBlend", "MarUGQ", "MarSup", "MarEsp"), function(inp) {
        updateAutonumericInput(session, inputId = inp, value = aux_mar[, inp])
      })
    })
    
    # Cálculos disparados por botón
    val_calculadora <- eventReactive(input$Calcular, ignoreNULL = FALSE, {
      req(input$TRM, input$CostoConsumo, input$PrecioCarga,
          input$PrimaEspecial, input$Prima, input$FleteInt,
          input$FleteExt, input$CostosFijos, input$Financieros, input$Precio)
      
      data.frame(
        TRM           = input$TRM,
        CostoConsumo  = input$CostoConsumo,
        PrecioCarga   = input$PrecioCarga,
        PrimaEspecial = input$PrimaEspecial,
        PrimaSupremo  = .inp0(input$PrimaSupremo),
        MarBlend      = .inp0(input$MarBlend),
        MarUGQ        = .inp0(input$MarUGQ),
        MarSup        = .inp0(input$MarSup),
        MarEsp        = .inp0(input$MarEsp),
        Prima         = input$Prima,
        FleteInt      = input$FleteInt,
        FleteExt      = input$FleteExt,
        CostosFijos   = input$CostosFijos,
        Financieros   = input$Financieros,
        Precio        = input$Precio
      ) %>%
        mutate(
          PrecioKilo    = PrecioCarga / 125,
          Rendimiento   = 70 / 96.89,
          CostoKilo     = PrecioKilo / Rendimiento,
          Costos        = CostoKilo + PrimaEspecial + FleteInt + Financieros,
          CosUGQ        = CostoKilo,
          PrecioUGQ     = CosUGQ + MarUGQ,
          UtilidadKilo  = PrecioUGQ - Costos,
          UtilidadCtvs  = ((UtilidadKilo * 70) / 1.54322) / TRM,
          CosBlend      = (CostoConsumo * 0.4) + (CosUGQ * 0.6),
          PreBlend      = CosBlend + MarBlend,
          UtBlend       = (((PreBlend - CosBlend) * 70) / 1.54322) / TRM,
          PreUGQ        = CosUGQ + MarUGQ,
          UtUGQ         = (((PreUGQ - CosUGQ) * 70) / 1.54322) / TRM,
          CosSup        = CosUGQ + PrimaSupremo,
          PreSup        = CosSup + MarSup,
          UtSup         = (((PreSup - CosSup) * 70) / 1.54322) / TRM,
          CosEsp        = CosUGQ + PrimaEspecial,
          PreEsp        = CosEsp + MarEsp,
          UtEsp         = (((PreEsp - CosEsp) * 70) / 1.54322) / TRM,
          Costos2       = CostoKilo + Prima + FleteInt + FleteExt + CostosFijos + Financieros,
          Utilidad2     = Precio - Costos2,
          UtilidadCtvs2 = ((Utilidad2 * 70) / 1.54322) / TRM
        )
    })
    
    .bind_outputs(output,
                  list(
                    PrecioKilo    = "dollar",  Rendimiento   = "percent", CostoKilo    = "dollar",
                    Costos        = "dollar",  PrecioUGQ     = "dollar",  UtilidadKilo = "dollar",
                    UtilidadCtvs  = "dollar",  CosBlend      = "dollar",  PreBlend     = "dollar",
                    UtBlend       = "dollar",  CosUGQ        = "dollar",  PreUGQ       = "dollar",
                    UtUGQ         = "dollar",  CosSup        = "dollar",  PreSup       = "dollar",
                    UtSup         = "dollar",  CosEsp        = "dollar",  PreEsp       = "dollar",
                    UtEsp         = "dollar",  Costos2       = "dollar",  Utilidad2    = "dollar",
                    UtilidadCtvs2 = "dollar"
                  ),
                  val_calculadora
    )
    
    # Persistir márgenes en BD al calcular (solo usuarios autorizados)
    observeEvent(input$Calcular, {
      if (usr() %in% c("CMEDINA", "JGCANON")) {
        SubirDatos(
          data.frame(
            UsuarioCrea   = usr(),
            FechaHoraCrea = Sys.time(),
            MarBlend      = input$MarBlend,
            MarUGQ        = input$MarUGQ,
            MarSup        = input$MarSup,
            MarEsp        = input$MarEsp,
            stringsAsFactors = FALSE
          ),
          "CRMNALCPMAR"
        )
      }
    })
  })
}


# Módulo integrador ----
CalculadoraUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Pesos - Excelso",   Saltos(), CalculadoraPesosExcelsoUI(ns("pesos"))),
    tabPanel("Dólares - Excelso", Saltos(), CalculadoraDolaresExcelsoUI(ns("dolares"))),
    tabPanel("Coproductos",       Saltos(), CalculadoraCoproductosUI(ns("coproductos")))
  )
}

Calculadora <- function(id, dat_ind, usr) {
  moduleServer(id, function(input, output, session) {
    CalculadoraPesosExcelso("pesos",      dat_ind, usr)
    CalculadoraDolaresExcelso("dolares",  dat_ind, usr)
    CalculadoraCoproductos("coproductos", dat_ind, usr)
  })
}
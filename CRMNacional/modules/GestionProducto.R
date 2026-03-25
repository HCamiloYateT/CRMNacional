# Modulos de Detalle ----
EditarProductoUI <- function(id) {
  ns <- NS(id)
  bs4Card(title = "Editar Productos", status = "white", solidHeader = TRUE, width = 12,
          fluidRow(
            column(6, 
                   ListaDesplegable(inputId = ns("linneg"), label   = "Línea de Negocio", choices = Choices()$linneg,
                                    selected = NULL, multiple = FALSE)),
            column(6, 
                   ListaDesplegable(inputId = ns("linpro"), label   = "Línea de Producto", choices = character(0),
                                    multiple = FALSE))
          ),
          fluidRow(
            column(6, 
                   ListaDesplegable(inputId = ns("mc"), label   = "Nombre Comercial", choices = character(0), 
                                    multiple = FALSE)),
            column(6, 
                   ListaDesplegable(inputId = ns("marca"), label   = "Marca", choices = character(0),
                                    multiple = FALSE))
          ),
          hr(),
          fluidRow(
            column(4, 
                   ListaDesplegable(inputId = ns("excluir"), label   = "Excluir", choices = c("SI", "NO"), 
                                    selected = "NO", multiple = FALSE)),
            column(4, 
                   ListaDesplegable(inputId = ns("categoria"), label   = "Categoría", choices = Choices()$categoria,
                                    selected = NULL, multiple = FALSE)),
            column(4, 
                   ListaDesplegable(inputId = ns("producto"), label   = "Producto", choices = character(0),
                                    multiple = FALSE))
          ),
          div(style = "text-align: right; margin-top: 10px;",
              div(style = "display: inline-block; width: 40%;",
                  actionBttn(inputId = ns("guardar"), label   = "Guardar Registro", style   = "unite",
                             color   = "danger", size    = "xs", icon    = icon("save"),
                             block   = FALSE)
              )
          )
  )
}
EditarProducto <- function(id, dd_data = reactive(NULL), usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Flag para controlar si dd_data tiene valores
    has_dd_data <- reactiveVal(FALSE)
    
    # Cargar datos una sola vez al inicio
    MARCOM <- ConsultaSistema("syscafe", query = "SELECT MCCod, MCNom FROM NMARCOM")
    NMARCAS <- ConsultaSistema("syscafe", query = "SELECT MrcCod, MrcNom AS Marca FROM NMARCAS")
    NLINEANE <- ConsultaSistema("syscafe", query = "SELECT LinNegCod, LinNegNom AS LinNeg FROM NLINEANE WHERE CiaCod = 10")
    NLINPRO <- ConsultaSistema("syscafe", query = "SELECT LinProCod, LinProNom FROM NTIPPROD WHERE CiaCod = 10")
    
    df_base <- data %>% 
      select(LinNegCod, LinProCod, MCCod, MrcCod) %>% distinct() %>% 
      left_join(NLINEANE %>% select(LinNegCod, LinNeg), by = "LinNegCod") %>%
      left_join(NLINPRO  %>% select(LinProCod, LinProNom), by = "LinProCod") %>%
      left_join(MARCOM   %>% select(MCCod, MCNom), by = "MCCod") %>%
      left_join(NMARCAS  %>% select(MrcCod, Marca), by = "MrcCod") 
    
    df_prods <- CargarDatos("CRMNALPRODS")
    
    # Crear listas pre-calculadas para búsquedas rápidas
    lookup_lists <- list(
      linneg_to_linpro = split(df_base$LinProNom, df_base$LinNeg),
      linpro_to_mc = split(df_base$MCNom, paste(df_base$LinNeg, df_base$LinProNom, sep = "|")),
      mc_to_marca = split(df_base$Marca, paste(df_base$LinNeg, df_base$LinProNom, df_base$MCNom, sep = "|")),
      categoria_to_producto = split(df_prods$Producto, df_prods$Categoria)
    )
    
    # Observar cambios en dd_data y actualizar flag
    observe({
      vals <- dd_data()
      has_dd_data(!is.null(vals))
    })
    
    # Inicializar inputs desde dd_data
    observeEvent(dd_data(), {
      vals <- dd_data()$data
      req(vals)
      
      linneg_ini <- vals$LinNeg[1]
      linpro_ini <- vals$LinProNom[1]
      mc_ini     <- vals$MCNom[1]
      marca_ini  <- vals$Marca[1]
      
      # Usar isolate para evitar reactividad durante la inicialización
      isolate({
        # LinNeg
        if (!is.null(linneg_ini) && linneg_ini %in% df_base$LinNeg) {
          updatePickerInput(session, "linneg", selected = linneg_ini)
        }
        
        # LinPro - usar lookup pre-calculado
        if (!is.null(linpro_ini) && !is.null(linneg_ini)) {
          choices_linpro <- unique(lookup_lists$linneg_to_linpro[[linneg_ini]])
          if (linpro_ini %in% choices_linpro) {
            updatePickerInput(session, "linpro",
                              choices  = choices_linpro,
                              selected = linpro_ini)
          }
        }
        
        # MC - usar lookup pre-calculado
        if (!is.null(mc_ini) && !is.null(linneg_ini) && !is.null(linpro_ini)) {
          key_mc <- paste(linneg_ini, linpro_ini, sep = "|")
          choices_mc <- unique(lookup_lists$linpro_to_mc[[key_mc]])
          if (mc_ini %in% choices_mc) {
            updatePickerInput(session, "mc",
                              choices  = choices_mc,
                              selected = mc_ini)
          }
        }
        
        # Marca - usar lookup pre-calculado
        if (!is.null(marca_ini) && !is.null(linneg_ini) && !is.null(linpro_ini) && !is.null(mc_ini)) {
          key_marca <- paste(linneg_ini, linpro_ini, mc_ini, sep = "|")
          choices_marca <- unique(lookup_lists$mc_to_marca[[key_marca]])
          if (marca_ini %in% choices_marca) {
            updatePickerInput(session, "marca",
                              choices  = choices_marca,
                              selected = marca_ini)
          }
        }
      })
      
    }, ignoreInit = FALSE)
    
    # Actualizaciones jerárquicas - SOLO cuando dd_data es NULL
    observeEvent(input$linneg, {
      req(input$linneg)
      req(!has_dd_data())
      
      choices_linpro <- unique(lookup_lists$linneg_to_linpro[[input$linneg]])
      choices_categoria <- unique(df_base$Categoria[df_base$LinNeg == input$linneg])
      
      updatePickerInput(session, "linpro", choices = choices_linpro, selected = NULL)
      updatePickerInput(session, "categoria", choices = choices_categoria, selected = NULL)
      updatePickerInput(session, "mc", choices = character(0))
      updatePickerInput(session, "marca", choices = character(0))
    })
    observeEvent(input$linpro, {
      req(input$linneg, input$linpro)
      req(!has_dd_data())
      
      key <- paste(input$linneg, input$linpro, sep = "|")
      choices_mc <- unique(lookup_lists$linpro_to_mc[[key]])
      
      updatePickerInput(session, "mc", choices = choices_mc, selected = NULL)
      updatePickerInput(session, "marca", choices = character(0))
    })
    observeEvent(input$mc, {
      req(input$linneg, input$linpro, input$mc)
      req(!has_dd_data())
      
      key <- paste(input$linneg, input$linpro, input$mc, sep = "|")
      choices_marca <- unique(lookup_lists$mc_to_marca[[key]])
      
      updatePickerInput(session, "marca", choices = choices_marca, selected = NULL)
    })
    observeEvent(input$categoria, {
      req(input$categoria)
      
      choices_producto <- unique(lookup_lists$categoria_to_producto[[input$categoria]])
      updatePickerInput(session, "producto", choices = choices_producto, selected = NULL)
    })
    
    # Guardado
    observeEvent(input$guardar, {
      req(input$linneg, input$linpro, input$mc, input$marca)
      req(input$excluir, input$categoria, input$producto)
      
      # Búsqueda más eficiente usando match
      idx_linneg <- match(input$linneg, df_base$LinNeg)
      idx_linpro <- match(input$linpro, df_base$LinProNom)
      idx_mc <- match(input$mc, df_base$MCNom)
      idx_marca <- match(input$marca, df_base$Marca)
      
      nuevo <- tibble::tibble(
        FecProceso = Sys.Date(),
        Usr        = usr(),
        LinNegCod  = df_base$LinNegCod[idx_linneg],
        LinNeg     = input$linneg,
        LinProCod  = df_base$LinProCod[idx_linpro],
        LinProNom  = input$linpro,
        MCCod      = df_base$MCCod[idx_mc],
        MCNom      = input$mc,
        MrcCod     = df_base$MrcCod[idx_marca],
        Marca      = input$marca,
        Excluir    = input$excluir,
        Categoria  = input$categoria,
        Producto   = input$producto
      )
      
      AgregarDatos(nuevo, "CRMNALPRODS")
      showNotification("Producto Modificado", duration = 4, type = "message")
    })
  })
}

DashboardProductoUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      bs4Card(title = "Filtros de Producto", width = 12, status = "white", solidHeader = TRUE,
              fluidRow(
                column(3,
                       ListaDesplegable(inputId = ns("linneg"), label   = "Línea de Negocio", choices = Choices()$linneg,
                                        selected = NULL, multiple = FALSE)),
                column(3,
                       ListaDesplegable(inputId = ns("linpro"), label   = "Línea de Producto", choices = character(0),
                                        multiple = FALSE)),
                column(3,
                       ListaDesplegable(inputId = ns("mc"), label   = "Nombre Comercial", choices = character(0),
                                        multiple = FALSE)
                       ),
                column(3,
                       ListaDesplegable(inputId = ns("marca"), label   = "Marca", choices = character(0),
                                        multiple = FALSE)
                       )
                )
              )
      ),
    fluidRow(bs4ValueBoxOutput(ns("vb_sacos"),    width = 3),
             bs4ValueBoxOutput(ns("vb_margen"),   width = 3),
             bs4ValueBoxOutput(ns("vb_clientes"), width = 3),
             bs4ValueBoxOutput(ns("vb_lotes"),    width = 3)
             ),
    fluidRow(bs4Card(title = "Serie Histórica", width = 12, status = "white", solidHeader = TRUE,
                     fluidRow(
                       column(12,
                              BotonesRadiales("serie_tipo", 
                                              choices  = c("Sacos", "Margen", "Clientes", "Lotes"),
                                              selected = "Sacos",
                                              ns = ns, alineacion = "right")
                              )
                       ),
                     fluidRow(
                       plotlyOutput(ns("serie_unica"), height = "350px")
                       )
                     )
             ),
    fluidRow(
      bs4Card(title = "Detalle de Clientes",
              width = 12, status = "white", solidHeader = TRUE,
              div(style = "max-height:500px; overflow-y:auto; overflow-x:auto; width:100%;",
                  gt_output(ns("tabla_clientes"))
                  )
              )
      ),
    fluidRow(
      bs4Card(title = "Detalle de Lotes",
              width = 12, status = "white", solidHeader = TRUE,
              div(style = "max-height:500px; overflow-y:auto; overflow-x:auto; width:100%;",
                  gt_output(ns("tabla_lotes"))
                  )
              )
      )
    )
}
DashboardProducto <- function(id, dd_data = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
   
    has_dd_data <- reactiveVal(FALSE)

    # Dimensiones básicas
    MARCOM   <- ConsultaSistema("syscafe", query = "SELECT MCCod, MCNom FROM NMARCOM")
    NMARCAS  <- ConsultaSistema("syscafe", query = "SELECT MrcCod, MrcNom AS Marca FROM NMARCAS")
    NLINEANE <- ConsultaSistema("syscafe", query = "SELECT LinNegCod, LinNegNom AS LinNeg FROM NLINEANE WHERE CiaCod = 10")
    NLINPRO  <- ConsultaSistema("syscafe", query = "SELECT LinProCod, LinProNom FROM NTIPPROD WHERE CiaCod = 10")
    df_prods <- CargarDatos("CRMNALPRODS") %>%
      mutate(FecProceso = as.Date(FecProceso)) %>%
      group_by(LinNegCod, LinProCod, MCCod, MrcCod) %>%
      filter(FecProceso == max(FecProceso), .preserve = TRUE) %>%
      ungroup() 
    segs <-  CargarDatos("CRMNALSEGR") %>%
        mutate(FecProceso = as.Date(FecProceso)) %>%
        filter(FecProceso == max(FecProceso)) %>%
        select(LinNegCod, CliNitPpal, SegmentoRacafe)
    clientes <- CargarDatos("CRMNALCLIENTE") %>%
        mutate(FecProceso = as.Date(FecProceso),
               across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
               across(where(is.character), ~ifelse(is.na(.) | . == "N/A", "", .))) %>% 
        group_by(LinNegCod, CliNitPpal) %>% 
        filter(FecProceso == max(FecProceso),
               CliNitPpal == CLCliNit)
      
    # Actualización de Inputs ----
    df_base <- reactive({
      data %>%
        left_join(NLINEANE, by = "LinNegCod") %>%
        left_join(NLINPRO,  by = "LinProCod") %>%
        left_join(MARCOM,   by = "MCCod") %>%
        left_join(NMARCAS,  by = "MrcCod") %>%
        select(LinNeg, LinProNom, MCNom, Marca) %>%
        distinct() 
    })
    lookup_lists <- reactive({
      base <- df_base() 
      list(
        linneg_to_linpro      = split(base$LinProNom, base$LinNeg),
        linpro_to_mc          = split(base$MCNom, paste(base$LinNeg, base$LinProNom, sep = "|")),
        mc_to_marca           = split(base$Marca, paste(base$LinNeg, base$LinProNom, base$MCNom, sep = "|")),
        categoria_to_producto = split(df_prods$Producto, df_prods$Categoria)
      )
    })

    observe({
      vals <- dd_data()
      has_dd_data(!is.null(vals))
    })
    # Inicialización desde dd_data
    observeEvent(dd_data(), {
      vals <- dd_data()$data
      req(vals)

      linneg_ini <- vals$LinNeg[1]
      linpro_ini <- vals$LinProNom[1]
      mc_ini     <- vals$MCNom[1]
      marca_ini  <- vals$Marca[1]

      iso <- isolate

      iso({
        # LinNeg
        if (!is.null(linneg_ini) && linneg_ini %in% df_base()$LinNeg) {
          updatePickerInput(session, "linneg", selected = linneg_ini)
        }

        # LinPro
        ch_lp <- unique(lookup_lists()$linneg_to_linpro[[linneg_ini]])
        updatePickerInput(session, "linpro", choices = ch_lp, selected = linpro_ini)

        # MC
        key_mc <- paste(linneg_ini, linpro_ini, sep = "|")
        ch_mc <- unique(lookup_lists()$linpro_to_mc[[key_mc]])
        updatePickerInput(session, "mc", choices = ch_mc, selected = mc_ini)

        # Marca
        key_ma <- paste(linneg_ini, linpro_ini, mc_ini, sep = "|")
        ch_ma <- unique(lookup_lists()$mc_to_marca[[key_ma]])
        updatePickerInput(session, "marca", choices = ch_ma, selected = marca_ini)
      })
    }, ignoreInit = FALSE)

    # Actualizaciones jerárquicas SOLO si dd_data es NULL
    observeEvent(input$linneg, {
      req(!has_dd_data())

      ch_lp <- unique(lookup_lists()$linneg_to_linpro[[input$linneg]])

      updatePickerInput(session, "linpro", choices = ch_lp, selected = NULL)
      updatePickerInput(session, "mc", choices = character(0))
      updatePickerInput(session, "marca", choices = character(0))
    })
    observeEvent(input$linpro, {
      req(!has_dd_data())

      key <- paste(input$linneg, input$linpro, sep = "|")
      ch_mc <- unique(lookup_lists()$linpro_to_mc[[key]])

      updatePickerInput(session, "mc", choices = ch_mc, selected = NULL)
      updatePickerInput(session, "marca", choices = character(0))
    })
    observeEvent(input$mc, {
      req(!has_dd_data())

      key <- paste(input$linneg, input$linpro, input$mc, sep = "|")
      ch_ma <- unique(lookup_lists()$mc_to_marca[[key]])

      updatePickerInput(session, "marca", choices = ch_ma, selected = NULL)
    })

    # Datos filtrados -----
    df_f <- reactive({
      req(input$linneg, input$linpro, input$mc, input$marca)
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      data %>%
        left_join(NLINEANE, by = join_by(LinNegCod)) %>% 
        left_join(NLINPRO, by = join_by(LinProCod)) %>% 
        left_join(MARCOM, by = join_by(MCCod)) %>% 
        left_join(NMARCAS, by = join_by(MrcCod)) %>% 
        filter(CLLinNegNo == input$linneg,
               LinProNom  == input$linpro,
               MCNom == input$mc,
               Marca == input$marca) %>%
        left_join(df_prods %>% select(LinNegCod, LinProCod, MCCod, MrcCod, Categoria, 
                                      Producto, ProdExcluir = Excluir), 
                  by = join_by(LinNegCod, LinProCod, MCCod, MrcCod)) %>% 
        left_join(segs, by = join_by(LinNegCod, CliNitPpal)) %>% 
        left_join(clientes %>% select(FecProceso, LinNegCod, CliNitPpal, Asesor, Responsable, Segmento,
                                  EstadoCliente, EstadoNegocio, RazonDescartado, DescartadoPrecio,
                                  DescartadoCalidad, FrecuenciaDias, NumMesesRecuperar,
                                  PptoSacos = SSPpto, PptoMargen = MNFCCPpto, Excluir), 
                  by = join_by(LinNegCod, CliNitPpal))
    })

    # KPIs -----
    output$vb_sacos <- renderbs4ValueBox({
      val_sacos <- sum(df_f()$SacosFact, na.rm = TRUE)
      CajaValor(valor = val_sacos, formato = "coma", texto = "Sacos Facturados", 
                icono = "warehouse", mostrar_boton = FALSE
      )
    })
    output$vb_margen <- renderbs4ValueBox({
      val_margen <- sum(df_f()$Margen, na.rm = TRUE)
      CajaValor(valor = val_margen, formato = "dinero", texto = "Margen Total", 
                icono = "dollar-sign", mostrar_boton = FALSE)
    })
    output$vb_clientes <- renderbs4ValueBox({
      val_clientes <- n_distinct(df_f()$PerRazSoc)
      CajaValor(valor = val_clientes, formato = "coma", texto = "Clientes",
                icono = "users", mostrar_boton = FALSE)
      })
    output$vb_lotes <- renderbs4ValueBox({
      val_lotes <- n_distinct(df_f()$CLLotCod)
      CajaValor(valor = val_lotes, formato = "coma", texto = "Lotes",
                icono = "layer-group", mostrar_boton = FALSE)
      })
    
    # Grafica ----
    df_serie <- reactive({
      req(df_f())
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      df <- df_f() %>%
        mutate(Mes = floor_date(FecFact, "month")) %>%
        group_by(Mes) %>%
        summarise(Sacos  = sum(SacosFact, na.rm = TRUE),
                  Margen = sum(Margen,    na.rm = TRUE),
                  Clientes = n_distinct(PerRazSoc),
                  Lotes    = n_distinct(CLLotCod),
                  .groups = "drop")
      
      # Rango completo de meses
      meses_full <- tibble(Mes = seq(min(df$Mes, na.rm = TRUE), max(df$Mes, na.rm = TRUE), by = "month"))
      
      df_full <- meses_full %>%
        left_join(df, by = "Mes") %>%
        mutate(Sacos = replace_na(Sacos, 0),
               Margen = replace_na(Margen, 0),
               Clientes = replace_na(Clientes, 0),
               Lotes = replace_na(Lotes, 0)
               )
      
      df_full
    })
    output$serie_unica <- renderPlotly({
      req(df_serie(), input$serie_tipo)
      
      df <- df_serie()
      
      # Métrica seleccionada
      metrica <- switch(input$serie_tipo, 
                        "Sacos"    = "Sacos",
                        "Margen"   = "Margen",
                        "Clientes" = "Clientes",
                        "Lotes"    = "Lotes")
      
      # Formato numérico según métrica
      formato_d3 <- switch(input$serie_tipo,
                           "Sacos"    = FormatoD3("coma"),
                           "Margen"   = FormatoD3("dinero"),
                           "Clientes" = FormatoD3("coma"),
                           "Lotes"    = FormatoD3("coma"))
      
      # Fechas en español
      df <- df %>%
        mutate(Mes_ES = format(Mes, "%d-%b-%Y", locale = "es_ES.UTF-8"))
      
      plot_ly(data = df, x    = ~Mes, y    = df[[metrica]], type = "scatter",
              mode = "lines+markers", name = metrica, line = list(width = 2),
              hovertemplate = paste0("<b>", metrica, ":</b> %{y:", formato_d3, "}<br>",
                                     "<b>Mes:</b> %{x|%d-%b-%Y}<extra></extra>"
                                     )) %>%
        layout(xaxis = list(title = "", tickformat = "%b-%Y", ticklabelmode = "period"),
               yaxis = list(title = metrica, tickformat = formato_d3),
               hovermode = "x unified", plot_bgcolor = "white",
               paper_bgcolor = "white") %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    
    
    
    
    
    # Clientes ----
    output$tabla_clientes <- render_gt({
      df <- df_f()
      req(df)
      
      df_clientes <- df %>%
        group_by(CliNitPpal, PerRazSoc, CLLinNegNo) %>%
        summarise(SegmentoRacafe = first(SegmentoRacafe),
                  Segmento = first(Segmento),
                  Asesor = first(Asesor),
                  Responsable = first(Responsable),          
                  EstadoCliente = first(EstadoCliente),
                  UltFact = max(FecFact, na.rm = TRUE),
                  SacosTotal = sum(SacosFact, na.rm = TRUE),
                  MargenTotal = sum(Margen, na.rm = TRUE),
                  PptoSacos = first(PptoSacos),
                  PptoMargen = first(PptoMargen),
                  .groups = "drop") %>%
        mutate(Cliente = PerRazSoc) %>% 
        crear_link_cliente(col_razsoc = "Cliente", col_linneg = "CLLinNegNo") %>%
        select(Cliente, `Línea de Negocio` = CLLinNegNo, SegmentoRacafe, Segmento,
               Asesor, Responsable, EstadoCliente, `Última Facturación` = UltFact, SacosTotal,
               MargenTotal, `Presupuesto Sacos`  = PptoSacos, `Presupuesto Margen` = PptoMargen
               )
      
      df_clientes %>%
        gt() %>%
        tab_header(title = "Detalle de Clientes", subtitle = paste("Total clientes:", nrow(df_clientes))) %>%
        fmt_number(columns = c(SacosTotal, `Presupuesto Sacos`), decimals = 0) %>%
        fmt_currency(columns = c(MargenTotal, `Presupuesto Margen`), currency = "COP", decimals = 0) %>%
        fmt_markdown(columns = Cliente) %>% 
        tab_style(style = cell_text(weight = "bold"), 
                  locations = cells_body(columns = c(Cliente, SacosTotal, MargenTotal)
                                         )) %>%
        gt_minimal_style() %>% 
        opt_interactive(use_pagination = FALSE, use_filters = FALSE)
    })
    
    
    # Lotes ----
    output$tabla_lotes <- render_gt({
      df <- df_f()
      req(df)
      
      df_lotes <- df %>%
        group_by(CLLotCod, Sucursal, CLLinNegNo, MCNom, Marca) %>%
        summarise(SacosTotal = sum(SacosFact, na.rm = TRUE),
                  MargenTotal = sum(Margen, na.rm = TRUE),
                  FecAsignUlt = max(FecAsignLote, na.rm = TRUE),
                  FecProdUlt = max(FecProd, na.rm = TRUE),
                  FecDespUlt = max(FecDesp, na.rm = TRUE),
                  FecFactUlt = max(FecFact, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(across(.cols = c(FecAsignUlt, FecProdUlt, FecDespUlt, FecFactUlt),
                      ~ if_else(is.infinite(.), NA, .))) %>%
        select(Lote = CLLotCod, Sucursal, `Línea de Negocio` = CLLinNegNo,
               SacosTotal, MargenTotal, `Asignación` = FecAsignUlt,
               `Producción` = FecProdUlt, `Despacho` = FecDespUlt,
               `Facturación` = FecFactUlt)
      
      df_lotes %>%
        gt() %>%
        tab_header(title = "Detalle de Lotes",
                   subtitle = paste("Total lotes:", nrow(df_lotes))) %>%
        fmt_number(columns = SacosTotal, decimals = 0) %>%
        fmt_currency(columns = MargenTotal, currency = "COP", decimals = 0) %>%
        fmt_date(columns = c(`Asignación`, `Producción`, `Despacho`, `Facturación`),
                 date_style = 3) %>%
        tab_style(style = cell_text(weight = "bold"),
                  locations = cells_body(columns = c(Lote, SacosTotal, MargenTotal))
                  ) %>%
        gt_minimal_style() %>% 
        opt_interactive(use_pagination = FALSE, use_filters = FALSE)
    })
    
  })
}

# Modulo Prinicipal ------
GestionProductoUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        box(
          title = "Marcas sin Categoria/Producto",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          GTBotonesUI(ns("TablaProductos"))
        )
      )
    ),
    fluidRow(
      column(
        12,
        EditarProductoUI(ns("EdicionIndividual"))
      )
    )
  )
}
GestionProducto <- function(id, dat, usr) {
  moduleServer(id, function(input, output, session) {
    
    # Tabla ----
    data_completa <- reactive({
      
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      
      MARCOM   <- ConsultaSistema("syscafe", query = "SELECT MCCod, MCNom FROM NMARCOM")
      NMARCAS  <- ConsultaSistema("syscafe", query = "SELECT MrcCod, MrcNom AS Marca FROM NMARCAS")
      NLINEANE <- ConsultaSistema(
        "syscafe",
        query = "SELECT LinNegCod, LinNegNom AS LinNeg FROM NLINEANE WHERE CiaCod = 10"
      )
      NLINPRO <- ConsultaSistema(
        "syscafe",
        query = "SELECT LinProCod, LinProNom FROM NTIPPROD WHERE CiaCod = 10"
      )
      
      linnegcod <- dplyr::case_when(
        usr() %in% c("GACORREDOR", "CMEDINA") ~ 10000,
        usr() %in% c("LABOYACA", "JGCANON")   ~ 21000,
        TRUE                                 ~ c(10000, 21000)
      )
      
      dat() %>%
        dplyr::filter(
          LinNegCod %in% linnegcod,
          is.na(Categoria) | Categoria %in% c("OTROS", "EN BLANCO") |
            is.na(Producto) | Producto %in% c("OTROS", "EN BLANCO")
        ) %>%
        dplyr::select(-c(CLLinNegNo, LinProNom, MCNom, Marca)) %>%
        dplyr::left_join(NLINEANE %>% dplyr::select(LinNegCod, LinNeg),
                         by = "LinNegCod") %>%
        dplyr::left_join(NLINPRO %>% dplyr::select(LinProCod, LinProNom),
                         by = "LinProCod") %>%
        dplyr::left_join(MARCOM %>% dplyr::select(MCCod, MCNom),
                         by = "MCCod") %>%
        dplyr::left_join(NMARCAS %>% dplyr::select(MrcCod, Marca),
                         by = "MrcCod")
    })
    
    mi_tabla_gt <- reactive({
      df <- data_completa()
      
      if (nrow(df) == 0) {
        return(gt_mensaje_vacio("No hay marcas para mostrar"))
      }
      
      year_now <- lubridate::year(Sys.Date())
      
      aux1 <- df %>%
        dplyr::group_by(LinNeg, LinProNom, MCNom, Marca) %>%
        dplyr::summarise(
          UltFact   = max(FecFact, na.rm = TRUE),
          LotesYTD  = sum(lubridate::year(FecFact) == year_now, na.rm = TRUE),
          SacosYTD  = sum(ifelse(lubridate::year(FecFact) == year_now, SacFact, 0),
                          na.rm = TRUE),
          MargenYTD = sum(ifelse(lubridate::year(FecFact) == year_now, Margen, 0),
                          na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        dplyr::filter(SacosYTD > 0.5) %>%
        dplyr::arrange(dplyr::desc(LotesYTD))
      
      aux1 %>%
        gt::gt() %>%
        gt::tab_header(title = paste("Total Marcas:", nrow(aux1))) %>%
        gt::cols_label(
          LinNeg    = "Línea de Negocio",
          LinProNom = "Línea de Producto",
          MCNom     = "Nombre Comercial",
          Marca     = "Marca",
          UltFact   = "Fecha Ult. Facturación",
          LotesYTD  = "Lotes (Acum)",
          SacosYTD  = "Sacos (70Kgs)(Acum)",
          MargenYTD = "Márgen (Acum)"
        ) %>%
        gt::fmt_number(columns = c(LotesYTD, SacosYTD), decimals = 0) %>%
        gt::fmt_currency(columns = MargenYTD,
                         currency = "COP",
                         decimals = 0) %>%
        gt::fmt_date(columns = UltFact, date_style = "day_m_year") %>%
        racafe::gt_minimal_style() %>%
        gt::opt_interactive(
          use_pagination = TRUE,
          use_filters    = FALSE,
          use_resizers   = TRUE
        )
    })
    
    # Configuración de botones ----
    botones_config <- crear_config_botones(
      editar = list(
        titulo        = "Modificar",
        tit_modal     = "Modificar Producto -",
        module_ui     = "EditarProductoUI",
        module_server = "EditarProducto",
        module_id     = "mod_modificar",
        show_modal    = FALSE,
        extra_params  = list(usr = usr)
      ),
      detalle = list(
        titulo        = "Detalle",
        tit_modal     = "Detalle de Producto -",
        module_ui     = "DashboardProductoUI",
        module_server = "DashboardProducto",
        module_id     = "mod_detalle"
      )
    )
    
    # Tabla GT con botones ----
    resultado <- GTBotones(
      id             = "TablaProductos",
      gt_table       = mi_tabla_gt,
      data           = data_completa,
      botones_config = botones_config,
      nombre_col     = c("Marca", "LinProNom")
    )
    
    # Drilldown exclusivo para edición fija ----
    dd_editar <- reactiveVal(NULL)
    
    observeEvent(resultado$dd_data(), {
      sel <- resultado$dd_data()
      req(sel)
      if (identical(sel$accion, "editar")) {
        dd_editar(sel)
      }
    }, ignoreInit = TRUE)
    
    # Edición fija (sin modal) ----
    EditarProducto(
      id      = "EdicionIndividual",
      dd_data = reactive(dd_editar()),
      usr     = usr
    )
  })
}

# App de prueba ----
ui <- bs4DashPage(
  title = "Prueba ResumenTotal",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    GestionProductoUI("resumen")
  )
)
server <- function(input, output, session) {
  GestionProducto("resumen", reactive(BaseDatos), reactive("HCYATE"))
}

shinyApp(ui, server)
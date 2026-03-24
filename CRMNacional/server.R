server <- function(input, output, session) {
  
  # Helpers ----
  
  # Niveles estandar de segmento analitico RFM
  .rfm_levels <- c(
    "CAMPEONES", "CLIENTES LEALES", "POTENCIALES LEALES", "NUEVOS CLIENTES",
    "PROMETEDORES", "NECESITAN ATENCIÓN", "A PUNTO DE DORMIR", "EN RIESGO",
    "NO PODEMOS PERDERLOS", "HIBERNANDO", "PERDIDOS", "NUEVOS EN BASE", "OTROS"
  )
  
  # Factory de cache con invalidacion y actualizacion manual
  create_cache <- function(loader_fn, process_fn = NULL) {
    data_cache  <- reactiveVal(NULL)
    last_update <- reactiveVal(Sys.time())
    get_data <- function() {
      if (is.null(isolate(data_cache()))) {
        tryCatch({
          raw    <- loader_fn()
          result <- if (!is.null(process_fn)) process_fn(raw) else raw
          data_cache(result)
          last_update(Sys.time())
        }, error = function(e) {
          warning(paste("Error cargando datos:", e$message))
          data_cache(data.frame())
        })
      }
      isolate(data_cache())
    }
    list(
      get        = get_data,
      invalidate = function() data_cache(NULL),
      refresh    = function() { data_cache(NULL); get_data() },
      last_update = last_update
    )
  }
  
  # Snapshot por grupo: retiene la fila con max(FecProceso) por grupo
  .snapshot_max <- function(dat, ...) {
    grp_vars <- rlang::enquos(...)
    dat %>%
      group_by(!!!grp_vars) %>%
      filter(FecProceso == max(FecProceso)) %>%
      slice(1) %>%
      ungroup()
  }
  
  # Snapshot RFM: aplica .snapshot_max y renombra columnas con sufijo
  .process_rfm <- function(dat, suffix) {
    cols_rfm <- c(
      "SegmentoAnalitica", "rfm_score", "transaction_count",
      "recency_days", "amount", "recency_score", "frequency_score", "monetary_score"
    )
    dat %>%
      mutate(FecProceso = as.Date(FecProceso)) %>%
      .snapshot_max(LinNegCod, CliNitPpal) %>%
      select(LinNegCod, CliNitPpal, all_of(cols_rfm)) %>%
      rename_with(~ paste0(.x, suffix), all_of(cols_rfm))
  }
  
  # Join temporal de CRMNALCLIENTE segun modo DINAMICO o ESTATICO
  .join_clientes <- function(dat_fact, clientes_raw, modo, cols_select) {
    snap_cols    <- c("LinNegCod", "CliNitPpal", "FecProceso", cols_select)
    clientes_sel <- clientes_raw %>% select(all_of(snap_cols))
    
    if (modo == "DINÁMICO") {
      snap <- clientes_sel %>%
        group_by(LinNegCod, CliNitPpal) %>%
        filter(FecProceso == max(FecProceso)) %>%
        slice(1) %>%
        ungroup() %>%
        select(-FecProceso)
      return(left_join(dat_fact, snap, by = c("CliNitPpal", "LinNegCod")))
    }
    
    # ESTATICO: para cada fila de dat_fact, tomar FecProceso mas reciente <= FecFact
    dat_fact %>%
      mutate(.row_id = row_number()) %>%
      left_join(clientes_sel, by = c("CliNitPpal", "LinNegCod"), relationship = "many-to-many") %>%
      filter(is.na(FecFact) | FecProceso <= as.Date(FecFact)) %>%
      group_by(.row_id) %>%
      filter(FecProceso == max(FecProceso)) %>%
      slice(1) %>%
      ungroup() %>%
      select(-.row_id, -FecProceso)
  }
  
  # Factory de reactivo RFM: filtra por segmento y estandariza columnas
  .rfm_reactive <- function(seg_racafe, col_seg, col_recency, col_count, col_amount) {
    reactive({
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      data_f() %>%
        filter(SegmentoRacafe == seg_racafe) %>%
        mutate(
          SegmentoAnalitica = ifelse(is.na(.data[[col_seg]]), "NUEVOS EN BASE", .data[[col_seg]]),
          SegmentoAnalitica = factor(SegmentoAnalitica, levels = .rfm_levels, ordered = TRUE)
        ) %>%
        rename(
          recency_days      = !!col_recency,
          transaction_count = !!col_count,
          amount            = !!col_amount
        )
    })
  }
  
  # Inicializacion ----
  ## Usuario ----
  usuario <- reactive({
    if (is.null(session$user)) "CMEDINA" else str_to_upper(session$user)
  })
  
  ## Filtros ----
  filtros <- FiltrosServer("Filtros", usuario, productos_cache)
  
  # Cache ----
  ## Cache raw de CRMNALCLIENTE: sin snapshot, para .join_clientes modo ESTATICO ----
  clientes_raw_cache <- create_cache(
    loader_fn = function() {
      CargarDatos("CRMNALCLIENTE") %>%
        mutate(
          FecProceso = as.Date(FecProceso),
          across(where(is.numeric),   ~ ifelse(is.na(.), 0, .)),
          across(where(is.character), ~ ifelse(is.na(.) | . == "N/A", "", .))
        )
    }
  )
  
  ## Pendientes de produccion, despacho y facturacion ----
  pend_cache <- create_cache(
    loader_fn = function() {
      ConsultaSistema("syscafe",
                      "SELECT CLSucCod, CLLotCod, CLLotSacPr, CLLotSacDe, CLLotSacFa
           FROM EXPCUALO
          WHERE CiaCod = 10 AND CLPdcVtaNa = 1
            AND CLCliNit <> 32 AND CLLinNegCo <> 0 AND CLLotCan > 0")
    }
  )
  
  ## Segmento Racafe: ultimo snapshot por cliente/linea ----
  segmentos_cache <- create_cache(
    loader_fn  = function() CargarDatos("CRMNALSEGR"),
    process_fn = function(dat) {
      dat %>%
        mutate(FecProceso = as.Date(FecProceso)) %>%
        filter(FecProceso == max(FecProceso)) %>%
        select(LinNegCod, CliNitPpal, SegmentoRacafe)
    }
  )
  
  ## RFM por sacos (sufijo S) ----
  rfm_cache <- create_cache(
    loader_fn  = function() CargarDatos("CRMNALRFM"),
    process_fn = function(dat) .process_rfm(dat, "S")
  )
  
  ## RFM por margen (sufijo M) ----
  rfmm_cache <- create_cache(
    loader_fn  = function() CargarDatos("CRMNALRFMM"),
    process_fn = function(dat) .process_rfm(dat, "M")
  )
  
  ## Customer Lifetime Value ----
  clv_cache <- create_cache(
    loader_fn  = function() CargarDatos("CRMNALCLV"),
    process_fn = function(dat) {
      dat %>%
        mutate(FecProceso = as.Date(FecProceso), CliNitPpal = as.numeric(CliNitPpal)) %>%
        group_by(CliNitPpal) %>%
        filter(FecProceso == max(FecProceso)) %>%
        slice(1) %>%
        ungroup() %>%
        select(CliNitPpal, Churn, SacosPred)
    }
  )
  
  ## Productos: ultimo snapshot por clave de producto ----
  productos_cache <- create_cache(
    loader_fn  = function() CargarDatos("CRMNALPRODS"),
    process_fn = function(dat) {
      dat %>%
        mutate(FecProceso = as.Date(FecProceso)) %>%
        group_by(LinNegCod, LinProCod, MCCod, MrcCod) %>%
        filter(FecProceso == max(FecProceso)) %>%
        slice(1) %>%
        ungroup()
    }
  )
  
  ## Ordenes de compra por lote ----
  ordcmp_cache <- create_cache(
    loader_fn = function() {
      CargarDatos("CRMNALORDCMP") %>% select(CLLotCod = Lote, OrdenCompra)
    }
  )
  
  ## Lista nombrada de todos los caches para actualizacion en lote ----
  all_caches <- list(
    pend      = pend_cache,
    clientes  = clientes_raw_cache,
    segmentos = segmentos_cache,
    rfm       = rfm_cache,
    rfmm      = rfmm_cache,
    clv       = clv_cache,
    productos = productos_cache,
    ordcmp    = ordcmp_cache
  )
  
  ## Trigger de actualizacion manual de oportunidades ----
  trigger_update_opt <- reactiveVal(0)
  
  # Datos ----
  ## Indicadores de precio ----
  indicators_cache <- reactiveVal(NULL)
  last_update_time <- reactiveVal(Sys.time() - 3600)
  
  data_ind <- reactive({
    waiter_show(html = preloader2$html, color = preloader2$color)
    on.exit(waiter_hide())
    current_time <- Sys.time()
    needs_update <- is.null(indicators_cache()) ||
      difftime(current_time, last_update_time(), units = "mins") > 30
    if (needs_update) {
      fnc_data    <- get_fnc_data()
      system_data <- get_system_data(uid, pwd)
      item_names  <- c(
        TRM         = "TRM (Hoja de trabajo)",
        PrecioNY    = "Precio NYC (HT)",
        PrecioCarga = "Precio Carga (Promedio de últimas entradas del día)",
        Diferencial = "Diferencial de Compra (HT)",
        UGCRacafe   = "Costo UGQ Racafé",
        PrecioBolsa = "Precio Bolsa (FNC)",
        PrecioFNC   = "Precio Carga (FNC)",
        UGCFNC      = "Costo UGQ (FNC)",
        CALConsumo  = "Precio Consumo (Calculadora)",
        COMConsumo  = "Precio Consumo (Compras)",
        CALPasilla  = "Precio Pasilla (Calculadora)",
        COMMolidos  = "Precio Molidos (Compras)",
        COMSoluble  = "Precio Soluble (Compras)",
        CALRipio    = "Precio Ripio (Calculadora)",
        COMRipio    = "Precio Ripio (Compras)",
        COMRobusta  = "Precio Robusta (Compras)"
      )
      result <- data.frame(
        PrecioBolsa = fnc_data$bolsa,
        TRM         = system_data$trm,
        PrecioFNC   = fnc_data$precio,
        UGCFNC      = (fnc_data$precio / 125) / (70 / 96.89),
        PrecioNY    = system_data$ny,
        PrecioCarga = system_data$precio_carga,
        Diferencial = system_data$precios_adicionales$Diferencial,
        UGCRacafe   = (system_data$precio_carga / 125) / (70 / 96.89),
        CALConsumo  = system_data$precios_adicionales$HTPreCon,
        COMConsumo  = system_data$precios_compras$CONSUMO,
        CALPasilla  = system_data$precios_adicionales$HTPrePas,
        COMMolidos  = system_data$precios_compras$MOLIDOS,
        COMSoluble  = system_data$precios_compras$SOLUBLE,
        CALRipio    = system_data$precios_adicionales$HTPreRip,
        COMRipio    = pluck(system_data, "precios_compras", "RIPIO",   .default = NA),
        COMRobusta  = pluck(system_data, "precios_compras", "ROBUSTA", .default = NA)
      ) %>%
        pivot_longer(cols = everything(), names_to = "Item", values_to = "Valor") %>%
        mutate(
          Item         = recode(Item, !!!item_names),
          Item         = factor(Item, levels = item_names),
          last_updated = format(current_time, "%Y-%m-%d %H:%M:%S")
        )
      indicators_cache(result)
      last_update_time(current_time)
    }
    indicators_cache()
  })
  observeEvent(data_ind(), { assign("DatosIndicadores", data_ind(), envir = .GlobalEnv) })
  
  ## Notas ----
  notes_data <- reactiveVal(CargarDatos("CRMNALNOTAS"))
  
  ## CRMNALCLIENTE: reactivos derivados del raw cache ----
  
  # Raw completo expuesto para .join_clientes y futura migracion de modulos
  clientes_raw <- reactive({ clientes_raw_cache$get() })
  
  # Snapshot dinamico para ped_sinlote (no depende de FecFact)
  clientes_sinlote <- reactive({
    clientes_raw_cache$get() %>%
      .snapshot_max(LinNegCod, CliNitPpal) %>%
      select(LinNegCod, PerCod = CLCliNit, Segmento) %>%
      distinct()
  })
  
  # Snapshot completo expuesto para modulos que aun hacen CargarDatos() internamente
  # (Presupuesto, Individual, Calculadoras, GestionProducto) — migracion futura
  clientes_snap <- reactive({
    clientes_raw_cache$get() %>% .snapshot_max(LinNegCod, CliNitPpal)
  })
  
  ## Dataset principal: joins y reglas de negocio ----
  # Columnas de CRMNALCLIENTE a incorporar (aliases: nombre_destino = nombre_origen)
  .cols_cli <- c(
    "Asesor", "Responsable", "Segmento", "Depto", "Mpio",
    "EstadoCliente", "EstadoNegocio", "RazonDescartado",
    "DescartadoPrecio", "DescartadoCalidad", "FrecuenciaDias",
    "NumMesesRecuperar", PptoSacos = "SSPpto", PptoMargen = "MNFCCPpto", "Excluir"
  )
  
  data_c <- reactive({
    waiter_show(html = preloader2$html, color = preloader2$color)
    on.exit(waiter_hide())
    f <- filtros()
    req(f$periodo)
    
    base_data <- data %>%
      # Pendientes de produccion, despacho y facturacion
      left_join(pend_cache$get(), by = c("CLSucCod", "CLLotCod")) %>%
      mutate(
        PendProducir  = SacLote - coalesce(CLLotSacPr, 0),
        PendDespachar = pmax(SacLote - pmax(coalesce(CLLotSacDe, 0), 0), 0),
        PendFacturar  = SacLote - pmax(PendDespachar, 0) - coalesce(CLLotSacFa, 0)
      ) %>%
      select(-c(CLLotSacPr, CLLotSacDe, CLLotSacFa)) %>%
      # Dimension de productos
      left_join(
        productos_cache$get() %>%
          select(LinNegCod, LinProCod, MCCod, MrcCod,
                 LinProNom, MCNom, Marca, Categoria, Producto, ProdExcluir = Excluir),
        by = c("LinNegCod", "LinProCod", "MCCod", "MrcCod")
      )
    
    # Join temporal de clientes segun modo DINAMICO o ESTATICO
    base_data <- .join_clientes(base_data, clientes_raw(), f$periodo, .cols_cli) %>%
      left_join(segmentos_cache$get(), by = c("CliNitPpal", "LinNegCod")) %>%
      left_join(rfm_cache$get(),       by = c("CliNitPpal", "LinNegCod")) %>%
      left_join(rfmm_cache$get(),      by = c("CliNitPpal", "LinNegCod")) %>%
      left_join(clv_cache$get(),       by = "CliNitPpal") %>%
      left_join(ordcmp_cache$get(),    by = "CLLotCod") %>%
      # Reglas de negocio: imputacion de segmento, asesor, responsable y campos vacios
      mutate(
        SegmentoAsignadoSistema = is.na(Segmento),
        Excluir     = ifelse(is.na(Excluir)     | Excluir     == "", "NO", Excluir),
        ProdExcluir = ifelse(is.na(ProdExcluir) | ProdExcluir == "", "NO", ProdExcluir),
        Segmento = case_when(
          !is.na(Segmento)                    ~ Segmento,
          LinNegCod == 10000 & SacLote <= 240 ~ "DETAL",
          LinNegCod == 10000 & SacLote >  240 ~ "MEDIANO",
          LinNegCod == 21000 & SacLote <   50 ~ "DETAL",
          LinNegCod == 21000 & SacLote >=  50 ~ "MEDIANO"
        ),
        Asesor = case_when(
          !is.na(Asesor)                      ~ Asesor,
          LinNegCod == 10000 & SacLote <  240 ~ "GACORREDOR",
          LinNegCod == 10000 & SacLote >= 240 ~ "CMEDINA",
          LinNegCod == 21000 & SacLote <   50 ~ "LABOYACA",
          LinNegCod == 21000 & SacLote >=  50 ~ "JGCANON"
        ),
        Responsable = case_when(
          !is.na(Responsable)                    ~ Responsable,
          LinNegCod == 10000 & SacLote <  240    ~ "GACORREDOR",
          LinNegCod == 10000 & SacLote >= 240    ~ "CMEDINA",
          LinNegCod == 21000 & SacLote <   50    ~ "LABOYACA",
          LinNegCod == 21000 & SacLote >=  50    ~ "JGCANON"
        ),
        SegmentoRacafe = ifelse(is.na(SegmentoRacafe) & !is.na(FecFact), "CLIENTE", SegmentoRacafe),
        Asesor      = ifelse(is.na(Asesor)      | Asesor      == "", "SIN DATO", Asesor),
        Responsable = ifelse(is.na(Responsable) | Responsable == "", "SIN DATO", Responsable),
        Segmento    = ifelse(is.na(Segmento)    | Segmento    == "", "SIN DATO", Segmento),
        CLLinNegNo  = ifelse(is.na(CLLinNegNo)  | CLLinNegNo  == "", "SIN DATO", CLLinNegNo),
        Categoria   = ifelse(is.na(Categoria)   | Categoria   == "", "SIN DATO", Categoria),
        Producto    = ifelse(is.na(Producto)    | Producto    == "", "SIN DATO", Producto)
      )
    
    base_data
  })
  observeEvent(data_c(), { assign("BaseDatos", data_c(), envir = .GlobalEnv) })
  
  ## Filtro por dimensiones de negocio ----
  data_t <- reactive({
    waiter_show(html = preloader2$html, color = preloader2$color)
    on.exit(waiter_hide())
    f <- filtros()
    req(data_c(), f$asesor, f$segmento, f$linneg, f$categoria, f$producto)
    data_c() %>%
      filter(
        Excluir    != "SI",
        Asesor     %in% f$asesor,
        Segmento   %in% f$segmento,
        CLLinNegNo %in% f$linneg,
        Categoria  %in% f$categoria,
        Producto   %in% f$producto
      )
  })
  
  ## Filtro por rango de fechas de factura ----
  data_f <- reactive({
    waiter_show(html = preloader2$html, color = preloader2$color)
    on.exit(waiter_hide())
    f <- filtros()
    req(data_t(), f$fecha)
    dat <- data_t()
    if (isTRUE(f$sin_factura) && !is.null(f$fecha)) {
      dat <- dat %>%
        filter(is.na(FecFact) | (FecFact >= f$fecha[1] & FecFact <= f$fecha[2]))
    } else if (!isTRUE(f$sin_factura) && !is.null(f$fecha)) {
      dat <- dat %>% filter(FecFact >= f$fecha[1], FecFact <= f$fecha[2])
    }
    dat
  })
  
  ## Pedidos sin Lote ----
  ped_sinlote <- reactive({
    waiter_show(html = preloader2$html, color = preloader2$color)
    on.exit(waiter_hide())
    f <- filtros()
    
    ConsultaSistema("syscafe",
                    "SELECT PdcCod, PdcLin, PdcCan, LinNegCod, LinProCod
         FROM EXPPEDI1
        WHERE CiaCod = 10 AND PdcCan > 0") %>%
      inner_join(
        ConsultaSistema("syscafe",
                        "SELECT PdcCod, PdcUsu, PdcFecCre, CliNit AS PerCod
             FROM EXPPEDID
            WHERE CiaCod = 10 AND CliNit <> 32
              AND PdcEst = 'A' AND PdcVtaNal = 1"),
        by = "PdcCod"
      ) %>%
      left_join(NCLIENTE %>% select(PerCod, PerRazSoc), by = join_by(PerCod)) %>%
      # Segmento desde cache: evita CargarDatos() redundante en ped_sinlote
      left_join(clientes_sinlote(), by = join_by(PerCod, LinNegCod)) %>%
      anti_join(
        ConsultaSistema("syscafe", "SELECT PdcCod, PdcLin FROM EXPLOT1"),
        by = join_by(PdcCod, PdcLin)
      ) %>%
      left_join(
        ConsultaSistema("syscafe",
                        "SELECT LinNegCod, LinNegNom AS LinNeg FROM NLINEANE WHERE CiaCod = 10"),
        by = join_by(LinNegCod)
      ) %>%
      left_join(
        ConsultaSistema("syscafe",
                        "SELECT LinProCod, LinProNom FROM NTIPPROD WHERE CiaCod = 10"),
        by = join_by(LinProCod)
      ) %>%
      filter(Segmento %in% f$segmento, LinNeg %in% f$linneg) %>%
      select(PerRazSoc, Segmento, PdcCod, PdcLin, PdcCan, PdcFecCre, LinNeg, LinProNom, PdcUsu) %>%
      mutate(PdcFecCre = as.Date(PdcFecCre))
  })
  observeEvent(ped_sinlote(), { assign("pedidos_sin_lote", ped_sinlote(), envir = .GlobalEnv) })
  
  ## RFM: cuatro reactivos derivados de data_f() ----
  data_rfm_cliente_f_s <- .rfm_reactive(
    "CLIENTE", "SegmentoAnaliticaS", "recency_daysS", "transaction_countS", "amountS")
  data_rfm_cliente_f_m <- .rfm_reactive(
    "CLIENTE", "SegmentoAnaliticaM", "recency_daysM", "transaction_countM", "amountM")
  data_rfm_clirec_f_s  <- .rfm_reactive(
    "CLIENTE A RECUPERAR", "SegmentoAnaliticaS", "recency_daysS", "transaction_countS", "amountS")
  data_rfm_clirec_f_m  <- .rfm_reactive(
    "CLIENTE A RECUPERAR", "SegmentoAnaliticaM", "recency_daysM", "transaction_countM", "amountM")
  
  ## Leads ----
  rv           <- reactiveValues(btn = NULL)
  v            <- FormularioLeads("Ingreso", rv = rv, usuario, tit = reactive(""))
  
  data_leads_f <- reactive({
    f <- filtros()
    CargarDatos("CRMNALLEAD") %>%
      filter(AutorizaTD == "SI", Segmento %in% f$segmento, LinNegocio %in% f$linneg)
  })
  observeEvent(data_leads_f(), { assign("BaseLeads", data_leads_f(), envir = .GlobalEnv) })
  
  ## Oportunidades ----
  data_oportunidades_f <- reactive({
    f <- filtros()
    req(f$linneg, f$segmento)
    base_op <- CargarDatos("CRMNALCLOPT") %>%
      mutate(
        Sacos70       = if_else(LineaNegocio == "CONVENCIONALES", SacosOP * 62.5 / 70, SacosOP),
        FechaCumpOP   = as.Date(FechaCumpOP),
        FechaHoraCrea = lubridate::as_datetime(FechaHoraCrea),
        Kilos         = if_else(LineaNegocio == "CONVENCIONALES", 62.5, 70),
        MargenTotalOP = Kilos * SacosOP * MargenOP,
        SacosMes      = SiError_0(SacosOP / (FrecuenciaDias / 30)),
        MargenMes     = SiError_0(MargenTotalOP / (FrecuenciaDias / 30)),
        Descartada    = if_else(is.na(Descartada), FALSE, Descartada)
      )
    facturacion_detalle <- data_c() %>%
      mutate(FecFact = as.Date(FecFact)) %>%
      select(PerRazSoc, CLLinNegNo, Categoria, Producto, FecFact, SacFact, Margen)
    base_op %>%
      left_join(facturacion_detalle,
                by = c("PerRazSoc", "LineaNegocio" = "CLLinNegNo", "Categoria", "Producto")) %>%
      filter(FecFact >= as.Date(FechaHoraCrea)) %>%
      mutate(Cliente = PerRazSoc) %>%
      crear_link_cliente("Cliente", "LineaNegocio")
  })
  observeEvent(data_oportunidades_f(), {
    assign("BaseOportunidades", data_oportunidades_f(), envir = .GlobalEnv)
  })
  
  ## Competencia ----
  data_competencia_f <- reactive({
    CargarDatos("CRMNALCOMPETENCIA") %>% filter(UsuarioCrea %in% usuario())
  })
  
  ## Consulta Individual ----
  data_individual <- reactive({
    waiter_show(html = preloader2$html, color = preloader2$color)
    on.exit(waiter_hide())
    req(input$IND_Cliente, input$IND_LinNeg)
    data_c() %>% filter(PerRazSoc == input$IND_Cliente, CLLinNegNo == input$IND_LinNeg)
  })
  
  # Actualizacion Manual ----
  observeEvent(input$FT_Actualizar, {
    waiter_show(html = preloader2$html, color = preloader2$color)
    tryCatch({
      isolate({ walk(all_caches, ~ .$refresh()) })
      indicators_cache(NULL)
      last_update_time(Sys.time() - 3600)
      data_ind()
    }, error = function(e) {
      refresh_status("error")
      refresh_message(paste("Error al actualizar:", e$message))
    }, finally = {
      waiter_hide()
    })
  })
  
  # Auto-refresh de indicadores cada 30 minutos
  observe({
    invalidateLater(30 * 60 * 1000)
    data_ind()
    data_c()
  })
  
  # UI Outputs ----
  ## Encabezados ----
  output$user <- renderUI({
    div(
      style = "display:flex; justify-content:center; align-items:center; height:100%; width:100%;",
      FormatearTexto(usuario(), tamano_pct = 1.2, negrita = TRUE)
    )
  })
  output$espacio <- renderUI({
    div(
      style = "display:flex; justify-content:center; align-items:center; height:100%; width:100%;",
      HTML(FormatearTexto("&nbsp;&nbsp;&nbsp;", tamano_pct = 1.2, negrita = TRUE, color = "#b2babb"))
    )
  })
  
  # Modulos ----
  ## Encabezado ----
  Cotizacion("Cotizador", usuario, data_c)
  Indicadores("Indicadores", data_ind)
  
  data_not <- reactive({ CargarDatos("CRMNALNOTAS") %>% filter(Responsable == usuario()) })
  Notificaciones("Notificaciones", data_not)
  
  TaskCreation("Tareas", usuario, notes_data)
  noteDisplay("NotasTareas", usuario, notes_data)
  Competencia("Competencia", data_ind, usuario)
  GestionProducto("Productos", data_c, usuario)
  
  ## Cuerpo ----
  ### Hoja de trabajo ----
  ResumenTotal(
    id                = "ResumenTotal",
    dat               = data_f,
    dat_c             = data_c,
    dat_leads         = data_leads_f,
    dat_oportunidades = data_oportunidades_f,
    dat_competencia   = data_competencia_f,
    usr               = usuario,
    trigger_update    = trigger_update_opt
  )
  ComparacionIndicadores("CompIndicadores", data_ind)
  Calculadora("Calculadoras", data_ind, usuario)
  Presupuesto("PresupuestoTotal", data_t)
  Pendientes("Pendientes", data_f, ped_sinlote, usuario)
  
  ### Oportunidades ----
  FormularioOportunidad("Formulario", dat = data_c, usr = usuario,
                        trigger_update = trigger_update_opt)
  TablaOportunidades("Listado", data_op = data_oportunidades_f, usr = usuario,
                     trigger_update = trigger_update_opt)
  DashboardOportunidades("Oportunidades", data_oportunidades_f)
  
  ### Clientes ----
  DetalleCliente(id = "ResumenClientes", data_f, usr = usuario,
                 trigger_update = trigger_update_opt)
  
  ppto_clientes <- reactive({ data_t() %>% filter(SegmentoRacafe == "CLIENTE") })
  Presupuesto("Presupuesto", ppto_clientes)
  
  RFM("RFMClientesSacos",  data_rfm_cliente_f_s)
  RFM("RFMClientesMargen", data_rfm_cliente_f_m, "$ MNFCC")
  ClientesNuevosRecuperados("ClientesNuevosRecuperar", data, data_f)
  
  ### Clientes a Recuperar ----
  DetalleClienteRecuperar(id = "ResumenClientesRecuperar", data_f, usr = usuario,
                          trigger_update = trigger_update_opt)
  
  output$ClientesRecuperarConPPto <- renderDT({
    aux1 <- data_f() %>%
      filter(SegmentoRacafe == "CLIENTE A RECUPERAR", PptoMargen != 0) %>%
      group_by(PerRazSoc, LineaNegocio = CLLinNegNo, Segmento) %>%
      summarise(
        UltDespacho = max(FecFact, na.rm = TRUE),
        Presupuesto = max(PptoMargen / 12),
        SacosMes   = sum(if_else(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Kilos / 70, 0), na.rm = TRUE),
        SacosAnho  = sum(if_else(year(FecFact) == year(Sys.Date()), Kilos / 70, 0), na.rm = TRUE),
        MargenMes  = sum(if_else(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Margen / 70, 0), na.rm = TRUE),
        MargenAnho = sum(if_else(year(FecFact) == year(Sys.Date()), Margen, 0), na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      janitor::adorn_totals("row", name = "TOTAL")
    ImprimirDTRAzSocLinNeg(
      aux1,
      noms     = c("Razón Social", "Línea de Negocio", "Segmento Racafé", "Última Facturación",
                   "Presupuesto", "Sacos Mes", "Sacos Año", "Márgen Mes", "Márgen Año"),
      formatos = c(NA, NA, NA, NA, "dinero", "sacos", "sacos", "dinero", "dinero"),
      dom = "Bft", buscar = TRUE, alto = 500
    )
  })
  
  data_sankey_clirec <- reactive({
    waiter_show(html = preloader2$html, color = preloader2$color)
    on.exit(waiter_hide())
    data_f() %>%
      mutate(
        Niv1 = ifelse(is.na(EstadoCliente), "POR CONTACTAR", EstadoCliente),
        Niv2 = ifelse(EstadoCliente == "CONTACTADO",
                      ifelse(is.na(EstadoNegocio), "SIN ESTADO", EstadoNegocio), NA),
        Niv3 = str_to_upper(case_when(
          EstadoCliente == "CONTACTADO" & EstadoNegocio == "DESCARTADO"       ~ RazonDescartado,
          EstadoCliente == "CONTACTADO" & grepl("OPORTUNIDAD", EstadoNegocio) ~
            paste("Cada", FrecuenciaDias, "días")
        ))
      )
  })
  SankeyTabla("ClienteRecuperar", data_sankey_clirec)
  
  RFM("RFMCliRecSacos",  data_rfm_clirec_f_s)
  RFM("RFMCliRecMargen", data_rfm_clirec_f_m, "$ MNFCC")
  
  output$ClientesRecuperar <- renderDT({
    aux1 <- CargarDatos("CRMNALSEGR") %>%
      select(LinNegCod, CliNitPpal, SegmentoRacafe, FecProceso) %>%
      mutate(FecProceso = as.Date(FecProceso)) %>%
      filter(FecProceso >= PrimerDia(Sys.Date()) - months(1)) %>%
      group_by(LinNegCod, CliNitPpal) %>%
      pivot_wider(names_from = FecProceso, values_from = SegmentoRacafe) %>%
      setNames(c("LinNegCod", "CliNitPpal", "Antes", "Ahora")) %>%
      filter(Antes %in% c("CLIENTE", NA) & Ahora == "CLIENTE A RECUPERAR") %>%
      select(LinNegCod, CliNitPpal)
    aux2 <- data_f() %>%
      inner_join(aux1, by = join_by(LinNegCod, CliNitPpal)) %>%
      group_by(PerRazSoc, LineaNegocio = CLLinNegNo, Segmento) %>%
      summarise(
        UltDespacho = max(FecFact, na.rm = TRUE),
        SacosMes   = sum(if_else(PrimerDia(FecDesp) == PrimerDia(Sys.Date()), Kilos / 70, 0), na.rm = TRUE),
        SacosAnho  = sum(if_else(year(FecDesp) == year(Sys.Date()), Kilos / 70, 0), na.rm = TRUE),
        MargenMes  = sum(if_else(PrimerDia(FecDesp) == PrimerDia(Sys.Date()), Margen / 70, 0), na.rm = TRUE),
        MargenAnho = sum(if_else(year(FecDesp) == year(Sys.Date()), Margen, 0), na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      janitor::adorn_totals("row", name = "TOTAL")
    ImprimirDTRAzSocLinNeg(
      aux2,
      noms     = c("Razón Social", "Línea de Negocio", "Segmento Racafé", "Última Facturación",
                   "Sacos Mes", "Sacos Año", "Márgen Mes", "Márgen Año"),
      formatos = c(NA, NA, NA, NA, "sacos", "sacos", "dinero", "dinero"),
      dom = "Bft", buscar = TRUE, alto = 500
    )
  })
  
  ### Leads ----
  dt_ResumenLeads <- reactive({
    waiter_show(html = preloader2$html, color = preloader2$color)
    on.exit(waiter_hide())
    aux1 <- data_leads_f() %>%
      select(-c(UsuarioCrea, FechaHoraCrea, UsuarioMod, FechaHoraModi)) %>%
      mutate(pct_missing = rowSums(is.na(.)) / ncol(.)) %>%
      select(PerRazSoc, Asesor, pct_missing, SacosPotencial, MargenPotencial)
    aux1$pct_missing <- sapply(aux1$pct_missing, function(x) {
      FormatearNumero(x, formato = "porcentaje", meta = c(0.5, 0.7), prop = FALSE)
    })
    aux1 %>%
      ImprimirDTLead(
        botones  = c("Contacto", "Editar"),
        noms     = c("Razón Social", "Asesor", "Pct Ausente", "Sacos (70kg)", "Márgen"),
        formatos = c(NA, NA, NA, "sacos", "dinero"),
        dom      = "ft",
        buscar   = TRUE
      )
  })
  dd_ResumenLeads <- reactive({
    waiter_show(html = preloader2$html, color = preloader2$color)
    on.exit(waiter_hide())
    data
  })
  TablaModalCelda("ResumenLeads", dt_ResumenLeads, dd_ResumenLeads, usuario, rv)
  
  data_sankey_lead <- reactive({
    waiter_show(html = preloader2$html, color = preloader2$color)
    on.exit(waiter_hide())
    data_leads_f() %>%
      mutate(
        CliNitPpal = PerRazSoc,
        CLLinNegNo = LinNegocio,
        FecFact    = NA,
        FecDesp    = NA,
        Kilos      = ifelse(LinNegocio == 10000, SacosPotencial * 62.5, SacosPotencial * 70),
        Margen     = MargenPotencial,
        Niv1       = ifelse(is.na(EstadoCuenta), "POR CONTACTAR", EstadoCuenta),
        Niv2       = ifelse(EstadoCuenta == "CONTACTADO",
                            ifelse(is.na(EstadoNegocio), "SIN ESTADO", EstadoNegocio), NA),
        Niv3       = str_to_upper(case_when(
          EstadoCuenta == "CONTACTADO" & EstadoNegocio == "DESCARTADO" ~ RazonDescartado
        ))
      )
  })
  SankeyTabla("Leads", data_sankey_lead)
  
  ### Consulta Individual ----
  Individual("ConsultaIndivual", data_individual, usuario)
  
  # Footer ----
  output$last_update_info <- renderText({
    updates       <- map_dbl(all_caches, ~ as.numeric(.$last_update())) %>%
      c(as.numeric(last_update_time()))
    oldest_update <- as.POSIXct(min(updates, na.rm = TRUE), origin = "1970-01-01")
    HTML(FormatearTexto(
      paste("Última actualización:", format(oldest_update, "%Y-%m-%d %H:%M:%S")),
      tamano_pct = 0.6
    ))
  })
  
}
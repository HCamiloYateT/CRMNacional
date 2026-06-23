server <- function(input, output, session) {
  
  # Helpers ----
  
  # Niveles estándar de segmento analítico RFM
  .rfm_levels <- c(
    "CAMPEONES", "CLIENTES LEALES", "POTENCIALES LEALES", "NUEVOS CLIENTES",
    "PROMETEDORES", "NECESITAN ATENCIÓN", "A PUNTO DE DORMIR", "EN RIESGO",
    "NO PODEMOS PERDERLOS", "HIBERNANDO", "PERDIDOS", "NUEVOS EN BASE", "OTROS"
  )
  
  # Columnas de facturación horneadas en datos_rv() que se reemplazan con fuente viva
  .cols_fact_stale <- c(
    "FcnTip", "FecPrimerFact", "FecFact", "KilosFact",
    "SacosFact", "SacFact70", "Margen", "SacosPYG", "Facturado", "MarKilo"
  )
  
  # Factory de cache con invalidación, actualización manual y TTL opcional en minutos
  create_cache <- function(loader_fn, process_fn = NULL, ttl_mins = NULL) {
    data_cache  <- reactiveVal(NULL)
    last_update <- reactiveVal(Sys.time())
    get_data <- function() {
      cached      <- isolate(data_cache())
      tiempo_mins <- difftime(Sys.time(), isolate(last_update()), units = "mins")
      needs_refresh <- is.null(cached) ||
        (!is.null(ttl_mins) && tiempo_mins > ttl_mins)
      if (needs_refresh) {
        tryCatch(
          {
            raw    <- loader_fn()
            result <- if (!is.null(process_fn)) process_fn(raw) else raw
            data_cache(result)
            last_update(Sys.time())
          },
          error = function(e) {
            warning(paste("Error cargando datos:", e$message))
            data_cache(data.frame())
          }
        )
      }
      isolate(data_cache())
    }
    list(
      get         = get_data,
      invalidate  = function() data_cache(NULL),
      refresh     = function() { data_cache(NULL); get_data() },
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
  
  # Snapshot RFM: aplica .snapshot_max y renombra columnas con sufijo dado
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
  
  # Join temporal de CRMNALCLIENTE según modo DINÁMICO o ESTÁTICO
  # Preserva todas las filas de dat_fact independientemente del match en clientes
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
    
    # ESTÁTICO: FecProceso más reciente <= FecFact; filas sin match se conservan con NA
    dat_con_id <- dat_fact %>% mutate(.row_id = row_number())
    
    snap_validos <- dat_con_id %>%
      left_join(
        clientes_sel, by = c("CliNitPpal", "LinNegCod"),
        relationship = "many-to-many"
      ) %>%
      filter(is.na(FecFact) | FecProceso <= as.Date(FecFact)) %>%
      group_by(.row_id) %>%
      filter(FecProceso == max(FecProceso, na.rm = TRUE)) %>%
      slice(1) %>%
      ungroup()
    
    sin_snap <- dat_con_id %>%
      filter(!.row_id %in% snap_validos$.row_id)
    
    bind_rows(snap_validos, sin_snap) %>%
      arrange(.row_id) %>%
      select(-.row_id, -FecProceso)
  }
  
  # Factory de reactivo RFM: sin preloader, opera sobre data_f() ya cacheado
  .rfm_reactive <- function(seg_racafe, col_seg, col_recency, col_count, col_amount) {
    reactive({
      data_f() %>%
        filter(SegmentoRacafe == seg_racafe) %>%
        mutate(
          SegmentoAnalitica = ifelse(
            is.na(.data[[col_seg]]), "NUEVOS EN BASE", .data[[col_seg]]
          ),
          SegmentoAnalitica = factor(
            SegmentoAnalitica, levels = .rfm_levels, ordered = TRUE
          )
        ) %>%
        rename(
          recency_days      = !!col_recency,
          transaction_count = !!col_count,
          amount            = !!col_amount
        )
    })
  }
  
  # Inicialización ----
  
  # Captura del usuario con fallback para desarrollo local
  usuario <- reactive({
    
    login <- if (is.null(session$user)) "JGCANON" else str_to_upper(session$user)
    nombre <- .MAP_USUARIO_ASESOR[login]
    # Fallback: si el login no está mapeado, devuelve el login mismo
    if (is.na(nombre)) login else unname(nombre)
    
  })
  
  filtros <- FiltrosServer("Filtros", usuario, productos_cache)
  # [DEBUG] observe({ f <- filtros(); assign("f", f, envir = .GlobalEnv) })
  
  # Valores reactivos base cargados desde DataPrep
  datos_rv    <- reactiveVal(data)
  ncliente_rv <- reactiveVal(NCLIENTE)
  fact_rv     <- reactiveVal(FACT)
  
  # Cache ----
  
  ## Clientes en bruto desde CRMNALCLIENTE (fuente única para todos los consumidores)
  clientes_raw_cache <- create_cache(loader_fn = function() {
    CargarDatos("CRMNALCLIENTE") %>%
      mutate(FecProceso = as.Date(FecProceso),
             across(where(is.numeric),   ~ ifelse(is.na(.), 0, .)),
             across(where(is.character), ~ ifelse(is.na(.) | . == "N/A", "", .))) %>%
      left_join(CargarDatos("CRMNALLOCAL") %>%
                  mutate(FecProceso = as.Date(FecProceso)) %>%
                  group_by(CliNitPpal) %>%
                  filter(FecProceso == max(FecProceso)),
                by = join_by(FecProceso, Usr, CliNitPpal)) %>%
      mutate(LinNegocio = ifelse(LinNegCod == 10000, "CONVENCIONALES", "A LA MEDIDA"))
  })
  
  ## Pendientes de producción, despacho y facturación desde EXPCUALO
  pend_cache <- create_cache(loader_fn = function() {
    ConsultaSistema(
      "syscafe",
      "SELECT CLSucCod, CLLotCod, CLLotSacPr, CLLotSacDe, CLLotSacFa
       FROM   EXPCUALO
       WHERE  CiaCod = 10 AND CLPdcVtaNa = 1
         AND  CLCliNit <> 32 AND CLLinNegCo <> 0 AND CLLotCan > 0"
    )
  })
  
  ## Segmento Racafe en bruto: todas las fechas disponibles para análisis histórico
  # output$ClientesRecuperar necesita dos snapshots consecutivos; no puede usar
  # segmentos_cache que solo expone el último
  segmentos_raw_cache <- create_cache(
    loader_fn = function() {
      CargarDatos("CRMNALSEGR") %>% mutate(FecProceso = as.Date(FecProceso))
    }
  )
  
  ## Segmento Racafe: último snapshot por cliente y línea (derivado del raw)
  segmentos_cache <- create_cache(
    loader_fn  = function() segmentos_raw_cache$get(),
    process_fn = function(dat) {
      dat %>%
        filter(FecProceso == max(FecProceso)) %>%
        select(LinNegCod, CliNitPpal, SegmentoRacafe)
    }
  )
  
  ## RFM por sacos: snapshot con sufijo S
  rfm_cache <- create_cache(
    loader_fn  = function() CargarDatos("CRMNALRFM"),
    process_fn = function(dat) .process_rfm(dat, "S")
  )
  
  ## RFM por margen: snapshot con sufijo M
  rfmm_cache <- create_cache(
    loader_fn  = function() CargarDatos("CRMNALRFMM"),
    process_fn = function(dat) .process_rfm(dat, "M")
  )
  
  ## Customer Lifetime Value: último snapshot por cliente
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
  
  ## Productos: último snapshot por clave de producto
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
  
  ## Órdenes de compra por lote
  ordcmp_cache <- create_cache(loader_fn = function() {
    CargarDatos("CRMNALORDCMP") %>% select(CLLotCod = Lote, OrdenCompra)
  })
  
  ## Facturación viva por lote: colapsada a una fila en process_fn (ejecuta una vez)
  fact_cache <- create_cache(
    loader_fn  = cargar_fact,
    process_fn = function(dat) {
      dat %>%
        group_by(CLLotCod) %>%
        summarise(
          FcnTip        = first(FcnTip),
          FecPrimerFact = min(FecPrimerFact, na.rm = TRUE),
          FecFact       = max(FecFact,       na.rm = TRUE),
          KilosFact     = sum(KilosFact,     na.rm = TRUE),
          SacosFact     = sum(SacosFact,     na.rm = TRUE),
          SacFact70     = sum(SacFact70,     na.rm = TRUE),
          # NA cuando todos los registros del lote son NA: preserva imputación jerárquica
          Margen   = if_else(all(is.na(Margen)),   NA_real_, sum(Margen,   na.rm = TRUE)),
          SacosPYG = if_else(all(is.na(SacosPYG)), NA_real_, sum(SacosPYG, na.rm = TRUE)),
          Facturado = any(coalesce(Facturado, FALSE)),
          .groups   = "drop"
        )
    }
  )
  
  ## Líneas de negocio: tabla cuasi-estática del sistema transaccional
  linneg_cache <- create_cache(loader_fn = function() {
    ConsultaSistema(
      "syscafe",
      "SELECT LinNegCod, LinNegNom AS LinNeg FROM NLINEANE WHERE CiaCod = 10"
    )
  })
  
  ## Tipos de producto: tabla cuasi-estática del sistema transaccional
  linpro_cache <- create_cache(loader_fn = function() {
    ConsultaSistema(
      "syscafe",
      "SELECT LinProCod, LinProNom FROM NTIPPROD WHERE CiaCod = 10"
    )
  })
  
  ## Catálogo de comercializaciones: cuasi-estático del sistema transaccional
  nmarcom_cache <- create_cache(loader_fn = function() {
    ConsultaSistema("syscafe", "SELECT MCCod, MCNom FROM NMARCOM")
  })
  
  ## Catálogo de marcas: cuasi-estático del sistema transaccional
  nmarcas_cache <- create_cache(loader_fn = function() {
    ConsultaSistema("syscafe", "SELECT MrcCod, MrcNom AS Marca FROM NMARCAS")
  })
  
  ## Lotes asignados a pedido
  lotes_asig_cache <- create_cache(loader_fn = function() {
    ConsultaSistema("syscafe", "SELECT PdcCod, PdcLin FROM EXPLOT1")
  })
  
  ## Leads: fuente única; data_leads_f() aplica filtros sobre este cache
  leads_cache <- create_cache(loader_fn = function() {
    CargarDatos("CRMNALLEAD") %>%
      mutate(
        AutorizaTD = "SI",  # TODO: reemplazar cuando llegue el campo real del sistema
        LinNegocio = ifelse(LinNegCod == 10000, "CONVENCIONALES", "A LA MEDIDA")
      )
  })
  
  ## Oportunidades: fuente única; data_oportunidades_f() aplica filtros sobre este cache
  oportunidades_cache <- create_cache(loader_fn = function() {
    CargarDatos("CRMNALCLOPT") %>%
      mutate(LinNegocio = ifelse(LinNegCod == 10000, "CONVENCIONALES", "A LA MEDIDA"))
  })
  
  ## Competencia: fuente única; data_competencia_f() filtra por usuario
  competencia_cache <- create_cache(
    loader_fn = function() CargarDatos("CRMNALCOMPETENCIA")
  )
  
  ## Notas y tareas: fuente única de carga; notes_data (reactiveVal) es el punto de acceso
  notas_cache <- create_cache(
    loader_fn = function() CargarDatos("CRMNALNOTAS")
  )
  
  ## Indicadores de precio: TTL de 30 minutos integrado al factory
  # Migrado desde implementación manual con dos reactiveVal y difftime inline
  indicadores_cache <- create_cache(
    ttl_mins  = 30,
    loader_fn = function() {
      fnc_data    <- get_fnc_data()
      system_data <- get_system_data(uid, pwd)
      item_names  <- c(
        TRM         = "TRM (Hoja de trabajo)",
        PrecioNY    = "Precio NYC (HT)",
        PrecioCarga = "Precio Carga (Promedio de \u00faltimas entradas del d\u00eda)",
        Diferencial = "Diferencial de Compra (HT)",
        UGCRacafe   = "Costo UGQ Racaf\u00e9",
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
      data.frame(
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
          Item = recode(Item, !!!item_names),
          Item = factor(Item, levels = item_names)
        )
    }
  )
  
  ## Lista nombrada de todos los caches para actualización en lote
  # segmentos_raw debe preceder a segmentos para respetar la cadena de dependencia
  all_caches <- list(
    pend          = pend_cache,
    clientes      = clientes_raw_cache,
    segmentos_raw = segmentos_raw_cache,
    segmentos     = segmentos_cache,
    rfm           = rfm_cache,
    rfmm          = rfmm_cache,
    clv           = clv_cache,
    productos     = productos_cache,
    ordcmp        = ordcmp_cache,
    fact          = fact_cache,
    linneg        = linneg_cache,
    linpro        = linpro_cache,
    nmarcom       = nmarcom_cache,
    nmarcas       = nmarcas_cache,
    lotes_asig    = lotes_asig_cache,
    leads         = leads_cache,
    oportunidades = oportunidades_cache,
    competencia   = competencia_cache,
    notas         = notas_cache,
    indicadores   = indicadores_cache
  )
  
  ## Trigger de actualización manual de oportunidades
  trigger_update_opt <- reactiveVal(0)
  
  # Datos ----
  ## Indicadores de precio ----
  data_ind <- reactive({
    waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
    on.exit(waiter_hide())
    indicadores_cache$get()
  })
  
  ## Notas y Tareas ----
  # reactiveVal: los módulos TaskCreation y noteDisplay escriben vía notes_data(nuevo)
  # La carga inicial viene del cache; FT_Actualizar sincroniza tras el refresh completo
  notes_data <- reactiveVal(notas_cache$get())
  
  ## Clientes en bruto: snapshots derivados del cache centralizado ----
  clientes_raw <- reactive({ clientes_raw_cache$get() })
  # [DEBUG] observeEvent(clientes_raw(), { assign("clientes_raw_r", clientes_raw(), envir = .GlobalEnv) })
  
  # Snapshot dinámico para ped_sinlote
  clientes_sinlote <- reactive({
    clientes_raw_cache$get() %>%
      .snapshot_max(LinNegCod, CliNitPpal) %>%
      select(LinNegCod, PerCod = CLCliNit, Segmento) %>%
      distinct()
  })
  
  # Snapshot completo por cliente y línea
  clientes_snap <- reactive({
    clientes_raw_cache$get() %>% .snapshot_max(LinNegCod, CliNitPpal)
  })
  
  ## Columnas de CRMNALCLIENTE a incorporar al cuadro de lotes
  .cols_cli <- c(
    "Asesor", "Segmento", "Depto", "Mpio",
    "NumMesesRecuperar", PptoSacos = "SSPpto", PptoMargen = "MNFCCPpto",
    "Excluir"
  )
  
  ## Lotes enriquecidos: reactivo pesado, invalida solo con cambios de datos base ----
  data_lotes_enriquecidos <- reactive({
    datos_rv() %>%
      select(-any_of(.cols_fact_stale)) %>%
      left_join(pend_cache$get(), by = c("CLSucCod", "CLLotCod")) %>%
      left_join(fact_cache$get(), by = "CLLotCod") %>%
      # Imputación jerárquica de margen por kilo (4 niveles), idéntica a DataPrep
      mutate(MarKilo = Margen / KilosFact) %>%
      group_by(CLCliNit, CLLinNegNo, LinProCod, MCCod, MrcCod) %>%
      mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = TRUE), MarKilo)) %>%
      group_by(CLLinNegNo, LinProCod, MCCod, MrcCod) %>%
      mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = TRUE), MarKilo)) %>%
      group_by(CLLinNegNo, LinProCod) %>%
      mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = TRUE), MarKilo)) %>%
      group_by(CLLinNegNo) %>%
      mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = TRUE), MarKilo)) %>%
      ungroup() %>%
      mutate(
        Margen = ifelse(
          is.na(Margen),
          MarKilo * SacLote * ifelse(LinNegCod == 10000L, 62.5, 70),
          Margen
        ),
        PendProducir  = SacLote - coalesce(CLLotSacPr, 0),
        PendDespachar = pmax(SacLote - pmax(coalesce(CLLotSacDe, 0), 0), 0),
        PendFacturar  = SacLote - pmax(PendDespachar, 0) - coalesce(CLLotSacFa, 0),
        # Patch: FCTFACN1 ya registró la factura pero EXPCUALO aún no refleja el cambio
        .patch     = (is.na(SacFact) | SacFact == 0) & coalesce(Facturado, FALSE),
        SacFact    = if_else(.patch, coalesce(SacosFact, SacFact), SacFact),
        CLLotDesXF = if_else(.patch, pmax(SacDesp - coalesce(SacosFact, 0), 0), CLLotDesXF),
        PendFacturar = if_else(
          .patch,
          SacLote - pmax(PendDespachar, 0) - coalesce(SacosFact, 0),
          PendFacturar
        )
      ) %>%
      select(-c(CLLotSacPr, CLLotSacDe, CLLotSacFa, .patch)) %>%
      left_join(
        productos_cache$get() %>%
          select(
            LinNegCod, LinProCod, MCCod, MrcCod,
            LinProNom, MCNom, Marca, Categoria, Producto, ProdExcluir = Excluir
          ),
        by = c("LinNegCod", "LinProCod", "MCCod", "MrcCod")
      )
  })
  
  ## data_c: enriquecimiento por cliente y periodo ----
  # Sin preloader: reactivo intermedio; el waiter pertenece únicamente a data_f()
  data_c <- reactive({
    f <- filtros()
    req(f$periodo)
    .join_clientes(data_lotes_enriquecidos(), clientes_raw(), f$periodo, .cols_cli) %>%
      left_join(segmentos_cache$get(), by = c("CliNitPpal", "LinNegCod")) %>%
      left_join(rfm_cache$get(),       by = c("CliNitPpal", "LinNegCod")) %>%
      left_join(rfmm_cache$get(),      by = c("CliNitPpal", "LinNegCod")) %>%
      left_join(clv_cache$get(),       by = "CliNitPpal") %>%
      left_join(ordcmp_cache$get(),    by = "CLLotCod") %>%
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
        SegmentoRacafe = ifelse(
          is.na(SegmentoRacafe) & !is.na(FecFact), "CLIENTE", SegmentoRacafe
        ),
        Asesor     = ifelse(is.na(Asesor)     | Asesor     == "", "SIN DATO", Asesor),
        Segmento   = ifelse(is.na(Segmento)   | Segmento   == "", "SIN DATO", Segmento),
        CLLinNegNo = ifelse(is.na(CLLinNegNo) | CLLinNegNo == "", "SIN DATO", CLLinNegNo),
        Categoria  = ifelse(is.na(Categoria)  | Categoria  == "", "SIN DATO", Categoria),
        Producto   = ifelse(is.na(Producto)   | Producto   == "", "SIN DATO", Producto)
      )
  })
  observeEvent(data_c(), { assign("BaseDatos_c", data_c(), envir = .GlobalEnv) }) #[DEBUG] 
  
  ## data_t: filtro por dimensiones de negocio ----
  # Sin preloader: filter() puro sobre data_c() ya en memoria
  data_t <- reactive({
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
  observeEvent(data_t(), { assign("BaseDatos_t", data_t(), envir = .GlobalEnv) }) # [DEBUG]
  
  ## data_f: filtro por rango de fechas de factura ----
  data_f <- reactive({
    waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
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
  observeEvent(data_f(), { assign("BaseDatos_f", data_f(), envir = .GlobalEnv) }) # [DEBUG]
  
  ## Data Cohortes ----
  data_cohortes <- reactive({
    anio <- year(Sys.Date())
    
    # t1: estados mensuales del año en curso desde CRMNALSEGR
    t1 <- segmentos_raw_cache$get() %>%
      mutate(FecProceso = as.Date(FecProceso),
             Estado = recode(SegmentoRacafe, 
                             "CLIENTE" = "ACTIVO", "CLIENTE A RECUPERAR" = "INACTIVO")) %>%
      filter(year(FecProceso) == anio) %>%
      select(FecProceso, LinNegCod, CliNitPpal, Estado)
    
    # t2: snapshot vigente de CRMNALCLIENTE — presupuesto mensual prorrateado
    t2 <- clientes_snap() %>%
      filter(year(FecProceso) == anio, Excluir != "SI") %>%
      mutate(PPtoSacos = SSPpto / 12,
             PPtoMargen = MNFCCPpto / 12,
             Presupuestado = ifelse(SSPpto > 0, "PRESUPUESTADO", "NO PRESUPUESTADO")) %>%
      select(LinNegCod, CliNitPpal, Asesor, Segmento, NumMesesRecuperar,
             PPtoSacos, PPtoMargen, Presupuestado, Excluir)
    
    # t3: ventas mensuales del año desde data_c()
    t3 <- data_c() %>%
      filter(year(FecFact) == anio, !is.na(FecFact)) %>%
      group_by(LinNegCod, CliNitPpal,
               FecProceso = PrimerDia(FecFact)) %>%
      summarise(Sacos  = sum(SacFact70, na.rm = TRUE),
                Margen = sum(Margen,    na.rm = TRUE),
                .groups = "drop")
    
    # t4: última fecha de facturación por UC — historial completo sin filtro de año
    t4 <- data_c() %>%
      group_by(LinNegCod, CliNitPpal, PerRazSoc) %>%
      summarise(UltFecFact = if (all(is.na(FecFact))) as.Date(NA) else max(FecFact, na.rm = TRUE),
                .groups    = "drop")
    
    # Ensamble longitudinal
    t1 %>%
      full_join(t3, by = join_by(FecProceso, LinNegCod, CliNitPpal)) %>%
      left_join(t4, by = join_by(LinNegCod, CliNitPpal)) %>%
      mutate(Estado = ifelse(is.na(Estado), "NUEVO", Estado),
             Sacos  = ifelse(is.na(Sacos),  0, Sacos),
             Margen = ifelse(is.na(Margen), 0, Margen)) %>%
      group_by(LinNegCod, CliNitPpal) %>%
      mutate(EstadoPanel = max(ifelse(Estado == "NUEVO", 1, 0)),
             FecIngreso  = if (any(Estado == "NUEVO")) {min(FecProceso[Estado == "NUEVO"])} else {as.Date(paste0(anio, "-01-01"))},
             EdadPanel   = lubridate::interval(FecIngreso, FecProceso) %/% months(1) + 1
             ) %>%
      ungroup() %>%
      mutate(EstadoPanel = ifelse(EstadoPanel == 1, "NUEVO", Estado)) %>%
      left_join(t2, by = join_by(LinNegCod, CliNitPpal)) %>%
      filter(Excluir != "SI")
  })
  observeEvent(data_cohortes(), { assign("BaseCohortes", data_cohortes(), envir = .GlobalEnv) }) #[DEBUG] 
  ## Pedidos sin lote asignado ----
  # ncliente_rv() reemplaza NCLIENTE global: refleja actualizaciones de FT_Actualizar
  ped_sinlote <- reactive({
    waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
    on.exit(waiter_hide())
    f <- filtros()
    ConsultaSistema(
      "syscafe",
      "SELECT PdcCod, PdcLin, PdcCan, LinNegCod, LinProCod
       FROM   EXPPEDI1
       WHERE  CiaCod = 10 AND PdcCan > 0"
    ) %>%
      inner_join(
        ConsultaSistema(
          "syscafe",
          "SELECT PdcCod, PdcUsu, PdcFecCre, CliNit AS PerCod
           FROM   EXPPEDID
           WHERE  CiaCod = 10 AND CliNit <> 32
             AND  PdcEst = 'A' AND PdcVtaNal = 1"
        ),
        by = "PdcCod"
      ) %>%
      left_join(ncliente_rv() %>% select(PerCod, PerRazSoc) %>% distinct(), by = join_by(PerCod)) %>%
      left_join(clientes_sinlote(),     by = join_by(PerCod, LinNegCod)) %>%
      anti_join(lotes_asig_cache$get(), by = join_by(PdcCod, PdcLin)) %>%
      left_join(linneg_cache$get(),     by = join_by(LinNegCod)) %>%
      left_join(linpro_cache$get(),     by = join_by(LinProCod)) %>%
      filter(Segmento %in% f$segmento, LinNeg %in% f$linneg) %>%
      select(PerRazSoc, Segmento, PdcCod, PdcLin, PdcCan, PdcFecCre, LinNeg, LinProNom, PdcUsu) %>%
      mutate(PdcFecCre = as.Date(PdcFecCre))
  })
  observeEvent(ped_sinlote(), { assign("pedidos_sin_lote", ped_sinlote(), envir = .GlobalEnv) }) # [DEBUG] 
  
  ## RFM: cuatro reactivos derivados de data_f() ----
  data_rfm_cliente_f_s <- .rfm_reactive(
    "CLIENTE", "SegmentoAnaliticaS", "recency_daysS", "transaction_countS", "amountS"
  )
  data_rfm_cliente_f_m <- .rfm_reactive(
    "CLIENTE", "SegmentoAnaliticaM", "recency_daysM", "transaction_countM", "amountM"
  )
  data_rfm_clirec_f_s <- .rfm_reactive(
    "CLIENTE A RECUPERAR", "SegmentoAnaliticaS", "recency_daysS", "transaction_countS", "amountS"
  )
  data_rfm_clirec_f_m <- .rfm_reactive(
    "CLIENTE A RECUPERAR", "SegmentoAnaliticaM", "recency_daysM", "transaction_countM", "amountM"
  )
  
  ## Leads ----
  data_leads_f <- reactive({
    f <- filtros()
    linneg_cod_activos <- linneg_cache$get() %>%
      filter(LinNeg %in% f$linneg) %>%
      pull(LinNegCod)
    leads_cache$get() %>%
      filter(AutorizaTD == "SI",
             LinNegCod  %in% linneg_cod_activos,
             Segmento   %in% f$segmento,
             Asesor     %in% f$asesor
             )
  })
  observeEvent(data_leads_f(), { assign("BaseLeads", data_leads_f(), envir = .GlobalEnv) }) # [DEBUG] 
  
  ## Oportunidades ----
  data_oportunidades_f <- reactive({
    f <- filtros()
    req(f$linneg, f$segmento, f$asesor)
    dims <- data_t() %>%
      select(CliNitPpal, LinNegCod, Segmento, Asesor) %>%
      distinct()
    oportunidades_cache$get() %>%
      mutate(
        Sacos70       = if_else(LinNegCod == 10000, Sacos * 62.5 / 70, Sacos),
        FechaCumpOP   = as.Date(FechaCumpOP),
        Kilos         = if_else(LinNegCod == 10000, 62.5, 70),
        MargenTotalOP = Kilos * Sacos * Margen,
        SacosMes      = SiError_0(Sacos / (FrecuenciaDias / 30)),
        MargenMes     = SiError_0(MargenTotalOP / (FrecuenciaDias / 30)),
        Descartado    = if_else(is.na(Descartado), "NO", Descartado)
      ) %>%
      inner_join(dims, by = c("CLCliNit" = "CliNitPpal", "LinNegCod")) %>%
      left_join(
        data_c() %>%
          mutate(FecFact = as.Date(FecFact)) %>%
          select(PerRazSoc, LinNegCod, Categoria, Producto, FecFact, SacFact, Margen),
        by = c("PerRazSoc", "LinNegCod", "Categoria", "Producto")
      ) %>%
      filter(FecFact > as.Date(FecProceso)) %>%
      mutate(Cliente = PerRazSoc) %>%
      crear_link_cliente("Cliente", "LineaNegocio")
  })
  observeEvent(data_oportunidades_f(), { assign("BaseOportunidades", data_oportunidades_f(), envir = .GlobalEnv) }) # [DEBUG] 
  
  ## Competencia ----
  data_competencia_f <- reactive({
    competencia_cache$get() %>% filter(UsuarioCrea %in% usuario())
  })
  
  observeEvent(data_competencia_f(), { assign("BaseCompetencia", data_competencia_f(), envir = .GlobalEnv) }) # [DEBUG] 
  
  ## Consulta individual ----
  data_individual <- reactive({
    waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
    on.exit(waiter_hide())
    req(input$IND_Cliente, input$IND_LinNeg)
    data_c() %>% filter(PerRazSoc == input$IND_Cliente, CLLinNegNo == input$IND_LinNeg)
  })
  # [DEBUG] observeEvent(data_individual(), { assign("BaseDatos_i", data_individual(), envir = .GlobalEnv) })
  
  # Actualización Manual ----
  # walk(all_caches) invalida y recarga todos los caches en secuencia, incluyendo
  # leads, oportunidades, competencia, notas e indicadores (antes ausentes de la lista)
  observeEvent(input$FT_Actualizar, {
    waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
    tryCatch(
      {
        nuevos_datos <- cargar_datos_base()
        datos_rv(nuevos_datos$data)
        ncliente_rv(nuevos_datos$NCLIENTE)
        fact_rv(nuevos_datos$FACT)
        isolate({ walk(all_caches, ~ .$refresh()) })
        # Sincroniza notes_data con el cache ya refrescado para que los módulos
        # que leen vía notes_data() vean los datos actualizados sin nueva carga a BD
        notes_data(notas_cache$get())
      },
      error   = function(e) warning("Error al actualizar datos base: ", e$message),
      finally = waiter_hide()
    )
  })
  
  # UI Outputs ----
  
  ## Encabezados ----
  
  ### Usuario ----
  output$user <- renderUI({
    FormatearTexto(
      paste(usuario()) %>% HTML,
      negrita = TRUE, tamano_pct = 0.75, alineacion = "center", color = "#999"
    )
  })
  
  ### Descarga Clientes ----
  output$FT_DescargarClientes <- downloadHandler(
    filename = function() {
      paste0("Clientes_",format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      
      datos <- CargarDatos("CRMNALCLIENTE")
      
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, sheetName = "Clientes")
      openxlsx::writeData(wb, sheet = "Clientes", x = datos)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  ### Menú de Indicadores ----
  IndicadoresServer("ind_kpis", dat = data_ind)
  res_ind <- MenuHeaderServer(
    id         = "MenuIndicadores",
    items_r    = IndicadoresUI("ind_kpis"),
    headerText = NULL,
    href       = "#",
    footerText = "Comparación de indicadores"
  )
  observeEvent(res_ind$footer_click(), {
    bs4Dash::updateTabItems(session, "menu_principal", selected = "HT_Indicadores")
  })
  
  ### Notificación de Tareas ----
  # data_not_header: filtro amplio (Usuario O Responsable) para el ícono de campana
  # Nombre explícito para evitar la sobreescritura silenciosa del original
  data_not_header <- reactive({
    u <- usuario()
    notes_data() %>%
      dplyr::filter(Usuario == u | grepl(u, Responsable, fixed = TRUE))
  })
  
  not_mod <- NotificacionesDropServer("not_content", dat = data_not_header)
  res_not <- MenuHeaderServer(
    id            = "MenuNotificaciones",
    items_r       = NotificacionesDropUI("not_content"),
    badgeStatus_r = not_mod$badge_st,
    badge_n_r     = not_mod$n,
    headerText    = "Tareas y Asignaciones",
    href          = "#",
    footerText    = "Ver todas"
  )
  observeEvent(not_mod$item_click(), {
    bs4Dash::updateTabItems(session, "menu_principal", selected = "HT_Tareas")
  })
  observeEvent(res_not$footer_click(), {
    bs4Dash::updateTabItems(session, "menu_principal", selected = "HT_Tareas")
  })
  
  ### Clientes sin información ----
  
  # Clientes activos sin registro en CRMNALCLIENTE
  # clientes_raw_cache reemplaza CargarDatos("CRMNALCLIENTE") directo
  # datos_rv()         reemplaza el objeto global `data`
  clientes_incompletos_r <- reactive({
    crm <- clientes_raw_cache$get() %>%
      dplyr::group_by(CliNitPpal, LinNegCod) %>%
      dplyr::arrange(dplyr::desc(FecProceso)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(CliNitPpal, LinNegCod, Segmento_crm = Segmento, Asesor_crm = Asesor)
    
    t1 <- data_c() %>%
      dplyr::distinct(CliNitPpal, LinNegCod, PerRazSoc, CLLinNegNo) %>%
      dplyr::anti_join(crm, by = c("CliNitPpal", "LinNegCod")) %>%
      dplyr::arrange(PerRazSoc) %>%
      dplyr::mutate(label = paste0(CLLinNegNo, " \u2014 ", PerRazSoc)) %>%
      dplyr::arrange(label)
  })
  # IndFormulario registrado una sola vez con identidad dinámica
  cli_identidad_rv <- reactiveVal(NULL)
  IndFormulario(id = "mod_form_cli",
                identidad = reactive(cli_identidad_rv()),
                dat = reactive({
                  id_val <- cli_identidad_rv()
                  req(!is.null(id_val), length(id_val$nit) == 1L, !is.na(id_val$nit))
                  data_c() %>%
                    dplyr::filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod)
                  }),
                usr = usuario)
  
  # Resumen de datos del modal de cliente
  output$resumen_cli_modal <- renderUI({
    id_val <- cli_identidad_rv()
    req(!is.null(id_val), length(id_val$nit) == 1L, !is.na(id_val$nit))
    
    crm <- clientes_raw_cache$get() %>%
      dplyr::filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod) %>%
      dplyr::arrange(dplyr::desc(FecProceso)) %>%
      dplyr::slice(1)
    
    dat_cli <- data_c() %>%
      dplyr::filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod)
    
    cod_ultimo <- dat_cli %>%
      dplyr::arrange(dplyr::desc(PdcFecCre)) %>%
      dplyr::pull(CLPdcCod) %>%
      dplyr::first()
    
    ultimo <- dat_cli %>% dplyr::filter(CLPdcCod == cod_ultimo)
    
    segmento <- if (nrow(crm) > 0 && nzchar(crm$Segmento[1] %||% "")) crm$Segmento[1] else "\u2014"
    asesor   <- if (nrow(crm) > 0 && nzchar(crm$Asesor[1]   %||% "")) crm$Asesor[1]   else "\u2014"
    ultima_fac <- tryCatch({
      m <- max(dat_cli$FecFact, na.rm = TRUE)
      if (is.infinite(m)) "\u2014" else format(m, "%d %b %Y")
    }, error = function(e) "\u2014")
    
    if (nrow(ultimo) > 0) {
      cod_pedido <- as.character(cod_ultimo %||% "\u2014")
      fec_pedido <- tryCatch(
        format(as.Date(ultimo$PdcFecCre[1]), "%d %b %Y"), error = function(e) "\u2014"
      )
      usr_pedido <- ultimo$Usuario[1] %||% "\u2014"
      sacos_tot  <- format(sum(ultimo$SacLote, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
    } else {
      cod_pedido <- fec_pedido <- usr_pedido <- sacos_tot <- "\u2014"
    }
    
    .seccion <- function(label) {
      tags$p(
        style = paste0(
          "font-size:10px; font-weight:700; color:#94A3B8; ",
          "text-transform:uppercase; letter-spacing:0.05em; margin:0 0 6px;"
        ),
        label
      )
    }
    
    .item <- function(icono, label, valor) {
      tags$div(
        style = "display:flex; align-items:flex-start; gap:6px; margin-bottom:4px;",
        tags$span(icon(icono), style = "color:#64748B; font-size:10px; width:13px; margin-top:1px;"),
        tags$span(label, style = "font-size:11px; color:#64748B; min-width:90px; flex-shrink:0;"),
        tags$span(valor, style = "font-size:11px; font-weight:600; color:#374151;")
      )
    }
    
    lotes_ui <- if (nrow(ultimo) > 0) {
      items <- lapply(seq_len(nrow(ultimo)), function(i) {
        f <- ultimo[i, ]
        tags$div(
          style = paste0(
            "background:white; border:1px solid #E2E8F0; border-radius:4px;",
            "padding:5px 8px; margin-bottom:4px;"
          ),
          tags$div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            tags$div(
              tags$span(
                as.character(f$CLLotCod),
                style = "font-size:11px; font-weight:700; color:#1E40AF;"
              ),
              tags$span(
                paste0(" \u00b7 ", f$Categoria %||% "\u2014", " / ", f$Producto %||% "\u2014"),
                style = "font-size:10px; color:#64748B;"
              )
            ),
            tags$span(
              paste0(format(f$SacLote, big.mark = ".", decimal.mark = ","), " sacos"),
              style = "font-size:11px; font-weight:600; color:#374151;"
            )
          )
        )
      })
      do.call(tagList, items)
    } else {
      tags$span("\u2014", style = "font-size:11px; color:#94A3B8;")
    }
    
    tags$div(
      style = paste0(
        "background:#F8FAFC; border:1px solid #E2E8F0; border-radius:6px;",
        "padding:12px 14px; margin-bottom:14px;"
      ),
      fluidRow(
        column(4,
               .seccion("Datos CRM"),
               .item("calendar",    "Última factura:", ultima_fac),
               .item("layer-group", "Segmento:",       segmento),
               .item("user",        "Asesor:",          asesor)
        ),
        column(4,
               .seccion("Último pedido"),
               .item("hashtag",        "Código:",     cod_pedido),
               .item("calendar-plus",  "Creación:",   fec_pedido),
               .item("user-pen",       "Usuario:",    usr_pedido),
               .item("weight-hanging", "Sacos tot.:", sacos_tot)
        ),
        column(4,
               .seccion(paste0("Lotes (", nrow(ultimo), ")")),
               lotes_ui
        )
      )
    )
  })
  
  MenuHeaderServer(
    id            = "MenuClientes",
    items_r       = clientes_incompletos_r,
    key_cols      = c("CliNitPpal", "LinNegCod", "PerRazSoc", "CLLinNegNo"),
    badgeStatus_r = "danger",
    headerText    = NULL,
    href          = "#",
    modal_icon    = "user-edit",
    modal_size    = "l",
    modal_pre_fn  = function(sel) {
      nit <- suppressWarnings(as.numeric(sel$CliNitPpal))
      lin <- suppressWarnings(as.numeric(sel$LinNegCod))
      req(length(nit) == 1L, !is.na(nit))
      cli_identidad_rv(list(nit = nit, linneg_cod = lin))
    },
    modal_titulo_fn    = function(sel) paste0(sel$PerRazSoc, " \u2014 ", sel$CLLinNegNo),
    modal_contenido_fn = function(sel) tagList(
      uiOutput("resumen_cli_modal"),
      IndFormularioUI("mod_form_cli")
    )
  )
  
  ### Clientes con información antigua ----
  
  # Clientes que tienen registro en CRMNALCLIENTE pero sin actualización
  # en el año en curso, y con facturación en el año en curso.
  clientes_info_antigua_r <- reactive({
    
    anho_actual <- year(Sys.Date())
    
    # Último registro CRM por cliente y línea — detecta el año del último proceso
    crm_reciente <- clientes_raw_cache$get() %>%
      dplyr::group_by(CliNitPpal, LinNegCod) %>%
      dplyr::arrange(dplyr::desc(FecProceso)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::filter(year(FecProceso) < anho_actual) %>%
      dplyr::select(CliNitPpal, LinNegCod, FecProceso,
                    Segmento_crm = Segmento, Asesor_crm = Asesor)
    
    # Clientes con facturación en el año en curso
    con_fact_anho <- datos_rv() %>%
      dplyr::filter(year(FecFact) == anho_actual) %>%
      dplyr::distinct(CliNitPpal, LinNegCod, PerRazSoc, CLLinNegNo)
    
    # Intersección: tienen registro antiguo Y han facturado este año
    con_fact_anho %>%
      dplyr::inner_join(crm_reciente, by = c("CliNitPpal", "LinNegCod")) %>%
      dplyr::mutate(
        label           = paste0(CLLinNegNo, " \u2014 ", PerRazSoc),
        UltActualizacion = format(FecProceso, "%d %b %Y")
      ) %>%
      dplyr::arrange(label) %>%
      dplyr::select(CliNitPpal, LinNegCod, PerRazSoc, CLLinNegNo,
                    UltActualizacion, label)
  })
  
  # reactiveVal de identidad independiente para no colisionar con cli_identidad_rv
  cli_ant_identidad_rv <- reactiveVal(NULL)
  
  # Resumen del modal para "información antigua" — reutiliza lógica de resumen_cli_modal
  # pero con identidad propia (cli_ant_identidad_rv) para aislamiento total
  output$resumen_cli_ant_modal <- renderUI({
    id_val <- cli_ant_identidad_rv()
    req(!is.null(id_val), length(id_val$nit) == 1L, !is.na(id_val$nit))
    
    crm <- clientes_raw_cache$get() %>%
      dplyr::filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod) %>%
      dplyr::arrange(dplyr::desc(FecProceso)) %>%
      dplyr::slice(1)
    
    dat_cli <- data_c() %>%
      dplyr::filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod)
    
    cod_ultimo <- dat_cli %>%
      dplyr::arrange(dplyr::desc(PdcFecCre)) %>%
      dplyr::pull(CLPdcCod) %>%
      dplyr::first()
    
    ultimo <- dat_cli %>% dplyr::filter(CLPdcCod == cod_ultimo)
    
    # Fecha de último proceso CRM — específico de este modal
    ult_proceso <- if (nrow(crm) > 0) {
      format(crm$FecProceso[1], "%d %b %Y")
    } else "\u2014"
    
    segmento <- if (nrow(crm) > 0 && nzchar(crm$Segmento[1] %||% "")) {
      crm$Segmento[1]
    } else "\u2014"
    asesor <- if (nrow(crm) > 0 && nzchar(crm$Asesor[1] %||% "")) {
      crm$Asesor[1]
    } else "\u2014"
    ultima_fac <- tryCatch({
      m <- max(dat_cli$FecFact, na.rm = TRUE)
      if (is.infinite(m)) "\u2014" else format(m, "%d %b %Y")
    }, error = function(e) "\u2014")
    
    if (nrow(ultimo) > 0) {
      cod_pedido <- as.character(cod_ultimo %||% "\u2014")
      fec_pedido <- tryCatch(
        format(as.Date(ultimo$PdcFecCre[1]), "%d %b %Y"), error = function(e) "\u2014"
      )
      usr_pedido <- ultimo$Usuario[1] %||% "\u2014"
      sacos_tot  <- format(
        sum(ultimo$SacLote, na.rm = TRUE), big.mark = ".", decimal.mark = ","
      )
    } else {
      cod_pedido <- fec_pedido <- usr_pedido <- sacos_tot <- "\u2014"
    }
    
    .seccion <- function(label) {
      tags$p(
        style = paste0(
          "font-size:10px; font-weight:700; color:#94A3B8; ",
          "text-transform:uppercase; letter-spacing:0.05em; margin:0 0 6px;"
        ),
        label
      )
    }
    
    .item <- function(icono, label, valor) {
      tags$div(
        style = "display:flex; align-items:flex-start; gap:6px; margin-bottom:4px;",
        tags$span(icon(icono), style = "color:#64748B; font-size:10px; width:13px; margin-top:1px;"),
        tags$span(label, style = "font-size:11px; color:#64748B; min-width:90px; flex-shrink:0;"),
        tags$span(valor, style = "font-size:11px; font-weight:600; color:#374151;")
      )
    }
    
    lotes_ui <- if (nrow(ultimo) > 0) {
      items <- lapply(seq_len(nrow(ultimo)), function(i) {
        f <- ultimo[i, ]
        tags$div(
          style = paste0(
            "background:white; border:1px solid #E2E8F0; border-radius:4px;",
            "padding:5px 8px; margin-bottom:4px;"
          ),
          tags$div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            tags$div(
              tags$span(
                as.character(f$CLLotCod),
                style = "font-size:11px; font-weight:700; color:#1E40AF;"
              ),
              tags$span(
                paste0(" \u00b7 ", f$Categoria %||% "\u2014", " / ", f$Producto %||% "\u2014"),
                style = "font-size:10px; color:#64748B;"
              )
            ),
            tags$span(
              paste0(format(f$SacLote, big.mark = ".", decimal.mark = ","), " sacos"),
              style = "font-size:11px; font-weight:600; color:#374151;"
            )
          )
        )
      })
      do.call(tagList, items)
    } else {
      tags$span("\u2014", style = "font-size:11px; color:#94A3B8;")
    }
    
    tags$div(
      style = paste0(
        "background:#F8FAFC; border:1px solid #E2E8F0; border-radius:6px;",
        "padding:12px 14px; margin-bottom:14px;"
      ),
      fluidRow(
        column(4,
               .seccion("Datos CRM"),
               # Fecha de última actualización CRM — diferenciador clave vs modal de sin info
               .item("clock",       "Últ. actualiz.:", ult_proceso),
               .item("calendar",    "Última factura:", ultima_fac),
               .item("layer-group", "Segmento:",       segmento),
               .item("user",        "Asesor:",          asesor)
        ),
        column(4,
               .seccion("Último pedido"),
               .item("hashtag",        "Código:",     cod_pedido),
               .item("calendar-plus",  "Creación:",   fec_pedido),
               .item("user-pen",       "Usuario:",    usr_pedido),
               .item("weight-hanging", "Sacos tot.:", sacos_tot)
        ),
        column(4,
               .seccion(paste0("Lotes (", nrow(ultimo), ")")),
               lotes_ui
        )
      )
    )
  })
  
  MenuHeaderServer(
    id            = "MenuClientesAnt",
    items_r       = clientes_info_antigua_r,
    key_cols      = c("CliNitPpal", "LinNegCod", "PerRazSoc", "CLLinNegNo"),
    badgeStatus_r = "warning",
    headerText    = NULL,
    href          = "#",
    modal_icon    = "user-clock",
    modal_size    = "l",
    modal_pre_fn  = function(sel) {
      nit <- suppressWarnings(as.numeric(sel$CliNitPpal))
      lin <- suppressWarnings(as.numeric(sel$LinNegCod))
      req(length(nit) == 1L, !is.na(nit))
      cli_ant_identidad_rv(list(nit = nit, linneg_cod = lin))
    },
    modal_titulo_fn    = function(sel) paste0(sel$PerRazSoc, " \u2014 ", sel$CLLinNegNo),
    modal_contenido_fn = function(sel) tagList(
      uiOutput("resumen_cli_ant_modal"),
      IndFormularioUI("mod_form_cli")          # reutiliza el mismo formulario
    )
  )
  IndFormulario(
    id        = "mod_form_cli_ant",
    identidad = reactive(cli_ant_identidad_rv()),
    dat       = reactive({
      id_val <- cli_ant_identidad_rv()
      req(!is.null(id_val), length(id_val$nit) == 1L, !is.na(id_val$nit))
      data_c() %>%
        dplyr::filter(CliNitPpal == id_val$nit, LinNegCod == id_val$linneg_cod)
    }),
    usr = usuario
  )
  ### Productos sin información ----
  # productos_cache reemplaza CargarDatos("CRMNALPRODS") directo
  # nmarcom_cache / nmarcas_cache reemplazan dos ConsultaSistema sin cache
  # datos_rv()         reemplaza el objeto global `data`
  productos_incompletos_r <- reactive({
    prods <- productos_cache$get() %>%
      dplyr::distinct(LinNegCod, LinProCod, MCCod, MrcCod)
    
    datos_rv() %>%
      dplyr::group_by(LinNegCod, LinProCod, MCCod, MrcCod, CLLinNegNo) %>%
      dplyr::summarise(
        SacosYTD = sum(ifelse(year(FecFact) == year(Sys.Date()), SacosFact, 0)),
        .groups  = "drop"
      ) %>%
      dplyr::anti_join(prods, by = c("LinNegCod", "LinProCod", "MCCod", "MrcCod")) %>%
      dplyr::left_join(nmarcom_cache$get(), by = "MCCod") %>%
      dplyr::left_join(nmarcas_cache$get(), by = "MrcCod") %>%
      dplyr::rename(LinNeg = CLLinNegNo) %>%
      dplyr::arrange(desc(SacosYTD)) %>%
      dplyr::mutate(label = paste0(LinNeg, " \u2014 ", MCNom, " / ", Marca)) %>%
      select(-SacosYTD)
  })
  
  prod_identidad_rv <- reactiveVal(NULL)
  ProdFormulario(
    id        = "mod_form_prod",
    identidad = reactive(prod_identidad_rv()),
    dat       = data_c,
    usr       = usuario
  )
  
  res_prod <- MenuHeaderServer(
    id            = "MenuProductos",
    items_r       = productos_incompletos_r,
    key_cols      = c("LinNegCod", "LinProCod", "MCCod", "MrcCod", "LinNeg", "MCNom", "Marca"),
    badgeStatus_r = "warning",
    headerText    = "Productos sin registro CRM",
    href          = "#",
    footerText    = "Ver productos",
    modal_icon    = "barcode",
    modal_size    = "m",
    modal_pre_fn  = function(sel) {
      prod_identidad_rv(list(
        linneg_cod = as.numeric(sel$LinNegCod),
        linpro_cod = as.numeric(sel$LinProCod),
        mc_cod     = as.numeric(sel$MCCod),
        mrc_cod    = as.numeric(sel$MrcCod),
        linneg     = sel$LinNeg,
        mc_nom     = sel$MCNom,
        marca      = sel$Marca,
        linpro_nom = NULL
      ))
    },
    modal_titulo_fn    = function(sel) paste0(sel$MCNom, " / ", sel$Marca),
    modal_contenido_fn = function(sel) tagList(
      uiOutput("resumen_cli_ant_modal"),
      IndFormularioUI("mod_form_cli_ant") 
    )
  )
  
  # Módulos ----
  
  ## Encabezado ----
  persona_r <- reactive({
    Unicos(c(data_c()$PerRazSoc, data_leads_f()$PerRazSoc))
  })
  Cotizacion("Cotizador", usuario, data_c, persona_r)

  data_not_modulo <- reactive({
    notes_data() %>% filter(Responsable == usuario())
  })
  Notificaciones("Notificaciones", data_not_modulo)
  
  TaskCreation("Tareas",     usuario, notes_data)
  noteDisplay("NotasTareas", usuario, notes_data)
  
  Competencia("Competencia", data_ind, usuario)
  GestionProducto("Productos", data_c, usuario)
  
  ## Cuerpo ----

  ### Hoja de trabajo ----
  ResumenTotal(id = "ResumenTotal", 
               dat = data_f, dat_t = data_t, dat_c = data_c,
               dat_leads = data_leads_f, dat_oportunidades = data_oportunidades_f, 
               dat_competencia = data_competencia_f, clientes_raw = clientes_raw,
               data_cohortes = data_cohortes, segmentos_raw = reactive(segmentos_raw_cache$get()),
               fact_r = fact_rv, 
               usr = usuario, trigger_update = trigger_update_opt)
  # ComparacionIndicadores("CompIndicadores", data_ind)
  # Calculadora("Calculadoras",     data_ind, usuario)
  # Presupuesto("PresupuestoTotal", data_t,   clientes_raw)
  # Pendientes("Pendientes",        data_f,   ped_sinlote, usuario)
  
  # ### Oportunidades ----
  # FormularioOportunidad("Formulario", dat = data_c, usr = usuario,
  #                       trigger_update = trigger_update_opt)
  # TablaOportunidades("Listado", data_op = data_oportunidades_f, usr = usuario,
  #                    trigger_update = trigger_update_opt)
  # DashboardOportunidades("Oportunidades", data_oportunidades_f)
  # 
  # ### Clientes ----
  # DetalleCliente(id = "ResumenClientes", data_f, usr = usuario,
  #                trigger_update = trigger_update_opt)
  # 
  # ppto_clientes <- reactive({ data_t() %>% filter(SegmentoRacafe == "CLIENTE") })
  # Presupuesto("Presupuesto", ppto_clientes, clientes_raw)
  # 
  # RFM("RFMClientesSacos",  data_rfm_cliente_f_s)
  # RFM("RFMClientesMargen", data_rfm_cliente_f_m, "$ MNFCC")
  # ClientesNuevosRecuperados("ClientesNuevosRecuperados", datos_rv, data_f)
  # 
  # ### Clientes a Recuperar ----
  # DetalleClienteRecuperar(id = "ResumenClientesRecuperar", data_f, usr = usuario,
  #                         trigger_update = trigger_update_opt)
  # 
  # output$ClientesRecuperarConPPto <- renderDT({
  #   aux1 <- data_f() %>%
  #     filter(SegmentoRacafe == "CLIENTE A RECUPERAR", PptoMargen != 0) %>%
  #     group_by(PerRazSoc, LineaNegocio = CLLinNegNo, Segmento) %>%
  #     summarise(
  #       UltDespacho = max(FecFact, na.rm = TRUE),
  #       Presupuesto = max(PptoMargen / 12),
  #       SacosMes    = sum(
  #         if_else(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Kilos / 70, 0), na.rm = TRUE
  #       ),
  #       SacosAnho   = sum(
  #         if_else(year(FecFact) == year(Sys.Date()), Kilos / 70, 0), na.rm = TRUE
  #       ),
  #       MargenMes   = sum(
  #         if_else(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Margen / 70, 0), na.rm = TRUE
  #       ),
  #       MargenAnho  = sum(
  #         if_else(year(FecFact) == year(Sys.Date()), Margen, 0), na.rm = TRUE
  #       ),
  #       .groups     = "drop"
  #     ) %>%
  #     janitor::adorn_totals("row", name = "TOTAL")
  #   ImprimirDTRAzSocLinNeg(
  #     aux1,
  #     noms     = c(
  #       "Raz\u00f3n Social", "L\u00ednea de Negocio", "Segmento Racaf\u00e9",
  #       "\u00daltima Facturaci\u00f3n", "Presupuesto",
  #       "Sacos Mes", "Sacos A\u00f1o", "M\u00e1rgen Mes", "M\u00e1rgen A\u00f1o"
  #     ),
  #     formatos = c(NA, NA, NA, NA, "dinero", "sacos", "sacos", "dinero", "dinero"),
  #     dom = "Bft", buscar = TRUE, alto = 500
  #   )
  # })
  # 
  # data_sankey_clirec <- reactive({
  #   data_f() %>%
  #     mutate(
  #       Niv1 = ifelse(is.na(EstadoCliente), "POR CONTACTAR", EstadoCliente),
  #       Niv2 = ifelse(
  #         EstadoCliente == "CONTACTADO",
  #         ifelse(is.na(EstadoNegocio), "SIN ESTADO", EstadoNegocio),
  #         NA
  #       ),
  #       Niv3 = str_to_upper(case_when(
  #         EstadoCliente == "CONTACTADO" & EstadoNegocio == "DESCARTADO" ~
  #           RazonDescartado,
  #         EstadoCliente == "CONTACTADO" & grepl("OPORTUNIDAD", EstadoNegocio) ~
  #           paste("Cada", FrecuenciaDias, "d\u00edas")
  #       ))
  #     )
  # })
  # SankeyTabla("ClienteRecuperar", data_sankey_clirec)
  # 
  # RFM("RFMCliRecSacos",  data_rfm_clirec_f_s)
  # RFM("RFMCliRecMargen", data_rfm_clirec_f_m, "$ MNFCC")
  # 
  # output$ClientesRecuperar <- renderDT({
  #   # segmentos_raw_cache reemplaza CargarDatos("CRMNALSEGR") directo
  #   # Se requiere histórico de dos snapshots; segmentos_cache solo expone el último
  #   aux1 <- segmentos_raw_cache$get() %>%
  #     select(LinNegCod, CliNitPpal, SegmentoRacafe, FecProceso) %>%
  #     filter(FecProceso >= PrimerDia(Sys.Date()) - months(1)) %>%
  #     group_by(LinNegCod, CliNitPpal) %>%
  #     pivot_wider(names_from = FecProceso, values_from = SegmentoRacafe) %>%
  #     setNames(c("LinNegCod", "CliNitPpal", "Antes", "Ahora")) %>%
  #     filter(Antes %in% c("CLIENTE", NA) & Ahora == "CLIENTE A RECUPERAR") %>%
  #     select(LinNegCod, CliNitPpal)
  # 
  #   aux2 <- data_f() %>%
  #     inner_join(aux1, by = join_by(LinNegCod, CliNitPpal)) %>%
  #     group_by(PerRazSoc, LineaNegocio = CLLinNegNo, Segmento) %>%
  #     summarise(
  #       UltDespacho = max(FecFact, na.rm = TRUE),
  #       SacosMes    = sum(
  #         if_else(PrimerDia(FecDesp) == PrimerDia(Sys.Date()), Kilos / 70, 0), na.rm = TRUE
  #       ),
  #       SacosAnho   = sum(
  #         if_else(year(FecDesp) == year(Sys.Date()), Kilos / 70, 0), na.rm = TRUE
  #       ),
  #       MargenMes   = sum(
  #         if_else(PrimerDia(FecDesp) == PrimerDia(Sys.Date()), Margen / 70, 0), na.rm = TRUE
  #       ),
  #       MargenAnho  = sum(
  #         if_else(year(FecDesp) == year(Sys.Date()), Margen, 0), na.rm = TRUE
  #       ),
  #       .groups     = "drop"
  #     ) %>%
  #     janitor::adorn_totals("row", name = "TOTAL")
  #   ImprimirDTRAzSocLinNeg(
  #     aux2,
  #     noms     = c(
  #       "Raz\u00f3n Social", "L\u00ednea de Negocio", "Segmento Racaf\u00e9",
  #       "\u00daltima Facturaci\u00f3n",
  #       "Sacos Mes", "Sacos A\u00f1o", "M\u00e1rgen Mes", "M\u00e1rgen A\u00f1o"
  #     ),
  #     formatos = c(NA, NA, NA, NA, "sacos", "sacos", "dinero", "dinero"),
  #     dom = "Bft", buscar = TRUE, alto = 500
  #   )
  # })
  # 
  # ### Leads ----
  # dt_ResumenLeads <- reactive({
  #   aux1 <- data_leads_f() %>%
  #     select(-c(UsuarioCrea, FechaHoraCrea, UsuarioMod, FechaHoraModi)) %>%
  #     mutate(pct_missing = rowSums(is.na(.)) / ncol(.)) %>%
  #     select(PerRazSoc, Asesor, pct_missing, SacosPotencial, MargenPotencial)
  #   aux1$pct_missing <- sapply(aux1$pct_missing, function(x) {
  #     FormatearNumero(x, formato = "porcentaje", meta = c(0.5, 0.7), prop = FALSE)
  #   })
  #   aux1 %>%
  #     ImprimirDTLead(
  #       botones  = c("Contacto", "Editar"),
  #       noms     = c("Raz\u00f3n Social", "Asesor", "Pct Ausente", "Sacos (70kg)", "M\u00e1rgen"),
  #       formatos = c(NA, NA, NA, "sacos", "dinero"),
  #       dom      = "ft",
  #       buscar   = TRUE
  #     )
  # })
  # dd_ResumenLeads <- reactive({ datos_rv() })
  # TablaModalCelda("ResumenLeads", dt_ResumenLeads, dd_ResumenLeads, usuario, rv)
  # 
  # data_sankey_lead <- reactive({
  #   data_leads_f() %>%
  #     mutate(
  #       CliNitPpal = PerRazSoc,
  #       CLLinNegNo = LinNegocio,
  #       FecFact    = NA,
  #       FecDesp    = NA,
  #       Kilos      = ifelse(LinNegocio == 10000, SacosPotencial * 62.5, SacosPotencial * 70),
  #       Margen     = MargenPotencial,
  #       Niv1       = ifelse(is.na(EstadoCuenta), "POR CONTACTAR", EstadoCuenta),
  #       Niv2       = ifelse(
  #         EstadoCuenta == "CONTACTADO",
  #         ifelse(is.na(EstadoNegocio), "SIN ESTADO", EstadoNegocio),
  #         NA
  #       ),
  #       Niv3 = str_to_upper(case_when(
  #         EstadoCuenta == "CONTACTADO" & EstadoNegocio == "DESCARTADO" ~ RazonDescartado
  #       ))
  #     )
  # })
  # SankeyTabla("Leads", data_sankey_lead)
  # 
  # ### Consulta Individual ----
  # Individual(
  #   "ConsultaIndivual",
  #   dat          = data_individual,
  #   usr          = usuario,
  #   clientes_raw = clientes_raw,
  #   dat_global   = data_c
  # )

  # Footer ----
  output$last_update_info <- renderText({
    updates    <- map_dbl(all_caches, ~ as.numeric(.$last_update()))
    oldest_upd <- as.POSIXct(min(updates, na.rm = TRUE), origin = "1970-01-01")
    HTML(FormatearTexto(
      paste("\u00daltima actualizaci\u00f3n:", format(oldest_upd, "%Y-%m-%d %H:%M:%S")),
      tamano_pct = 0.6
    ))
  })
  
}
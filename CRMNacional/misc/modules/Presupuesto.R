# TablaDetalleDat ----
TablaDetalleDatUI <- function(id) {
  ns <- NS(id)
  div(
    class = "rt-contenedor reactable-wrap rt-sortable",
    div(class = "rt-titulo",    "Detalle de Transacciones"),
    div(class = "rt-subtitulo", "Todas las columnas del dataset activo"),
    reactable::reactableOutput(ns("tabla"))
  )
}
TablaDetalleDat <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    
    # Helpers: inferencia de formato por nombre y tipo de columna
    .es_fecha  <- function(nm) grepl("Fec|Date|fecha", nm, ignore.case = TRUE)
    .es_dinero <- function(nm) grepl("Margen|Precio|Valor|Monto|MNFCC", nm, ignore.case = TRUE)
    .es_num    <- function(nm) grepl("Sac|Kilo|Cant|Num", nm, ignore.case = TRUE)
    
    # Construccion de colDef evaluada con el df disponible en el momento del render
    .build_coldefs <- function(df) {
      nms  <- names(df)
      cols <- lapply(nms, function(nm) {
        col <- df[[nm]]
        if (.es_fecha(nm) && inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
          reactable::colDef(
            name = nm, minWidth = 110,
            cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %Y")
          )
        } else if (.es_dinero(nm) && is.numeric(col)) {
          reactable::colDef(
            name   = nm, minWidth = 110,
            format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
          )
        } else if (.es_num(nm) && is.numeric(col)) {
          reactable::colDef(
            name   = nm, minWidth = 90,
            format = reactable::colFormat(separators = TRUE, digits = 1)
          )
        } else {
          reactable::colDef(name = nm, minWidth = 100)
        }
      })
      names(cols) <- nms
      cols
    }
    
    # Render principal: colDef construidos dentro del renderReactable con df disponible
    output$tabla <- reactable::renderReactable({
      req(dat())
      df <- dat()
      reactable::reactable(
        data            = df,
        columns         = .build_coldefs(df),
        sortable        = TRUE,
        searchable      = TRUE,
        defaultPageSize = 20,
        compact         = TRUE,
        bordered        = TRUE,
        striped         = FALSE,
        highlight       = TRUE,
        language = reactable::reactableLang(
          searchPlaceholder = "Buscar...",
          noData            = "Sin resultados",
          pageInfo          = "{rowStart}\u2013{rowEnd} de {rows} registros",
          pagePrevious      = "Anterior",
          pageNext          = "Siguiente"
        ),
        theme = reactable::reactableTheme(
          headerStyle = list(fontWeight = "600", fontSize = "12px", cursor = "pointer"),
          cellStyle   = list(fontSize = "12px", padding = "3px 6px")
        )
      )
    })
  })
}

# Presupuesto ----
PresupuestoUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Header informativo: contexto activo del modulo
    # uiOutput(ns("header_contexto")),
    # KPIs principales
    fluidRow(
      column(3, racafeModulos::CajaModalUI(ns("kpi_cumpl_sacos_ytd"))),
      column(3, racafeModulos::CajaModalUI(ns("kpi_cumpl_margen_ytd"))),
      column(3, racafeModulos::CajaModalUI(ns("kpi_ritmo_req_sacos"))),
      column(3, racafeModulos::CajaModalUI(ns("kpi_ritmo_req_margen")))
    ),
    # Tabla GT resumen mensual/acumulado
    fluidRow(
      bs4Dash::bs4Card(
        title = "Presupuesto", status = "white", solidHeader = TRUE,
        width = 12, collapsible = FALSE,
        gt_output(ns("Presupuesto"))
      )
    ),
    # Switch vista acumulada / mensual para graficos de serie
    fluidRow(
      column(4,
             div(style = "margin-left: 5px;",
                 shinyWidgets::materialSwitch(
                   inputId = ns("vista_acumulada"), label = "Vista Acumulada",
                   value = TRUE, status = "danger", inline = TRUE, width = "100%"
                 )
             )
      )
    ),
    # Graficos de serie temporal sacos y margen
    fluidRow(
      column(6,
             bs4Dash::bs4Card(
               title = "Presupuesto de Sacos (70 Kgs)", status = "white",
               solidHeader = TRUE, width = 12, collapsible = TRUE,
               plotlyOutput(ns("GraficoPresupuestoSacos"))
             )
      ),
      column(6,
             bs4Dash::bs4Card(
               title = "Presupuesto de $MNFCC", status = "white",
               solidHeader = TRUE, width = 12, collapsible = TRUE,
               plotlyOutput(ns("GraficoPresupuestoMargen"))
             )
      )
    ),
    # Tabla unificada: cumplimiento + proyeccion por dimension
    fluidRow(
      bs4Dash::bs4Card(
        title = "Cumplimiento y Proyeccion por Dimension", status = "white",
        solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
        fluidRow(
          column(10,
                 racafe::BotonesRadiales(
                   inputId = "dim_breakdown",
                   choices = c(
                     "Linea de Negocio" = "linneg", "Segmento" = "segmento",
                     "Asesor" = "asesor", "Departamento" = "departamento",
                     "Municipio" = "municipio", "Unidad Comercial" = "unidadcomercial"
                   ),
                   selected = "linneg", ns = ns
                 )
          )
        ),
        uiOutput(ns("tabla_breakdown_ui")),
        uiOutput(ns("footer_metodologia")),
        fluidRow(
          column(6,
                 bs4Dash::bs4Card(
                   title = uiOutput(ns("titulo_proyeccion")),
                   status = "white", solidHeader = FALSE, width = 12,
                   collapsible = TRUE, elevation = 1,
                   fluidRow(
                     column(12,
                            div(style = "display:flex; align-items:center; gap:12px; margin-bottom:4px;",
                                racafe::BotonesRadiales(
                                  inputId = "medida_proyeccion",
                                  choices = c("Sacos" = "sacos", "Margen" = "margen"),
                                  selected = "sacos", ns = ns
                                )
                            )
                     )
                   ),
                   plotlyOutput(ns("grafico_proyeccion"))
                 )
          ),
          column(6,
                 bs4Dash::bs4Card(
                   title = "Velocidad de Ejecucion vs Requerida",
                   status = "white", solidHeader = FALSE, width = 12,
                   collapsible = TRUE, elevation = 1,
                   fluidRow(
                     column(12,
                            div(style = "display:flex; align-items:center; gap:12px; margin-bottom:4px;",
                                racafe::BotonesRadiales(
                                  inputId = "medida_velocidad",
                                  choices = c("Sacos" = "sacos", "Margen" = "margen"),
                                  selected = "sacos", ns = ns
                                )
                            )
                     )
                   ),
                   plotlyOutput(ns("grafico_velocidad"))
                 )
          )
        )
      )
    )
  )
}
Presupuesto <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    
    # Constantes ----
    .MESES <- c(
      "ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
      "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"
    )
    
    # Escala semaforica unificada
    .SEMAFORO <- list(
      umbral_exceso  = 1.00, umbral_alto = 0.80, umbral_bajo = 0.50,
      col_azul       = "#1D4ED8", col_verde    = "#15803D",
      col_amarillo   = "#B45309", col_rojo     = "#B91C1C",
      fondo_azul     = "#EFF6FF", fondo_verde    = "#F0FDF4",
      fondo_amarillo = "#FFFBEB", fondo_rojo     = "#FEF2F2",
      punto_azul     = "#2563EB", punto_verde    = "#16A34A",
      punto_amarillo = "#D97706", punto_rojo     = "#DC2626"
    )
    
    # Colores de brecha
    .CV <- "#15803D"; .CA <- "#B45309"; .CR <- "#B91C1C"
    
    # Helpers ----
    
    # Helper ETL: normalizacion de kilos por linea de negocio (evita triplicacion)
    .normalizar_kilos <- function(df) {
      df %>% mutate(
        Margen    = ifelse(is.infinite(Margen), NA, Margen),
        KILOS     = ifelse(LinNegCod == 10000, SacLote * 62.5, SacLote * 70),
        KilosFact = ifelse(is.na(KilosFact), KILOS, KilosFact)
      )
    }
    
    # Mapeo dimension a columna y etiqueta
    .dim_col <- function(dim) {
      switch(dim,
             linneg = "LinNeg", segmento = "Segmento", asesor = "Asesor",
             departamento = "Departamento", municipio = "Municipio",
             unidadcomercial = "UnidadComercial"
      )
    }
    .dim_lbl <- function(dim) {
      switch(dim,
             linneg = "Linea de Negocio", segmento = "Segmento", asesor = "Asesor",
             departamento = "Departamento", municipio = "Municipio",
             unidadcomercial = "Unidad Comercial"
      )
    }
    
    # Helper semaforo unificado — reemplaza cinco helpers anteriores
    # tipo: "fondo" | "fondo_na" | "texto_na" | "emoji" | "tramo"
    .semaforo <- function(v, tipo = "fondo_na") {
      s <- .SEMAFORO
      switch(tipo,
             tramo = dplyr::case_when(
               is.na(v)             ~ "na",
               v >= s$umbral_exceso ~ "azul",
               v >= s$umbral_alto   ~ "verde",
               v >= s$umbral_bajo   ~ "amarillo",
               TRUE                 ~ "rojo"
             ),
             fondo = dplyr::case_when(
               v >= s$umbral_exceso ~ s$fondo_azul,
               v >= s$umbral_alto   ~ s$fondo_verde,
               v >= s$umbral_bajo   ~ s$fondo_amarillo,
               TRUE                 ~ s$fondo_rojo
             ),
             fondo_na = dplyr::case_when(
               is.na(v)             ~ "white",
               v >= s$umbral_exceso ~ s$fondo_azul,
               v >= s$umbral_alto   ~ s$fondo_verde,
               v >= s$umbral_bajo   ~ s$fondo_amarillo,
               TRUE                 ~ s$fondo_rojo
             ),
             texto_na = dplyr::case_when(
               is.na(v)             ~ "#374151",
               v >= s$umbral_exceso ~ s$col_azul,
               v >= s$umbral_alto   ~ s$col_verde,
               v >= s$umbral_bajo   ~ s$col_amarillo,
               TRUE                 ~ s$col_rojo
             ),
             emoji = dplyr::case_when(
               is.na(v)             ~ "",
               v >= s$umbral_exceso ~ "\U1F535",
               v >= s$umbral_alto   ~ "\U1F7E2",
               v >= s$umbral_bajo   ~ "\U1F7E1",
               TRUE                 ~ "\U1F534"
             )
      )
    }
    
    # Color de punto scatter segun proyeccion de cierre
    .color_punto_cumpl <- function(cx, cy) {
      s <- .SEMAFORO
      dplyr::case_when(
        cy >= s$umbral_exceso                     ~ s$punto_azul,
        cy >= s$umbral_alto & cx >= s$umbral_alto ~ s$punto_verde,
        cy >= s$umbral_bajo & cx >= s$umbral_bajo ~ s$punto_amarillo,
        TRUE                                      ~ s$punto_rojo
      )
    }
    
    # Reemplazo local de gt_pct_style con escala de 4 tramos
    gt_pct_style_semaforo <- function(gt_table, ...) {
      cols <- rlang::ensyms(...)
      s    <- .SEMAFORO
      for (col in cols) {
        gt_table <- gt_table %>%
          gt::tab_style(
            style     = gt::cell_text(color = s$col_rojo, weight = "bold"),
            locations = gt::cells_body(columns = !!col, rows = !!col < s$umbral_bajo)
          ) %>%
          gt::tab_style(
            style     = gt::cell_text(color = s$col_amarillo, weight = "bold"),
            locations = gt::cells_body(
              columns = !!col, rows = !!col >= s$umbral_bajo & !!col < s$umbral_alto
            )
          ) %>%
          gt::tab_style(
            style     = gt::cell_text(color = s$col_verde, weight = "bold"),
            locations = gt::cells_body(
              columns = !!col, rows = !!col >= s$umbral_alto & !!col < s$umbral_exceso
            )
          ) %>%
          gt::tab_style(
            style     = gt::cell_text(color = s$col_azul, weight = "bold"),
            locations = gt::cells_body(columns = !!col, rows = !!col >= s$umbral_exceso)
          )
      }
      gt_table
    }
    
    # ETL base: agrupa ventas por cliente/linea/periodo/mes
    procesar_datos_base <- function(data, periodo_desde) {
      data %>%
        filter(!is.na(FecFact), year(FecFact) >= periodo_desde) %>%
        mutate(Anho = year(FecFact)) %>%
        .normalizar_kilos() %>%
        group_by(
          CliNitPpal, LinNegCod, Periodo = Anho,
          Fecha = str_to_upper(format(PrimerDia(FecFact), "%B"))
        ) %>%
        summarise(
          Sacos70   = sum(KilosFact / 70),
          MargenFCC = sum(Margen, na.rm = TRUE),
          .groups   = "drop"
        )
    }
    
    # ETL con dimensiones extendidas para breakdowns
    procesar_datos_dim <- function(data, periodo) {
      data %>%
        filter(!is.na(FecFact), year(FecFact) == periodo) %>%
        .normalizar_kilos() %>%
        group_by(
          CliNitPpal, LinNegCod, LinNeg = CLLinNegNo, Segmento,
          Asesor, Departamento = Depto, Municipio = Mpio, RazSoc = PerRazSoc,
          Fecha = str_to_upper(format(PrimerDia(FecFact), "%B"))
        ) %>%
        summarise(
          Sacos70   = sum(KilosFact / 70),
          MargenFCC = sum(Margen, na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        mutate(
          Mes_Num         = match(Fecha, .MESES),
          UnidadComercial = paste0(RazSoc, " \u2014 ", LinNeg)
        )
    }
    
    # ETL lotes: detalle de facturas para modal de breakdown
    procesar_lotes_dim <- function(data, periodo) {
      data %>%
        filter(!is.na(FecFact), year(FecFact) == periodo) %>%
        .normalizar_kilos() %>%
        mutate(
          Sacos70         = KilosFact / 70,
          UnidadComercial = paste0(PerRazSoc, " \u2014 ", CLLinNegNo)
        ) %>%
        select(
          FecFact, CliNitPpal, PerRazSoc, LinNegCod, CLLinNegNo,
          Segmento, Asesor, Departamento = Depto, Municipio = Mpio,
          UnidadComercial, CLLotCod, Sacos70, Margen
        )
    }
    
    # Presupuesto mensual: ultimo proceso del anio por cliente/linea, cruzado con dat()
    obtener_presupuesto <- function(data, periodo) {
      CargarDatos("CRMNALCLIENTE") %>%
        filter(year(FecProceso) == year(Sys.Date())) %>%
        group_by(LinNegCod, CliNitPpal) %>%
        filter(FecProceso == max(FecProceso)) %>%
        ungroup() %>%
        inner_join(
          data %>% select(LinNegCod, Segmento, Asesor, Responsable) %>% distinct(),
          by = join_by(LinNegCod, Segmento, Asesor, Responsable)
        ) %>%
        group_by(LinNegCod, CliNitPpal, Segmento) %>%
        summarise(
          PptoSS = sum(SSPpto,    na.rm = TRUE) / 12,
          PptoMa = sum(MNFCCPpto, na.rm = TRUE) / 12,
          .groups = "drop"
        ) %>%
        crossing(Periodo = periodo, Fecha = .MESES)
    }
    
    # Presupuesto anual con dimensiones extendidas
    obtener_presupuesto_dim <- function(data, periodo) {
      CargarDatos("CRMNALCLIENTE") %>%
        filter(year(FecProceso) == year(Sys.Date())) %>%
        group_by(LinNegCod, CliNitPpal) %>%
        filter(FecProceso == max(FecProceso)) %>%
        ungroup() %>%
        inner_join(
          data %>% select(LinNegCod, Segmento, Asesor, Responsable) %>% distinct(),
          by = join_by(LinNegCod, Segmento, Asesor, Responsable)
        ) %>%
        group_by(
          CliNitPpal, LinNegCod, LinNeg = LinNegocio, Segmento,
          Asesor, Departamento = Depto, Municipio = Mpio
        ) %>%
        summarise(
          PptoSS_anual = sum(SSPpto,    na.rm = TRUE),
          PptoMa_anual = sum(MNFCCPpto, na.rm = TRUE),
          .groups      = "drop"
        )
    }
    
    # Serie temporal por periodo/mes para graficos de linea
    preparar_datos_grafico <- function(datos_base, ppto, periodo) {
      db <- datos_base %>%
        filter(Periodo == periodo) %>%
        group_by(Periodo, Fecha) %>%
        summarise(
          Sacos70   = sum(Sacos70,   na.rm = TRUE),
          MargenFCC = sum(MargenFCC, na.rm = TRUE),
          .groups   = "drop"
        )
      ppto_ag <- ppto %>%
        filter(Periodo == periodo) %>%
        group_by(Periodo, Fecha) %>%
        summarise(
          PptoSS = sum(PptoSS, na.rm = TRUE),
          PptoMa = sum(PptoMa, na.rm = TRUE),
          .groups = "drop"
        )
      db %>%
        full_join(ppto_ag, by = c("Periodo", "Fecha")) %>%
        mutate(
          Mes_Num     = match(Fecha, .MESES),
          Sacos_Real  = coalesce(Sacos70,   0),
          Margen_Real = coalesce(MargenFCC, 0),
          Sacos_Ppto  = coalesce(PptoSS,   0),
          Margen_Ppto = coalesce(PptoMa,   0)
        ) %>%
        arrange(Mes_Num)
    }
    
    # Metricas de cumplimiento y proyeccion agregadas por dimension
    # Para unidadcomercial (es_multi=TRUE): ppto_dim no tiene RazSoc — se agrupa por
    # CliNitPpal+LinNegCod (clave natural) y luego se enriquece desde datos_dim.
    agregar_por_dimension <- function(datos_dim, ppto_dim, dim_cols) {
      cal        <- calendario_r()
      mes_actual <- cal$mes_actual
      meses_rest <- cal$meses_rest
      es_multi   <- length(dim_cols) > 1
      
      # Columnas de agrupacion disponibles en ppto_dim (no tiene RazSoc)
      ppto_dim_cols <- if (es_multi) c("CliNitPpal", "LinNegCod") else dim_cols
      
      .group_by_dims <- function(df) {
        if (es_multi) group_by(df, across(all_of(dim_cols)))
        else          group_by(df, dim = !!sym(dim_cols))
      }
      .group_by_ppto <- function(df) {
        if (es_multi) group_by(df, across(all_of(ppto_dim_cols)))
        else          group_by(df, dim = !!sym(ppto_dim_cols))
      }
      .join_by_dims <- function(x, y) {
        if (es_multi) left_join(x, y, by = dim_cols)
        else          left_join(x, y, by = "dim")
      }
      .join_by_ppto <- function(x, y) {
        if (es_multi) left_join(x, y, by = ppto_dim_cols)
        else          left_join(x, y, by = "dim")
      }
      
      ejec_ytd <- datos_dim %>%
        filter(Mes_Num <= mes_actual) %>%
        .group_by_dims() %>%
        summarise(
          Sacos_YTD  = sum(Sacos70,   na.rm = TRUE),
          Margen_YTD = sum(MargenFCC, na.rm = TRUE),
          .groups    = "drop"
        )
      ejec_mes <- datos_dim %>%
        filter(Fecha == .MESES[mes_actual]) %>%
        .group_by_dims() %>%
        summarise(
          Sacos_Mes  = sum(Sacos70,   na.rm = TRUE),
          Margen_Mes = sum(MargenFCC, na.rm = TRUE),
          .groups    = "drop"
        )
      
      # ppto_ag se agrega por claves disponibles en ppto_dim
      ppto_ag <- ppto_dim %>%
        .group_by_ppto() %>%
        summarise(
          Ppto_Sacos_Anual  = sum(PptoSS_anual, na.rm = TRUE),
          Ppto_Margen_Anual = sum(PptoMa_anual, na.rm = TRUE),
          .groups           = "drop"
        ) %>%
        mutate(
          Ppto_Sacos_Mes  = Ppto_Sacos_Anual  / 12,
          Ppto_Margen_Mes = Ppto_Margen_Anual / 12,
          Ppto_Sacos_YTD  = Ppto_Sacos_Anual  * mes_actual / 12,
          Ppto_Margen_YTD = Ppto_Margen_Anual * mes_actual / 12
        )
      
      # Para unidadcomercial: enriquecer ejec con dim_cols luego hacer join con ppto por clave natural
      if (es_multi) {
        ejec_base <- ejec_ytd %>%
          .join_by_dims(ejec_mes) %>%
          mutate(across(c(Sacos_YTD, Margen_YTD, Sacos_Mes, Margen_Mes), ~ coalesce(.x, 0)))
        
        # Recuperar CliNitPpal y LinNegCod desde datos_dim para poder joinear con ppto_ag
        dim_keys <- datos_dim %>%
          select(all_of(c(dim_cols, "CliNitPpal", "LinNegCod"))) %>%
          distinct()
        
        ejec_base %>%
          left_join(dim_keys, by = dim_cols) %>%
          .join_by_ppto(ppto_ag) %>%
          mutate(
            Cumpl_Sacos_YTD     = SiError_0(Sacos_YTD  / Ppto_Sacos_YTD),
            Cumpl_Margen_YTD    = SiError_0(Margen_YTD / Ppto_Margen_YTD),
            Cumpl_Sacos_Mes     = SiError_0(Sacos_Mes  / Ppto_Sacos_Mes),
            Cumpl_Margen_Mes    = SiError_0(Margen_Mes / Ppto_Margen_Mes),
            Brecha_Sacos        = Sacos_YTD  - Ppto_Sacos_YTD,
            Brecha_Margen       = Margen_YTD - Ppto_Margen_YTD,
            Ritmo_Sacos_Actual  = SiError_0(Sacos_YTD  / pmax(mes_actual, 1)),
            Ritmo_Margen_Actual = SiError_0(Margen_YTD / pmax(mes_actual, 1)),
            Ritmo_Sacos_Req     = SiError_0((Ppto_Sacos_Anual  - Sacos_YTD)  / pmax(meses_rest, 1)),
            Ritmo_Margen_Req    = SiError_0((Ppto_Margen_Anual - Margen_YTD) / pmax(meses_rest, 1)),
            Meses_Restantes     = meses_rest,
            Meta_Mensual_Sacos  = Ritmo_Sacos_Req,
            Meta_Mensual_Margen = Ritmo_Margen_Req,
            Proyeccion_Sacos    = Sacos_YTD  + Ritmo_Sacos_Actual  * meses_rest,
            Proyeccion_Margen   = Margen_YTD + Ritmo_Margen_Actual * meses_rest,
            Prob_Sacos          = pmin(SiError_0(Proyeccion_Sacos  / Ppto_Sacos_Anual),  1),
            Prob_Margen         = pmin(SiError_0(Proyeccion_Margen / Ppto_Margen_Anual), 1)
          ) %>%
          arrange(desc(Sacos_YTD))
      } else {
        ppto_ag %>%
          .join_by_dims(ejec_ytd) %>%
          .join_by_dims(ejec_mes) %>%
          mutate(
            across(c(Sacos_YTD, Margen_YTD, Sacos_Mes, Margen_Mes), ~ coalesce(.x, 0)),
            Cumpl_Sacos_YTD     = SiError_0(Sacos_YTD  / Ppto_Sacos_YTD),
            Cumpl_Margen_YTD    = SiError_0(Margen_YTD / Ppto_Margen_YTD),
            Cumpl_Sacos_Mes     = SiError_0(Sacos_Mes  / Ppto_Sacos_Mes),
            Cumpl_Margen_Mes    = SiError_0(Margen_Mes / Ppto_Margen_Mes),
            Brecha_Sacos        = Sacos_YTD  - Ppto_Sacos_YTD,
            Brecha_Margen       = Margen_YTD - Ppto_Margen_YTD,
            Ritmo_Sacos_Actual  = SiError_0(Sacos_YTD  / pmax(mes_actual, 1)),
            Ritmo_Margen_Actual = SiError_0(Margen_YTD / pmax(mes_actual, 1)),
            Ritmo_Sacos_Req     = SiError_0((Ppto_Sacos_Anual  - Sacos_YTD)  / pmax(meses_rest, 1)),
            Ritmo_Margen_Req    = SiError_0((Ppto_Margen_Anual - Margen_YTD) / pmax(meses_rest, 1)),
            Meses_Restantes     = meses_rest,
            Meta_Mensual_Sacos  = Ritmo_Sacos_Req,
            Meta_Mensual_Margen = Ritmo_Margen_Req,
            Proyeccion_Sacos    = Sacos_YTD  + Ritmo_Sacos_Actual  * meses_rest,
            Proyeccion_Margen   = Margen_YTD + Ritmo_Margen_Actual * meses_rest,
            Prob_Sacos          = pmin(SiError_0(Proyeccion_Sacos  / Ppto_Sacos_Anual),  1),
            Prob_Margen         = pmin(SiError_0(Proyeccion_Margen / Ppto_Margen_Anual), 1)
          ) %>%
          arrange(desc(Sacos_YTD))
      }
    }
    
    # Grafico de serie temporal ejecucion vs presupuesto con meta futura
    construir_grafico_serie <- function(
    datos_grafico, vista_acumulada, col_real, col_ppto, col_ref_na,
    titulo_acum, titulo_mensual, label_y_acum, label_y_mensual, formato_y = "numero"
    ) {
      mes_actual <- month(Sys.Date())
      
      datos_grafico <- datos_grafico %>%
        arrange(Mes_Num) %>%
        mutate(
          acum_real = cumsum(ifelse(is.na(.data[[col_real]]), 0, .data[[col_real]])),
          acum_ppto = cumsum(.data[[col_ppto]])
        )
      
      ultimo_mes <- suppressWarnings(
        max(datos_grafico$Mes_Num[!is.na(datos_grafico[[col_ref_na]])], na.rm = TRUE)
      )
      if (is.infinite(ultimo_mes)) ultimo_mes <- 0
      
      datos_grafico <- datos_grafico %>%
        mutate(acum_real = ifelse(Mes_Num <= ultimo_mes, acum_real, NA_real_))
      
      # ejec_acum restringido al mismo criterio que acum_real (correccion punto 8)
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
          ),
          Fecha = factor(Fecha, levels = .MESES, ordered = TRUE)
        )
      
      if (vista_acumulada) {
        datos_plot <- datos_grafico %>%
          select(Fecha, acum_ppto, acum_real, meta_acum) %>%
          pivot_longer(c(acum_ppto, acum_real, meta_acum), names_to = "Tipo", values_to = "Valor") %>%
          mutate(
            Tipo = dplyr::recode(Tipo,
                                 acum_ppto = "Presupuesto Acumulado",
                                 acum_real = "Ejecutado Acumulado",
                                 meta_acum = "Meta Acumulada para Ppto"
            ),
            Tipo = factor(Tipo, levels = c(
              "Presupuesto Acumulado", "Ejecutado Acumulado", "Meta Acumulada para Ppto"
            ))
          )
        titulo_g <- titulo_acum; titulo_y <- label_y_acum
      } else {
        datos_plot <- datos_grafico %>%
          select(Fecha, ppto = all_of(col_ppto), real = all_of(col_real), meta_men = meta_mensual) %>%
          pivot_longer(c(ppto, real, meta_men), names_to = "Tipo", values_to = "Valor") %>%
          mutate(
            Tipo = dplyr::recode(Tipo,
                                 ppto     = "Presupuesto Mensual",
                                 real     = "Ejecutado Mensual",
                                 meta_men = "Meta Mensual para Ppto"
            ),
            Tipo = factor(Tipo, levels = c(
              "Presupuesto Mensual", "Ejecutado Mensual", "Meta Mensual para Ppto"
            ))
          )
        titulo_g <- titulo_mensual; titulo_y <- label_y_mensual
      }
      
      trazas <- list(
        list(
          tipo  = if (vista_acumulada) "Presupuesto Acumulado"    else "Presupuesto Mensual",
          color = "#52525C", dash = "dash"
        ),
        list(
          tipo  = if (vista_acumulada) "Ejecutado Acumulado"      else "Ejecutado Mensual",
          color = "#1C398E", dash = "solid"
        ),
        list(
          tipo  = if (vista_acumulada) "Meta Acumulada para Ppto" else "Meta Mensual para Ppto",
          color = "#82181A", dash = "dot"
        )
      )
      
      p <- plot_ly()
      for (tr in trazas) {
        p <- p %>% add_trace(
          data = filter(datos_plot, Tipo == tr$tipo), x = ~Fecha, y = ~Valor,
          name = tr$tipo, type = "scatter", mode = "lines+markers",
          line   = list(color = tr$color, dash = tr$dash, width = 2),
          marker = list(color = tr$color),
          hovertemplate = paste0(
            "<b>", tr$tipo, "</b><br>Mes: %{x}<br>Valor: %{y:,.0f}<extra></extra>"
          )
        )
      }
      
      yax <- list(title = titulo_y, tickformat = ",", rangemode = "tozero")
      if (formato_y == "dinero") yax$tickprefix <- "$"
      
      p %>%
        layout(
          title     = titulo_g,
          xaxis     = list(title = "Mes"),
          yaxis     = yax,
          legend    = list(
            orientation = "h", x = 0.5, y = -0.2, xanchor = "center", yanchor = "top"
          ),
          hovermode = "x unified"
        ) %>%
        config(displayModeBar = FALSE, displaylogo = FALSE)
    }
    
    # Reactivos ----
    
    # Calendario centralizado: evita multiples Sys.Date() en KPIs
    calendario_r <- reactive({
      mes <- month(Sys.Date())
      list(mes_actual = mes, meses_rest = 13L - mes)
    })
    
    # Generales
    periodo_r    <- reactive({ year(max(dat()$FecFact, na.rm = TRUE)) })
    datos_base_r <- reactive({ procesar_datos_base(dat(), periodo_r() - 1L) })
    datos_act_r  <- reactive({ procesar_datos_base(dat(), periodo_r()) })
    ppto_r       <- reactive({ obtener_presupuesto(data = dat(), periodo = periodo_r()) })
    datos_dim_r  <- reactive({ procesar_datos_dim(dat(), periodo_r()) })
    ppto_dim_r   <- reactive({ obtener_presupuesto_dim(dat(), periodo_r()) })
    lotes_dim_r  <- reactive({ procesar_lotes_dim(dat(), periodo_r()) })
    
    # Datos para graficos de serie
    datos_grafico_r <- reactive({ preparar_datos_grafico(datos_act_r(), ppto_r(), periodo_r()) })
    
    # Nombres de columnas dinamicos segun periodo
    col_names_r <- reactive({
      p  <- periodo_r()
      pa <- p - 1L
      list(
        sacos_ant       = paste0("Sacos70_", pa),    margen_ant      = paste0("MargenFCC_", pa),
        sacos_act       = paste0("Sacos70_", p),     margen_act      = paste0("MargenFCC_", p),
        ppto_sacos      = paste0("PptoSS_", p),      ppto_margen     = paste0("PptoMa_", p),
        cumpl_sacos     = paste0("CumplSS", p),      cumpl_margen    = paste0("CumplMgn", p),
        acum_sacos_ant  = paste0("Acum_Sacos70_", pa),
        acum_margen_ant = paste0("Acum_MargenFCC_", pa),
        acum_ppto_ss    = paste0("Acum_PptoSS_", p),
        acum_sacos_act  = paste0("Acum_Sacos70_", p),
        acum_ppto_ma    = paste0("Acum_PptoMa_", p),
        acum_margen_act = paste0("Acum_MargenFCC_", p),
        cumpl_ss_acum   = paste0("CumplSS_Acum_", p),
        cumpl_mgn_acum  = paste0("CumplMgn_Acum_", p),
        varss_acum      = paste0("VarSS_Acum_", p),
        varmgn_acum     = paste0("VarMgn_Acum_", p)
      )
    })
    
    # Breakdown por dimension seleccionada
    breakdown_r <- reactive({
      req(datos_dim_r(), ppto_dim_r())
      dim_cols <- if (input$dim_breakdown == "unidadcomercial") {
        c("RazSoc", "LinNeg")
      } else {
        .dim_col(input$dim_breakdown)
      }
      agregar_por_dimension(datos_dim_r(), ppto_dim_r(), dim_cols)
    })
    
    # Datos de breakdown preparados para TablaReactable (con .dim_key y fila TOTAL)
    breakdown_tabla_r <- reactive({
      req(breakdown_r())
      df    <- breakdown_r()
      es_uc <- input$dim_breakdown == "unidadcomercial"
      
      cols_suma <- c(
        "Sacos_Mes", "Ppto_Sacos_Mes", "Sacos_YTD", "Ppto_Sacos_YTD",
        "Brecha_Sacos", "Proyeccion_Sacos", "Meta_Mensual_Sacos",
        "Margen_Mes", "Ppto_Margen_Mes", "Margen_YTD", "Ppto_Margen_YTD",
        "Brecha_Margen", "Proyeccion_Margen", "Meta_Mensual_Margen",
        "Ppto_Sacos_Anual", "Ppto_Margen_Anual"
      )
      
      fila_total <- df %>%
        summarise(across(all_of(cols_suma), ~ sum(.x, na.rm = TRUE))) %>%
        mutate(
          Cumpl_Sacos_YTD  = SiError_0(Sacos_YTD  / Ppto_Sacos_YTD),
          Cumpl_Margen_YTD = SiError_0(Margen_YTD / Ppto_Margen_YTD),
          Cumpl_Sacos_Mes  = SiError_0(Sacos_Mes  / Ppto_Sacos_Mes),
          Cumpl_Margen_Mes = SiError_0(Margen_Mes / Ppto_Margen_Mes),
          Prob_Sacos       = pmin(SiError_0(Proyeccion_Sacos  / Ppto_Sacos_Anual), 1),
          Prob_Margen      = pmin(SiError_0(Proyeccion_Margen / Ppto_Margen_Anual), 1),
          Sem_Sacos        = purrr::map_chr(Prob_Sacos,  ~ .semaforo(.x, "emoji")),
          Sem_Margen       = purrr::map_chr(Prob_Margen, ~ .semaforo(.x, "emoji")),
          .dim_key         = "TOTAL"
        )
      
      dim_label <- .dim_lbl(input$dim_breakdown)
      if (es_uc) {
        df <- df %>%
          mutate(
            Sem_Sacos  = purrr::map_chr(Prob_Sacos,  ~ .semaforo(.x, "emoji")),
            Sem_Margen = purrr::map_chr(Prob_Margen, ~ .semaforo(.x, "emoji")),
            .dim_key   = paste0(RazSoc, "||", LinNeg)
          )
        fila_total <- fila_total %>% mutate(RazSoc = "TOTAL", LinNeg = "")
        bind_rows(df, fila_total) %>%
          select(
            .dim_key, RazSoc, LinNeg,
            Sacos_Mes, Ppto_Sacos_Mes, Cumpl_Sacos_Mes,
            Sacos_YTD, Ppto_Sacos_YTD, Cumpl_Sacos_YTD, Brecha_Sacos,
            Proyeccion_Sacos, Prob_Sacos, Sem_Sacos, Meta_Mensual_Sacos,
            Margen_Mes, Ppto_Margen_Mes, Cumpl_Margen_Mes,
            Margen_YTD, Ppto_Margen_YTD, Cumpl_Margen_YTD, Brecha_Margen,
            Proyeccion_Margen, Prob_Margen, Sem_Margen, Meta_Mensual_Margen
          )
      } else {
        df <- df %>%
          mutate(
            Sem_Sacos  = purrr::map_chr(Prob_Sacos,  ~ .semaforo(.x, "emoji")),
            Sem_Margen = purrr::map_chr(Prob_Margen, ~ .semaforo(.x, "emoji")),
            .dim_key   = dim
          )
        fila_total <- fila_total %>% mutate(.dim_key = "TOTAL")
        bind_rows(df, fila_total) %>%
          select(
            .dim_key,
            Sacos_Mes, Ppto_Sacos_Mes, Cumpl_Sacos_Mes,
            Sacos_YTD, Ppto_Sacos_YTD, Cumpl_Sacos_YTD, Brecha_Sacos,
            Proyeccion_Sacos, Prob_Sacos, Sem_Sacos, Meta_Mensual_Sacos,
            Margen_Mes, Ppto_Margen_Mes, Cumpl_Margen_Mes,
            Margen_YTD, Ppto_Margen_YTD, Cumpl_Margen_YTD, Brecha_Margen,
            Proyeccion_Margen, Prob_Margen, Sem_Margen, Meta_Mensual_Margen
          )
      }
    })
    
    # KPIs derivados de datos_grafico_r — sin pasada extra sobre dat()
    kpi_sacos_r <- reactive({
      cal  <- calendario_r()
      dg   <- datos_grafico_r() %>% filter(Mes_Num <= cal$mes_actual)
      ejec <- sum(dg$Sacos_Real,  na.rm = TRUE)
      ppto <- sum(dg$Sacos_Ppto,  na.rm = TRUE)
      list(ejec = ejec, ppto = ppto, cumpl = SiError_0(ejec / ppto),
           periodo = periodo_r(), mes_actual = cal$mes_actual)
    })
    kpi_margen_r <- reactive({
      cal  <- calendario_r()
      dg   <- datos_grafico_r() %>% filter(Mes_Num <= cal$mes_actual)
      ejec <- sum(dg$Margen_Real, na.rm = TRUE)
      ppto <- sum(dg$Margen_Ppto, na.rm = TRUE)
      list(ejec = ejec, ppto = ppto, cumpl = SiError_0(ejec / ppto),
           periodo = periodo_r(), mes_actual = cal$mes_actual)
    })
    kpi_ritmo_sacos_r <- reactive({
      cal        <- calendario_r()
      dg         <- datos_grafico_r()
      ejec_ytd   <- dg %>% filter(Mes_Num <= cal$mes_actual) %>%
        summarise(s = sum(Sacos_Real,  na.rm = TRUE)) %>% pull(s)
      ppto_anual <- dg %>% summarise(p = sum(Sacos_Ppto,  na.rm = TRUE)) %>% pull(p)
      faltante   <- ppto_anual - ejec_ytd
      list(ritmo = SiError_0(faltante / pmax(cal$meses_rest, 1)),
           faltante = faltante, meses_rest = cal$meses_rest)
    })
    kpi_ritmo_margen_r <- reactive({
      cal        <- calendario_r()
      dg         <- datos_grafico_r()
      ejec_ytd   <- dg %>% filter(Mes_Num <= cal$mes_actual) %>%
        summarise(m = sum(Margen_Real, na.rm = TRUE)) %>% pull(m)
      ppto_anual <- dg %>% summarise(p = sum(Margen_Ppto, na.rm = TRUE)) %>% pull(p)
      faltante   <- ppto_anual - ejec_ytd
      list(ritmo = SiError_0(faltante / pmax(cal$meses_rest, 1)),
           faltante = faltante, meses_rest = cal$meses_rest)
    })
    
    # Outputs ----
    
    ## Header informativo: contexto activo derivado de dat() como titulo textual
    output$header_contexto <- renderUI({
      df      <- dat()
      periodo <- periodo_r()
      
      # Extrae valores unicos limpios de una columna
      .vals <- function(col) {
        v <- sort(unique(col[!is.na(col) & col != "" & col != "SIN DATO"]))
        if (length(v) == 0) "Todos" else paste(v, collapse = ", ")
      }
      
      asesores <- .vals(df$Asesor)
      lineas   <- .vals(df$CLLinNegNo)
      segmentos <- .vals(df$Segmento)
      
      titulo_txt <- paste0(
        "Seguimiento de Presupuesto ", periodo,
        " \u2014 ", asesores,
        " \u00b7 ", lineas,
        " \u00b7 ", segmentos
      )
      
      tags$h4(
        titulo_txt,
        style = "font-weight:700; color:#1E293B; margin:0 0 12px 0; line-height:1.3;"
      )
    })
    
    ## Modulos KPI: CajaModal ----
    racafeModulos::CajaModal(
      id = "kpi_cumpl_sacos_ytd", valor = reactive(kpi_sacos_r()$cumpl),
      formato = "porcentaje",
      texto   = reactive(paste0("Cumpl. Sacos \u2014 Acum. ", kpi_sacos_r()$periodo)),
      icono   = "check-double", colores = reactive(c(fondo = "white")),
      color_fondo_hex = reactive(.semaforo(kpi_sacos_r()$cumpl, "fondo")),
      mostrar_boton   = FALSE,
      footer = reactive(
        paste0(FormatearNumero(kpi_sacos_r()$ejec, "coma"), " sacos facturados de ",
               FormatearNumero(kpi_sacos_r()$ppto, "coma"), " presupuestados") %>% HTML
      )
    )
    racafeModulos::CajaModal(
      id = "kpi_cumpl_margen_ytd", valor = reactive(kpi_margen_r()$cumpl),
      formato = "porcentaje",
      texto   = reactive(paste0("Cumpl. Margen \u2014 Acum. ", kpi_margen_r()$periodo)),
      icono   = "dollar-sign", colores = reactive(c(fondo = "white")),
      color_fondo_hex = reactive(.semaforo(kpi_margen_r()$cumpl, "fondo")),
      mostrar_boton   = FALSE,
      footer = reactive(
        paste0(FormatearNumero(kpi_margen_r()$ejec, "dinero"), " facturado de ",
               FormatearNumero(kpi_margen_r()$ppto, "dinero"), " presupuestado") %>% HTML
      )
    )
    racafeModulos::CajaModal(
      id = "kpi_ritmo_req_sacos", valor = reactive(kpi_ritmo_sacos_r()$ritmo),
      formato = "numero", texto = "Sacos / mes requeridos", icono = "arrow-trend-up",
      colores = reactive(c(fondo = "white")), color_fondo_hex = "#F8FAFC",
      mostrar_boton = FALSE,
      footer = reactive(
        paste0("Faltan ", FormatearNumero(kpi_ritmo_sacos_r()$faltante, "coma"),
               " sacos en ", kpi_ritmo_sacos_r()$meses_rest,
               " meses para cerrar el presupuesto") %>% HTML
      )
    )
    racafeModulos::CajaModal(
      id = "kpi_ritmo_req_margen", valor = reactive(kpi_ritmo_margen_r()$ritmo),
      formato = "dinero", texto = "Margen / mes requerido", icono = "money-bill-trend-up",
      colores = reactive(c(fondo = "white")), color_fondo_hex = "#F8FAFC",
      mostrar_boton = FALSE,
      footer = reactive(
        paste0("Faltan ", FormatearNumero(kpi_ritmo_margen_r()$faltante, "dinero"),
               " en ", kpi_ritmo_margen_r()$meses_rest,
               " meses para cerrar el presupuesto de margen") %>% HTML
      )
    )
    
    ## Tabla GT mensual/acumulada ----
    output$Presupuesto <- render_gt({
      waiter_show(html = preloader2$html, color = preloader2$color)
      periodo     <- periodo_r()
      periodo_ant <- periodo - 1L
      cn          <- col_names_r()
      
      datos_ant <- datos_base_r() %>% filter(Periodo == periodo_ant)
      datos_act <- datos_act_r()  %>% filter(Periodo == periodo)
      ppto      <- ppto_r()
      
      t1 <- bind_rows(datos_ant, datos_act) %>%
        full_join(ppto, by = join_by("CliNitPpal", "LinNegCod", "Periodo", "Fecha")) %>%
        complete(CliNitPpal, LinNegCod, Periodo, Fecha,
                 fill = list(Sacos70 = 0, MargenFCC = 0)) %>%
        group_by(Periodo, Fecha) %>%
        summarise(
          Sacos70   = sum(Sacos70,   na.rm = TRUE),
          MargenFCC = sum(MargenFCC, na.rm = TRUE),
          MargenKilo = sum(MargenFCC / (Sacos70 / 70), na.rm = TRUE),
          PptoSS    = sum(PptoSS,    na.rm = TRUE),
          PptoMa    = sum(PptoMa,    na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        pivot_wider(names_from = Periodo, values_from = Sacos70:PptoMa) %>%
        mutate(
          Fecha = factor(Fecha, levels = .MESES, ordered = TRUE),
          across(starts_with(c("Sacos70_", "MargenFCC_")), ~ ifelse(.x == 0, NA, .x)),
          !!cn$cumpl_sacos  := !!sym(cn$sacos_act)  / !!sym(cn$ppto_sacos),
          VarSS              = Variacion(!!sym(cn$sacos_ant),  !!sym(cn$sacos_act)),
          !!cn$cumpl_margen := !!sym(cn$margen_act) / !!sym(cn$ppto_margen),
          VarMgn             = Variacion(!!sym(cn$margen_ant), !!sym(cn$margen_act))
        ) %>%
        arrange(Fecha) %>%
        mutate(
          !!cn$acum_sacos_ant  := ifelse(!is.na(!!sym(cn$sacos_ant)),
                                         cumsum(coalesce(!!sym(cn$sacos_ant),  0)), NA),
          !!cn$acum_margen_ant := ifelse(!is.na(!!sym(cn$margen_ant)),
                                         cumsum(coalesce(!!sym(cn$margen_ant), 0)), NA),
          !!cn$acum_ppto_ss    := ifelse(!is.na(!!sym(cn$ppto_sacos)),
                                         cumsum(coalesce(!!sym(cn$ppto_sacos), 0)), NA),
          !!cn$acum_ppto_ma    := ifelse(!is.na(!!sym(cn$ppto_margen)),
                                         cumsum(coalesce(!!sym(cn$ppto_margen), 0)), NA),
          !!cn$acum_sacos_act  := ifelse(!is.na(!!sym(cn$sacos_act)),
                                         cumsum(coalesce(!!sym(cn$sacos_act),  0)), NA),
          !!cn$acum_margen_act := ifelse(!is.na(!!sym(cn$margen_act)),
                                         cumsum(coalesce(!!sym(cn$margen_act), 0)), NA),
          !!cn$cumpl_ss_acum   := ifelse(
            !is.na(!!sym(cn$acum_sacos_act)) & !is.na(!!sym(cn$acum_ppto_ss)) &
              !!sym(cn$acum_ppto_ss) != 0,
            !!sym(cn$acum_sacos_act) / !!sym(cn$acum_ppto_ss), NA
          ),
          !!cn$cumpl_mgn_acum  := ifelse(
            !is.na(!!sym(cn$acum_margen_act)) & !is.na(!!sym(cn$acum_ppto_ma)) &
              !!sym(cn$acum_ppto_ma) != 0,
            !!sym(cn$acum_margen_act) / !!sym(cn$acum_ppto_ma), NA
          ),
          !!cn$varss_acum      := ifelse(
            !is.na(!!sym(cn$acum_sacos_ant)) & !is.na(!!sym(cn$acum_sacos_act)),
            Variacion(!!sym(cn$acum_sacos_ant), !!sym(cn$acum_sacos_act)), NA
          ),
          !!cn$varmgn_acum     := ifelse(
            !is.na(!!sym(cn$acum_margen_ant)) & !is.na(!!sym(cn$acum_margen_act)),
            Variacion(!!sym(cn$acum_margen_ant), !!sym(cn$acum_margen_act)), NA
          )
        ) %>%
        select(
          Fecha,
          !!sym(cn$sacos_ant),  !!sym(cn$acum_sacos_ant),
          !!sym(cn$margen_ant), !!sym(cn$acum_margen_ant),
          !!sym(cn$ppto_sacos), !!sym(cn$sacos_act), !!sym(cn$cumpl_sacos), VarSS,
          !!sym(cn$acum_ppto_ss),   !!sym(cn$acum_sacos_act),
          !!sym(cn$cumpl_ss_acum),  !!sym(cn$varss_acum),
          !!sym(cn$ppto_margen), !!sym(cn$margen_act), !!sym(cn$cumpl_margen), VarMgn,
          !!sym(cn$acum_ppto_ma),   !!sym(cn$acum_margen_act),
          !!sym(cn$cumpl_mgn_acum), !!sym(cn$varmgn_acum)
        )
      
      fila_total <- t1 %>%
        summarise(
          !!sym(cn$sacos_ant)   := sum(!!sym(cn$sacos_ant),   na.rm = TRUE),
          !!sym(cn$margen_ant)  := sum(!!sym(cn$margen_ant),  na.rm = TRUE),
          !!sym(cn$ppto_sacos)  := sum(!!sym(cn$ppto_sacos),  na.rm = TRUE),
          !!sym(cn$sacos_act)   := sum(!!sym(cn$sacos_act),   na.rm = TRUE),
          !!sym(cn$ppto_margen) := sum(!!sym(cn$ppto_margen), na.rm = TRUE),
          !!sym(cn$margen_act)  := sum(!!sym(cn$margen_act),  na.rm = TRUE),
          across(c(
            !!sym(cn$acum_sacos_ant), !!sym(cn$acum_margen_ant),
            !!sym(cn$acum_ppto_ss),   !!sym(cn$acum_sacos_act),
            !!sym(cn$acum_ppto_ma),   !!sym(cn$acum_margen_act),
            !!sym(cn$cumpl_ss_acum),  !!sym(cn$varss_acum),
            !!sym(cn$cumpl_mgn_acum), !!sym(cn$varmgn_acum)
          ), ~ NA_real_)
        ) %>%
        mutate(
          Fecha = "TOTAL",
          !!cn$cumpl_sacos  := !!sym(cn$sacos_act)  / !!sym(cn$ppto_sacos),
          VarSS              = Variacion(!!sym(cn$sacos_ant),  !!sym(cn$sacos_act)),
          !!cn$cumpl_margen := !!sym(cn$margen_act) / !!sym(cn$ppto_margen),
          VarMgn             = Variacion(!!sym(cn$margen_ant), !!sym(cn$margen_act))
        )
      
      gt1 <- bind_rows(t1, fila_total) %>%
        gt() %>%
        tab_header(
          title    = md(paste0("**Seguimiento de Presupuesto ", periodo, "**")),
          subtitle = md(paste0(
            "Comparativo mensual y acumulado de Sacos y Margen FCC vs. Presupuesto \u2014 ",
            "Periodo anterior: ", periodo_ant
          ))
        ) %>%
        tab_spanner(label = as.character(periodo_ant), columns = 2:5) %>%
        tab_spanner(label = as.character(periodo),     columns = 6:21) %>%
        tab_spanner(label = "Sacos Mensual",    columns = 6:9) %>%
        tab_spanner(label = "Sacos Acumulado",  columns = 10:13) %>%
        tab_spanner(label = "$MNFCC Mensual",   columns = 14:17) %>%
        tab_spanner(label = "$MNFCC Acumulado", columns = 18:21) %>%
        cols_label(
          Fecha = "",
          .list = setNames(
            c("", "Sacos", "Sacos Acum.", "Margen", "Margen Acum.",
              rep(c("Presupuesto", "Ejecutado", "Cumplimiento", "Comp. Per. Ant."), 4)),
            c("Fecha", cn$sacos_ant, cn$acum_sacos_ant, cn$margen_ant, cn$acum_margen_ant,
              cn$ppto_sacos, cn$sacos_act, cn$cumpl_sacos, "VarSS",
              cn$acum_ppto_ss, cn$acum_sacos_act, cn$cumpl_ss_acum, cn$varss_acum,
              cn$ppto_margen, cn$margen_act, cn$cumpl_margen, "VarMgn",
              cn$acum_ppto_ma, cn$acum_margen_act, cn$cumpl_mgn_acum, cn$varmgn_acum)
          )
        ) %>%
        fmt_number(columns   = c(2, 3, 6, 7, 10, 11), decimals = 0) %>%
        fmt_currency(columns = c(4, 5, 14, 15, 18, 19), currency = "COP", decimals = 0) %>%
        fmt_percent(columns  = c(8, 9, 12, 13, 16, 17, 20, 21), decimals = 1) %>%
        gt_minimal_style() %>%
        gt_color_columns(
          columns = c(cn$sacos_ant, cn$acum_sacos_ant, cn$margen_ant, cn$acum_margen_ant),
          color   = "#F0FDF4"
        ) %>%
        gt_color_columns(
          columns = c(
            cn$ppto_sacos, cn$sacos_act, cn$cumpl_sacos, "VarSS",
            cn$ppto_margen, cn$margen_act, cn$cumpl_margen, "VarMgn"
          ),
          color = "#F8FAFC"
        ) %>%
        gt_color_columns(
          columns = c(
            cn$acum_ppto_ss, cn$acum_sacos_act, cn$cumpl_ss_acum, cn$varss_acum,
            cn$acum_ppto_ma, cn$acum_margen_act, cn$cumpl_mgn_acum, cn$varmgn_acum
          ),
          color = "#EFF6FF"
        ) %>%
        tab_source_note(
          source_note = htmltools::tags$div(
            style = "margin-top:8px; font-size:12px; text-align:left;",
            tags$span(style = paste0(
              "display:inline-block;width:15px;height:15px;background-color:#F0FDF4;",
              "border:1px solid #ccc;margin-right:6px;vertical-align:middle;"
            )),
            tags$span(paste0("Ano Anterior (", periodo_ant, ")"),
                      style = "font-weight:bold;font-size:12px;"),
            tags$span(style = paste0(
              "display:inline-block;width:15px;height:15px;background-color:#F8FAFC;",
              "border:1px solid #ccc;margin-left:18px;margin-right:6px;vertical-align:middle;"
            )),
            tags$span(paste0("Mensual (", periodo, ")"),
                      style = "font-weight:bold;font-size:12px;"),
            tags$span(style = paste0(
              "display:inline-block;width:15px;height:15px;background-color:#EFF6FF;",
              "border:1px solid #ccc;margin-left:18px;margin-right:6px;vertical-align:middle;"
            )),
            tags$span(paste0("Acumulado (", periodo, ")"),
                      style = "font-weight:bold;font-size:12px;")
          )
        ) %>%
        tab_source_note(
          source_note = htmltools::tags$div(
            style = paste0(
              "margin-top:10px;padding:12px 14px;background:#F8FAFC;",
              "border-left:3px solid #94A3B8;border-radius:4px;font-size:11px;",
              "color:#374151;line-height:1.8;"
            ),
            tags$span(HTML(paste0(
              "<b>Cumplimiento:</b> ",
              "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
              "Ejecutado \u00f7 Presupuesto</code>"
            ))),
            tags$br(),
            tags$span(HTML(paste0(
              "<b>Comparacion con ano anterior:</b> ",
              "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
              "(Actual \u2212 Anterior) \u00f7 |Anterior|</code>"
            )))
          )
        ) %>%
        sub_missing(columns = everything(), rows = everything(), missing_text = "") %>%
        tab_style(
          style     = cell_text(weight = "bold"),
          locations = cells_body(rows = Fecha == "TOTAL")
        ) %>%
        gt_pct_style_semaforo(!!sym(cn$cumpl_sacos))    %>% gt_var_style(VarSS) %>%
        gt_pct_style_semaforo(!!sym(cn$cumpl_ss_acum))  %>% gt_var_style(!!sym(cn$varss_acum)) %>%
        gt_pct_style_semaforo(!!sym(cn$cumpl_margen))   %>% gt_var_style(VarMgn) %>%
        gt_pct_style_semaforo(!!sym(cn$cumpl_mgn_acum)) %>% gt_var_style(!!sym(cn$varmgn_acum))
      
      waiter_hide()
      return(gt1)
    })
    
    ## Titulo dinamico sub-card proyeccion ----
    output$titulo_proyeccion <- renderUI({
      paste0("Proyeccion de Cierre por Dimension \u2014 ", periodo_r())
    })
    
    ## Graficos de serie temporal ----
    output$GraficoPresupuestoSacos <- renderPlotly({
      construir_grafico_serie(
        datos_grafico_r(), input$vista_acumulada,
        "Sacos_Real", "Sacos_Ppto", "Sacos70",
        "Ejecucion vs Presupuesto \u2014 Sacos (Acumulado)",
        "Ejecucion vs Presupuesto \u2014 Sacos (Mensual)",
        "Sacos Acumulados", "Sacos Mensuales", "numero"
      )
    })
    output$GraficoPresupuestoMargen <- renderPlotly({
      construir_grafico_serie(
        datos_grafico_r(), input$vista_acumulada,
        "Margen_Real", "Margen_Ppto", "MargenFCC",
        "Ejecucion vs Presupuesto \u2014 Margen (Acumulado)",
        "Ejecucion vs Presupuesto \u2014 Margen (Mensual)",
        "Margen Acumulado", "Margen Mensual", "dinero"
      )
    })
    
    ## Tabla breakdown: recreada al cambiar dimension ----
    
    # Helper: colDef de dimension segun dim
    .col_dim_def <- function(dim) {
      es_uc <- dim == "unidadcomercial"
      if (es_uc) list(
        .dim_key = reactable::colDef(show = FALSE),
        RazSoc   = reactable::colDef(name = "Razon Social",      minWidth = 200, sticky = "left"),
        LinNeg   = reactable::colDef(name = "Linea de Negocio",  minWidth = 150, sticky = "left")
      ) else list(
        .dim_key = reactable::colDef(name = .dim_lbl(dim), minWidth = 180, sticky = "left")
      )
    }
    
    # Columnas fijas de metricas — identicas para todas las dimensiones
    .cols_metricas <- function() {
      list(
        Sacos_Mes = reactable::colDef(
          name = "Eje. Mes", minWidth = 90,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Ppto_Sacos_Mes = reactable::colDef(
          name = "Ppto Mes", minWidth = 90,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Cumpl_Sacos_Mes = reactable::colDef(
          name = "% Mes", minWidth = 70,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(background = .semaforo(v, "fondo_na"), fontWeight = "600")
        ),
        Sacos_YTD = reactable::colDef(
          name = "Eje. Acum", minWidth = 100,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Ppto_Sacos_YTD = reactable::colDef(
          name = "Ppto Acum", minWidth = 100,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Cumpl_Sacos_YTD = reactable::colDef(
          name = "% Acum", minWidth = 70,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(background = .semaforo(v, "fondo_na"), fontWeight = "700")
        ),
        Brecha_Sacos = reactable::colDef(
          name = "Brecha", minWidth = 90,
          format = reactable::colFormat(separators = TRUE, digits = 0),
          style  = function(v) list(
            color = if (!is.na(v) && v < 0) .CR else .CV, fontWeight = "600"
          )
        ),
        Proyeccion_Sacos = reactable::colDef(
          name = "Proy. Cierre", minWidth = 100,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Prob_Sacos = reactable::colDef(
          name = "Verosimilitud", minWidth = 90,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(color = .semaforo(v, "texto_na"), fontWeight = "700")
        ),
        Sem_Sacos = reactable::colDef(name = "", minWidth = 35),
        Meta_Mensual_Sacos = reactable::colDef(
          name = "Meta Mens.", minWidth = 90,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Margen_Mes = reactable::colDef(
          name = "Eje. Mes", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        ),
        Ppto_Margen_Mes = reactable::colDef(
          name = "Ppto Mes", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        ),
        Cumpl_Margen_Mes = reactable::colDef(
          name = "% Mes", minWidth = 70,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(background = .semaforo(v, "fondo_na"), fontWeight = "600")
        ),
        Margen_YTD = reactable::colDef(
          name = "Eje. Acum", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        ),
        Ppto_Margen_YTD = reactable::colDef(
          name = "Ppto Acum", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        ),
        Cumpl_Margen_YTD = reactable::colDef(
          name = "% Acum", minWidth = 70,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(background = .semaforo(v, "fondo_na"), fontWeight = "700")
        ),
        Brecha_Margen = reactable::colDef(
          name = "Brecha", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0),
          style  = function(v) list(
            color = if (!is.na(v) && v < 0) .CR else .CV, fontWeight = "600"
          )
        ),
        Proyeccion_Margen = reactable::colDef(
          name = "Proy. Cierre", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        ),
        Prob_Margen = reactable::colDef(
          name = "Verosimilitud", minWidth = 90,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(color = .semaforo(v, "texto_na"), fontWeight = "700")
        ),
        Sem_Margen = reactable::colDef(name = "", minWidth = 35),
        Meta_Mensual_Margen = reactable::colDef(
          name = "Meta Mens.", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        )
      )
    }
    
    # Contador para id unico al recrear el modulo (inicia en 1 para evitar race condition en init)
    .breakdown_ver <- reactiveVal(1L)
    observeEvent(input$dim_breakdown, {
      .breakdown_ver(.breakdown_ver() + 1L)
    }, ignoreInit = FALSE)
    
    .ns_local <- session$ns
    
    # UI dinamica: recrea TablaReactableUI con id unico por version
    output$tabla_breakdown_ui <- renderUI({
      ver <- .breakdown_ver()
      racafeModulos::TablaReactableUI(
        .ns_local(paste0("tabla_bd_", ver)),
        titulo    = NULL,
        subtitulo = NULL,
        sortable  = TRUE
      )
    })
    
    # Server: monta TablaReactable al cambiar version
    # ignoreInit = FALSE + req(!is.null(dim)) garantiza carga inicial sin race condition
    observeEvent(.breakdown_ver(), {
      ver <- .breakdown_ver()
      dim <- isolate(input$dim_breakdown)
      req(!is.null(dim))
      mid <- paste0("tabla_bd_", ver)
      
      racafeModulos::TablaReactable(
        id = mid,
        data = breakdown_tabla_r,
        id_col = ".dim_key",
        modo_seleccion = "fila",
        sortable = TRUE,
        searchable = TRUE,
        page_size = 20,
        compact = TRUE,
        mostrar_badge = FALSE,
        columnas = c(.col_dim_def(dim), .cols_metricas()),
        modal_titulo_fn = function(sel) {
          titulo_dim <- gsub("\\|\\|", " \u2014 ", sel$id, fixed = FALSE)
          paste0("Lotes facturados \u2014 ", titulo_dim, " \u00b7 ", isolate(periodo_r()))
        },
        modal_contenido_fn = function(sel) {
          dim_val <- sel$id
          dim_sel <- isolate(input$dim_breakdown)
          lotes   <- isolate(lotes_dim_r())
          es_uc   <- dim_sel == "unidadcomercial"
          
          lotes_fil <- if (dim_val == "TOTAL") {
            lotes
          } else if (es_uc) {
            uc_val <- gsub("\\|\\|", " \u2014 ", dim_val, fixed = FALSE)
            lotes %>% filter(UnidadComercial == uc_val)
          } else {
            col_dim <- switch(dim_sel,
                              linneg       = "CLLinNegNo",
                              segmento     = "Segmento",
                              asesor       = "Asesor",
                              departamento = "Departamento",
                              municipio    = "Municipio"
            )
            lotes %>% filter(.data[[col_dim]] == dim_val)
          }
          
          df_modal <- lotes_fil %>%
            group_by(FecFact, PerRazSoc, CLLinNegNo, CLLotCod, Asesor, Segmento) %>%
            summarise(
              Sacos70 = sum(Sacos70, na.rm = TRUE),
              Margen  = sum(Margen,  na.rm = TRUE),
              .groups = "drop"
            ) %>%
            arrange(desc(FecFact))
          
          if (nrow(df_modal) == 0) {
            return(p("Sin lotes registrados para esta dimension en el periodo.",
                     style = "color:#888; margin-top:12px;"))
          }
          
          reactable::reactable(
            data            = df_modal,
            sortable        = TRUE,
            searchable      = TRUE,
            defaultPageSize = 15,
            compact         = TRUE,
            bordered        = TRUE,
            highlight       = TRUE,
            language = reactable::reactableLang(
              searchPlaceholder = "Buscar...",
              noData            = "Sin resultados",
              pageInfo          = "{rowStart}\u2013{rowEnd} de {rows} registros",
              pagePrevious      = "Anterior",
              pageNext          = "Siguiente"
            ),
            theme = reactable::reactableTheme(
              headerStyle = list(fontWeight = "600", fontSize = "12px"),
              cellStyle   = list(fontSize = "12px", padding = "3px 6px")
            ),
            columns = list(
              FecFact    = reactable::colDef(
                name = "Fecha Factura", minWidth = 110,
                cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %Y")
              ),
              PerRazSoc  = reactable::colDef(name = "Razon Social",  minWidth = 180),
              CLLinNegNo = reactable::colDef(name = "Linea Negocio", minWidth = 130),
              CLLotCod   = reactable::colDef(name = "Lote",          minWidth = 80),
              Asesor     = reactable::colDef(name = "Asesor",        minWidth = 120),
              Segmento   = reactable::colDef(name = "Segmento",      minWidth = 100),
              Sacos70    = reactable::colDef(
                name = "Sacos (70Kg)", minWidth = 100,
                format = reactable::colFormat(separators = TRUE, digits = 1)
              ),
              Margen     = reactable::colDef(
                name = "Margen ($)", minWidth = 110,
                format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
              )
            )
          )
        },
        modal_size = "xl",
        modal_icon = "box-archive"
      )
    }, ignoreInit = FALSE)
    
    ## Footer metodologia ----
    output$footer_metodologia <- renderUI({
      htmltools::tags$div(
        style = paste0(
          "margin-top:4px;margin-bottom:16px;padding:12px 14px;background:#F8FAFC;",
          "border-left:3px solid #94A3B8;border-radius:4px;font-size:11px;",
          "color:#374151;line-height:1.8;"
        ),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>Cumplimiento:</b> ",
          "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
          "Ejecutado \u00f7 Presupuesto</code>"
        ))),
        htmltools::tags$br(),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>Brecha:</b> ",
          "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
          "Ejecutado \u2212 Presupuesto acumulado al mes</code>"
        ))),
        htmltools::tags$br(),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>Proyeccion de cierre:</b> ",
          "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
          "Ejecutado YTD \u00f7 Meses transcurridos \u00d7 12</code>"
        ))),
        htmltools::tags$br(),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>Verosimilitud:</b> ",
          "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
          "min(Proyeccion \u00f7 Presupuesto anual, 100%)</code>"
        ))),
        htmltools::tags$br(),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>Meta mensual:</b> ",
          "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
          "Presupuesto anual \u00f7 12</code>"
        ))),
        htmltools::tags$br(),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>TOTAL:</b> suma de todas las dimensiones del periodo. ",
          "Los indicadores se calculan sobre la fila y no como el promedio de la columna."
        )))
      )
    })
    
    ## Grafico cuadrantes: proyeccion de cierre vs cumplimiento acumulado ----
    output$grafico_proyeccion <- renderPlotly({
      req(breakdown_r())
      df      <- breakdown_r()
      lbl     <- .dim_lbl(input$dim_breakdown)
      medida  <- input$medida_proyeccion
      periodo <- periodo_r()
      s       <- .SEMAFORO
      
      if (medida == "sacos") {
        col_cumpl  <- "Cumpl_Sacos_YTD"; col_prob   <- "Prob_Sacos"
        col_ytd    <- "Sacos_YTD";       col_brecha <- "Brecha_Sacos"
        lbl_medida <- "Sacos";           fmt_val    <- function(x) scales::comma(x, accuracy = 1)
      } else {
        col_cumpl  <- "Cumpl_Margen_YTD"; col_prob   <- "Prob_Margen"
        col_ytd    <- "Margen_YTD";       col_brecha <- "Brecha_Margen"
        lbl_medida <- "Margen"
        fmt_val    <- function(x) scales::dollar(x, prefix = "$", accuracy = 1)
      }
      
      x_max <- max(df[[col_cumpl]] * 1.15, 1.1, na.rm = TRUE)
      y_max <- max(df[[col_prob]]  * 1.15, 1.1, na.rm = TRUE)
      
      df <- df %>%
        mutate(
          dim     = if ("dim" %in% names(df)) dim else paste0(RazSoc, " \u2014 ", LinNeg),
          .cx     = !!sym(col_cumpl), .cy = !!sym(col_prob),
          .ytd    = !!sym(col_ytd),   .brecha = !!sym(col_brecha),
          color_punto = .color_punto_cumpl(.cx, .cy),
          tooltip = paste0(
            "<b>", dim, "</b><br>",
            "Cumpl. Acum. ", periodo, ": ", scales::percent(.cx, accuracy = 0.1), "<br>",
            "Proyeccion cierre: ",          scales::percent(.cy, accuracy = 0.1), "<br>",
            lbl_medida, " Acum.: ", fmt_val(.ytd), "<br>",
            "Brecha: ", fmt_val(.brecha)
          )
        )
      
      plot_ly(
        df, x = ~.cx, y = ~.cy,
        text       = ~gsub("\n", "<br>", stringr::str_wrap(dim, width = 18)),
        customdata = ~tooltip,
        type = "scatter", mode = "markers+text",
        marker = list(
          color = ~color_punto, size = 14, opacity = 0.90,
          line  = list(color = "white", width = 1.5)
        ),
        textfont      = list(size = 8, color = "#374151"),
        textposition  = "top center",
        hovertemplate = "%{customdata}<extra></extra>"
      ) %>%
        add_segments(
          x = s$umbral_alto, xend = s$umbral_alto, y = 0, yend = y_max,
          inherit = FALSE, line = list(color = "#94A3B8", dash = "dot", width = 1),
          showlegend = FALSE
        ) %>%
        add_segments(
          x = 0, xend = x_max, y = s$umbral_exceso, yend = s$umbral_exceso,
          inherit = FALSE, line = list(color = "#94A3B8", dash = "dot", width = 1),
          showlegend = FALSE
        ) %>%
        layout(
          title = paste0(
            "Proyeccion de Cierre \u2014 ", periodo, " \u00b7 ", lbl, " (", lbl_medida, ")"
          ),
          xaxis = list(
            title = paste0("Cumpl. Acum. ", periodo, " (%)"),
            tickformat = ".0%", range = c(0, x_max)
          ),
          yaxis = list(
            title = "Proyeccion de Cierre (%)",
            tickformat = ".0%", range = c(0, y_max)
          ),
          shapes = list(
            list(type = "rect", x0 = 0, y0 = 0,
                 x1 = s$umbral_alto, y1 = s$umbral_exceso,
                 fillcolor = "#FEE2E2", opacity = 0.50, line = list(width = 0)),
            list(type = "rect", x0 = s$umbral_alto, y0 = s$umbral_exceso,
                 x1 = x_max, y1 = y_max,
                 fillcolor = "#DCFCE7", opacity = 0.50, line = list(width = 0)),
            list(type = "rect", x0 = 0, y0 = s$umbral_exceso,
                 x1 = s$umbral_alto, y1 = y_max,
                 fillcolor = "#FEF9C3", opacity = 0.55, line = list(width = 0)),
            list(type = "rect", x0 = s$umbral_alto, y0 = 0,
                 x1 = x_max, y1 = s$umbral_exceso,
                 fillcolor = "#FEF9C3", opacity = 0.55, line = list(width = 0))
          )
        ) %>%
        config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    
    ## Diagrama bullet: ritmo mensual actual vs requerido ----
    output$grafico_velocidad <- renderPlotly({
      req(breakdown_r())
      df      <- breakdown_r()
      lbl     <- .dim_lbl(input$dim_breakdown)
      medida  <- input$medida_velocidad
      periodo <- periodo_r()
      
      if (medida == "sacos") {
        col_actual <- "Ritmo_Sacos_Actual"; col_req    <- "Ritmo_Sacos_Req"
        lbl_medida <- "Sacos";              fmt_val    <- function(x) scales::comma(x, accuracy = 1)
        lbl_unidad <- "sacos/mes"
      } else {
        col_actual <- "Ritmo_Margen_Actual"; col_req   <- "Ritmo_Margen_Req"
        lbl_medida <- "Margen"
        fmt_val    <- function(x) scales::dollar(x, prefix = "$", accuracy = 1)
        lbl_unidad <- "$/mes"
      }
      
      df <- df %>%
        arrange(!!sym(col_actual)) %>%
        mutate(
          dim         = if ("dim" %in% names(df)) dim else paste0(RazSoc, " \u2014 ", LinNeg),
          dim         = factor(dim, levels = dim),
          .act        = !!sym(col_actual), .req = !!sym(col_req),
          .b_rojo     = .req * 0.70,
          .b_amarillo = .req * 0.90,
          .b_verde    = .req * 1.20,
          .ratio      = SiError_0(.act / pmax(.req, 1e-9)),
          tooltip     = paste0(
            "<b>", dim, "</b><br>",
            "Ritmo actual: ",    fmt_val(.act), " ", lbl_unidad, "<br>",
            "Ritmo requerido: ", fmt_val(.req), " ", lbl_unidad, "<br>",
            "Ratio: ", scales::percent(.ratio, accuracy = 0.1)
          )
        )
      
      bar_w  <- 0.25
      band_w <- 0.55
      
      p <- plot_ly() %>%
        add_bars(
          data = df, x = ~.b_rojo, y = ~dim, base = 0, orientation = "h",
          width = band_w, name = "Riesgo (< 70%)",
          marker = list(color = "#FEE2E2", line = list(color = "#FECACA", width = 0.5)),
          hoverinfo = "skip", showlegend = TRUE
        ) %>%
        add_bars(
          data = df, x = ~(.b_amarillo - .b_rojo), y = ~dim, base = ~.b_rojo,
          orientation = "h", width = band_w, name = "Atencion (70-90%)",
          marker = list(color = "#FEF9C3", line = list(color = "#FDE68A", width = 0.5)),
          hoverinfo = "skip", showlegend = TRUE
        ) %>%
        add_bars(
          data = df, x = ~(.b_verde - .b_amarillo), y = ~dim, base = ~.b_amarillo,
          orientation = "h", width = band_w, name = "En ruta (\u2265 90%)",
          marker = list(color = "#DCFCE7", line = list(color = "#BBF7D0", width = 0.5)),
          hoverinfo = "skip", showlegend = TRUE
        ) %>%
        add_bars(
          data = df, x = ~.act, y = ~dim, base = 0, orientation = "h",
          width = bar_w, name = paste0("Ritmo actual (", lbl_unidad, ")"),
          marker     = list(color = "#334155", opacity = 0.90, line = list(width = 0)),
          customdata = ~tooltip,
          hovertemplate = "%{customdata}<extra></extra>", showlegend = TRUE
        ) %>%
        add_markers(
          data = df, x = ~.req, y = ~dim, name = "Ritmo requerido",
          marker = list(
            color  = "#0F172A", symbol = "line-ns-open", size = 16,
            line   = list(color = "#0F172A", width = 4)
          ),
          hoverinfo = "skip", showlegend = TRUE
        )
      
      x_max <- max(df$.b_verde * 1.05, df$.act * 1.05, na.rm = TRUE)
      
      p %>%
        layout(
          title   = paste0(
            "Velocidad de Ejecucion vs Requerida \u2014 ", lbl, " (", lbl_medida, ")"
          ),
          barmode = "overlay",
          xaxis   = list(
            title = paste0(lbl_medida, " por mes"), tickformat = ",", range = c(0, x_max)
          ),
          yaxis  = list(title = ""),
          legend = list(
            orientation = "v", x = 1.02, y = 1,
            xanchor = "left", yanchor = "top",
            font = list(size = 10), traceorder = "normal"
          ),
          hovermode = "y unified"
        ) %>%
        config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    
  })
}

# App de prueba ----
ui <- bs4DashPage(
  title = "Presupuesto", header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(), controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body   = bs4DashBody(useShinyjs(), PresupuestoUI("presupuesto"))
)
server <- function(input, output, session) {
  Presupuesto(
    "presupuesto",
    dat = reactive({ BaseDatos %>% filter(Asesor == "JGCANON", Segmento == "GRANDES")})
  )
}
shinyApp(ui, server)
# Helpers Detalle Clientes ----

# Carga y preprocesa CRMNALSEGR: limpieza, cast de fechas y resumen Antes/Ahora por cliente.
# NOTA: usado únicamente en módulos externos; los módulos de UC no lo requieren porque
# dat() llega pre-filtrado desde ResumenTotal con el universo correcto.
dc_cargar_segr <- function() {
  CargarDatos("CRMNALSEGR") %>%
    mutate(
      FecProceso = as.Date(FecProceso),
      across(where(is.numeric),   ~ ifelse(is.na(.), 0, .)),
      across(where(is.character), ~ ifelse(is.na(.) | . == "N/A", "", .))
    ) %>%
    filter(FecProceso >= PrimerDia(Sys.Date()) - months(1)) %>%
    group_by(LinNegCod, CliNitPpal) %>%
    arrange(FecProceso) %>%
    summarise(Antes = first(SegmentoRacafe), Ahora = last(SegmentoRacafe), .groups = "drop")
}

# Calcula ejecución mensual y YTD por cliente desde datos de facturación filtrados
dc_ejecucion <- function(dat_df) {
  dat_df %>%
    group_by(LinNegCod, CliNitPpal) %>%
    summarise(
      SacosMes  = sum(
        ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), SacFact70, 0), na.rm = TRUE
      ),
      MargenMes = sum(
        ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Margen, 0),    na.rm = TRUE
      ),
      SacosYTD  = sum(
        ifelse(year(FecFact) == year(Sys.Date()), SacFact70, 0), na.rm = TRUE
      ),
      MargenYTD = sum(
        ifelse(year(FecFact) == year(Sys.Date()), Margen, 0),    na.rm = TRUE
      ),
      .groups = "drop"
    )
}

# Calcula primera y última fecha de facturación por cliente desde el historial completo.
# CORRECCIÓN: guardia defensiva sobre el global `data`; retorna tibble vacío si el objeto
# no existe o carece del esquema esperado (p. ej. tras refactorización de cargar_datos_base).
# DEUDA TÉCNICA: dat_hist debe llegar como reactivo desde el módulo padre (FACT completo).
# Próximo paso: añadir parámetro dat_hist a los módulos y eliminiar la dependencia global.
dc_fechas_fact <- function(dat_hist = NULL) {
  df <- if (!is.null(dat_hist)) {
    dat_hist
  } else {
    tryCatch({
      d <- get("data", envir = .GlobalEnv)
      if (!is.data.frame(d) || !("CliNitPpal" %in% names(d))) stop("esquema inválido")
      d
    }, error = function(e) NULL)
  }
  if (is.null(df)) {
    return(tibble::tibble(
      LinNegCod  = character(),
      CliNitPpal = character(),
      PrimFact   = as.Date(NA_character_),
      UltFact    = as.Date(NA_character_)
    ))
  }
  df %>%
    filter(!is.na(FecFact)) %>%
    group_by(LinNegCod, CliNitPpal) %>%
    summarise(
      PrimFact = min(FecFact, na.rm = TRUE),
      UltFact  = max(FecFact, na.rm = TRUE),
      .groups = "drop"
    )
}

# Normaliza presupuesto a base mensual y calcula acumulado proporcional al mes actual
dc_ppto_normalizar <- function(df, mes) {
  df %>%
    mutate(
      ConPpto       = ifelse(PptoSacos > 0, "CON PRESUPUESTO", "SIN PRESUPUESTO"),
      PptoSacos     = PptoSacos / 12,
      PptoMargen    = PptoMargen / 12,
      PptoSacosYTD  = PptoSacos * mes,
      PptoMargenYTD = PptoMargen * mes
    )
}

# Agrega métricas de cumplimiento mensual, acumulado, proyección y etiquetas al df
dc_metricas_cumpl <- function(df, mes_falt, lbl_acum) {
  df %>%
    mutate(
      ConPpto        = ifelse(is.na(ConPpto), "SIN PRESUPUESTO", ConPpto),
      SacosCumpPpto  = pmax(
        ((PptoSacosYTD - SacosYTD) + ((PptoSacos * 12) - PptoSacosYTD)) / mes_falt, 0
      ),
      MargenCumpPpto = pmax(
        ((PptoMargenYTD - MargenYTD) + ((PptoMargen * 12) - PptoMargenYTD)) / mes_falt, 0
      ),
      SacosMes       = ifelse(is.na(SacosMes),  0, SacosMes),
      MargenMes      = ifelse(is.na(MargenMes),  0, MargenMes),
      SacosYTD       = ifelse(is.na(SacosYTD),   0, SacosYTD),
      MargenYTD      = ifelse(is.na(MargenYTD),  0, MargenYTD),
      CumpSacosMes   = SacosMes  / PptoSacos,
      CumpMargenMes  = MargenMes / PptoMargen,
      CumpSacosYTD   = SacosYTD  / PptoSacosYTD,
      CumpMargenYTD  = MargenYTD / PptoMargenYTD,
      LblAcum        = lbl_acum,
      Oportunidad    = "Crear"
    )
}

# Construye fila de totales agregados para bind_rows en data_tabla
dc_fila_total <- function(df) {
  s <- function(col) sum(df[[col]], na.rm = TRUE)
  d <- function(num, den) if (den > 0) num / den else NA_real_
  tibble::tibble(
    Oportunidad    = "",
    PerRazSoc      = "TOTAL",
    CLLinNegNo     = "",
    ConPpto        = "",
    Segmento       = "",
    UltFact        = as.Date(NA),
    PptoSacos      = s("PptoSacos"),
    SacosMes       = s("SacosMes"),
    CumpSacosMes   = d(s("SacosMes"),   s("PptoSacos")),
    PptoMargen     = s("PptoMargen"),
    MargenMes      = s("MargenMes"),
    CumpMargenMes  = d(s("MargenMes"),  s("PptoMargen")),
    PptoSacosYTD   = s("PptoSacosYTD"),
    SacosYTD       = s("SacosYTD"),
    CumpSacosYTD   = d(s("SacosYTD"),   s("PptoSacosYTD")),
    PptoMargenYTD  = s("PptoMargenYTD"),
    MargenYTD      = s("MargenYTD"),
    CumpMargenYTD  = d(s("MargenYTD"),  s("PptoMargenYTD")),
    SacosCumpPpto  = s("SacosCumpPpto"),
    MargenCumpPpto = s("MargenCumpPpto"),
    LblAcum        = ""
  )
}

# Gráfico de barras con resaltado del filtro activo; niveles permite ordenar el eje X
dc_grafico <- function(df, columna, source_name, filtro_activo, niveles = NULL) {
  if (nrow(df) == 0) {
    return(
      plotly_empty(type = "bar") %>%
        layout(title = list(text = "Sin datos disponibles", font = list(size = 14)))
    )
  }
  if (!is.null(niveles)) {
    df <- df %>%
      mutate(!!sym(columna) := factor(!!sym(columna), levels = niveles, ordered = TRUE))
  }
  resumen <- df %>%
    count(!!sym(columna), name = "Total") %>%
    mutate(
      Porcentaje = round(100 * Total / sum(Total), 1),
      Hover      = paste0("<b>Total: </b>", Total, "<br><b>%: </b>", Porcentaje, "%"),
      Color      = if (is.null(filtro_activo)) {
        "#4A5565"
      } else {
        ifelse(!!sym(columna) == filtro_activo, "#C11007", "#4A5565")
      }
    )
  ymax <- max(resumen$Total, na.rm = TRUE) * 1.10
  plot_ly(
    data = resumen, x = ~get(columna), y = ~Total, type = "bar",
    text = ~Total, textposition = "outside", textangle = 0,
    textfont = list(size = 12), hovertext = ~Hover, hoverinfo = "text",
    marker = list(color = ~Color), source = source_name
  ) %>%
    layout(
      xaxis = list(title = "", tickangle = 0, tickfont = list(size = 12)),
      yaxis = list(title = "Cantidad de Clientes", range = c(0, ymax))
    ) %>%
    event_register("plotly_click") %>%
    config(displayModeBar = FALSE)
}

# Formatea número entero con separador de miles "." y decimal "," (convención Colombia).
# Necesario porque las funciones cell de reactable se ejecutan fuera del contexto donde
# options(OutDec = ",") está activo, lo que provoca el warning "big.mark and decimal.mark
# are both '.'". Especificar decimal.mark explícitamente elimina el conflicto.
fmt_num <- function(v, prefijo = "") {
  if (is.na(v) || is.infinite(v)) return("\u2014")
  paste0(prefijo, format(round(v), big.mark = ".", decimal.mark = ",", scientific = FALSE))
}

# Genera lista de colDefs compartidas entre todos los módulos de detalle de clientes.
# data_tabla se recibe como reactivo para que los headers dinámicos de YTD puedan
# leer LblAcum en tiempo de renderizado sin necesidad de recalcular colDefs.
dc_coldefs_comunes <- function(data_tabla) {
  list(
    # Primeras 3 columnas: identificación (col_header_n = 3L)
    # CORRECCIÓN Oportunidad: sin html=TRUE — TablaReactable inyecta el HTML del botón
    # internamente vía cols_activos; nuestro colDef sólo define clase y ancho.
    Oportunidad = reactable::colDef(name     = "",
                                    minWidth = 70,
                                    html     = TRUE,
                                    sortable = FALSE,
                                    cell     = function(v) {
                                      if (is.na(v) || v == "") return("")
                                      as.character(tags$span(style = paste("display:inline-flex; align-items:center; justify-content:center;",
                                                                           "width:28px; height:28px; border-radius:6px;",
                                                                           "background:#C11007; color:white; font-size:13px; cursor:pointer;"
                                                                           ),
                                                             icon("hand-holding-dollar")
                                      ))
                                      }
                                    ),
    # CORRECCIÓN PerRazSoc: html=TRUE necesario para que crear_link_cliente renderice
    # como enlace HTML y no como texto plano escapado.
    PerRazSoc = reactable::colDef(
      name     = "Razón Social",
      html     = TRUE,
      minWidth = 200,
      class    = "rt-col-header"
    ),
    CLLinNegNo = reactable::colDef(
      name     = "Línea de Negocio",
      minWidth = 120,
      class    = "rt-col-header"
    ),
    
    # Columnas categóricas y fecha
    ConPpto  = reactable::colDef(name = "Presupuestado", minWidth = 130),
    Segmento = reactable::colDef(name = "Segmento",      minWidth = 100),
    UltFact  = reactable::colDef(
      name     = "Últ. Factura",
      minWidth = 110,
      cell     = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d/%m/%Y")
    ),
    
    # Bloque sacos / margen del mes
    PptoSacos = reactable::colDef(
      name = "Ppto Sacos",  minWidth = 110,
      cell = function(v) fmt_num(v)
    ),
    SacosMes = reactable::colDef(
      name = "Sacos Mes",   minWidth = 110,
      cell = function(v) fmt_num(v)
    ),
    CumpSacosMes = reactable::colDef(
      name  = "% Cumpl Mes",  minWidth = 110,
      cell  = function(v) {
        if (is.na(v) || is.infinite(v)) "\u2014" else paste0(round(v * 100, 1), "%")
      },
      style = function(v) {
        if (is.na(v) || is.infinite(v)) return(NULL)
        list(
          background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
          fontWeight = "600"
        )
      }
    ),
    PptoMargen = reactable::colDef(
      name = "Ppto Margen",  minWidth = 130,
      cell = function(v) fmt_num(v, "$")
    ),
    MargenMes = reactable::colDef(
      name = "Margen Mes",   minWidth = 130,
      cell = function(v) fmt_num(v, "$")
    ),
    CumpMargenMes = reactable::colDef(
      name  = "% Cumpl Margen Mes",  minWidth = 150,
      cell  = function(v) {
        if (is.na(v) || is.infinite(v)) "\u2014" else paste0(round(v * 100, 1), "%")
      },
      style = function(v) {
        if (is.na(v) || is.infinite(v)) return(NULL)
        list(
          background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
          fontWeight = "600"
        )
      }
    ),
    
    # Bloque acumulado YTD — header dinámico lee LblAcum desde data_tabla()
    PptoSacosYTD = reactable::colDef(
      minWidth = 130,
      header   = function(value, name) {
        lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
        paste0("Ppto Sacos Acum. ", lbl)
      },
      cell = function(v) fmt_num(v)
    ),
    SacosYTD = reactable::colDef(
      minWidth = 130,
      header   = function(value, name) {
        lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
        paste0("Sacos Acum. ", lbl)
      },
      cell = function(v) fmt_num(v)
    ),
    CumpSacosYTD = reactable::colDef(
      minWidth = 150,
      header   = function(value, name) {
        lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
        paste0("% Cumpl Sacos Acum. ", lbl)
      },
      cell  = function(v) {
        if (is.na(v) || is.infinite(v)) "\u2014" else paste0(round(v * 100, 1), "%")
      },
      style = function(v) {
        if (is.na(v) || is.infinite(v)) return(NULL)
        list(
          background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
          fontWeight = "600"
        )
      }
    ),
    PptoMargenYTD = reactable::colDef(
      minWidth = 150,
      header   = function(value, name) {
        lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
        paste0("Ppto Margen Acum. ", lbl)
      },
      cell = function(v) fmt_num(v, "$")
    ),
    MargenYTD = reactable::colDef(
      minWidth = 140,
      header   = function(value, name) {
        lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
        paste0("Margen Acum. ", lbl)
      },
      cell = function(v) fmt_num(v, "$")
    ),
    CumpMargenYTD = reactable::colDef(
      minWidth = 160,
      header   = function(value, name) {
        lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
        paste0("% Cumpl Margen Acum. ", lbl)
      },
      cell  = function(v) {
        if (is.na(v) || is.infinite(v)) "\u2014" else paste0(round(v * 100, 1), "%")
      },
      style = function(v) {
        if (is.na(v) || is.infinite(v)) return(NULL)
        list(
          background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
          fontWeight = "600"
        )
      }
    ),
    
    # Bloque proyección anual
    SacosCumpPpto  = reactable::colDef(
      name = "Sacos proy. cumplir Ppto",  minWidth = 180,
      cell = function(v) fmt_num(v)
    ),
    MargenCumpPpto = reactable::colDef(
      name = "Margen proy. cumplir Ppto", minWidth = 190,
      cell = function(v) fmt_num(v, "$")
    ),
    
    # Columna de etiqueta acumulado: oculta, usada solo por los headers dinámicos
    LblAcum = reactable::colDef(show = FALSE)
  )
}

# UI reutilizable: botón limpiar, tres gráficos plotly, tabla reactable y botón descarga
dc_ui_base <- function(ns, graficos, titulo_tabla, mostrar_nota = FALSE) {
  tagList(
    fluidRow(
      column(12,
             BotonGuardar(
               id              = ns("limpiar_filtros"),
               label           = "Limpiar Filtros",
               icon            = "eraser",
               color           = "warning",
               size            = "sm",
               align           = "right",
               style_container = "display:flex; gap:15px; margin:0 0 10px 0;"
             )
      )
    ),
    do.call(fluidRow, lapply(graficos, function(g) {
      column(4,
             h5(g$titulo),
             p(g$descripcion, style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns(g$output_id), height = "300px")
      )
    })),
    fluidRow(
      column(12,
             div(style = "margin-top: 20px;",
                 TablaReactableUI(
                   ns("TablaClientes"),
                   titulo       = titulo_tabla,
                   footer       = "Clic en el botón Crear para registrar una oportunidad.",
                   footer_tipo  = "info",
                   mostrar_nota = mostrar_nota
                 )
             )
      )
    ),
    fluidRow(
      html(paste0(
        '<div style="text-align: right; width: 100%;">',
        BotonDescarga("Descargar", size = "md", ns = ns),
        '</div>'
      ))
    )
  )
}

# Retorna lista para do.call(downloadHandler, ...) con descarga Excel/CSV de datos filtrados
dc_descarga <- function(data_filtrada_fn, prefijo) {
  list(
    filename = function() paste0(prefijo, "_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      df <- data_filtrada_fn()
      if (nrow(df) == 0) {
        showNotification("No hay datos para descargar", type = "warning")
        return()
      }
      tryCatch({
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(df, file)
        } else {
          write.csv(df, file, row.names = FALSE)
        }
        showNotification("Archivo descargado exitosamente", type = "message")
      }, error = function(e) {
        showNotification(paste("Error al descargar:", e$message), type = "error")
      })
    }
  )
}

# Pre-registra source IDs en session$userData$.plotlyShinyEventIDs para evitar warnings.
# sources: named list(campo = src_id_namespaceado). Observers de click encapsulados en local()
# para captura correcta de variables en el loop.
dc_registrar_clicks <- function(session, sources, filtros) {
  ids_actuales <- session$userData$.plotlyShinyEventIDs
  session$userData$.plotlyShinyEventIDs <- unique(c(ids_actuales, unname(unlist(sources))))
  for (campo in names(sources)) {
    local({
      cam <- campo
      src <- sources[[cam]]
      observeEvent(event_data("plotly_click", source = src), {
        click <- event_data("plotly_click", source = src)
        if (!is.null(click))
          filtros[[cam]] <- if (
            !is.null(filtros[[cam]]) && filtros[[cam]] == click$x
          ) NULL else click$x
      }, ignoreNULL = TRUE)
    })
  }
}

# Config base compartida para llamadas a TablaReactable en todos los módulos de detalle.
# CORRECCIÓN: col_specs recibe el named list de colDef (antes se pasaba erróneamente a
# `columnas`, lo que provocaba que TablaReactable construyera nms_visibles como lista de
# listas y fallara con "invalid subscript type 'list'" al indexar col_specs[[nm_i]]).
# columnas = NULL indica a TablaReactable que use todas las columnas de data en orden natural.
dc_tabla_reactable_base <- function(data_tabla, data_filtrada, ns,
                                    col_specs = NULL, col_header_n = 3L) {
  racafeModulos::TablaReactable(
    id             = "TablaClientes",
    data           = data_tabla,
    columnas       = NULL,
    col_specs      = col_specs,
    modo_seleccion = "celda",
    id_col         = NULL,
    cols_activos   = "Oportunidad",
    col_header_n   = col_header_n,
    sortable       = TRUE,
    searchable     = TRUE,
    page_size      = 15L,
    compact        = TRUE,
    mostrar_badge  = FALSE,
    mostrar_nota   = FALSE,
    filas_bloqueadas   = "TOTAL",
    modal_icon         = "file-alt",
    modal_size         = "xl",
    modal_titulo_fn    = function(sel) {
      razsoc <- as.character(sel$fila$PerRazSoc[[1]])
      paste0("Nueva Oportunidad \u2014 ", razsoc)
    },
    modal_pre_fn       = NULL,
    modal_contenido_fn = function(sel) {
      FormularioOportunidadUI(ns("mod_formulario"))
    }
  )
}

# Propaga mensaje validate a todos los outputs cuando el universo de entrada está vacío
dc_sin_datos_msg <- function() {
  validate(need(FALSE, paste(
    "No hay datos disponibles para el universo seleccionado.",
    "Ajusta los filtros activos e intenta nuevamente."
  )))
}

# Clientes ----
# Módulo: clientes clasificados como CLIENTE en el snapshot del corte del mes.
# dat() llega pre-filtrado desde ResumenTotal via df_activas; no requiere re-filtro interno.
# Indicadores: compra mes vigente (gráfico), presupuestado (gráfico), pérdida (gráfico).
# BucketCumpl (cumplimiento YTD) disponible como columna de tabla para drill-down individual.

DetalleClienteUI <- function(id) {
  ns <- NS(id)
  dc_ui_base(
    ns,
    titulo_tabla = "Detalle de Clientes Activos",
    graficos = list(
      list(
        output_id   = "FactMesVigente",
        titulo      = "Compra en el Mes",
        descripcion = paste(
          "Clientes activos con y sin facturación registrada en el mes en curso.",
          "Clic para filtrar la tabla."
        )
      ),
      list(
        output_id   = "Presupuestado",
        titulo      = "Presupuestado",
        descripcion = paste(
          "Clientes con presupuesto de sacos mayor a cero para el año vigente.",
          "Clic para filtrar la tabla."
        )
      ),
      list(
        output_id   = "Tiempo",
        titulo      = "Riesgo de Pérdida",
        descripcion = paste(
          "Clientes agrupados por meses transcurridos desde su última facturación.",
          "Clic para filtrar la tabla."
        )
      )
    )
  )
}
DetalleCliente <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Source IDs namespaceados para evitar colisiones entre instancias del módulo
    src_factmes <- ns("factmes")
    src_conppto <- ns("conppto")
    src_meses   <- ns("meses")
    
    # Estado reactivo de filtros por clic en gráficos
    filtros <- reactiveValues(factmes = NULL, conppto = NULL, meses = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$factmes <- NULL; filtros$conppto <- NULL; filtros$meses <- NULL
    })
    
    # Datos base: flag compra mes vigente, bucket cumplimiento YTD y métricas de presupuesto
    data_cliente <- reactive({
      waiter_show(html = preloader_calculando$html, color = preloader_calculando$color)
      on.exit(waiter_hide())
      req(dat())
      if (nrow(dat()) == 0) dc_sin_datos_msg()
      tryCatch({
        eje      <- dc_ejecucion(dat())
        fec      <- dc_fechas_fact()
        mes      <- month(Sys.Date())
        mes_falt <- pmax(12 - mes, 1)
        # Consolidación con presupuesto normalizado, joins, métricas e indicadores de gestión
        dat() %>%
          dc_ppto_normalizar(mes) %>%
          select(
            LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
            ConPpto, PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD
          ) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            # Flag de compra en el mes: SacosMes llega 0 por left_join si no facturó
            FactMesVigente = ifelse(
              !is.na(SacosMes) & SacosMes > 0, "CON COMPRA", "SIN COMPRA"
            ),
            Meses = paste(
              pmax(0, lubridate::interval(UltFact, Sys.Date()) %/% months(1)), "MESES"
            )
          ) %>%
          dc_metricas_cumpl(mes_falt, format(Sys.Date(), "%b %Y")) %>%
          mutate(
            # Bucket de cumplimiento YTD: semáforo de gestión por cliente
            BucketCumpl = dplyr::case_when(
              is.na(CumpSacosYTD) | CumpSacosYTD == 0 ~ "Sin ejecución",
              CumpSacosYTD >= 1.00                     ~ ">=100%",
              CumpSacosYTD >= 0.80                     ~ "80-99%",
              CumpSacosYTD >= 0.50                     ~ "50-79%",
              TRUE                                     ~ "<50%"
            )
          )
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Datos filtrados según selección activa en gráficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$factmes)) df <- df %>% filter(FactMesVigente == filtros$factmes)
      if (!is.null(filtros$conppto)) df <- df %>% filter(ConPpto == filtros$conppto)
      if (!is.null(filtros$meses))   df <- df %>% filter(Meses == filtros$meses)
      df
    })
    
    # Datos para tabla con fila de totales; columnas indicador inicializadas en blanco
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      fila_total <- dc_fila_total(df) %>%
        mutate(FactMesVigente = "", BucketCumpl = "")
      df %>%
        crear_link_cliente(col_razsoc = "PerRazSoc", col_linneg = "CLLinNegNo") %>%
        select(
          Oportunidad, PerRazSoc, CLLinNegNo,
          FactMesVigente, BucketCumpl, ConPpto, Segmento, UltFact,
          PptoSacos, SacosMes, CumpSacosMes,
          PptoMargen, MargenMes, CumpMargenMes,
          PptoSacosYTD, SacosYTD, CumpSacosYTD,
          PptoMargenYTD, MargenYTD, CumpMargenYTD,
          SacosCumpPpto, MargenCumpPpto, LblAcum
        ) %>%
        bind_rows(fila_total)
    })
    
    # Patrón eager: FormularioOportunidad registrado en startup antes de TablaReactable
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE")
    )
    
    # Renderizado de gráficos con source IDs namespaceados
    output$FactMesVigente <- renderPlotly(
      dc_grafico(data_filtrada(), "FactMesVigente", src_factmes, filtros$factmes)
    )
    output$Presupuestado <- renderPlotly(
      dc_grafico(data_filtrada(), "ConPpto", src_conppto, filtros$conppto)
    )
    output$Tiempo <- renderPlotly(
      dc_grafico(data_filtrada(), "Meses", src_meses, filtros$meses)
    )
    
    # Registro diferido de observers de clic para eliminar warnings de plotly
    dc_registrar_clicks(session,
                        sources = list(factmes = src_factmes, conppto = src_conppto, meses = src_meses),
                        filtros = filtros
    )
    
    # Tabla reactable con colDefs específicos para indicadores de clientes activos.
    # CORRECCIÓN: col_specs recibe el named list de colDef (antes: argumento columnas).
    dc_tabla_reactable_base(data_tabla, data_filtrada, ns,
                            col_specs = modifyList(dc_coldefs_comunes(data_tabla), list(
                              FactMesVigente = reactable::colDef(
                                name  = "Compra Mes", minWidth = 120,
                                style = function(v) {
                                  list(
                                    background = switch(v, "CON COMPRA" = "#EDFBF2", "SIN COMPRA" = "#FEF2F2", "white"),
                                    color      = switch(v, "CON COMPRA" = "#1E8449", "SIN COMPRA" = "#943126", "#333"),
                                    fontWeight = "600"
                                  )
                                }
                              ),
                              BucketCumpl = reactable::colDef(
                                name  = "Cumpl. YTD", minWidth = 120,
                                style = function(v) {
                                  list(
                                    background = switch(v,
                                                        ">=100%"         = "#D5F5E3",
                                                        "80-99%"         = "#FCF3CF",
                                                        "50-79%"         = "#FDEBD0",
                                                        "<50%"           = "#FADBD8",
                                                        "Sin ejecución"  = "#F8F9F9",
                                                        "white"
                                    ),
                                    color = switch(v,
                                                   ">=100%"         = "#1A5226",
                                                   "80-99%"         = "#7D6608",
                                                   "50-79%"         = "#784212",
                                                   "<50%"           = "#922B21",
                                                   "Sin ejecución"  = "#707B7C",
                                                   "#333"
                                    ),
                                    fontWeight = "600"
                                  )
                                }
                              )
                            ))
    )
    
    # Descarga Excel de datos filtrados sin fila de totales
    output$Descargar <- do.call(downloadHandler, dc_descarga(data_filtrada, "clientes_activos"))
  })
}

# Clientes a Recuperar ----
# Módulo: clientes clasificados como CLIENTE A RECUPERAR en el snapshot del corte del mes.
# dat() llega pre-filtrado desde ResumenTotal via df_recuperar.
# Indicadores: distribución por segmento, presupuesto, urgencia por meses sin facturar
# y valor en riesgo como columna de acción en tabla.

DetalleClienteRecuperarUI <- function(id) {
  ns <- NS(id)
  dc_ui_base(
    ns,
    titulo_tabla = "Detalle de Clientes a Recuperar",
    graficos = list(
      list(
        output_id   = "Segmento",
        titulo      = "Segmento",
        descripcion = paste(
          "Distribución de clientes a recuperar por segmento de negocio.",
          "Clic para filtrar la tabla."
        )
      ),
      list(
        output_id   = "Presupuestado",
        titulo      = "Presupuestado",
        descripcion = paste(
          "Clientes con presupuesto de sacos mayor a cero para el año vigente.",
          "Clic para filtrar la tabla."
        )
      ),
      list(
        output_id   = "Tiempo",
        titulo      = "Meses sin Facturar",
        descripcion = paste(
          "Clientes agrupados por meses transcurridos desde su última facturación.",
          "Clic para filtrar la tabla."
        )
      )
    )
  )
}

DetalleClienteRecuperar <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Source IDs namespaceados para evitar colisiones entre instancias del módulo
    src_segmento <- ns("segmento")
    src_conppto  <- ns("conppto")
    src_meses    <- ns("meses")
    
    # Estado reactivo de filtros por clic en gráficos
    filtros <- reactiveValues(segmento = NULL, conppto = NULL, meses = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$segmento <- NULL; filtros$conppto <- NULL; filtros$meses <- NULL
    })
    
    # Datos base: urgencia por meses sin facturar, valor en riesgo y métricas de presupuesto
    data_cliente <- reactive({
      waiter_show(html = preloader_calculando$html, color = preloader_calculando$color)
      on.exit(waiter_hide())
      req(dat())
      if (nrow(dat()) == 0) dc_sin_datos_msg()
      tryCatch({
        eje      <- dc_ejecucion(dat())
        fec      <- dc_fechas_fact()
        mes      <- month(Sys.Date())
        mes_falt <- pmax(12 - mes, 1)
        # Consolidación con presupuesto normalizado, joins y cálculo de indicadores
        dat() %>%
          dc_ppto_normalizar(mes) %>%
          select(
            LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
            ConPpto, PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD
          ) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            # Bucket de meses ordenado para urgencia de recuperación
            Meses = {
              m <- pmax(0, lubridate::interval(UltFact, Sys.Date()) %/% months(1))
              dplyr::case_when(
                m <= 3 ~ "Hasta 3 meses",
                m <= 6 ~ "De 4 a 6 meses",
                TRUE   ~ "Más de 6 meses"
              )
            }
          ) %>%
          dc_metricas_cumpl(mes_falt, format(Sys.Date(), "%b %Y")) %>%
          mutate(
            # Valor en riesgo: margen YTD histórico como proxy del impacto de no recuperar
            ValorEnRiesgo = dplyr::case_when(
              is.na(MargenYTD) | MargenYTD == 0 ~ "Sin ejecución",
              MargenYTD > 5e6                   ~ "Alto (>5M)",
              MargenYTD > 1e6                   ~ "Medio (1-5M)",
              TRUE                              ~ "Bajo (<1M)"
            )
          )
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Datos filtrados según selección activa en gráficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$segmento)) df <- df %>% filter(Segmento == filtros$segmento)
      if (!is.null(filtros$conppto))  df <- df %>% filter(ConPpto == filtros$conppto)
      if (!is.null(filtros$meses))    df <- df %>% filter(Meses == filtros$meses)
      df
    })
    
    # Datos para tabla con fila de totales; ValorEnRiesgo inicializado en blanco en total
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      fila_total <- dc_fila_total(df) %>% mutate(ValorEnRiesgo = "")
      df %>%
        crear_link_cliente(col_razsoc = "PerRazSoc", col_linneg = "CLLinNegNo") %>%
        select(
          Oportunidad, PerRazSoc, CLLinNegNo,
          ValorEnRiesgo, ConPpto, Segmento, UltFact,
          PptoSacos, SacosMes, CumpSacosMes,
          PptoMargen, MargenMes, CumpMargenMes,
          PptoSacosYTD, SacosYTD, CumpSacosYTD,
          PptoMargenYTD, MargenYTD, CumpMargenYTD,
          SacosCumpPpto, MargenCumpPpto, LblAcum
        ) %>%
        bind_rows(fila_total)
    })
    
    # Patrón eager: FormularioOportunidad registrado en startup antes de TablaReactable
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE A RECUPERAR")
    )
    
    # Niveles ordenados de urgencia para el gráfico de meses sin facturar
    lvl_meses <- c("Hasta 3 meses", "De 4 a 6 meses", "Más de 6 meses")
    
    # Renderizado de gráficos con source IDs namespaceados
    output$Segmento <- renderPlotly(
      dc_grafico(data_filtrada(), "Segmento", src_segmento, filtros$segmento)
    )
    output$Presupuestado <- renderPlotly(
      dc_grafico(data_filtrada(), "ConPpto", src_conppto, filtros$conppto)
    )
    output$Tiempo <- renderPlotly(
      dc_grafico(data_filtrada(), "Meses", src_meses, filtros$meses, niveles = lvl_meses)
    )
    
    # Registro diferido de observers de clic para eliminar warnings de plotly
    dc_registrar_clicks(session,
                        sources = list(segmento = src_segmento, conppto = src_conppto, meses = src_meses),
                        filtros = filtros
    )
    
    # Tabla reactable con colDefs específicos para clientes a recuperar.
    # CORRECCIÓN: col_specs recibe el named list de colDef (antes: argumento columnas).
    dc_tabla_reactable_base(data_tabla, data_filtrada, ns,
                            col_specs = modifyList(dc_coldefs_comunes(data_tabla), list(
                              Segmento = reactable::colDef(
                                name  = "Segmento", minWidth = 110,
                                style = function(v) list(
                                  background = "#FDEDEC", color = "#943126", fontWeight = "600"
                                )
                              ),
                              ValorEnRiesgo = reactable::colDef(
                                name  = "Valor en Riesgo", minWidth = 140,
                                style = function(v) {
                                  list(
                                    background = switch(v,
                                                        "Alto (>5M)"   = "#FADBD8",
                                                        "Medio (1-5M)" = "#FDEBD0",
                                                        "Bajo (<1M)"   = "#FCF3CF",
                                                        "Sin ejecución" = "#F8F9F9",
                                                        "white"
                                    ),
                                    color = switch(v,
                                                   "Alto (>5M)"   = "#922B21",
                                                   "Medio (1-5M)" = "#784212",
                                                   "Bajo (<1M)"   = "#7D6608",
                                                   "Sin ejecución" = "#707B7C",
                                                   "#333"
                                    ),
                                    fontWeight = "600"
                                  )
                                }
                              )
                            ))
    )
    
    # Descarga Excel de datos filtrados sin fila de totales
    output$Descargar <- do.call(
      downloadHandler, dc_descarga(data_filtrada, "clientes_recuperar")
    )
  })
}

# Clientes Nuevos ----
# Módulo: clientes con primera factura en el mes vigente sin historial previo en CRMNALSEGR
# ni en FACT. dat() llega pre-filtrado desde ResumenTotal via df_nuevas.
# Presupuesto excluido: clientes nuevos no tienen asignación presupuestal en el año vigente.
# Indicadores: segmento, línea de negocio, tamaño de primera compra (bucket sacos).
# PrimFact incluida en tabla como referencia de fecha de adquisición.

DetalleClienteNuevoUI <- function(id) {
  ns <- NS(id)
  dc_ui_base(
    ns,
    titulo_tabla = "Detalle de Clientes Nuevos",
    graficos = list(
      list(
        output_id   = "Segmento",
        titulo      = "Segmento",
        descripcion = paste(
          "Distribución de clientes nuevos del mes por segmento de negocio.",
          "Clic para filtrar la tabla."
        )
      ),
      list(
        output_id   = "LinNeg",
        titulo      = "Línea de Negocio",
        descripcion = paste(
          "Líneas de negocio que generaron nuevos clientes en el mes.",
          "Clic para filtrar la tabla."
        )
      ),
      list(
        output_id   = "BucketSacos",
        titulo      = "Tamaño de Primera Compra",
        descripcion = paste(
          "Clientes nuevos agrupados por volumen de sacos en su primera factura del mes.",
          "Clic para filtrar la tabla."
        )
      )
    )
  )
}

DetalleClienteNuevo <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Source IDs namespaceados para evitar colisiones entre instancias del módulo
    src_segmento    <- ns("segmento")
    src_linneg      <- ns("linneg")
    src_bucketsacos <- ns("bucketsacos")
    
    # Estado reactivo de filtros por clic en gráficos
    filtros <- reactiveValues(segmento = NULL, linneg = NULL, bucketsacos = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$segmento <- NULL; filtros$linneg <- NULL; filtros$bucketsacos <- NULL
    })
    
    # Datos base: tamaño de primera compra, línea de negocio y primera fecha de adquisición.
    # ConPpto excluido: clientes nuevos no tienen presupuesto asignado en el año vigente.
    data_cliente <- reactive({
      waiter_show(html = preloader_calculando$html, color = preloader_calculando$color)
      on.exit(waiter_hide())
      req(dat())
      if (nrow(dat()) == 0) dc_sin_datos_msg()
      tryCatch({
        eje      <- dc_ejecucion(dat())
        fec      <- dc_fechas_fact()
        mes      <- month(Sys.Date())
        mes_falt <- pmax(12 - mes, 1)
        # Consolidación: dat() ya contiene solo nuevos — no requiere filtro ni semi_join
        dat() %>%
          dc_ppto_normalizar(mes) %>%
          select(
            LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
            PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD
          ) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(SacosMes = ifelse(is.na(SacosMes), 0, SacosMes)) %>%
          dc_metricas_cumpl(mes_falt, format(Sys.Date(), "%b %Y")) %>%
          mutate(
            # Bucket de tamaño de primera compra: indica potencial del cliente nuevo
            BucketSacos = dplyr::case_when(
              SacosMes >= 100 ~ ">=100 sacos",
              SacosMes >= 50  ~ "50-99 sacos",
              SacosMes >= 10  ~ "10-49 sacos",
              SacosMes > 0    ~ "<10 sacos",
              TRUE            ~ "Sin compra"
            )
          )
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Datos filtrados según selección activa en gráficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$segmento))    df <- df %>% filter(Segmento == filtros$segmento)
      if (!is.null(filtros$linneg))      df <- df %>% filter(CLLinNegNo == filtros$linneg)
      if (!is.null(filtros$bucketsacos)) df <- df %>% filter(BucketSacos == filtros$bucketsacos)
      df
    })
    
    # Datos para tabla con fila de totales; PrimFact y BucketSacos inicializados en total
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      fila_total <- dc_fila_total(df) %>%
        mutate(BucketSacos = "", PrimFact = as.Date(NA))
      df %>%
        crear_link_cliente(col_razsoc = "PerRazSoc", col_linneg = "CLLinNegNo") %>%
        select(
          Oportunidad, PerRazSoc, CLLinNegNo,
          BucketSacos, Segmento, PrimFact, UltFact,
          PptoSacos, SacosMes, CumpSacosMes,
          PptoMargen, MargenMes, CumpMargenMes,
          PptoSacosYTD, SacosYTD, CumpSacosYTD,
          PptoMargenYTD, MargenYTD, CumpMargenYTD,
          SacosCumpPpto, MargenCumpPpto, LblAcum
        ) %>%
        bind_rows(fila_total)
    })
    
    # Patrón eager: FormularioOportunidad registrado en startup antes de TablaReactable
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE")
    )
    
    # Niveles ordenados de menor a mayor para el gráfico de tamaño de primera compra
    lvl_sacos <- c("Sin compra", "<10 sacos", "10-49 sacos", "50-99 sacos", ">=100 sacos")
    
    # Renderizado de gráficos con source IDs namespaceados
    output$Segmento <- renderPlotly(
      dc_grafico(data_filtrada(), "Segmento", src_segmento, filtros$segmento)
    )
    output$LinNeg <- renderPlotly(
      dc_grafico(data_filtrada(), "CLLinNegNo", src_linneg, filtros$linneg)
    )
    output$BucketSacos <- renderPlotly(
      dc_grafico(
        data_filtrada(), "BucketSacos", src_bucketsacos, filtros$bucketsacos,
        niveles = lvl_sacos
      )
    )
    
    # Registro diferido de observers de clic para eliminar warnings de plotly
    dc_registrar_clicks(session,
                        sources = list(
                          segmento    = src_segmento,
                          linneg      = src_linneg,
                          bucketsacos = src_bucketsacos
                        ),
                        filtros = filtros
    )
    
    # Tabla reactable: PrimFact agregado via modifyList; no está en dc_coldefs_comunes.
    # CORRECCIÓN: col_specs recibe el named list de colDef (antes: argumento columnas).
    dc_tabla_reactable_base(data_tabla, data_filtrada, ns,
                            col_specs = modifyList(dc_coldefs_comunes(data_tabla), list(
                              Segmento = reactable::colDef(
                                name  = "Segmento", minWidth = 110,
                                style = function(v) list(
                                  background = "#EDFBF2", color = "#1E8449", fontWeight = "600"
                                )
                              ),
                              BucketSacos = reactable::colDef(
                                name  = "Primera Compra", minWidth = 130,
                                style = function(v) {
                                  list(
                                    background = switch(v,
                                                        ">=100 sacos" = "#D5F5E3",
                                                        "50-99 sacos" = "#EAFAF1",
                                                        "10-49 sacos" = "#FCF3CF",
                                                        "<10 sacos"   = "#FEF9E7",
                                                        "Sin compra"  = "#F8F9F9",
                                                        "white"
                                    ),
                                    color = switch(v,
                                                   ">=100 sacos" = "#1A5226",
                                                   "50-99 sacos" = "#1E8449",
                                                   "10-49 sacos" = "#7D6608",
                                                   "<10 sacos"   = "#9A7D0A",
                                                   "Sin compra"  = "#707B7C",
                                                   "#333"
                                    ),
                                    fontWeight = "600"
                                  )
                                }
                              ),
                              PrimFact = reactable::colDef(
                                name = "Primera Factura", minWidth = 130,
                                cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d/%m/%Y")
                              )
                            ))
    )
    
    # Descarga Excel de datos filtrados sin fila de totales
    output$Descargar <- do.call(
      downloadHandler, dc_descarga(data_filtrada, "clientes_nuevos")
    )
  })
}
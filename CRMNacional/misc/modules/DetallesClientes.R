# Helpers Detalle Clientes ----

# Carga y preprocesa CRMNALSEGR: limpieza, cast de fechas y resumen Antes/Ahora por cliente
dc_cargar_segr <- function() {
  CargarDatos("CRMNALSEGR") %>%
    mutate(
      FecProceso = as.Date(FecProceso),
      across(where(is.numeric),   ~ifelse(is.na(.), 0, .)),
      across(where(is.character), ~ifelse(is.na(.) | . == "N/A", "", .))
    ) %>%
    filter(FecProceso >= PrimerDia(Sys.Date()) - months(1)) %>%
    group_by(LinNegCod, CliNitPpal) %>%
    arrange(FecProceso) %>%
    summarise(Antes = first(SegmentoRacafe), Ahora = last(SegmentoRacafe), .groups = "drop")
}

# Calcula ejecucion mensual y YTD por cliente desde datos de facturacion filtrados
dc_ejecucion <- function(dat_df) {
  dat_df %>%
    group_by(LinNegCod, CliNitPpal) %>%
    summarise(
      SacosMes  = sum(
        ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), SacFact70, 0), na.rm = TRUE
      ),
      MargenMes = sum(
        ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Margen, 0), na.rm = TRUE
      ),
      SacosYTD  = sum(
        ifelse(year(FecFact) == year(Sys.Date()), SacFact70, 0), na.rm = TRUE
      ),
      MargenYTD = sum(
        ifelse(year(FecFact) == year(Sys.Date()), Margen, 0), na.rm = TRUE
      ),
      .groups = "drop"
    )
}

# Calcula primera y ultima fecha de facturacion por cliente desde objeto global data
dc_fechas_fact <- function() {
  data %>%
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

# Agrega metricas de cumplimiento mensual, acumulado, proyeccion y etiquetas al df
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
    CumpSacosMes   = d(s("SacosMes"),  s("PptoSacos")),
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

# Grafico de barras con resaltado del filtro activo; niveles permite ordenar el eje X
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
  plot_ly(data = resumen, x = ~get(columna), y = ~Total, type = "bar",
          text = ~Total, textposition = "outside", textangle = 0,
          textfont = list(size = 12), hovertext = ~Hover, hoverinfo = "text",
          marker = list(color = ~Color), source = source_name) %>%
    layout(
      xaxis = list(title = "", tickangle = 0, tickfont = list(size = 12)),
      yaxis = list(title = "Cantidad de Clientes", range = c(0, ymax))
    ) %>%
    event_register("plotly_click") %>%
    config(displayModeBar = FALSE)
}

# Genera lista de colDefs compartidas entre todos los modulos de detalle de clientes
dc_coldefs_comunes <- function(data_tabla_fn) {
  get_lbl   <- function() tryCatch(unique(data_tabla_fn()$LblAcum)[1], error = function(e) "")
  cell_pct  <- function(v) {
    if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%")
  }
  style_sem <- function(v) {
    if (is.na(v) || is.infinite(v)) return(NULL)
    list(
      background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
      fontWeight = "600"
    )
  }
  cell_num <- function(v, px = "") {
    if (is.na(v)) "—" else paste0(px, format(round(v), big.mark = ","))
  }
  lbl_col  <- function(pre) function(value, name) paste0(pre, get_lbl())
  list(
    Oportunidad = reactable::colDef(
      name = "", minWidth = 55, html = TRUE,
      cell = function(v) {
        if (v == "") return("")
        as.character(tags$span(
          style = paste(
            "display:inline-flex; align-items:center; justify-content:center;",
            "width:28px; height:28px; border-radius:6px;",
            "background:#C11007; color:white; font-size:13px; cursor:pointer;"
          ),
          icon("hand-holding-dollar")
        ))
      }
    ),
    PerRazSoc = reactable::colDef(
      name = "Cliente", minWidth = 200, html = TRUE,
      cell = function(v) as.character(htmltools::HTML(v))
    ),
    CLLinNegNo    = reactable::colDef(name = "Linea de Negocio", minWidth = 140),
    ConPpto       = reactable::colDef(name = "Presupuestado",    minWidth = 130),
    Segmento      = reactable::colDef(name = "Segmento",         minWidth = 110),
    UltFact       = reactable::colDef(
      name = "Ult. Facturacion", minWidth = 120,
      cell = function(v) if (is.na(v)) "—" else format(as.Date(v), "%d/%m/%Y")
    ),
    PptoSacos     = reactable::colDef(
      name = "Ppto Sacos Mes", minWidth = 120, cell = function(v) cell_num(v)
    ),
    SacosMes      = reactable::colDef(
      name = "Sacos Mes", minWidth = 100,
      cell = function(v) format(round(v), big.mark = ",")
    ),
    CumpSacosMes  = reactable::colDef(
      name = "% Cumpl Sacos Mes", minWidth = 130, cell = cell_pct, style = style_sem
    ),
    PptoMargen    = reactable::colDef(
      name = "Ppto Margen Mes", minWidth = 130, cell = function(v) cell_num(v, "$")
    ),
    MargenMes     = reactable::colDef(
      name = "Margen Mes", minWidth = 120,
      cell = function(v) paste0("$", format(round(v), big.mark = ","))
    ),
    CumpMargenMes = reactable::colDef(
      name = "% Cumpl Margen Mes", minWidth = 140, cell = cell_pct, style = style_sem
    ),
    LblAcum       = reactable::colDef(show = FALSE),
    PptoSacosYTD  = reactable::colDef(
      minWidth = 140, header = lbl_col("Ppto Sacos Acum. "), cell = function(v) cell_num(v)
    ),
    SacosYTD      = reactable::colDef(
      minWidth = 130, header = lbl_col("Sacos Acum. "),
      cell = function(v) format(round(v), big.mark = ",")
    ),
    CumpSacosYTD  = reactable::colDef(
      minWidth = 150, header = lbl_col("% Cumpl Sacos Acum. "), cell = cell_pct, style = style_sem
    ),
    PptoMargenYTD = reactable::colDef(
      minWidth = 150, header = lbl_col("Ppto Margen Acum. "), cell = function(v) cell_num(v, "$")
    ),
    MargenYTD     = reactable::colDef(
      minWidth = 140, header = lbl_col("Margen Acum. "),
      cell = function(v) paste0("$", format(round(v), big.mark = ","))
    ),
    CumpMargenYTD = reactable::colDef(
      minWidth = 160, header = lbl_col("% Cumpl Margen Acum. "), cell = cell_pct, style = style_sem
    ),
    SacosCumpPpto  = reactable::colDef(
      name = "Sacos proy. cumplir Ppto", minWidth = 180,
      cell = function(v) format(round(v), big.mark = ",")
    ),
    MargenCumpPpto = reactable::colDef(
      name = "Margen proy. cumplir Ppto", minWidth = 190,
      cell = function(v) paste0("$", format(round(v), big.mark = ","))
    )
  )
}

# UI reutilizable: boton limpiar, tres graficos plotly, tabla reactable y boton descarga
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
                 TablaReactableUI(ns("TablaClientes"),
                                  titulo       = titulo_tabla,
                                  footer       = paste(
                                    "Clic en el boton Crear para registrar una oportunidad."
                                  ),
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

# Pre-registra source IDs en session$userData$.plotlyShinyEventIDs para que event_data()
# no emita warnings antes de que los renderPlotly hayan ejecutado. Registra observers
# sincronamente ya que el registry esta poblado desde el inicio.
# sources es named list(campo = src_id_namespaceado)
dc_registrar_clicks <- function(session, sources, filtros) {
  # Insercion directa en el registry interno de plotly; garantiza que event_data()
  # encuentre los source IDs registrados antes del primer ciclo reactivo
  ids_actuales <- session$userData$.plotlyShinyEventIDs
  session$userData$.plotlyShinyEventIDs <- unique(c(ids_actuales, unname(unlist(sources))))
  for (campo in names(sources)) {
    local({
      cam <- campo
      src <- sources[[cam]]
      observeEvent(event_data("plotly_click", source = src), {
        click <- event_data("plotly_click", source = src)
        if (!is.null(click))
          filtros[[cam]] <- if (!is.null(filtros[[cam]]) && filtros[[cam]] == click$x) {
            NULL
          } else {
            click$x
          }
      }, ignoreNULL = TRUE)
    })
  }
}

# Config base compartida para llamadas a TablaReactable en todos los modulos de detalle
dc_tabla_reactable_base <- function(data_tabla, data_filtrada, ns, columnas) {
  dd_oportunidad_rv <- reactiveVal(NULL)
  TablaReactable(
    id              = "TablaClientes",
    data            = data_tabla,
    modo_seleccion  = "celda",
    id_col          = NULL,
    col_header_n    = 2L,
    cols_activos    = "Oportunidad",
    sortable        = TRUE,
    searchable      = TRUE,
    page_size       = 15,
    compact         = TRUE,
    mostrar_badge   = FALSE,
    mostrar_nota    = FALSE,
    modal_icon      = "hand-holding-dollar",
    modal_size      = "xl",
    modal_titulo_fn = function(sel) {
      paste0("Crear Oportunidad — ", as.character(sel$fila$PerRazSoc[[1]]))
    },
    modal_pre_fn = function(sel) {
      dd_oportunidad_rv(list(
        data   = data_filtrada() %>%
          filter(
            PerRazSoc  == as.character(sel$fila$PerRazSoc[[1]]),
            CLLinNegNo == as.character(sel$fila$CLLinNegNo[[1]])
          ),
        accion = "oportunidad"
      ))
    },
    modal_contenido_fn = function(sel) FormularioOportunidadUI(ns("mod_formulario")),
    columnas = columnas
  )
}

# Clientes ----

DetalleClienteUI <- function(id) {
  ns <- NS(id)
  dc_ui_base(
    ns,
    titulo_tabla = "Detalle de Clientes",
    graficos = list(
      list(
        output_id   = "TipoCliente",
        titulo      = "Tipo de Cliente",
        descripcion = paste(
          "Nuevos y Recuperados: clientes que a corte del mes en curso pasaron a ser clientes."
        )
      ),
      list(
        output_id   = "Presupuestado",
        titulo      = "Presupuestado",
        descripcion = "Clientes con presupuesto de sacos mayor a cero para el ano vigente."
      ),
      list(
        output_id   = "Tiempo",
        titulo      = "Riesgo de Perdida",
        descripcion = "Clientes agrupados por meses transcurridos desde su ultima facturacion."
      )
    )
  )
}

DetalleCliente <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Source IDs namespaceados para evitar colisiones entre instancias del modulo
    src_tipo    <- ns("tipo")
    src_conppto <- ns("conppto")
    src_meses   <- ns("meses")
    
    # Estado reactivo de filtros por clic en graficos
    filtros <- reactiveValues(tipo = NULL, conppto = NULL, meses = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$tipo <- NULL; filtros$conppto <- NULL; filtros$meses <- NULL
    })
    
    # Datos base: clasificacion tipo cliente, ejecucion, fechas y metricas de presupuesto
    data_cliente <- reactive({
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      req(dat())
      tryCatch({
        # Clasificacion Activos/Nuevos/Recuperados segun transicion en CRMNALSEGR
        rec <- dc_cargar_segr() %>%
          filter(Ahora == "CLIENTE") %>%
          mutate(Tipo = case_when(
            is.na(Antes) & Ahora == "CLIENTE"                   ~ "CLIENTE NUEVO",
            Antes == "CLIENTE A RECUPERAR" & Ahora == "CLIENTE" ~ "CLIENTE RECUPERADO",
            TRUE                                                 ~ "CLIENTE"
          )) %>%
          select(LinNegCod, CliNitPpal, Tipo)
        eje      <- dc_ejecucion(dat())
        fec      <- dc_fechas_fact()
        mes      <- month(Sys.Date()); mes_falt <- pmax(12 - mes, 1)
        # Consolidacion con presupuesto normalizado, joins y metricas finales
        dat() %>%
          filter(SegmentoRacafe == "CLIENTE") %>%
          dc_ppto_normalizar(mes) %>%
          select(LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
                 ConPpto, PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(rec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            Tipo  = ifelse(is.na(Tipo), "CLIENTE NUEVO", Tipo),
            Meses = paste(pmax(0, lubridate::interval(UltFact, Sys.Date()) %/% months(1)), "MESES")
          ) %>%
          dc_metricas_cumpl(mes_falt, format(Sys.Date(), "%b %Y"))
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Datos filtrados segun seleccion activa en graficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$tipo))    df <- df %>% filter(Tipo    == filtros$tipo)
      if (!is.null(filtros$conppto)) df <- df %>% filter(ConPpto == filtros$conppto)
      if (!is.null(filtros$meses))   df <- df %>% filter(Meses   == filtros$meses)
      df
    })
    
    # Datos para tabla con fila de totales; Tipo agregado manualmente al total
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      fila_total <- dc_fila_total(df) %>% mutate(Tipo = "")
      df %>%
        crear_link_cliente(col_razsoc = "PerRazSoc", col_linneg = "CLLinNegNo") %>%
        select(Oportunidad, PerRazSoc, CLLinNegNo, Tipo, ConPpto, Segmento, UltFact,
               PptoSacos, SacosMes, CumpSacosMes,
               PptoMargen, MargenMes, CumpMargenMes,
               PptoSacosYTD, SacosYTD, CumpSacosYTD,
               PptoMargenYTD, MargenYTD, CumpMargenYTD,
               SacosCumpPpto, MargenCumpPpto, LblAcum) %>%
        bind_rows(fila_total)
    })
    
    # Patron eager: FormularioOportunidad registrado en startup antes de TablaReactable
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE"))
    
    # Renderizado de graficos con source IDs namespaceados
    output$TipoCliente   <- renderPlotly(
      dc_grafico(data_filtrada(), "Tipo", src_tipo, filtros$tipo)
    )
    output$Presupuestado <- renderPlotly(
      dc_grafico(data_filtrada(), "ConPpto", src_conppto, filtros$conppto)
    )
    output$Tiempo        <- renderPlotly(
      dc_grafico(data_filtrada(), "Meses", src_meses, filtros$meses)
    )
    
    
    # Registro diferido de observers de click para eliminar warnings de fuentes no registradas
    dc_registrar_clicks(session,
                        sources = list(tipo = src_tipo, conppto = src_conppto, meses = src_meses),
                        filtros = filtros)
    
    # Tabla reactable con colDef de Tipo especifico para clientes activos
    dc_tabla_reactable_base(data_tabla, data_filtrada, ns,
                            columnas = modifyList(dc_coldefs_comunes(data_tabla), list(
                              Tipo = reactable::colDef(
                                name = "Tipo", minWidth = 140,
                                style = function(v) {
                                  list(
                                    background = switch(v,
                                                        "CLIENTE"            = "#EFF6FF",
                                                        "CLIENTE NUEVO"      = "#EDFBF2",
                                                        "CLIENTE RECUPERADO" = "#FFF8EC",
                                                        "white"
                                    ),
                                    color = switch(v,
                                                   "CLIENTE"            = "#1A5276",
                                                   "CLIENTE NUEVO"      = "#1E8449",
                                                   "CLIENTE RECUPERADO" = "#784212",
                                                   "#333"
                                    ),
                                    fontWeight = "600"
                                  )
                                }
                              )
                            ))
    )
    
    # Descarga Excel de datos filtrados sin fila de totales
    output$Descargar <- do.call(downloadHandler, dc_descarga(data_filtrada, "clientes_detalle"))
  })
}

# Clientes a Recuperar ----

DetalleClienteRecuperarUI <- function(id) {
  ns <- NS(id)
  dc_ui_base(
    ns,
    titulo_tabla = "Detalle de Clientes a Recuperar",
    graficos = list(
      list(
        output_id   = "TipoCliente",
        titulo      = "Tipo de Cliente a Recuperar",
        descripcion = paste(
          "Nuevos A Recuperar: clientes que a corte del mes en curso pasaron a este segmento."
        )
      ),
      list(
        output_id   = "Presupuestado",
        titulo      = "Presupuestado",
        descripcion = "Clientes con presupuesto de sacos mayor a cero para el ano vigente."
      ),
      list(
        output_id   = "Tiempo",
        titulo      = "Meses sin Facturar",
        descripcion = "Clientes agrupados por meses transcurridos desde su ultima facturacion."
      )
    )
  )
}

DetalleClienteRecuperar <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Source IDs namespaceados para evitar colisiones entre instancias del modulo
    src_tipo    <- ns("tipo")
    src_conppto <- ns("conppto")
    src_meses   <- ns("meses")
    
    # Estado reactivo de filtros por clic en graficos
    filtros <- reactiveValues(tipo = NULL, conppto = NULL, meses = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$tipo <- NULL; filtros$conppto <- NULL; filtros$meses <- NULL
    })
    
    # Datos base: clasificacion tipo a recuperar, ejecucion, fechas y metricas de presupuesto
    data_cliente <- reactive({
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      req(dat())
      tryCatch({
        # Identificacion de nuevos vs ya existentes en el segmento A RECUPERAR
        rec <- dc_cargar_segr() %>%
          filter(Ahora == "CLIENTE A RECUPERAR") %>%
          mutate(Tipo = case_when(
            Antes == "CLIENTE" ~ "NUEVO CLIENTE A RECUPERAR",
            TRUE               ~ "CLIENTE A RECUPERAR"
          )) %>%
          select(LinNegCod, CliNitPpal, Tipo)
        eje      <- dc_ejecucion(dat())
        fec      <- dc_fechas_fact()
        mes      <- month(Sys.Date()); mes_falt <- pmax(12 - mes, 1)
        # Consolidacion con presupuesto normalizado, joins y metricas finales
        dat() %>%
          filter(SegmentoRacafe == "CLIENTE A RECUPERAR") %>%
          dc_ppto_normalizar(mes) %>%
          select(LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
                 ConPpto, PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(rec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            Tipo  = ifelse(is.na(Tipo), "CLIENTE A RECUPERAR", Tipo),
            Meses = {
              m <- pmax(0, lubridate::interval(UltFact, Sys.Date()) %/% months(1))
              dplyr::case_when(
                m <= 3 ~ "Hasta 3 meses",
                m <= 6 ~ "De 4 a 6 meses",
                TRUE   ~ "Mas de 6 meses"
              )
            }
          ) %>%
          dc_metricas_cumpl(mes_falt, format(Sys.Date(), "%b %Y"))
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Datos filtrados segun seleccion activa en graficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$tipo))    df <- df %>% filter(Tipo    == filtros$tipo)
      if (!is.null(filtros$conppto)) df <- df %>% filter(ConPpto == filtros$conppto)
      if (!is.null(filtros$meses))   df <- df %>% filter(Meses   == filtros$meses)
      df
    })
    
    # Datos para tabla con fila de totales; Tipo agregado manualmente al total
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      fila_total <- dc_fila_total(df) %>% mutate(Tipo = "")
      df %>%
        crear_link_cliente(col_razsoc = "PerRazSoc", col_linneg = "CLLinNegNo") %>%
        select(Oportunidad, PerRazSoc, CLLinNegNo, Tipo, ConPpto, Segmento, UltFact,
               PptoSacos, SacosMes, CumpSacosMes,
               PptoMargen, MargenMes, CumpMargenMes,
               PptoSacosYTD, SacosYTD, CumpSacosYTD,
               PptoMargenYTD, MargenYTD, CumpMargenYTD,
               SacosCumpPpto, MargenCumpPpto, LblAcum) %>%
        bind_rows(fila_total)
    })
    
    # Patron eager: FormularioOportunidad registrado en startup antes de TablaReactable
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE A RECUPERAR"))
    
    # Renderizado con niveles ordenados para el grafico de Meses
    lvl_meses <- c("Hasta 3 meses", "De 4 a 6 meses", "Mas de 6 meses")
    output$TipoCliente   <- renderPlotly(
      dc_grafico(data_filtrada(), "Tipo", src_tipo, filtros$tipo)
    )
    output$Presupuestado <- renderPlotly(
      dc_grafico(data_filtrada(), "ConPpto", src_conppto, filtros$conppto)
    )
    output$Tiempo        <- renderPlotly(
      dc_grafico(data_filtrada(), "Meses", src_meses, filtros$meses, niveles = lvl_meses)
    )
    
    
    # Registro diferido de observers de click para eliminar warnings de fuentes no registradas
    dc_registrar_clicks(session,
                        sources = list(tipo = src_tipo, conppto = src_conppto, meses = src_meses),
                        filtros = filtros)
    
    # Tabla reactable con colDef de Tipo especifico para clientes a recuperar
    dc_tabla_reactable_base(data_tabla, data_filtrada, ns,
                            columnas = modifyList(dc_coldefs_comunes(data_tabla), list(
                              Tipo = reactable::colDef(
                                name = "Tipo", minWidth = 180,
                                style = function(v) {
                                  list(
                                    background = switch(v,
                                                        "NUEVO CLIENTE A RECUPERAR" = "#FFF8EC",
                                                        "CLIENTE A RECUPERAR"       = "#FDEDEC",
                                                        "white"
                                    ),
                                    color = switch(v,
                                                   "NUEVO CLIENTE A RECUPERAR" = "#784212",
                                                   "CLIENTE A RECUPERAR"       = "#943126",
                                                   "#333"
                                    ),
                                    fontWeight = "600"
                                  )
                                }
                              )
                            ))
    )
    
    # Descarga Excel de datos filtrados sin fila de totales
    output$Descargar <- do.call(downloadHandler, dc_descarga(data_filtrada, "clientes_recuperar"))
  })
}

# Clientes Recuperados ----

DetalleClienteRecuperadoUI <- function(id) {
  ns <- NS(id)
  dc_ui_base(
    ns,
    titulo_tabla = "Detalle de Clientes Recuperados",
    graficos = list(
      list(
        output_id   = "Segmento",
        titulo      = "Segmento",
        descripcion = "Distribucion de clientes recuperados del mes por segmento de negocio."
      ),
      list(
        output_id   = "Presupuestado",
        titulo      = "Presupuestado",
        descripcion = "Clientes con presupuesto de sacos mayor a cero para el ano vigente."
      ),
      list(
        output_id   = "Tiempo",
        titulo      = "Meses desde Recuperacion",
        descripcion = "Clientes agrupados por meses transcurridos desde que volvieron a facturar."
      )
    )
  )
}

DetalleClienteRecuperado <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Source IDs namespaceados para evitar colisiones entre instancias del modulo
    src_segmento <- ns("segmento")
    src_conppto  <- ns("conppto")
    src_meses    <- ns("meses")
    
    # Estado reactivo de filtros por clic en graficos
    filtros <- reactiveValues(segmento = NULL, conppto = NULL, meses = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$segmento <- NULL; filtros$conppto <- NULL; filtros$meses <- NULL
    })
    
    # Datos base: clientes que pasaron de A RECUPERAR a CLIENTE en el mes actual
    data_cliente <- reactive({
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      req(dat())
      tryCatch({
        # Poblacion recuperados: transicion de CLIENTE A RECUPERAR a CLIENTE en el mes
        rec <- dc_cargar_segr() %>%
          filter(Ahora == "CLIENTE", Antes == "CLIENTE A RECUPERAR") %>%
          select(LinNegCod, CliNitPpal)
        eje      <- dc_ejecucion(dat())
        fec      <- dc_fechas_fact()
        mes      <- month(Sys.Date()); mes_falt <- pmax(12 - mes, 1)
        # Consolidacion restringida a poblacion recuperada via semi_join
        dat() %>%
          filter(SegmentoRacafe == "CLIENTE") %>%
          semi_join(rec, by = c("LinNegCod", "CliNitPpal")) %>%
          dc_ppto_normalizar(mes) %>%
          select(LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
                 ConPpto, PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            Meses = paste(pmax(0, lubridate::interval(UltFact, Sys.Date()) %/% months(1)), "MESES")
          ) %>%
          dc_metricas_cumpl(mes_falt, format(Sys.Date(), "%b %Y"))
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Datos filtrados segun seleccion activa en graficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$segmento)) df <- df %>% filter(Segmento == filtros$segmento)
      if (!is.null(filtros$conppto))  df <- df %>% filter(ConPpto  == filtros$conppto)
      if (!is.null(filtros$meses))    df <- df %>% filter(Meses    == filtros$meses)
      df
    })
    
    # Datos para tabla con fila de totales
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      df %>%
        crear_link_cliente(col_razsoc = "PerRazSoc", col_linneg = "CLLinNegNo") %>%
        select(Oportunidad, PerRazSoc, CLLinNegNo, Segmento, ConPpto, UltFact,
               PptoSacos, SacosMes, CumpSacosMes,
               PptoMargen, MargenMes, CumpMargenMes,
               PptoSacosYTD, SacosYTD, CumpSacosYTD,
               PptoMargenYTD, MargenYTD, CumpMargenYTD,
               SacosCumpPpto, MargenCumpPpto, LblAcum) %>%
        bind_rows(dc_fila_total(df))
    })
    
    # Patron eager: FormularioOportunidad registrado en startup antes de TablaReactable
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE"))
    
    # Renderizado de graficos con source IDs namespaceados
    output$Segmento      <- renderPlotly(
      dc_grafico(data_filtrada(), "Segmento", src_segmento, filtros$segmento)
    )
    output$Presupuestado <- renderPlotly(
      dc_grafico(data_filtrada(), "ConPpto", src_conppto, filtros$conppto)
    )
    output$Tiempo        <- renderPlotly(
      dc_grafico(data_filtrada(), "Meses", src_meses, filtros$meses)
    )
    
    
    # Registro diferido de observers de click para eliminar warnings de fuentes no registradas
    dc_registrar_clicks(session,
                        sources = list(
                          segmento = src_segmento, conppto = src_conppto, meses = src_meses
                        ),
                        filtros = filtros)
    
    # Tabla reactable con Segmento coloreado segun tonos de recuperacion
    dc_tabla_reactable_base(data_tabla, data_filtrada, ns,
                            columnas = modifyList(dc_coldefs_comunes(data_tabla), list(
                              Segmento = reactable::colDef(
                                name = "Segmento", minWidth = 110,
                                style = function(v) list(background = "#FFF8EC", color = "#784212", fontWeight = "600")
                              )
                            ))
    )
    
    # Descarga Excel de datos filtrados sin fila de totales
    output$Descargar <- do.call(downloadHandler, dc_descarga(data_filtrada, "clientes_recuperados"))
  })
}

# Clientes Nuevos ----

DetalleClienteNuevoUI <- function(id) {
  ns <- NS(id)
  dc_ui_base(
    ns,
    titulo_tabla = "Detalle de Clientes Nuevos",
    graficos = list(
      list(
        output_id   = "Segmento",
        titulo      = "Segmento",
        descripcion = "Distribucion de clientes nuevos del mes por segmento de negocio."
      ),
      list(
        output_id   = "Presupuestado",
        titulo      = "Presupuestado",
        descripcion = "Clientes con presupuesto de sacos mayor a cero para el ano vigente."
      ),
      list(
        output_id   = "Tiempo",
        titulo      = "Meses desde Primera Facturacion",
        descripcion = paste(
          "Clientes agrupados por meses transcurridos desde su primera facturacion en el ano."
        )
      )
    )
  )
}

DetalleClienteNuevo <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Source IDs namespaceados para evitar colisiones entre instancias del modulo
    src_segmento <- ns("segmento")
    src_conppto  <- ns("conppto")
    src_meses    <- ns("meses")
    
    # Estado reactivo de filtros por clic en graficos
    filtros <- reactiveValues(segmento = NULL, conppto = NULL, meses = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$segmento <- NULL; filtros$conppto <- NULL; filtros$meses <- NULL
    })
    
    # Datos base: clientes sin estado previo que facturaron por primera vez en el mes
    data_cliente <- reactive({
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      req(dat())
      tryCatch({
        # Poblacion nuevos: CLIENTE sin Antes en el snapshot del mes
        rec <- dc_cargar_segr() %>%
          filter(Ahora == "CLIENTE", is.na(Antes)) %>%
          select(LinNegCod, CliNitPpal)
        eje      <- dc_ejecucion(dat())
        fec      <- dc_fechas_fact()
        mes      <- month(Sys.Date()); mes_falt <- pmax(12 - mes, 1)
        # Consolidacion restringida a poblacion nueva via semi_join
        dat() %>%
          filter(SegmentoRacafe == "CLIENTE") %>%
          semi_join(rec, by = c("LinNegCod", "CliNitPpal")) %>%
          dc_ppto_normalizar(mes) %>%
          select(LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
                 ConPpto, PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            # Meses desde primera facturacion, no desde ultima (diferencia vs otros modulos)
            Meses = paste(pmax(0, lubridate::interval(PrimFact, Sys.Date()) %/% months(1)), "MESES")
          ) %>%
          dc_metricas_cumpl(mes_falt, format(Sys.Date(), "%b %Y"))
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Datos filtrados segun seleccion activa en graficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$segmento)) df <- df %>% filter(Segmento == filtros$segmento)
      if (!is.null(filtros$conppto))  df <- df %>% filter(ConPpto  == filtros$conppto)
      if (!is.null(filtros$meses))    df <- df %>% filter(Meses    == filtros$meses)
      df
    })
    
    # Datos para tabla con fila de totales; UltFact presente aunque no se use en Meses
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      df %>%
        crear_link_cliente(col_razsoc = "PerRazSoc", col_linneg = "CLLinNegNo") %>%
        select(Oportunidad, PerRazSoc, CLLinNegNo, Segmento, ConPpto, UltFact,
               PptoSacos, SacosMes, CumpSacosMes,
               PptoMargen, MargenMes, CumpMargenMes,
               PptoSacosYTD, SacosYTD, CumpSacosYTD,
               PptoMargenYTD, MargenYTD, CumpMargenYTD,
               SacosCumpPpto, MargenCumpPpto, LblAcum) %>%
        bind_rows(dc_fila_total(df))
    })
    
    # Patron eager: FormularioOportunidad registrado en startup antes de TablaReactable
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE"))
    
    # Renderizado de graficos con source IDs namespaceados
    output$Segmento      <- renderPlotly(
      dc_grafico(data_filtrada(), "Segmento", src_segmento, filtros$segmento)
    )
    output$Presupuestado <- renderPlotly(
      dc_grafico(data_filtrada(), "ConPpto", src_conppto, filtros$conppto)
    )
    output$Tiempo        <- renderPlotly(
      dc_grafico(data_filtrada(), "Meses", src_meses, filtros$meses)
    )
    
    
    # Registro diferido de observers de click para eliminar warnings de fuentes no registradas
    dc_registrar_clicks(session,
                        sources = list(
                          segmento = src_segmento, conppto = src_conppto, meses = src_meses
                        ),
                        filtros = filtros)
    
    # Tabla reactable con Segmento coloreado segun tonos de clientes nuevos
    dc_tabla_reactable_base(data_tabla, data_filtrada, ns,
                            columnas = modifyList(dc_coldefs_comunes(data_tabla), list(
                              Segmento = reactable::colDef(
                                name = "Segmento", minWidth = 110,
                                style = function(v) list(background = "#EDFBF2", color = "#1E8449", fontWeight = "600")
                              )
                            ))
    )
    
    # Descarga Excel de datos filtrados sin fila de totales
    output$Descargar <- do.call(downloadHandler, dc_descarga(data_filtrada, "clientes_nuevos"))
  })
}

# App de prueba ----
ui <- bs4DashPage(
  title      = "Prueba DetalleCliente",
  header     = bs4DashNavbar(),
  sidebar    = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer     = bs4DashFooter(),
  body       = bs4DashBody(useShinyjs(), DetalleClienteUI("resumen"))
)

server <- function(input, output, session) {
  DetalleCliente(
    "resumen",
    dat            = reactive(BaseDatos),
    usr            = reactive("HCYATE"),
    trigger_update = reactive(0)
  )
}

shinyApp(ui, server)
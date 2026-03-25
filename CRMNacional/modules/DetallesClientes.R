# Clientes ----
DetalleClienteUI <- function(id) {
  ns <- NS(id)
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
    fluidRow(
      column(4,
             h5("Tipo de Cliente"),
             p("Nuevos y Recuperados: clientes que a corte del mes en curso pasaron a ser clientes.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("TipoCliente"), height = "300px")
      ),
      column(4,
             h5("Presupuestado"),
             p("Clientes con presupuesto de sacos mayor a cero para el ano vigente.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("Presupuestado"), height = "300px")
      ),
      column(4,
             h5("Riesgo de Perdida"),
             p("Clientes agrupados por meses transcurridos desde su ultima facturacion.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("Tiempo"), height = "300px")
      )
    ),
    fluidRow(
      column(12,
             div(style = "margin-top: 20px;",
                 TablaReactableUI(ns("TablaClientes"),
                                  titulo       = "Detalle de Clientes",
                                  footer       = "Clic en el boton Crear para registrar una oportunidad.",
                                  footer_tipo  = "info"
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
DetalleCliente <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filtros reactivos por clic en graficos con patron toggle por categoria
    filtros <- reactiveValues(tipo = NULL, conppto = NULL, meses = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$tipo    <- NULL
      filtros$conppto <- NULL
      filtros$meses   <- NULL
    })
    
    # Datos base procesados
    data_cliente <- reactive({
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      req(dat())
      
      tryCatch({
        # Clasificacion de tipo de cliente vs periodo anterior
        rec <- CargarDatos("CRMNALSEGR") %>%
          mutate(FecProceso = as.Date(FecProceso),
                 across(where(is.numeric),   ~ifelse(is.na(.), 0, .)),
                 across(where(is.character), ~ifelse(is.na(.) | . == "N/A", "", .))) %>%
          filter(FecProceso >= PrimerDia(Sys.Date()) - months(1)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          arrange(FecProceso) %>%
          summarise(Antes = first(SegmentoRacafe),
                    Ahora = last(SegmentoRacafe),
                    .groups = "drop") %>%
          filter(Ahora == "CLIENTE") %>%
          mutate(Tipo = case_when(
            is.na(Antes) & Ahora == "CLIENTE"                   ~ "CLIENTE NUEVO",
            Antes == "CLIENTE A RECUPERAR" & Ahora == "CLIENTE" ~ "CLIENTE RECUPERADO",
            TRUE                                                 ~ "CLIENTE"
          )) %>%
          select(LinNegCod, CliNitPpal, Tipo)
        
        # Ejecucion mes actual y acumulado anual — YTD filtrado explicitamente al year vigente
        eje <- dat() %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(
            SacosMes  = sum(ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), SacFact70, 0),
                            na.rm = TRUE),
            MargenMes = sum(ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Margen, 0),
                            na.rm = TRUE),
            SacosYTD  = sum(ifelse(year(FecFact) == year(Sys.Date()), SacFact70, 0),
                            na.rm = TRUE),
            MargenYTD = sum(ifelse(year(FecFact) == year(Sys.Date()), Margen, 0),
                            na.rm = TRUE),
            .groups = "drop"
          )
        
        # Fechas extremas de facturacion por cliente
        fec <- data %>%
          filter(!is.na(FecFact)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(PrimFact = min(FecFact, na.rm = TRUE),
                    UltFact  = max(FecFact, na.rm = TRUE),
                    .groups = "drop")
        
        mes      <- month(Sys.Date())
        mes_falt <- pmax(12 - mes, 1)
        lbl_acum <- format(Sys.Date(), "%b %Y")
        
        # Consolidacion final con metricas de cumplimiento y proyeccion
        dat() %>%
          filter(SegmentoRacafe == "CLIENTE") %>%
          mutate(
            ConPpto       = ifelse(PptoSacos > 0, "CON PRESUPUESTO", "SIN PRESUPUESTO"),
            PptoSacos     = PptoSacos / 12,
            PptoMargen    = PptoMargen / 12,
            PptoSacosYTD  = PptoSacos * mes,
            PptoMargenYTD = PptoMargen * mes
          ) %>%
          select(LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
                 ConPpto, PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(rec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            Tipo           = ifelse(is.na(Tipo), "CLIENTE NUEVO", Tipo),
            ConPpto        = ifelse(is.na(ConPpto), "SIN PRESUPUESTO", ConPpto),
            Meses          = paste(
              pmax(0, lubridate::interval(UltFact, Sys.Date()) %/% months(1)), "MESES"
            ),
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
            # Cumplimiento mensual: real mes / presupuesto mensual
            CumpSacosMes   = SacosMes  / PptoSacos,
            CumpMargenMes  = MargenMes / PptoMargen,
            # Cumplimiento acumulado: real YTD / presupuesto proporcional a meses transcurridos
            CumpSacosYTD   = SacosYTD  / PptoSacosYTD,
            CumpMargenYTD  = MargenYTD / PptoMargenYTD,
            LblAcum        = lbl_acum,
            Oportunidad    = "Crear"
          )
        
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Toggles de filtro por clic en graficos plotly
    observeEvent(event_data("plotly_click", source = "tipo"), {
      click <- event_data("plotly_click", source = "tipo")
      if (!is.null(click))
        filtros$tipo <- if (!is.null(filtros$tipo) && filtros$tipo == click$x) NULL else click$x
    })
    
    observeEvent(event_data("plotly_click", source = "conppto"), {
      click <- event_data("plotly_click", source = "conppto")
      if (!is.null(click))
        filtros$conppto <- if (
          !is.null(filtros$conppto) && filtros$conppto == click$x
        ) NULL else click$x
    })
    
    observeEvent(event_data("plotly_click", source = "meses"), {
      click <- event_data("plotly_click", source = "meses")
      if (!is.null(click))
        filtros$meses <- if (!is.null(filtros$meses) && filtros$meses == click$x) NULL else click$x
    })
    
    # Datos filtrados por seleccion activa en graficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$tipo))    df <- df %>% filter(Tipo    == filtros$tipo)
      if (!is.null(filtros$conppto)) df <- df %>% filter(ConPpto == filtros$conppto)
      if (!is.null(filtros$meses))   df <- df %>% filter(Meses   == filtros$meses)
      df
    })
    
    # Datos para TablaReactable con fila de totales via bind_rows
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      
      # Totales: suma directa para valores absolutos; porcentajes desde columnas sumadas
      tot_ppto_sac   <- sum(df$PptoSacos,     na.rm = TRUE)
      tot_ppto_mar   <- sum(df$PptoMargen,     na.rm = TRUE)
      tot_ppto_sac_y <- sum(df$PptoSacosYTD,  na.rm = TRUE)
      tot_ppto_mar_y <- sum(df$PptoMargenYTD, na.rm = TRUE)
      tot_sac_mes    <- sum(df$SacosMes,       na.rm = TRUE)
      tot_mar_mes    <- sum(df$MargenMes,       na.rm = TRUE)
      tot_sac_ytd    <- sum(df$SacosYTD,       na.rm = TRUE)
      tot_mar_ytd    <- sum(df$MargenYTD,      na.rm = TRUE)
      
      fila_total <- tibble::tibble(
        Oportunidad    = "",
        PerRazSoc      = "TOTAL",
        CLLinNegNo     = "",
        Tipo           = "",
        ConPpto        = "",
        Segmento       = "",
        UltFact        = NA,
        PptoSacos      = tot_ppto_sac,
        PptoMargen     = tot_ppto_mar,
        SacosMes       = tot_sac_mes,
        MargenMes      = tot_mar_mes,
        CumpSacosMes   = if (tot_ppto_sac  > 0) tot_sac_mes / tot_ppto_sac  else NA_real_,
        CumpMargenMes  = if (tot_ppto_mar  > 0) tot_mar_mes / tot_ppto_mar  else NA_real_,
        PptoSacosYTD   = tot_ppto_sac_y,
        SacosYTD       = tot_sac_ytd,
        CumpSacosYTD   = if (tot_ppto_sac_y > 0) tot_sac_ytd / tot_ppto_sac_y else NA_real_,
        PptoMargenYTD  = tot_ppto_mar_y,
        MargenYTD      = tot_mar_ytd,
        CumpMargenYTD  = if (tot_ppto_mar_y > 0) tot_mar_ytd / tot_ppto_mar_y else NA_real_,
        SacosCumpPpto  = sum(df$SacosCumpPpto,  na.rm = TRUE),
        MargenCumpPpto = sum(df$MargenCumpPpto,  na.rm = TRUE),
        LblAcum        = ""
      )
      
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
    
    # Patron eager: FormularioOportunidad registrado antes de TablaReactable
    dd_oportunidad_rv <- reactiveVal(NULL)
    
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE")
    )
    
    # Graficos de distribucion con toggle de filtro activo
    crear_grafico <- function(columna, source_name) {
      df <- data_filtrada()
      if (nrow(df) == 0) {
        return(plotly_empty(type = "bar") %>%
                 layout(title = list(text = "Sin datos disponibles", font = list(size = 14))))
      }
      
      filtro_activo <- switch(source_name,
                              tipo    = filtros$tipo,
                              conppto = filtros$conppto,
                              meses   = filtros$meses,
                              NULL
      )
      
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
        layout(xaxis = list(title = "", tickangle = 0, tickfont = list(size = 12)),
               yaxis = list(title = "Cantidad de Clientes", range = c(0, ymax))) %>%
        event_register("plotly_click") %>%
        config(displayModeBar = FALSE)
    }
    
    output$TipoCliente   <- renderPlotly({ crear_grafico("Tipo",    "tipo")    })
    output$Presupuestado <- renderPlotly({ crear_grafico("ConPpto", "conppto") })
    output$Tiempo        <- renderPlotly({ crear_grafico("Meses",   "meses")   })
    
    # Tabla reactable; seleccion restringida al boton Oportunidad via cols_activos
    TablaReactable(
      id             = "TablaClientes",
      data           = data_tabla,
      modo_seleccion = "celda",
      id_col         = NULL,
      col_header_n   = 2L,
      cols_activos   = "Oportunidad",
      sortable       = TRUE,
      searchable     = TRUE,
      page_size      = 15,
      compact        = TRUE,
      mostrar_badge  = FALSE,
      mostrar_nota   = FALSE,
      modal_icon     = "hand-holding-dollar",
      modal_size     = "xl",
      modal_titulo_fn = function(sel) {
        paste0("Crear Oportunidad — ", as.character(sel$fila$PerRazSoc[[1]]))
      },
      modal_pre_fn = function(sel) {
        dd_oportunidad_rv(list(
          data   = data_filtrada() %>%
            filter(PerRazSoc  == as.character(sel$fila$PerRazSoc[[1]]),
                   CLLinNegNo == as.character(sel$fila$CLLinNegNo[[1]])),
          accion = "oportunidad"
        ))
      },
      modal_contenido_fn = function(sel) FormularioOportunidadUI(ns("mod_formulario")),
      columnas = list(
        Oportunidad = reactable::colDef(
          name     = "",
          minWidth = 55,
          html     = TRUE,
          cell     = function(v) {
            if (v == "") return("")
            as.character(tags$span(
              style = paste("display:inline-flex; align-items:center; justify-content:center;",
                            "width:28px; height:28px; border-radius:6px;",
                            "background:#C11007; color:white; font-size:13px; cursor:pointer;"),
              icon("hand-holding-dollar")
            ))
          }
        ),
        PerRazSoc = reactable::colDef(
          name     = "Cliente",
          minWidth = 200,
          html     = TRUE,
          cell     = function(v) as.character(htmltools::HTML(v))
        ),
        CLLinNegNo     = reactable::colDef(name = "Linea de Negocio",  minWidth = 140),
        Tipo           = reactable::colDef(name = "Tipo",              minWidth = 140,
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
        ),
        ConPpto        = reactable::colDef(name = "Presupuestado",    minWidth = 130),
        Segmento       = reactable::colDef(name = "Segmento",         minWidth = 110),
        UltFact        = reactable::colDef(name = "Ult. Facturacion", minWidth = 120,
                                           cell = function(v) if (is.na(v)) "—" else format(as.Date(v), "%d/%m/%Y")
        ),
        PptoSacos      = reactable::colDef(name = "Ppto Sacos Mes",    minWidth = 120,
                                           cell = function(v) if (is.na(v)) "—" else format(round(v), big.mark = ",")
        ),
        SacosMes       = reactable::colDef(name = "Sacos Mes",          minWidth = 100,
                                           cell = function(v) format(round(v), big.mark = ",")
        ),
        CumpSacosMes   = reactable::colDef(name = "% Cumpl Sacos Mes",  minWidth = 130,
                                           cell  = function(v) {
                                             if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%")
                                           },
                                           style = function(v) {
                                             if (is.na(v) || is.infinite(v)) return(NULL)
                                             list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                                                  fontWeight = "600")
                                           }
        ),
        PptoMargen     = reactable::colDef(name = "Ppto Margen Mes",    minWidth = 130,
                                           cell = function(v) if (is.na(v)) "—" else paste0("$", format(round(v), big.mark = ","))
        ),
        MargenMes      = reactable::colDef(name = "Margen Mes",         minWidth = 120,
                                           cell = function(v) paste0("$", format(round(v), big.mark = ","))
        ),
        CumpMargenMes  = reactable::colDef(name = "% Cumpl Margen Mes", minWidth = 140,
                                           cell  = function(v) {
                                             if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%")
                                           },
                                           style = function(v) {
                                             if (is.na(v) || is.infinite(v)) return(NULL)
                                             list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                                                  fontWeight = "600")
                                           }
        ),
        # Bloque acumulado: presupuesto, ejecucion y cumplimiento con periodo dinamico
        LblAcum        = reactable::colDef(show = FALSE),
        PptoSacosYTD   = reactable::colDef(
          minWidth = 140,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Ppto Sacos Acum. ", lbl)
          },
          cell = function(v) if (is.na(v)) "—" else format(round(v), big.mark = ",")
        ),
        SacosYTD       = reactable::colDef(
          minWidth = 130,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Sacos Acum. ", lbl)
          },
          cell = function(v) format(round(v), big.mark = ",")
        ),
        CumpSacosYTD   = reactable::colDef(
          minWidth = 150,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("% Cumpl Sacos Acum. ", lbl)
          },
          cell  = function(v) {
            if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%")
          },
          style = function(v) {
            if (is.na(v) || is.infinite(v)) return(NULL)
            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                 fontWeight = "600")
          }
        ),
        PptoMargenYTD  = reactable::colDef(
          minWidth = 150,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Ppto Margen Acum. ", lbl)
          },
          cell = function(v) if (is.na(v)) "—" else paste0("$", format(round(v), big.mark = ","))
        ),
        MargenYTD      = reactable::colDef(
          minWidth = 140,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Margen Acum. ", lbl)
          },
          cell = function(v) paste0("$", format(round(v), big.mark = ","))
        ),
        CumpMargenYTD  = reactable::colDef(
          minWidth = 160,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("% Cumpl Margen Acum. ", lbl)
          },
          cell  = function(v) {
            if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%")
          },
          style = function(v) {
            if (is.na(v) || is.infinite(v)) return(NULL)
            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                 fontWeight = "600")
          }
        ),
        # Bloque proyeccion anual
        SacosCumpPpto  = reactable::colDef(name = "Sacos proy. cumplir Ppto",  minWidth = 180,
                                           cell = function(v) format(round(v), big.mark = ",")
        ),
        MargenCumpPpto = reactable::colDef(name = "Margen proy. cumplir Ppto", minWidth = 190,
                                           cell = function(v) paste0("$", format(round(v), big.mark = ","))
        )
      )
    )
    
    # Descarga en Excel de datos filtrados sin fila de totales
    output$Descargar <- downloadHandler(
      filename = function() paste0("clientes_detalle_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        df <- data_filtrada()
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
    
  })
}

# Clientes a Recuperar -----
DetalleClienteRecuperarUI <- function(id) {
  ns <- NS(id)
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
    fluidRow(
      column(4,
             h5("Tipo de Cliente a Recuperar"),
             p("Nuevos A Recuperar: clientes que a corte del mes en curso pasaron a este segmento.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("TipoCliente"), height = "300px")
      ),
      column(4,
             h5("Presupuestado"),
             p("Clientes con presupuesto de sacos mayor a cero para el ano vigente.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("Presupuestado"), height = "300px")
      ),
      column(4,
             h5("Meses sin Facturar"),
             p("Clientes agrupados por meses transcurridos desde su ultima facturacion.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("Tiempo"), height = "300px")
      )
    ),
    fluidRow(
      column(12,
             div(style = "margin-top: 20px;",
                 TablaReactableUI(ns("TablaClientes"),
                                  titulo       = "Detalle de Clientes a Recuperar",
                                  footer       = "Clic en el boton Crear para registrar una oportunidad.",
                                  footer_tipo  = "info",
                                  mostrar_nota = FALSE
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
DetalleClienteRecuperar <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filtros reactivos por clic en graficos con patron toggle por categoria
    filtros <- reactiveValues(tipo = NULL, conppto = NULL, meses = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$tipo    <- NULL
      filtros$conppto <- NULL
      filtros$meses   <- NULL
    })
    
    # Datos base procesados
    data_cliente <- reactive({
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      req(dat())
      
      tryCatch({
        # Clasificacion de tipo: nuevo a recuperar vs ya estaba en el segmento
        rec <- CargarDatos("CRMNALSEGR") %>%
          mutate(FecProceso = as.Date(FecProceso),
                 across(where(is.numeric),   ~ifelse(is.na(.), 0, .)),
                 across(where(is.character), ~ifelse(is.na(.) | . == "N/A", "", .))) %>%
          filter(FecProceso >= PrimerDia(Sys.Date()) - months(1)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          arrange(FecProceso) %>%
          summarise(Antes = first(SegmentoRacafe),
                    Ahora = last(SegmentoRacafe),
                    .groups = "drop") %>%
          filter(Ahora == "CLIENTE A RECUPERAR") %>%
          mutate(Tipo = case_when(
            Antes == "CLIENTE" ~ "NUEVO CLIENTE A RECUPERAR",
            TRUE               ~ "CLIENTE A RECUPERAR"
          )) %>%
          select(LinNegCod, CliNitPpal, Tipo)
        
        # Ejecucion mes actual y acumulado anual — YTD filtrado al year vigente
        eje <- dat() %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(
            SacosMes  = sum(ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), SacFact70, 0),
                            na.rm = TRUE),
            MargenMes = sum(ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Margen, 0),
                            na.rm = TRUE),
            SacosYTD  = sum(ifelse(year(FecFact) == year(Sys.Date()), SacFact70, 0),
                            na.rm = TRUE),
            MargenYTD = sum(ifelse(year(FecFact) == year(Sys.Date()), Margen, 0),
                            na.rm = TRUE),
            .groups = "drop"
          )
        
        # Fechas extremas de facturacion por cliente
        fec <- data %>%
          filter(!is.na(FecFact)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(PrimFact = min(FecFact, na.rm = TRUE),
                    UltFact  = max(FecFact, na.rm = TRUE),
                    .groups = "drop")
        
        mes      <- month(Sys.Date())
        mes_falt <- pmax(12 - mes, 1)
        lbl_acum <- format(Sys.Date(), "%b %Y")
        
        # Consolidacion final con metricas de cumplimiento y proyeccion
        dat() %>%
          filter(SegmentoRacafe == "CLIENTE A RECUPERAR") %>%
          mutate(
            ConPpto       = ifelse(PptoSacos > 0, "CON PRESUPUESTO", "SIN PRESUPUESTO"),
            PptoSacos     = PptoSacos / 12,
            PptoMargen    = PptoMargen / 12,
            PptoSacosYTD  = PptoSacos * mes,
            PptoMargenYTD = PptoMargen * mes
          ) %>%
          select(LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
                 ConPpto, PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(rec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            Tipo    = ifelse(is.na(Tipo), "CLIENTE A RECUPERAR", Tipo),
            ConPpto = ifelse(is.na(ConPpto), "SIN PRESUPUESTO", ConPpto),
            Meses   = {
              m <- pmax(0, lubridate::interval(UltFact, Sys.Date()) %/% months(1))
              dplyr::case_when(
                m <= 3 ~ "Hasta 3 meses",
                m <= 6 ~ "De 4 a 6 meses",
                TRUE   ~ "Mas de 6 meses"
              )
            },
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
            # Cumplimiento mensual: real mes / presupuesto mensual
            CumpSacosMes   = SacosMes  / PptoSacos,
            CumpMargenMes  = MargenMes / PptoMargen,
            # Cumplimiento acumulado: real YTD / presupuesto proporcional a meses transcurridos
            CumpSacosYTD   = SacosYTD  / PptoSacosYTD,
            CumpMargenYTD  = MargenYTD / PptoMargenYTD,
            LblAcum        = lbl_acum,
            Oportunidad    = "Crear"
          )
        
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Toggles de filtro por clic en graficos plotly
    observeEvent(event_data("plotly_click", source = "tipo"), {
      click <- event_data("plotly_click", source = "tipo")
      if (!is.null(click))
        filtros$tipo <- if (!is.null(filtros$tipo) && filtros$tipo == click$x) NULL else click$x
    })
    
    observeEvent(event_data("plotly_click", source = "conppto"), {
      click <- event_data("plotly_click", source = "conppto")
      if (!is.null(click))
        filtros$conppto <- if (
          !is.null(filtros$conppto) && filtros$conppto == click$x
        ) NULL else click$x
    })
    
    observeEvent(event_data("plotly_click", source = "meses"), {
      click <- event_data("plotly_click", source = "meses")
      if (!is.null(click))
        filtros$meses <- if (!is.null(filtros$meses) && filtros$meses == click$x) NULL else click$x
    })
    
    # Datos filtrados por seleccion activa en graficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$tipo))    df <- df %>% filter(Tipo    == filtros$tipo)
      if (!is.null(filtros$conppto)) df <- df %>% filter(ConPpto == filtros$conppto)
      if (!is.null(filtros$meses))   df <- df %>% filter(Meses   == filtros$meses)
      df
    })
    
    # Datos para TablaReactable con fila de totales via bind_rows
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      
      # Totales: suma directa para valores absolutos; porcentajes desde columnas sumadas
      tot_ppto_sac   <- sum(df$PptoSacos,     na.rm = TRUE)
      tot_ppto_mar   <- sum(df$PptoMargen,     na.rm = TRUE)
      tot_ppto_sac_y <- sum(df$PptoSacosYTD,  na.rm = TRUE)
      tot_ppto_mar_y <- sum(df$PptoMargenYTD, na.rm = TRUE)
      tot_sac_mes    <- sum(df$SacosMes,       na.rm = TRUE)
      tot_mar_mes    <- sum(df$MargenMes,       na.rm = TRUE)
      tot_sac_ytd    <- sum(df$SacosYTD,       na.rm = TRUE)
      tot_mar_ytd    <- sum(df$MargenYTD,      na.rm = TRUE)
      
      fila_total <- tibble::tibble(
        Oportunidad    = "",
        PerRazSoc      = "TOTAL",
        CLLinNegNo     = "",
        Tipo           = "",
        ConPpto        = "",
        Segmento       = "",
        UltFact        = NA,
        PptoSacos      = tot_ppto_sac,
        SacosMes       = tot_sac_mes,
        CumpSacosMes   = if (tot_ppto_sac  > 0) tot_sac_mes / tot_ppto_sac  else NA_real_,
        PptoMargen     = tot_ppto_mar,
        MargenMes      = tot_mar_mes,
        CumpMargenMes  = if (tot_ppto_mar  > 0) tot_mar_mes / tot_ppto_mar  else NA_real_,
        PptoSacosYTD   = tot_ppto_sac_y,
        SacosYTD       = tot_sac_ytd,
        CumpSacosYTD   = if (tot_ppto_sac_y > 0) tot_sac_ytd / tot_ppto_sac_y else NA_real_,
        PptoMargenYTD  = tot_ppto_mar_y,
        MargenYTD      = tot_mar_ytd,
        CumpMargenYTD  = if (tot_ppto_mar_y > 0) tot_mar_ytd / tot_ppto_mar_y else NA_real_,
        SacosCumpPpto  = sum(df$SacosCumpPpto,  na.rm = TRUE),
        MargenCumpPpto = sum(df$MargenCumpPpto,  na.rm = TRUE),
        LblAcum        = ""
      )
      
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
    
    # Patron eager: FormularioOportunidad registrado antes de TablaReactable
    dd_oportunidad_rv <- reactiveVal(NULL)
    
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE A RECUPERAR")
    )
    
    # Graficos de distribucion con toggle de filtro activo
    crear_grafico <- function(columna, source_name) {
      df <- data_filtrada()
      if (nrow(df) == 0) {
        return(plotly_empty(type = "bar") %>%
                 layout(title = list(text = "Sin datos disponibles", font = list(size = 14))))
      }
      
      filtro_activo <- switch(source_name,
                              tipo    = filtros$tipo,
                              conppto = filtros$conppto,
                              meses   = filtros$meses,
                              NULL
      )
      
      # Para la columna Meses aplicar orden de rangos temporales
      if (columna == "Meses") {
        df <- df %>%
          mutate(Meses = factor(Meses,
                                levels = c("Hasta 3 meses", "De 4 a 6 meses", "Mas de 6 meses"),
                                ordered = TRUE
          ))
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
        layout(xaxis = list(title = "", tickangle = 0, tickfont = list(size = 12)),
               yaxis = list(title = "Cantidad de Clientes", range = c(0, ymax))) %>%
        event_register("plotly_click") %>%
        config(displayModeBar = FALSE)
    }
    
    output$TipoCliente   <- renderPlotly({ crear_grafico("Tipo",    "tipo")    })
    output$Presupuestado <- renderPlotly({ crear_grafico("ConPpto", "conppto") })
    output$Tiempo        <- renderPlotly({ crear_grafico("Meses",   "meses")   })
    
    # Tabla reactable; seleccion restringida al boton Oportunidad via cols_activos
    TablaReactable(
      id             = "TablaClientes",
      data           = data_tabla,
      modo_seleccion = "celda",
      id_col         = NULL,
      col_header_n   = 2L,
      cols_activos   = "Oportunidad",
      sortable       = TRUE,
      searchable     = TRUE,
      page_size      = 15,
      compact        = TRUE,
      mostrar_badge  = FALSE,
      mostrar_nota   = FALSE,
      modal_icon     = "hand-holding-dollar",
      modal_size     = "xl",
      modal_titulo_fn = function(sel) {
        paste0("Crear Oportunidad — ", as.character(sel$fila$PerRazSoc[[1]]))
      },
      modal_pre_fn = function(sel) {
        dd_oportunidad_rv(list(
          data   = data_filtrada() %>%
            filter(PerRazSoc  == as.character(sel$fila$PerRazSoc[[1]]),
                   CLLinNegNo == as.character(sel$fila$CLLinNegNo[[1]])),
          accion = "oportunidad"
        ))
      },
      modal_contenido_fn = function(sel) FormularioOportunidadUI(ns("mod_formulario")),
      columnas = list(
        Oportunidad = reactable::colDef(
          name     = "",
          minWidth = 55,
          html     = TRUE,
          cell     = function(v) {
            if (v == "") return("")
            as.character(tags$span(
              style = paste("display:inline-flex; align-items:center; justify-content:center;",
                            "width:28px; height:28px; border-radius:6px;",
                            "background:#C11007; color:white; font-size:13px; cursor:pointer;"),
              icon("hand-holding-dollar")
            ))
          }
        ),
        PerRazSoc = reactable::colDef(
          name     = "Cliente",
          minWidth = 200,
          html     = TRUE,
          cell     = function(v) as.character(htmltools::HTML(v))
        ),
        CLLinNegNo = reactable::colDef(name = "Linea de Negocio",  minWidth = 140),
        Tipo       = reactable::colDef(name = "Tipo",              minWidth = 180,
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
        ),
        ConPpto  = reactable::colDef(name = "Presupuestado",    minWidth = 130),
        Segmento = reactable::colDef(name = "Segmento",         minWidth = 110),
        UltFact  = reactable::colDef(name = "Ult. Facturacion", minWidth = 120,
                                     cell = function(v) if (is.na(v)) "—" else format(as.Date(v), "%d/%m/%Y")
        ),
        # Bloque mensual: presupuesto, ejecucion y cumplimiento
        PptoSacos    = reactable::colDef(name = "Ppto Sacos Mes",    minWidth = 120,
                                         cell = function(v) if (is.na(v)) "—" else format(round(v), big.mark = ",")
        ),
        SacosMes     = reactable::colDef(name = "Sacos Mes",         minWidth = 100,
                                         cell = function(v) format(round(v), big.mark = ",")
        ),
        CumpSacosMes = reactable::colDef(name = "% Cumpl Sacos Mes", minWidth = 130,
                                         cell  = function(v) {
                                           if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%")
                                         },
                                         style = function(v) {
                                           if (is.na(v) || is.infinite(v)) return(NULL)
                                           list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                                                fontWeight = "600")
                                         }
        ),
        PptoMargen    = reactable::colDef(name = "Ppto Margen Mes",    minWidth = 130,
                                          cell = function(v) if (is.na(v)) "—" else paste0("$", format(round(v), big.mark = ","))
        ),
        MargenMes     = reactable::colDef(name = "Margen Mes",         minWidth = 120,
                                          cell = function(v) paste0("$", format(round(v), big.mark = ","))
        ),
        CumpMargenMes = reactable::colDef(name = "% Cumpl Margen Mes", minWidth = 140,
                                          cell  = function(v) {
                                            if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%")
                                          },
                                          style = function(v) {
                                            if (is.na(v) || is.infinite(v)) return(NULL)
                                            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                                                 fontWeight = "600")
                                          }
        ),
        # Bloque acumulado: presupuesto, ejecucion y cumplimiento con periodo dinamico
        LblAcum      = reactable::colDef(show = FALSE),
        PptoSacosYTD = reactable::colDef(
          minWidth = 140,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Ppto Sacos Acum. ", lbl)
          },
          cell = function(v) if (is.na(v)) "—" else format(round(v), big.mark = ",")
        ),
        SacosYTD     = reactable::colDef(
          minWidth = 130,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Sacos Acum. ", lbl)
          },
          cell = function(v) format(round(v), big.mark = ",")
        ),
        CumpSacosYTD = reactable::colDef(
          minWidth = 150,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("% Cumpl Sacos Acum. ", lbl)
          },
          cell  = function(v) {
            if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%")
          },
          style = function(v) {
            if (is.na(v) || is.infinite(v)) return(NULL)
            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                 fontWeight = "600")
          }
        ),
        PptoMargenYTD = reactable::colDef(
          minWidth = 150,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Ppto Margen Acum. ", lbl)
          },
          cell = function(v) if (is.na(v)) "—" else paste0("$", format(round(v), big.mark = ","))
        ),
        MargenYTD    = reactable::colDef(
          minWidth = 140,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Margen Acum. ", lbl)
          },
          cell = function(v) paste0("$", format(round(v), big.mark = ","))
        ),
        CumpMargenYTD = reactable::colDef(
          minWidth = 160,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("% Cumpl Margen Acum. ", lbl)
          },
          cell  = function(v) {
            if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%")
          },
          style = function(v) {
            if (is.na(v) || is.infinite(v)) return(NULL)
            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                 fontWeight = "600")
          }
        ),
        # Bloque proyeccion anual
        SacosCumpPpto  = reactable::colDef(name = "Sacos proy. cumplir Ppto",  minWidth = 180,
                                           cell = function(v) format(round(v), big.mark = ",")
        ),
        MargenCumpPpto = reactable::colDef(name = "Margen proy. cumplir Ppto", minWidth = 190,
                                           cell = function(v) paste0("$", format(round(v), big.mark = ","))
        )
      )
    )
    
    # Descarga en Excel de datos filtrados sin fila de totales
    output$Descargar <- downloadHandler(
      filename = function() paste0("clientes_recuperar_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        df <- data_filtrada()
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
    
  })
}

# Clientes Recuperados -----
DetalleClienteRecuperadoUI <- function(id) {
  ns <- NS(id)
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
    fluidRow(
      column(4,
             h5("Segmento"),
             p("Distribucion de clientes recuperados del mes por segmento de negocio.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("Segmento"), height = "300px")
      ),
      column(4,
             h5("Presupuestado"),
             p("Clientes con presupuesto de sacos mayor a cero para el ano vigente.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("Presupuestado"), height = "300px")
      ),
      column(4,
             h5("Meses desde Recuperacion"),
             p("Clientes agrupados por meses transcurridos desde que volvieron a facturar.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("Tiempo"), height = "300px")
      )
    ),
    fluidRow(
      column(12,
             div(style = "margin-top: 20px;",
                 TablaReactableUI(ns("TablaClientes"),
                                  titulo       = "Detalle de Clientes Recuperados",
                                  footer       = "Clic en el boton Crear para registrar una oportunidad.",
                                  footer_tipo  = "info",
                                  mostrar_nota = FALSE
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
DetalleClienteRecuperado <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filtros reactivos por clic en graficos con patron toggle por categoria
    filtros <- reactiveValues(segmento = NULL, conppto = NULL, meses = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$segmento <- NULL
      filtros$conppto  <- NULL
      filtros$meses    <- NULL
    })
    
    # Datos base procesados
    data_cliente <- reactive({
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      req(dat())
      
      tryCatch({
        # Clientes recuperados del mes: venian de CLIENTE A RECUPERAR y ahora son CLIENTE
        rec <- CargarDatos("CRMNALSEGR") %>%
          mutate(FecProceso = as.Date(FecProceso),
                 across(where(is.numeric),   ~ifelse(is.na(.), 0, .)),
                 across(where(is.character), ~ifelse(is.na(.) | . == "N/A", "", .))) %>%
          filter(FecProceso >= PrimerDia(Sys.Date()) - months(1)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          arrange(FecProceso) %>%
          summarise(Antes = first(SegmentoRacafe),
                    Ahora = last(SegmentoRacafe),
                    .groups = "drop") %>%
          filter(Ahora == "CLIENTE", Antes == "CLIENTE A RECUPERAR") %>%
          select(LinNegCod, CliNitPpal)
        
        # Ejecucion mes actual y acumulado anual — YTD filtrado al year vigente
        eje <- dat() %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(
            SacosMes  = sum(ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), SacFact70, 0),
                            na.rm = TRUE),
            MargenMes = sum(ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Margen, 0),
                            na.rm = TRUE),
            SacosYTD  = sum(ifelse(year(FecFact) == year(Sys.Date()), SacFact70, 0),
                            na.rm = TRUE),
            MargenYTD = sum(ifelse(year(FecFact) == year(Sys.Date()), Margen, 0),
                            na.rm = TRUE),
            .groups = "drop"
          )
        
        # Fechas extremas de facturacion por cliente
        fec <- data %>%
          filter(!is.na(FecFact)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(PrimFact = min(FecFact, na.rm = TRUE),
                    UltFact  = max(FecFact, na.rm = TRUE),
                    .groups = "drop")
        
        mes      <- month(Sys.Date())
        mes_falt <- pmax(12 - mes, 1)
        lbl_acum <- format(Sys.Date(), "%b %Y")
        
        # Consolidacion: solo clientes identificados como recuperados del mes
        dat() %>%
          filter(SegmentoRacafe == "CLIENTE") %>%
          semi_join(rec, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            ConPpto       = ifelse(PptoSacos > 0, "CON PRESUPUESTO", "SIN PRESUPUESTO"),
            PptoSacos     = PptoSacos / 12,
            PptoMargen    = PptoMargen / 12,
            PptoSacosYTD  = PptoSacos * mes,
            PptoMargenYTD = PptoMargen * mes
          ) %>%
          select(LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
                 ConPpto, PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            ConPpto        = ifelse(is.na(ConPpto), "SIN PRESUPUESTO", ConPpto),
            Meses          = paste(
              pmax(0, lubridate::interval(UltFact, Sys.Date()) %/% months(1)), "MESES"
            ),
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
        
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Toggles de filtro por clic en graficos plotly
    observeEvent(event_data("plotly_click", source = "segmento"), {
      click <- event_data("plotly_click", source = "segmento")
      if (!is.null(click))
        filtros$segmento <- if (
          !is.null(filtros$segmento) && filtros$segmento == click$x
        ) NULL else click$x
    })
    
    observeEvent(event_data("plotly_click", source = "conppto"), {
      click <- event_data("plotly_click", source = "conppto")
      if (!is.null(click))
        filtros$conppto <- if (
          !is.null(filtros$conppto) && filtros$conppto == click$x
        ) NULL else click$x
    })
    
    observeEvent(event_data("plotly_click", source = "meses"), {
      click <- event_data("plotly_click", source = "meses")
      if (!is.null(click))
        filtros$meses <- if (!is.null(filtros$meses) && filtros$meses == click$x) NULL else click$x
    })
    
    # Datos filtrados por seleccion activa en graficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$segmento)) df <- df %>% filter(Segmento == filtros$segmento)
      if (!is.null(filtros$conppto))  df <- df %>% filter(ConPpto  == filtros$conppto)
      if (!is.null(filtros$meses))    df <- df %>% filter(Meses    == filtros$meses)
      df
    })
    
    # Datos para TablaReactable con fila de totales via bind_rows
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      
      tot_ppto_sac   <- sum(df$PptoSacos,     na.rm = TRUE)
      tot_ppto_mar   <- sum(df$PptoMargen,     na.rm = TRUE)
      tot_ppto_sac_y <- sum(df$PptoSacosYTD,  na.rm = TRUE)
      tot_ppto_mar_y <- sum(df$PptoMargenYTD, na.rm = TRUE)
      tot_sac_mes    <- sum(df$SacosMes,       na.rm = TRUE)
      tot_mar_mes    <- sum(df$MargenMes,       na.rm = TRUE)
      tot_sac_ytd    <- sum(df$SacosYTD,       na.rm = TRUE)
      tot_mar_ytd    <- sum(df$MargenYTD,      na.rm = TRUE)
      
      fila_total <- tibble::tibble(
        Oportunidad    = "",
        PerRazSoc      = "TOTAL",
        CLLinNegNo     = "",
        Segmento       = "",
        ConPpto        = "",
        UltFact        = NA,
        PptoSacos      = tot_ppto_sac,
        SacosMes       = tot_sac_mes,
        CumpSacosMes   = if (tot_ppto_sac  > 0) tot_sac_mes / tot_ppto_sac  else NA_real_,
        PptoMargen     = tot_ppto_mar,
        MargenMes      = tot_mar_mes,
        CumpMargenMes  = if (tot_ppto_mar  > 0) tot_mar_mes / tot_ppto_mar  else NA_real_,
        PptoSacosYTD   = tot_ppto_sac_y,
        SacosYTD       = tot_sac_ytd,
        CumpSacosYTD   = if (tot_ppto_sac_y > 0) tot_sac_ytd / tot_ppto_sac_y else NA_real_,
        PptoMargenYTD  = tot_ppto_mar_y,
        MargenYTD      = tot_mar_ytd,
        CumpMargenYTD  = if (tot_ppto_mar_y > 0) tot_mar_ytd / tot_ppto_mar_y else NA_real_,
        SacosCumpPpto  = sum(df$SacosCumpPpto,  na.rm = TRUE),
        MargenCumpPpto = sum(df$MargenCumpPpto,  na.rm = TRUE),
        LblAcum        = ""
      )
      
      df %>%
        crear_link_cliente(col_razsoc = "PerRazSoc", col_linneg = "CLLinNegNo") %>%
        select(Oportunidad, PerRazSoc, CLLinNegNo, Segmento, ConPpto, UltFact,
               PptoSacos, SacosMes, CumpSacosMes,
               PptoMargen, MargenMes, CumpMargenMes,
               PptoSacosYTD, SacosYTD, CumpSacosYTD,
               PptoMargenYTD, MargenYTD, CumpMargenYTD,
               SacosCumpPpto, MargenCumpPpto, LblAcum) %>%
        bind_rows(fila_total)
    })
    
    # Patron eager: FormularioOportunidad registrado antes de TablaReactable
    dd_oportunidad_rv <- reactiveVal(NULL)
    
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE")
    )
    
    # Graficos de distribucion con toggle de filtro activo
    crear_grafico <- function(columna, source_name) {
      df <- data_filtrada()
      if (nrow(df) == 0) {
        return(plotly_empty(type = "bar") %>%
                 layout(title = list(text = "Sin datos disponibles", font = list(size = 14))))
      }
      
      filtro_activo <- switch(source_name,
                              segmento = filtros$segmento,
                              conppto  = filtros$conppto,
                              meses    = filtros$meses,
                              NULL
      )
      
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
        layout(xaxis = list(title = "", tickangle = 0, tickfont = list(size = 12)),
               yaxis = list(title = "Cantidad de Clientes", range = c(0, ymax))) %>%
        event_register("plotly_click") %>%
        config(displayModeBar = FALSE)
    }
    
    output$Segmento      <- renderPlotly({ crear_grafico("Segmento", "segmento") })
    output$Presupuestado <- renderPlotly({ crear_grafico("ConPpto",  "conppto")  })
    output$Tiempo        <- renderPlotly({ crear_grafico("Meses",    "meses")    })
    
    # Tabla reactable; seleccion restringida al boton Oportunidad via cols_activos
    TablaReactable(
      id             = "TablaClientes",
      data           = data_tabla,
      modo_seleccion = "celda",
      id_col         = NULL,
      col_header_n   = 2L,
      cols_activos   = "Oportunidad",
      sortable       = TRUE,
      searchable     = TRUE,
      page_size      = 15,
      compact        = TRUE,
      mostrar_badge  = FALSE,
      mostrar_nota   = FALSE,
      modal_icon     = "hand-holding-dollar",
      modal_size     = "xl",
      modal_titulo_fn = function(sel) {
        paste0("Crear Oportunidad — ", as.character(sel$fila$PerRazSoc[[1]]))
      },
      modal_pre_fn = function(sel) {
        dd_oportunidad_rv(list(
          data   = data_filtrada() %>%
            filter(PerRazSoc  == as.character(sel$fila$PerRazSoc[[1]]),
                   CLLinNegNo == as.character(sel$fila$CLLinNegNo[[1]])),
          accion = "oportunidad"
        ))
      },
      modal_contenido_fn = function(sel) FormularioOportunidadUI(ns("mod_formulario")),
      columnas = list(
        Oportunidad = reactable::colDef(
          name     = "",
          minWidth = 55,
          html     = TRUE,
          cell     = function(v) {
            if (v == "") return("")
            as.character(tags$span(
              style = paste("display:inline-flex; align-items:center; justify-content:center;",
                            "width:28px; height:28px; border-radius:6px;",
                            "background:#C11007; color:white; font-size:13px; cursor:pointer;"),
              icon("hand-holding-dollar")
            ))
          }
        ),
        PerRazSoc = reactable::colDef(
          name     = "Cliente",
          minWidth = 200,
          html     = TRUE,
          cell     = function(v) as.character(htmltools::HTML(v))
        ),
        CLLinNegNo = reactable::colDef(name = "Linea de Negocio", minWidth = 140),
        Segmento   = reactable::colDef(name = "Segmento",         minWidth = 110,
                                       style = function(v) list(background = "#FFF8EC", color = "#784212", fontWeight = "600")
        ),
        ConPpto  = reactable::colDef(name = "Presupuestado",    minWidth = 130),
        UltFact  = reactable::colDef(name = "Ult. Facturacion", minWidth = 120,
                                     cell = function(v) if (is.na(v)) "—" else format(as.Date(v), "%d/%m/%Y")
        ),
        # Bloque mensual
        PptoSacos    = reactable::colDef(name = "Ppto Sacos Mes",    minWidth = 120,
                                         cell = function(v) if (is.na(v)) "—" else format(round(v), big.mark = ",")
        ),
        SacosMes     = reactable::colDef(name = "Sacos Mes",         minWidth = 100,
                                         cell = function(v) format(round(v), big.mark = ",")
        ),
        CumpSacosMes = reactable::colDef(name = "% Cumpl Sacos Mes", minWidth = 130,
                                         cell  = function(v) if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%"),
                                         style = function(v) {
                                           if (is.na(v) || is.infinite(v)) return(NULL)
                                           list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                                                fontWeight = "600")
                                         }
        ),
        PptoMargen    = reactable::colDef(name = "Ppto Margen Mes",    minWidth = 130,
                                          cell = function(v) if (is.na(v)) "—" else paste0("$", format(round(v), big.mark = ","))
        ),
        MargenMes     = reactable::colDef(name = "Margen Mes",         minWidth = 120,
                                          cell = function(v) paste0("$", format(round(v), big.mark = ","))
        ),
        CumpMargenMes = reactable::colDef(name = "% Cumpl Margen Mes", minWidth = 140,
                                          cell  = function(v) if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%"),
                                          style = function(v) {
                                            if (is.na(v) || is.infinite(v)) return(NULL)
                                            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                                                 fontWeight = "600")
                                          }
        ),
        # Bloque acumulado con periodo dinamico
        LblAcum      = reactable::colDef(show = FALSE),
        PptoSacosYTD = reactable::colDef(
          minWidth = 140,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Ppto Sacos Acum. ", lbl)
          },
          cell = function(v) if (is.na(v)) "—" else format(round(v), big.mark = ",")
        ),
        SacosYTD     = reactable::colDef(
          minWidth = 130,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Sacos Acum. ", lbl)
          },
          cell = function(v) format(round(v), big.mark = ",")
        ),
        CumpSacosYTD = reactable::colDef(
          minWidth = 150,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("% Cumpl Sacos Acum. ", lbl)
          },
          cell  = function(v) if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%"),
          style = function(v) {
            if (is.na(v) || is.infinite(v)) return(NULL)
            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                 fontWeight = "600")
          }
        ),
        PptoMargenYTD = reactable::colDef(
          minWidth = 150,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Ppto Margen Acum. ", lbl)
          },
          cell = function(v) if (is.na(v)) "—" else paste0("$", format(round(v), big.mark = ","))
        ),
        MargenYTD    = reactable::colDef(
          minWidth = 140,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Margen Acum. ", lbl)
          },
          cell = function(v) paste0("$", format(round(v), big.mark = ","))
        ),
        CumpMargenYTD = reactable::colDef(
          minWidth = 160,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("% Cumpl Margen Acum. ", lbl)
          },
          cell  = function(v) if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%"),
          style = function(v) {
            if (is.na(v) || is.infinite(v)) return(NULL)
            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                 fontWeight = "600")
          }
        ),
        # Bloque proyeccion anual
        SacosCumpPpto  = reactable::colDef(name = "Sacos proy. cumplir Ppto",  minWidth = 180,
                                           cell = function(v) format(round(v), big.mark = ",")
        ),
        MargenCumpPpto = reactable::colDef(name = "Margen proy. cumplir Ppto", minWidth = 190,
                                           cell = function(v) paste0("$", format(round(v), big.mark = ","))
        )
      )
    )
    
    # Descarga en Excel de datos filtrados sin fila de totales
    output$Descargar <- downloadHandler(
      filename = function() paste0("clientes_recuperados_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        df <- data_filtrada()
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
    
  })
}


# Clientes Nuevos ----
DetalleClienteNuevoUI <- function(id) {
  ns <- NS(id)
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
    fluidRow(
      column(4,
             h5("Segmento"),
             p("Distribucion de clientes nuevos del mes por segmento de negocio.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("Segmento"), height = "300px")
      ),
      column(4,
             h5("Presupuestado"),
             p("Clientes con presupuesto de sacos mayor a cero para el ano vigente.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("Presupuestado"), height = "300px")
      ),
      column(4,
             h5("Meses desde Primera Facturacion"),
             p("Clientes agrupados por meses transcurridos desde su primera facturacion en el ano.",
               style = "font-size:11px; color:#888; margin:-4px 0 6px 0;"),
             plotlyOutput(ns("Tiempo"), height = "300px")
      )
    ),
    fluidRow(
      column(12,
             div(style = "margin-top: 20px;",
                 TablaReactableUI(ns("TablaClientes"),
                                  titulo       = "Detalle de Clientes Nuevos",
                                  footer       = "Clic en el boton Crear para registrar una oportunidad.",
                                  footer_tipo  = "info",
                                  mostrar_nota = FALSE
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
DetalleClienteNuevo <- function(id, dat, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filtros reactivos por clic en graficos con patron toggle por categoria
    filtros <- reactiveValues(segmento = NULL, conppto = NULL, meses = NULL)
    
    observeEvent(input$limpiar_filtros, {
      filtros$segmento <- NULL
      filtros$conppto  <- NULL
      filtros$meses    <- NULL
    })
    
    # Datos base procesados
    data_cliente <- reactive({
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      req(dat())
      
      tryCatch({
        # Clientes nuevos del mes: sin estado previo y ahora en CLIENTE
        rec <- CargarDatos("CRMNALSEGR") %>%
          mutate(FecProceso = as.Date(FecProceso),
                 across(where(is.numeric),   ~ifelse(is.na(.), 0, .)),
                 across(where(is.character), ~ifelse(is.na(.) | . == "N/A", "", .))) %>%
          filter(FecProceso >= PrimerDia(Sys.Date()) - months(1)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          arrange(FecProceso) %>%
          summarise(Antes = first(SegmentoRacafe),
                    Ahora = last(SegmentoRacafe),
                    .groups = "drop") %>%
          filter(Ahora == "CLIENTE", is.na(Antes)) %>%
          select(LinNegCod, CliNitPpal)
        
        # Ejecucion mes actual y acumulado anual — YTD filtrado al year vigente
        eje <- dat() %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(
            SacosMes  = sum(ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), SacFact70, 0),
                            na.rm = TRUE),
            MargenMes = sum(ifelse(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Margen, 0),
                            na.rm = TRUE),
            SacosYTD  = sum(ifelse(year(FecFact) == year(Sys.Date()), SacFact70, 0),
                            na.rm = TRUE),
            MargenYTD = sum(ifelse(year(FecFact) == year(Sys.Date()), Margen, 0),
                            na.rm = TRUE),
            .groups = "drop"
          )
        
        # Fechas extremas de facturacion por cliente
        fec <- data %>%
          filter(!is.na(FecFact)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(PrimFact = min(FecFact, na.rm = TRUE),
                    UltFact  = max(FecFact, na.rm = TRUE),
                    .groups = "drop")
        
        mes      <- month(Sys.Date())
        mes_falt <- pmax(12 - mes, 1)
        lbl_acum <- format(Sys.Date(), "%b %Y")
        
        # Consolidacion: solo clientes identificados como nuevos del mes
        dat() %>%
          filter(SegmentoRacafe == "CLIENTE") %>%
          semi_join(rec, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            ConPpto       = ifelse(PptoSacos > 0, "CON PRESUPUESTO", "SIN PRESUPUESTO"),
            PptoSacos     = PptoSacos / 12,
            PptoMargen    = PptoMargen / 12,
            PptoSacosYTD  = PptoSacos * mes,
            PptoMargenYTD = PptoMargen * mes
          ) %>%
          select(LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Segmento,
                 ConPpto, PptoSacos, PptoMargen, PptoSacosYTD, PptoMargenYTD) %>%
          distinct() %>%
          left_join(fec, by = c("LinNegCod", "CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod", "CliNitPpal")) %>%
          mutate(
            ConPpto        = ifelse(is.na(ConPpto), "SIN PRESUPUESTO", ConPpto),
            Meses          = paste(
              pmax(0, lubridate::interval(PrimFact, Sys.Date()) %/% months(1)), "MESES"
            ),
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
        
      }, error = function(e) {
        showNotification(paste("Error procesando datos:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Toggles de filtro por clic en graficos plotly
    observeEvent(event_data("plotly_click", source = "segmento"), {
      click <- event_data("plotly_click", source = "segmento")
      if (!is.null(click))
        filtros$segmento <- if (
          !is.null(filtros$segmento) && filtros$segmento == click$x
        ) NULL else click$x
    })
    
    observeEvent(event_data("plotly_click", source = "conppto"), {
      click <- event_data("plotly_click", source = "conppto")
      if (!is.null(click))
        filtros$conppto <- if (
          !is.null(filtros$conppto) && filtros$conppto == click$x
        ) NULL else click$x
    })
    
    observeEvent(event_data("plotly_click", source = "meses"), {
      click <- event_data("plotly_click", source = "meses")
      if (!is.null(click))
        filtros$meses <- if (!is.null(filtros$meses) && filtros$meses == click$x) NULL else click$x
    })
    
    # Datos filtrados por seleccion activa en graficos
    data_filtrada <- reactive({
      df <- data_cliente()
      if (nrow(df) == 0) return(df)
      if (!is.null(filtros$segmento)) df <- df %>% filter(Segmento == filtros$segmento)
      if (!is.null(filtros$conppto))  df <- df %>% filter(ConPpto  == filtros$conppto)
      if (!is.null(filtros$meses))    df <- df %>% filter(Meses    == filtros$meses)
      df
    })
    
    # Datos para TablaReactable con fila de totales via bind_rows
    data_tabla <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      
      tot_ppto_sac   <- sum(df$PptoSacos,     na.rm = TRUE)
      tot_ppto_mar   <- sum(df$PptoMargen,     na.rm = TRUE)
      tot_ppto_sac_y <- sum(df$PptoSacosYTD,  na.rm = TRUE)
      tot_ppto_mar_y <- sum(df$PptoMargenYTD, na.rm = TRUE)
      tot_sac_mes    <- sum(df$SacosMes,       na.rm = TRUE)
      tot_mar_mes    <- sum(df$MargenMes,       na.rm = TRUE)
      tot_sac_ytd    <- sum(df$SacosYTD,       na.rm = TRUE)
      tot_mar_ytd    <- sum(df$MargenYTD,      na.rm = TRUE)
      
      fila_total <- tibble::tibble(
        Oportunidad    = "",
        PerRazSoc      = "TOTAL",
        CLLinNegNo     = "",
        Segmento       = "",
        ConPpto        = "",
        UltFact        = NA,
        PptoSacos      = tot_ppto_sac,
        SacosMes       = tot_sac_mes,
        CumpSacosMes   = if (tot_ppto_sac  > 0) tot_sac_mes / tot_ppto_sac  else NA_real_,
        PptoMargen     = tot_ppto_mar,
        MargenMes      = tot_mar_mes,
        CumpMargenMes  = if (tot_ppto_mar  > 0) tot_mar_mes / tot_ppto_mar  else NA_real_,
        PptoSacosYTD   = tot_ppto_sac_y,
        SacosYTD       = tot_sac_ytd,
        CumpSacosYTD   = if (tot_ppto_sac_y > 0) tot_sac_ytd / tot_ppto_sac_y else NA_real_,
        PptoMargenYTD  = tot_ppto_mar_y,
        MargenYTD      = tot_mar_ytd,
        CumpMargenYTD  = if (tot_ppto_mar_y > 0) tot_mar_ytd / tot_ppto_mar_y else NA_real_,
        SacosCumpPpto  = sum(df$SacosCumpPpto,  na.rm = TRUE),
        MargenCumpPpto = sum(df$MargenCumpPpto,  na.rm = TRUE),
        LblAcum        = ""
      )
      
      df %>%
        crear_link_cliente(col_razsoc = "PerRazSoc", col_linneg = "CLLinNegNo") %>%
        select(Oportunidad, PerRazSoc, CLLinNegNo, Segmento, ConPpto, UltFact,
               PptoSacos, SacosMes, CumpSacosMes,
               PptoMargen, MargenMes, CumpMargenMes,
               PptoSacosYTD, SacosYTD, CumpSacosYTD,
               PptoMargenYTD, MargenYTD, CumpMargenYTD,
               SacosCumpPpto, MargenCumpPpto, LblAcum) %>%
        bind_rows(fila_total)
    })
    
    # Patron eager: FormularioOportunidad registrado antes de TablaReactable
    dd_oportunidad_rv <- reactiveVal(NULL)
    
    FormularioOportunidad("mod_formulario",
                          dat                  = dat,
                          usr                  = usr,
                          trigger_update       = trigger_update,
                          tipo_cliente_default = reactive("CLIENTE")
    )
    
    # Graficos de distribucion con toggle de filtro activo
    crear_grafico <- function(columna, source_name) {
      df <- data_filtrada()
      if (nrow(df) == 0) {
        return(plotly_empty(type = "bar") %>%
                 layout(title = list(text = "Sin datos disponibles", font = list(size = 14))))
      }
      
      filtro_activo <- switch(source_name,
                              segmento = filtros$segmento,
                              conppto  = filtros$conppto,
                              meses    = filtros$meses,
                              NULL
      )
      
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
        layout(xaxis = list(title = "", tickangle = 0, tickfont = list(size = 12)),
               yaxis = list(title = "Cantidad de Clientes", range = c(0, ymax))) %>%
        event_register("plotly_click") %>%
        config(displayModeBar = FALSE)
    }
    
    output$Segmento    <- renderPlotly({ crear_grafico("Segmento", "segmento") })
    output$Presupuestado <- renderPlotly({ crear_grafico("ConPpto", "conppto")  })
    output$Tiempo      <- renderPlotly({ crear_grafico("Meses",    "meses")    })
    
    # Tabla reactable; seleccion restringida al boton Oportunidad via cols_activos
    TablaReactable(
      id             = "TablaClientes",
      data           = data_tabla,
      modo_seleccion = "celda",
      id_col         = NULL,
      col_header_n   = 2L,
      cols_activos   = "Oportunidad",
      sortable       = TRUE,
      searchable     = TRUE,
      page_size      = 15,
      compact        = TRUE,
      mostrar_badge  = FALSE,
      mostrar_nota   = FALSE,
      modal_icon     = "hand-holding-dollar",
      modal_size     = "xl",
      modal_titulo_fn = function(sel) {
        paste0("Crear Oportunidad — ", as.character(sel$fila$PerRazSoc[[1]]))
      },
      modal_pre_fn = function(sel) {
        dd_oportunidad_rv(list(
          data   = data_filtrada() %>%
            filter(PerRazSoc  == as.character(sel$fila$PerRazSoc[[1]]),
                   CLLinNegNo == as.character(sel$fila$CLLinNegNo[[1]])),
          accion = "oportunidad"
        ))
      },
      modal_contenido_fn = function(sel) FormularioOportunidadUI(ns("mod_formulario")),
      columnas = list(
        Oportunidad = reactable::colDef(
          name     = "",
          minWidth = 55,
          html     = TRUE,
          cell     = function(v) {
            if (v == "") return("")
            as.character(tags$span(
              style = paste("display:inline-flex; align-items:center; justify-content:center;",
                            "width:28px; height:28px; border-radius:6px;",
                            "background:#C11007; color:white; font-size:13px; cursor:pointer;"),
              icon("hand-holding-dollar")
            ))
          }
        ),
        PerRazSoc = reactable::colDef(
          name     = "Cliente",
          minWidth = 200,
          html     = TRUE,
          cell     = function(v) as.character(htmltools::HTML(v))
        ),
        CLLinNegNo = reactable::colDef(name = "Linea de Negocio", minWidth = 140),
        Segmento   = reactable::colDef(name = "Segmento",         minWidth = 110,
                                       style = function(v) list(background = "#EDFBF2", color = "#1E8449", fontWeight = "600")
        ),
        ConPpto  = reactable::colDef(name = "Presupuestado",    minWidth = 130),
        UltFact  = reactable::colDef(name = "Ult. Facturacion", minWidth = 120,
                                     cell = function(v) if (is.na(v)) "—" else format(as.Date(v), "%d/%m/%Y")
        ),
        # Bloque mensual
        PptoSacos    = reactable::colDef(name = "Ppto Sacos Mes",    minWidth = 120,
                                         cell = function(v) if (is.na(v)) "—" else format(round(v), big.mark = ",")
        ),
        SacosMes     = reactable::colDef(name = "Sacos Mes",         minWidth = 100,
                                         cell = function(v) format(round(v), big.mark = ",")
        ),
        CumpSacosMes = reactable::colDef(name = "% Cumpl Sacos Mes", minWidth = 130,
                                         cell  = function(v) if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%"),
                                         style = function(v) {
                                           if (is.na(v) || is.infinite(v)) return(NULL)
                                           list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                                                fontWeight = "600")
                                         }
        ),
        PptoMargen    = reactable::colDef(name = "Ppto Margen Mes",    minWidth = 130,
                                          cell = function(v) if (is.na(v)) "—" else paste0("$", format(round(v), big.mark = ","))
        ),
        MargenMes     = reactable::colDef(name = "Margen Mes",         minWidth = 120,
                                          cell = function(v) paste0("$", format(round(v), big.mark = ","))
        ),
        CumpMargenMes = reactable::colDef(name = "% Cumpl Margen Mes", minWidth = 140,
                                          cell  = function(v) if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%"),
                                          style = function(v) {
                                            if (is.na(v) || is.infinite(v)) return(NULL)
                                            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                                                 fontWeight = "600")
                                          }
        ),
        # Bloque acumulado con periodo dinamico
        LblAcum      = reactable::colDef(show = FALSE),
        PptoSacosYTD = reactable::colDef(
          minWidth = 140,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Ppto Sacos Acum. ", lbl)
          },
          cell = function(v) if (is.na(v)) "—" else format(round(v), big.mark = ",")
        ),
        SacosYTD     = reactable::colDef(
          minWidth = 130,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Sacos Acum. ", lbl)
          },
          cell = function(v) format(round(v), big.mark = ",")
        ),
        CumpSacosYTD = reactable::colDef(
          minWidth = 150,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("% Cumpl Sacos Acum. ", lbl)
          },
          cell  = function(v) if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%"),
          style = function(v) {
            if (is.na(v) || is.infinite(v)) return(NULL)
            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                 fontWeight = "600")
          }
        ),
        PptoMargenYTD = reactable::colDef(
          minWidth = 150,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Ppto Margen Acum. ", lbl)
          },
          cell = function(v) if (is.na(v)) "—" else paste0("$", format(round(v), big.mark = ","))
        ),
        MargenYTD    = reactable::colDef(
          minWidth = 140,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("Margen Acum. ", lbl)
          },
          cell = function(v) paste0("$", format(round(v), big.mark = ","))
        ),
        CumpMargenYTD = reactable::colDef(
          minWidth = 160,
          header   = function(value, name) {
            lbl <- tryCatch(unique(data_tabla()$LblAcum)[1], error = function(e) "")
            paste0("% Cumpl Margen Acum. ", lbl)
          },
          cell  = function(v) if (is.na(v) || is.infinite(v)) "—" else paste0(round(v * 100, 1), "%"),
          style = function(v) {
            if (is.na(v) || is.infinite(v)) return(NULL)
            list(background = if (v >= 1) "#D5F5E3" else if (v >= 0.85) "#FCF3CF" else "#FADBD8",
                 fontWeight = "600")
          }
        ),
        # Bloque proyeccion anual
        SacosCumpPpto  = reactable::colDef(name = "Sacos proy. cumplir Ppto",  minWidth = 180,
                                           cell = function(v) format(round(v), big.mark = ",")
        ),
        MargenCumpPpto = reactable::colDef(name = "Margen proy. cumplir Ppto", minWidth = 190,
                                           cell = function(v) paste0("$", format(round(v), big.mark = ","))
        )
      )
    )
    
    # Descarga en Excel de datos filtrados sin fila de totales
    output$Descargar <- downloadHandler(
      filename = function() paste0("clientes_nuevos_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        df <- data_filtrada()
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

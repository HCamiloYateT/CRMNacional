# Consulta e impresion de Indicadores ----
IndicadoresUI <- function(id) {
  ns <- NS(id)
  tagList(
    gt_output(ns("tabla_indicadores")),
    tags$hr(),
    h6("Disponible"),
    gt_output(ns("tabla_disponible"))
  )
}

Indicadores <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    
    # Diccionario canonico de nombres legibles (orden = orden de visualizacion)
    nombres_items <- c(
      "TRM"         = "TRM (Hoja de trabajo)",
      "PrecioNY"    = "Precio NYC (HT)",
      "PrecioCarga" = "Precio Carga (Promedio de últimas entradas del día)",
      "Diferencial" = "Diferencial de Compra (HT)",
      "UGCRacafe"   = "Costo UGQ Racafé",
      "PrecioBolsa" = "Precio Bolsa (FNC)",
      "PrecioFNC"   = "Precio Carga (FNC)",
      "UGCFNC"      = "Costo UGQ (FNC)",
      "CALConsumo"  = "Precio Consumo (Calculadora)",
      "COMConsumo"  = "Precio Consumo (Compras)",
      "CALPasilla"  = "Precio Pasilla (Calculadora)",
      "COMMolidos"  = "Precio Molidos (Compras)",
      "COMSoluble"  = "Precio Soluble (Compras)",
      "CALRipio"    = "Precio Ripio (Calculadora)",
      "COMRipio"    = "Precio Ripio (Compras)",
      "COMRobusta"  = "Precio Robusta (Compras)"
    )
    
    # Datos del dia anterior: seleccion explicita por nombre (evita exclusion por rango posicional)
    anterior <- CargarDatos("CRMINDICADORES") %>%
      mutate(Fecha = as.Date(FechaActualizacion)) %>%
      filter(Fecha == Sys.Date() - 1) %>%
      select(all_of(names(nombres_items))) %>%
      pivot_longer(cols = all_of(names(nombres_items)), names_to = "Item", values_to = "anterior") %>%
      mutate(Item = dplyr::recode(Item, !!!nombres_items))
    
    output$tabla_indicadores <- render_gt({
      req(dat())
      
      orden_items <- unname(nombres_items)
      
      tabla <- dat() %>%
        select(-last_updated) %>%
        mutate(Item = as.character(Item)) %>%
        left_join(anterior, by = "Item") %>%
        rowwise() %>%
        mutate(
          Cambio = case_when(
            Valor > anterior ~ FormatearNumero(Valor, formato = "dolares", color = "#1F7A55"),
            Valor < anterior ~ FormatearNumero(Valor, formato = "dolares", color = "#9F0712"),
            TRUE             ~ FormatearNumero(Valor, formato = "dolares", color = "#000")
          )
        ) %>%
        ungroup() %>%
        select(Item, Valor = Cambio) %>%
        mutate(Item = factor(Item, levels = orden_items)) %>%
        arrange(Item)
      
      tabla %>%
        gt() %>%
        fmt(columns = Valor, fns = function(x) purrr::map(x, gt::html)) %>%
        cols_align(align = "left",  columns = Item) %>%
        cols_align(align = "right", columns = Valor) %>%
        tab_style(
          style     = cell_text(weight = "bold"),
          locations = cells_body(columns = Item)
        ) %>%
        gt_minimal_style() %>%
        tab_style(
          style     = cell_borders(sides = "bottom", color = "black", weight = px(1)),
          locations = cells_body(rows = 3)
        ) %>%
        tab_options(column_labels.hidden = TRUE)
    })
    
    output$tabla_disponible <- render_gt({
      t1 <- readRDS(
        "/home/htamara/6_IndustriaNacional/CRM Cliente Nacional/CRMNacional/data/cafexasignar.rds"
      )
      texto <- FormatearTexto(
        paste("**Posición con cierre al**", format(Unicos(t1$Fecha), "%d %b %Y")),
        tamano_pct = 0.8
      )
      t1 %>%
        filter(Tipo_Negocio == "Disponible") %>%
        select(Tipo_Negocio, Total_Sacos, PrecioxCarga) %>%
        pivot_longer(cols = Total_Sacos:PrecioxCarga, names_to = "name", values_to = "value") %>%
        mutate(name = recode(name, "Total_Sacos" = "Sacos", "PrecioxCarga" = "Precio de Carga")) %>%
        select(Tipo_Negocio = name, Total_Sacos = value) %>%
        gt() %>%
        fmt_number(columns = Total_Sacos, decimals = 2) %>%
        tab_style(
          style     = cell_text(weight = "bold", size = px(14)),
          locations = cells_body(columns = Tipo_Negocio)
        ) %>%
        tab_style(
          style     = cell_text(size = px(12)),
          locations = cells_body(columns = Total_Sacos)
        ) %>%
        tab_style(
          style     = cell_borders(sides = c("top", "bottom", "left", "right"), color = "transparent"),
          locations = cells_body()
        ) %>%
        gt_minimal_style() %>%
        tab_source_note(source_note = md(texto)) %>%
        tab_options(column_labels.hidden = TRUE)
    })
    
    output$last_update <- renderUI({
      req(dat())
      FormatearTexto(
        paste("Última actualización:", dat()$last_updated[1]) %>% HTML,
        tamano_pct = 0.6
      )
    })
    
    output$refresh_button <- renderUI({
      actionBttn(session$ns("refresh_indicators"), icon = icon("sync"), size = "xs")
    })
    
    reactive(input$refresh_indicators)
  })
}


# Comparacion de Indicadores ----

# Diccionario canonico DB -> label de visualizacion (compartido entre UI y server)
.IND_NOMBRES_DB <- c(
  "PrecioBolsa" = "Precio Bolsa FNC (¢USD/lb)",
  "TRM"         = "TRM ($COP/USD)",
  "PrecioFNC"   = "Precio FNC ($COP/carga)",
  "UGCFNC"      = "UGC FNC ($COP/kg)",
  "PrecioNY"    = "Precio NYC (HT) (¢USD/lb)",
  "PrecioCarga" = "Precio Carga ($COP/carga)",
  "Diferencial" = "Diferencial de Compra (HT) ($USD/lb)",
  "UGCRacafe"   = "UGC Racafe ($COP/kg)",
  "CALConsumo"  = "CAL Consumo ($COP/kg)",
  "COMConsumo"  = "COM Consumo ($COP/kg)",
  "CALPasilla"  = "CAL Pasilla ($COP/kg)",
  "COMMolidos"  = "COM Molidos ($COP/kg)",
  "COMSoluble"  = "COM Soluble ($COP/kg)",
  "CALRipio"    = "CAL Ripio ($COP/kg)",
  "COMRipio"    = "COM Ripio ($COP/kg)",
  "COMRobusta"  = "COM Robusta ($COP/kg)"
)

# Diccionario inverso: label de data_ind() -> nombre de columna en BD
.IND_NOMBRES_INV <- c(
  "TRM (Hoja de trabajo)"                              = "TRM",
  "Precio NYC (HT)"                                    = "PrecioNY",
  "Precio Carga (Promedio de últimas entradas del día)" = "PrecioCarga",
  "Diferencial de Compra (HT)"                         = "Diferencial",
  "Costo UGQ Racafé"                                   = "UGCRacafe",
  "Precio Bolsa (FNC)"                                 = "PrecioBolsa",
  "Precio Carga (FNC)"                                 = "PrecioFNC",
  "Costo UGQ (FNC)"                                    = "UGCFNC",
  "Precio Consumo (Calculadora)"                       = "CALConsumo",
  "Precio Consumo (Compras)"                           = "COMConsumo",
  "Precio Pasilla (Calculadora)"                       = "CALPasilla",
  "Precio Molidos (Compras)"                           = "COMMolidos",
  "Precio Soluble (Compras)"                           = "COMSoluble",
  "Precio Ripio (Calculadora)"                         = "CALRipio",
  "Precio Ripio (Compras)"                             = "COMRipio",
  "Precio Robusta (Compras)"                           = "COMRobusta"
)

ComparacionIndicadoresUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Caja de filtros con estilo tsk-filtros-wrap (consistente con modulo Tareas)
    tags$div(
      class = "tsk-filtros-wrap",
      fluidRow(
        column(3,
               tags$div(class = "tsk-filtros-label", "Fecha Inicial"),
               dateInput(ns("fecha_anterior"), label = NULL, value = Sys.Date() - 1,
                         format = "yyyy-mm-dd", language = "es", width = "100%")
        ),
        column(3,
               tags$div(class = "tsk-filtros-label", "Fecha Final"),
               dateInput(ns("fecha_actual"), label = NULL, value = Sys.Date(),
                         format = "yyyy-mm-dd", language = "es", width = "100%")
        ),
        column(3,
               tags$div(class = "tsk-filtros-label", "Accion"),
               actionButton(ns("comparar"), "Comparar Indicadores",
                            icon = icon("exchange-alt"), class = "btn-danger",
                            style = "width:100%;")
        )
      )
    ),
    # Tabla comparativa
    fluidRow(
      column(12,
             bs4Card(
               title = "Tabla Comparativa de Indicadores", width = 12,
               status = "white", solidHeader = TRUE, collapsible = TRUE,
               gt_output(ns("tabla_gt_comparacion"))
             )
      )
    )
  )
}

ComparacionIndicadores <- function(id, data_ind) {
  moduleServer(id, function(input, output, session) {
    
    # Carga y normaliza datos historicos de CRMINDICADORES para una fecha dada
    .cargar_historico <- function(fecha) {
      CargarDatos("CRMINDICADORES") %>%
        mutate(FechaActualizacion = as.Date(FechaActualizacion)) %>%
        filter(FechaActualizacion == fecha) %>%
        arrange(desc(FechaActualizacion)) %>%
        slice(1)
    }
    
    # Normaliza data_ind() (long con Item como factor) a wide con nombres de columna de BD
    .normalizar_cache <- function(datos_raw) {
      datos_raw %>%
        mutate(Item = as.character(Item)) %>%
        select(Item, Valor) %>%
        mutate(Item = dplyr::recode(Item, !!!.IND_NOMBRES_INV)) %>%
        pivot_wider(names_from = Item, values_from = Valor)
    }
    
    datos_comparacion <- eventReactive(input$comparar, ignoreNULL = FALSE, {
      req(input$fecha_anterior, input$fecha_actual)
      es_hoy <- input$fecha_actual == Sys.Date()
      
      # Datos de fecha actual: cache en tiempo real o historico
      if (es_hoy) {
        datos_actual_raw <- data_ind()
        if (is.null(datos_actual_raw) || nrow(datos_actual_raw) == 0) {
          showNotification("No se pudieron cargar los datos del caché para hoy",
                           type = "error", duration = 5)
          return(NULL)
        }
        datos_actual  <- .normalizar_cache(datos_actual_raw)
        fuente_actual <- "Datos en tiempo real"
      } else {
        datos_actual <- .cargar_historico(input$fecha_actual)
        if (nrow(datos_actual) == 0) {
          showNotification(
            paste("No se encontraron datos para la fecha vigente:", input$fecha_actual),
            type = "error", duration = 5
          )
          return(NULL)
        }
        fuente_actual <- "Base de datos histórica"
      }
      
      # Datos de fecha anterior: siempre historico
      datos_anterior <- .cargar_historico(input$fecha_anterior)
      if (nrow(datos_anterior) == 0) {
        showNotification(
          paste("No se encontraron datos para la fecha anterior:", input$fecha_anterior),
          type = "error", duration = 5
        )
        return(NULL)
      }
      
      # Construccion de tabla comparativa sobre columnas canonicas del diccionario
      cols_bd <- names(.IND_NOMBRES_DB)
      cols_disponibles <- cols_bd[
        cols_bd %in% names(datos_anterior) & cols_bd %in% names(datos_actual)
      ]
      
      comparacion <- tibble(Indicador = cols_disponibles) %>%
        mutate(
          Nombre_Indicador = .IND_NOMBRES_DB[Indicador],
          Valor_Anterior = map_dbl(Indicador, ~ {
            val <- as.numeric(datos_anterior[[.x]])
            ifelse(length(val) == 0 || is.na(val), NA_real_, val)
          }),
          Valor_Actual = map_dbl(Indicador, ~ {
            val <- as.numeric(datos_actual[[.x]])
            ifelse(length(val) == 0 || is.na(val), NA_real_, val)
          }),
          Diferencia_Absoluta  = Valor_Actual - Valor_Anterior,
          Variacion_Porcentual = Variacion(ini = Valor_Anterior, fin = Valor_Actual)
        ) %>%
        select(Nombre_Indicador, Valor_Anterior, Valor_Actual,
               Diferencia_Absoluta, Variacion_Porcentual)
      
      showNotification("Comparación completada exitosamente", type = "message", duration = 3)
      
      list(datos = comparacion, fuente_actual = fuente_actual, es_hoy = es_hoy)
    })
    
    output$tabla_gt_comparacion <- render_gt({
      req(datos_comparacion())
      
      datos <- datos_comparacion()$datos %>%
        filter(!is.na(Valor_Anterior) | !is.na(Valor_Actual))
      
      if (nrow(datos) == 0) {
        return(
          data.frame(Mensaje = "No hay datos disponibles para comparar") %>%
            gt() %>% tab_options(table.width = pct(100))
        )
      }
      
      datos %>%
        gt() %>%
        tab_header(
          title    = md("**Comparación de Indicadores**"),
          subtitle = md(paste0(
            "Comparación entre **", format(input$fecha_anterior, "%d/%b/%Y"),
            "** y **", format(input$fecha_actual, "%d/%b/%Y"), "**"
          ))
        ) %>%
        cols_label(
          Nombre_Indicador     = "Indicador",
          Valor_Anterior       = "Valor Anterior",
          Valor_Actual         = "Valor Vigente",
          Diferencia_Absoluta  = "Diferencia Absoluta",
          Variacion_Porcentual = "Variación %"
        ) %>%
        fmt_currency(
          columns  = c(Valor_Anterior, Valor_Actual, Diferencia_Absoluta),
          currency = "USD", decimals = 2
        ) %>%
        fmt_percent(columns = Variacion_Porcentual, decimals = 2) %>%
        tab_style(
          style     = cell_text(weight = "bold"),
          locations = cells_body(columns = Nombre_Indicador)
        ) %>%
        gt_var_style("Variacion_Porcentual") %>%
        gt_pct_style("Diferencia_Absoluta") %>%
        gt_minimal_style() %>%
        tab_source_note(source_note = md(paste0(
          "Fuente de datos en fecha final: ", datos_comparacion()$fuente_actual, "*"
        )))
    })
  })
}
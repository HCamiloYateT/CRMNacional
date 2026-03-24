DetalleCompetenciaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBox(
      id = ns("tabs_competencia"),
      width = 12,
      
      # Tab 1: Tablas de precios
      tabPanel(
        title = "Matriz de Precios",
        icon = icon("table"),
        
        fluidRow(
          column(
            12,
            br(),
            h4("Precios actuales por competidor y categoría"),
            rHandsontableOutput(ns("tabla_precios")),
            br(),
            h4("Fecha de última actualización (por competidor y categoría)"),
            rHandsontableOutput(ns("tabla_fechas")),
            br(),
            h4("Detalle de registros históricos"),
            rHandsontableOutput(ns("tabla_historial")),
            br(),
            fluidRow(
              column(
                4,
                actionButton(
                  ns("btn_guardar"),
                  "Guardar Cambios",
                  icon = icon("save"),
                  class = "btn-success",
                  style = "width: 100%;"
                )
              ),
              column(
                4,
                actionButton(
                  ns("btn_reestablecer"),
                  "Reestablecer Cambios",
                  icon = icon("undo"),
                  class = "btn-secondary",
                  style = "width: 100%;"
                )
              )
            ),
            br(), br()
          )
        )
      ),
      
      # Tab 2: Tendencia histórica
      tabPanel(
        title = "Tendencia Histórica",
        icon = icon("chart-line"),
        
        fluidRow(
          column(
            12,
            br(),
            fluidRow(
              column(
                6,
                selectInput(
                  ns("sel_competidor"),
                  "Seleccionar Competidor:",
                  choices = NULL,
                  width = "100%"
                )
              ),
              column(
                6,
                selectInput(
                  ns("sel_categoria"),
                  "Seleccionar Categoría:",
                  choices = NULL,
                  width = "100%"
                )
              )
            ),
            br(),
            plotlyOutput(ns("grafico_tendencia"), height = "500px"),
            br()
          )
        )
      ),
      
      # Tab 3: Tabla completa de competidores
      tabPanel(
        title = "Datos Completos",
        icon = icon("database"),
        
        fluidRow(
          column(
            12,
            br(),
            h4("Tabla completa de competidores (CRMNALCOMPETENCIA)"),
            rHandsontableOutput(ns("tabla_competencia_completa")),
            br(),
            h4("Tabla completa de precios históricos (CRMNALCOMPETENCIAPRECIOS)"),
            rHandsontableOutput(ns("tabla_precios_completa")),
            br()
          )
        )
      )
    )
  )
}

DetalleCompetencia <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Categorías de productos (sin vacío inicial)
    categorias <- c(
      "PASILLA", "CONSUMO", "UGQ", "SUPREMO",
      "PERFIL DE TAZA", "REGIONAL", "EXÓTICO"
    )
    
    # Reactive values
    rv <- reactiveValues(
      datos_competencia = NULL,
      tabla_historica   = NULL
    )
    
    # Cargar datos iniciales
    observe({
      # Competidores - CARGAR TODAS LAS COLUMNAS
      rv$datos_competencia <- CargarDatos("CRMNALCOMPETENCIA") %>%
        arrange(desc(FechaHoraCrea))
      
      # Tabla histórica - CARGAR TODAS LAS COLUMNAS
      rv$tabla_historica <- CargarDatos("CRMNALCOMPETENCIAPRECIOS") %>%
        mutate(
          FechaHoraCrea = as.POSIXct(FechaHoraCrea, tz = "America/Bogota"),
          Categoria     = as.character(Categoria)
        )
    })
    
    # Matriz de PRECIOS (una fila por competidor, columnas por categoría)
    matriz_precios <- reactive({
      req(rv$datos_competencia)
      
      competidores <- unique(rv$datos_competencia$Competencia)
      
      # Base: solo precios
      matriz <- data.frame(
        Competidor = competidores,
        stringsAsFactors = FALSE
      )
      
      # Columnas por categoría con precios
      for (cat in categorias) {
        matriz[[cat]] <- NA_real_
      }
      
      if (!is.null(rv$tabla_historica) && nrow(rv$tabla_historica) > 0) {
        for (i in seq_len(nrow(matriz))) {
          comp <- matriz$Competidor[i]
          
          for (cat in categorias) {
            ult <- rv$tabla_historica %>%
              filter(Competencia == comp, Categoria == cat) %>%
              arrange(desc(FechaHoraCrea)) %>%
              slice(1)
            
            if (nrow(ult) > 0) {
              matriz[i, cat] <- ult$Precio
            }
          }
        }
      }
      
      matriz
    })
    
    # Matriz de FECHAS (una fila por competidor, columnas por categoría)
    matriz_fechas <- reactive({
      req(rv$datos_competencia)
      
      competidores <- unique(rv$datos_competencia$Competencia)
      
      # Base: solo fechas
      matriz <- data.frame(
        Competidor = competidores,
        stringsAsFactors = FALSE
      )
      
      for (cat in categorias) {
        matriz[[cat]] <- NA_character_
      }
      
      if (!is.null(rv$tabla_historica) && nrow(rv$tabla_historica) > 0) {
        for (i in seq_len(nrow(matriz))) {
          comp <- matriz$Competidor[i]
          
          for (cat in categorias) {
            ult <- rv$tabla_historica %>%
              filter(Competencia == comp, Categoria == cat) %>%
              arrange(desc(FechaHoraCrea)) %>%
              slice(1)
            
            if (nrow(ult) > 0) {
              f_act <- as.Date(ult$FechaHoraCrea)
              matriz[i, cat] <- format(f_act, "%d/%m/%Y")
            }
          }
        }
      }
      
      matriz
    })
    
    # Tabla historial (registros crudos) - MOSTRAR TODAS LAS COLUMNAS
    tabla_historial <- reactive({
      req(rv$tabla_historica)
      
      rv$tabla_historica %>%
        arrange(desc(FechaHoraCrea))
    })
    
    # Tabla completa de competidores - TODAS LAS COLUMNAS
    tabla_competencia_completa <- reactive({
      req(rv$datos_competencia)
      rv$datos_competencia
    })
    
    # Render matriz de precios (editable)
    render_tabla_precios <- function() {
      mat <- matriz_precios()
      
      rhandsontable(mat, rowHeaders = NULL, stretchH = "all") %>%
        hot_col("Competidor", readOnly = TRUE) %>%
        hot_cols(columnSorting = TRUE, manualColumnResize = TRUE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
    
    output$tabla_precios <- renderRHandsontable({
      req(matriz_precios())
      render_tabla_precios()
    })
    
    # Render matriz de fechas (solo lectura)
    output$tabla_fechas <- renderRHandsontable({
      req(matriz_fechas())
      
      rhandsontable(matriz_fechas(), rowHeaders = NULL, stretchH = "all") %>%
        hot_col("Competidor", readOnly = TRUE) %>%
        hot_cols(readOnly = TRUE) %>%
        hot_cols(columnSorting = TRUE, manualColumnResize = TRUE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    # Render tabla historial - TODAS LAS COLUMNAS (solo lectura)
    output$tabla_historial <- renderRHandsontable({
      req(tabla_historial())
      
      df <- tabla_historial()
      
      # Crear tabla con todas las columnas en modo solo lectura
      hot <- rhandsontable(df, rowHeaders = NULL, stretchH = "all") %>%
        hot_cols(readOnly = TRUE, columnSorting = TRUE, manualColumnResize = TRUE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
      hot
    })
    
    # Render tabla completa de competidores
    output$tabla_competencia_completa <- renderRHandsontable({
      req(tabla_competencia_completa())
      
      df <- tabla_competencia_completa()
      
      rhandsontable(df, rowHeaders = NULL, stretchH = "all") %>%
        hot_cols(readOnly = TRUE, columnSorting = TRUE, manualColumnResize = TRUE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    # Render tabla completa de precios históricos
    output$tabla_precios_completa <- renderRHandsontable({
      req(rv$tabla_historica)
      
      df <- rv$tabla_historica %>%
        arrange(desc(FechaHoraCrea))
      
      rhandsontable(df, rowHeaders = NULL, stretchH = "all") %>%
        hot_cols(readOnly = TRUE, columnSorting = TRUE, manualColumnResize = TRUE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    # Reestablecer cambios (descartar edición local)
    observeEvent(input$btn_reestablecer, {
      output$tabla_precios <- renderRHandsontable({
        render_tabla_precios()
      })
      toastr_info(
        "Se reestablecieron los valores desde la base de datos",
        title    = "Cambios descartados",
        position = "top-right"
      )
    })
    
    # Guardar cambios en CRMNALCOMPETENCIAPRECIOS
    observeEvent(input$btn_guardar, {
      req(input$tabla_precios)
      req(rv$datos_competencia)
      
      datos_editados <- hot_to_r(input$tabla_precios)
      
      nuevos_registros <- data.frame()
      
      for (i in seq_len(nrow(datos_editados))) {
        comp <- datos_editados$Competidor[i]
        
        for (cat in categorias) {
          precio_nuevo <- datos_editados[i, cat]
          
          if (!is.na(precio_nuevo)) {
            # Último registro en BD para comparar
            ult <- rv$tabla_historica %>%
              filter(Competencia == comp, Categoria == cat) %>%
              arrange(desc(FechaHoraCrea)) %>%
              slice(1)
            
            if (nrow(ult) == 0 || is.na(ult$Precio) || ult$Precio != precio_nuevo) {
              nuevo_reg <- data.frame(
                UsuarioCrea   = uid,
                FechaHoraCrea = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                Competencia   = comp,
                Observaciones = "",
                Categoria     = cat,
                Precio        = precio_nuevo,
                PrecioUGQ     = NA_real_,
                stringsAsFactors = FALSE
              )
              
              nuevos_registros <- bind_rows(nuevos_registros, nuevo_reg)
            }
          }
        }
      }
      
      if (nrow(nuevos_registros) > 0) {
        tryCatch({
          SubirDatos(nuevos_registros, "CRMNALCOMPETENCIAPRECIOS")
          
          # Recargar tabla histórica y por ende ambas tablas
          rv$tabla_historica <- CargarDatos("CRMNALCOMPETENCIAPRECIOS") %>%
            mutate(
              FechaHoraCrea = as.POSIXct(FechaHoraCrea, tz = "America/Bogota"),
              Categoria     = as.character(Categoria)
            )
          
          toastr_success(
            paste("Se guardaron", nrow(nuevos_registros), "registros exitosamente"),
            title    = "Guardado Exitoso",
            position = "top-right"
          )
        }, error = function(e) {
          toastr_error(
            paste("Error al guardar:", e$message),
            title    = "Error",
            position = "top-right"
          )
        })
      } else {
        toastr_info(
          "No hay cambios para guardar",
          title    = "Información",
          position = "top-right"
        )
      }
    })
    
    # Selectores del gráfico
    observe({
      req(rv$tabla_historica)
      
      comps <- sort(unique(rv$tabla_historica$Competencia))
      
      updateSelectInput(
        session,
        "sel_competidor",
        choices  = c("Todos" = "TODOS", setNames(comps, comps)),
        selected = "TODOS"
      )
      
      updateSelectInput(
        session,
        "sel_categoria",
        choices  = c("Todas" = "TODAS", setNames(categorias, categorias)),
        selected = "TODAS"
      )
    })
    
    # Datos para gráfico
    datos_grafico <- reactive({
      req(rv$tabla_historica)
      req(input$sel_competidor)
      req(input$sel_categoria)
      
      datos <- rv$tabla_historica %>%
        mutate(Fecha = as.Date(FechaHoraCrea))
      
      if (input$sel_competidor != "TODOS") {
        datos <- datos %>% filter(Competencia == input$sel_competidor)
      }
      
      if (input$sel_categoria != "TODAS") {
        datos <- datos %>% filter(Categoria == input$sel_categoria)
      }
      
      datos %>% arrange(Fecha)
    })
    
    # Gráfico de tendencia
    output$grafico_tendencia <- renderPlotly({
      req(datos_grafico())
      
      datos <- datos_grafico()
      
      if (nrow(datos) == 0) {
        plot_ly() %>%
          layout(
            title = "No hay datos disponibles para los filtros seleccionados",
            xaxis = list(title = "Fecha"),
            yaxis = list(title = "Precio")
          ) %>%
          config(locale = "es", displayModeBar = TRUE)
      } else {
        if (input$sel_competidor != "TODOS" && input$sel_categoria != "TODAS") {
          # Una sola línea
          plot_ly(
            datos,
            x    = ~Fecha,
            y    = ~Precio,
            type = "scatter",
            mode = "lines+markers",
            name = paste(input$sel_competidor, "-", input$sel_categoria),
            line   = list(width = 3),
            marker = list(size = 8)
          ) %>%
            layout(
              title = paste("Tendencia de Precios:", input$sel_competidor, "-", input$sel_categoria),
              xaxis = list(title = "Fecha"),
              yaxis = list(title = "Precio (COP)"),
              hovermode = "x unified"
            ) %>%
            config(locale = "es", displayModeBar = TRUE)
        } else if (input$sel_competidor != "TODOS") {
          # Varias líneas por categoría
          plot_ly(
            datos,
            x     = ~Fecha,
            y     = ~Precio,
            color = ~Categoria,
            type  = "scatter",
            mode  = "lines+markers",
            line   = list(width = 2),
            marker = list(size = 6)
          ) %>%
            layout(
              title = paste("Tendencia de Precios por Categoría:", input$sel_competidor),
              xaxis = list(title = "Fecha"),
              yaxis = list(title = "Precio (COP)"),
              hovermode = "x unified",
              legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center")
            ) %>%
            config(locale = "es", displayModeBar = TRUE)
        } else if (input$sel_categoria != "TODAS") {
          # Varias líneas por competidor
          plot_ly(
            datos,
            x     = ~Fecha,
            y     = ~Precio,
            color = ~Competencia,
            type  = "scatter",
            mode  = "lines+markers",
            line   = list(width = 2),
            marker = list(size = 6)
          ) %>%
            layout(
              title = paste("Tendencia de Precios por Competidor:", input$sel_categoria),
              xaxis = list(title = "Fecha"),
              yaxis = list(title = "Precio (COP)"),
              hovermode = "x unified",
              legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center")
            ) %>%
            config(locale = "es", displayModeBar = TRUE)
        } else {
          # Todas las combinaciones
          datos <- datos %>%
            mutate(Serie = paste(Competencia, "-", Categoria))
          
          plot_ly(
            datos,
            x     = ~Fecha,
            y     = ~Precio,
            color = ~Serie,
            type  = "scatter",
            mode  = "lines+markers",
            line   = list(width = 2),
            marker = list(size = 6)
          ) %>%
            layout(
              title = "Tendencia de Precios - Todos los Competidores y Categorías",
              xaxis = list(title = "Fecha"),
              yaxis = list(title = "Precio (COP)"),
              hovermode = "x unified",
              legend = list(orientation = "v", x = 1.05, y = 1)
            ) %>%
            config(locale = "es", displayModeBar = TRUE)
        }
      }
    })
  })
}
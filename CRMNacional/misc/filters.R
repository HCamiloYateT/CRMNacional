# Helpers -----

# Opciones de picker con umbral dinamico para label "Todos/Todas"
.picker_opts_update <- function(choices, fem = FALSE) {
  tod <- if (fem) "Todas" else "Todos"
  nin <- if (fem) "Ninguna" else "Ninguno"
  n   <- length(choices)
  pickerOptions(
    `selected-text-format` = paste0("count > ", max(n - 1, 0)),
    `count-selected-text`  = tod,
    noneSelectedText       = nin
  )
}

# Modulo ----
FiltrosUI <- function(id) {
  ns <- NS(id)
  
  # Variables de fecha derivadas del objeto global data
  .fec_min <- min(data$FecFact, na.rm = TRUE)
  .fec_max <- max(data$FecFact, na.rm = TRUE)
  
  # Choices iniciales (llamada unica a Choices())
  cho <- Choices()
  
  tagList(
    # Fila: periodo y rango de fechas
    fluidRow(
      column(5,
             ListaDesplegable(ns("FT_Periodo"), "Valores a Usar",
                              multiple = FALSE, choices = c("DINÁMICO", "ESTÁTICO"))
      ),
      column(7,
             FormatearTexto("Fecha de Factura", tamano_pct = 1),
             dateRangeInput(ns("FT_Fecha"), "",
                            start    = PrimerDia(Sys.Date(), uni = "year"),
                            end      = .fec_max,
                            min      = .fec_min,
                            max      = .fec_max,
                            language = "es", separator = " - ",
                            format   = "dd/mm/yyyy"),
             materialSwitch(inputId = ns("FT_Fechas"),
                            label   = FormatearTexto("Incluir lotes sin facturar", tamano_pct = 0.8),
                            value   = TRUE, status = "danger", width = "100%")
      )
    ),
    # Fila: asesor
    fluidRow(
      column(12,
             ListaDesplegable(ns("FT_Asesor"), "Asesor", choices = cho$personas)
      )
    ),
    # Fila: linea de negocio y segmento
    fluidRow(
      column(6,
             ListaDesplegable(ns("FT_LinNeg"), "Linea de Negocio",
                              choices = cho$linneg, fem = TRUE)
      ),
      column(6,
             ListaDesplegable(ns("FT_Segmento"), "Segmento", choices = cho$segmento)
      )
    ),
    # Fila: categoria y producto — inician vacios, se pueblan por cascada
    fluidRow(
      column(6,
             ListaDesplegable(ns("FT_Categoria"), "Categoria",
                              choices = cho$categoria, selected = NULL, fem = TRUE)
      ),
      column(6,
             ListaDesplegable(ns("FT_Producto"), "Producto",
                              choices = cho$producto, selected = NULL)
      )
    ),
    # Boton aplicar filtros
    BotonGuardar(id              = ns("FT_Aplicar"),
                 label           = "Aplicar Filtros",
                 align           = "right",
                 color           = "danger",
                 size            = "xs",
                 icon            = "check",
                 style_container = "display:flex; gap:15px; margin-top:10px;")
  )
}

FiltrosServer <- function(id, usuario, productos_cache) {
  moduleServer(id, function(input, output, session) {
    
    # Estado interno: almacena el snapshot de filtros vigente
    filtros_aplicados <- reactiveVal(NULL)
    
    # Bandera: TRUE cuando la cascada inicial ha terminado de poblarse
    .cascada_lista <- reactiveVal(FALSE)
    
    # Helper interno: construye el snapshot de filtros desde los inputs actuales
    .capturar_filtros <- function() {
      list(
        periodo     = input$FT_Periodo,
        fecha       = input$FT_Fecha,
        sin_factura = input$FT_Fechas,
        linneg      = setdiff(input$FT_LinNeg,    ""),
        segmento    = setdiff(input$FT_Segmento,  ""),
        asesor      = setdiff(input$FT_Asesor,    ""),
        categoria   = setdiff(input$FT_Categoria, ""),
        producto    = setdiff(input$FT_Producto,  "")
      )
    }
    
    # Inicializacion: preseleccion de asesor por usuario
    observeEvent(usuario(), ignoreNULL = FALSE, {
      req(usuario())
      updatePickerInput(session, "FT_Asesor", selected = usuario())
    })
    
    # Cascada: linea de negocio -> categoria y producto
    observeEvent(input$FT_LinNeg, ignoreNULL = FALSE, ignoreInit = FALSE, {
      req(productos_cache$get())
      linneg <- setdiff(input$FT_LinNeg, "")
      req(length(linneg) > 0)
      
      categorias <- productos_cache$get() %>%
        filter(LinNeg %in% linneg) %>%
        pull(Categoria) %>%
        unique() %>%
        sort()
      categorias <- categorias[!is.na(categorias)]
      
      updatePickerInput(session, "FT_Categoria",
                        choices  = categorias,
                        selected = categorias,
                        options  = .picker_opts_update(categorias, fem = TRUE))
      
      # Puebla producto directamente desde linneg para evitar dependencia
      # del observer de categoria que puede no haberse ejecutado aun
      productos <- productos_cache$get() %>%
        filter(LinNeg %in% linneg) %>%
        pull(Producto) %>%
        unique() %>%
        sort()
      productos <- productos[!is.na(productos)]
      
      updatePickerInput(session, "FT_Producto",
                        choices  = productos,
                        selected = productos,
                        options  = .picker_opts_update(productos, fem = FALSE))
    })
    
    # Cascada: categoria -> producto
    observeEvent(input$FT_Categoria, ignoreNULL = FALSE, ignoreInit = FALSE, {
      req(productos_cache$get())
      categorias <- setdiff(input$FT_Categoria, "")
      
      if (length(categorias) == 0) {
        updatePickerInput(session, "FT_Producto", choices = character(0), selected = NULL)
        return()
      }
      
      productos <- productos_cache$get() %>%
        filter(Categoria %in% categorias) %>%
        pull(Producto) %>%
        unique() %>%
        sort()
      productos <- productos[!is.na(productos)]
      
      updatePickerInput(session, "FT_Producto",
                        choices  = productos,
                        selected = productos,
                        options  = .picker_opts_update(productos, fem = FALSE))
    })
    
    # Disparo inicial: espera a que FT_Producto este poblado para capturar filtros
    # once = TRUE garantiza que solo ocurre una vez en la vida del modulo
    observeEvent(input$FT_Producto, ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE, {
      filtros_aplicados(.capturar_filtros())
      .cascada_lista(TRUE)
    })
    
    # Disparo manual: captura filtros al presionar Aplicar
    observeEvent(input$FT_Aplicar, ignoreNULL = FALSE, {
      filtros_aplicados(.capturar_filtros())
    })
    
    return(filtros_aplicados)
  })
}
CompetenciaUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    div(
      fluidRow(
        column(
          12,
          selectizeInput(
            ns("COMP_Nombre"),
            label = Obligatorio("Nombre de la Competencia"),
            choices = "",
            multiple = FALSE,
            options = list(
              create = TRUE,
              placeholder = "Escriba o seleccione una competencia..."
            ),
            width = "100%"
          )
        )
      ),
      fluidRow(
        column(
          12,
          textAreaInput(
            ns("COMP_Observaciones"),
            label = h6("Observaciones"),
            placeholder = "Información adicional sobre la competencia...",
            width = "100%", rows = 3
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            style = "display: flex; justify-content: center; gap: 15px; margin: 20px 0;",
            actionBttn(
              inputId = ns("COMP_Guardar"),
              label = "Guardar Competencia",
              icon = icon("save"), style = "unite",
              color = "danger", size = "xs"
            )
          )
        )
      )
    )
  )
}
Competencia <- function(id, dat_ind, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Valor UGQ actual
    ugq_actual <- reactive({
      dat_ind() %>%
        filter(Item == "Costo UGQ Racafé") %>%
        pull(Valor)
    })
    
    # Cargar tabla de competencia para validar existencia
    datos_competencia <- reactive({
      df <- CargarDatos("CRMNALCOMPETENCIA")
      if (is.null(df)) {
        df <- data.frame(
          UsuarioCrea = character(),
          FechaHoraCrea = as.POSIXct(character()),
          Competencia = character(),
          PrecioUGQ = numeric(),
          Observaciones = character(),
          stringsAsFactors = FALSE
        )
      }
      df
    })
    
    observe({
      comp_existentes <- sort(unique(na.omit(datos_competencia()$Competencia)))
      updateSelectizeInput(
        session, "COMP_Nombre",
        choices = comp_existentes,
        server = TRUE
      )
    })
    
    # Validación
    validar_formulario <- function() {
      errores <- c()
      
      # Nombre obligatorio
      if (is.null(input$COMP_Nombre) || trimws(input$COMP_Nombre) == "") {
        errores <- c(errores, "El nombre de la competencia es obligatorio")
      }
      
      # Verificar que la competencia no exista ya en la tabla
      if (!is.null(input$COMP_Nombre) && trimws(input$COMP_Nombre) != "") {
        existe <- any(
          tolower(trimws(datos_competencia()$Competencia)) ==
            tolower(trimws(input$COMP_Nombre))
        )
        if (existe) {
          errores <- c(
            errores,
            paste0(
              "La competencia '", trimws(input$COMP_Nombre),
              "' ya existe en la tabla CRMNALCOMPETENCIA. No se puede volver a registrar."
            )
          )
        }
      }
      
      errores
    }
    
    # Habilitar/deshabilitar botón Guardar
    observe({
      errores <- validar_formulario()
      if (length(errores) == 0) {
        shinyjs::enable("COMP_Guardar")
      } else {
        shinyjs::disable("COMP_Guardar")
      }
    })
    
    # Guardar registro
    guardar_registro <- function() {
      tryCatch({
        df_row <- data.frame(
          UsuarioCrea = usr(),
          FechaHoraCrea = Sys.time(),
          Competencia = trimws(input$COMP_Nombre),
          Observaciones = trimws(input$COMP_Observaciones %||% ""),
          PrecioUGQ = ugq_actual() %||% NA_real_,
          stringsAsFactors = FALSE
        )
        
        AgregarDatos(df_row, "CRMNALCOMPETENCIA")
        TRUE
      }, error = function(e) {
        warning("Error al guardar: ", e$message)
        FALSE
      })
    }
    
    # Evento Guardar
    observeEvent(input$COMP_Guardar, {
      errores <- validar_formulario()
      if (length(errores) > 0) {
        showNotification(
          paste(errores, collapse = "\n"),
          type = "error", duration = 5
        )
        return()
      }
      
      if (guardar_registro()) {
        showNotification(
          "Competencia guardada exitosamente",
          type = "message", duration = 4
        )
        
        # Limpiar formulario
        updateSelectizeInput(session, "COMP_Nombre", selected = "")
        updateTextAreaInput(session, "COMP_Observaciones", value = "")
        
      } else {
        showNotification(
          "Error al guardar la competencia. Intente nuevamente.",
          type = "error", duration = 5
        )
      }
    })
  })
}
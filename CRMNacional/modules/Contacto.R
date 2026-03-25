## Modulo Contacto ------
ContactoUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, uiOutput(ns("Titulo")))
    ),
    br(),
    fluidRow(
      column(6, h5("Últimos contactos"), DTOutput(ns("Contactos"))),
      column(6, style = "text-align: right;",  
             textAreaInput(ns("Comentarios"), h6("Comentarios"), "",
                           placeholder="Ingrese comentarios", width = "100%",
                           height = "200px"),
             actionBttn(inputId = ns("Comentario"), label = "Guardar", 
                        style = "bordered",  color = "danger", 
                        icon = icon("save"), 
                        size = "sm")
      )
    ),
    tags$hr(style = "border-color: grey;"),
    h5("Programar Recordatorio"),
    fluidRow(
      column(3, 
             ListaDesplegable(ns("Asesor"), label = "Asesor",
                              choices = Choices()$personas,  selected = NULL, multiple = FALSE)
             ),
      column(3, airDatepickerInput("FechaCorreo", Obligatorio("Fecha y Hora del Recordatorio"), 
                                   minDate = Sys.time(),
                                   timepicker = T, 
                                   timepickerOpts = timepickerOptions(minHours = 7, 
                                                                      maxHours = 17),
                                   width = "100%")
             ),
      column(6, style = "text-align: right;",
             textAreaInput(ns("Mensaje"), h6("Mensaje"), "",
                           placeholder="Ingrese el Mensaje", width = "100%",
                           height = "200px"),
             actionBttn(inputId = ns("Mensaje"), label = "Programar Mensaje", 
                        style = "bordered",  color = "danger", 
                        icon = icon("calendar"), 
                        size = "sm")
      )
    )  
  )
}
Contacto <- function(id, tit) {
  moduleServer(id, function(input, output, session) {
    
    # usr <- "hcyate"
    usr <- session$user
    output$Titulo <- renderUI({
      h4(paste0("Registro de contacto para: ",tit()))
    })
    output$Contactos <- renderDT({
      ImprimirTablaContacto(tit())
    })
    observeEvent(input$Comentario, {
      
      nom <- connectapi::get_users(cont2, limit = 100) %>%
        mutate(Nombre = paste(first_name, last_name)) %>%
        filter(username == usr) %>%
        .$Nombre
      
      aux1 <- data.frame(
        Usuario = nom,
        PerCod = NA,
        PerRazSoc = tit(),
        Canal = "MANUAL",
        Fecha = Sys.time(),
        Comentarios = input$Comentarios
      )
      
      SubirDatos(aux1, "CRMNALCONT")
      shinyjs::reset("Comentarios")
      
      toastr_success(paste("Contacto almacenado exitosamente"))
      
    })
    observeEvent(input$Mensaje, {
      
      toastr_success(paste("Recordatorio almacenado exitosamente"))
      
    })
    
  })
}

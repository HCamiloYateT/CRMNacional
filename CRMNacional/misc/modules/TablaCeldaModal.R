## Modulo datatable seleccion de celda ----
TablaModalCeldaUI <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns("Tabla"), width = "100%"),
    modalDialogUI(
      modalId = ns("ModalEditar"),
      title = "",  
      button = NULL,
      easyClose = TRUE,  
      footer = actionButton(ns("Cerrar_ModalEditar"), "Cerrar"),  
      FormularioLeadsUI(ns("Editar"), "editar")
    ),
    modalDialogUI(
      modalId = ns("ModalContacto"),
      title = "",  
      button = NULL,
      easyClose = TRUE,  
      footer = actionButton(ns("Cerrar_ModalContacto"), "Cerrar"),  
      ContactoUI(ns("Contacto"))
    )
  )
}
TablaModalCelda <- function(id, dt, df, usr, rv) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    selected_value <- reactiveVal(NULL)
    
    output$Tabla <- renderDataTable({
      dt()
    })
    proxy <- dataTableProxy("Tabla")
    observeEvent(input$Tabla_cells_selected, {
      req(input$Tabla_cells_selected)
      
      selected_row <- input$Tabla_cells_selected[1]
      selected_col <- input$Tabla_cells_selected[2]
      
      col <- names(dt()$x$data)[selected_col+1]
      selected_value(dt()$x$data[selected_row, "PerRazSoc"])
      
      if (col == "Editar") {
        showModalUI("ModalEditar")
      } 
      else if (col == "Contacto") {
        showModalUI("ModalContacto")
      } 
      
      proxy %>%
        selectCells(NULL)
      
      
    })
    
    ## Ejecucion de los modulos.
    v = FormularioLeads("Editar", rv = rv, usr(), tit = selected_value)
    w = Contacto("Contacto", tit = selected_value)
    
    ## Botones de cierre de modal.
    observeEvent(input$Cerrar_ModalEditar, {
      hideModalUI("ModalEditar")
      proxy %>%
        selectCells(NULL)
    })
    observeEvent(input$Cerrar_ModalContacto, {
      hideModalUI("ModalContacto")
      proxy %>%
        selectCells(NULL)
    })
    
    return(v)
    
  })
}
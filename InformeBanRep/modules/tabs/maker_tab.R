maker_tab <- tabPanel("Informe de Derivados",
                      filtros,
                      uiOutput("Titulo"),
                      dataTableOutput("Informe"),
                      br(),br(),
                      fluidRow(
                        column(2),
                        column(8,
                               fluidRow(
                                 column(6, downloadBttn("Descargar", block = T,
                                                        label = "Descargar", 
                                                        style = "bordered",
                                                        color = "danger",                                                 
                                                        icon = icon("download"))),
                                 column(6, actionBttn(inputId = "Enviar", block = T,
                                                      label = "Enviar Correo", 
                                                      style = "bordered",
                                                      color = "danger",                                                 
                                                      icon = icon("envelope")))
                                 )),
                        column(2)),
                      br(),br(),
                      )
areas <- c('Operaciones','Control Interno','Mesa de Negocios','Contabilidad','Talento Humano',
           'Auditoria','Trilladoras','Calidades','Información Financiera', 'Tesoreria', 'Industria Nacional')
periodicidad <- c('Tiempo Real','Diario','Semanal','Mensual')

portal <- tabPanel("",
                   fluidRow(
                     column(4, br(),br(),
                            textInput("Buscar", label = NULL , value = "", placeholder = "Buscar ...", width = "100%")),
                     column(4,
                            pickerInput("Area", label = h6("Área"), choices = areas, selected = areas, 
                                        multiple = T, width = "100%",
                                        options = list(`live-search` = TRUE,
                                                       `live-search-normalize` = TRUE,
                                                       `live-search-placeholder` = "Buscar",
                                                       `actions-box` = TRUE,
                                                       `deselect-all-text` = "Ninguna",
                                                       `select-all-text` = "Todas",
                                                       `none-selected-text` = "Ninguna",
                                                       `count-selected-text` = "Todas", virtualScroll=500,
                                                       `selected-text-format` = paste0("count > ", length(areas) -1)))
                            ),
                     column(3,
                            pickerInput("Periodicidad", label = h6("Periodicidad"), choices = periodicidad, selected = periodicidad, 
                                        multiple = T, width = "100%",
                                        options = list(`live-search` = TRUE,
                                                       `live-search-normalize` = TRUE,
                                                       `live-search-placeholder` = "Buscar",
                                                       `actions-box` = TRUE,
                                                       `deselect-all-text` = "Ninguna",
                                                       `select-all-text` = "Todas",
                                                       `none-selected-text` = "Ninguna",
                                                       `count-selected-text` = "Todas", virtualScroll=500,
                                                       `selected-text-format` = paste0("count > ", length(periodicidad) -1)))
                            ),
                     column(1, style = "text-align: right;",
                            h6("Vista"),
                            radioGroupButtons(
                              inputId = "Vista", label = NULL, size = "xs",
                              choices = c(`<span class="glyphicon glyphicon-list-alt"></span>` = "table",
                                          `<span class="glyphicon glyphicon-th-list"></span>` = "list"),
                              individual = TRUE, selected = "list", width = "100%", justified = T)
                            )
                     ),
                   conditionalPanel("input.Vista == 'table'",
                                    fluidRow(
                                      column(12, dataTableOutput("Tabla"))
                                      )
                                    ),
                   conditionalPanel("input.Vista == 'list'",
                                    fluidRow(
                                      column(12, uiOutput("Lista"))
                                      )
                                    ),
                   br()
                   )

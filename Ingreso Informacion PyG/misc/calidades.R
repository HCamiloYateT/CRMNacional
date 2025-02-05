calidades <- tagList(
  box(title = "Calidades", footer = "", status = "danger", solidHeader = F, 
      collapsible = T, collapsed = T, width = "100%",
      fluidRow(
        column(12,
               tabBox(
                 title = "", id = "Calidades", width = "100%",
                 tabPanel("Rechazos", icon = ph("prohibit"),
                          fluidRow(
                            column(2, 
                                   airDatepickerInput("FechaRechazos",
                                                      label = h5("Mes"),
                                                      value = fecha,
                                                      view = "months",
                                                      minView = "months",
                                                      dateFormat = "yyyy-mm"
                                   )
                            ),
                            column(9, 
                                   h5("Trilladoras"),
                                   fluidRow(column(12, rHandsontableOutput("Rechazos", width = "100%"))),
                                   h5("Arenales"),
                                   fluidRow(column(12, rHandsontableOutput("RechazosArenales", width = "100%")))
                                   ),
                            column(1, actionBttn(inputId = "GuardarRechazos", label = "Guardar",
                                                 style = "unite", color = "danger",
                                                 icon = icon("save")))
                          ),
                          tags$p("Sacos de 70 kilos")
                 ),
                 tabPanel("Reclamos", icon = ph("hand"),
                          fluidRow(
                            column(2, airDatepickerInput("FechaReclamos",
                                                         label = h5("Mes"),
                                                         value = fecha,
                                                         view = "months",
                                                         minView = "months",
                                                         dateFormat = "yyyy-mm")
                                   ),
                            column(9, 
                                   h5("Trilladoras"),
                                   fluidRow(column(12, rHandsontableOutput("Reclamos", width = "100%"))),
                                   h5("Arenales"),
                                   fluidRow(column(12, rHandsontableOutput("ReclamosArenales", width = "100%")))
                                   ),
                            column(1, actionBttn(inputId = "GuardarReclamos", label = "Guardar",
                                                 style = "unite", color = "danger",
                                                 icon = icon("save")))
                          ),
                          tags$p("Sacos de 70 kilos")
                          )
                 )
               )
        )
      )
  )
                   
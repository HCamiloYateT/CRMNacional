tal_humano <- tagList(
  box(title = "Talento Humano", footer = "", status = "danger", solidHeader = F, 
      collapsible = T, collapsed = T, width = "100%",
      fluidRow(
        column(12,
               tabBox(
                 title = "", id = "Calidades", width = "100%",
                 tabPanel("Movilizadores", icon = ph("user-circle"),
                          CajaHOTableUI("NumMovilizadores"),
                          tags$p("Número de personas")
                          ),
                 tabPanel("Planta de Personal", icon = ph("users-four"),
                          CajaHOTableUI("PlantaPersonal"),
                          tags$p("Número de personas")
                          ),
                 tabPanel("Horas Extras", icon = ph("calendar-plus"),
                          CajaHOTableUI("NumHorasExtra"),
                          tags$p("Número de horas")
                          ),
                 tabPanel("Empleados con Horas Extra", icon = ph("user-plus"),
                          CajaHOTableUI("EmpHorasExtras"),
                          tags$p("Número de empleados")
                          ),
                 tabPanel("Costos Horas Extra", icon = ph("money"),
                          CajaHOTableUI("ValHorasExtras"),
                          tags$p("Valores en pesos")
                          ),
                 tabPanel("Accidentes", icon = ph("first-aid-kit"),
                          CajaHOTableUI("NumAccidentes"),
                          tags$p("Número de Accidentes")
                          ),
                 tabPanel("Dias de ausencia por Accidentes", icon = ph("calendar-x"),
                          CajaHOTableUI("DiasAccidentes"),
                          tags$p("Número de días")
                          )
                 )
               )
        )
      )
  )
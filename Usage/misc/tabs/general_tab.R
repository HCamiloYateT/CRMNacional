general_tab <- tabPanel(title = NULL,
                       fluidRow(
                         column(6, 
                                h4(paste(Espacios(), "Usuarios") %>% HTML),
                                DTOutput("Usuarios")),
                         column(6, 
                                h4(paste(Espacios(), "Aplicaciones") %>% HTML),
                                DTOutput("Apps")
                                )
                         ),
                       br(),
                       accordion(id = "acc_usr", multiple = T, open = F,
                                 accordion_panel(title = "Consulta por Usuario", 
                                                 selectizeInput("Usuario", h6("Seleccione usuario"), choices = sort(unique(data$Nombre))),
                                                 fluidRow(
                                                   column(5, DTOutput("UsuarioApps")),
                                                   column(7, plotOutput("CalUsuario"))
                                                   )
                                                 )
                                 ),
                       br(),
                       accordion(id = "acc_app", multiple = T, open = F,
                         accordion_panel(title = "Consulta por Aplicación", 
                                         selectizeInput("App", h6("Seleccione Aplicación"), choices = sort(unique(data$App))),
                                         fluidRow(
                                           column(5, DTOutput("AppsUsuarios")),
                                           column(7, plotOutput("CalApps"))
                                           )
                                         )
                         )
                       )
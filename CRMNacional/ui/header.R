header <- bs4DashNavbar(status = "white", border = FALSE,
                        sidebarIcon = icon("bars"), 
                        title = dashboardBrand(title = "CRM Nacional",
                                               href = "https://analitica.racafe.com/PortalAnalitica/",
                                               image = "logo2.png"),
                        controlbarIcon = icon("gears"),
                        leftUi = tagList(uiOutput("user"),
                                         uiOutput("espacio"),
                                         tags$li(class='dropdown', 
                                                 div(style = "display: flex; justify-content: center; 
                                                     align-items: center; height: 100%; width: 100%;",
                                                     actionBttn("FT_Actualizar", icon = icon("sync"), size = "xs")
                                                     )
                                                 )
                                         ),
                        rightUi = tagList(
                          customDropdownMenu(icon = icon("arrow-trend-up"), showBadge = FALSE,
                                             showHeader = FALSE,
                                             IndicadoresUI("Indicadores")
                                             ),
                          dropdownMenu(badgeStatus = "danger", type = "notifications", 
                                       headerText = "", 
                                       NotificacionesUI("Notificaciones")
                                       )
                          )
                        )
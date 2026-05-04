header <- bs4DashNavbar(status = "white", border = FALSE,
                        sidebarIcon = icon("bars"), 
                        title = dashboardBrand(title = "CRM Nacional",
                                               href = "https://analitica.racafe.com/PortalAnalitica/",
                                               image = "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/img/logo2.png"),
                        controlbarIcon = icon("gears"),
                        leftUi = tagList(
                                         tags$li(class = "dropdown",
                                                 style = "display:flex;align-items:center; gap:8px;padding:8px 12px;cursor:default;",
                                                 uiOutput("user"),
                                                 actionBttn("FT_Actualizar", icon = icon("sync"), size = "xs")
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
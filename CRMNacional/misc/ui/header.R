header <- bs4DashNavbar(status = "white", border = FALSE, sidebarIcon = icon("bars"), 
                        title = dashboardBrand(title = "CRM Nacional",
                                               href = "https://analitica.racafe.com/PortalAnalitica/",
                                               image = "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/img/logo2.png"),
                        controlbarIcon = icon("gears"),
                        leftUi = tagList(
                          tags$li(class = "dropdown",
                                  style = "display:flex;align-items:center; gap:8px;padding:8px 12px;cursor:default;",
                                  tags$span(uiOutput("user")),
                                  racafeShiny::Boton("FT_Actualizar", label = NULL, icono = "sync",
                                                     size = "xxs", title = "Actualizar", color_fondo = "#6c757d", 
                                                     color_hover = "#DA291C")
                                  )
                          ),
                        rightUi = tagList(MenuHeaderUI("MenuIndicadores", icon = shiny::icon("arrow-trend-up"), min_width = "360px"),
                                          MenuHeaderUI("MenuNotificaciones", icon = shiny::icon("bell"), min_width = "360px"),
                                          MenuHeaderUI("MenuClientes", icon = shiny::icon("users"), min_width = "360px"),
                                          MenuHeaderUI("MenuProductos", icon = shiny::icon("delicious"), min_width = "360px")
                                          )
                        )
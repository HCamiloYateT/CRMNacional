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
                                                     color_hover = "#DA291C"),
                                  racafeShiny::BotonDescarga("FT_DescargarClientes", icono = "users",
                                                             size = "xxs", title = "Descargar Clientes", color_fondo = "#6c757d", 
                                                             color_hover = "#DA291C")
                          )
                        ),
                        rightUi = tagList(MenuHeaderUI("MenuIndicadores", icon = shiny::icon("arrow-trend-up"),
                                                       min_width = "400px", title = "Indicadores"),
                                          MenuHeaderUI("MenuTareas", icon = shiny::icon("bars-progress"),
                                                       min_width = "360px", title = "Tareas"),
                                          # Restringidos a JONATHAN CAÑON — id añadido para toggle server-side ----
                                          htmltools::tagAppendAttributes(
                                            MenuHeaderUI("MenuClientes", icon = shiny::icon("users"),
                                                         min_width = "360px", title = "Clientes sin información"),
                                            id = "li_menu_clientes"
                                          ),
                                          htmltools::tagAppendAttributes(
                                            MenuHeaderUI("MenuClientesAnt", icon = shiny::icon("user-clock"),
                                                         min_width = "360px", title = "Clientes con Información antigüa"),
                                            id = "li_menu_clientes_ant"
                                          ),
                                          htmltools::tagAppendAttributes(
                                            MenuHeaderUI("MenuMigrarCartera", icon = shiny::icon("people-arrows"),
                                                         min_width = "360px", title = "Migrar Cartera"),
                                            id = "li_menu_migrar_cartera"
                                            ),
                                          htmltools::tagAppendAttributes(
                                            MenuHeaderUI("MenuProductos", icon = shiny::icon("delicious"),
                                                         min_width = "360px", title = "Productos"),
                                            id = "li_menu_productos"
                                          )
                        )
)
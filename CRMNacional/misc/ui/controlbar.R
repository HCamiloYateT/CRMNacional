controlbar <- bs4DashControlbar(id = "controlbar", skin = "light", pinned = NULL, 
                                overlay = FALSE, width = "500px",
                                controlbarMenu(
                                  id = "controlbarMenu",
                                  type = "tabs",
                                  controlbarItem("Filtros",
                                                 FiltrosUI("Filtros")
                                                 ),
                                  controlbarItem("Crear Tareas/Notas",
                                                 TaskCreationUI("Tareas")),
                                  controlbarItem("Crear Contacto",
                                                 FormularioContactoUI("Contacto")
                                                 ),
                                  controlbarItem("Cotizador",
                                                 CotizacionUI("Cotizador")
                                                 ),
                                  controlbarItem("Competencia",
                                                 CompetenciaUI("Competencia")
                                                 ),
                                  controlbarItem("Productos",
                                                 GestionProductoUI("Productos")
                                                 ),
                                  controlbarItem("Categorias",
                                                 h6("Categorias")
                                                 )
                                  )
                                )
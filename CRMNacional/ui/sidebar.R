sidebar <- bs4DashSidebar(status = "danger", expandOnHover = FALSE,
                          bs4SidebarMenu(id = "menu_principal",
                                         bs4SidebarMenuItem("Hoja de Trabajo", icon = icon("clipboard-list"), selected = FALSE, 
                                                            bs4SidebarMenuSubItem("Reportes", tabName = "HT_Reportes", icon = icon("file-contract")),
                                                            bs4SidebarMenuSubItem("Comparacion Indicadores", tabName = "HT_Indicadores", icon = icon("chart-bar")),
                                                            bs4SidebarMenuSubItem("Calculadoras", tabName = "HT_Calculadoras", icon = icon("calculator")),
                                                            bs4SidebarMenuSubItem("Presupuesto", tabName = "HT_Presupuesto", icon = icon("file-invoice-dollar")),
                                                            bs4SidebarMenuSubItem("Pendientes", tabName = "HT_Pendientes", icon = icon("exclamation-triangle")),
                                                            bs4SidebarMenuSubItem("Notas y Tareas", tabName = "HT_Tareas", icon = icon("tasks"))
                                         ),
                                         bs4SidebarMenuItem("Oportunidades", icon = icon("bullseye"), selected = FALSE,
                                                            bs4SidebarMenuSubItem("Registro", tabName = "OP_Registro", icon = icon("clipboard-list")),
                                                            bs4SidebarMenuSubItem("Listado", tabName = "OP_Listado", icon = icon("table")),
                                                            bs4SidebarMenuSubItem("Seguimiento", tabName = "OP_Seguimiento", icon = icon("chart-line"))
                                                            ),
                                         bs4SidebarMenuItem("Clientes", icon = icon("users"), 
                                                            bs4SidebarMenuSubItem("Resumen", tabName = "CL_Resumen", icon = icon("list")),
                                                            bs4SidebarMenuSubItem("Presupuesto", tabName = "CL_Presupuesto", icon = icon("file-invoice-dollar")),
                                                            bs4SidebarMenuSubItem("Segmentación RFM", tabName = "CL_RFM", icon = icon("layer-group"))
                                         ),
                                         bs4SidebarMenuItem("Clientes a Recuperar", icon = icon("user-friends"),
                                                            bs4SidebarMenuSubItem("Resumen", tabName = "CR_Resumen", icon = icon("list")),
                                                            bs4SidebarMenuSubItem("Presupuesto", tabName = "CR_Presupuesto", icon = icon("file-invoice-dollar")),
                                                            bs4SidebarMenuSubItem("Embudo", tabName = "CR_Embudo", icon = icon("funnel-dollar")),
                                                            bs4SidebarMenuSubItem("Segmentación RFM", tabName = "CR_RFM", icon = icon("layer-group"))
                                         ),
                                         bs4SidebarMenuItem("Leads", icon = icon("bullseye"), 
                                                            bs4SidebarMenuSubItem("Listado", tabName = "LE_Listado", icon = icon("list-ul")),
                                                            bs4SidebarMenuSubItem("Embudo", tabName = "LE_Embudo", icon = icon("funnel-dollar"))
                                         ),
                                         bs4SidebarMenuItem("Consulta Individual", icon = icon("user"), tabName = "IN_Consulta")
                          )
)
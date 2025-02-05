info_fina <- tagList(
  box(title = "PLAN COMERCIAL INDUSTRIA NACIONAL", footer = "", status = "danger", solidHeader = F, 
      collapsible = T, collapsed = T, width = "100%",
      h5("MÁRGEN POR NEGOCIO"),
      CajaHOTableUI("Margen")
      ),
  box(title = "INFORMACIÓN FINANCIERA", footer = "", status = "danger", solidHeader = F, 
      collapsible = T, collapsed = T, width = "100%",
      h5("Parámetros Informe de Gestión"),
      fluidRow(
        column(12,
               tabBox(title = "", width = 12,
                      tabPanel("Clasificación de Productos", icon = ph("rows"),
                               fluidRow(
                                 column(1),
                                 column(10,
                                        rHandsontableOutput("ClasProductos", width = "100%")
                                        ),
                                 column(1,
                                        actionBttn(inputId = "GuardarClasProductos", label = "Guardar",
                                                   style = "unite", color = "danger",
                                                   icon = icon("save"))
                                        )
                                 ),
                               br(),
                               tags$p("Los tipos de costo pueden ser: EXCELSOS, SUPREMOS, DUNKIN Y MILLSTONES, PRODUCTO DE COLOMBIA,
                                      PRACTICES, 4C Y CRECER,CERTIFICADOS, REGIONALES, PERFIL DE TAZA, FINCA, A LA MEDIDA o CONSUMOS Y PASILLAS CONSUMOS Y PASILLAS SACOS DE 70KG"),
                               ),
                      tabPanel("Costo Estándar por Producto", icon = ph("money"),
                               fluidRow(
                                 column(2, pickerInput("FechaValores", label = "Fecha", 
                                                       choices = CargarDatos("ANPYGCOSES")$Fecha %>% unique, 
                                                       selected = max(CargarDatos("ANPYGCOSES")$Fecha), 
                                                       multiple = F)
                                        ),
                                 column(9, 
                                        fluidRow(column(12, rHandsontableOutput("ValoresXsaco", width = "100%")))
                                        ),
                                 column(1, actionBttn(inputId = "GuardarValoresXsaco", label = "Guardar",
                                                      style = "unite", color = "danger",
                                                      icon = icon("save")))
                                 )
                               ),
                      tabPanel("Presupuesto de Compras (Kilos)", icon = ph("stack-simple"),
                               CajaHOTableUI("PresupuestoCompras"),
                               tags$p("Presupuesto de compras en kilogramos")
                               ),
                      tabPanel("Kilos en Punto de Compra", icon = ph("stack-simple"),
                               CajaHOTableUI("KilosPuntoCompra")
                               ),
                      tabPanel("Otros Ingresos", icon = ph("currency-dollar"),
                               CajaHOTableUI("Extraordinarios")
                               ),
                      tabPanel("Otros Costos", icon = ph("currency-dollar"),
                               CajaHOTableUI("OtrosCostos")
                               ),
                      tabPanel("Metas", icon = ph("crosshair"),
                               fluidRow(
                                 column(2,
                                        selectInput("FechaMetas", label = "Fecha", 
                                                    choices = CargarDatos("ANPYGMETAS")$Fecha %>% unique, 
                                                    selected = max(CargarDatos("ANPYGMETAS")$Fecha), 
                                                    multiple = F)
                                        ),
                                 column(9, rHandsontableOutput("MetaDespachos")),
                                 column(1, actionBttn(inputId = "GuardarMetaDespachos", label = "Guardar",
                                                      style = "unite", color = "danger",
                                                      icon = icon("save")))
                                 )
                               )
                      )
               )
        ),
      h5("Despachos"),
      fluidRow(
        column(12,
               tabBox(
                 title = "", id = "Despachos", width = "100%",
                 tabPanel("Trillas para terceros", icon = ph("arrow-arc-left"),
                          h5("Sacos Trillados - Excelsos"),
                          CajaHOTableUI("TrillasTerceros"),
                          tags$p("Sacos de 70 kilogramos"),
                          h5("Sacos Trillados - Coproductos"),
                          CajaHOTableUI("TrillasTercerosCop")),
                 tabPanel("Traslados Entre Trilladoras", icon = ph("package"),
                          CajaHOTableUI("TrasladosTrll")),
                 tabPanel("% Oportunidad Logistica", icon = ph("package"),
                          CajaHOTableUI("OpLogistica")),
                 tabPanel("% Oportunidad Logistica Acumulada", icon = ph("package"),
                          CajaHOTableUI("OpLogisticaAcum"))
                 )
               )
        )
      ),
  box(title = "Costos", footer = "", status = "danger", solidHeader = F, 
      collapsible = T, collapsed = T, width = "100%",
      fluidRow(
        column(12,
               tabBox(
                 title = "", id = "Costos", width = "100%",
                 tabPanel("Costos de Viajes", icon = ph("airplane-takeoff"),
                          CajaHOTableUI("CostosViajes"),
                          tags$p("Cifras en Miles de Pesos")
                          ),
                 tabPanel("Consumo Energía", icon = ph("lightbulb-filament"),
                          CajaHOTableUI("ConsumoEnergia"),
                          tags$p("Energía consumida en KW")
                 ),
                 tabPanel("Costo de Energía", icon = ph("lightbulb-filament"),
                          CajaHOTableUI("CostoEnergia"),
                          tags$p("Valor que va al PyG. Cifra en miles de pesos")
                 ),
                 tabPanel("Costo de Movilización", icon = ph("person"),
                          CajaHOTableUI("CostoMovilizacion"),
                          tags$p("Valor que suma al costo de movilización y resta al costo de personal. Cifra en miles de pesos")
                 ),
                 tabPanel("Financieros Arenales", icon = ph("money"),
                          CajaHOTableUI("FinancierosArenales"),
                          tags$p("Valor que resta al costo financiero, cifra en pesos")
                 )
               ))
        )
      ),
  box(title = "Prespuestos", footer = "", status = "danger", solidHeader = F, 
      collapsible = T, collapsed = T, width = "100%",
      fluidRow(
        column(12,
               tabBox(
                 title = "", id = "Costos", width = "100%",
                 tabPanel("Presupuestos de Costos", icon = ph("currency-dollar"),
                          fluidRow(
                            column(6, 
                                   fluidRow(
                                     column(6, selectInput("FechaPtoCosto", label = "Fecha", 
                                                            choices = CargarDatos("ANPYGPTCOS")$Fecha %>% unique, 
                                                            selected = max(CargarDatos("ANPYGPTCOS")$Fecha), 
                                                            multiple = F)
                                            ),
                                     column(6, selectInput("GrupoPtoCosto", "Grupo de Costo", 
                                                            choices = CargarDatos("ANPYGPTCOS")$Grupo %>% unique)
                                            )
                                     )
                                   )
                            ),
                          fluidRow(
                            column(11, rHandsontableOutput("Prespuestos", width = "100%")),
                            column(1, actionBttn(inputId = "GuardarPrespuestos", label = "Guardar",
                                                 style = "unite", color = "danger",
                                                 icon = icon("save")))
                            ),
                          tags$p("Valores en Miles de pesos"),
                          tags$p("Los tipos de costo pueden ser: FIJO o VARIABLE"),
                          
                          )
                 )
               )
        )
      )
  )
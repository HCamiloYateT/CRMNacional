# Valores mínimos requeridos ----
cho_res <- Unicos(c("", "CMEDINA", "JGCANON", "GACORREDOR", "LABOYACA", "TRESTREPO"))
cho_seg <- c("", "GRANDES", "MEDIANO", "DETAL")
cho_escu <- c("", "CONTACTADO", "POR CONTACTAR", "MUERTO")
cho_neg_con <- c("", "OPORTUNIDAD INMEDIATA", "OPORTUNIDAD CORTO PLAZO", "OPORTUNIDAD LARGO PLAZO", "OPORTUNIDAD ESPORÁDICA", "DESCARTADO")
cho_neg_mue <- c("", "NUMERO ERRADO", "FUERA DE LÍNEA", "BLOQUEO CONTROL INTERNO", "NUNCA CONTESTA")

# Modulo ----
IndividualUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("wrapper"))
  )
}
Individual <- function(id, dat, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1) Wrapper: no mostrar nada si no hay filas  -----
    output$wrapper <- renderUI({
      if (nrow(dat()) == 0) return(NULL)
      tagList(
        tags$style(HTML(".shiny-input-container { width: 100% !important; }")),
        Saltos(),
        uiOutput(ns("Titulo")),
        Saltos(),
        fluidRow(
          column(3,
                 box(title = "Información de Contacto", width = 12, collapsible = TRUE, collapsed = TRUE,
                     gt_output(ns("IND_Contacto"))),
                 box(
                   title = "Datos de Cliente",
                   width = 12,
                   collapsible = TRUE,
                   collapsed = TRUE,
                   fluidRow(
                     textInput(ns("IND_NIT"), label = h6("NIT Principal"),
                               placeholder = "Sin dígito de verificación", width = "100%"),
                     pickerInput(ns("IND_Asesor"), label = h6("Asesor"), width = "100%",
                                 choices = cho_res, multiple = FALSE, options = pick_opt(cho_res)),
                     pickerInput(ns("IND_Responsable"), label = h6("Responsable"), width = "100%",
                                 choices = cho_res, multiple = FALSE, options = pick_opt(cho_res)),
                     pickerInput(ns("IND_Segmento"), label = h6("Segmento"), width = "100%",
                                 choices = cho_seg, multiple = FALSE, options = pick_opt(cho_seg)),
                     div(style = "width: 100%;", uiOutput(ns("uiEstadoCliente"))),
                     div(style = "width: 100%;", uiOutput(ns("uiEstadoNegocio"))),
                     div(style = "width: 100%;", uiOutput(ns("uiRazonDescartado"))),
                     div(style = "width: 100%;", uiOutput(ns("uiDescartadoPrecio"))),
                     div(style = "width: 100%;", uiOutput(ns("uiDescartadoCalidad"))),
                     autonumericInput(ns("IND_FrecuenciaDias"), label = h6("Frecuencia Días"), value = NA, width = "100%"),
                     autonumericInput(ns("IND_MesesRecuperar"), label = h6("Meses cliente a recuperar"), value = NA, width = "100%", decimalPlaces = 0),
                     pickerInput(ns("IND_Excluir"), label = h6("Excluir"), width = "100%",
                                 choices = c("SI", "NO"), multiple = FALSE, options = pick_opt(c("SI", "NO"))),
                     div(style = "display: flex; justify-content: center; margin-top: 10px;",                       
                         actionButton(ns("guardar_datos_cliente"), "Guardar cambios", icon = icon("save"), class = "btn-danger")
                     )
                   )
                 ),
                 box(
                   title = "Metas",
                   width = 12,
                   collapsible = TRUE,
                   collapsed = TRUE,
                   h6("Meta Mensual"),
                   autonumericInput(ns("IND_SacosMes"), label = h6("Sacos Mes"), value = NA, width = "100%"),
                   autonumericInput(ns("IND_MargenMes"), label = h6("Márgen Mes"), value = NULL, currencySymbol = "$", width = "100%"),
                   h6("Meta Semanal"),
                   autonumericInput(ns("IND_SacosSemana"), label = h6("Sacos Semanal"), value = NA, width = "100%"),
                   autonumericInput(ns("IND_MargenSemana"), label = h6("Márgen Semanal"), value = NULL, currencySymbol = "$", width = "100%"),
                   div(style = "display: flex; justify-content: center; margin-top: 10px;",                       
                       actionButton(ns("guardar_datos_meta"), "Guardar Metas", icon = icon("save"), class = "btn-danger")
                   )
                 )
          ),
          column(
            9,
            box(
              title = "Detalle de Lote",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              fluidRow(
                column(4, createSwitch("IND_PendProducir", "Pend. por Producir", ns = ns)),
                column(4, createSwitch("IND_PendDespachar", "Pend. por despachar", ns = ns)),
                column(4, createSwitch("IND_DespPendFacturar", "Despachados pend. por facturar", TRUE, ns = ns))
              ),
              gt_output(ns("IND_Lotes"))
            )
          )
        ),
        Saltos(1),
        fluidRow(
          box(
            title = "Presupuesto",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            PresupuestoUI(ns("PresupuestoCliente")))
        ),
        fluidRow(
          box(
            title = "Productos y Categorías",
            width = 4,
            collapsible = TRUE,
            collapsed = TRUE,
            fluidRow(
              column(6,
                     BotonesRadiales(
                       inputId = "IND_Producto",
                       label = NULL,
                       choices = c("Categoría" = "Categoria", "Producto" = "Producto"),
                       selected = "Categoria",
                       ns = ns
                     )
              ),
              column(2),
              column(4, style = "text-align: right;",
                     BotonesRadiales(
                       inputId = "IND_VariableProducto",
                       label = NULL,
                       choices = c("Sacos", "Margen"),
                       selected = "Sacos",
                       ns = ns
                     )
              )
            ),
            plotlyOutput(ns("IND_DistrProducto"))
          ),
          box(title = "Histórico de Facturación",
              width = 8,
              collapsible = TRUE,
              collapsed = TRUE,
              fluidRow(
                column(4,
                       BotonesRadiales(
                         inputId = "IND_TipoSerie",
                         label = NULL,
                         choices = c("Histórico", "Mensual"),
                         selected = "Histórico",
                         ns = ns
                       )
                ),
                column(4),
                column(4, style = "text-align: right;",
                       BotonesRadiales(
                         inputId = "IND_VariableSerie",
                         label = NULL,
                         choices = c("Sacos", "Margen"),
                         selected = "Sacos",
                         ns = ns
                       )
                )
              ),
              plotlyOutput(ns("IND_Serie"))
          )
        )
        
      )
    })
    
    # 2) Título -----
    output$Titulo <- renderUI({
      if (nrow(dat()) > 0) {
        cliente <- dat() %>% pull(PerRazSoc) %>% Unicos()
        linneg <- dat() %>% pull(CLLinNegNo) %>% Unicos()
        h4(paste0("Análisis individual de: ", cliente, " en la línea de negocios: ", linneg))
      } else {
        h4("No existe información para la selección de cliente en esa línea de negocio")
      }
    })
    # 3) Información de contacto -----
    output$IND_Contacto <- render_gt({
      aux0 <- dat() %>%
        group_by(`Razón Social` = PerRazSoc) %>%
        summarise(
          `Contacto` = paste(unique(CliCont), collapse = "<br>"),
          `Dirección` = paste(unique(CliDir), collapse = "<br>"),
          `Ciudad` = paste(unique(CiuExtNom), collapse = "<br>"),
          `Teléfono` = paste(unique(CliTel), collapse = "<br>"),
          `Contacto Comercial` = paste(unique(CliConCom), collapse = "<br>"),
          `Teléfono Comercial` = paste(unique(CliTelCom), collapse = "<br>"),
          `EMail Comercial` = paste(unique(CliEmlCom), collapse = "<br>")
        ) %>%
        pivot_longer(`Razón Social`:`EMail Comercial`, names_to = "Item", values_to = "Registro")
      
      gt(aux0) %>%
        gt_minimal_style() %>%
        cols_label(Item = "", Registro = "")
    })
    # 4) Formulario Clientes -----
    data_ult_mod <- reactive({
      if (nrow(dat()) == 0) return(NULL)
      cliente <- dat() %>% pull(CliNitPpal) %>% Unicos()
      linneg <- dat() %>% pull(CLLinNegNo) %>% Unicos()
      
      CargarDatos("CRMNALCLIENTE") %>%
        mutate(FecProceso = as.Date(FecProceso)) %>%
        filter(CliNitPpal == cliente,
               LinNegocio  ==  linneg) %>%
        filter(FecProceso == max(FecProceso))
    })
    output$uiEstadoCliente <- renderUI({
      pickerInput(ns("IND_EstadoCliente"), label = h6("Estado del Cliente"), width = "100%",
                  choices = cho_escu, multiple = FALSE, options = pick_opt(cho_escu),
                  selected = {
                    if (nrow(dat()) == 0) {
                      NULL
                    } else {
                      ult_contacto <- data_ult_mod()$FecProceso
                      if (is.na(ult_contacto) || ult_contacto < PrimerDia(Sys.Date())) {
                        "POR CONTACTAR"
                      } else {
                        dat()$EstadoCliente[1]
                      }
                    }
                  })
    })
    output$uiEstadoNegocio <- renderUI({
      req(input$IND_EstadoCliente)
      if (input$IND_EstadoCliente == "POR CONTACTAR") {
        return(NULL)
      } else {
        pickerInput(ns("IND_EstadoNegocio"), label = h6("Estado del Negocio"), width = "100%",
                    choices = cho_neg_con, multiple = FALSE, options = pick_opt(cho_neg_con),
                    selected = dat()$EstadoNegocio[1])
      }
    })
    output$uiRazonDescartado <- renderUI({
      req(input$IND_EstadoNegocio)
      if (input$IND_EstadoNegocio == "DESCARTADO") {
        pickerInput(ns("IND_RazonDescartado"), label = h6("Razón Descartado"), width = "100%",
                    choices = c("", "PRECIO", "CALIDAD", "NO DA DATOS", "NO LE INTERESA"),
                    multiple = FALSE, options = pick_opt(c("", "PRECIO", "CALIDAD", "NO DA DATOS", "NO LE INTERESA")),
                    selected = dat()$RazonDescartado[1] %||% "")
      } else {
        return(NULL)
      }
    })
    output$uiDescartadoPrecio <- renderUI({
      req(input$IND_RazonDescartado)
      if (input$IND_RazonDescartado == "PRECIO") {
        autonumericInput(ns("IND_DescartadoPrecioInput"), label = h6("Precio Descartado"), value = dat()$DescartadoPrecio[1] %||% NA, currencySymbol = "$", width = "100%")
      } else {
        return(NULL)
      }
    })
    output$uiDescartadoCalidad <- renderUI({
      req(input$IND_RazonDescartado)
      if (input$IND_RazonDescartado == "CALIDAD") {
        textInput(ns("IND_DescartadoCalidadInput"), label = h6("Calidad Descartada"), value = dat()$DescartadoCalidad[1] %||% "", width = "100%")
      } else {
        return(NULL)
      }
    })
    observe({
      req(data_ult_mod())
      datos <- data_ult_mod()[1,]
      
      updateTextInput(session, "IND_NIT", value = datos$CliNitPpal %||% "")
      updatePickerInput(session, "IND_Asesor", selected = datos$Asesor %||% "")
      updatePickerInput(session, "IND_Responsable", selected = datos$Responsable %||% "")
      updatePickerInput(session, "IND_Segmento", selected = datos$Segmento %||% "")
      updateAutonumericInput(session, "IND_FrecuenciaDias", value = datos$FrecuenciaDias %||% NA)
      updateAutonumericInput(session, "IND_MesesRecuperar", value = datos$NumMesesRecuperar %||% 3)
      updatePickerInput(session, "IND_Excluir", selected = datos$Excluir %||% "NO")
      
      if (!is.null(input$IND_RazonDescartado) && input$IND_RazonDescartado == "PRECIO") {
        updateAutonumericInput(session, "IND_DescartadoPrecioInput", value = datos$DescartadoPrecio %||% NA)
      }
      if (!is.null(input$IND_RazonDescartado) && input$IND_RazonDescartado == "CALIDAD") {
        updateTextInput(session, "IND_DescartadoCalidadInput", value = datos$DescartadoCalidad %||% "")
      }
    })
    observeEvent(input$IND_EstadoCliente, {
      req(input$IND_EstadoCliente)
      
      cho_neg <- if (input$IND_EstadoCliente == "CONTACTADO") {
        c("", "OPORTUNIDAD INMEDIATA", "OPORTUNIDAD CORTO PLAZO", "OPORTUNIDAD LARGO PLAZO", "OPORTUNIDAD ESPORÁDICA", "DESCARTADO")
      } else if (input$IND_EstadoCliente == "MUERTO") {
        cho_neg_mue
      } else {
        cho_neg_con
      }
      
      updatePickerInput(session, "IND_EstadoNegocio", choices = cho_neg, selected = cho_neg[1])
    })
    observeEvent(input$guardar_datos_cliente, {
      req(dat())
      nit_input <- input$IND_NIT
      
      if (is.null(nit_input) || nit_input == "" || is.na(suppressWarnings(as.numeric(nit_input))) || as.numeric(nit_input) == 0) {
        showNotification("Error: El NIT no puede estar vacío ni ser cero y debe ser numérico", type = "error")
        return()
      }
      
      confirmSweetAlert(
        session = session,
        inputId = ns("confirm_guardar_cliente"),
        title = "Confirmar guardado",
        text = "¿Desea guardar los datos del cliente?",
        type = "warning",
        btn_labels = c("Cancelar", "Guardar"),
        btn_colors = c("#E7180B", "#1F7A55"),
        html = TRUE,
        width = "400px"
      )
    })
    observeEvent(input$confirm_guardar_cliente, {
      if (isTRUE(input$confirm_guardar_cliente)) {
        datos_actuales <- dat()[1, ]
        nit_input <- input$IND_NIT
        
        RazonDescartado <- {
          estado <- input$IND_EstadoNegocio
          razon <- input$IND_RazonDescartado
          if (is.null(estado) || length(estado) == 0) {
            NA
          } else if (estado == "DESCARTADO") {
            if (is.null(razon) || length(razon) == 0) NA else razon
          } else {
            NA
          }
        }
        
        DescartadoPrecio <- {
          estado <- input$IND_EstadoNegocio
          razon <- input$IND_RazonDescartado
          if (is.null(estado) || length(estado) == 0 || estado != "DESCARTADO") {
            NA
          } else if (razon == "PRECIO") {
            input$IND_DescartadoPrecioInput
          } else {
            NA
          }
        }
        
        DescartadoCalidad <- {
          estado <- input$IND_EstadoNegocio
          razon <- input$IND_RazonDescartado
          if (is.null(estado) || length(estado) == 0 || estado != "DESCARTADO") {
            NA
          } else if (razon == "CALIDAD") {
            input$IND_DescartadoCalidadInput
          } else {
            NA
          }
        }
        
        nuevo_registro <- data.frame(
          FecProceso = as.character(Sys.Date()),
          Usr = usr(),
          LinNegCod = datos_actuales$LinNegCod,
          LinNegocio = datos_actuales$CLLinNegNo,
          CLCliNit = datos_actuales$CliNitPpal,
          CliNitPpal = as.numeric(nit_input),
          Segmento = input$IND_Segmento,
          SSPpto = datos_actuales$PptoSacos,
          MNFCCPpto = datos_actuales$PptoMargen,
          Asesor = input$IND_Asesor,
          Responsable = input$IND_Responsable,
          EstadoCliente = input$IND_EstadoCliente,
          EstadoNegocio = ifelse(input$IND_EstadoCliente == "POR CONTACTAR", NA, input$IND_EstadoNegocio),
          RazonDescartado = RazonDescartado,
          DescartadoPrecio = DescartadoPrecio,
          DescartadoCalidad = DescartadoCalidad,
          FrecuenciaDias = ifelse(is.null(input$IND_FrecuenciaDias), NA, input$IND_FrecuenciaDias),
          NumMesesRecuperar = input$IND_MesesRecuperar,
          Excluir = input$IND_Excluir,
          stringsAsFactors = FALSE
        )
        
        tryCatch({
          SubirDatos(nuevo_registro, "CRMNALCLIENTE")
          showNotification("Datos guardados exitosamente", type = "message")
        }, error = function(e) {
          showNotification(paste("Error al guardar:", e$message), type = "error")
        })
      }
    })
    
    # 5) Metas Clientes ------
    data_ult_met <- reactive({
      if (nrow(dat()) == 0) return(NULL)
      cliente <- dat() %>% pull(PerRazSoc) %>% Unicos()
      linneg <- dat() %>% pull(CLLinNegNo) %>% Unicos()
      
      CargarDatos("CRMNALMETAS") %>%
        mutate(FechaHoraCrea = as.POSIXct(FechaHoraCrea)) %>%
        filter(PerRazSoc == cliente,
               LineaNegocio  ==  linneg) %>%
        filter(FechaHoraCrea >= PrimerDia(Sys.Date()))
    })
    observeEvent(input$guardar_datos_meta, {
      req(dat())
      
      confirmSweetAlert(
        session = session,
        inputId = ns("confirm_guardar_meta"),
        title = "Confirmar Meta",
        text = "¿Desea guardar la meta?",
        type = "warning",
        btn_labels = c("Cancelar", "Guardar"),
        btn_colors = c("#E7180B", "#1F7A55"),
        html = TRUE,
        width = "400px"
      )
    })
    observeEvent(input$confirm_guardar_meta, {
      if (isTRUE(input$confirm_guardar_meta)) {
        datos_actuales <- dat()[1, ]
        cliente <- dat() %>% pull(PerRazSoc) %>% Unicos()
        linneg <- dat() %>% pull(CLLinNegNo) %>% Unicos()
        
        nuevo_registro <- data.frame(Usuario = usr(),
                                     FechaHoraCrea = Sys.time(),
                                     PerRazSoc = cliente,
                                     LineaNegocio = linneg,
                                     MetaSacosMes = input$IND_SacosMes,
                                     MetaMargenMes = input$IND_MargenMes,
                                     MetaSacosSemana = input$IND_SacosSemana,
                                     MetaMargenSemana = input$IND_MargenSemana,
                                     stringsAsFactors = FALSE)
        
        tryCatch({
          SubirDatos(nuevo_registro, "CRMNALMETAS")
          showNotification("Meta guardada exitosamente", type = "message")
        }, error = function(e) {
          showNotification(paste("Error al guardar:", e$message), type = "error")
        })
      }
    })
    observeEvent(data_ult_met(), {
      req(data_ult_met())
      df <- data_ult_met()
      if (!is.data.frame(df) || nrow(df) == 0) return(invisible())
      
      # último registro por fecha/hora
      ult <- df[order(df$FechaHoraCrea, decreasing = TRUE), ][1, ]
      
      # helper para setear solo si existe valor
      set_auto_if <- function(id, value) {
        if (!is.na(value)) updateAutonumericInput(session, id, value = value)
      }
      
      # actualizar valores si existen
      set_auto_if("IND_SacosMes",      ult$MetaSacosMes)
      set_auto_if("IND_MargenMes",     ult$MetaMargenMes)
      set_auto_if("IND_SacosSemana",   ult$MetaSacosSemana)
      set_auto_if("IND_MargenSemana",  ult$MetaMargenSemana)
      
      if (any(!is.na(df$MetaSacosMes)))  shinyjs::disable(id = "IND_SacosMes")  else shinyjs::enable(id = "IND_SacosMes")
      if (any(!is.na(df$MetaMargenMes))) shinyjs::disable(id = "IND_MargenMes") else shinyjs::enable(id = "IND_MargenMes")
    })
    
    
    
    # 6) Lotes Clientes -----
    det_lot_f <- reactive({
      
      data_filtered <- dat()
      if (input$IND_PendProducir && input$IND_PendDespachar && input$IND_DespPendFacturar) {
        data_filtered <- data_filtered %>%
          filter(PendProducir > 0.1 | PendDespachar > 0.1 | PendFacturar > 0.1)
      } else {
        conditions <- list()
        if (input$IND_PendProducir) conditions <- append(conditions, quote(PendProducir > 0.1))
        if (input$IND_PendDespachar) conditions <- append(conditions, quote(PendDespachar > 0.1))
        if (input$IND_DespPendFacturar) conditions <- append(conditions, quote(PendFacturar > 0.1))
        
        if (length(conditions) > 0) {
          filter_expr <- Reduce(function(x, y) bquote(.(x) | .(y)), conditions)
          data_filtered <- data_filtered %>% filter(!!filter_expr)
        }
      }
      
      data_filtered %>%
        left_join(NCLIENTE %>% select(PerCod, ClientePedido = PerRazSoc) %>% distinct(), by = c("CLCliNit"="PerCod")) %>% 
        select(Sucursal, Pedido = PdcRefCli, CLLotCod, Sacos = SacLote, 
               FecAsignLote, CLLinNegNo, Segmento, Categoria, 
               Producto, CliNitPpal, PerRazSoc, NitPedido = CLCliNit,  ClientePedido,
               SegmentoRacafe, 
               PendProducir, PendDespachar, PendFacturar) %>% 
        arrange(desc(FecAsignLote)) %>% 
        mutate(CLLotCod = as.character(CLLotCod)) %>% 
        janitor::adorn_totals("row", name = "TOTAL") 
      
    })
    output$IND_Lotes <- render_gt({
      data_lotes <- det_lot_f()
      
      if (nrow(data_lotes) <= 1) {
        data.frame(Mensaje = "No hay lotes pendientes con los filtros seleccionados") %>%
          gt() %>%
          cols_label(Mensaje = "") %>%
          tab_options(
            table.width = pct(100),
            table.font.size = px(14),
            column_labels.hidden = TRUE
          ) %>%
          tab_style(
            style = cell_text(align = "center", style = "italic", color = "#6c757d"),
            locations = cells_body()
          )
      } else {
        data_lotes %>% 
          gt() %>%
          cols_label(
            Sucursal = "Sucursal", Pedido = "Pedido", CLLotCod = "Lote", Sacos = "Sacos Lote",
            FecAsignLote = "Fecha Asignación", CLLinNegNo = "Línea Negocio",
            Categoria = "Categoría", Producto = "Producto", CliNitPpal = "NIT Principal", PerRazSoc = "Cliente",
            NitPedido = "NIT Pedido", ClientePedido = "Cliente Pedido",
            Segmento = "Segmento", SegmentoRacafe = "Tipo Cliente", 
            PendProducir = "Sacos Pend. Producir", PendDespachar = "Sacos Pend. Despachar",
            PendFacturar = "Sacos Pend. Facturar"
          ) %>%
          fmt_number(
            columns = c("Sacos", "PendProducir", "PendDespachar", "PendFacturar"), 
            decimals = 0, sep_mark = ",", dec_mark = "."
          ) %>% 
          opt_interactive(
            use_pagination = FALSE, 
            use_filters = TRUE
          ) %>% 
          gt_minimal_style()
      }
    })
    
    # 7) Presupuesto ----
    Presupuesto("PresupuestoCliente", dat)
    
    
    # 8) Productos y Categorias -----
    output$IND_DistrProducto <- renderPlotly({
      
      tit <- case_when(input$IND_Producto == "Categoria" ~ "OTRAS CATEGORIAS",
                       input$IND_Producto == "Producto" ~ "OTROS PRODUCTOS")
      
      tmp1 <- dat() %>%
        group_by(!!sym(input$IND_Producto)) %>%
        filter(!is.na(!!sym(input$IND_Producto))) %>%
        summarise(Sacos = sum(SacFact70),
                  Margen = sum(Margen)) %>% 
        TopRelativo(var_recode = input$IND_Producto, var_top = input$IND_VariableProducto,
                    fun_Top = "sum", pct_min = 0.05, nom_var = "Agrupado",
                    lab_recodificar = tit)
      
      ImprimirAnillo(tmp1, var_label = "Agrupado", var_medida = input$IND_VariableProducto, funcion = "sum")
      
      
    })
    # 9) Serie Historica -----
    
    modelo_f <- eventReactive({
      dat()
      input$IND_TipoSerie
      input$IND_VariableSerie
    }, {
      
      waiter_show(html = preloader2$html, color = preloader2$color)
      on.exit(waiter_hide())
      
      ts_alm <- dat() %>% 
        filter(!is.na(FecFact)) %>% 
        group_by(fecha = PrimerDia(FecFact)) %>% 
        summarise(Sacos = sum(SacFact70),
                  Margen = sum(Margen))
      
      meses <- 6
      modelo <- Pronosticar(ts_alm, fecha_col = "fecha", 
                            metodos = c("prophet"),
                            periodos_pronostico = meses, 
                            metodo_imputacion = "cero",
                            tipo_frecuencia = "mensual",
                            incluir_historicos = TRUE) %>% 
        PronSeleccionar()
      
      p <- if (input$IND_TipoSerie == "Histórico") {
        PronSerie(seleccion = modelo, columna = input$IND_VariableSerie)
      } else{
        PronMensual(seleccion = modelo, columna = input$IND_VariableSerie)
      }
      return(p)
      
    }, ignoreNULL = TRUE)
    
    output$IND_Serie <- renderPlotly({
      modelo_f()
    })
  })
}

### App de prueba ----
ui <- bs4DashPage(
  title = "Prueba ResumenTotal",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    IndividualUI("resumen")
  )
)

server <- function(input, output, session) {
  Individual("resumen", reactive({
    BaseDatos %>% 
      filter(CliNitPpal == 860056500,
             LinNegCod == 10000)}),
    reactive("HCYATE")
  )
}

shinyApp(ui, server)
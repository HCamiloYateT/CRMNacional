# Módulo Formulario Lote ----
AsignarOrdenCompraUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             bs4Card(title = "Orden de Compra", width = 12, solidHeader = TRUE, status = "white",
                     uiOutput(ns("info_lote")),
                     textInput(ns("campo_valor"), label = "Orden de Compra", value = "", placeholder = "Ingrese valor")
                     )
             )
      ),
    fluidRow(
      column(10),
      column(2,
             actionBttn(inputId = ns("btn_guardar"), label   = "Asignar", style   = "unite", color   = "danger",
                        size    = "xs", icon    = icon("save"), block   = TRUE)
             )
      ),
    br(),
    # BITÁCORA ----
    fluidRow(
      column(12,
             bs4Card(title = "Bitácora del lote", width = 12, solidHeader = TRUE, status = "white",
                     gt_output(ns("tabla_bitacora"))
                     )
             )
      )
    )
}
AsignarOrdenCompra <- function(id, dd_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    lote_codigo <- reactive({
      req(dd_data())
      dd_data()$fila_completa$CLLotCod[1]
    })
    output$info_lote <- renderUI({
      req(lote_codigo())
      tagList(
        strong("Lote: "),
        span(lote_codigo())
      )
    })
    
    # Trigger para refrescar bitácora ----
    refresh_bitacora <- reactiveVal(Sys.time())
    
    observeEvent(input$btn_guardar, {
      req(lote_codigo(), input$campo_valor)
      
      nuevo_registro <- tibble::tibble(
        UsuarioCrea   = "HCYATE",
        FechaHoraCrea = Sys.time(),
        Lote          = lote_codigo(),
        OrdenCompra   = trimws(input$campo_valor)
      )
      
      AgregarDatos(nuevo_registro, "CRMNALORDCMP")
      
      refresh_bitacora(Sys.time())
      
      showNotification(
        "Orden de compra asignada correctamente",
        type = "message"
      )
    })
    
    # Bitácora por lote ----
    output$tabla_bitacora <- render_gt({
      req(lote_codigo(), refresh_bitacora())
      
      CargarDatos("CRMNALORDCMP") %>%
        filter(Lote == lote_codigo()) %>%
        arrange(desc(FechaHoraCrea)) %>%
        gt() %>%
        gt_minimal_style()
    })
  })
}

PendientesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # FILA 1: Cajas superiores ----
    fluidRow(
      column(4, bs4ValueBoxOutput(ns("PEN_CajaPendPedidos"), width = 12)),
      column(4, bs4ValueBoxOutput(ns("PEN_CajaVencidosFact"), width = 12)),
      column(4, bs4ValueBoxOutput(ns("PEN_CajaVencidosProd"), width = 12))
    ),
    
    # FILA 2: Cajas inferiores ----
    fluidRow(
      column(4, bs4ValueBoxOutput(ns("PEN_LotProducir"), width = 12)),
      column(4, bs4ValueBoxOutput(ns("PEN_LotDespachar"), width = 12)),
      column(4, bs4ValueBoxOutput(ns("PEN_LotFacturar"), width = 12))
    ),
    
    br(),
    
    # FILA 3: Filtros + descarga + tabla ----
    fluidRow(
      column(
        12,
        fluidRow(
          column(4, createSwitch(
            "PEN_PendProducir",
            "Pend. por Producir",
            ns = ns
          )),
          column(4, createSwitch(
            "PEN_PendDespachar",
            "Pend. por despachar",
            ns = ns
          )),
          column(4, createSwitch(
            "PEN_DespPendFacturar",
            "Despachados pend. por facturar",
            TRUE,
            ns = ns
          ))
        ),
        br(),
        div(
          style = "text-align: left;",
          BotonDescarga("btn_descargar", size = "md", ns = ns)
        ),
        br(),
        GTBotonesUI(ns("Tabla"))
      )
    )
  )
}
Pendientes <- function(id, dat, dat_ped, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

        output$PEN_CajaPendPedidos <- renderbs4ValueBox({
      val_pedidos <- nrow(dat_ped())
      val_sacos <- sum(dat_ped()$PdcCan, na.rm = TRUE)
      texto_completo <- paste("Pedidos sin lote asignado", "<br>Sacos:", comma(val_sacos, accuracy = 0.1))
      CajaValor(val_pedidos, "coma", texto_completo, "clock", ns("btn_PendPedidos"))
    })
    output$PEN_CajaVencidosFact <- renderbs4ValueBox({
      data_vencidos <- dat() %>%
        filter(PendFacturar > 0, PdcFecCre <= Sys.Date() - months(3))
      val_lotes <- nrow(data_vencidos)
      val_sacos <- sum(data_vencidos$SacLote, na.rm = TRUE)
      texto_completo <- paste("Lotes antiguos sin facturar", "<br>Sacos:", comma(val_sacos, accuracy = 0.1))
      CajaValor(val_lotes, "coma", texto_completo, "hourglass-end", ns("btn_VencidosFact"))
    })
    output$PEN_CajaVencidosProd <- renderbs4ValueBox({
      data_vencidos <- dat() %>%
        filter(PendProducir > 0, PdcFecCre <= Sys.Date() - months(3))
      val_lotes <- nrow(data_vencidos)
      val_sacos <- sum(data_vencidos$SacLote, na.rm = TRUE)
      texto_completo <- paste("Lotes antiguos sin producir", "<br>Sacos:", comma(val_sacos, accuracy = 0.1))
      CajaValor(val_lotes, "coma", texto_completo, "hourglass-end", ns("btn_VencidosProd"))
    })
    output$PEN_LotProducir <- renderbs4ValueBox({
      data_producir <- dat() %>% filter(PendProducir > 0)
      val_lotes <- nrow(data_producir)
      val_sacos <- sum(data_producir$SacLote, na.rm = TRUE)
      texto_completo <- paste("Lotes pend. por producir", "<br>Sacos:", comma(val_sacos, accuracy = 0.1))
      CajaValor(val_lotes, "coma", texto_completo, "industry", ns("btn_LotProducir"), mostrar_boton = FALSE)
    })
    output$PEN_LotDespachar <- renderbs4ValueBox({
      data_despachar <- dat() %>% filter(PendDespachar > 0)
      val_lotes <- nrow(data_despachar)
      val_sacos <- sum(data_despachar$SacLote, na.rm = TRUE)
      texto_completo <- paste("Lotes pend. por despachar", "<br>Sacos:", comma(val_sacos, accuracy = 0.1))
      CajaValor(val_lotes, "coma", texto_completo, "truck", ns("btn_LotDespachar"), mostrar_boton = FALSE)
    })
    output$PEN_LotFacturar <- renderbs4ValueBox({
      data_facturar <- dat() %>% filter(PendFacturar > 0)
      val_lotes <- nrow(data_facturar)
      val_sacos <- sum(data_facturar$SacLote, na.rm = TRUE)
      texto_completo <- paste("Lotes desp. pend. por facturar", "<br>Sacos:", comma(val_sacos, accuracy = 0.1))
      CajaValor(val_lotes, "coma", texto_completo, "money-bill", ns("btn_LotFacturar"), mostrar_boton = FALSE)
    })
    
    observeEvent(input$btn_PendPedidos, {
      showModal(
        modalDialog(
          title = "Detalle - Pedidos sin lote asignado",
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Cerrar"),
          gt_output(ns("TablaPendPedidos"))
        )
      )
    })
    output$TablaPendPedidos <- render_gt({
      req(dat_ped())
      data_pedidos <- dat_ped() %>%
        select(-PdcUsu) %>% 
        arrange(desc(PdcFecCre))
      
      if (nrow(data_pedidos) == 0) {
        data.frame(Mensaje = "No hay pedidos pendientes") %>%
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
        data_pedidos %>%
          gt() %>%
          cols_label(PerRazSoc = "Razón Social", Segmento = "Segmento",
                     PdcCod = "Cod Pedido", PdcLin = "Línea de Pedido", 
                     PdcCan = "Sacos Pedido", PdcFecCre = "Fecha Creación", 
                     LinNeg = "Línea de Negocio", LinProNom = "Línea de Producto") %>%
          fmt_number(columns = "PdcCan", decimals = 0, sep_mark = ",", dec_mark = ".") %>% 
          gt_minimal_style()
      }
    })
    observeEvent(input$btn_VencidosFact, {
      showModal(
        modalDialog(
          title = tagList(icon("hourglass-end"), "Detalle - Lotes antiguos sin facturar"),
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Cerrar"),
          gt_output(ns("TablaVencidosFact"))
        )
      )
    })
    output$TablaVencidosFact <- render_gt({
      data_vencidos <- dat() %>%
        filter(PendFacturar > 0, PdcFecCre <= Sys.Date() - months(3)) %>%
        select(Sucursal, CLLotCod, CLPdcCod, PdcFecCre, SacLote, Categoria, Producto, PerRazSoc, PendFacturar) %>%
        arrange(PdcFecCre)
      
      if (nrow(data_vencidos) == 0) {
        data.frame(Mensaje = "No hay lotes vencidos sin facturar") %>%
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
        data_vencidos %>%
          gt() %>%
          cols_label(Sucursal = "Sucursal", 
                     CLLotCod = "Lote", 
                     CLPdcCod = "Cod. Pedido",
                     PdcFecCre = "Fecha Pedido",
                     SacLote = "Sacos",
                     Categoria = "Categoría",
                     Producto = "Producto", 
                     PerRazSoc = "Cliente", 
                     PendFacturar = "Pend. Facturar") %>%
          fmt_number(columns = c("SacLote", "PendFacturar"), decimals = 0, sep_mark = ",", dec_mark = ".") %>%
          gt_minimal_style()
      }
    })
    observeEvent(input$btn_VencidosProd, {
      showModal(
        modalDialog(
          title = tagList(icon("hourglass-end"), "Detalle - Lotes antiguos sin producir"),
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Cerrar"),
          gt_output(ns("TablaVencidosPend"))
        )
      )
    })
    output$TablaVencidosPend <- render_gt({
      data_vencidos <- dat() %>%
        filter(PendProducir > 0, PdcFecCre <= Sys.Date() - months(3)) %>%
        select(Sucursal, CLLotCod, CLPdcCod, PdcFecCre, SacLote, Categoria, Producto, PerRazSoc, PendFacturar) %>%
        arrange(PdcFecCre)
      
      if (nrow(data_vencidos) == 0) {
        data.frame(Mensaje = "No hay lotes vencidos sin facturar") %>%
          gt() %>%
          cols_label(Mensaje = "") %>%
          gt_minimal_style()
          
      } else {
        data_vencidos %>%
          gt() %>%
          cols_label(Sucursal = "Sucursal", 
                     CLLotCod = "Lote", 
                     CLPdcCod = "Cod. Pedido",
                     PdcFecCre = "Fecha Pedido",
                     SacLote = "Sacos",
                     Categoria = "Categoría",
                     Producto = "Producto", 
                     PerRazSoc = "Cliente", 
                     PendFacturar = "Pend. Facturar") %>%
          fmt_number(columns = c("SacLote", "PendFacturar"), decimals = 0, sep_mark = ",", dec_mark = ".") %>%
          gt_minimal_style()
      }
    })
    
    data_dt <- reactive({
      data_filtered <- dat()
      
      if (input$PEN_PendProducir && input$PEN_PendDespachar && input$PEN_DespPendFacturar) {
        data_filtered <- data_filtered %>%
          filter(PendProducir > 0.1 | PendDespachar > 0.1 | PendFacturar > 0.1)
      } else {
        conditions <- list()
        if (input$PEN_PendProducir) conditions <- append(conditions, quote(PendProducir > 0.1))
        if (input$PEN_PendDespachar) conditions <- append(conditions, quote(PendDespachar > 0.1))
        if (input$PEN_DespPendFacturar) conditions <- append(conditions, quote(PendFacturar > 0.1))
        
        if (length(conditions) > 0) {
          filter_expr <- Reduce(function(x, y) bquote(.(x) | .(y)), conditions)
          data_filtered <- data_filtered %>% filter(!!filter_expr)
        }
      }
      
      data_filtered %>%
        left_join(
          NCLIENTE %>%
            select(PerCod, ClientePedido = PerRazSoc) %>%
            distinct(),
          by = c("CLCliNit" = "PerCod")
        ) %>%
        select(
          Sucursal,
          CLPdcCod,
          PdcRefCli,
          CLLotCod,
          SacLote,
          PdcPrecioKilo,
          FecAsignLote,
          OrdenCompra,
          CLLinNegNo,
          Segmento,
          Categoria,
          Producto,
          CliNitPpal,
          PerRazSoc,
          CLCliNit,
          ClientePedido,
          SegmentoRacafe,
          PendProducir,
          PendDespachar,
          PendFacturar
        ) %>%
        arrange(desc(FecAsignLote)) %>%
        mutate(CLLotCod = as.character(CLLotCod)) %>%
        janitor::adorn_totals("row", name = "TOTAL")
    })
    tabla_lotes_gt <- reactive({
      data_lotes <- data_dt()
      
      if (nrow(data_lotes) <= 1) {
        return(
          data.frame(Mensaje = "No hay lotes pendientes con los filtros seleccionados") %>%
            gt() %>%
            cols_label(Mensaje = "")
        )
      }
      
      data_lotes %>%
        gt() %>%
        cols_label(
          Sucursal        = "Sucursal",
          CLPdcCod        = "Código Pedido",
          PdcRefCli       = "Pedido",
          CLLotCod        = "Lote",
          SacLote         = "Sacos Lote",
          PdcPrecioKilo   = "Precio Kilo",
          FecAsignLote    = "Fecha Asignación",
          OrdenCompra    = "Orden de Compra",
          CLLinNegNo      = "Línea Negocio",
          Categoria       = "Categoría",
          Producto        = "Producto",
          CliNitPpal      = "NIT Principal",
          PerRazSoc       = "Cliente",
          CLCliNit        = "NIT Pedido",
          ClientePedido   = "Cliente Pedido",
          Segmento        = "Segmento",
          SegmentoRacafe  = "Tipo Cliente",
          PendProducir    = "Sacos Pend. Producir",
          PendDespachar   = "Sacos Pend. Despachar",
          PendFacturar   = "Sacos Pend. Facturar"
        ) %>%
        fmt_number(
          columns = c("SacLote", "PendProducir", "PendDespachar", "PendFacturar"),
          decimals = 0,
          sep_mark = ",",
          dec_mark = "."
        ) %>%
        fmt_currency(columns = "PdcPrecioKilo", decimals = 0) %>%
        gt_minimal_style() %>%
        gt::opt_interactive(
          use_pagination = FALSE,
          use_filters = FALSE,
          use_resizers = TRUE
        )
    })
    
    ## Configuración de botones ----
    botones_config <- crear_config_botones(
      asignar = list(
        titulo        = "Asignar Orden de Compra",
        tit_modal     = "Asignar Orden de Compra al lote -",
        module_ui     = "AsignarOrdenCompraUI",
        module_server = "AsignarOrdenCompra",
        module_id     = "mod_formulario",
        icono         = "arrow-right"
        )
      ) 
    
    ## Reactivos de drilldown para los sub-módulos ----
    dd <- setNames(
      lapply(names(botones_config), function(x) reactiveVal(NULL)),
      paste0(names(botones_config), "_dd")
    )
    ## Llamada de la parte UI de los sub-módulos ----
    modulos_ui <- setNames(
      lapply(botones_config, function(cfg) {
        ui_fun <- get(cfg$module_ui)
        function(ns_parent) ui_fun(ns_parent(cfg$module_id))
      }),
      paste0(names(botones_config), "_module")
    )
    
    ## Llamado módulo de tabla gt ----
    resultado <- GTBotones(id             = "Tabla",
                           gt_table       = tabla_lotes_gt,
                           data           = data_dt,
                           botones_config = botones_config,
                           nombre_col     = c("CLLotCod"))
    
    
    output$btn_descargar <- downloadHandler(
      filename = function() {
        paste0("LotesPendientes_", Sys.time(), ".xlsx")
      },
      content = function(file) {
        data_export <- data_dt()
        openxlsx::write.xlsx(data_export, file, rowNames = FALSE)
      }
    )
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
    PendientesUI("resumen")
  )
)

server <- function(input, output, session) {
  Pendientes(id = "resumen", dat = reactive({BaseDatos}), dat_ped = reactive({BaseDatos}), usr = reactive("CMEDINA"))
}

shinyApp(ui, server)

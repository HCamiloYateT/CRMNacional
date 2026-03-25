# Módulo: Formulario Orden de Compra ----

AsignarOrdenCompraUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Formulario de ingreso ----
    fluidRow(
      column(12,
             bs4Card(title = "Orden de Compra", width = 12, solidHeader = TRUE, status = "white",
                     uiOutput(ns("info_lote")),
                     textInput(ns("campo_valor"), label = "Orden de Compra", value = "",
                               placeholder = "Ingrese valor")
             )
      )
    ),
    fluidRow(
      column(10),
      column(2,
             actionBttn(inputId = ns("btn_guardar"), label = "Asignar", style = "unite",
                        color = "danger", size = "xs", icon = icon("save"), block = TRUE)
      )
    ),
    br(),
    # Bitacora del lote ----
    fluidRow(
      column(12,
             bs4Card(title = "Bitácora del lote", width = 12, solidHeader = TRUE, status = "white",
                     reactableOutput(ns("tabla_bitacora"))
             )
      )
    )
  )
}

AsignarOrdenCompra <- function(id, dd_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Codigo del lote seleccionado ----
    lote_codigo <- reactive({
      req(dd_data())
      dd_data()$fila_completa$CLLotCod[1]
    })
    
    output$info_lote <- renderUI({
      req(lote_codigo())
      tagList(strong("Lote: "), span(lote_codigo()))
    })
    
    # Trigger refresco bitacora ----
    refresh_bitacora <- reactiveVal(Sys.time())
    
    # Guardado de orden de compra ----
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
      showNotification("Orden de compra asignada correctamente", type = "message")
    })
    
    # Tabla de bitacora ----
    output$tabla_bitacora <- renderReactable({
      req(lote_codigo(), refresh_bitacora())
      datos <- CargarDatos("CRMNALORDCMP") %>%
        filter(Lote == lote_codigo()) %>%
        arrange(desc(FechaHoraCrea))
      reactable::reactable(datos, sortable = TRUE, pagination = FALSE,
                           compact = TRUE, striped = TRUE, borderless = TRUE)
    })
  })
}


# Sub-modulo: Detalle pedidos sin lote asignado ----

DetallePendPedidosUI <- function(id) {
  ns <- NS(id)
  TablaReactableUI(ns("tabla"), titulo = "Pedidos sin lote asignado")
}

DetallePendPedidos <- function(id, dat_ped) {
  moduleServer(id, function(input, output, session) {
    data_r <- reactive({
      req(dat_ped())
      dat_ped() %>%
        select(-PdcUsu) %>%
        arrange(desc(PdcFecCre))
    })
    TablaReactable(
      id             = "tabla",
      data           = data_r,
      modo_seleccion = "ninguno",
      sortable       = TRUE,
      searchable     = TRUE,
      page_size      = 99999L,
      compact        = TRUE,
      mostrar_badge  = FALSE,
      mostrar_nota   = FALSE,
      columnas = list(
        PerRazSoc = reactable::colDef(name = "Razón Social"),
        Segmento  = reactable::colDef(name = "Segmento"),
        PdcCod    = reactable::colDef(name = "Cód. Pedido"),
        PdcLin    = reactable::colDef(name = "Línea Pedido"),
        PdcCan    = reactable::colDef(name = "Sacos",
                                      cell = function(v) if (is.na(v)) "\u2014" else format(round(v, 0), big.mark = ",")),
        PdcFecCre = reactable::colDef(name = "Fecha Creación"),
        LinNeg    = reactable::colDef(name = "Línea Negocio"),
        LinProNom = reactable::colDef(name = "Línea de Producto")
      )
    )
  })
}


# Sub-modulo: Detalle lotes antiguos sin facturar ----

DetalleVencidosFactUI <- function(id) {
  ns <- NS(id)
  TablaReactableUI(ns("tabla"), titulo = "Lotes antiguos sin facturar")
}

DetalleVencidosFact <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    data_r <- reactive({
      req(dat())
      dat() %>%
        filter(PendFacturar > 0, PdcFecCre <= Sys.Date() - months(3)) %>%
        select(Sucursal, CLLotCod, CLPdcCod, PdcFecCre, SacLote,
               Categoria, Producto, PerRazSoc, PendFacturar) %>%
        arrange(PdcFecCre)
    })
    TablaReactable(
      id             = "tabla",
      data           = data_r,
      modo_seleccion = "ninguno",
      sortable       = TRUE,
      searchable     = TRUE,
      page_size      = 99999L,
      compact        = TRUE,
      mostrar_badge  = FALSE,
      mostrar_nota   = FALSE,
      columnas = list(
        Sucursal     = reactable::colDef(name = "Sucursal"),
        CLLotCod     = reactable::colDef(name = "Lote"),
        CLPdcCod     = reactable::colDef(name = "Cód. Pedido"),
        PdcFecCre    = reactable::colDef(name = "Fecha Pedido"),
        SacLote      = reactable::colDef(name = "Sacos",
                                         cell = function(v) if (is.na(v)) "\u2014" else format(round(v, 0), big.mark = ",")),
        Categoria    = reactable::colDef(name = "Categoria"),
        Producto     = reactable::colDef(name = "Producto"),
        PerRazSoc    = reactable::colDef(name = "Cliente"),
        PendFacturar = reactable::colDef(name = "Pend. Facturar",
                                         cell = function(v) if (is.na(v)) "\u2014" else format(round(v, 0), big.mark = ","))
      )
    )
  })
}


# Sub-modulo: Detalle lotes antiguos sin producir ----

DetalleVencidosProdUI <- function(id) {
  ns <- NS(id)
  TablaReactableUI(ns("tabla"), titulo = "Lotes antiguos sin producir")
}

DetalleVencidosProd <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    data_r <- reactive({
      req(dat())
      dat() %>%
        filter(PendProducir > 0, PdcFecCre <= Sys.Date() - months(3)) %>%
        select(Sucursal, CLLotCod, CLPdcCod, PdcFecCre, SacLote,
               Categoria, Producto, PerRazSoc, PendProducir) %>%
        arrange(PdcFecCre)
    })
    TablaReactable(
      id             = "tabla",
      data           = data_r,
      modo_seleccion = "ninguno",
      sortable       = TRUE,
      searchable     = TRUE,
      page_size      = 99999L,
      compact        = TRUE,
      mostrar_badge  = FALSE,
      mostrar_nota   = FALSE,
      columnas = list(
        Sucursal     = reactable::colDef(name = "Sucursal"),
        CLLotCod     = reactable::colDef(name = "Lote"),
        CLPdcCod     = reactable::colDef(name = "Cód. Pedido"),
        PdcFecCre    = reactable::colDef(name = "Fecha Pedido"),
        SacLote      = reactable::colDef(name = "Sacos",
                                         cell = function(v) if (is.na(v)) "\u2014" else format(round(v, 0), big.mark = ",")),
        Categoria    = reactable::colDef(name = "Categoria"),
        Producto     = reactable::colDef(name = "Producto"),
        PerRazSoc    = reactable::colDef(name = "Cliente"),
        PendProducir = reactable::colDef(name = "Pend. Producir",
                                         cell = function(v) if (is.na(v)) "\u2014" else format(round(v, 0), big.mark = ","))
      )
    )
  })
}


# Módulo principal: Pendientes ----

PendientesUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Fila 1: KPI con modal de detalle ----
    fluidRow(
      column(4, CajaModalUI(ns("kpi_pend_pedidos"))),
      column(4, CajaModalUI(ns("kpi_venc_fact"))),
      column(4, CajaModalUI(ns("kpi_venc_prod")))
    ),
    # Fila 2: KPI informativos sin modal ----
    fluidRow(
      column(4, CajaModalUI(ns("kpi_lot_producir"))),
      column(4, CajaModalUI(ns("kpi_lot_despachar"))),
      column(4, CajaModalUI(ns("kpi_lot_facturar")))
    ),
    br(),
    # Fila 3: Filtros, tabla y descarga ----
    fluidRow(
      column(12,
             fluidRow(
               column(4, createSwitch("PEN_PendProducir",      "Pend. por Producir",              ns = ns)),
               column(4, createSwitch("PEN_PendDespachar",     "Pend. por despachar",             ns = ns)),
               column(4, createSwitch("PEN_DespPendFacturar",  "Despachados pend. por facturar",  TRUE, ns = ns))
             ),
             br(),
             uiOutput(ns("bloque_tabla")),
             br(),
             div(
               style = "text-align: left;",
               BotonDescarga("btn_descargar", size = "md", ns = ns)
             )
      )
    )
  )
}

Pendientes <- function(id, dat, dat_ped, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactivos de datos por categoria KPI ----
    # Evita recalcular dat() N veces; cada reactive comparte la misma fuente filtrada
    data_venc_fact_r  <- reactive({
      dat() %>% filter(PendFacturar > 0, PdcFecCre <= Sys.Date() - months(3))
    })
    data_venc_prod_r  <- reactive({
      dat() %>% filter(PendProducir > 0, PdcFecCre <= Sys.Date() - months(3))
    })
    data_lot_prod_r   <- reactive({ dat() %>% filter(PendProducir  > 0) })
    data_lot_desp_r   <- reactive({ dat() %>% filter(PendDespachar > 0) })
    data_lot_fact_r   <- reactive({ dat() %>% filter(PendFacturar  > 0) })
    
    # Registro eager de sub-modulos antes de CajaModal y TablaReactable ----
    # Patron eager: moduleServer registrado en startup; modal_pre_fn actualiza el reactiveVal
    dd_asignar_rv <- reactiveVal(NULL)
    
    DetallePendPedidos( "det_pend_ped",  dat_ped = dat_ped)
    DetalleVencidosFact("det_venc_fact", dat     = dat)
    DetalleVencidosProd("det_venc_prod", dat     = dat)
    AsignarOrdenCompra( "mod_formulario", dd_data = reactive({ dd_asignar_rv() }))
    
    # Helper local: formato numerico plano (sin HTML) para footers y celdas ----
    .fmt <- function(v) format(round(v, 0), big.mark = ",", scientific = FALSE)
    .fmt_precio <- function(v) paste0("$ ", format(round(v, 0), big.mark = ",", scientific = FALSE))
    
    # Cajas KPI con detalle en modal ----
    CajaModal("kpi_pend_pedidos",
              valor           = reactive(html_valor(nrow(dat_ped()), formato = "entero")),
              formato         = "entero",
              texto           = "Pedidos sin lote asignado",
              icono           = "clock",
              colores         = c(fondo = "white"),
              mostrar_boton   = TRUE,
              titulo_modal    = "Detalle \u2014 Pedidos sin lote asignado",
              icono_modal     = "clock",
              contenido_modal = function() DetallePendPedidosUI(ns("det_pend_ped")),
              footer          = reactive(
                paste0("Sacos: ", .fmt(sum(dat_ped()$PdcCan, na.rm = TRUE)))
              ),
              footer_class    = "caja-modal-footer"
    )
    
    CajaModal("kpi_venc_fact",
              valor           = reactive(html_valor(nrow(data_venc_fact_r()), formato = "entero")),
              formato         = "entero",
              texto           = "Lotes antiguos sin facturar",
              icono           = "hourglass-end",
              colores         = c(fondo = "white"),
              mostrar_boton   = TRUE,
              titulo_modal    = "Detalle \u2014 Lotes antiguos sin facturar",
              icono_modal     = "hourglass-end",
              contenido_modal = function() DetalleVencidosFactUI(ns("det_venc_fact")),
              footer          = reactive(
                paste0("Sacos: ", .fmt(sum(data_venc_fact_r()$SacLote, na.rm = TRUE)))
              ),
              footer_class    = "caja-modal-footer"
    )
    
    CajaModal("kpi_venc_prod",
              valor           = reactive(html_valor(nrow(data_venc_prod_r()), formato = "entero")),
              formato         = "entero",
              texto           = "Lotes antiguos sin producir",
              icono           = "hourglass-end",
              colores         = c(fondo = "white"),
              mostrar_boton   = TRUE,
              titulo_modal    = "Detalle \u2014 Lotes antiguos sin producir",
              icono_modal     = "hourglass-end",
              contenido_modal = function() DetalleVencidosProdUI(ns("det_venc_prod")),
              footer          = reactive(
                paste0("Sacos: ", .fmt(sum(data_venc_prod_r()$SacLote, na.rm = TRUE)))
              ),
              footer_class    = "caja-modal-footer"
    )
    
    # Cajas KPI informativos sin modal ----
    CajaModal("kpi_lot_producir",
              valor         = reactive(html_valor(nrow(data_lot_prod_r()), formato = "entero")),
              formato       = "entero",
              texto         = "Lotes pend. por producir",
              icono         = "industry",
              colores       = c(fondo = "white"),
              mostrar_boton = FALSE,
              footer        = reactive(
                paste0("Sacos: ", .fmt(sum(data_lot_prod_r()$SacLote, na.rm = TRUE)))
              ),
              footer_class  = "caja-modal-footer"
    )
    
    CajaModal("kpi_lot_despachar",
              valor         = reactive(html_valor(nrow(data_lot_desp_r()), formato = "entero")),
              formato       = "entero",
              texto         = "Lotes pend. por despachar",
              icono         = "truck",
              colores       = c(fondo = "white"),
              mostrar_boton = FALSE,
              footer        = reactive(
                paste0("Sacos: ", .fmt(sum(data_lot_desp_r()$SacLote, na.rm = TRUE)))
              ),
              footer_class  = "caja-modal-footer"
    )
    
    CajaModal("kpi_lot_facturar",
              valor         = reactive(html_valor(nrow(data_lot_fact_r()), formato = "entero")),
              formato       = "entero",
              texto         = "Lotes desp. pend. por facturar",
              icono         = "money-bill",
              colores       = c(fondo = "white"),
              mostrar_boton = FALSE,
              footer        = reactive(
                paste0("Sacos: ", .fmt(sum(data_lot_fact_r()$SacLote, na.rm = TRUE)))
              ),
              footer_class  = "caja-modal-footer"
    )
    
    # Datos base de la tabla (sin fila total) ----
    # Separado para poder guardar estado vacio antes de agregar el total.
    data_base_r <- reactive({
      pend_prod <- isTRUE(input$PEN_PendProducir)
      pend_desp <- isTRUE(input$PEN_PendDespachar)
      pend_fact <- isTRUE(input$PEN_DespPendFacturar)
      
      data_f <- dat()
      if (any(c(pend_prod, pend_desp, pend_fact))) {
        data_f <- data_f %>%
          filter(
            (pend_prod & PendProducir  > 0.1) |
              (pend_desp & PendDespachar > 0.1) |
              (pend_fact & PendFacturar  > 0.1)
          )
      }
      
      data_f %>%
        left_join(
          NCLIENTE %>% select(PerCod, ClientePedido = PerRazSoc) %>% distinct(),
          by = c("CLCliNit" = "PerCod")
        ) %>%
        select(Sucursal, CLPdcCod, PdcRefCli, CLLotCod, SacLote, PdcPrecioKilo,
               FecAsignLote, OrdenCompra, CLLinNegNo, Segmento, Categoria, Producto,
               CliNitPpal, PerRazSoc, CLCliNit, ClientePedido, SegmentoRacafe,
               PendProducir, PendDespachar, PendFacturar) %>%
        arrange(desc(FecAsignLote)) %>%
        mutate(
          Asignar    = "btn",
          CLLotCod   = as.character(CLLotCod),
          CLPdcCod   = as.character(CLPdcCod),
          CliNitPpal = as.character(CliNitPpal),
          CLCliNit   = as.character(CLCliNit)
        ) %>%
        select(Asignar, everything())
    })
    
    # Datos con fila de totales (solo cuando hay datos) ----
    data_tabla <- reactive({
      base <- data_base_r()
      if (nrow(base) == 0) return(base)
      fila_total <- tibble::tibble(
        Asignar        = "",
        Sucursal       = "TOTAL",
        CLPdcCod       = "",
        PdcRefCli      = "",
        CLLotCod       = "TOTAL",
        SacLote        = sum(base$SacLote,       na.rm = TRUE),
        PdcPrecioKilo  = NA_real_,
        FecAsignLote   = as.Date(NA_character_),
        OrdenCompra    = "",
        CLLinNegNo     = "",
        Segmento       = "",
        Categoria      = "",
        Producto       = "",
        CliNitPpal     = "",
        PerRazSoc      = "TOTAL",
        CLCliNit       = "",
        ClientePedido  = "",
        SegmentoRacafe = "",
        PendProducir   = sum(base$PendProducir,  na.rm = TRUE),
        PendDespachar  = sum(base$PendDespachar, na.rm = TRUE),
        PendFacturar   = sum(base$PendFacturar,  na.rm = TRUE)
      )
      bind_rows(base, fila_total)
    })
    
    # Bloque tabla: mensaje vacio o TablaReactable ----
    # TablaReactable server se registra siempre (patron eager); renderUI controla la vista.
    output$bloque_tabla <- renderUI({
      if (nrow(data_base_r()) == 0) {
        return(
          div(
            style = paste(
              "padding:24px 16px; text-align:center; color:#6c757d;",
              "font-style:italic; border:1px solid #dee2e6; border-radius:6px;"
            ),
            icon("inbox", style = "font-size:28px; margin-bottom:8px; display:block;"),
            "No hay lotes pendientes con los filtros seleccionados."
          )
        )
      }
      TablaReactableUI(ns("TablaPendientes"),
                       titulo      = "Lotes Pendientes",
                       footer      = "Clic en el boton para asignar una orden de compra al lote.",
                       footer_tipo = "info"
      )
    })
    
    # Tabla principal con TablaReactable ----
    # page_size = 99999L deshabilita la paginacion efectivamente (scroll completo).
    # Si TablaReactable expone pagination = FALSE en version futura, preferir ese parametro.
    TablaReactable(
      id              = "TablaPendientes",
      data            = data_tabla,
      modo_seleccion  = "celda",
      id_col          = NULL,
      col_header_n    = 1L,
      cols_activos    = "Asignar",
      sortable        = FALSE,
      searchable      = TRUE,
      page_size       = 99999L,
      compact         = TRUE,
      mostrar_badge   = FALSE,
      mostrar_nota    = FALSE,
      modal_icon      = "arrow-right",
      modal_size      = "xl",
      modal_titulo_fn = function(sel) {
        paste0("Asignar Orden de Compra \u2014 Lote ", sel$fila$CLLotCod[[1]])
      },
      modal_pre_fn = function(sel) {
        if (isTRUE(sel$fila$Asignar == "")) return(invisible(NULL))
        dd_asignar_rv(list(fila_completa = sel$fila))
      },
      modal_contenido_fn = function(sel) AsignarOrdenCompraUI(ns("mod_formulario")),
      columnas = list(
        # Columna de accion al inicio: boton asignar orden de compra
        Asignar = reactable::colDef(
          name = "", minWidth = 50, html = TRUE,
          cell = function(v) {
            if (v == "") return("")
            as.character(tags$span(
              style = paste(
                "display:inline-flex;align-items:center;justify-content:center;",
                "width:26px;height:26px;border-radius:6px;",
                "background:#C11007;color:white;font-size:12px;cursor:pointer;"
              ),
              icon("arrow-right")
            ))
          }
        ),
        # Columnas descriptivas
        Sucursal       = reactable::colDef(name = "Sucursal",         minWidth = 90),
        CLPdcCod       = reactable::colDef(name = "Cod. Pedido",      minWidth = 110),
        PdcRefCli      = reactable::colDef(name = "Pedido",           minWidth = 90),
        CLLotCod       = reactable::colDef(name = "Lote",             minWidth = 80),
        OrdenCompra    = reactable::colDef(name = "Orden de Compra",  minWidth = 130),
        FecAsignLote   = reactable::colDef(name = "Fecha Asignacion", minWidth = 130),
        CLLinNegNo     = reactable::colDef(name = "Linea Negocio",    minWidth = 110),
        Segmento       = reactable::colDef(name = "Segmento",         minWidth = 100),
        Categoria      = reactable::colDef(name = "Categoria",        minWidth = 100),
        Producto       = reactable::colDef(name = "Producto",         minWidth = 120),
        CliNitPpal     = reactable::colDef(name = "NIT Principal",    minWidth = 110),
        PerRazSoc      = reactable::colDef(name = "Cliente",          minWidth = 180),
        CLCliNit       = reactable::colDef(name = "NIT Pedido",       minWidth = 110),
        ClientePedido  = reactable::colDef(name = "Cliente Pedido",   minWidth = 180),
        SegmentoRacafe = reactable::colDef(name = "Tipo Cliente",     minWidth = 110),
        # Columnas numericas — texto plano via format() para evitar HTML crudo en celdas
        SacLote = reactable::colDef(name = "Sacos Lote", minWidth = 100,
                                    cell = function(v) if (is.na(v)) "\u2014" else .fmt(v)),
        PdcPrecioKilo = reactable::colDef(name = "Precio Kilo", minWidth = 100,
                                          cell = function(v) if (is.na(v)) "\u2014" else .fmt_precio(v)),
        PendProducir = reactable::colDef(name = "Sacos Pend. Producir", minWidth = 140,
                                         cell = function(v) if (is.na(v)) "\u2014" else .fmt(v)),
        PendDespachar = reactable::colDef(name = "Sacos Pend. Despachar", minWidth = 150,
                                          cell = function(v) if (is.na(v)) "\u2014" else .fmt(v)),
        PendFacturar = reactable::colDef(name = "Sacos Pend. Facturar", minWidth = 150,
                                         cell = function(v) if (is.na(v)) "\u2014" else .fmt(v))
      )
    )
    
    # Descarga de datos ----
    output$btn_descargar <- downloadHandler(
      filename = function() {
        paste0("LotesPendientes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx(data_tabla(), file, rowNames = FALSE)
      }
    )
  })
}


# App de prueba ----
ui <- bs4DashPage(
  title    = "Prueba Pendientes",
  header   = bs4DashNavbar(),
  sidebar  = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer   = bs4DashFooter(),
  body     = bs4DashBody(PendientesUI("pendientes"))
)

server <- function(input, output, session) {
  Pendientes(
    id      = "pendientes",
    dat     = reactive({ BaseDatos }),
    dat_ped = reactive({ BaseDatos }),
    usr     = reactive("CMEDINA")
  )
}

shinyApp(ui, server)
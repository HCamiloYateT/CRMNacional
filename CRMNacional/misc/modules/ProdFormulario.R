# ProdFormulario ----

# UI ----
ProdFormularioUI <- function(id) {
  ns <- NS(id)
  box(
    title = "Registro de Producto CRM", width = 12,
    status = "white", collapsible = TRUE, collapsed = FALSE,
    uiOutput(ns("info_producto")),
    tags$hr(style = "margin:8px 0 12px;"),
    fluidRow(
      column(12,
             shinyWidgets::pickerInput(
               ns("frm_Categoria"), h6("Categoría"),
               choices = c("Seleccionar..." = "", .app_choices$categoria),
               width   = "100%",
               options = shinyWidgets::pickerOptions(liveSearch = TRUE, noneSelectedText = "Seleccionar...")
             )
      ),
      column(12,
             shinyWidgets::pickerInput(
               ns("frm_Producto"), h6("Producto"),
               choices = c("Seleccionar..." = "", .app_choices$producto),
               width   = "100%",
               options = shinyWidgets::pickerOptions(liveSearch = TRUE, noneSelectedText = "Seleccionar...")
             )
      ),
      column(12,
             shinyWidgets::pickerInput(
               ns("frm_Excluir"), h6("Excluir"),
               choices = c("NO", "SI"),
               width   = "100%"
             )
      )
    ),
    tags$hr(),
    div(style = "text-align:right;",
        actionButton(ns("btn_guardar"), "Guardar cambios",
                     icon = icon("save"), class = "btn-danger"))
  )
}

# Server ----
# dat: reactive con data principal (para YTD y último pedido)
ProdFormulario <- function(id, identidad, dat, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0L)
    
    # Registro existente en CRMNALPRODS ----
    data_prod <- reactive({
      id_val <- identidad()
      req(!is.null(id_val), length(id_val$linneg_cod) == 1L, !is.na(id_val$linneg_cod))
      refresh()
      CargarDatos("CRMNALPRODS") %>%
        dplyr::filter(
          LinNegCod == id_val$linneg_cod,
          LinProCod == id_val$linpro_cod,
          MCCod     == id_val$mc_cod,
          MrcCod    == id_val$mrc_cod
        ) %>%
        dplyr::arrange(dplyr::desc(as.Date(FecProceso))) %>%
        dplyr::slice(1)
    })
    
    # Filas de dat() filtradas por producto ----
    datos_prod_r <- reactive({
      id_val <- identidad()
      req(!is.null(id_val))
      dat() %>%
        dplyr::filter(
          LinNegCod == id_val$linneg_cod,
          LinProCod == id_val$linpro_cod,
          MCCod     == id_val$mc_cod,
          MrcCod    == id_val$mrc_cod
        )
    })
    
    # Panel de información ----
    output$info_producto <- renderUI({
      id_val  <- identidad()
      req(!is.null(id_val))
      dp      <- datos_prod_r()
      ano_act <- lubridate::year(Sys.Date())
      
      # YTD sacos ----
      sacos_ytd <- dp %>%
        dplyr::filter(lubridate::year(FecFact) == ano_act) %>%
        dplyr::summarise(s = sum(SacLote, na.rm = TRUE)) %>%
        dplyr::pull(s)
      
      # Último pedido ----
      cod_ult <- dp %>%
        dplyr::arrange(dplyr::desc(PdcFecCre)) %>%
        dplyr::pull(CLPdcCod) %>%
        dplyr::first()
      
      ult <- dp %>% dplyr::filter(CLPdcCod == cod_ult)
      
      if (nrow(ult) > 0) {
        fec_ult     <- tryCatch(format(as.Date(ult$PdcFecCre[1]), "%d %b %Y"), error = function(e) "\u2014")
        sacos_ult   <- format(sum(ult$SacLote, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
        usuario_ult <- ult$Usuario[1]    %||% "\u2014"
        cliente_ult <- ult$PerRazSoc[1] %||% "\u2014"
      } else {
        fec_ult <- sacos_ult <- usuario_ult <- cliente_ult <- "\u2014"
      }
      
      # Helpers ----
      .seccion <- function(label) {
        tags$p(
          style = paste0(
            "font-size:10px; font-weight:700; color:#94A3B8; ",
            "text-transform:uppercase; letter-spacing:0.05em; margin:8px 0 5px;"
          ),
          label
        )
      }
      
      .item <- function(icono, label, valor) {
        tags$div(
          style = "display:flex; align-items:flex-start; gap:6px; margin-bottom:3px;",
          tags$span(icon(icono), style = "color:#64748B; font-size:10px; width:13px; margin-top:1px;"),
          tags$span(label, style = "font-size:11px; color:#64748B; min-width:110px; flex-shrink:0;"),
          tags$span(valor %||% "\u2014", style = "font-size:11px; font-weight:600; color:#374151;")
        )
      }
      
      tags$div(
        style = paste0(
          "background:#F8FAFC; border:1px solid #E2E8F0;",
          "border-radius:6px; padding:10px 14px;"
        ),
        fluidRow(
          # Columna 1 — Identificación
          column(4,
                 .seccion("Producto"),
                 .item("network-wired", "Línea negocio:", id_val$linneg),
                 .item("tag",           "Nombre comerc.:", id_val$mc_nom),
                 .item("award",         "Marca:",           id_val$marca)
          ),
          # Columna 2 — YTD
          column(4,
                 .seccion(paste0("Sacos YTD ", ano_act)),
                 tags$div(
                   style = "font-size:22px; font-weight:700; color:#1E40AF; margin:4px 0 2px;",
                   format(sacos_ytd, big.mark = ".", decimal.mark = ",")
                 ),
                 tags$span("sacos en el año", style = "font-size:10px; color:#64748B;")
          ),
          # Columna 3 — Último pedido
          column(4,
                 .seccion("Último pedido"),
                 .item("calendar",       "Fecha:",   fec_ult),
                 .item("weight-hanging", "Sacos:",   sacos_ult),
                 .item("user-pen",       "Usuario:", usuario_ult),
                 .item("building-user",  "Cliente:", cliente_ult)
          )
        )
      )
    })
    
    # Poblar pickers desde registro existente ----
    observe({
      prod    <- data_prod()
      cat_val <- if (nrow(prod) > 0) prod$Categoria[1] %||% "" else ""
      pro_val <- if (nrow(prod) > 0) prod$Producto[1]  %||% "" else ""
      exc_val <- if (nrow(prod) > 0) prod$Excluir[1]   %||% "NO" else "NO"
      
      shinyWidgets::updatePickerInput(session, "frm_Categoria", selected = cat_val)
      shinyWidgets::updatePickerInput(session, "frm_Producto",  selected = pro_val)
      shinyWidgets::updatePickerInput(session, "frm_Excluir",   selected = exc_val)
    })
    
    # Payload al schema de CRMNALPRODS ----
    construir_payload <- function() {
      id_val <- identidad()
      data.frame(
        FecProceso = as.character(Sys.Date()),
        Usr        = usr(),
        LinNegCod  = id_val$linneg_cod,
        LinNeg     = id_val$linneg     %||% NA_character_,
        LinProCod  = id_val$linpro_cod,
        LinProNom  = id_val$linpro_nom %||% NA_character_,
        MCCod      = id_val$mc_cod,
        MCNom      = id_val$mc_nom     %||% NA_character_,
        MrcCod     = id_val$mrc_cod,
        Marca      = id_val$marca      %||% NA_character_,
        Excluir    = input$frm_Excluir   %||% "NO",
        Categoria  = input$frm_Categoria %||% NA_character_,
        Producto   = input$frm_Producto  %||% NA_character_,
        stringsAsFactors = FALSE
      )
    }
    
    # Guardar con confirmación ----
    observeEvent(input$btn_guardar, {
      confirmSweetAlert(
        session    = session,
        inputId    = ns("confirm_guardar"),
        title      = "Confirmar guardado",
        text       = "\u00bfDesea registrar este producto en el CRM?",
        type       = "warning",
        btn_labels = c("Cancelar", "Guardar"),
        btn_colors = c("#E7180B", "#1F7A55"),
        html = TRUE, width = "400px"
      )
    })
    
    observeEvent(input$confirm_guardar, {
      req(isTRUE(input$confirm_guardar))
      tryCatch({
        SubirDatos(construir_payload(), "CRMNALPRODS")
        showNotification("Producto registrado exitosamente.", type = "message")
        refresh(refresh() + 1L)
      }, error = function(e) {
        showNotification(paste("Error al guardar:", e$message), type = "error", duration = NULL)
      })
    })
    
    list(refresh = refresh)
  })
}
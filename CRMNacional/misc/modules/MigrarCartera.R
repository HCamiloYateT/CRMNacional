# MigrarCartera -------------------------------------------------------
MigrarCarteraUI <- function(ns) {
  tagList(
    fluidRow(
      column(6, ind_picker("frm_AsesorOrigen", "Asesor Origen", .IND_CHO$personas, ns = ns)),
      column(6, ind_picker("frm_AsesorDestino", "Asesor Destino", .IND_CHO$personas, ns = ns))
    ),
    uiOutput(ns("resumen_migracion")),
    div(style = "text-align: right; margin-top: 8px;",
        actionButton(ns("btn_migrar_asesor"), "Migrar Cuentas", icon = icon("people-arrows"),
                     class = "btn-danger"))
  )
}
MigrarCartera <- function(id, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Cuentas principales vigentes del asesor origen
    resumen_migracion_r <- reactive({
      req(input$frm_AsesorOrigen, input$frm_AsesorDestino)
      req(nzchar(input$frm_AsesorOrigen), nzchar(input$frm_AsesorDestino))
      CargarDatos("CRMNALCLIENTE") %>%
        dplyr::mutate(FecProceso = as_datetime(FecProceso)) %>%
        dplyr::group_by(CliNitPpal, LinNegCod) %>%
        dplyr::arrange(dplyr::desc(FecProceso)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::filter(Asesor == input$frm_AsesorOrigen)
    })
    
    # Resumen visual: cuántas cuentas se migrarán
    output$resumen_migracion <- renderUI({
      if (is.null(input$frm_AsesorOrigen) || !nzchar(input$frm_AsesorOrigen %||% "")) return(NULL)
      n <- tryCatch(nrow(resumen_migracion_r()), error = function(e) 0)
      tags$p(style = "font-size:13px; color:#64748B; margin-top:8px;", icon("circle-info"), " ",
             paste0(n, " cuenta(s) principal(es) ser\u00e1n migradas de ",
                    input$frm_AsesorOrigen %||% "\u2014", " a ", input$frm_AsesorDestino %||% "\u2014", "."))
    })
    
    # Validaciones antes de confirmar la migración
    observeEvent(input$btn_migrar_asesor, {
      req(input$frm_AsesorOrigen, input$frm_AsesorDestino)
      if (input$frm_AsesorOrigen == input$frm_AsesorDestino) {
        showNotification("El asesor origen y destino no pueden ser el mismo.", type = "error")
        return()
      }
      n <- tryCatch(nrow(resumen_migracion_r()), error = function(e) 0)
      if (n == 0) {
        showNotification("El asesor origen no tiene cuentas asignadas actualmente.", type = "warning")
        return()
      }
      confirmSweetAlert(session = session, inputId = ns("confirm_migracion"),
                        title = "Confirmar migraci\u00f3n de cartera",
                        text = paste0("Se migrar\u00e1n ", n, " cuenta(s) de ", input$frm_AsesorOrigen,
                                      " a ", input$frm_AsesorDestino, ". \u00bfDeseas continuar?"),
                        type = "warning", btn_labels = c("Cancelar", "Migrar"),
                        btn_colors = c("#E7180B", "#1F7A55"), html = TRUE, width = "440px")
    })
    
    # Escritura masiva: un registro nuevo por cuenta, con el asesor destino
    observeEvent(input$confirm_migracion, {
      req(isTRUE(input$confirm_migracion))
      tryCatch({
        destino <- isolate(input$frm_AsesorDestino)
        base <- isolate(resumen_migracion_r())
        payload <- base %>%
          dplyr::transmute(FecProceso = Sys.time(), Usr = usr(), LinNegCod, CLCliNit, CliNitPpal,
                           Segmento, SSPpto, MNFCCPpto, Asesor = destino, NumMesesRecuperar, Excluir)
        racafe::AgregarDatos(payload, "CRMNALCLIENTE")
        showNotification(paste0(nrow(payload), " cuenta(s) migradas exitosamente."), type = "message")
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error en la migraci\u00f3n:", e$message), type = "error", duration = NULL)
      })
    })
  })
}
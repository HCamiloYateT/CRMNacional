NotificacionesDropUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    style = "max-height:340px; overflow-y:auto;",
    shiny::uiOutput(ns("items"))
  )
}
NotificacionesDropServer <- function(id, dat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helpers ----
    .color <- function(p) switch(p, "Urgente" = "danger", "Importante" = "warning", "info")
    .badge_status <- function(ps) {
      if ("Urgente" %in% ps)    return("danger")
      if ("Importante" %in% ps) return("warning")
      "info"
    }
    
    # Pendientes ordenados por prioridad y fecha ----
    pendientes_r <- shiny::reactive({
      dat() %>%
        dplyr::filter(Cumplido == 0, Archivado == 0) %>%
        dplyr::arrange(
          dplyr::case_when(
            Prioridad == "Urgente"    ~ 1L,
            Prioridad == "Importante" ~ 2L,
            TRUE                      ~ 3L
          ),
          FechaCumplimiento
        )
    })
    
    # Items clicables ----
    output$items <- shiny::renderUI({
      df <- pendientes_r()
      
      if (nrow(df) == 0) {
        return(shiny::tags$div(
          class = "dropdown-item text-muted",
          style = "font-size:12px; padding:12px 16px;",
          shiny::icon("check-circle"), " Sin tareas pendientes"
        ))
      }
      
      items_ui <- lapply(seq_len(nrow(df)), function(i) {
        fila  <- df[i, ]
        color <- .color(fila$Prioridad)
        fecha <- tryCatch(
          format(as.Date(fila$FechaCumplimiento), "%d %b"),
          error = function(e) "\u2014"
        )
        
        shiny::tags$a(
          class   = "dropdown-item",
          href    = "javascript:void(0);",
          style   = "white-space:normal; padding:8px 14px; border-bottom:1px solid #F1F5F9;",
          onclick = sprintf(
            "Shiny.setInputValue('%s', %d, {priority:'event'});",
            ns("item_click"), fila$cons
          ),
          shiny::tags$div(
            style = "display:flex; align-items:flex-start; gap:8px;",
            shiny::tags$span(
              class = paste0("badge badge-", color),
              style = "margin-top:2px; flex-shrink:0; font-size:9px;",
              fila$Prioridad
            ),
            shiny::tags$div(
              shiny::tags$span(
                fila$titulo,
                style = "font-size:12px; font-weight:600; color:#374151; line-height:1.4;"
              ),
              if (nzchar(trimws(fila$Descripcion %||% ""))) shiny::tags$div(
                trimws(fila$Descripcion),
                style = "font-size:10px; color:#64748B; margin-top:1px;"
              ),
              shiny::tags$div(
                style = "font-size:10px; color:#94A3B8; margin-top:2px;",
                shiny::icon("calendar-alt"), " ", fecha
              )
            )
          )
        )
      })
      
      do.call(shiny::tagList, items_ui)
    })
    
    list(
      n          = shiny::reactive(nrow(pendientes_r())),
      badge_st   = shiny::reactive({
        df <- pendientes_r()
        if (nrow(df) == 0) return(NULL)
        .badge_status(df$Prioridad)
      }),
      item_click = shiny::reactive(input$item_click)
    )
  })
}
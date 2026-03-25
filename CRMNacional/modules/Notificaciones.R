NotificacionesUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("notificaciones"))
}
Notificaciones <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$notificaciones <- renderUI({
      # Si dat es reactivo, usa tareas()
      df <- if (is.reactive(dat)) dat() else dat
      
      # Filtra tareas no cumplidas y no archivadas
      tareas_filtradas <- df[df$Cumplido == 0 & df$Archivado == 0, ]
      
      tagList(
        lapply(seq_len(nrow(tareas_filtradas)), function(i) {
          notificationItem(
            # inputId = ns(paste0("tarea_", tareas_filtradas$cons[i])),
            inputId = i,
            text = HTML(sprintf('<div style="white-space: normal; word-break: break-word; max-width: 350px;">
                                 <b>%s</b><br>%s<br><i>Prioridad: %s</i></div>',
                                tareas_filtradas$titulo[i],
                                tareas_filtradas$Descripcion[i],
                                tareas_filtradas$Prioridad[i]
                                )
                        ),
            status = switch(tareas_filtradas$Prioridad[i], 
                            "Urgente" = "danger",
                            "Importante" = "warning",
                            "Media" = "info",
                            "Baja" = "success",
                            "primary"),
            icon = icon("circle")
            )
          })
        )
    })
  })
}
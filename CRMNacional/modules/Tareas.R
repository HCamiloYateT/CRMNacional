# Helpers internos ----

# Prioridades en mayúsculas — orden canonico del kanban
.TSK_PRIORIDADES <- c("URGENTE", "IMPORTANTE", "MEDIA", "BAJA")

.TSK_COLORES <- list(
  URGENTE    = list(header = "#c0392b", bg = "#fdf2f2", border = "#e74c3c", badge = "#e74c3c"),
  IMPORTANTE = list(header = "#d35400", bg = "#fef5ec", border = "#e67e22", badge = "#e67e22"),
  MEDIA      = list(header = "#b7950b", bg = "#fefdf0", border = "#f1c40f", badge = "#f39c12"),
  BAJA       = list(header = "#1a5276", bg = "#eaf4fb", border = "#2980b9", badge = "#2980b9")
)

# Conexion unica para UPDATE / DELETE
.tsk_ejecutar_query <- function(query, notes_data, rv, on_success = NULL) {
  con <- NULL
  tryCatch({
    con <- dbConnect(
      RMySQL::MySQL(),
      dbname = "Analitica", host = "localhost", port = 3306,
      user = "datos", password = "R4c4f3*1", DBMSencoding = "UTF-8"
    )
    res <- dbSendQuery(con, query)
    dbClearResult(res)
    isolate({
      notes_data(CargarDatos("CRMNALNOTAS"))
      rv$last_update <- Sys.time()
    })
    if (!is.null(on_success)) on_success()
  }, finally = {
    if (!is.null(con) && dbIsValid(con)) dbDisconnect(con)
  })
}

# Extraer etiquetas unicas desde columna pipe-separated
.tsk_extraer_etiquetas <- function(df) {
  con_tags <- df %>%
    filter(!is.na(Etiquetas), Etiquetas != "") %>%
    pull(Etiquetas) %>%
    strsplit("\\|") %>%
    unlist() %>%
    str_to_sentence() %>%
    unique() %>%
    sort()
  sin_tag <- any(is.na(df$Etiquetas) | df$Etiquetas == "")
  if (sin_tag) c(con_tags, "Sin etiqueta") else con_tags
}

# Envio de email blastula con manejo silencioso de errores
.tsk_enviar_email <- function(to, subject, body_md) {
  tryCatch({
    compose_email(body = md(body_md)) %>%
      smtp_send(
        to = to, from = "reportesanalitica@racafe.com",
        cc = "hcyate@racafe.com", subject = subject,
        credentials = creds_file(file = "/home/htamara/1_Analitica/Correos/cred"),
        verbose = FALSE
      )
  }, error = function(e) cat("Error email:", conditionMessage(e), "\n"))
}

# Badge HTML estatico
.tsk_badge <- function(texto, color = "#212f3d") {
  tags$span(class = "tsk-badge", style = paste0("background-color:", color, ";"), texto)
}

# Construir data.frame para SubirDatos (sin campo Responsable)
.tsk_construir_registro <- function(input, usr, note_id, fecha_cum,
                                    list_items, list_checked) {
  data.frame(
    cons              = note_id,
    titulo            = trimws(input$TSK_Titulo),
    Usuario           = usr(),
    FechaHoraCrea     = Sys.time(),
    Tipo              = input$TSK_TipoNota,
    Descripcion       = if (is.null(input$TSK_Descripcion)) "" else trimws(input$TSK_Descripcion),
    FechaCumplimiento = fecha_cum,
    Actividades       = paste(list_items, collapse = "|"),
    Cumplidas         = paste(list_checked, collapse = "|"),
    Etiquetas         = if (is.null(input$TSK_Tags)) "" else paste(input$TSK_Tags, collapse = "|"),
    Prioridad         = str_to_upper(trimws(input$TSK_Prioridad)),
    Archivado         = 0L, Cumplido = 0L, Respuesta = "",
    stringsAsFactors  = FALSE
  )
}

# Limpiar observers dinamicos cuyo note_id ya no está en la vista
.tsk_limpiar_observers <- function(rv_list, prefijo, note_ids) {
  for (btn_id in names(rv_list)) {
    note_id <- suppressWarnings(as.numeric(gsub(prefijo, "", btn_id)))
    if (!is.na(note_id) && !(note_id %in% note_ids)) {
      rv_list[[btn_id]]$destroy()
      rv_list[[btn_id]] <- NULL
    }
  }
  rv_list
}

# Factory de observer dinamico — patron unico para los 5 tipos de accion
.tsk_registrar_observer <- function(rv, rv_slot, prefijo, ids, session, input, handler_fn) {
  rv[[rv_slot]] <- .tsk_limpiar_observers(rv[[rv_slot]], prefijo, ids)
  lapply(ids, function(note_id) {
    btn_id <- paste0(prefijo, note_id)
    if (is.null(rv[[rv_slot]][[btn_id]])) {
      rv[[rv_slot]][[btn_id]] <- observeEvent(
        input[[btn_id]], handler_fn(note_id),
        ignoreInit = TRUE, ignoreNULL = TRUE
      )
    }
  })
}

# Render del checklist interactivo en el modal — checkboxes con IDs Shiny
.tsk_render_checklist_modal <- function(note_row, session) {
  items   <- unlist(strsplit(as.character(note_row$Actividades[[1]]), "\\|"))
  checked <- unlist(strsplit(as.character(note_row$Cumplidas[[1]]),   "\\|"))
  if (length(items) == 0 || all(items == "")) return(NULL)
  
  n_total  <- length(items)
  n_done   <- sum(checked == "1", na.rm = TRUE)
  pct      <- if (n_total > 0) round(100 * n_done / n_total) else 0L
  col_prog <- (.TSK_COLORES[[as.character(note_row$Prioridad[[1]])]] %||%
                 list(header = "#888"))$header
  cons_id  <- note_row$cons[[1]]
  
  # Cada item como checkboxInput Shiny con ID unico — permite observeEvent posterior
  items_ui <- Map(function(item, chk, idx) {
    tachado <- isTRUE(chk == "1")
    tags$div(
      class = "tsk-checklist-item",
      checkboxInput(
        inputId = session$ns(paste0("chk_", cons_id, "_", idx)),
        label   = tags$span(class = if (tachado) "tsk-item-label-done" else "", item),
        value   = tachado,
        width   = "100%"
      )
    )
  }, items, checked, seq_along(items))
  
  tagList(
    tags$div(
      class = "tsk-progress-wrap",
      tags$div(class = "tsk-progress-bar",
               tags$div(class = "tsk-progress-fill",
                        style = paste0("width:", pct, "%;background:", col_prog, ";"))),
      tags$span(class = "tsk-progress-label",
                FormatearTexto(paste0(n_done, "/", n_total, " completadas — ", pct, "%"),
                               tamano_pct = 0.85))
    ),
    tags$div(class = "tsk-checklist-modal", do.call(tagList, items_ui))
  )
}

# Render del cuerpo del modal de detalle — sin botones (van en footer)
.tsk_modal_body <- function(note_row, session) {
  if (is.null(note_row) || nrow(note_row) == 0) return(tags$p("Sin datos."))
  
  pri         <- as.character(note_row$Prioridad[[1]])
  col_info    <- .TSK_COLORES[[pri]] %||%
    list(header = "#555", bg = "#fafafa", border = "#ccc", badge = "#888")
  tipo_label  <- if (note_row$Tipo[[1]] == "list") "Tarea" else "Nota de Reunión"
  estado_label <- dplyr::case_when(
    note_row$Cumplido[[1]]  == 1L ~ "Cumplida",
    note_row$Archivado[[1]] == 1L ~ "Archivada",
    TRUE                          ~ "Pendiente"
  )
  estado_color <- if (estado_label == "Cumplida") "#1a7a3f"
  else if (estado_label == "Archivada") "#888" else "#444"
  
  etiquetas_ui <- if (!is.na(note_row$Etiquetas[[1]]) && note_row$Etiquetas[[1]] != "") {
    tags$div(class = "tsk-tags-wrap",
             lapply(strsplit(note_row$Etiquetas[[1]], "\\|")[[1]], function(t)
               .tsk_badge(str_to_title(t), col_info$badge)))
  }
  
  vence_ui <- if (note_row$Tipo[[1]] == "list" && !is.na(note_row$FechaCumplimiento[[1]])) {
    vencida <- tryCatch(
      as.Date(note_row$FechaCumplimiento[[1]]) < Sys.Date(), error = function(e) FALSE
    )
    tags$div(class = "tsk-meta-row", icon("calendar-days"),
             FormatearTexto(
               paste0("Vence: ", format(as.Date(note_row$FechaCumplimiento[[1]]), "%d/%m/%Y")),
               color = if (vencida) "#c0392b" else "#555", negrita = isTRUE(vencida)
             ))
  }
  
  tags$div(
    class = "tsk-modal-detalle",
    style = paste0("border-left:4px solid ", col_info$border,
                   ";background:", col_info$bg, ";padding:14px;border-radius:6px;"),
    
    tags$div(class = "tsk-modal-header-meta",
             .tsk_badge(tipo_label,  col_info$header),
             .tsk_badge(pri,          col_info$badge),
             .tsk_badge(estado_label, estado_color)),
    
    if (!is.na(note_row$Descripcion[[1]]) && note_row$Descripcion[[1]] != "")
      tags$div(class = "tsk-modal-desc",
               tags$p(FormatearTexto("Descripción:", negrita = TRUE)),
               tags$p(note_row$Descripcion[[1]])),
    
    if (note_row$Tipo[[1]] == "list" &&
        !is.na(note_row$Actividades[[1]]) && note_row$Actividades[[1]] != "")
      tags$div(class = "tsk-modal-section",
               tags$p(FormatearTexto("Actividades:", negrita = TRUE)),
               .tsk_render_checklist_modal(note_row, session)),
    
    vence_ui,
    
    tags$div(class = "tsk-meta-row", icon("clock"),
             FormatearTexto(
               paste0("Creación: ", format(as.POSIXct(note_row$FechaHoraCrea[[1]]),
                                           "%d/%m/%Y %H:%M")),
               tamano_pct = 0.85, negrita = FALSE, color = "#888"
             )),
    
    etiquetas_ui,
    
    if (!is.na(note_row$Respuesta[[1]]) && note_row$Respuesta[[1]] != "")
      tags$div(class = "tsk-card-comment",
               icon("comment-dots"),
               FormatearTexto(note_row$Respuesta[[1]], tamano_pct = 0.82, negrita = FALSE))
  )
}

# Footer del modal con botones de accion contextuales
.tsk_modal_footer <- function(note_row, session) {
  cons_id  <- note_row$cons[[1]]
  cumplido <- isTRUE(note_row$Cumplido[[1]] == 1L)
  archivado <- isTRUE(note_row$Archivado[[1]] == 1L)
  
  tagList(
    tags$div(class = "tsk-modal-footer-actions",
             # Comentar
             span(title = "Comentar",
                  actionBttn(session$ns(paste0("respond_note_", cons_id)), "Comentar",
                             icon = icon("message"), style = "bordered",
                             size = "xs", color = "primary")),
             # Cumplida / Pendiente — icono y label contextual
             span(title = if (cumplido) "Marcar como pendiente" else "Marcar como cumplida",
                  actionBttn(session$ns(paste0("accomplish_note_", cons_id)),
                             if (cumplido) "Pendiente" else "Cumplida",
                             icon = icon(if (cumplido) "circle-xmark" else "circle-check"),
                             style = "bordered", size = "xs",
                             color = if (cumplido) "warning" else "success")),
             # Archivar / Desarchivar — icono y label contextual
             span(title = if (archivado) "Desarchivar" else "Archivar",
                  actionBttn(session$ns(paste0("archive_note_", cons_id)),
                             if (archivado) "Desarchivar" else "Archivar",
                             icon = icon(if (archivado) "box-open" else "archive"),
                             style = "bordered", size = "xs", color = "warning")),
             # Eliminar
             span(title = "Eliminar nota",
                  actionBttn(session$ns(paste0("delete_note_", cons_id)), "Eliminar",
                             icon = icon("trash"), style = "bordered",
                             size = "xs", color = "danger"))
    ),
    modalButton("Cerrar")
  )
}


# Formulario de Creación ----
TaskCreationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, textInput(ns("TSK_Titulo"), Obligatorio("Título"), "", width = "100%"))
    ),
    fluidRow(
      column(12,
             prettyRadioButtons(
               inputId = ns("TSK_TipoNota"), label = h6("Tipo de nota"),
               choices = c("Notas de Reunión" = "text", "Tarea" = "list"),
               shape = "round", status = "danger", fill = TRUE, inline = TRUE
             )
      )
    ),
    fluidRow(
      column(12, textAreaInput(ns("TSK_Descripcion"), h6("Descripción"), "",
                               height = "120px", width = "100%"))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'list'", ns("TSK_TipoNota")),
      fluidRow(
        column(12,
               dateInput(inputId = ns("TSK_FechaFin"), label = h6("Fecha de Cumplimiento"),
                         value = Sys.Date(), width = "100%", language = "es",
                         format = "dd/mm/yyyy")
        )
      ),
      fluidRow(
        column(12,
               div(class = "tsk-switch-inline", style = "margin:10px 0;",
                   switchInput(inputId = ns("TSK_CheckList"),
                               label = "Incluir Lista de Actividades",
                               onStatus = "success", offStatus = "danger", size = "mini",
                               onLabel = "Sí", offLabel = "No", width = "auto"))
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("TSK_CheckList")),
        div(id = ns("list_items_container"),
            fluidRow(
              column(1, checkboxInput(ns("item_check_1"), "", FALSE, width = "100%")),
              column(9, textInput(ns("item_text_1"), "", placeholder = "Actividad 1",
                                  width = "100%")),
              column(2, span(title = "Eliminar actividad",
                             actionBttn(ns("remove_item_1"), "", style = "bordered",
                                        icon = icon("trash"), color = "danger", size = "xs")))
            )
        ),
        fluidRow(
          column(12, style = "text-align:right;margin-top:10px;",
                 span(title = "Añadir nueva actividad",
                      actionBttn(ns("add_list_item"), "Añadir elemento",
                                 style = "bordered", icon = icon("plus"),
                                 color = "primary", size = "xs"))
          )
        )
      )
    ),
    fluidRow(
      column(6,
             ListaDesplegable(inputId = ns("TSK_Tags"), label = "Etiquetas",
                              choices = NULL, multiple = TRUE)
      ),
      column(6,
             ListaDesplegable(inputId = ns("TSK_Prioridad"), label = "Prioridad",
                              choices = .TSK_PRIORIDADES, selected = NULL, multiple = FALSE)
      )
    ),
    br(),
    BotonGuardar(ns("TSK_Guardar"), label = "Guardar Nota/Tarea", align = "center")
  )
}
TaskCreation <- function(id, usr, notes_data) {
  moduleServer(id, function(input, output, session) {
    
    list_item_count <- reactiveVal(1L)
    
    # Cargar etiquetas existentes al iniciar
    observe({
      tryCatch({
        notas <- CargarDatos("CRMNALNOTAS")
        if (nrow(notas) > 0) {
          etiquetas <- notas %>%
            pull(Etiquetas) %>%
            paste(collapse = "|") %>%
            strsplit("\\|") %>%
            unlist() %>%
            unique() %>%
            .[. != ""] %>%
            str_to_sentence()
          updateSelectizeInput(session, "TSK_Tags", choices = etiquetas, selected = character(0))
        }
      }, error = function(e) cat("Error etiquetas:", conditionMessage(e), "\n"))
    })
    
    # Agregar item de checklist dinamico
    observeEvent(input$add_list_item, {
      n <- list_item_count() + 1L
      list_item_count(n)
      insertUI(
        selector = paste0("#", session$ns("list_items_container")),
        ui = div(
          id = session$ns(paste0("list_item_", n)), class = "tsk-form-item",
          fluidRow(
            column(1, checkboxInput(session$ns(paste0("item_check_", n)), "", FALSE,
                                    width = "100%")),
            column(9, textInput(session$ns(paste0("item_text_", n)), "",
                                placeholder = paste("Actividad", n), width = "100%")),
            column(2, span(title = "Eliminar",
                           actionBttn(session$ns(paste0("remove_item_", n)), "",
                                      style = "bordered", icon = icon("trash"),
                                      color = "danger", size = "xs")))
          )
        )
      )
      local({
        idx <- n
        observeEvent(input[[paste0("remove_item_", idx)]], {
          removeUI(selector = paste0("#", session$ns(paste0("list_item_", idx))))
        }, ignoreInit = TRUE)
      })
    })
    
    # Limpiar primer item sin eliminarlo
    observeEvent(input$remove_item_1, {
      updateTextInput(session, "item_text_1", value = "")
      updateCheckboxInput(session, "item_check_1", value = FALSE)
    }, ignoreInit = TRUE)
    
    # Guardar nota o tarea
    observeEvent(input$TSK_Guardar, {
      waiter_show(html = preloader2$html, color = preloader2$color)
      
      if (is.null(input$TSK_Titulo) || trimws(input$TSK_Titulo) == "") {
        waiter_hide()
        showNotification("El título es obligatorio", type = "error", duration = 3)
        return()
      }
      if (is.null(input$TSK_Prioridad) || input$TSK_Prioridad == "") {
        waiter_hide()
        showNotification("Debe seleccionar una prioridad", type = "error", duration = 3)
        return()
      }
      
      tryCatch({
        existentes <- CargarDatos("CRMNALNOTAS")
        note_id    <- if (nrow(existentes) == 0) 1L else max(existentes$cons, na.rm = TRUE) + 1L
        
        list_items <- list_checked <- character(0)
        if (input$TSK_TipoNota == "list" && isTRUE(input$TSK_CheckList)) {
          for (i in seq_len(list_item_count())) {
            txt <- input[[paste0("item_text_", i)]]
            if (!is.null(txt) && trimws(txt) != "") {
              list_items   <- c(list_items, trimws(txt))
              list_checked <- c(list_checked,
                                ifelse(isTRUE(input[[paste0("item_check_", i)]]), "1", "0"))
            }
          }
        }
        
        fecha_cum <- if (input$TSK_TipoNota == "list") {
          if (is.null(input$TSK_FechaFin)) Sys.Date() else as.Date(input$TSK_FechaFin)
        } else Sys.Date()
        
        SubirDatos(
          .tsk_construir_registro(input, usr, note_id, fecha_cum, list_items, list_checked),
          "CRMNALNOTAS"
        )
        notes_data(CargarDatos("CRMNALNOTAS"))
        
        waiter_hide()
        showNotification("Nota/Tarea guardada correctamente", type = "message", duration = 4)
        
        updateTextInput(session, "TSK_Titulo", value = "")
        updateTextAreaInput(session, "TSK_Descripcion", value = "")
        updateSelectizeInput(session, "TSK_Tags", selected = character(0))
        updateRadioButtons(session, "TSK_TipoNota", selected = "text")
        updatePickerInput(session, "TSK_Prioridad", selected = "MEDIA")
        updateSwitchInput(session, "TSK_CheckList", value = FALSE)
        
        for (i in seq_len(list_item_count())) {
          if (i > 1L) removeUI(paste0("#", session$ns(paste0("list_item_", i))))
          else {
            updateTextInput(session, "item_text_1", value = "")
            updateCheckboxInput(session, "item_check_1", value = FALSE)
          }
        }
        list_item_count(1L)
        
      }, error = function(e) {
        waiter_hide()
        showNotification(paste("Error al guardar:", conditionMessage(e)),
                         type = "error", duration = 5)
      })
    })
  })
}

# Modulo de Impresion ----
# Mostrar Notas — UI ----
noteDisplayUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "tsk-filtros-wrap",
      fluidRow(
        # Busqueda de texto
        column(3,
               tags$div(class = "tsk-filtros-label", "Buscar"),
               textInput(ns("search_text"), label = NULL, value = "", width = "100%")
        ),
        # Filtro por etiqueta
        column(3,
               tags$div(class = "tsk-filtros-label", "Etiqueta"),
               ListaDesplegable(ns("filter_tag"), label = NULL, choices = "", multiple = TRUE)
        ),
        # Filtro por prioridad
        column(3,
               tags$div(class = "tsk-filtros-label", "Prioridad"),
               ListaDesplegable(inputId = ns("priority_tag"), label = NULL,
                                choices = .TSK_PRIORIDADES, multiple = TRUE)
        ),
        # Switches de visibilidad con labels descriptivos
        column(3,
               tags$div(class = "tsk-filtros-label", "Incluir en vista"),
               tags$div(class = "tsk-filtros-switches",
                        createSwitch("show_archived", "Notas archivadas", ns = ns),
                        createSwitch("show_completed", "Notas cumplidas",  ns = ns))
        )
      )
    ),
    tags$div(class = "tsk-kanban-outer", uiOutput(ns("kanban"))),
    br(),
    # Exportar
    column(2,
           tags$div(class = "tsk-filtros-acciones",
                    BotonDescarga(ns("btn_descarga"), title = "Exportar notas filtradas",
                                  icon_name = "file-excel", align = "left"))
           )
    )
}


# Mostrar Notas — Server ----
noteDisplay <- function(id, usr, notes_data) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(
      last_update       = NULL,
      accomplish_clicks = list(),
      respond_clicks    = list(),
      archive_clicks    = list(),
      delete_clicks     = list(),
      card_clicks       = list(),
      checklist_clicks  = list()
    )
    
    # Refrescar al montar — cubre notas cargadas antes de navegar a esta vista
    observeEvent(TRUE, once = TRUE, ignoreNULL = TRUE, {
      notes_data(CargarDatos("CRMNALNOTAS"))
    })
    
    # Actualizar etiquetas disponibles cuando cambia notes_data
    observeEvent(rv$last_update, ignoreNULL = FALSE, {
      disponibles <- .tsk_extraer_etiquetas(notes_data())
      actual      <- input$filter_tag
      seleccion   <- if (is.null(actual) || length(actual) == 0) {
        disponibles
      } else {
        validos <- actual[actual %in% disponibles]
        if (length(validos) == 0) disponibles else validos
      }
      updatePickerInput(session, "filter_tag", choices = disponibles, selected = seleccion)
    })
    
    # Notas visibles para el usuario — creadas por él O con Responsable que lo incluye
    # (punto 5: mostrar tareas creadas O asignadas al usuario)
    filtered_notes <- reactive({
      u <- usr()
      notes <- notes_data() %>%
        mutate(Prioridad = str_to_upper(trimws(Prioridad))) %>%
        filter(Usuario == u | grepl(u, coalesce(Responsable, ""), fixed = TRUE))
      
      if (is.null(notes) || nrow(notes) == 0) return(notes)
      
      if (!is.null(input$filter_tag) && length(input$filter_tag) > 0) {
        sin_etiq     <- "Sin etiqueta" %in% input$filter_tag
        tags_activos <- input$filter_tag[input$filter_tag != "Sin etiqueta"]
        cons_tags <- if (length(tags_activos) > 0) {
          notes %>%
            filter(!is.na(Etiquetas), Etiquetas != "") %>%
            separate_longer_delim(Etiquetas, delim = "|") %>%
            filter(str_to_upper(Etiquetas) %in% str_to_upper(tags_activos)) %>%
            pull(cons) %>% unique()
        } else integer(0)
        cons_sin <- if (sin_etiq) {
          notes %>% filter(is.na(Etiquetas) | Etiquetas == "") %>% pull(cons)
        } else integer(0)
        notes <- notes %>% filter(cons %in% unique(c(cons_tags, cons_sin)))
      }
      
      if (!is.null(input$search_text) && trimws(input$search_text) != "") {
        txt   <- tolower(trimws(input$search_text))
        notes <- notes %>%
          filter(grepl(txt, tolower(titulo)) |
                   grepl(txt, tolower(Descripcion)) |
                   grepl(txt, tolower(Actividades)))
      }
      
      if (!is.null(input$priority_tag) && length(input$priority_tag) > 0)
        notes <- notes %>% filter(Prioridad %in% input$priority_tag)
      if (!isTRUE(input$show_archived))  notes <- notes %>% filter(!Archivado)
      if (!isTRUE(input$show_completed)) notes <- notes %>% filter(!Cumplido)
      
      notes
    })
    
    # Render del tablero kanban — siempre 4 columnas (grid CSS de 4 columnas iguales)
    output$kanban <- renderUI({
      notes <- filtered_notes()
      # Siempre mostrar las 4 columnas aunque no haya notas en alguna
      columnas_ui <- lapply(.TSK_PRIORIDADES, function(p) {
        col_info  <- .TSK_COLORES[[p]]
        notas_col <- if (!is.null(notes) && nrow(notes) > 0)
          notes[notes$Prioridad == p, ] else data.frame()
        n         <- nrow(notas_col)
        
        cards_ui <- if (n == 0) {
          div(class = "tsk-empty-col", icon("inbox"), p("Sin notas"))
        } else {
          do.call(tagList, lapply(seq_len(n), function(i) {
            note <- notas_col[i, ]
            
            # Progreso checklist compacto en la card
            prog_ui <- if (!is.na(note$Tipo) && note$Tipo == "list" &&
                           !is.na(note$Actividades) && note$Actividades != "") {
              items   <- unlist(strsplit(note$Actividades, "\\|"))
              checked <- unlist(strsplit(note$Cumplidas,   "\\|"))
              n_t     <- length(items)
              n_d     <- sum(checked == "1", na.rm = TRUE)
              pct     <- if (n_t > 0) round(100 * n_d / n_t) else 0L
              div(class = "tsk-progress-wrap",
                  div(class = "tsk-progress-bar",
                      div(class = "tsk-progress-fill",
                          style = paste0("width:", pct, "%;background:", col_info$header, ";"))),
                  span(class = "tsk-progress-label",
                       FormatearTexto(paste0(n_d, "/", n_t, " — ", pct, "%"), tamano_pct = 0.8))
              )
            }
            
            # Fecha vencimiento con semáforo de color
            fecha_ui <- if (!is.na(note$Tipo) && note$Tipo == "list" &&
                            !is.na(note$FechaCumplimiento)) {
              vencida <- as.Date(note$FechaCumplimiento) < Sys.Date()
              div(class = "tsk-meta-row", icon("calendar-days"),
                  FormatearTexto(format(as.Date(note$FechaCumplimiento), "%d/%m/%Y"),
                                 color = if (vencida) "#c0392b" else "#777",
                                 negrita = isTRUE(vencida), tamano_pct = 0.82))
            }
            
            cumplido  <- isTRUE(note$Cumplido == 1L)
            archivado <- isTRUE(note$Archivado == 1L)
            
            div(
              class = paste("tsk-card",
                            if (cumplido)  "tsk-card--done"     else "",
                            if (archivado) "tsk-card--archived" else ""),
              style = paste0("border-left-color:", col_info$border,
                             ";background:", col_info$bg, ";"),
              div(class = "tsk-card-header",
                  # Titulo en su propia línea — no compite con botones
                  div(class = "tsk-card-title-wrap",
                      actionLink(session$ns(paste0("card_click_", note$cons)),
                                 label = span(class = "tsk-card-title", note$titulo),
                                 style = "text-decoration:none;color:inherit;")
                  ),
                  # Botones debajo del titulo — icono contextual por estado
                  div(class = "tsk-card-actions",
                      span(title = "Comentar",
                           actionBttn(session$ns(paste0("respond_note_",    note$cons)), "",
                                      icon = icon("message"),
                                      style = "bordered", size = "xs", color = "primary")),
                      span(title = if (cumplido) "Marcar pendiente" else "Marcar cumplida",
                           actionBttn(session$ns(paste0("accomplish_note_", note$cons)), "",
                                      icon = icon(if (cumplido) "circle-xmark" else "circle-check"),
                                      style = "bordered", size = "xs",
                                      color = if (cumplido) "warning" else "success")),
                      span(title = if (archivado) "Desarchivar" else "Archivar",
                           actionBttn(session$ns(paste0("archive_note_",    note$cons)), "",
                                      icon = icon(if (archivado) "box-open" else "archive"),
                                      style = "bordered", size = "xs", color = "warning")),
                      span(title = "Eliminar",
                           actionBttn(session$ns(paste0("delete_note_",     note$cons)), "",
                                      icon = icon("trash"),
                                      style = "bordered", size = "xs", color = "danger"))
                  )
              ),
              prog_ui,
              fecha_ui
            )
          }))
        }
        
        div(class = "tsk-kanban-col",
            div(class = "tsk-kanban-col-header",
                style = paste0("background:", col_info$header, ";"),
                span(class = "tsk-col-title", str_to_title(tolower(p))),
                tags$span(class = "tsk-col-badge", n)),
            div(class = "tsk-kanban-col-body", cards_ui))
      })
      
      div(class = "tsk-kanban-wrap", do.call(tagList, columnas_ui))
    })
    
    # Descarga CSV
    output$btn_descarga <- downloadHandler(
      filename = function() paste0("notas_crm_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content  = function(file) {
        notes <- filtered_notes()
        if (is.null(notes) || nrow(notes) == 0) {
          write.csv(data.frame(Sin_datos = "No hay notas con los filtros activos"),
                    file, row.names = FALSE)
          return()
        }
        notes %>%
          mutate(
            FechaCreacion = format(as.POSIXct(FechaHoraCrea), "%d/%m/%Y %H:%M"),
            FechaVence    = if_else(Tipo == "list" & !is.na(FechaCumplimiento),
                                    format(as.Date(FechaCumplimiento), "%d/%m/%Y"), "—"),
            Estado        = case_when(Cumplido  == 1L ~ "Cumplida",
                                      Archivado == 1L ~ "Archivada", TRUE ~ "Pendiente"),
            TipoLabel     = if_else(Tipo == "list", "Tarea", "Nota")
          ) %>%
          select(cons, titulo, TipoLabel, Prioridad, Estado,
                 FechaCreacion, FechaVence, Etiquetas, Descripcion) %>%
          write.csv(file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # Observer: click en tarjeta — abre modal con botones de accion en footer
    observe({
      req(filtered_notes()); notes <- filtered_notes(); req(nrow(notes) > 0)
      rv$card_clicks <- .tsk_limpiar_observers(rv$card_clicks, "card_click_", notes$cons)
      lapply(notes$cons, function(note_id) {
        btn_id <- paste0("card_click_", note_id)
        if (is.null(rv$card_clicks[[btn_id]])) {
          rv$card_clicks[[btn_id]] <- observeEvent(input[[btn_id]], {
            nota <- isolate(filtered_notes()) %>% filter(cons == note_id)
            if (nrow(nota) == 0) return()
            tipo_label <- if (nota$Tipo[[1]] == "list") "Tarea" else "Nota de Reunión"
            showModal(modalDialog(
              title = tags$span(icon("note-sticky"), " ", nota$titulo[[1]],
                                " — ", tipo_label),
              .tsk_modal_body(nota, session),
              footer = .tsk_modal_footer(nota, session),
              size = "l", easyClose = TRUE
            ))
            # Registrar observers de checklist para items de ESTA nota
            if (nota$Tipo[[1]] == "list" &&
                !is.na(nota$Actividades[[1]]) && nota$Actividades[[1]] != "") {
              items_n <- length(unlist(strsplit(nota$Actividades[[1]], "\\|")))
              for (idx in seq_len(items_n)) {
                local({
                  li <- idx
                  chk_id <- paste0("chk_", note_id, "_", li)
                  if (is.null(rv$checklist_clicks[[chk_id]])) {
                    rv$checklist_clicks[[chk_id]] <- observeEvent(
                      input[[chk_id]], ignoreInit = TRUE, ignoreNULL = TRUE, {
                        nota_actual <- CargarDatos("CRMNALNOTAS") %>%
                          filter(cons == note_id)
                        if (nrow(nota_actual) == 0) return()
                        items_v   <- unlist(strsplit(nota_actual$Actividades[[1]], "\\|"))
                        checked_v <- unlist(strsplit(nota_actual$Cumplidas[[1]], "\\|"))
                        if (li > length(checked_v)) return()
                        checked_v[li] <- if (isTRUE(input[[chk_id]])) "1" else "0"
                        nueva_cumplidas <- paste(checked_v, collapse = "|")
                        .tsk_ejecutar_query(
                          sprintf("UPDATE CRMNALNOTAS SET Cumplidas = '%s' WHERE cons = %d",
                                  nueva_cumplidas, note_id),
                          notes_data, rv
                        )
                      }
                    )
                  }
                })
              }
            }
          }, ignoreInit = TRUE, ignoreNULL = TRUE)
        }
      })
    })
    
    # Observer: Archivar/Desarchivar
    observe({
      req(filtered_notes()); notes <- filtered_notes(); req(nrow(notes) > 0)
      .tsk_registrar_observer(rv, "archive_clicks", "archive_note_", notes$cons,
                              session, input, function(note_id) {
                                withProgress(message = "Actualizando nota...", value = 0.1, {
                                  tryCatch({
                                    estado_actual <- isolate(filtered_notes()) %>%
                                      filter(cons == note_id) %>% pull(Archivado)
                                    nuevo_estado  <- as.integer(!estado_actual)
                                    .tsk_ejecutar_query(
                                      sprintf("UPDATE CRMNALNOTAS SET Archivado = %d WHERE cons = %d",
                                              nuevo_estado, note_id),
                                      notes_data, rv,
                                      on_success = function()
                                        toastr_success(
                                          sprintf("Nota %s", if (nuevo_estado) "archivada" else "desarchivada"),
                                          position = "bottom-left"
                                        )
                                    )
                                  }, error = function(e)
                                    showNotification(sprintf("Error: %s", conditionMessage(e)), type = "error"))
                                })
                              })
    })
    
    # Observer: Eliminar
    observe({
      req(filtered_notes()); notes <- filtered_notes(); req(nrow(notes) > 0)
      .tsk_registrar_observer(rv, "delete_clicks", "delete_note_", notes$cons,
                              session, input, function(note_id) {
                                nota <- isolate(filtered_notes()) %>% filter(cons == note_id)
                                if (nota$Usuario != usr()) {
                                  toastr_error("Solo el creador puede eliminar esta nota", position = "bottom-left")
                                  return()
                                }
                                confirmSweetAlert(session = session,
                                                  inputId = paste0("sweet_confirm_", note_id),
                                                  title   = "¿Está seguro?",
                                                  text    = "Esta acción eliminará la nota permanentemente",
                                                  type    = "warning", showCancelButton = TRUE,
                                                  btn_labels = c("Cancelar", "Sí, Eliminar"),
                                                  btn_colors = c("#138d75", "#d33"))
                                observeEvent(input[[paste0("sweet_confirm_", note_id)]], {
                                  req(input[[paste0("sweet_confirm_", note_id)]])
                                  withProgress(message = "Eliminando nota...", value = 0.1, {
                                    tryCatch({
                                      .tsk_ejecutar_query(
                                        sprintf("DELETE FROM CRMNALNOTAS WHERE cons = %d", note_id),
                                        notes_data, rv,
                                        on_success = function()
                                          sendSweetAlert(session, title = "¡Eliminado!",
                                                         text = "Nota eliminada correctamente", type = "success")
                                      )
                                    }, error = function(e)
                                      sendSweetAlert(session, title = "Error",
                                                     text = sprintf("Error: %s", conditionMessage(e)), type = "error"))
                                  })
                                }, ignoreInit = TRUE, ignoreNULL = TRUE)
                              })
    })
    
    # Observer: Cumplida/Pendiente
    observe({
      req(filtered_notes()); notes <- filtered_notes(); req(nrow(notes) > 0)
      .tsk_registrar_observer(rv, "accomplish_clicks", "accomplish_note_", notes$cons,
                              session, input, function(note_id) {
                                withProgress(message = "Actualizando estado...", value = 0.1, {
                                  tryCatch({
                                    nota         <- isolate(filtered_notes()) %>% filter(cons == note_id)
                                    if (nota$Usuario != usr()) {
                                      toastr_error("Solo el creador puede marcar esta tarea", position = "bottom-left")
                                      return()
                                    }
                                    nuevo_estado <- as.integer(!nota$Cumplido)
                                    .tsk_ejecutar_query(
                                      sprintf("UPDATE CRMNALNOTAS SET Cumplido = %d WHERE cons = %d",
                                              nuevo_estado, note_id),
                                      notes_data, rv,
                                      on_success = function() {
                                        if (nuevo_estado == 1L) {
                                          .tsk_enviar_email(
                                            to      = paste0(nota$Usuario, "@racafe.com"),
                                            subject = sprintf("CRM - Tarea '%s' cumplida", nota$titulo),
                                            body_md = sprintf("Hola,\n\nTarea cumplida por %s.\n\nTítulo: %s\n",
                                                              usr(), nota$titulo)
                                          )
                                        }
                                        toastr_success(
                                          sprintf("Tarea marcada como %s",
                                                  if (nuevo_estado) "cumplida" else "pendiente"),
                                          position = "bottom-left"
                                        )
                                      }
                                    )
                                  }, error = function(e)
                                    toastr_error(sprintf("Error: %s", conditionMessage(e)), position = "bottom-left"))
                                })
                              })
    })
    
    # Observer: Comentar
    observe({
      req(filtered_notes()); notes <- filtered_notes(); req(nrow(notes) > 0)
      .tsk_registrar_observer(rv, "respond_clicks", "respond_note_", notes$cons,
                              session, input, function(note_id) {
                                nota <- isolate(filtered_notes()) %>% filter(cons == note_id)
                                showModal(modalDialog(
                                  title = paste("Comentar:", nota$titulo),
                                  textAreaInput(session$ns(paste0("comment_text_", note_id)),
                                                "Escribe tu comentario:", "", width = "100%", height = "150px"),
                                  footer = tagList(
                                    modalButton("Cancelar"),
                                    actionButton(session$ns(paste0("submit_comment_", note_id)),
                                                 "Enviar comentario", class = "btn-primary")
                                  ),
                                  size = "m", easyClose = TRUE
                                ))
                                observeEvent(input[[paste0("submit_comment_", note_id)]], {
                                  req(input[[paste0("comment_text_", note_id)]])
                                  txt_com <- input[[paste0("comment_text_", note_id)]]
                                  if (nchar(txt_com) < 3) {
                                    toastr_error("El comentario debe tener al menos 3 caracteres", position = "bottom-left")
                                    return()
                                  }
                                  withProgress(message = "Guardando comentario...", value = 0.1, {
                                    tryCatch({
                                      previo    <- if (!is.null(nota$Respuesta) && !is.na(nota$Respuesta) &&
                                                       nota$Respuesta != "") nota$Respuesta else ""
                                      nuevo_com <- paste0(
                                        if (previo != "") paste0(previo, "\n---\n") else "",
                                        format(Sys.time(), "%d/%m/%Y %H:%M"), " - ", usr(), ":\n", txt_com
                                      )
                                      .tsk_ejecutar_query(
                                        sprintf("UPDATE CRMNALNOTAS SET Respuesta = '%s' WHERE cons = %d",
                                                gsub("'", "''", nuevo_com), note_id),
                                        notes_data, rv,
                                        on_success = function() {
                                          removeModal()
                                          toastr_success("Comentario añadido correctamente", position = "bottom-left")
                                        }
                                      )
                                    }, error = function(e)
                                      toastr_error(sprintf("Error: %s", conditionMessage(e)), position = "bottom-left"))
                                  })
                                }, ignoreInit = TRUE, ignoreNULL = TRUE)
                              })
    })
    
  })
}
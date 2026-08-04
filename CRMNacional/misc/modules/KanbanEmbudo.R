# Funciones ----

# Este archivo depende de TablaContactos.R, TablaProspectos.R,
# TablaLeads.R y TablaDescartados.R: derivar_etapa_actual,
# derivar_fecha_entrada_etapa, EditarContacto/UI, GestionContacto/UI,
# FormularioAscenderLead/UI, FormularioConvertirProspecto/UI,
# FormularioReclasificarLead/UI, FormularioVincularNit/UI,
# FormularioDescartarContacto/UI, FormularioDescartarLead/UI,
# FormularioDescartarProspecto/UI, FormularioReactivar/UI,
# detectar_conversion_leads

ETAPAS_EMBUDO <- c("CONTACTO", "LEAD", "PROSPECTO", "CLIENTE", "DESCARTADO")

COLOR_ETAPA_EMBUDO <- c(
  "CONTACTO"   = "#5a6474",
  "LEAD"       = "#C8862A",
  "PROSPECTO"  = "#6f42c1",
  "CLIENTE"    = "#1e8449",
  "DESCARTADO" = "#95a5a6"
)

.semaforo_gestion <- function(dias) {
  dplyr::case_when(dias >= 30 ~ "critical", dias >= 15 ~ "warning", TRUE ~ "ok")
}

CSS_KANBAN_EMBUDO <- "
.kpb-filtros { background:#fff; border-radius:8px; padding:10px 16px; margin-bottom:14px; box-shadow:0 1px 4px rgba(0,0,0,.07); }
.kpb-filtros-inner { display:flex; flex-wrap:wrap; align-items:center; gap:12px; }
.kpb-filtro-item { display:flex; align-items:center; }
.kpb-filtro-item .form-group { margin-bottom:0 !important; }
.kpb-filtro-item select { font-size:.82rem !important; }
.kpb-kpi-bar { display:flex; gap:12px; margin-bottom:14px; flex-wrap:wrap; }
.kpb-kpi { flex:1; min-width:150px; background:#fff; border-radius:8px; padding:10px 16px; box-shadow:0 1px 4px rgba(0,0,0,.07); border-top:3px solid #1A3C5E; }
.kpb-kpi-valor { font-size:1.45rem; font-weight:700; color:#1A3C5E; line-height:1.2; }
.kpb-kpi-label { font-size:.7rem; color:#8e9aaf; margin-top:2px; text-transform:uppercase; letter-spacing:.04em; }
.kpb-kpi-alerta { border-top-color:#e74c3c; } .kpb-kpi-alerta .kpb-kpi-valor { color:#e74c3c; }
.kpb-kpi-success { border-top-color:#1e8449; } .kpb-kpi-success .kpb-kpi-valor { color:#1e8449; }
.kpb-board { display:flex; gap:14px; width:100%; padding-bottom:18px; align-items:flex-start; min-height:400px; }
.kpb-board-vacio { justify-content:center; align-items:center; flex-direction:column; color:#adb5bd; font-size:.88rem; padding:50px 20px; }
.kpb-col { flex:1 1 0; min-width:0; background:#f4f6f9; border-radius:8px; display:flex; flex-direction:column; max-height:720px; min-height:200px; }
.kpb-col-header { border-radius:8px 8px 0 0; padding:10px 12px; }
.kpb-col-header-inner { display:flex; justify-content:space-between; align-items:center; }
.kpb-col-titulo { color:#fff; font-weight:600; font-size:.78rem; letter-spacing:.06em; text-transform:uppercase; }
.kpb-col-badges { display:flex; align-items:center; gap:6px; }
.kpb-col-count { background:rgba(255,255,255,.25); color:#fff; border-radius:10px; padding:1px 8px; font-size:.74rem; font-weight:600; }
.kpb-col-alert-badge { background:rgba(231,76,60,.85); color:#fff; border-radius:10px; padding:1px 7px; font-size:.7rem; }
.kpb-col-add { background:rgba(255,255,255,.25); border:none; color:#fff; border-radius:6px; padding:2px 8px; font-size:.75rem; cursor:pointer; }
.kpb-col-add:hover { background:rgba(255,255,255,.45); }
.kpb-col-body { flex:1; overflow-y:auto; padding:10px; display:flex; flex-direction:column; gap:8px; }
.kpb-col-vacio { display:flex; flex-direction:column; align-items:center; gap:6px; color:#adb5bd; font-size:.76rem; padding:22px 10px; }
.kpb-card { background:#fff; border-radius:6px; padding:8px; box-shadow:0 1px 3px rgba(0,0,0,.08); border-left:4px solid #dee2e6; transition:box-shadow .18s ease, transform .15s ease; display:flex; gap:8px; }
.kpb-card:hover { box-shadow:0 4px 14px rgba(0,0,0,.13); transform:translateY(-2px); }
.kpb-card.gestion-warning { border-left-color:#d35400; }
.kpb-card.gestion-critical { background:#fff8f8; border-left-color:#e74c3c !important; }
.kpb-card-content { flex:1; min-width:0; }
.kpb-card-header { margin-bottom:5px; }
.kpb-card-title { display:flex; flex-direction:column; gap:1px; margin-bottom:3px; }
.kpb-empresa { font-weight:600; font-size:.79rem; color:#2c3e50; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; max-width:190px; display:block; }
.kpb-nit { font-size:.68rem; color:#8e9aaf; }
.kpb-card-meta { font-size:.72rem; color:#8e9aaf; }
.kpb-card-body { border-top:1px solid #f0f2f5; padding-top:5px; margin-top:3px; display:flex; flex-direction:column; gap:3px; }
.kpb-metric { font-size:.73rem; color:#5a6474; display:flex; align-items:center; gap:5px; }
.kpb-alianza-badge { display:inline-block; margin:1px 2px 1px 0; padding:1px 7px; border-radius:10px; background:#F0E6D2; color:#C8862A; font-size:.66rem; }
.kpb-card-sla { margin-top:5px; }
.kpb-sla { display:inline-flex; align-items:center; gap:4px; padding:2px 8px; border-radius:10px; font-size:.68rem; font-weight:600; }
.kpb-sla-ok { background:#eafaf1; color:#1e8449; }
.kpb-sla-warning { background:#fef5e7; color:#d35400; }
.kpb-sla-critical { background:#fdedec; color:#c0392b; }
.kpb-card-actions { display:flex; flex-direction:column; gap:4px; }
.kpb-btn-accion { width:22px; height:22px; border-radius:50%; border:1px solid #eee; background:#fff; cursor:pointer; display:flex; align-items:center; justify-content:center; font-size:.62rem; box-shadow:0 1px 2px rgba(0,0,0,.08); transition:transform .12s ease, box-shadow .12s ease; padding:0; }
.kpb-btn-accion:hover { transform:scale(1.12); box-shadow:0 2px 5px rgba(0,0,0,.18); }
"

JS_KANBAN_EMBUDO <- "
$(document).on('click', '[data-kanban-action]', function(e) {
  e.stopPropagation();
  var $btn = $(this);
  var $board = $btn.closest('[data-kanban-ns]');
  if ($board.length === 0) return;
  var ns_prefix = $board.data('kanban-ns');
  Shiny.setInputValue(
    ns_prefix + 'kanban_accion',
    { cod_contacto: $btn.data('cod-contacto'), accion: $btn.data('kanban-action'), ts: Date.now() },
    { priority: 'event' }
  );
});
"

.html_badge_gestion <- function(dias) {
  estado <- .semaforo_gestion(dias)
  icono <- switch(estado, critical = "fas fa-exclamation-triangle", warning = "fas fa-clock", "fas fa-check-circle")
  as.character(tags$span(
    class = paste0("kpb-sla kpb-sla-", estado), title = paste0(dias, " días sin gestión"),
    tags$i(class = icono), paste0(" ", dias, "d")
  ))
}

.kpi_embudo <- function(valor, label, clase_extra = "") {
  div(class = trimws(paste0("kpb-kpi ", clase_extra)),
      div(class = "kpb-kpi-valor", as.character(valor)),
      div(class = "kpb-kpi-label", label))
}

.COLOR_ACCION_KANBAN <- c(
  editar = "#1D4ED8", gestionar = "#6f42c1", ascender = "#198754", prospecto = "#C8862A",
  vincular = "#0F6E56", reclasificar = "#198754", descartar = "#C11007", reactivar = "#198754"
)

.btn_kanban <- function(cod_contacto, accion, label, icono) {
  tags$button(
    class = "kpb-btn-accion", title = label,
    `data-cod-contacto` = cod_contacto, `data-kanban-action` = accion,
    tags$i(class = paste0("fas fa-", icono), style = paste0("color:", .COLOR_ACCION_KANBAN[[accion]], "; font-size:.65rem;"))
  )
}

# Trae el pipeline completo con Etapa derivada del historial (fuente única
# de verdad) y datos propios de cada etapa (Lead/Prospecto/Descartado)
cargar_pipeline_embudo <- function() {
  contactos <- derivar_etapa_actual() %>% derivar_fecha_entrada_etapa()
  
  lead_data <- CargarDatos("CONTACTOLEAD") %>% select(CodContacto, Asesor, Segmento, LinNegocio)
  alianzas  <- tryCatch(CargarDatos("CRMNALPROSPECTOALIANZA") %>% count(CodContacto, name = "NumAlianzas"),
                        error = function(e) data.frame(CodContacto = character(), NumAlianzas = integer()))
  
  if (!"EtapaPreDescarte" %in% names(contactos)) contactos$EtapaPreDescarte <- NA_character_
  
  contactos %>%
    left_join(lead_data, by = "CodContacto") %>%
    left_join(alianzas, by = "CodContacto") %>%
    mutate(
      EtapaPreDescarte = ifelse(is.na(EtapaPreDescarte), "CONTACTO", EtapaPreDescarte),
      NumAlianzas = coalesce(NumAlianzas, 0),
      DiasEnEtapa = as.numeric(difftime(Sys.time(), FechaEntradaEtapa, units = "days")),
      EstadoGestion = .semaforo_gestion(DiasEnEtapa)
    )
}

.render_tarjeta_embudo <- function(fila) {
  clase_card <- paste0("kpb-card",
                       if (fila$Etapa %in% c("CONTACTO", "LEAD") && fila$EstadoGestion == "critical") " gestion-critical"
                       else if (fila$Etapa %in% c("CONTACTO", "LEAD") && fila$EstadoGestion == "warning") " gestion-warning" else "")
  
  identificador <- fila$PerRazSoc %||% "SIN RAZÓN SOCIAL"
  nit <- fila$PerCod %||% "SIN NIT"
  
  acciones <- switch(fila$Etapa,
                     "CONTACTO" = tagList(
                       .btn_kanban(fila$CodContacto, "editar", "Editar", "pen"),
                       .btn_kanban(fila$CodContacto, "gestionar", "Gestionar", "comment"),
                       .btn_kanban(fila$CodContacto, "ascender", "Ascender a Lead", "arrow-up"),
                       .btn_kanban(fila$CodContacto, "prospecto", "Marcar como Prospecto", "handshake"),
                       .btn_kanban(fila$CodContacto, "descartar", "Descartar", "ban")
                     ),
                     "LEAD" = tagList(
                       .btn_kanban(fila$CodContacto, "editar", "Editar", "pen"),
                       .btn_kanban(fila$CodContacto, "gestionar", "Gestionar", "comment"),
                       .btn_kanban(fila$CodContacto, "vincular", "Vincular NIT", "link"),
                       .btn_kanban(fila$CodContacto, "descartar", "Descartar", "ban")
                     ),
                     "PROSPECTO" = tagList(
                       .btn_kanban(fila$CodContacto, "editar", "Editar", "pen"),
                       .btn_kanban(fila$CodContacto, "gestionar", "Gestionar", "comment"),
                       .btn_kanban(fila$CodContacto, "reclasificar", "Reclasificar a Lead", "arrow-up"),
                       .btn_kanban(fila$CodContacto, "descartar", "Descartar", "ban")
                     ),
                     "CLIENTE" = tagList(
                       .btn_kanban(fila$CodContacto, "gestionar", "Gestionar", "comment")
                     ),
                     "DESCARTADO" = tagList(
                       .btn_kanban(fila$CodContacto, "reactivar", "Reactivar", "rotate-left")
                     )
  )
  
  # Cuerpo de la tarjeta, distinto según la información relevante de cada etapa
  cuerpo <- switch(fila$Etapa,
                   "CONTACTO" = tagList(
                     if (!is.na(fila$Origen %||% NA)) div(class = "kpb-metric", tags$i(class = "fas fa-bullseye fa-xs"), " ", fila$Origen)
                   ),
                   "LEAD" = tagList(
                     if (!is.na(fila$LinNegocio %||% NA)) div(class = "kpb-metric", tags$i(class = "fas fa-briefcase fa-xs"), " ", fila$LinNegocio),
                     if (!is.na(fila$Segmento %||% NA)) div(class = "kpb-metric", tags$i(class = "fas fa-layer-group fa-xs"), " ", fila$Segmento),
                     if (!is.na(fila$Asesor %||% NA)) div(class = "kpb-metric", tags$i(class = "fas fa-user fa-xs"), " ", fila$Asesor)
                   ),
                   "PROSPECTO" = {
                     alianzas <- listar_alianzas_prospecto(fila$CodContacto)
                     if (nrow(alianzas) == 0) {
                       div(class = "kpb-metric", tags$i(class = "fas fa-handshake fa-xs"), " Sin alianzas")
                     } else {
                       div(lapply(alianzas$ClienteAliado, function(nombre) tags$span(class = "kpb-alianza-badge", nombre)))
                     }
                   },
                   "CLIENTE" = tagList(
                     if (!is.na(fila$LinNegocio %||% NA)) div(class = "kpb-metric", tags$i(class = "fas fa-briefcase fa-xs"), " ", fila$LinNegocio),
                     if (!is.na(fila$Segmento %||% NA)) div(class = "kpb-metric", tags$i(class = "fas fa-layer-group fa-xs"), " ", fila$Segmento),
                     if (!is.na(fila$Asesor %||% NA)) div(class = "kpb-metric", tags$i(class = "fas fa-user fa-xs"), " ", fila$Asesor)
                   ),
                   "DESCARTADO" = div(class = "kpb-metric", tags$i(class = "fas fa-rotate-left fa-xs"), " Origen: ", fila$EtapaPreDescarte)
  )
  
  div(
    class = clase_card, `data-cod-contacto` = fila$CodContacto,
    div(class = "kpb-card-actions", acciones),
    div(class = "kpb-card-content",
        div(class = "kpb-card-header",
            div(class = "kpb-card-title",
                tags$span(class = "kpb-empresa", title = identificador, identificador),
                tags$span(class = "kpb-nit", nit))),
        div(class = "kpb-card-body", cuerpo),
        if (fila$Etapa %in% c("CONTACTO", "LEAD")) div(class = "kpb-card-sla", HTML(.html_badge_gestion(round(fila$DiasEnEtapa, 0))))
    )
  )
}

# Modulos Auxiliares ----
## (ninguno propio — reutiliza los módulos de los 4 archivos anteriores)

# Modulo Principal ----

## KanbanEmbudo ----

KanbanEmbudoUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(CSS_KANBAN_EMBUDO)), tags$script(HTML(JS_KANBAN_EMBUDO))),
    div(class = "kpb-filtros",
        div(class = "kpb-filtros-inner",
            div(class = "kpb-filtro-item", selectInput(ns("filtro_origen"), label = NULL,
                                                       choices = c("Todos los canales" = "Todos"), selected = "Todos", width = "180px")),
            div(class = "kpb-filtro-item", selectInput(ns("filtro_asesor"), label = NULL,
                                                       choices = c("Todos los asesores" = "Todos"), selected = "Todos", width = "180px")),
            div(class = "kpb-filtro-item", shinyWidgets::materialSwitch(ns("filtro_alertas"), label = "Solo alertas de gestión",
                                                                        value = FALSE, status = "danger")))),
    div(class = "kpb-kpi-bar",
        uiOutput(ns("kpi_total")), uiOutput(ns("kpi_alertas")), uiOutput(ns("kpi_prospectos")),
        uiOutput(ns("kpi_clientes")), uiOutput(ns("kpi_conversion"))),
    uiOutput(ns("kanban_board"))
  )
}

KanbanEmbudo <- function(id, usr) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    
    observeEvent(refresh_trigger(), { detectar_conversion_leads(usr()) }, ignoreNULL = FALSE)
    
    pipeline_raw <- reactive({
      refresh_trigger()
      dat <- cargar_pipeline_embudo()
      updateSelectInput(session, "filtro_origen",
                        choices = c("Todos los canales" = "Todos", stats::setNames(sort(unique(na.omit(dat$Origen))), sort(unique(na.omit(dat$Origen))))))
      updateSelectInput(session, "filtro_asesor",
                        choices = c("Todos los asesores" = "Todos", stats::setNames(sort(unique(na.omit(dat$Asesor))), sort(unique(na.omit(dat$Asesor))))))
      dat
    })
    
    pipeline_filtrado <- reactive({
      dat <- pipeline_raw()
      if (!is.null(input$filtro_origen) && input$filtro_origen != "Todos") dat <- dat %>% filter(Origen == input$filtro_origen)
      if (!is.null(input$filtro_asesor) && input$filtro_asesor != "Todos") dat <- dat %>% filter(Asesor == input$filtro_asesor)
      if (isTRUE(input$filtro_alertas)) dat <- dat %>% filter(Etapa %in% c("CONTACTO", "LEAD"), EstadoGestion == "critical")
      dat
    })
    
    output$kpi_total <- renderUI({
      n <- pipeline_filtrado() %>% filter(Etapa %in% c("CONTACTO", "LEAD")) %>% nrow()
      .kpi_embudo(n, "Contactos y Leads activos")
    })
    
    output$kpi_alertas <- renderUI({
      n <- pipeline_filtrado() %>% filter(Etapa %in% c("CONTACTO", "LEAD"), EstadoGestion == "critical") %>% nrow()
      .kpi_embudo(n, "Alertas de gestión (+30d)", clase_extra = if (n > 0) "kpb-kpi-alerta" else "")
    })
    
    output$kpi_prospectos <- renderUI({
      n <- pipeline_filtrado() %>% filter(Etapa == "PROSPECTO") %>% nrow()
      .kpi_embudo(n, "Prospectos activos")
    })
    
    output$kpi_clientes <- renderUI({
      n <- pipeline_filtrado() %>% filter(Etapa == "CLIENTE") %>% nrow()
      .kpi_embudo(n, "Clientes generados", clase_extra = "kpb-kpi-success")
    })
    
    output$kpi_conversion <- renderUI({
      total_leads_historico <- tryCatch(nrow(CargarDatos("CONTACTOLEAD")), error = function(e) 0)
      total_clientes <- pipeline_raw() %>% filter(Etapa == "CLIENTE") %>% nrow()
      tasa <- if (total_leads_historico > 0) round(total_clientes / total_leads_historico * 100) else 0
      .kpi_embudo(paste0(tasa, "%"), "Conversión Lead → Cliente", clase_extra = if (tasa >= 20) "kpb-kpi-success" else "")
    })
    
    output$kanban_board <- renderUI({
      dat <- pipeline_filtrado()
      if (nrow(dat) == 0) {
        return(div(class = "kpb-board kpb-board-vacio", tags$i(class = "fas fa-inbox fa-2x"),
                   tags$p("Sin registros con los filtros activos.")))
      }
      
      construir_columna <- function(etapa_col, boton_extra = NULL) {
        filas <- dat %>% filter(Etapa == etapa_col)
        n_col <- nrow(filas)
        n_alertas <- if (etapa_col %in% c("CONTACTO", "LEAD")) sum(filas$EstadoGestion == "critical") else 0
        
        cuerpo <- if (n_col == 0) {
          div(class = "kpb-col-vacio", tags$i(class = "fas fa-inbox"), tags$span("Sin registros"))
        } else {
          do.call(tagList, lapply(seq_len(n_col), function(i) .render_tarjeta_embudo(filas[i, ])))
        }
        
        div(class = "kpb-col",
            div(class = "kpb-col-header", style = paste0("background:", COLOR_ETAPA_EMBUDO[[etapa_col]], ";"),
                div(class = "kpb-col-header-inner",
                    tags$span(class = "kpb-col-titulo", etapa_col),
                    div(class = "kpb-col-badges",
                        tags$span(class = "kpb-col-count", as.character(n_col)),
                        if (n_alertas > 0) tags$span(class = "kpb-col-alert-badge", title = paste0(n_alertas, " alerta(s)"),
                                                     tags$i(class = "fas fa-exclamation-triangle")),
                        boton_extra))),
            div(class = "kpb-col-body", cuerpo))
      }
      
      boton_nuevo_contacto <- actionButton(ns("btn_nuevo_contacto"), label = tagList(icon("plus"), "Añadir"), class = "kpb-col-add")
      
      div(id = ns("kpb_board"), `data-kanban-ns` = ns(""), class = "kpb-board",
          construir_columna("CONTACTO", boton_extra = boton_nuevo_contacto),
          construir_columna("LEAD"),
          construir_columna("PROSPECTO"),
          construir_columna("CLIENTE"),
          construir_columna("DESCARTADO"))
    })
    
    editar_cod_rv       <- reactiveVal(NULL)
    gestion_cod_rv      <- reactiveVal(NULL)
    ascender_cod_rv     <- reactiveVal(NULL)
    prospecto_cod_rv    <- reactiveVal(NULL)
    reclasificar_cod_rv <- reactiveVal(NULL)
    vincular_cod_rv     <- reactiveVal(NULL)
    descartar_ct_rv     <- reactiveVal(NULL)
    descartar_ld_rv     <- reactiveVal(NULL)
    descartar_pr_rv     <- reactiveVal(NULL)
    reactivar_cod_rv    <- reactiveVal(NULL)
    
    EditarContacto(id = "mod_editar", usr = usr, cod_contacto = reactive(editar_cod_rv()))
    GestionContacto(id = "mod_gestion", usr = usr, cod_contacto = reactive(gestion_cod_rv()))
    ascender_mod      <- FormularioAscenderLead(id = "mod_ascender", usr = usr, cod_contacto = reactive(ascender_cod_rv()))
    prospecto_mod     <- FormularioConvertirProspecto(id = "mod_prospecto", usr = usr, cod_contacto = reactive(prospecto_cod_rv()))
    reclasificar_mod  <- FormularioReclasificarLead(id = "mod_reclasificar", usr = usr, cod_contacto = reactive(reclasificar_cod_rv()))
    vincular_mod      <- FormularioVincularNit(id = "mod_vincular", usr = usr, cod_contacto = reactive(vincular_cod_rv()))
    descartar_ct_mod  <- FormularioDescartarContacto(id = "mod_descartar_ct", usr = usr, cod_contacto = reactive(descartar_ct_rv()))
    descartar_ld_mod  <- FormularioDescartarLead(id = "mod_descartar_ld", usr = usr, cod_contacto = reactive(descartar_ld_rv()))
    descartar_pr_mod  <- FormularioDescartarProspecto(id = "mod_descartar_pr", usr = usr, cod_contacto = reactive(descartar_pr_rv()))
    reactivar_mod     <- FormularioReactivar(id = "mod_reactivar", usr = usr, cod_contacto = reactive(reactivar_cod_rv()))
    
    contacto_mod <- FormularioContacto(id = "mod_nuevo_contacto", usr = usr, cod_contacto = reactive(""))
    
    observeEvent(input$btn_nuevo_contacto, {
      showModal(modalDialog(title = "Nuevo Contacto", size = "l", easyClose = TRUE, footer = modalButton("Cerrar"),
                            FormularioContactoUI(ns("mod_nuevo_contacto"))))
    })
    
    observeEvent(contacto_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    
    observeEvent(input$kanban_accion, {
      acc <- input$kanban_accion
      req(acc$cod_contacto, acc$accion)
      cod <- acc$cod_contacto
      fila <- pipeline_raw() %>% filter(CodContacto == cod)
      req(nrow(fila) > 0)
      etapa <- fila$Etapa[[1]]
      
      if (acc$accion == "editar") {
        editar_cod_rv(cod)
        showModal(modalDialog(title = "Editar", size = "xl", easyClose = TRUE, footer = modalButton("Cerrar"),
                              EditarContactoUI(ns("mod_editar"))))
      } else if (acc$accion == "gestionar") {
        gestion_cod_rv(cod)
        showModal(modalDialog(title = "Gestión Comercial", size = "l", easyClose = TRUE, footer = modalButton("Cerrar"),
                              GestionContactoUI(ns("mod_gestion"))))
      } else if (acc$accion == "ascender") {
        ascender_cod_rv(cod)
        showModal(modalDialog(title = "Ascender a Lead", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioAscenderLeadUI(ns("mod_ascender"))))
      } else if (acc$accion == "prospecto") {
        prospecto_cod_rv(cod)
        showModal(modalDialog(title = "Marcar como Prospecto", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioConvertirProspectoUI(ns("mod_prospecto"))))
      } else if (acc$accion == "reclasificar") {
        reclasificar_cod_rv(cod)
        showModal(modalDialog(title = "Reclasificar a Lead", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioReclasificarLeadUI(ns("mod_reclasificar"))))
      } else if (acc$accion == "vincular") {
        vincular_cod_rv(cod)
        showModal(modalDialog(title = "Vincular NIT", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioVincularNitUI(ns("mod_vincular"))))
      } else if (acc$accion == "descartar" && etapa == "CONTACTO") {
        descartar_ct_rv(cod)
        showModal(modalDialog(title = "Descartar Contacto", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioDescartarContactoUI(ns("mod_descartar_ct"))))
      } else if (acc$accion == "descartar" && etapa == "LEAD") {
        descartar_ld_rv(cod)
        showModal(modalDialog(title = "Descartar Lead", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioDescartarLeadUI(ns("mod_descartar_ld"))))
      } else if (acc$accion == "descartar" && etapa == "PROSPECTO") {
        descartar_pr_rv(cod)
        showModal(modalDialog(title = "Descartar Prospecto", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioDescartarProspectoUI(ns("mod_descartar_pr"))))
      } else if (acc$accion == "reactivar") {
        reactivar_cod_rv(cod)
        showModal(modalDialog(title = "Reactivar Registro", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioReactivarUI(ns("mod_reactivar"))))
      }
    })
    
    observeEvent(ascender_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(prospecto_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(reclasificar_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(vincular_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(descartar_ct_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(descartar_ld_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(descartar_pr_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(reactivar_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
  })
}

# App de prueba ----
# Requiere haber cargado antes TablaContactos.R, TablaProspectos.R,
# TablaLeads.R y TablaDescartados.R

ui <- bs4DashPage(
  title = "Prueba Kanban Embudo",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(), controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    KanbanEmbudoUI("EmbudoKanban")
  )
)

server <- function(input, output, session) {
  KanbanEmbudo("EmbudoKanban", usr = reactive("CMEDINA"))
}

shinyApp(ui, server)
# Funciones ----

# Este archivo depende de TablaContactos.R: reactivar_contacto,
# .coldef_accion, .RAZONES_DESCARTE_CONTACTO

# Trae, por CodContacto, el motivo de la transición más reciente hacia
# DESCARTADO — el Motivo vive en CRMNALHISTORIALETAPA, no en CRMNALCONTACTO
obtener_ultimo_motivo_descarte <- function() {
  tryCatch({
    CargarDatos("CRMNALHISTORIALETAPA") %>%
      filter(EtapaNueva == "DESCARTADO") %>%
      mutate(FechaHora = as_datetime(FechaHora)) %>%
      group_by(CodContacto) %>% filter(FechaHora == max(FechaHora)) %>% slice(1) %>% ungroup() %>%
      select(CodContacto, Motivo)
  }, error = function(e) data.frame(CodContacto = character(), Motivo = character()))
}

# Agrupa un motivo de descarte bajo su categoría fija (de cualquiera de los
# 3 catálogos posibles), o "OTRAS" si es texto libre
.categoria_motivo_descarte <- function(motivo) {
  catalogos <- unique(c(.RAZONES_DESCARTE_CONTACTO, .RAZONES_DESCARTE_LEAD, .RAZONES_DESCARTE_PROSPECTO))
  ifelse(motivo %in% catalogos, motivo, "OTRAS")
}

# Modulos Auxiliares ----

## FormularioReactivar ----

# UI del formulario de reactivación (aplica a Contacto, Lead o Prospecto descartado)
FormularioReactivarUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    tags$p(uiOutput(ns("DestinoTexto"))),
    div(style = "text-align: right; margin-top: 10px;",
        actionBttn(ns("REA_Confirmar"), label = "Reactivar",
                   style = "unite", color = "success", size = "xs", icon = icon("rotate-left")))
  )
}

# Server del formulario de reactivación — devuelve al registro a su
# EtapaPreDescarte, cualquiera que sea (CONTACTO, LEAD o PROSPECTO)
FormularioReactivar <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ret <- reactiveVal(0)
    
    data_contacto <- reactive({
      req(cod_contacto())
      CargarDatos("CRMNALCONTACTO") %>% filter(CodContacto == cod_contacto())
    })
    
    output$Titulo <- renderUI({
      req(nrow(data_contacto()) > 0)
      identificador <- data_contacto()$PerRazSoc %||% data_contacto()$PerCod
      h4(paste0(data_contacto()$CodContacto, " - ", identificador))
    })
    
    # Muestra a qué etapa volverá al reactivar (Contacto, Lead o Prospecto)
    output$DestinoTexto <- renderUI({
      req(nrow(data_contacto()) > 0)
      destino <- if ("EtapaPreDescarte" %in% names(data_contacto())) {
        data_contacto()$EtapaPreDescarte[[1]] %||% "CONTACTO"
      } else "CONTACTO"
      paste0("Este registro volverá a la etapa: ", destino)
    })
    
    observeEvent(input$REA_Confirmar, {
      tryCatch({
        reactivar_contacto(cod_contacto(), usr())
        showNotification("Registro reactivado exitosamente", duration = 4, type = "message")
        ret(ret() + 1)
      }, error = function(e) showNotification(paste("Error al reactivar:", conditionMessage(e)), duration = 6, type = "error"))
    })
    
    list(n = reactive(ret()))
  })
}

# Modulo Principal ----

## TablaDescartados ----

TablaDescartadosUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(6, box(title = "Descartados por Etapa de Origen", width = 12, collapsible = FALSE, uiOutput(ns("kpi_etapa")))),
      column(6, box(title = "Descartados por Motivo", width = 12, collapsible = FALSE, uiOutput(ns("kpi_motivo"))))
    ),
    br(),
    TablaReactableUI(ns("tabla_descartados"), titulo = "Descartados",
                     footer = "Reactivar devuelve el registro a su etapa de origen (Contacto, Lead o Prospecto).", footer_tipo = "info")
  )
}

TablaDescartados <- function(id, usr) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    
    descartados_raw <- reactive({
      refresh_trigger()
      dat <- CargarDatos("CRMNALCONTACTO") %>% filter(Estado == "DESCARTADO")
      
      if (!"EtapaPreDescarte" %in% names(dat)) dat$EtapaPreDescarte <- NA_character_
      
      dat %>%
        mutate(FechaHoraModi = as_datetime(FechaHoraModi),
               EtapaPreDescarte = ifelse(is.na(EtapaPreDescarte), "CONTACTO", EtapaPreDescarte)) %>%
        left_join(obtener_ultimo_motivo_descarte(), by = "CodContacto") %>%
        mutate(CategoriaMotivo = .categoria_motivo_descarte(Motivo))
    })
    
    output$kpi_etapa <- renderUI({
      dat <- descartados_raw() %>% count(EtapaPreDescarte, sort = TRUE)
      tags$div(style = "display:flex; gap:10px; flex-wrap:wrap; justify-content:space-around;",
               lapply(seq_len(nrow(dat)), function(i) .kpi_card(dat$EtapaPreDescarte[i], dat$n[i])))
    })
    
    output$kpi_motivo <- renderUI({
      dat <- descartados_raw() %>%
        mutate(CategoriaMotivo = ifelse(is.na(CategoriaMotivo), "SIN MOTIVO", CategoriaMotivo)) %>%
        count(CategoriaMotivo, sort = TRUE)
      tags$div(style = "display:flex; gap:10px; flex-wrap:wrap; justify-content:space-around;",
               lapply(seq_len(nrow(dat)), function(i) .kpi_card(dat$CategoriaMotivo[i], dat$n[i], color = "#C11007")))
    })
    
    data_tabla <- reactive({
      descartados_raw() %>%
        mutate(Reactivar = CodContacto) %>%
        arrange(desc(FechaHoraModi)) %>%
        select(Reactivar, CodContacto, PerRazSoc, PerCod, EtapaPreDescarte, Motivo, FechaHoraModi)
    })
    
    mod_tabla <- TablaReactable(
      id = "tabla_descartados", data = data_tabla, columnas = NULL,
      col_specs = list(
        Reactivar        = .coldef_accion("Reactivar", "rotate-left", "#198754"),
        CodContacto       = reactable::colDef(name = "Código", minWidth = 110),
        PerRazSoc         = reactable::colDef(name = "Razón Social", minWidth = 180),
        PerCod            = reactable::colDef(name = "NIT", minWidth = 100),
        EtapaPreDescarte  = reactable::colDef(name = "Etapa de Origen", minWidth = 120),
        Motivo            = reactable::colDef(name = "Motivo", minWidth = 160),
        FechaHoraModi     = reactable::colDef(name = "Fecha de Descarte", minWidth = 140, format = reactable::colFormat(datetime = TRUE))
      ),
      modo_seleccion = "celda", id_col = "CodContacto", cols_activos = "Reactivar",
      sortable = TRUE, searchable = TRUE, page_size = 20, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = TRUE
    )
    
    reactivar_cod_rv <- reactiveVal(NULL)
    reactivar_mod <- FormularioReactivar(id = "mod_reactivar", usr = usr, cod_contacto = reactive(reactivar_cod_rv()))
    
    observeEvent(mod_tabla$seleccion(), {
      sel <- mod_tabla$seleccion()
      req(sel, sel$col == "Reactivar")
      reactivar_cod_rv(sel$fila$CodContacto[[1]])
      showModal(modalDialog(title = "Reactivar Registro", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                            FormularioReactivarUI(ns("mod_reactivar"))))
    })
    
    observeEvent(reactivar_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
  })
}

# App de prueba ----
# Requiere haber cargado antes TablaContactos.R, TablaProspectos.R y TablaLeads.R
# (este último aporta .RAZONES_DESCARTE_LEAD, usado en .categoria_motivo_descarte)

ui <- bs4DashPage(
  title = "Prueba Tab Descartados",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(), controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    TablaDescartadosUI("tab_descartados")
  )
)

server <- function(input, output, session) {
  TablaDescartados("tab_descartados", usr = reactive("CMEDINA"))
}

shinyApp(ui, server)
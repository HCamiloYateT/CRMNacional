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
      column(6, box(title = "Descartados por Etapa de Origen", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_etapa"), height = "220px"))),
      column(6, box(title = "Descartados por Motivo", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_motivo"), height = "220px")))
    ),
    fluidRow(
      column(6, box(title = "Descartados por Canal de Origen", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_canal"), height = "220px"))),
      column(6, box(title = "Descartados por Asesor (solo los que fueron Lead)", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_asesor"), height = "220px")))
    ),
    fluidRow(
      column(6, box(title = "Antigüedad desde el Descarte", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_antiguedad_descarte"), height = "220px"))),
      column(6, box(title = "Días Promedio hasta el Descarte (por Etapa)", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_tiempo_descarte"), height = "220px")))
    ),
    fluidRow(
      column(12, box(title = "Motivo × Canal de Origen", width = 12, collapsible = FALSE,
                     plotly::plotlyOutput(ns("kpi_motivo_canal"), height = "220px"),
                     tags$p(style = "font-size:11px; color:#888; margin-top:6px;",
                            "Nota: un análisis de descarte por rango de precio requeriría capturar un valor numérico de precio en el motivo de descarte, que hoy no existe en el catálogo — actualmente 'PRECIO' no es una categoría capturada; si se agrega, este cruce se puede extender.")))
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
      lead_data <- tryCatch(CargarDatos("CONTACTOLEAD") %>% select(CodContacto, Asesor), error = function(e) data.frame(CodContacto = character(), Asesor = character()))
      
      if (!"EtapaPreDescarte" %in% names(dat)) dat$EtapaPreDescarte <- NA_character_
      
      dat %>%
        mutate(FechaHoraModi = as_datetime(FechaHoraModi),
               EtapaPreDescarte = ifelse(is.na(EtapaPreDescarte), "CONTACTO", EtapaPreDescarte),
               DiasDesdeDescarte = as.numeric(difftime(Sys.time(), FechaHoraModi, units = "days")),
               DiasHastaDescarte = as.numeric(difftime(FechaHoraModi, as_datetime(FechaHoraCrea), units = "days")),
               RangoAntiguedad = .rangos_antiguedad(DiasDesdeDescarte)) %>%
        left_join(obtener_ultimo_motivo_descarte(), by = "CodContacto") %>%
        left_join(lead_data, by = "CodContacto") %>%
        mutate(CategoriaMotivo = .categoria_motivo_descarte(Motivo),
               Origen = ifelse(is.na(Origen) | Origen == "", "SIN DATO", Origen))
    })
    
    output$kpi_tiempo_descarte <- plotly::renderPlotly({
      dat <- descartados_raw() %>%
        group_by(EtapaPreDescarte) %>%
        summarise(n = round(mean(DiasHastaDescarte, na.rm = TRUE), 1), .groups = "drop")
      .grafico_barras_horizontal(dat, "EtapaPreDescarte", "n", color = "#5a6474", titulo_x = "Días promedio hasta el descarte")
    })
    
    output$kpi_etapa <- plotly::renderPlotly({
      dat <- descartados_raw() %>% count(EtapaPreDescarte, sort = TRUE, name = "n")
      .grafico_barras_horizontal(dat, "EtapaPreDescarte", "n", color = "#1C398E", titulo_x = "Descartados")
    })
    
    output$kpi_motivo <- plotly::renderPlotly({
      dat <- descartados_raw() %>% mutate(CategoriaMotivo = ifelse(is.na(CategoriaMotivo), "SIN MOTIVO", CategoriaMotivo)) %>%
        count(CategoriaMotivo, sort = TRUE, name = "n")
      .grafico_barras_horizontal(dat, "CategoriaMotivo", "n", color = "#C11007", titulo_x = "Descartados")
    })
    
    output$kpi_canal <- plotly::renderPlotly({
      dat <- descartados_raw() %>% count(Origen, sort = TRUE, name = "n")
      .grafico_barras_horizontal(dat, "Origen", "n", color = "#0F6E56", titulo_x = "Descartados")
    })
    
    output$kpi_asesor <- plotly::renderPlotly({
      dat <- descartados_raw() %>% filter(!is.na(Asesor)) %>% count(Asesor, sort = TRUE, name = "n")
      .grafico_barras_horizontal(dat, "Asesor", "n", color = "#6f42c1", titulo_x = "Descartados")
    })
    
    output$kpi_antiguedad_descarte <- plotly::renderPlotly({
      dat <- descartados_raw() %>% count(RangoAntiguedad, .drop = FALSE, name = "n")
      .grafico_barras_horizontal(dat, "RangoAntiguedad", "n", color = "#C8862A", titulo_x = "Descartados")
    })
    
    output$kpi_motivo_canal <- plotly::renderPlotly({
      dat <- descartados_raw() %>%
        mutate(CategoriaMotivo = ifelse(is.na(CategoriaMotivo), "SIN MOTIVO", CategoriaMotivo)) %>%
        count(Origen, CategoriaMotivo)
      if (nrow(dat) == 0) return(plotly::config(plotly::plotly_empty(type = "heatmap"), displayModeBar = FALSE))
      p <- plotly::plot_ly(
        dat, x = ~CategoriaMotivo, y = ~Origen, z = ~n, type = "heatmap", colorscale = "Reds",
        hovertemplate = "<b>%{y} — %{x}</b><br>Descartados: %{z}<extra></extra>"
      ) %>%
        plotly::layout(margin = list(l = 80, r = 20, t = 10, b = 60),
                       paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                       hoverlabel = list(bgcolor = "#1A3C5E", font = list(color = "white", size = 12)))
      plotly::config(p, displayModeBar = FALSE)
    })
    
    data_tabla <- reactive({
      descartados_raw() %>%
        mutate(Acciones = CodContacto) %>%
        arrange(desc(FechaHoraModi)) %>%
        select(Acciones, CodContacto, PerRazSoc, PerCod, EtapaPreDescarte, Motivo, Origen, DiasHastaDescarte, DiasDesdeDescarte, FechaHoraModi)
    })
    
    mod_tabla <- TablaReactable(
      id = "tabla_descartados", data = data_tabla, columnas = NULL,
      col_specs = list(
        Acciones = .coldef_dropdown_acciones(ns, c(reactivar = "Reactivar")),
        CodContacto       = reactable::colDef(show = FALSE),
        PerRazSoc         = reactable::colDef(name = "Razón Social", minWidth = 180),
        PerCod            = reactable::colDef(name = "NIT", minWidth = 100),
        EtapaPreDescarte  = reactable::colDef(name = "Etapa de Origen", minWidth = 120),
        Motivo            = reactable::colDef(name = "Motivo", minWidth = 160),
        Origen            = reactable::colDef(name = "Canal", minWidth = 100),
        DiasHastaDescarte = reactable::colDef(name = "Días hasta el Descarte", minWidth = 140, cell = function(v) round(v, 0)),
        DiasDesdeDescarte = reactable::colDef(name = "Días desde Descarte", minWidth = 130, cell = function(v) round(v, 0)),
        FechaHoraModi     = reactable::colDef(name = "Fecha de Descarte", minWidth = 140, format = reactable::colFormat(datetime = TRUE))
      ),
      modo_seleccion = "ninguno", id_col = "CodContacto",
      sortable = TRUE, searchable = TRUE, page_size = 20, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = TRUE
    )
    
    reactivar_cod_rv <- reactiveVal(NULL)
    reactivar_mod <- FormularioReactivar(id = "mod_reactivar", usr = usr, cod_contacto = reactive(reactivar_cod_rv()))
    
    observeEvent(input$accion_tabla, {
      acc <- input$accion_tabla
      req(acc$cod_contacto, acc$accion == "reactivar")
      reactivar_cod_rv(acc$cod_contacto)
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
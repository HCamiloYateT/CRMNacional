# Funciones ----

# Este archivo depende de TablaContactos.R, TablaProspectos.R,
# TablaLeads.R y TablaDescartados.R: derivar_etapa_actual,
# .RAZONES_DESCARTE_CONTACTO/.RAZONES_DESCARTE_LEAD/.RAZONES_DESCARTE_PROSPECTO,
# .categoria_motivo_descarte, CajaModal/CajaModalUI (racafeModulos)

# Sanea NA/NaN/Inf a 0 — evita que racafe::CajaValor rechace el valor cuando
# aún no hay suficientes datos para calcular un promedio
.valor_seguro <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) return(0)
  round(x, digits)
}

# Calcula el conjunto completo de métricas del embudo en una sola pasada.
# Etapa derivada de derivar_etapa_actual(): Lead ahora puede venir de
# Contacto directo o de Prospecto reclasificado, por eso el historial
# (no solo CONTACTOLEAD) es la fuente de las tasas Contacto/Prospecto -> Lead
calcular_metricas_embudo <- function() {
  contactos <- derivar_etapa_actual()
  historial <- tryCatch({
    CargarDatos("CRMNALHISTORIALETAPA") %>% mutate(FechaHora = as_datetime(FechaHora))
  }, error = function(e) data.frame(CodContacto = character(), EtapaAnterior = character(),
                                    EtapaNueva = character(), Motivo = character(), FechaHora = as.POSIXct(character())))
  
  vinculos <- tryCatch(CargarDatos("CRMNALVINCULONIT"), error = function(e) data.frame(CodContacto = character()))
  leads_dat <- tryCatch(CargarDatos("CONTACTOLEAD"), error = function(e) data.frame(CodContacto = character(), Asesor = character()))
  
  # Volumen histórico por etapa vigente
  total_contactos  <- contactos %>% filter(Etapa == "CONTACTO") %>% nrow()
  total_leads       <- contactos %>% filter(Etapa == "LEAD") %>% nrow()
  total_prospectos  <- contactos %>% filter(Etapa == "PROSPECTO") %>% nrow()
  total_clientes    <- contactos %>% filter(Etapa == "CLIENTE") %>% nrow()
  total_descartados <- contactos %>% filter(Estado == "DESCARTADO") %>% nrow()
  
  descartados_contacto  <- contactos %>% filter(Estado == "DESCARTADO", EtapaPreDescarte == "CONTACTO") %>% nrow()
  descartados_lead      <- contactos %>% filter(Estado == "DESCARTADO", EtapaPreDescarte == "LEAD") %>% nrow()
  descartados_prospecto <- contactos %>% filter(Estado == "DESCARTADO", EtapaPreDescarte == "PROSPECTO") %>% nrow()
  
  # Total histórico que alguna vez llegó a Lead (vía cualquier camino) y a
  # Prospecto — usa el historial, no el conteo de la etapa vigente, porque
  # un Lead puede haber salido de esa etapa (Cliente o Descartado)
  total_leads_historico     <- historial %>% filter(EtapaNueva == "LEAD") %>% distinct(CodContacto) %>% nrow()
  total_prospectos_historico <- historial %>% filter(EtapaNueva == "PROSPECTO") %>% distinct(CodContacto) %>% nrow()
  leads_desde_prospecto      <- historial %>% filter(EtapaNueva == "LEAD", EtapaAnterior == "PROSPECTO") %>% distinct(CodContacto) %>% nrow()
  
  # Tasas de conversión (proporciones 0-1; racafe::FormatearNumero ya
  # multiplica por 100 con formato="porcentaje")
  tasa_contacto_lead     <- if (total_contactos + total_leads_historico > 0) total_leads_historico / (total_contactos + total_leads_historico) else 0
  tasa_lead_cliente      <- if (total_leads_historico > 0) total_clientes / total_leads_historico else 0
  tasa_global            <- if (total_contactos > 0) total_clientes / total_contactos else 0
  tasa_descarte_contacto <- if (total_contactos > 0) descartados_contacto / total_contactos else 0
  tasa_descarte_lead     <- if (total_leads_historico > 0) descartados_lead / total_leads_historico else 0
  tasa_descarte_prospecto <- if (total_prospectos_historico > 0) descartados_prospecto / total_prospectos_historico else 0
  tasa_prospecto_a_lead  <- if (total_prospectos_historico > 0) leads_desde_prospecto / total_prospectos_historico else 0
  
  # Tiempos promedio entre etapas (días)
  tiempo_contacto_lead <- .valor_seguro({
    leads_dat %>%
      left_join(CargarDatos("CRMNALCONTACTO") %>% select(CodContacto, FechaHoraCrea), by = "CodContacto") %>%
      mutate(dias = as.numeric(difftime(as_datetime(FechaConversion), as_datetime(FechaHoraCrea), units = "days"))) %>%
      summarise(m = mean(dias, na.rm = TRUE)) %>% pull(m)
  })
  
  tiempo_lead_cliente <- .valor_seguro({
    clientes_dat <- tryCatch(CargarDatos("CRMNALLEADCLIENTE"), error = function(e) data.frame(CodContacto = character(), FechaConversion = as.Date(character())))
    clientes_dat %>%
      left_join(leads_dat %>% select(CodContacto, FechaConversion), by = "CodContacto", suffix = c("_cli", "_lead")) %>%
      mutate(dias = as.numeric(difftime(as_datetime(FechaConversion_cli), as_datetime(FechaConversion_lead), units = "days"))) %>%
      summarise(m = mean(dias, na.rm = TRUE)) %>% pull(m)
  })
  
  tiempo_ciclo_total <- .valor_seguro({
    clientes_dat <- tryCatch(CargarDatos("CRMNALLEADCLIENTE"), error = function(e) data.frame(CodContacto = character(), FechaConversion = as.Date(character())))
    clientes_dat %>%
      left_join(CargarDatos("CRMNALCONTACTO") %>% select(CodContacto, FechaHoraCrea), by = "CodContacto") %>%
      mutate(dias = as.numeric(difftime(as_datetime(FechaConversion), as_datetime(FechaHoraCrea), units = "days"))) %>%
      summarise(m = mean(dias, na.rm = TRUE)) %>% pull(m)
  })
  
  # Actividad reciente (últimos 30 días), sobre fecha de creación / conversión
  base_contactos <- CargarDatos("CRMNALCONTACTO")
  contactos_30d <- base_contactos %>% filter(as_datetime(FechaHoraCrea) >= Sys.time() - lubridate::days(30)) %>% nrow()
  leads_30d     <- historial %>% filter(EtapaNueva == "LEAD", FechaHora >= Sys.time() - lubridate::days(30)) %>% nrow()
  clientes_30d  <- historial %>% filter(EtapaNueva == "CLIENTE", FechaHora >= Sys.time() - lubridate::days(30)) %>% nrow()
  
  # Calidad de identificación en origen
  leads_con_vinculo <- leads_dat %>% filter(CodContacto %in% vinculos$CodContacto) %>% nrow()
  pct_leads_vinculo <- if (nrow(leads_dat) > 0) leads_con_vinculo / nrow(leads_dat) else 0
  
  # Conversión por canal de origen
  conversion_por_canal <- base_contactos %>%
    mutate(EsLead = CodContacto %in% (historial %>% filter(EtapaNueva == "LEAD") %>% pull(CodContacto)),
           EsCliente = CodContacto %in% contactos$CodContacto[contactos$Etapa == "CLIENTE"]) %>%
    group_by(Origen) %>%
    summarise(Contactos = n(), Leads = sum(EsLead), Clientes = sum(EsCliente), .groups = "drop") %>%
    mutate(TasaConversion = ifelse(Contactos > 0, Clientes / Contactos, 0)) %>%
    arrange(desc(TasaConversion))
  
  # Conversión por asesor — solo cubre registros que alguna vez tuvieron
  # Asesor asignado (etapa Lead en adelante). Contactos y Prospectos no
  # tienen Asesor en el modelo de datos (se asigna recién al ascender a
  # Lead), así que no pueden atribuirse a un asesor específico aquí.
  asesor_etapa <- leads_dat %>%
    distinct(CodContacto, Asesor) %>%
    left_join(contactos %>% select(CodContacto, Etapa), by = "CodContacto") %>%
    filter(Etapa %in% c("LEAD", "CLIENTE", "DESCARTADO"))
  
  conversion_por_asesor <- asesor_etapa %>%
    distinct(Asesor) %>%
    mutate(
      Leads = sapply(Asesor, function(a) sum(asesor_etapa$Asesor == a & asesor_etapa$Etapa == "LEAD")),
      Clientes = sapply(Asesor, function(a) sum(asesor_etapa$Asesor == a & asesor_etapa$Etapa == "CLIENTE")),
      Descartados = sapply(Asesor, function(a) sum(asesor_etapa$Asesor == a & asesor_etapa$Etapa == "DESCARTADO")),
      Total = Leads + Clientes + Descartados,
      TasaConversion = ifelse(Total > 0, Clientes / Total, 0),
      TasaDescarte = ifelse(Total > 0, Descartados / Total, 0)
    ) %>%
    arrange(desc(TasaConversion))
  
  # Motivos de descarte
  ultimo_descarte <- historial %>%
    filter(EtapaNueva == "DESCARTADO") %>%
    group_by(CodContacto) %>% filter(FechaHora == max(FechaHora)) %>% slice(1) %>% ungroup() %>%
    mutate(CategoriaMotivo = .categoria_motivo_descarte(Motivo))
  
  motivos_descarte <- ultimo_descarte %>% count(CategoriaMotivo, sort = TRUE)
  
  motivos_por_origen <- ultimo_descarte %>%
    left_join(contactos %>% select(CodContacto, EtapaPreDescarte), by = "CodContacto") %>%
    filter(!is.na(EtapaPreDescarte)) %>%
    count(EtapaPreDescarte, CategoriaMotivo)
  
  list(
    total_contactos = total_contactos, total_leads = total_leads, total_prospectos = total_prospectos,
    total_clientes = total_clientes, total_descartados = total_descartados,
    descartados_contacto = descartados_contacto, descartados_lead = descartados_lead,
    descartados_prospecto = descartados_prospecto,
    tasa_contacto_lead = tasa_contacto_lead, tasa_lead_cliente = tasa_lead_cliente, tasa_global = tasa_global,
    tasa_descarte_contacto = tasa_descarte_contacto, tasa_descarte_lead = tasa_descarte_lead,
    tasa_descarte_prospecto = tasa_descarte_prospecto, tasa_prospecto_a_lead = tasa_prospecto_a_lead,
    tiempo_contacto_lead = tiempo_contacto_lead, tiempo_lead_cliente = tiempo_lead_cliente,
    tiempo_ciclo_total = tiempo_ciclo_total,
    contactos_30d = contactos_30d, leads_30d = leads_30d, clientes_30d = clientes_30d,
    leads_con_vinculo = leads_con_vinculo, pct_leads_vinculo = pct_leads_vinculo,
    conversion_por_canal = conversion_por_canal, conversion_por_asesor = conversion_por_asesor,
    motivos_descarte = motivos_descarte, motivos_por_origen = motivos_por_origen,
    ultimo_descarte = ultimo_descarte
  )
}

# Trae la ubicación de cada contacto para el resumen geográfico y el mapa
calcular_georeferenciacion <- function() {
  ubicacion <- tryCatch(CargarDatos("CRMNALCONTACTOUBICACION"), error = function(e) data.frame(
    CodContacto = character(), Pais = character(), Depto = character(), Mpio = character(),
    Direccion = character(), lat = double(), lng = double()
  ))
  contactos <- CargarDatos("CRMNALCONTACTO") %>% select(CodContacto, PerRazSoc, PerCod)
  dat <- ubicacion %>% left_join(contactos, by = "CodContacto")
  
  resumen_depto <- dat %>%
    filter(!is.na(Depto), Depto != "") %>%
    mutate(Mpio = ifelse(is.na(Mpio) | Mpio == "", "SIN DATO", Mpio)) %>%
    count(Depto, Mpio, sort = TRUE, name = "Contactos")
  
  puntos_mapa <- dat %>% filter(!is.na(lat), !is.na(lng))
  
  list(resumen_depto = resumen_depto, puntos_mapa = puntos_mapa,
       total_con_ubicacion = nrow(dat), total_con_coordenadas = nrow(puntos_mapa))
}

# Construye el gráfico de embudo (funnel) limpio de 3 etapas de venta
# directa (Contacto -> Lead -> Cliente); Prospecto se reporta aparte, ya
# que no es un paso secuencial de este mismo camino
grafico_embudo <- function(metricas) {
  etapas  <- c("Contacto", "Lead", "Cliente")
  valores <- c(metricas$total_contactos, metricas$total_leads, metricas$total_clientes)
  colores <- c("#5a6474", "#C8862A", "#1e8449")
  
  p <- plotly::plot_ly(
    type = "funnel", y = etapas, x = valores, textinfo = "value+percent initial",
    marker = list(color = colores),
    hovertemplate = paste0("<b>%{y}</b><br>Registros: %{x}<br>% del total inicial: %{percentInitial}<br>",
                           "% de la etapa anterior: %{percentPrevious}<extra></extra>")
  ) %>%
    plotly::layout(
      margin = list(l = 100, r = 40, t = 20, b = 20), showlegend = FALSE,
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      hoverlabel = list(bgcolor = "#1A3C5E", bordercolor = "#1A3C5E", font = list(color = "white", size = 12))
    )
  plotly::config(p, displayModeBar = FALSE)
}

# Barras apiladas de Descartados: eje X = Motivo, color/leyenda = Etapa de origen
grafico_descartados <- function(dat_motivo_origen) {
  if (nrow(dat_motivo_origen) == 0) return(plotly::config(plotly::plotly_empty(type = "bar"), displayModeBar = FALSE))
  
  totales_motivo <- dat_motivo_origen %>% group_by(CategoriaMotivo) %>% summarise(total = sum(n), .groups = "drop")
  dat_motivo_origen <- dat_motivo_origen %>% left_join(totales_motivo, by = "CategoriaMotivo") %>% mutate(pct = round(n / total * 100, 1))
  
  p <- plotly::plot_ly(
    dat_motivo_origen, x = ~CategoriaMotivo, y = ~n, color = ~EtapaPreDescarte, type = "bar",
    colors = c("CONTACTO" = "#5a6474", "LEAD" = "#C8862A", "PROSPECTO" = "#6f42c1"),
    customdata = ~pct,
    hovertemplate = paste0("<b>%{fullData.name}</b><br>Motivo: %{x}<br>Descartados: %{y}<br>",
                           "% de ese motivo: %{customdata}%<extra></extra>")
  ) %>%
    plotly::layout(
      barmode = "stack", xaxis = list(title = "", tickangle = -25, automargin = TRUE),
      yaxis = list(title = "Descartados"), margin = list(b = 90),
      showlegend = TRUE, legend = list(orientation = "h", x = 0, y = -0.25, title = list(text = "Etapa de origen")),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      hoverlabel = list(bgcolor = "#1A3C5E", bordercolor = "#1A3C5E", font = list(color = "white", size = 12))
    )
  plotly::config(p, displayModeBar = FALSE)
}

# Treemap de conversión por canal
treemap_canal <- function(dat) {
  if (nrow(dat) == 0) return(plotly::config(plotly::plotly_empty(type = "treemap"), displayModeBar = FALSE))
  dat <- dat %>% mutate(Etiqueta = ifelse(is.na(Origen) | Origen == "", "SIN DATO", Origen),
                        PctTexto = paste0(round(TasaConversion * 100, 1), "%"))
  p <- plotly::plot_ly(
    type = "treemap", labels = ~dat$Etiqueta, parents = rep("", nrow(dat)), values = ~dat$Contactos, customdata = ~dat$PctTexto,
    marker = list(colors = ~dat$TasaConversion, colorscale = "Greens", showscale = TRUE, colorbar = list(title = "Tasa de\nconversión")),
    text = ~paste0(dat$Contactos, " contactos | ", dat$Leads, " leads | ", dat$Clientes, " clientes"),
    hovertemplate = paste0("<b>%{label}</b><br>%{text}<br>Tasa de conversión a Cliente: %{customdata}<extra></extra>")
  ) %>% plotly::layout(margin = list(t = 10, l = 10, r = 10, b = 10))
  plotly::config(p, displayModeBar = FALSE)
}

# Treemap de motivos de descarte
treemap_motivo <- function(dat) {
  if (nrow(dat) == 0) return(plotly::config(plotly::plotly_empty(type = "treemap"), displayModeBar = FALSE))
  total <- sum(dat$n)
  dat <- dat %>% mutate(PctTexto = paste0(round(n / total * 100, 1), "%"))
  p <- plotly::plot_ly(
    type = "treemap", labels = ~dat$CategoriaMotivo, parents = rep("", nrow(dat)), values = ~dat$n, customdata = ~dat$PctTexto,
    marker = list(colors = ~dat$n, colorscale = "Reds", showscale = TRUE, colorbar = list(title = "Casos")),
    hovertemplate = paste0("<b>%{label}</b><br>Casos: %{value}<br>% del total de descartes: %{customdata}<extra></extra>")
  ) %>% plotly::layout(margin = list(t = 10, l = 10, r = 10, b = 10))
  plotly::config(p, displayModeBar = FALSE)
}

.STOPWORDS_ES <- c(
  "el", "la", "los", "las", "un", "una", "unos", "unas", "de", "del", "al", "a", "en", "y", "o",
  "que", "no", "se", "por", "con", "para", "es", "su", "sus", "lo", "le", "les", "mas", "más",
  "muy", "sin", "sobre", "entre", "este", "esta", "esto", "estos", "estas", "ya", "fue", "ha",
  "han", "como", "porque", "cuando", "donde", "pero", "si", "ni", "nos", "va", "hay", "era",
  "eran", "son", "ser", "estan", "están", "asi", "así"
)

# Tokeniza los motivos de descarte en texto libre (categoría OTRAS)
tokenizar_motivos_libres <- function(cod_contacto_vec, historial_dat) {
  catalogos <- unique(c(.RAZONES_DESCARTE_CONTACTO, .RAZONES_DESCARTE_LEAD, .RAZONES_DESCARTE_PROSPECTO))
  textos <- historial_dat %>%
    filter(CodContacto %in% cod_contacto_vec, !Motivo %in% catalogos, !is.na(Motivo), nzchar(trimws(Motivo))) %>%
    pull(Motivo)
  if (length(textos) == 0) return(data.frame(word = character(), freq = integer()))
  
  palabras <- textos %>% str_to_lower() %>% str_replace_all("[^a-záéíóúñ ]", " ") %>% str_split("\\s+") %>% unlist() %>% trimws()
  palabras <- palabras[nchar(palabras) > 2 & !(palabras %in% .STOPWORDS_ES)]
  if (length(palabras) == 0) return(data.frame(word = character(), freq = integer()))
  
  as.data.frame(table(palabras), stringsAsFactors = FALSE) %>% rename(word = palabras, freq = Freq) %>% arrange(desc(freq))
}

# Modulos Auxiliares ----

.GraficoDetalleEmbudoUI <- function(id) plotly::plotlyOutput(shiny::NS(id, "grafico"), height = "380px")
.GraficoDetalleEmbudo <- function(id, plot_reactive) {
  moduleServer(id, function(input, output, session) { output$grafico <- plotly::renderPlotly({ plot_reactive() }) })
}

.NubePalabrasUI <- function(id) wordcloud2::wordcloud2Output(shiny::NS(id, "nube"), height = "380px")
.NubePalabras <- function(id, dat_reactive) {
  moduleServer(id, function(input, output, session) {
    output$nube <- wordcloud2::renderWordcloud2({
      dat <- dat_reactive()
      if (nrow(dat) == 0) return(NULL)
      wordcloud2::wordcloud2(dat, size = 0.7, color = "random-dark")
    })
  })
}

# Modulo Principal ----

## EmbudoConversion ----

EmbudoConversionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, box(title = "Embudo de Conversión", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("grafico_embudo"), height = "320px"))),
      column(6, box(title = "Descartados por Etapa de Origen y Motivo", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("grafico_descartados"), height = "320px")))
    ),
    h5("Volumen"),
    fluidRow(
      column(2, CajaModalUI(ns("kpi_total_contactos"))),
      column(2, CajaModalUI(ns("kpi_total_leads"))),
      column(2, CajaModalUI(ns("kpi_total_prospectos"))),
      column(3, CajaModalUI(ns("kpi_total_clientes"))),
      column(3, CajaModalUI(ns("kpi_total_descartados")))
    ),
    h5("Conversión"),
    fluidRow(
      column(3, CajaModalUI(ns("kpi_tasa_contacto_lead"))),
      column(3, CajaModalUI(ns("kpi_tasa_prospecto_a_lead"))),
      column(3, CajaModalUI(ns("kpi_tasa_lead_cliente"))),
      column(3, CajaModalUI(ns("kpi_mejor_canal")))
    ),
    h5("Velocidad del Ciclo"),
    fluidRow(
      column(4, CajaModalUI(ns("kpi_tiempo_contacto_lead"))),
      column(4, CajaModalUI(ns("kpi_tiempo_lead_cliente"))),
      column(4, CajaModalUI(ns("kpi_tiempo_ciclo_total")))
    ),
    h5("Pérdida y Calidad"),
    fluidRow(
      column(2, CajaModalUI(ns("kpi_tasa_descarte_contacto"))),
      column(2, CajaModalUI(ns("kpi_tasa_descarte_lead"))),
      column(2, CajaModalUI(ns("kpi_tasa_descarte_prospecto"))),
      column(3, CajaModalUI(ns("kpi_motivo_top"))),
      column(3, CajaModalUI(ns("kpi_pct_vinculo")))
    ),
    h5("Actividad Reciente (30 días)"),
    fluidRow(
      column(4, CajaModalUI(ns("kpi_contactos_30d"))),
      column(4, CajaModalUI(ns("kpi_leads_30d"))),
      column(4, CajaModalUI(ns("kpi_clientes_30d")))
    ),
    h5("Por Asesor"),
    fluidRow(column(12, box(width = 12, collapsible = FALSE, title = "Desempeño por Asesor (desde etapa Lead)",
                            reactable::reactableOutput(ns("tabla_asesor")),
                            tags$p(style = "font-size:11px; color:#888; margin-top:8px;",
                                   "Nota: Contactos y Prospectos no tienen Asesor asignado en el modelo de datos (se asigna al ascender a Lead), por eso esta tabla solo cubre Leads, Clientes y Descartados atribuibles a un asesor.")))),
    h5("Georreferenciación"),
    fluidRow(column(12, box(width = 12, collapsible = FALSE, title = "Ubicación de Contactos por Departamento y Municipio",
                            fluidRow(
                              column(5, reactable::reactableOutput(ns("tabla_geo"))),
                              column(7, leaflet::leafletOutput(ns("mapa_geo"), height = "420px"))
                            ),
                            tags$p(style = "font-size:11px; color:#888; margin-top:8px;", uiOutput(ns("nota_geo"))))))
  )
}

EmbudoConversion <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    metricas <- reactive({ calcular_metricas_embudo() })
    
    output$grafico_embudo <- plotly::renderPlotly({ grafico_embudo(metricas()) })
    output$grafico_descartados <- plotly::renderPlotly({ grafico_descartados(metricas()$motivos_por_origen) })
    
    .GraficoDetalleEmbudo("grafico_canal", plot_reactive = reactive(treemap_canal(metricas()$conversion_por_canal)))
    .GraficoDetalleEmbudo("grafico_motivo", plot_reactive = reactive(treemap_motivo(metricas()$motivos_descarte)))
    .NubePalabras("nube_motivo", dat_reactive = reactive(
      tokenizar_motivos_libres(metricas()$ultimo_descarte$CodContacto, metricas()$ultimo_descarte)
    ))
    
    output$tabla_asesor <- reactable::renderReactable({
      reactable::reactable(
        metricas()$conversion_por_asesor %>%
          mutate(`Tasa a Cliente` = paste0(round(TasaConversion * 100, 1), "%"),
                 `Tasa de Descarte` = paste0(round(TasaDescarte * 100, 1), "%")) %>%
          select(Asesor, Leads, Clientes, Descartados, Total, `Tasa a Cliente`, `Tasa de Descarte`),
        sortable = TRUE, compact = TRUE, searchable = FALSE, defaultSorted = "Total"
      )
    })
    
    geo <- reactive({ calcular_georeferenciacion() })
    
    output$tabla_geo <- reactable::renderReactable({
      reactable::reactable(geo()$resumen_depto, sortable = TRUE, compact = TRUE, searchable = TRUE,
                           columns = list(Depto = reactable::colDef(name = "Departamento"),
                                          Mpio = reactable::colDef(name = "Municipio"),
                                          Contactos = reactable::colDef(name = "Contactos")))
    })
    
    output$mapa_geo <- leaflet::renderLeaflet({
      puntos <- geo()$puntos_mapa
      mapa <- leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
      if (nrow(puntos) == 0) return(mapa %>% leaflet::setView(lng = -74.1, lat = 4.6, zoom = 5))
      mapa %>%
        leaflet::addMarkers(
          data = puntos, lng = ~lng, lat = ~lat,
          clusterOptions = leaflet::markerClusterOptions(),
          popup = ~paste0("<b>", ifelse(is.na(PerRazSoc), PerCod, PerRazSoc), "</b><br>", Mpio, ", ", Depto, "<br>", ifelse(is.na(Direccion), "", Direccion))
        ) %>%
        leaflet::fitBounds(lng1 = min(puntos$lng), lat1 = min(puntos$lat), lng2 = max(puntos$lng), lat2 = max(puntos$lat))
    })
    
    output$nota_geo <- renderUI({
      paste0(geo()$total_con_coordenadas, " de ", geo()$total_con_ubicacion,
             " contactos con ubicación tienen coordenadas validadas (lat/lng).")
    })
    
    # Volumen ----
    CajaModal("kpi_total_contactos", valor = reactive(metricas()$total_contactos), texto = "Contactos totales", icono = "address-book",
              mostrar_boton = FALSE, footer = "Contactos cuya etapa vigente es CONTACTO. No incluye los que ascendieron, se hicieron Prospecto o se descartaron.")
    
    CajaModal("kpi_total_leads", valor = reactive(metricas()$total_leads), texto = "Leads activos", icono = "bullseye",
              mostrar_boton = FALSE, footer = "Contactos cuya etapa vigente es LEAD, sin importar si llegaron directo desde Contacto o vía Prospecto.")
    
    CajaModal("kpi_total_prospectos", valor = reactive(metricas()$total_prospectos), texto = "Prospectos activos", icono = "handshake",
              mostrar_boton = FALSE, footer = "Contactos cuya etapa vigente es PROSPECTO: se gestionan vía alianzas con Clientes existentes, no con venta directa.")
    
    CajaModal("kpi_total_clientes", valor = reactive(metricas()$total_clientes), texto = "Clientes generados", icono = "handshake",
              mostrar_boton = FALSE, footer = "Leads con al menos una factura detectada (NIT propio o vinculado). Estado final del embudo.")
    
    CajaModal("kpi_total_descartados", valor = reactive(metricas()$total_descartados), texto = "Descartados totales", icono = "ban",
              mostrar_boton = FALSE,
              footer = reactive(paste0("Registros con Estado = DESCARTADO. De estos, ", metricas()$descartados_contacto,
                                       " desde Contacto, ", metricas()$descartados_lead, " desde Lead y ",
                                       metricas()$descartados_prospecto, " desde Prospecto.")))
    
    # Conversión ----
    CajaModal("kpi_tasa_contacto_lead", valor = reactive(metricas()$tasa_contacto_lead), formato = "porcentaje",
              texto = "Contacto → Lead", icono = "arrow-right", mostrar_boton = FALSE,
              footer = "Total histórico que llegó a Lead / (Contactos activos + ese total histórico). Incluye tanto el ascenso directo como la reclasificación desde Prospecto.")
    
    CajaModal("kpi_tasa_prospecto_a_lead", valor = reactive(metricas()$tasa_prospecto_a_lead), formato = "porcentaje",
              texto = "Prospecto → Lead", icono = "arrow-right", mostrar_boton = FALSE,
              footer = "De todos los que alguna vez fueron Prospecto, qué % se reclasificó a Lead. Mide cuánto crece el volumen de un Prospecto hasta interesar para venta directa.")
    
    CajaModal("kpi_tasa_lead_cliente", valor = reactive(metricas()$tasa_lead_cliente), formato = "porcentaje",
              texto = "Lead → Cliente", icono = "arrow-right", mostrar_boton = FALSE,
              footer = "Clientes / total histórico de Leads. Mide la efectividad de cierre una vez el lead está calificado y asignado.")
    
    CajaModal("kpi_mejor_canal", valor = reactive({ d <- metricas()$conversion_por_canal; if (nrow(d) == 0) "N/A" else d$Origen[1] }),
              texto = "Mejor Canal (Origen)", icono = "trophy", mostrar_boton = TRUE,
              titulo_modal = "Conversión por Canal de Origen", icono_modal = "trophy", tamano_modal = "l",
              contenido_modal = function() .GraficoDetalleEmbudoUI(ns("grafico_canal")),
              footer = "Canal de Origen con mayor tasa Contacto→Cliente. Ver detalle para el treemap por canal (tamaño = contactos, color = tasa de conversión).")
    
    # Velocidad ----
    CajaModal("kpi_tiempo_contacto_lead", valor = reactive(metricas()$tiempo_contacto_lead), formato = "numero",
              texto = "Días prom. hasta Lead", icono = "hourglass-half", mostrar_boton = FALSE,
              footer = "Promedio de días entre la creación del contacto y su llegada a Lead (directo o vía Prospecto).")
    
    CajaModal("kpi_tiempo_lead_cliente", valor = reactive(metricas()$tiempo_lead_cliente), formato = "numero",
              texto = "Días prom. Lead → Cliente", icono = "hourglass-half", mostrar_boton = FALSE,
              footer = "Promedio de días entre el ascenso a Lead y la primera factura detectada. Mide el ciclo de venta activo.")
    
    CajaModal("kpi_tiempo_ciclo_total", valor = reactive(metricas()$tiempo_ciclo_total), formato = "numero",
              texto = "Días prom. Ciclo Completo", icono = "stopwatch", mostrar_boton = FALSE,
              footer = "Promedio de días desde la creación del contacto hasta la primera factura. Ciclo de venta total del embudo.")
    
    # Pérdida y calidad ----
    CajaModal("kpi_tasa_descarte_contacto", valor = reactive(metricas()$tasa_descarte_contacto), formato = "porcentaje",
              texto = "Descarte en Contacto", icono = "circle-xmark", mostrar_boton = FALSE,
              footer = "Descartados con etapa de origen Contacto / Contactos totales. Pérdida antes de calificar como Lead o Prospecto.")
    
    CajaModal("kpi_tasa_descarte_lead", valor = reactive(metricas()$tasa_descarte_lead), formato = "porcentaje",
              texto = "Descarte en Lead", icono = "circle-xmark", mostrar_boton = FALSE,
              footer = "Descartados con etapa de origen Lead / total histórico de Leads. Pérdida después de asignar asesor y calificar.")
    
    CajaModal("kpi_tasa_descarte_prospecto", valor = reactive(metricas()$tasa_descarte_prospecto), formato = "porcentaje",
              texto = "Descarte en Prospecto", icono = "circle-xmark", mostrar_boton = FALSE,
              footer = "Descartados con etapa de origen Prospecto / total histórico de Prospectos. Pérdida en el modelo de alianzas.")
    
    CajaModal("kpi_motivo_top", valor = reactive({ d <- metricas()$motivos_descarte; if (nrow(d) == 0) "N/A" else d$CategoriaMotivo[1] }),
              texto = "Motivo Principal de Descarte", icono = "list-check", mostrar_boton = TRUE,
              titulo_modal = "Motivos de Descarte", icono_modal = "list-check", tamano_modal = "xl",
              contenido_modal = function() tagList(
                h5("Categorías de descarte"), .GraficoDetalleEmbudoUI(ns("grafico_motivo")), tags$hr(),
                h5("Palabras más frecuentes en descartes con motivo \"OTRAS\""), .NubePalabrasUI(ns("nube_motivo"))
              ),
              footer = "Categoría de descarte más frecuente (última transición a DESCARTADO). La nube de palabras resume el texto libre de la categoría OTRAS, quitando artículos, preposiciones y conectores.")
    
    CajaModal("kpi_pct_vinculo", valor = reactive(metricas()$pct_leads_vinculo), formato = "porcentaje",
              texto = "Leads con NIT Vinculado", icono = "link", mostrar_boton = FALSE,
              footer = "% de Leads que requirieron vincular manualmente un NIT distinto al de captura para detectar su factura. Un valor alto sugiere datos de identificación incompletos en la etapa Contacto.")
    
    # Actividad reciente ----
    CajaModal("kpi_contactos_30d", valor = reactive(metricas()$contactos_30d), texto = "Contactos (últimos 30d)", icono = "calendar-plus",
              mostrar_boton = FALSE, footer = "Contactos creados en los últimos 30 días corridos. Flujo de entrada reciente al embudo.")
    
    CajaModal("kpi_leads_30d", valor = reactive(metricas()$leads_30d), texto = "Leads (últimos 30d)", icono = "calendar-plus",
              mostrar_boton = FALSE, footer = "Transiciones a Lead (directo o vía Prospecto) en los últimos 30 días. Actividad comercial reciente.")
    
    CajaModal("kpi_clientes_30d", valor = reactive(metricas()$clientes_30d), texto = "Clientes (últimos 30d)", icono = "calendar-plus",
              mostrar_boton = FALSE, footer = "Conversiones a Cliente detectadas en los últimos 30 días corridos. Resultados recientes del embudo.")
  })
}

# App de prueba ----
# Requiere haber cargado antes TablaContactos.R, TablaProspectos.R,
# TablaLeads.R y TablaDescartados.R

ui <- bs4DashPage(
  title = "Prueba Embudo de Conversión",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(), controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    EmbudoConversionUI("EmbudoConversion")
  )
)

server <- function(input, output, session) {
  EmbudoConversion("EmbudoConversion")
}

shinyApp(ui, server)
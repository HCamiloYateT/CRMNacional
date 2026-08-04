# Funciones ----

# Muestra un modal y le agrega una clase CSS de tamaño al .modal-dialog real
MostrarModalConClase <- function(modal_ui, clase) {
  showModal(modal_ui)
  shinyjs::delay(0, shinyjs::runjs(
    sprintf("$('.modal-dialog').not('.%s').last().addClass('%s');", clase, clase)
  ))
}

# Deriva la etapa VIGENTE de cada contacto a partir de una única fuente de
# verdad: CRMNALHISTORIALETAPA. Regla: Estado=DESCARTADO -> "DESCARTADO";
# sin transiciones -> "CONTACTO" (nacimiento); si no, la EtapaNueva de la
# transición más reciente (incluye reclasificaciones y reactivaciones)
derivar_etapa_actual <- function(contactos = NULL) {
  if (is.null(contactos)) contactos <- CargarDatos("CRMNALCONTACTO")
  
  # Defensivo: si EtapaPreDescarte aún no existe en la tabla (ALTER TABLE
  # pendiente en CRMNALCONTACTO), se crea vacía en vez de fallar — todos los
  # módulos del embudo dependen de esta función para leer esa columna
  if (!"EtapaPreDescarte" %in% names(contactos)) contactos$EtapaPreDescarte <- NA_character_
  
  historial <- tryCatch({
    CargarDatos("CRMNALHISTORIALETAPA") %>% mutate(FechaHora = as_datetime(FechaHora))
  }, error = function(e) data.frame(CodContacto = character(), EtapaNueva = character(),
                                    FechaHora = as.POSIXct(character())))
  
  ultima_transicion <- historial %>%
    group_by(CodContacto) %>%
    filter(FechaHora == max(FechaHora)) %>%
    slice(1) %>% ungroup() %>%
    select(CodContacto, EtapaUltimaTransicion = EtapaNueva)
  
  contactos %>%
    left_join(ultima_transicion, by = "CodContacto") %>%
    mutate(
      EtapaPreDescarte = ifelse(is.na(EtapaPreDescarte), "CONTACTO", EtapaPreDescarte),
      Etapa = case_when(
        Estado == "DESCARTADO"       ~ "DESCARTADO",
        is.na(EtapaUltimaTransicion) ~ "CONTACTO",
        TRUE                         ~ EtapaUltimaTransicion
      )
    ) %>%
    select(-EtapaUltimaTransicion)
}

# Fecha en que un contacto entró a su etapa vigente: la transición más
# reciente, o FechaHoraCrea si nunca transicionó
derivar_fecha_entrada_etapa <- function(contactos_con_etapa) {
  historial <- tryCatch({
    CargarDatos("CRMNALHISTORIALETAPA") %>% mutate(FechaHora = as_datetime(FechaHora))
  }, error = function(e) data.frame(CodContacto = character(), EtapaNueva = character(),
                                    FechaHora = as.POSIXct(character())))
  
  ultima_transicion <- historial %>%
    group_by(CodContacto) %>%
    filter(FechaHora == max(FechaHora)) %>%
    slice(1) %>% ungroup() %>%
    select(CodContacto, FechaUltimaTransicion = FechaHora)
  
  contactos_con_etapa %>%
    left_join(ultima_transicion, by = "CodContacto") %>%
    mutate(FechaEntradaEtapa = coalesce(FechaUltimaTransicion, as_datetime(FechaHoraCrea))) %>%
    select(-FechaUltimaTransicion)
}

# Genera código consecutivo de contacto: prefijo + fecha + consecutivo diario
generar_codigo_contacto <- function() {
  hoy     <- format(Sys.Date(), "%Y%m%d")
  prefijo <- paste0("CT-", hoy, "-")
  
  existentes <- tryCatch({
    CargarDatos("CRMNALCONTACTO") %>%
      filter(str_starts(CodContacto, prefijo)) %>%
      pull(CodContacto)
  }, error = function(e) character(0))
  
  if (length(existentes) == 0) {
    consecutivo <- 1
  } else {
    consecutivo <- existentes %>%
      str_remove(prefijo) %>% as.integer() %>% max(na.rm = TRUE) %>% {. + 1}
  }
  
  paste0(prefijo, formatC(consecutivo, width = 3, flag = "0"))
}

# Valida NIT contra contactos activos y contra CRMNALMARLOT
validar_nit <- function(nit_input, cod_actual = NULL) {
  if (is.null(nit_input) || nit_input == "" || is.na(nit_input)) return(NULL)
  
  if (!EsEnteroPositivo(nit_input)) {
    return(FormatearTexto("* El NIT debe ser un valor numérico válido", negrita = T,
                          color = "red", tamano_pct = 0.75))
  }
  
  contactos <- CargarDatos("CRMNALCONTACTO")
  if (!is.null(cod_actual)) contactos <- contactos %>% filter(CodContacto != cod_actual)
  
  if (nit_input %in% contactos$PerCod) {
    return(FormatearTexto("* El NIT ya existe como contacto activo", negrita = T,
                          color = "red", tamano_pct = 0.75))
  }
  
  if (nit_input %in% CargarDatos("CRMNALMARLOT")$CLIENTE) {
    return(FormatearTexto("* El NIT ya existe en CRMNALMARLOT", negrita = T,
                          color = "red", tamano_pct = 0.75))
  }
  
  NULL
}

# Busca coincidencia exacta de razón social entre contactos y retorna su NIT
buscar_contacto_por_razon_social <- function(razon_social_input, cod_actual = NULL) {
  if (is.null(razon_social_input) || nchar(trimws(razon_social_input)) < 1) return(NULL)
  
  razon_social_input <- str_to_upper(trimws(razon_social_input))
  contactos <- CargarDatos("CRMNALCONTACTO")
  if (!is.null(cod_actual)) contactos <- contactos %>% filter(CodContacto != cod_actual)
  
  match <- contactos %>% filter(str_to_upper(PerRazSoc) == razon_social_input) %>% slice(1)
  if (nrow(match) == 0) return(NULL)
  
  list(cod_contacto = match$CodContacto, nit = match$PerCod)
}

# Valida razón social de forma únicamente informativa, nunca bloquea guardado
validar_razon_social <- function(razon_social_input, cod_actual = NULL) {
  if (is.null(razon_social_input) || nchar(trimws(razon_social_input)) < 1) return(NULL)
  
  razon_social_upper <- str_to_upper(razon_social_input)
  match_exacto <- buscar_contacto_por_razon_social(razon_social_input, cod_actual)
  
  similares <- NULL
  if (nchar(razon_social_upper) >= 3) {
    ncliente_similares <- NCLIENTE %>%
      filter(grepl(razon_social_upper, str_to_upper(PerRazSoc), fixed = TRUE)) %>%
      pull(PerRazSoc) %>% str_to_upper() %>% unique()
    
    fact_similares <- FACT %>%
      filter(grepl(razon_social_upper, str_to_upper(PerRazSoc), fixed = TRUE)) %>%
      pull(PerRazSoc) %>% str_to_upper() %>% unique()
    
    similares <- unique(c(ncliente_similares, fact_similares))
    if (length(similares) > 5) similares <- similares[1:5]
  }
  
  if (!is.null(match_exacto)) {
    return(FormatearTexto(
      paste0("* Ya existe un contacto con esta razón social (NIT: ", match_exacto$nit %||% "sin NIT", ")"),
      negrita = T, color = "orange", tamano_pct = 0.75))
  }
  if (!is.null(similares) && length(similares) > 0) {
    return(tagList(
      FormatearTexto("Nombres similares encontrados:", negrita = T, color = "orange", tamano_pct = 0.75),
      tags$ul(lapply(similares, function(x) tags$li(x)))
    ))
  }
  NULL
}

# Modal de confirmación de guardado de contacto, tamaño "aviso"
ModalConfirmarGuardadoContacto <- function(ns, texto, es_edicion) {
  modalDialog(
    title = "Confirmar guardado", easyClose = FALSE,
    footer = tagList(
      racafeShiny::Boton(ns("CON_CancelarGuardado"), label = "Cancelar", icono = "xmark",
                         color_fondo = "transparent", color_fuente = "#6c757d"),
      racafeShiny::Boton(ns("CON_ConfirmarGuardado"),
                         label = if (es_edicion) "Guardar Cambios" else "Crear Contacto",
                         icono = "floppy-disk", color_fondo = "#198754")
    ),
    tags$p(texto)
  )
}

# Determina CodLinNegocio y LinNegocio a partir del segmento principal
determinar_linea_negocio <- function(segmento_principal) {
  if (is.null(segmento_principal) || is.na(segmento_principal)) {
    return(list(cod = NA_character_, nombre = NA_character_))
  }
  if (segmento_principal == "A LA MEDIDA") {
    return(list(cod = "21000", nombre = "A LA MEDIDA"))
  } else if (segmento_principal == "CONVENCIONALES") {
    return(list(cod = "10000", nombre = "CONVENCIONALES"))
  } else {
    return(list(cod = NA_character_, nombre = NA_character_))
  }
}

# Registra una transición de etapa en el historial — fuente de verdad única
# consumida por derivar_etapa_actual()
registrar_transicion_etapa <- function(cod_contacto, etapa_anterior, etapa_nueva, usr,
                                       motivo = NA_character_) {
  registro_historial <- data.frame(
    CodContacto = cod_contacto, EtapaAnterior = etapa_anterior, EtapaNueva = etapa_nueva,
    Usuario = usr, FechaHora = Sys.time(), Motivo = motivo, stringsAsFactors = FALSE
  )
  AgregarDatos(registro_historial, "CRMNALHISTORIALETAPA")
}

# Descarta un contacto/lead/prospecto, registrando la etapa de origen para
# poder reactivarlo correctamente
descartar_generico <- function(cod_contacto, etapa_origen, motivo, usr) {
  fila_actual <- CargarDatos("CRMNALCONTACTO") %>% filter(CodContacto == cod_contacto)
  if (nrow(fila_actual) == 0) stop("Contacto no encontrado: ", cod_contacto)
  
  fila_actual$Estado           <- "DESCARTADO"
  fila_actual$EtapaPreDescarte <- etapa_origen
  fila_actual$UsuarioMod       <- usr
  fila_actual$FechaHoraModi    <- Sys.time()
  
  ReemplazarDatos(fila_actual, "CRMNALCONTACTO", llaves = list(CodContacto = cod_contacto))
  registrar_transicion_etapa(cod_contacto, etapa_origen, "DESCARTADO", usr, motivo = motivo)
  invisible(fila_actual)
}

# Reactiva un descartado, devolviéndolo a su etapa de origen (Contacto,
# Lead o Prospecto)
reactivar_contacto <- function(cod_contacto, usr) {
  fila_actual <- CargarDatos("CRMNALCONTACTO") %>% filter(CodContacto == cod_contacto)
  if (nrow(fila_actual) == 0) stop("Contacto no encontrado: ", cod_contacto)
  
  etapa_origen <- if ("EtapaPreDescarte" %in% names(fila_actual)) {
    fila_actual$EtapaPreDescarte[[1]] %||% "CONTACTO"
  } else "CONTACTO"
  
  fila_actual$Estado        <- "ACTIVO"
  fila_actual$UsuarioMod    <- usr
  fila_actual$FechaHoraModi <- Sys.time()
  
  ReemplazarDatos(fila_actual, "CRMNALCONTACTO", llaves = list(CodContacto = cod_contacto))
  registrar_transicion_etapa(cod_contacto, "DESCARTADO", etapa_origen, usr, motivo = "Reactivación")
  invisible(fila_actual)
}

# Registra una gestión comercial (comentario, llamada, visita, reunión, correo)
registrar_gestion_comercial <- function(cod_contacto, tipo, comentario, usr) {
  fila <- data.frame(CodContacto = cod_contacto, TipoGestion = tipo, Comentario = comentario,
                     Usuario = usr, FechaHora = Sys.time(), stringsAsFactors = FALSE)
  AgregarDatos(fila, "CRMNALGESTIONCOMERCIAL")
}

# Lista las gestiones comerciales de un contacto, más recientes primero
listar_gestion_comercial <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALGESTIONCOMERCIAL") %>%
      mutate(FechaHora = as_datetime(FechaHora)) %>%
      filter(CodContacto == cod_contacto) %>%
      arrange(desc(FechaHora))
  }, error = function(e) data.frame(TipoGestion = character(), Comentario = character(),
                                    Usuario = character(), FechaHora = as.POSIXct(character())))
}

# Badge HTML por tipo de gestión comercial
.badge_tipo_gestion <- function(tipo) {
  color <- switch(tipo, "Comentario" = "#1D4ED8", "Llamada" = "#0F6E56", "Visita" = "#c87000",
                  "Reunión" = "#6f42c1", "Correo" = "#C11007", "#6c757d")
  as.character(tags$span(
    style = paste0("display:inline-block; padding:2px 8px; border-radius:10px;",
                   "font-size:11px; background:", color, "1A; color:", color, ";"),
    tipo
  ))
}

# Registra un recordatorio en su tabla propia, consumida por un job de correo
registrar_recordatorio_contacto <- function(cod_contacto, asesor, fecha_hora, mensaje, usr) {
  fila <- data.frame(
    CodContacto = cod_contacto, Asesor = asesor, FechaRecordatorio = fecha_hora,
    Mensaje = mensaje, UsuarioCrea = usr, FechaHoraCrea = Sys.time(), Enviado = 0,
    stringsAsFactors = FALSE
  )
  AgregarDatos(fila, "CRMNALRECORDATORIOCONTACTO")
}

# Lista los recordatorios programados para un contacto, más próximos primero
listar_recordatorios_contacto <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALRECORDATORIOCONTACTO") %>%
      mutate(FechaRecordatorio = as_datetime(FechaRecordatorio)) %>%
      filter(CodContacto == cod_contacto) %>% arrange(FechaRecordatorio)
  }, error = function(e) data.frame(Asesor = character(), FechaRecordatorio = as.POSIXct(character()),
                                    Mensaje = character(), Enviado = numeric()))
}

# Tarjeta KPI simple usada en los análisis de antigüedad y canal
.kpi_card <- function(titulo, valor, color = "#1C398E") {
  tags$div(
    style = paste0("background:#FFFFFF; border:1px solid #E2E8F0; border-left:4px solid ", color, ";",
                   "border-radius:6px; padding:10px 14px; min-width:120px; text-align:center;"),
    FormatearTexto(as.character(valor), negrita = TRUE, tamano_pct = 1.3, color = color),
    tags$div(style = "margin-top:2px;", FormatearTexto(titulo, tamano_pct = 0.75, color = "#64748B"))
  )
}

# Gráfico de barras horizontales para reemplazar las tarjetas KPI planas —
# hover con conteo y etiqueta completa, útil cuando las categorías son largas
.grafico_barras_horizontal <- function(dat, col_label, col_valor, color = "#1C398E", titulo_x = "Conteo") {
  if (nrow(dat) == 0) return(plotly::config(plotly::plotly_empty(type = "bar"), displayModeBar = FALSE))
  
  dat <- dat[order(dat[[col_valor]]), ]
  p <- plotly::plot_ly(
    dat, x = dat[[col_valor]], y = factor(dat[[col_label]], levels = dat[[col_label]]),
    type = "bar", orientation = "h", marker = list(color = color),
    hovertemplate = paste0("<b>%{y}</b><br>", titulo_x, ": %{x}<extra></extra>")
  ) %>%
    plotly::layout(
      margin = list(l = 10, r = 20, t = 10, b = 30), xaxis = list(title = titulo_x),
      yaxis = list(title = ""), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      hoverlabel = list(bgcolor = "#1A3C5E", font = list(color = "white", size = 12))
    )
  plotly::config(p, displayModeBar = FALSE)
}

# colDef de un menú desplegable de acciones por fila — reemplaza tener una
# columna por cada botón; escalable a futuras acciones sin agregar columnas.
# `ns` se captura por clausura desde el server que la invoca, y `opciones`
# es un vector nombrado valor_accion = "Etiqueta visible"
.coldef_dropdown_acciones <- function(ns, opciones) {
  input_id <- ns("accion_tabla")
  opts_html <- paste0('<option value="">Acciones</option>',
                      paste0('<option value="', names(opciones), '">', opciones, '</option>', collapse = ""))
  reactable::colDef(
    name = "Acciones", minWidth = 130, html = TRUE, sortable = FALSE,
    cell = function(value) {
      sprintf(
        paste0('<select class="form-select form-select-sm" style="font-size:11px; padding:2px 6px;" ',
               'onchange="Shiny.setInputValue(\'%s\', {cod_contacto:\'%s\', accion:this.value, ts:Date.now()}, ',
               '{priority:\'event\'}); this.selectedIndex=0;">%s</select>'),
        input_id, value, opts_html
      )
    }
  )
}

# Rangos de antigüedad en días, cortes fijos de negocio
.rangos_antiguedad <- function(dias) {
  cut(dias, breaks = c(-Inf, 7, 15, 30, 60, Inf),
      labels = c("0-7 días", "8-15 días", "16-30 días", "31-60 días", "+60 días"), right = TRUE)
}

# Indicador de completitud de identificación: Razón Social / NIT / ambos
.indicador_identificacion <- function(razon_social, nit) {
  case_when(
    !EsVacio(razon_social) & !EsVacio(nit) ~ "Completo",
    !EsVacio(razon_social) & EsVacio(nit)   ~ "Solo Razón Social",
    EsVacio(razon_social) & !EsVacio(nit)   ~ "Solo NIT",
    TRUE                                    ~ "Sin datos"
  )
}

# Badge HTML para el indicador de identificación
.badge_identificacion <- function(valor) {
  color <- switch(valor, "Completo" = "#198754", "Solo Razón Social" = "#c87000",
                  "Solo NIT" = "#c87000", "#C11007")
  as.character(tags$span(
    style = paste0("display:inline-block; padding:2px 8px; border-radius:10px; font-size:11px;",
                   "background:", color, "1A; color:", color, ";"),
    valor
  ))
}

# colDef genérico para columnas de acción (botón HTML por fila)
.coldef_accion <- function(etiqueta, icono, color) {
  reactable::colDef(
    name = "", minWidth = 46, html = TRUE, sortable = FALSE,
    cell = function(value) {
      as.character(tags$span(
        title = etiqueta,
        style = paste0("display:inline-flex; align-items:center; justify-content:center;",
                       "width:28px; height:28px; border-radius:6px; cursor:pointer;",
                       "background:", color, "; color:white; font-size:12px;"),
        icon(icono)
      ))
    }
  )
}

# Consulta Nominatim (OpenStreetMap) y retorna lat/lng/dirección normalizada
geocodificar_direccion <- function(direccion, mpio, depto, pais = "Colombia") {
  consulta <- paste(direccion, mpio, depto, pais, sep = ", ")
  Sys.sleep(1)
  
  resp <- tryCatch({
    httr::GET("https://nominatim.openstreetmap.org/search",
              query = list(q = consulta, format = "json", addressdetails = 1, limit = 1),
              httr::add_headers(`User-Agent` = "CRMRacafe/1.0 (contacto@racafe.com)"))
  }, error = function(e) NULL)
  
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  resultado <- tryCatch(httr::content(resp, as = "parsed"), error = function(e) NULL)
  if (is.null(resultado) || length(resultado) == 0) return(NULL)
  
  list(lat = as.numeric(resultado[[1]]$lat), lng = as.numeric(resultado[[1]]$lon),
       direccion_formateada = resultado[[1]]$display_name)
}

.TIPOS_RED_SOCIAL <- c("WhatsApp", "LinkedIn", "Instagram", "Facebook", "Sitio Web")
.MAX_PERSONAS_CONTACTO <- 10

.generar_id_persona <- function() paste0("PC-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))
.generar_id_sucursal <- function() paste0("SU-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))
.generar_id_red      <- function() paste0("RD-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))

# Registra una persona de contacto (sin redes sociales, van en tabla aparte)
registrar_persona_contacto <- function(cod_contacto, nombre, cargo, telefono, correo, usr) {
  id_persona <- .generar_id_persona()
  fila <- data.frame(
    IdPersona = id_persona, CodContacto = cod_contacto, Nombre = nombre, Cargo = cargo,
    Telefono = telefono, Correo = correo, UsuarioCrea = usr, FechaHoraCrea = Sys.time(),
    stringsAsFactors = FALSE
  )
  AgregarDatos(fila, "CRMNALCONTACTOPERSONA")
  id_persona
}

# Lista las personas de contacto, con sus redes sociales concatenadas
listar_personas_contacto <- function(cod_contacto) {
  personas <- tryCatch({
    CargarDatos("CRMNALCONTACTOPERSONA") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame(IdPersona = character(), Nombre = character(),
                                    Cargo = character(), Telefono = character(), Correo = character()))
  if (nrow(personas) == 0) return(personas %>% mutate(RedesResumen = character()))
  
  redes <- tryCatch({
    CargarDatos("CRMNALCONTACTOPERSONARED") %>%
      filter(IdPersona %in% personas$IdPersona) %>%
      group_by(IdPersona) %>%
      summarise(RedesResumen = paste(paste0(TipoRedSocial, ": ", UsuarioRed), collapse = " | "), .groups = "drop")
  }, error = function(e) data.frame(IdPersona = character(), RedesResumen = character()))
  
  personas %>% left_join(redes, by = "IdPersona") %>% mutate(RedesResumen = RedesResumen %||% "")
}

# Elimina una persona de contacto y sus redes sociales asociadas
eliminar_persona_contacto <- function(id_persona) {
  vacio_persona <- CargarDatos("CRMNALCONTACTOPERSONA") %>% filter(FALSE)
  ReemplazarDatos(vacio_persona, "CRMNALCONTACTOPERSONA", llaves = list(IdPersona = id_persona))
  
  redes <- tryCatch(CargarDatos("CRMNALCONTACTOPERSONARED") %>% filter(IdPersona == id_persona),
                    error = function(e) data.frame(IdRed = character()))
  vacio_red <- redes %>% filter(FALSE)
  for (id_red in redes$IdRed) {
    ReemplazarDatos(vacio_red, "CRMNALCONTACTOPERSONARED", llaves = list(IdRed = id_red))
  }
}

registrar_red_social_persona <- function(id_persona, tipo_red, usuario_red) {
  fila <- data.frame(IdRed = .generar_id_red(), IdPersona = id_persona,
                     TipoRedSocial = tipo_red, UsuarioRed = usuario_red, stringsAsFactors = FALSE)
  AgregarDatos(fila, "CRMNALCONTACTOPERSONARED")
}

guardar_ubicacion_contacto <- function(cod_contacto, pais, depto, mpio, direccion, lat, lng, usr) {
  fila <- data.frame(CodContacto = cod_contacto, Pais = pais, Depto = depto, Mpio = mpio,
                     Direccion = direccion, lat = lat, lng = lng, UsuarioMod = usr,
                     FechaHoraModi = Sys.time(), stringsAsFactors = FALSE)
  ReemplazarDatos(fila, "CRMNALCONTACTOUBICACION", llaves = list(CodContacto = cod_contacto))
}

cargar_ubicacion_contacto <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALCONTACTOUBICACION") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame())
}

guardar_potencial_contacto <- function(cod_contacto, es_tostador, alianza, detalle_alianza,
                                       maquila, detalle_maquila, observaciones, usr) {
  fila <- data.frame(CodContacto = cod_contacto, EsTostador = es_tostador, AlianzaTostadora = alianza,
                     DetalleAlianza = detalle_alianza, Maquila = maquila, DetalleMaquila = detalle_maquila,
                     Observaciones = observaciones, UsuarioMod = usr, FechaHoraModi = Sys.time(),
                     stringsAsFactors = FALSE)
  ReemplazarDatos(fila, "CRMNALCONTACTOPOTENCIAL", llaves = list(CodContacto = cod_contacto))
}

cargar_potencial_contacto <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALCONTACTOPOTENCIAL") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame())
}

registrar_sucursal_contacto <- function(cod_contacto, nombre, pais, depto, mpio, direccion,
                                        lat, lng, es_principal, usr) {
  if (isTRUE(es_principal)) {
    sucursales <- CargarDatos("CRMNALCONTACTOSUCURSAL") %>%
      filter(CodContacto == cod_contacto, EsPrincipal == 1)
    if (nrow(sucursales) > 0) {
      for (id in sucursales$IdSucursal) {
        fila_desmarcada <- sucursales %>% filter(IdSucursal == id) %>% mutate(EsPrincipal = 0)
        ReemplazarDatos(fila_desmarcada, "CRMNALCONTACTOSUCURSAL", llaves = list(IdSucursal = id))
      }
    }
  }
  fila <- data.frame(IdSucursal = .generar_id_sucursal(), CodContacto = cod_contacto, Nombre = nombre,
                     Pais = pais, Depto = depto, Mpio = mpio, Direccion = direccion, lat = lat, lng = lng,
                     EsPrincipal = as.numeric(es_principal), UsuarioCrea = usr, FechaHoraCrea = Sys.time(),
                     stringsAsFactors = FALSE)
  AgregarDatos(fila, "CRMNALCONTACTOSUCURSAL")
}

listar_sucursales_contacto <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALCONTACTOSUCURSAL") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame(IdSucursal = character(), Nombre = character(), Depto = character(),
                                    Mpio = character(), Direccion = character(), EsPrincipal = numeric()))
}

eliminar_sucursal_contacto <- function(id_sucursal) {
  vacio <- CargarDatos("CRMNALCONTACTOSUCURSAL") %>% filter(FALSE)
  ReemplazarDatos(vacio, "CRMNALCONTACTOSUCURSAL", llaves = list(IdSucursal = id_sucursal))
}

# Modulos Auxiliares ----

## CapturaDireccion ----

CapturaDireccionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, ListaDesplegable(ns("CD_Pais"), label = h6("País"),
                                 choices = Choices()$paises, selected = "COLOMBIA", multiple = FALSE)),
      column(6, ListaDesplegable(ns("CD_Depto"), label = h6("Departamento"),
                                 choices = Choices()$deptos, selected = NULL, multiple = FALSE))
    ),
    ListaDesplegable(ns("CD_Mpio"), label = h6("Municipio"), choices = "", selected = "", multiple = FALSE),
    textAreaInput(ns("CD_Direccion"), label = h6("Dirección"), value = "",
                  placeholder = "Ingrese la dirección completa", width = "100%", height = "60px"),
    div(style = "text-align: right;",
        actionBttn(ns("CD_Validar"), label = "Validar Dirección",
                   style = "bordered", color = "primary", size = "xs", icon = icon("map-pin"))),
    uiOutput(ns("CD_Resultado"))
  )
}

CapturaDireccion <- function(id, valores_iniciales = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    coordenadas <- reactiveVal(NULL)
    
    observeEvent(valores_iniciales(), {
      v <- valores_iniciales()
      req(!is.null(v))
      updatePickerInput(session, "CD_Pais", selected = v$Pais %||% "COLOMBIA")
      updatePickerInput(session, "CD_Depto", selected = v$Depto)
      updateTextAreaInput(session, "CD_Direccion", value = v$Direccion %||% "")
      if (!is.na(v$lat %||% NA) && !is.na(v$lng %||% NA)) coordenadas(list(lat = v$lat, lng = v$lng))
    })
    
    observeEvent(input$CD_Depto, {
      req(input$CD_Depto)
      cho <- c("", Unicos(CargarDatos("ANDIVIPOLA") %>% filter(NomDep == input$CD_Depto) %>% pull(Mun)))
      sel <- if (!is.null(valores_iniciales()) && !is.null(valores_iniciales()$Mpio)) valores_iniciales()$Mpio else cho[1]
      updatePickerInput(session, "CD_Mpio", choices = cho, selected = sel)
    })
    
    observeEvent(input$CD_Validar, {
      req(input$CD_Direccion, input$CD_Mpio, input$CD_Depto)
      resultado <- geocodificar_direccion(input$CD_Direccion, input$CD_Mpio, input$CD_Depto, input$CD_Pais)
      if (is.null(resultado)) {
        coordenadas(NULL)
        showNotification("No se pudo validar la dirección con OpenStreetMap", type = "warning", duration = 4)
      } else {
        coordenadas(resultado)
        showNotification("Dirección validada exitosamente", type = "message", duration = 3)
      }
    })
    
    output$CD_Resultado <- renderUI({
      c <- coordenadas()
      if (is.null(c)) return(NULL)
      tags$div(style = "margin-top:6px;",
               FormatearTexto(paste0("✓ ", c$direccion_formateada %||% "Coordenadas obtenidas"),
                              tamano_pct = 0.75, color = "#198754"))
    })
    
    reactive({
      list(Pais = input$CD_Pais, Depto = input$CD_Depto, Mpio = input$CD_Mpio, Direccion = input$CD_Direccion,
           lat = coordenadas()$lat %||% NA_real_, lng = coordenadas()$lng %||% NA_real_)
    })
  })
}

## FormularioContacto ----

FormularioContactoUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    uiOutput(ns("Titulo")),
    box(title = "Datos Básicos", width = 12, collapsible = FALSE,
        materialSwitch(inputId = ns("CON_AutDatos"), label = Obligatorio("Autoriza Tratamiento de Datos"),
                       value = TRUE, status = "danger", width = "100%")),
    conditionalPanel(
      condition = paste0("input['", ns("CON_AutDatos"), "'] == true"),
      box(title = "Identificación", width = 12, collapsible = FALSE,
          fluidRow(
            column(6, textInput(ns("CON_RazSoc"), label = h6("Razón Social"), width = "100%")),
            column(6, textInput(ns("CON_NIT"), label = h6("NIT"),
                                placeholder = "Sin dígito de verificación", width = "100%"))
          ),
          fluidRow(column(12, FormatearTexto("* Debe diligenciar Razón Social, NIT o ambos",
                                             tamano_pct = 0.75, color = "grey"))),
          fluidRow(
            column(6, uiOutput(ns("CON_RazSoc_VAL"))),
            column(6, uiOutput(ns("CON_NIT_VAL")))
          ),
          ListaDesplegable(ns("CON_Origen"), label = Obligatorio("Origen del Contacto"),
                           choices = Choices()$origen, selected = NULL, multiple = FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("CON_Origen"), "'] == 'DIGITAL' || ",
                               "input['", ns("CON_Origen"), "'] == 'PRESENCIAL'"),
            ListaDesplegable(ns("CON_Origen_Med"), label = Obligatorio("Detalle del Origen"),
                             choices = "", selected = "", multiple = FALSE))
      ),
      div(style = "text-align: right; margin-top: 10px;",
          div(style = "display: inline-block; width: 20%;",
              actionBttn(inputId = ns("CON_Guardar"), label = "Guardar Contacto",
                         style = "unite", color = "danger", size = "xs", icon = icon("save"), block = TRUE)))
    )
  )
}

FormularioContacto <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ns           <- session$ns
    ret          <- reactiveVal(0)
    cod_guardado <- reactiveVal(NULL)
    
    es_edicion <- reactive(!is.null(cod_contacto()) && nzchar(cod_contacto()))
    
    data_contacto_edit <- reactive({
      req(es_edicion(), cod_contacto())
      CargarDatos("CRMNALCONTACTO") %>% filter(CodContacto == cod_contacto())
    })
    
    output$Titulo <- renderUI({
      if (!es_edicion()) h4("Nuevo Contacto")
      else h4(paste0(data_contacto_edit()$CodContacto, " - ", data_contacto_edit()$PerRazSoc))
    })
    
    output$CON_RazSoc_VAL <- renderUI({
      cod_actual <- if (es_edicion()) cod_contacto() else NULL
      validar_razon_social(input$CON_RazSoc, cod_actual = cod_actual)
    })
    
    output$CON_NIT_VAL <- renderUI({
      cod_actual <- if (es_edicion()) cod_contacto() else NULL
      validar_nit(input$CON_NIT, cod_actual = cod_actual)
    })
    
    observeEvent(data_contacto_edit(), {
      if (nrow(data_contacto_edit()) > 0) {
        updateTextInput(session = session, inputId = "CON_RazSoc", value = data_contacto_edit()$PerRazSoc)
        updateTextInput(session = session, inputId = "CON_NIT", value = data_contacto_edit()$PerCod)
        updateMaterialSwitch(session = session, inputId = "CON_AutDatos",
                             value = data_contacto_edit()$AutorizaTD == "SI")
        updatePickerInput(session = session, inputId = "CON_Origen", selected = data_contacto_edit()$Origen)
        disable("CON_RazSoc")
      }
    })
    
    observeEvent(input$CON_Origen, {
      req(input$CON_Origen)
      t1 <- tryCatch(
        CargarDatos("CRMNALORILEAD") %>% filter(Estado == "A", Origen == input$CON_Origen),
        error = function(e) NULL
      )
      cho <- if (!is.null(t1) && input$CON_Origen %in% c("PRESENCIAL", "DIGITAL")) Unicos(t1$Detalle) else ""
      updatePickerInput(session, "CON_Origen_Med", choices = cho, selected = NULL)
    })
    
    validar_campos_contacto <- function() {
      nit_input    <- input$CON_NIT
      nit_original <- if (es_edicion()) data_contacto_edit()$PerCod else NA
      nit_cambio   <- !EsVacio(nit_input) && (!es_edicion() || nit_input != nit_original)
      
      contactos <- CargarDatos("CRMNALCONTACTO")
      if (es_edicion()) contactos <- contactos %>% filter(CodContacto != cod_contacto())
      
      c(
        "Debe diligenciar Razón Social, NIT o ambos" = EsVacio(input$CON_RazSoc) && EsVacio(nit_input),
        "El campo Origen es obligatorio" = EsVacio(input$CON_Origen),
        "El campo detalle del origen del contacto es obligatorio" =
          input$CON_Origen %in% c("DIGITAL", "PRESENCIAL") & EsVacio(input$CON_Origen_Med),
        "El NIT ya existe como contacto activo" = nit_cambio && nit_input %in% contactos$PerCod,
        "El NIT ya existe en CRMNALMARLOT" = nit_cambio && nit_input %in% CargarDatos("CRMNALMARLOT")$CLIENTE
      )
    }
    
    limpiar_formulario_contacto <- function() {
      updateTextInput(session = session, inputId = "CON_RazSoc", value = "")
      updateTextInput(session = session, inputId = "CON_NIT", value = "")
      updateMaterialSwitch(session = session, inputId = "CON_AutDatos", value = TRUE)
      updatePickerInput(session = session, inputId = "CON_Origen", selected = "")
      updatePickerInput(session = session, inputId = "CON_Origen_Med", choices = "", selected = "")
      enable("CON_RazSoc")
    }
    
    observeEvent(input$CON_Guardar, {
      cond <- validar_campos_contacto()
      if (any(cond)) {
        sapply(names(cond), function(x) if (cond[[x]]) showNotification(x, duration = 4, type = "error"))
        return(invisible(NULL))
      }
      cod_actual   <- if (es_edicion()) cod_contacto() else NULL
      match_exacto <- buscar_contacto_por_razon_social(input$CON_RazSoc, cod_actual = cod_actual)
      
      texto_confirmacion <- if (!is.null(match_exacto)) {
        paste0("Ya existe un contacto con esta razón social (NIT: ", match_exacto$nit %||% "sin NIT",
               "). ¿Deseas continuar y guardar de todas formas?")
      } else if (es_edicion()) "¿Deseas guardar los cambios de este contacto?"
      else "¿Deseas crear este contacto?"
      
      MostrarModalConClase(ModalConfirmarGuardadoContacto(ns, texto_confirmacion, es_edicion()), "aviso")
    })
    
    observeEvent(input$CON_CancelarGuardado, { removeModal() })
    
    observeEvent(input$CON_ConfirmarGuardado, {
      codigo <- if (es_edicion()) cod_contacto() else generar_codigo_contacto()
      
      fila <- data.frame(
        CodContacto   = codigo,
        UsuarioCrea   = if (es_edicion()) data_contacto_edit()$UsuarioCrea else usr(),
        FechaHoraCrea = if (es_edicion()) data_contacto_edit()$FechaHoraCrea else Sys.time(),
        UsuarioMod    = usr(), FechaHoraModi = Sys.time(),
        AutorizaTD    = ifelse(input$CON_AutDatos, "SI", "NO"),
        PerRazSoc     = input$CON_RazSoc, PerCod = input$CON_NIT,
        Origen        = input$CON_Origen, DetOrigen = input$CON_Origen_Med,
        Estado        = if (es_edicion()) data_contacto_edit()$Estado else "ACTIVO",
        stringsAsFactors = FALSE
      ) %>% mutate(across(where(is.character), ~ str_to_upper(ifelse(. == "", NA_character_, .))))
      
      tryCatch({
        if (es_edicion()) {
          ReemplazarDatos(fila, "CRMNALCONTACTO", llaves = list(CodContacto = codigo))
          showNotification("Contacto modificado exitosamente", duration = 4, type = "message")
        } else {
          AgregarDatos(fila, "CRMNALCONTACTO")
          showNotification(paste("Contacto creado:", codigo), duration = 5, type = "message")
        }
        removeModal()
        if (!es_edicion()) limpiar_formulario_contacto()
        cod_guardado(codigo)
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al guardar el contacto:", conditionMessage(e)), duration = 6, type = "error")
      })
    })
    
    list(n = reactive(ret()), codigo = reactive(cod_guardado()))
  })
}

## FormularioAscenderLead ----

FormularioAscenderLeadUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    box(title = "Ascender a Lead", width = 12, collapsible = FALSE,
        ListaDesplegable(ns("ASC_Asesor"), label = Obligatorio("Asesor"),
                         choices = Choices()$personas, selected = NULL, multiple = FALSE),
        ListaDesplegable(ns("ASC_Segmento"), label = Obligatorio("Segmento"),
                         choices = Choices()$segmento, selected = NULL, multiple = FALSE),
        ListaDesplegable(ns("ASC_LinNeg"), label = Obligatorio("Línea de Negocio"),
                         choices = Choices()$linneg, selected = NULL, multiple = FALSE)),
    div(style = "text-align: right; margin-top: 10px;",
        div(style = "display: inline-block; width: 40%;",
            actionBttn(ns("ASC_Solicitar"), label = "Ascender a Lead",
                       style = "unite", color = "success", size = "xs", icon = icon("arrow-up"), block = TRUE)))
  )
}

FormularioAscenderLead <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ns  <- session$ns
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
    
    validar_campos_ascenso <- function() {
      c(
        "El campo Asesor es obligatorio" = EsVacio(input$ASC_Asesor),
        "El campo Segmento es obligatorio" = EsVacio(input$ASC_Segmento),
        "El campo Línea de Negocio es obligatorio" = EsVacio(input$ASC_LinNeg)
      )
    }
    
    observeEvent(input$ASC_Solicitar, {
      cond <- validar_campos_ascenso()
      if (any(cond)) {
        sapply(names(cond), function(x) if (cond[[x]]) showNotification(x, duration = 4, type = "error"))
        return(invisible(NULL))
      }
      MostrarModalConClase(
        modalDialog(
          title = "Confirmar ascenso a Lead", easyClose = FALSE,
          footer = tagList(
            racafeShiny::Boton(ns("ASC_Cancelar"), label = "Cancelar", icono = "xmark",
                               color_fondo = "transparent", color_fuente = "#6c757d"),
            racafeShiny::Boton(ns("ASC_Confirmar"), label = "Ascender a Lead",
                               icono = "arrow-up", color_fondo = "#198754")
          ),
          tags$p(paste0("¿Deseas ascender este contacto a Lead con asesor ", input$ASC_Asesor,
                        ", segmento ", input$ASC_Segmento, " y línea de negocio ", input$ASC_LinNeg, "?"))
        ), "aviso"
      )
    })
    
    observeEvent(input$ASC_Cancelar, { removeModal() })
    
    observeEvent(input$ASC_Confirmar, {
      tryCatch({
        convertir_contacto_a_lead(cod_contacto(), input$ASC_Asesor, input$ASC_Segmento, input$ASC_LinNeg, usr())
        removeModal()
        showNotification("Contacto ascendido a Lead exitosamente", duration = 4, type = "message")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al ascender el contacto:", conditionMessage(e)), duration = 6, type = "error")
      })
    })
    
    list(n = reactive(ret()))
  })
}

## FormularioDescartarContacto ----

.RAZONES_DESCARTE_CONTACTO <- c(
  "NO AUTORIZA TRATAMIENTO DE DATOS", "SIN INTERÉS COMERCIAL",
  "DATOS INCOMPLETOS O ERRÓNEOS", "DUPLICADO", "OTRAS"
)

FormularioDescartarContactoUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    box(title = "Descartar Contacto", width = 12, collapsible = FALSE,
        ListaDesplegable(ns("DES_Razon"), label = Obligatorio("Razón de Descarte"),
                         choices = .RAZONES_DESCARTE_CONTACTO, selected = NULL, multiple = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("DES_Razon"), "'] == 'OTRAS'"),
          textAreaInput(ns("DES_RazonOtra"), label = Obligatorio("Especifique la razón"),
                        value = "", placeholder = "Describa el motivo del descarte", width = "100%", height = "80px"))),
    div(style = "text-align: right; margin-top: 10px;",
        div(style = "display: inline-block; width: 40%;",
            actionBttn(ns("DES_Solicitar"), label = "Descartar Contacto",
                       style = "unite", color = "danger", size = "xs", icon = icon("ban"), block = TRUE)))
  )
}

FormularioDescartarContacto <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ns  <- session$ns
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
    
    motivo_final <- reactive({
      if (identical(input$DES_Razon, "OTRAS")) trimws(input$DES_RazonOtra %||% "") else input$DES_Razon
    })
    
    observeEvent(input$DES_Solicitar, {
      cond <- c(
        "El campo Razón de Descarte es obligatorio" = EsVacio(input$DES_Razon),
        "Debe especificar la razón cuando selecciona OTRAS" =
          identical(input$DES_Razon, "OTRAS") && EsVacio(input$DES_RazonOtra)
      )
      if (any(cond)) {
        sapply(names(cond), function(x) if (cond[[x]]) showNotification(x, duration = 4, type = "error"))
        return(invisible(NULL))
      }
      MostrarModalConClase(
        modalDialog(
          title = "Confirmar descarte", easyClose = FALSE,
          footer = tagList(
            racafeShiny::Boton(ns("DES_Cancelar"), label = "Cancelar", icono = "xmark",
                               color_fondo = "transparent", color_fuente = "#6c757d"),
            racafeShiny::Boton(ns("DES_Confirmar"), label = "Descartar Contacto",
                               icono = "ban", color_fondo = "#C11007")
          ),
          tags$p(paste0("¿Deseas descartar este contacto? Motivo: ", motivo_final()))
        ), "aviso"
      )
    })
    
    observeEvent(input$DES_Cancelar, { removeModal() })
    
    observeEvent(input$DES_Confirmar, {
      tryCatch({
        descartar_generico(cod_contacto(), "CONTACTO", motivo_final(), usr())
        removeModal()
        showNotification("Contacto descartado exitosamente", duration = 4, type = "message")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al descartar el contacto:", conditionMessage(e)), duration = 6, type = "error")
      })
    })
    
    list(n = reactive(ret()))
  })
}

## GestionContacto ----

GestionContactoUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    fluidRow(
      column(6,
             h5("Nueva Gestión"),
             ListaDesplegable(ns("GES_Tipo"), label = Obligatorio("Tipo de Gestión"),
                              choices = c("Comentario", "Llamada", "Visita", "Reunión", "Correo"),
                              selected = NULL, multiple = FALSE),
             textAreaInput(ns("GES_Comentario"), label = Obligatorio("Comentario"), value = "",
                           placeholder = "Describa la gestión realizada", width = "100%", height = "100px"),
             actionBttn(ns("GES_Guardar"), label = "Guardar Gestión",
                        style = "unite", color = "danger", size = "xs", icon = icon("save"), block = TRUE)),
      column(6, h5("Historial de Gestiones"), uiOutput(ns("GES_Timeline")))
    ),
    tags$hr(style = "border-color: grey;"),
    h5("Programar Recordatorio"),
    fluidRow(
      column(4, ListaDesplegable(ns("REC_Asesor"), label = Obligatorio("Asesor"),
                                 choices = Choices()$personas, selected = NULL, multiple = FALSE)),
      column(4, airDatepickerInput(ns("REC_Fecha"), label = Obligatorio("Fecha y Hora"),
                                   minDate = Sys.time(), timepicker = TRUE,
                                   timepickerOpts = timepickerOptions(minHours = 7, maxHours = 17), width = "100%")),
      column(4, br(), actionBttn(ns("REC_Programar"), label = "Programar Recordatorio",
                                 style = "unite", color = "danger", size = "xs", icon = icon("calendar"), block = TRUE))
    ),
    textAreaInput(ns("REC_Mensaje"), label = Obligatorio("Mensaje"), value = "",
                  placeholder = "Ingrese el mensaje del recordatorio", width = "100%", height = "80px"),
    h6("Recordatorios Programados"), uiOutput(ns("REC_Lista"))
  )
}

GestionContacto <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    data_contacto <- reactive({
      req(cod_contacto())
      CargarDatos("CRMNALCONTACTO") %>% filter(CodContacto == cod_contacto())
    })
    
    identificador <- reactive({
      req(nrow(data_contacto()) > 0)
      data_contacto()$PerRazSoc %||% data_contacto()$PerCod
    })
    
    output$Titulo <- renderUI({
      req(nrow(data_contacto()) > 0)
      h4(paste0("Gestión Comercial — ", data_contacto()$CodContacto, " - ", identificador()))
    })
    
    gestion_trigger <- reactiveVal(0)
    
    output$GES_Timeline <- renderUI({
      gestion_trigger()
      dat <- listar_gestion_comercial(cod_contacto())
      if (nrow(dat) == 0) return(tags$p("Sin gestiones registradas aún.", style = "color:#94A3B8; font-size:12px;"))
      tagList(lapply(seq_len(nrow(dat)), function(i) {
        tags$div(style = "margin-bottom:8px; padding:6px; border-left:3px solid #1C398E; background-color:#f4f6f8;",
                 tags$div(style = "font-size:0.8em; color:#5a6370;",
                          HTML(.badge_tipo_gestion(dat$TipoGestion[i])), " — ",
                          tags$strong(dat$Usuario[i]), " — ", format(dat$FechaHora[i], "%d/%m/%Y %H:%M")),
                 tags$div(dat$Comentario[i]))
      }))
    })
    
    observeEvent(input$GES_Guardar, {
      cond <- c("El tipo de gestión es obligatorio" = EsVacio(input$GES_Tipo),
                "El comentario es obligatorio" = EsVacio(input$GES_Comentario))
      if (any(cond)) {
        sapply(names(cond), function(x) if (cond[[x]]) showNotification(x, duration = 4, type = "error"))
        return(invisible(NULL))
      }
      tryCatch({
        registrar_gestion_comercial(cod_contacto(), input$GES_Tipo, input$GES_Comentario, usr())
        updateTextAreaInput(session, "GES_Comentario", value = "")
        gestion_trigger(gestion_trigger() + 1)
        showNotification("Gestión registrada exitosamente", duration = 3, type = "message")
      }, error = function(e) showNotification(paste("Error al registrar la gestión:", conditionMessage(e)),
                                              duration = 6, type = "error"))
    })
    
    recordatorio_trigger <- reactiveVal(0)
    
    output$REC_Lista <- renderUI({
      recordatorio_trigger()
      dat <- listar_recordatorios_contacto(cod_contacto())
      if (nrow(dat) == 0) return(tags$p("Sin recordatorios programados.", style = "color:#94A3B8; font-size:12px;"))
      tagList(lapply(seq_len(nrow(dat)), function(i) {
        estado <- if (isTRUE(dat$Enviado[i] == 1)) "Enviado" else "Pendiente"
        tags$div(style = "margin-bottom:6px; padding:6px; border-left:3px solid #c87000; background-color:#f9f6f0;",
                 tags$div(style = "font-size:0.8em; color:#5a6370;",
                          tags$strong(dat$Asesor[i]), " — ", format(dat$FechaRecordatorio[i], "%d/%m/%Y %H:%M"),
                          " — ", estado),
                 tags$div(dat$Mensaje[i]))
      }))
    })
    
    observeEvent(input$REC_Programar, {
      cond <- c("El Asesor es obligatorio" = EsVacio(input$REC_Asesor),
                "La fecha y hora son obligatorias" = is.null(input$REC_Fecha),
                "El mensaje es obligatorio" = EsVacio(input$REC_Mensaje))
      if (any(cond)) {
        sapply(names(cond), function(x) if (cond[[x]]) showNotification(x, duration = 4, type = "error"))
        return(invisible(NULL))
      }
      tryCatch({
        registrar_recordatorio_contacto(cod_contacto(), input$REC_Asesor, input$REC_Fecha, input$REC_Mensaje, usr())
        updateTextAreaInput(session, "REC_Mensaje", value = "")
        recordatorio_trigger(recordatorio_trigger() + 1)
        showNotification("Recordatorio programado exitosamente", duration = 3, type = "message")
      }, error = function(e) showNotification(paste("Error al programar el recordatorio:", conditionMessage(e)),
                                              duration = 6, type = "error"))
    })
  })
}

## TabPersonasContacto ----

TabPersonasContactoUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "text-align: right; margin-bottom: 10px;",
        actionBttn(ns("PER_Anadir"), label = "Añadir Persona",
                   style = "bordered", color = "success", size = "xs", icon = icon("user-plus"))),
    shinyjs::hidden(
      div(id = ns("PER_Panel"), style = "border:1px solid #E2E8F0; border-radius:6px; padding:12px; margin-bottom:14px;",
          fluidRow(
            column(6, textInput(ns("PER_Nombre"), label = Obligatorio("Nombre"), width = "100%")),
            column(6, textInput(ns("PER_Cargo"), label = h6("Cargo"), width = "100%"))),
          fluidRow(
            column(6, textInput(ns("PER_Telefono"), label = h6("Teléfono"), width = "100%")),
            column(6, textInput(ns("PER_Correo"), label = h6("Correo"), width = "100%"))),
          tags$hr(style = "border-color: grey;"),
          h6("Redes Sociales"),
          fluidRow(
            column(5, ListaDesplegable(ns("PER_TipoRed"), label = NULL,
                                       choices = .TIPOS_RED_SOCIAL, selected = NULL, multiple = FALSE)),
            column(5, textInput(ns("PER_UsuarioRed"), label = NULL, placeholder = "Usuario o enlace", width = "100%")),
            column(2, actionBttn(ns("PER_AgregarRed"), label = NULL, icon = icon("plus"),
                                 style = "bordered", color = "primary", size = "xs"))),
          uiOutput(ns("PER_RedesTemp")),
          div(style = "text-align: right; margin-top: 10px;",
              actionBttn(ns("PER_Cancelar"), label = "Cancelar", style = "bordered", color = "default", size = "xs"),
              actionBttn(ns("PER_Guardar"), label = "Guardar Persona", style = "unite", color = "danger", size = "xs",
                         icon = icon("save"))))
    ),
    TablaReactableUI(ns("tabla_personas"), titulo = "Personas de Contacto")
  )
}

TabPersonasContacto <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    redes_temporales <- reactiveVal(list())
    
    data_personas <- reactive({
      refresh_trigger()
      req(cod_contacto())
      listar_personas_contacto(cod_contacto()) %>% mutate(Eliminar = IdPersona)
    })
    
    mod_tabla <- TablaReactable(
      id = "tabla_personas", data = data_personas, columnas = NULL,
      col_specs = list(
        Eliminar = .coldef_accion("Eliminar", "trash", "#C11007"),
        Nombre   = reactable::colDef(name = "Nombre", minWidth = 120),
        Cargo    = reactable::colDef(name = "Cargo", minWidth = 100),
        Telefono = reactable::colDef(name = "Teléfono", minWidth = 100),
        Correo   = reactable::colDef(name = "Correo", minWidth = 140),
        RedesResumen = reactable::colDef(name = "Redes Sociales", minWidth = 180),
        IdPersona    = reactable::colDef(show = FALSE)
      ),
      modo_seleccion = "celda", id_col = "IdPersona", cols_activos = "Eliminar",
      sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    observeEvent(mod_tabla$seleccion(), {
      sel <- mod_tabla$seleccion()
      req(sel, sel$col == "Eliminar")
      tryCatch({
        eliminar_persona_contacto(sel$fila$IdPersona[[1]])
        refresh_trigger(isolate(refresh_trigger()) + 1)
        showNotification("Persona eliminada", duration = 3, type = "message")
      }, error = function(e) showNotification(paste("Error al eliminar:", conditionMessage(e)), duration = 5, type = "error"))
    })
    
    observeEvent(input$PER_Anadir, {
      if (nrow(data_personas()) >= .MAX_PERSONAS_CONTACTO) {
        showNotification(paste0("Máximo ", .MAX_PERSONAS_CONTACTO, " personas de contacto"), type = "warning", duration = 4)
        return(invisible(NULL))
      }
      redes_temporales(list())
      shinyjs::show("PER_Panel")
    })
    
    observeEvent(input$PER_Cancelar, { shinyjs::hide("PER_Panel") })
    
    observeEvent(input$PER_AgregarRed, {
      if (EsVacio(input$PER_TipoRed) || EsVacio(input$PER_UsuarioRed)) {
        showNotification("Seleccione el tipo de red y el usuario/enlace", type = "warning", duration = 3)
        return(invisible(NULL))
      }
      nuevas <- redes_temporales()
      nuevas[[length(nuevas) + 1]] <- list(tipo = input$PER_TipoRed, usuario = input$PER_UsuarioRed)
      redes_temporales(nuevas)
      updateTextInput(session, "PER_UsuarioRed", value = "")
    })
    
    output$PER_RedesTemp <- renderUI({
      redes <- redes_temporales()
      if (length(redes) == 0) return(NULL)
      tagList(lapply(seq_along(redes), function(i) {
        tags$span(style = "display:inline-block; margin:3px 4px; padding:2px 8px; border-radius:10px; background:#EEF2FF; font-size:11px;",
                  paste0(redes[[i]]$tipo, ": ", redes[[i]]$usuario), " ",
                  actionLink(ns(paste0("PER_QuitarRed_", i)), label = icon("xmark")))
      }))
    })
    
    observe({
      n <- length(redes_temporales())
      req(n > 0)
      lapply(seq_len(n), function(i) {
        observeEvent(input[[paste0("PER_QuitarRed_", i)]], {
          nuevas <- redes_temporales()
          nuevas[[i]] <- NULL
          redes_temporales(nuevas)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    observeEvent(input$PER_Guardar, {
      if (EsVacio(input$PER_Nombre)) {
        showNotification("El nombre es obligatorio", type = "error", duration = 4)
        return(invisible(NULL))
      }
      tryCatch({
        id_persona <- registrar_persona_contacto(cod_contacto(), input$PER_Nombre, input$PER_Cargo,
                                                 input$PER_Telefono, input$PER_Correo, usr())
        for (red in redes_temporales()) registrar_red_social_persona(id_persona, red$tipo, red$usuario)
        
        shinyjs::hide("PER_Panel")
        updateTextInput(session, "PER_Nombre", value = "")
        updateTextInput(session, "PER_Cargo", value = "")
        updateTextInput(session, "PER_Telefono", value = "")
        updateTextInput(session, "PER_Correo", value = "")
        redes_temporales(list())
        refresh_trigger(isolate(refresh_trigger()) + 1)
        showNotification("Persona de contacto agregada", duration = 3, type = "message")
      }, error = function(e) showNotification(paste("Error al guardar:", conditionMessage(e)), duration = 5, type = "error"))
    })
  })
}

## TabUbicacion ----

TabUbicacionUI <- function(id) {
  ns <- NS(id)
  tagList(
    CapturaDireccionUI(ns("direccion")),
    div(style = "text-align: right; margin-top: 10px;",
        actionBttn(ns("UBI_Guardar"), label = "Guardar Ubicación",
                   style = "unite", color = "danger", size = "xs", icon = icon("save")))
  )
}

TabUbicacion <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ubicacion_existente <- reactive({
      req(cod_contacto())
      dat <- cargar_ubicacion_contacto(cod_contacto())
      if (nrow(dat) == 0) return(NULL)
      as.list(dat[1, ])
    })
    
    direccion_mod <- CapturaDireccion("direccion", valores_iniciales = ubicacion_existente)
    
    observeEvent(input$UBI_Guardar, {
      d <- direccion_mod()
      tryCatch({
        guardar_ubicacion_contacto(cod_contacto(), d$Pais, d$Depto, d$Mpio, d$Direccion, d$lat, d$lng, usr())
        showNotification("Ubicación guardada exitosamente", duration = 3, type = "message")
      }, error = function(e) showNotification(paste("Error al guardar la ubicación:", conditionMessage(e)),
                                              duration = 5, type = "error"))
    })
  })
}

## TabPotencial ----

TabPotencialUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "Potencial de Negocio", width = 12, collapsible = FALSE,
        materialSwitch(ns("POT_Tostador"), label = "¿Es tostador?", value = FALSE, status = "danger"),
        materialSwitch(ns("POT_Alianza"), label = "¿Tiene alianza con tostadora?", value = FALSE, status = "danger"),
        conditionalPanel(condition = paste0("input['", ns("POT_Alianza"), "']"),
                         textInput(ns("POT_DetalleAlianza"), label = h6("¿Con cuál?"), width = "100%")),
        materialSwitch(ns("POT_Maquila"), label = "¿Maquila?", value = FALSE, status = "danger"),
        conditionalPanel(condition = paste0("input['", ns("POT_Maquila"), "']"),
                         textInput(ns("POT_DetalleMaquila"), label = h6("Detalle de maquila"), width = "100%")),
        textAreaInput(ns("POT_Observaciones"), label = h6("Observaciones"), value = "", width = "100%", height = "70px"),
        div(style = "text-align: right;",
            actionBttn(ns("POT_Guardar"), label = "Guardar Potencial",
                       style = "unite", color = "danger", size = "xs", icon = icon("save")))),
    box(title = "Sucursales", width = 12, collapsible = FALSE,
        div(style = "text-align: right; margin-bottom: 10px;",
            actionBttn(ns("SUC_Anadir"), label = "Añadir Sucursal",
                       style = "bordered", color = "success", size = "xs", icon = icon("plus"))),
        shinyjs::hidden(
          div(id = ns("SUC_Panel"), style = "border:1px solid #E2E8F0; border-radius:6px; padding:12px; margin-bottom:14px;",
              textInput(ns("SUC_Nombre"), label = Obligatorio("Nombre de la Sucursal"), width = "100%"),
              CapturaDireccionUI(ns("suc_direccion")),
              materialSwitch(ns("SUC_EsPrincipal"), label = "Marcar como sede principal/administrativa",
                             value = FALSE, status = "danger"),
              div(style = "text-align: right; margin-top: 10px;",
                  actionBttn(ns("SUC_Cancelar"), label = "Cancelar", style = "bordered", color = "default", size = "xs"),
                  actionBttn(ns("SUC_Guardar"), label = "Guardar Sucursal", style = "unite", color = "danger", size = "xs",
                             icon = icon("save"))))
        ),
        TablaReactableUI(ns("tabla_sucursales"), titulo = "Sucursales registradas"))
  )
}

TabPotencial <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    
    observeEvent(cod_contacto(), {
      req(cod_contacto())
      dat <- cargar_potencial_contacto(cod_contacto())
      if (nrow(dat) == 0) return(invisible(NULL))
      updateMaterialSwitch(session, "POT_Tostador", value = dat$EsTostador[[1]] == 1)
      updateMaterialSwitch(session, "POT_Alianza", value = dat$AlianzaTostadora[[1]] == 1)
      updateTextInput(session, "POT_DetalleAlianza", value = dat$DetalleAlianza[[1]] %||% "")
      updateMaterialSwitch(session, "POT_Maquila", value = dat$Maquila[[1]] == 1)
      updateTextInput(session, "POT_DetalleMaquila", value = dat$DetalleMaquila[[1]] %||% "")
      updateTextAreaInput(session, "POT_Observaciones", value = dat$Observaciones[[1]] %||% "")
    })
    
    observeEvent(input$POT_Guardar, {
      tryCatch({
        guardar_potencial_contacto(cod_contacto(), as.numeric(input$POT_Tostador), as.numeric(input$POT_Alianza),
                                   input$POT_DetalleAlianza, as.numeric(input$POT_Maquila), input$POT_DetalleMaquila,
                                   input$POT_Observaciones, usr())
        showNotification("Potencial de negocio guardado", duration = 3, type = "message")
      }, error = function(e) showNotification(paste("Error al guardar:", conditionMessage(e)), duration = 5, type = "error"))
    })
    
    data_sucursales <- reactive({
      refresh_trigger()
      req(cod_contacto())
      listar_sucursales_contacto(cod_contacto()) %>% mutate(Eliminar = IdSucursal, Principal = ifelse(EsPrincipal == 1, "Sí", "No"))
    })
    
    suc_direccion_mod <- CapturaDireccion("suc_direccion")
    
    mod_tabla_suc <- TablaReactable(
      id = "tabla_sucursales", data = data_sucursales, columnas = NULL,
      col_specs = list(
        Eliminar   = .coldef_accion("Eliminar", "trash", "#C11007"),
        Nombre     = reactable::colDef(name = "Nombre", minWidth = 130),
        Depto      = reactable::colDef(name = "Departamento", minWidth = 110),
        Mpio       = reactable::colDef(name = "Municipio", minWidth = 110),
        Direccion  = reactable::colDef(name = "Dirección", minWidth = 180),
        Principal  = reactable::colDef(name = "Sede Principal/Admin.", minWidth = 130),
        IdSucursal = reactable::colDef(show = FALSE), EsPrincipal = reactable::colDef(show = FALSE)
      ),
      modo_seleccion = "celda", id_col = "IdSucursal", cols_activos = "Eliminar",
      sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    observeEvent(mod_tabla_suc$seleccion(), {
      sel <- mod_tabla_suc$seleccion()
      req(sel, sel$col == "Eliminar")
      tryCatch({
        eliminar_sucursal_contacto(sel$fila$IdSucursal[[1]])
        refresh_trigger(isolate(refresh_trigger()) + 1)
        showNotification("Sucursal eliminada", duration = 3, type = "message")
      }, error = function(e) showNotification(paste("Error al eliminar:", conditionMessage(e)), duration = 5, type = "error"))
    })
    
    observeEvent(input$SUC_Anadir, { shinyjs::show("SUC_Panel") })
    observeEvent(input$SUC_Cancelar, { shinyjs::hide("SUC_Panel") })
    
    observeEvent(input$SUC_Guardar, {
      if (EsVacio(input$SUC_Nombre)) {
        showNotification("El nombre de la sucursal es obligatorio", type = "error", duration = 4)
        return(invisible(NULL))
      }
      d <- suc_direccion_mod()
      tryCatch({
        registrar_sucursal_contacto(cod_contacto(), input$SUC_Nombre, d$Pais, d$Depto, d$Mpio,
                                    d$Direccion, d$lat, d$lng, input$SUC_EsPrincipal, usr())
        shinyjs::hide("SUC_Panel")
        updateTextInput(session, "SUC_Nombre", value = "")
        refresh_trigger(isolate(refresh_trigger()) + 1)
        showNotification("Sucursal agregada exitosamente", duration = 3, type = "message")
      }, error = function(e) showNotification(paste("Error al guardar:", conditionMessage(e)), duration = 5, type = "error"))
    })
  })
}

## EditarContacto ----

EditarContactoUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Identificación", br(), FormularioContactoUI(ns("tab_identificacion"))),
    tabPanel("Personas de Contacto", br(), TabPersonasContactoUI(ns("tab_personas"))),
    tabPanel("Ubicación Geográfica", br(), TabUbicacionUI(ns("tab_ubicacion"))),
    tabPanel("Potencial de Negocio", br(), TabPotencialUI(ns("tab_potencial")))
  )
}

EditarContacto <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    FormularioContacto(id = "tab_identificacion", usr = usr, cod_contacto = cod_contacto)
    TabPersonasContacto(id = "tab_personas", usr = usr, cod_contacto = cod_contacto)
    TabUbicacion(id = "tab_ubicacion", usr = usr, cod_contacto = cod_contacto)
    TabPotencial(id = "tab_potencial", usr = usr, cod_contacto = cod_contacto)
  })
}

## FormularioConvertirProspecto ----
# (declarada aquí porque el botón "Marcar Prospecto" vive en la tabla de
# Contactos; su lógica de datos -catalogo_clientes_aliados, etc.- vive en
# TablaProspectos.R, que debe cargarse ANTES de invocar esta UI en producción
# si se usa el botón desde este módulo)

FormularioConvertirProspectoUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    box(title = "Vincular Alianzas", width = 12, collapsible = FALSE,
        FormatearTexto("Un Prospecto no se gestiona con venta directa: se vincula a uno o varios Clientes existentes, quienes canalizarán el volumen.",
                       tamano_pct = 0.8, color = "#64748B"),
        ListaDesplegable(ns("PRO_Alianzas"), label = Obligatorio("Clientes Aliados"),
                         choices = "", selected = NULL, multiple = TRUE),
        textAreaInput(ns("PRO_Observacion"), label = h6("Observación"), value = "",
                      placeholder = "Contexto de la alianza (opcional)", width = "100%", height = "60px")),
    div(style = "text-align: right; margin-top: 10px;",
        actionBttn(ns("PRO_Solicitar"), label = "Marcar como Prospecto",
                   style = "unite", color = "success", size = "xs", icon = icon("handshake")))
  )
}

FormularioConvertirProspecto <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ns  <- session$ns
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
    
    observeEvent(cod_contacto(), {
      req(cod_contacto())
      catalogo <- catalogo_clientes_aliados()
      updatePickerInput(session, "PRO_Alianzas", choices = setNames(catalogo$CodContacto, catalogo$Etiqueta), selected = NULL)
    })
    
    observeEvent(input$PRO_Solicitar, {
      if (length(input$PRO_Alianzas) == 0) {
        showNotification("Debe vincular al menos un Cliente aliado", type = "error", duration = 4)
        return(invisible(NULL))
      }
      MostrarModalConClase(
        modalDialog(
          title = "Confirmar calificación como Prospecto", easyClose = FALSE,
          footer = tagList(
            racafeShiny::Boton(ns("PRO_Cancelar"), label = "Cancelar", icono = "xmark",
                               color_fondo = "transparent", color_fuente = "#6c757d"),
            racafeShiny::Boton(ns("PRO_Confirmar"), label = "Marcar como Prospecto",
                               icono = "handshake", color_fondo = "#198754")
          ),
          tags$p(paste0("¿Deseas marcar este contacto como Prospecto, vinculado a ",
                        length(input$PRO_Alianzas), " Cliente(s) aliado(s)?"))
        ), "aviso"
      )
    })
    
    observeEvent(input$PRO_Cancelar, { removeModal() })
    
    observeEvent(input$PRO_Confirmar, {
      tryCatch({
        convertir_contacto_a_prospecto(cod_contacto(), input$PRO_Alianzas, usr(), input$PRO_Observacion)
        removeModal()
        showNotification("Contacto marcado como Prospecto exitosamente", duration = 4, type = "message")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al marcar como Prospecto:", conditionMessage(e)), duration = 6, type = "error")
      })
    })
    
    list(n = reactive(ret()))
  })
}

# Modulo Principal ----

## TablaContactos ----

TablaContactosUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(3, actionBttn(ns("btn_nuevo_contacto"), label = "Añadir Contacto",
                           style = "unite", color = "danger", size = "sm", icon = icon("plus"), block = TRUE)),
      column(3, offset = 6, ListaDesplegable(ns("filtro_estado"), label = h6("Sub Estado"),
                                             choices = c("ACTIVO", "DESCARTADO", "TODOS"), selected = "ACTIVO", multiple = FALSE))
    ),
    br(),
    fluidRow(
      column(6, box(title = "Antigüedad de Contactos", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_antiguedad"), height = "220px"))),
      column(6, box(title = "Contactos por Canal", width = 12, collapsible = FALSE, plotly::plotlyOutput(ns("kpi_canal"), height = "220px")))
    ),
    br(),
    TablaReactableUI(ns("tabla_contactos"), titulo = "Contactos",
                     footer = "Usa el menú de Acciones de cada fila para gestionar el contacto.", footer_tipo = "info")
  )
}

TablaContactos <- function(id, usr) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    
    # Datos base: Etapa derivada desde el historial (fuente única de verdad)
    contactos_raw <- reactive({
      refresh_trigger()
      derivar_etapa_actual() %>%
        filter(Etapa == "CONTACTO") %>%
        mutate(
          FechaHoraCrea = as_datetime(FechaHoraCrea),
          DiasSinGestion = as.numeric(difftime(Sys.time(), FechaHoraCrea, units = "days")),
          RangoAntiguedad = .rangos_antiguedad(DiasSinGestion),
          Identificacion = .indicador_identificacion(PerRazSoc, PerCod)
        )
    })
    
    contactos_filtrados <- reactive({
      dat <- contactos_raw()
      if (input$filtro_estado != "TODOS") dat <- dat %>% filter(Estado == input$filtro_estado)
      dat
    })
    
    output$kpi_antiguedad <- plotly::renderPlotly({
      dat <- contactos_filtrados() %>% count(RangoAntiguedad, .drop = FALSE, name = "n")
      .grafico_barras_horizontal(dat, "RangoAntiguedad", "n", color = "#1C398E", titulo_x = "Contactos")
    })
    
    output$kpi_canal <- plotly::renderPlotly({
      dat <- contactos_filtrados() %>% mutate(Origen = ifelse(is.na(Origen), "SIN DATO", Origen)) %>% count(Origen, sort = TRUE, name = "n")
      .grafico_barras_horizontal(dat, "Origen", "n", color = "#0F6E56", titulo_x = "Contactos")
    })
    
    data_tabla <- reactive({
      contactos_filtrados() %>%
        mutate(IdentificacionBadge = sapply(Identificacion, .badge_identificacion), Acciones = CodContacto) %>%
        arrange(desc(FechaHoraCrea)) %>%
        select(Acciones, CodContacto, PerRazSoc, PerCod, IdentificacionBadge, Origen, DetOrigen,
               AutorizaTD, FechaHoraCrea, DiasSinGestion, Estado)
    })
    
    mod_tabla <- TablaReactable(
      id = "tabla_contactos", data = data_tabla, columnas = NULL,
      col_specs = list(
        Acciones = .coldef_dropdown_acciones(ns, c(editar = "Editar", comentar = "Comentar",
                                                   ascender = "Ascender a Lead", prospecto = "Marcar como Prospecto",
                                                   descartar = "Descartar")),
        CodContacto = reactable::colDef(show = FALSE),
        PerRazSoc   = reactable::colDef(name = "Razón Social", minWidth = 180),
        PerCod      = reactable::colDef(name = "NIT", minWidth = 100),
        IdentificacionBadge = reactable::colDef(name = "Identificación", html = TRUE, minWidth = 130),
        Origen      = reactable::colDef(name = "Origen", minWidth = 100),
        DetOrigen   = reactable::colDef(name = "Detalle Origen", minWidth = 110),
        AutorizaTD  = reactable::colDef(name = "Autoriza TD", minWidth = 90),
        FechaHoraCrea = reactable::colDef(name = "Fecha Creación", minWidth = 130, format = reactable::colFormat(datetime = TRUE)),
        DiasSinGestion = reactable::colDef(name = "Días sin gestión", minWidth = 120, cell = function(v) round(v, 0)),
        Estado = reactable::colDef(name = "Sub Estado", minWidth = 100)
      ),
      modo_seleccion = "ninguno", id_col = "CodContacto",
      sortable = TRUE, searchable = TRUE, page_size = 20, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = TRUE
    )
    
    ascender_cod_rv   <- reactiveVal(NULL)
    ascender_mod      <- FormularioAscenderLead(id = "mod_ascender", usr = usr, cod_contacto = reactive(ascender_cod_rv()))
    
    prospecto_cod_rv  <- reactiveVal(NULL)
    prospecto_mod     <- FormularioConvertirProspecto(id = "mod_prospecto", usr = usr, cod_contacto = reactive(prospecto_cod_rv()))
    
    descartar_cod_rv  <- reactiveVal(NULL)
    descartar_mod     <- FormularioDescartarContacto(id = "mod_descartar", usr = usr, cod_contacto = reactive(descartar_cod_rv()))
    
    gestion_cod_rv    <- reactiveVal(NULL)
    GestionContacto(id = "mod_gestion", usr = usr, cod_contacto = reactive(gestion_cod_rv()))
    
    editar_cod_rv     <- reactiveVal(NULL)
    EditarContacto(id = "mod_editar", usr = usr, cod_contacto = reactive(editar_cod_rv()))
    
    # Enruta la acción elegida en el desplegable de cada fila
    observeEvent(input$accion_tabla, {
      acc <- input$accion_tabla
      req(acc$cod_contacto, acc$accion)
      cod <- acc$cod_contacto
      
      if (acc$accion == "editar") {
        editar_cod_rv(cod)
        showModal(modalDialog(title = "Editar Contacto", size = "xl", easyClose = TRUE, footer = modalButton("Cerrar"),
                              EditarContactoUI(ns("mod_editar"))))
      } else if (acc$accion == "ascender") {
        ascender_cod_rv(cod)
        showModal(modalDialog(title = "Ascender a Lead", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioAscenderLeadUI(ns("mod_ascender"))))
      } else if (acc$accion == "prospecto") {
        prospecto_cod_rv(cod)
        showModal(modalDialog(title = "Marcar como Prospecto", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioConvertirProspectoUI(ns("mod_prospecto"))))
      } else if (acc$accion == "descartar") {
        descartar_cod_rv(cod)
        showModal(modalDialog(title = "Descartar Contacto", size = "m", easyClose = TRUE, footer = modalButton("Cerrar"),
                              FormularioDescartarContactoUI(ns("mod_descartar"))))
      } else if (acc$accion == "comentar") {
        gestion_cod_rv(cod)
        showModal(modalDialog(title = "Gestión Comercial", size = "l", easyClose = TRUE, footer = modalButton("Cerrar"),
                              GestionContactoUI(ns("mod_gestion"))))
      }
    })
    
    observeEvent(ascender_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(prospecto_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    observeEvent(descartar_mod$n(), { removeModal(); refresh_trigger(isolate(refresh_trigger()) + 1) })
    
    contacto_mod <- FormularioContacto(id = "mod_nuevo_contacto", usr = usr, cod_contacto = reactive(""))
    
    observeEvent(input$btn_nuevo_contacto, {
      showModal(modalDialog(title = "Nuevo Contacto", size = "l", easyClose = TRUE, footer = modalButton("Cerrar"),
                            FormularioContactoUI(ns("mod_nuevo_contacto"))))
    })
    
    observeEvent(contacto_mod$n(), {
      req(contacto_mod$codigo())
      removeModal()
      refresh_trigger(isolate(refresh_trigger()) + 1)
    })
  })
}

# App de prueba ----

ui <- bs4DashPage(
  title = "Prueba Tab Contactos",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(), controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    TablaContactosUI("tab_contactos")
  )
)

server <- function(input, output, session) {
  TablaContactos("tab_contactos", usr = reactive("CMEDINA"))
}

shinyApp(ui, server)
# Funciones ----

# Envuelve showModal(): abre el modal y le agrega la clase de tamaño
# indicada al .modal-dialog real vía JS — modalDialog(class=) aplica la
# clase a .modal-content, no a .modal-dialog (que es donde vive el
# max-width real en el CSS del proyecto). Copiada de GastosOficinaPpal.R;
# candidata a moverse a racafeShiny como utilidad exportada, ya que se usa
# en más de un módulo
MostrarModalConClase <- function(modal_ui, clase) {
  showModal(modal_ui)
  shinyjs::delay(0, shinyjs::runjs(
    sprintf("$('.modal-dialog').not('.%s').last().addClass('%s');", clase, clase)
  ))
}

# Función para generar código consecutivo de contacto (prefijo + fecha + consecutivo diario)
# Nota: calculada en el momento del guardado para minimizar ventana de colisión;
# candidato a reemplazar por una función de secuencia atómica en racafeBD
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
      str_remove(prefijo) %>%
      as.integer() %>%
      max(na.rm = TRUE) %>%
      {. + 1}
  }
  
  paste0(prefijo, formatC(consecutivo, width = 3, flag = "0"))
}

# Función auxiliar para validar NIT - contra CRMNALMARLOT (histórico) y
# contra CRMNALCONTACTO (evita duplicar el mismo NIT como contacto activo)
validar_nit <- function(nit_input, cod_actual = NULL) {
  
  if (is.null(nit_input) || nit_input == "" || is.na(nit_input)) return(NULL)
  
  if (!EsEnteroPositivo(nit_input)) {
    return(FormatearTexto("* El NIT debe ser un valor numérico válido", negrita = T,
                          color = "red", tamano_pct = 0.75))
  }
  
  contactos <- CargarDatos("CRMNALCONTACTO")
  if (!is.null(cod_actual)) {
    contactos <- contactos %>% filter(CodContacto != cod_actual)
  }
  
  if (nit_input %in% contactos$PerCod) {
    return(FormatearTexto("* El NIT ya existe como contacto activo", negrita = T,
                          color = "red", tamano_pct = 0.75))
  }
  
  if (nit_input %in% CargarDatos("CRMNALMARLOT")$PerCod) {
    return(FormatearTexto("* El NIT ya existe en CRMNALMARLOT", negrita = T,
                          color = "red", tamano_pct = 0.75))
  }
  
  NULL
}

# Función que busca coincidencia EXACTA de razón social contra contactos
# existentes, y retorna su NIT — usada tanto para el aviso bajo el campo
# como para el texto del modal de confirmación de guardado
buscar_contacto_por_razon_social <- function(razon_social_input, cod_actual = NULL) {
  
  if (is.null(razon_social_input) || nchar(trimws(razon_social_input)) < 1) return(NULL)
  
  razon_social_input <- str_to_upper(trimws(razon_social_input))
  
  contactos <- CargarDatos("CRMNALCONTACTO")
  if (!is.null(cod_actual)) {
    contactos <- contactos %>% filter(CodContacto != cod_actual)
  }
  
  match <- contactos %>%
    filter(str_to_upper(PerRazSoc) == razon_social_input) %>%
    slice(1)
  
  if (nrow(match) == 0) return(NULL)
  
  list(cod_contacto = match$CodContacto, nit = match$PerCod)
}

# Función para validar razón social - únicamente informativa, nunca bloquea
# el guardado (dos empresas pueden compartir razón social; el NIT es la llave)
validar_razon_social <- function(razon_social_input, cod_actual = NULL) {
  
  if (is.null(razon_social_input) || nchar(trimws(razon_social_input)) < 1) return(NULL)
  
  razon_social_upper <- str_to_upper(razon_social_input)
  
  match_exacto <- buscar_contacto_por_razon_social(razon_social_input, cod_actual)
  
  similares <- NULL
  if (nchar(razon_social_upper) >= 3) {
    ncliente_similares <- NCLIENTE %>%
      filter(grepl(razon_social_upper, str_to_upper(PerRazSoc), fixed = TRUE)) %>%
      pull(PerRazSoc) %>%
      str_to_upper() %>%
      unique()
    
    fact_similares <- FACT %>%
      filter(grepl(razon_social_upper, str_to_upper(PerRazSoc), fixed = TRUE)) %>%
      pull(PerRazSoc) %>%
      str_to_upper() %>%
      unique()
    
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
      FormatearTexto("Nombres similares encontrados:", negrita = T,
                     color = "orange", tamano_pct = 0.75),
      tags$ul(lapply(similares, function(x) tags$li(x)))
    ))
  }
  
  NULL
}

# Modal de confirmación de guardado de contacto — mismo patrón que el resto
# de modales del CRM (modalDialog + MostrarModalConClase), tamaño "aviso"
# (el más compacto, pensado para confirmaciones breves)
ModalConfirmarGuardadoContacto <- function(ns, texto, es_edicion) {
  modalDialog(
    title = "Confirmar guardado",
    easyClose = FALSE,
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

# Modulo -----

FormularioContactoUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    uiOutput(ns("Titulo")),
    box(
      title = "Datos Básicos", width = 12, collapsible = FALSE,
      materialSwitch(inputId = ns("CON_AutDatos"), label = Obligatorio("Autoriza Tratamiento de Datos"),
                     value = TRUE, status = "danger", width = "100%")
    ),
    conditionalPanel(
      condition = paste0("input['", ns("CON_AutDatos"), "'] == true"),
      box(
        title = "Identificación", width = 12, collapsible = FALSE,
        fluidRow(
          column(6, textInput(ns("CON_RazSoc"), label = h6("Razón Social"), width = "100%")),
          column(6, textInput(ns("CON_NIT"), label = h6("NIT"),
                              placeholder = "Sin dígito de verificación", width = "100%"))
        ),
        fluidRow(
          column(12,
                 FormatearTexto("* Debe diligenciar Razón Social, NIT o ambos", tamano_pct = 0.75,
                                color = "grey")
          )
        ),
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
                           choices = "", selected = "", multiple = FALSE)
        )
      ),
      div(style = "text-align: right; margin-top: 10px;",
          div(style = "display: inline-block; width: 20%;",
              actionBttn(inputId = ns("CON_Guardar"), label = "Guardar Contacto",
                         style = "unite", color = "danger", size = "xs",
                         icon = icon("save"), block = TRUE)
          )
      )
    )
  )
}

FormularioContacto <- function(id, usr, cod_contacto) {
  moduleServer(id, function(input, output, session) {
    
    ns           <- session$ns
    ret          <- reactiveVal(0)
    cod_guardado <- reactiveVal(NULL)
    
    # Modo derivado de la identidad, no de un parámetro externo — mismo
    # patrón que cli_identidad_rv/IndFormulario: cod_contacto() vacío o NULL
    # implica modo crear, con valor implica modo editar
    es_edicion <- reactive(!is.null(cod_contacto()) && nzchar(cod_contacto()))
    
    # DATOS REACTIVOS ----
    data_contacto_edit <- reactive({
      req(es_edicion(), cod_contacto())
      CargarDatos("CRMNALCONTACTO") %>%
        filter(CodContacto == cod_contacto())
    })
    
    # OUTPUTS ----
    output$Titulo <- renderUI({
      if (!es_edicion()) {
        h4("Nuevo Contacto")
      } else {
        h4(paste0(data_contacto_edit()$CodContacto, " - ", data_contacto_edit()$PerRazSoc))
      }
    })
    
    output$CON_RazSoc_VAL <- renderUI({
      cod_actual <- if (es_edicion()) cod_contacto() else NULL
      validar_razon_social(input$CON_RazSoc, cod_actual = cod_actual)
    })
    
    output$CON_NIT_VAL <- renderUI({
      cod_actual <- if (es_edicion()) cod_contacto() else NULL
      validar_nit(input$CON_NIT, cod_actual = cod_actual)
    })
    
    # CARGA DE DATOS EN MODO EDICIÓN ----
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
    
    # ACTUALIZACIÓN DINÁMICA DE CAMPOS ----
    # Tolerante a la ausencia de CRMNALORILEAD: solo se crea CRMNALCONTACTO
    # en esta etapa, por lo que el detalle de origen se degrada a lista
    # vacía en vez de romper el formulario si esa tabla aún no existe
    observeEvent(input$CON_Origen, {
      req(input$CON_Origen)
      
      t1 <- tryCatch(
        CargarDatos("CRMNALORILEAD") %>% filter(Estado == "A", Origen == input$CON_Origen),
        error = function(e) NULL
      )
      
      cho <- if (!is.null(t1) && input$CON_Origen %in% c("PRESENCIAL", "DIGITAL")) {
        Unicos(t1$Detalle)
      } else {
        ""
      }
      updatePickerInput(session, "CON_Origen_Med", choices = cho, selected = NULL)
    })
    
    # VALIDACIONES BLOQUEANTES ----
    # Razón social queda fuera: es informativa (renderUI), no bloquea guardado
    validar_campos_contacto <- function() {
      
      nit_input    <- input$CON_NIT
      nit_original <- if (es_edicion()) data_contacto_edit()$PerCod else NA
      nit_cambio   <- !EsVacio(nit_input) && (!es_edicion() || nit_input != nit_original)
      
      contactos <- CargarDatos("CRMNALCONTACTO")
      if (es_edicion()) {
        contactos <- contactos %>% filter(CodContacto != cod_contacto())
      }
      
      c(
        "Debe diligenciar Razón Social, NIT o ambos" =
          EsVacio(input$CON_RazSoc) && EsVacio(nit_input),
        "El campo Origen es obligatorio" = EsVacio(input$CON_Origen),
        "El campo detalle del origen del contacto es obligatorio" =
          input$CON_Origen %in% c("DIGITAL", "PRESENCIAL") & EsVacio(input$CON_Origen_Med),
        "El NIT ya existe como contacto activo" =
          nit_cambio && nit_input %in% contactos$PerCod,
        "El NIT ya existe en CRMNALMARLOT" =
          nit_cambio && nit_input %in% CargarDatos("CRMNALMARLOT")$PerCod
      )
    }
    
    # LIMPIEZA DEL FORMULARIO ----
    # Solo aplica en modo crear: en modo editar el formulario debe seguir
    # reflejando el estado vigente del registro tras guardar
    limpiar_formulario_contacto <- function() {
      updateTextInput(session = session, inputId = "CON_RazSoc", value = "")
      updateTextInput(session = session, inputId = "CON_NIT", value = "")
      updateMaterialSwitch(session = session, inputId = "CON_AutDatos", value = TRUE)
      updatePickerInput(session = session, inputId = "CON_Origen", selected = "")
      updatePickerInput(session = session, inputId = "CON_Origen_Med", choices = "", selected = "")
      enable("CON_RazSoc")
    }
    
    # ABRIR MODAL DE CONFIRMACIÓN ----
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
      } else if (es_edicion()) {
        "¿Deseas guardar los cambios de este contacto?"
      } else {
        "¿Deseas crear este contacto?"
      }
      
      MostrarModalConClase(
        ModalConfirmarGuardadoContacto(ns, texto_confirmacion, es_edicion()),
        "aviso"
      )
    })
    
    observeEvent(input$CON_CancelarGuardado, {
      removeModal()
    })
    
    # GUARDAR (tras confirmación) ----
    observeEvent(input$CON_ConfirmarGuardado, {
      
      codigo <- if (es_edicion()) cod_contacto() else generar_codigo_contacto()
      
      fila <- data.frame(
        CodContacto   = codigo,
        UsuarioCrea   = if (es_edicion()) data_contacto_edit()$UsuarioCrea else usr(),
        FechaHoraCrea = if (es_edicion()) data_contacto_edit()$FechaHoraCrea else Sys.time(),
        UsuarioMod    = usr(),
        FechaHoraModi = Sys.time(),
        AutorizaTD    = ifelse(input$CON_AutDatos, "SI", "NO"),
        PerRazSoc     = input$CON_RazSoc,
        PerCod        = input$CON_NIT,
        Origen        = input$CON_Origen,
        DetOrigen     = input$CON_Origen_Med,
        Estado        = if (es_edicion()) data_contacto_edit()$Estado else "ACTIVO",
        stringsAsFactors = FALSE
      ) %>%
        mutate(across(where(is.character), ~ str_to_upper(ifelse(. == "", NA_character_, .))))
      
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
        showNotification(paste("Error al guardar el contacto:", conditionMessage(e)),
                         duration = 6, type = "error")
      })
    })
    
    # RETORNO DEL MÓDULO ----
    # Expuesto como reactivos reales (no return() dentro de observeEvent,
    # que nunca llegaba a ser el valor de retorno del módulo)
    list(
      n      = reactive(ret()),
      codigo = reactive(cod_guardado())
    )
  })
}

# App de prueba ----

ui <- bs4DashPage(
  title = "Prueba Formulario Contacto",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0(
      "https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
      "refs/heads/main/Styles/style.css"
    )),
    FormularioContactoUI("contacto_test")
  )
)

server <- function(input, output, session) {
  
  contacto <- FormularioContacto(
    id           = "Contacto",
    usr          = reactive("CMEDINA"),
    cod_contacto = reactive("")
  )
  
  observeEvent(contacto$n(), {
    req(contacto$codigo())
    message("Contacto guardado con código: ", contacto$codigo())
  })
}

shinyApp(ui, server)
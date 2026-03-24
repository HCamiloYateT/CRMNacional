# Funciones -----
# Función para calcular el score de información de un lead
calcular_info_score <- function(df) {
  # Si es un data.frame con múltiples filas, aplicar fila por fila
  if (nrow(df) > 1) {
    return(sapply(1:nrow(df), function(i) calcular_info_score(df[i, , drop = FALSE])))
  }
  
  # Si es una sola fila, calcular el score
  df_row <- df
  
  ## 1. DEFINIR GRUPOS DE CAMPOS --------------------------------------------
  # Básicos (incluyendo negocio)
  core <- c(
    "Asesor",
    "PerRazSoc",
    "PerCod",
    "AutorizaTD",
    "Origen",
    "Pais",
    "Depto",
    "Mpio",
    "CodLinNegocio",  # negocio como básico
    "Segmento"        # negocio como básico
  )
  
  # Contacto: al menos un nombre + (teléfono o mail)
  contacto_ok <- {
    n <- trimws(unlist(strsplit(as.character(df_row$Nombres)   %||% "", "\\|")))
    t <- trimws(unlist(strsplit(as.character(df_row$Telefonos) %||% "", "\\|")))
    m <- trimws(unlist(strsplit(as.character(df_row$Mails)     %||% "", "\\|")))
    any(n != "" & !is.na(n)) && (any(t != "" & !is.na(t)) || any(m != "" & !is.na(m)))
  }
  
  # Dirección: conjuntos distintos según tipo de vía
  via_val <- as.character(df_row$via %||% "")
  if (isTRUE(via_val == "KILOMETRO")) {
    req_dir <- c("km_num", "km_com")  # obligatorios cuando es KILOMETRO
    opt_dir <- character(0)
  } else if (via_val != "" && !is.na(via_val)) {
    req_dir <- c("via_num", "com_num", "com2_num")   # obligatorios
    opt_dir <- c("via_let", "bis", "complemento",
                 "cardinalidad", "com_let", "com2_card") # opcionales
  } else {
    # Si no hay vía seleccionada, dirección = 0
    req_dir <- character(0)
    opt_dir <- character(0)
  }
  
  ## 2. PUNTAJES PARCIALES ---------------------------------------------------
  # Función auxiliar para evaluar campos
  evaluar_campos <- function(campos) {
    if (length(campos) == 0) return(0)
    vals <- as.character(df_row[, campos, drop = TRUE])
    mean(!is.na(vals) & vals != "")
  }
  
  pct_core     <- evaluar_campos(core)
  pct_dir_req  <- evaluar_campos(req_dir)
  pct_dir_opt  <- evaluar_campos(opt_dir)
  
  ## 3. COMBINACIÓN (PESOS) --------------------------------------------------
  # Ajustado: core (incluye línea/segmento) pesa mucho más
  score <- (
    0.70 * pct_core +              # datos básicos + negocio
      0.20 * pct_dir_req +           # dirección obligatoria
      0.05 * pct_dir_opt +           # dirección opcional
      0.05 * as.numeric(contacto_ok) # al menos un contacto "mínimo"
  )
  
  round(score * 100, 1)            # escala 0‑100 %
}
# Función para colapsar contactos en formato pipe-separated
colapsar_contactos <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  paste0(x, collapse = "|")
}
# Función auxiliar para validar NIT
validar_nit <- function(nit_input) {
  if (is.null(nit_input) || nit_input == "" || is.na(nit_input)) {
    return(NULL)
  }
  
  if (!EsEnteroPositivo(nit_input)) {
    return(FormatearTexto("* El NIT debe ser un valor numérico válido", negrita = T,
                          color = "red", tamano_pct = 0.75))
  }
  
  if (nit_input %in% CargarDatos("CRMNALMARLOT")$CLIENTE || nit_input %in% FACT$FctNit) {
    return(FormatearTexto("* El NIT ya existe en bases de datos de facturaciones", negrita = T,
                          color = "red", tamano_pct = 0.75))
  }
  
  return(NULL)
}
# Función para validar valores numéricos
validar_numerico <- function(valor, mensaje = "* Debe ser un valor numérico válido") {
  cond <- !EsNumero(valor) & !(valor %in% c("", NA))
  if (cond) {
    return(FormatearTexto(mensaje, negrita = T, color = "red", tamano_pct = 0.75))
  }
  return(NULL)
}
# Función para validar letras (complementos de dirección)
validar_letra <- function(valor, mensaje = "* Ingrese complemento válido") {
  cond <- !(nchar(valor) == 1 & grepl("^[A-Za-z]$", valor)) & !(valor %in% c("", NA))
  if (cond) {
    return(FormatearTexto(mensaje, negrita = T, color = "red", tamano_pct = 0.75))
  }
  return(NULL)
}
# Función para validar enteros positivos
validar_entero_positivo <- function(valor, mensaje = "* Debe ser un valor numérico válido") {
  cond <- !EsEnteroPositivo(valor) & !(valor %in% c("", NA))
  if (cond) {
    return(FormatearTexto(mensaje, negrita = T, color = "red", tamano_pct = 0.75))
  }
  return(NULL)
}
# Función para construir dirección completa
construir_direccion_texto <- function(input_list) {
  safe <- function(x) {
    if (is.null(x) || x == "" || is.na(x)) return(NULL)
    trimws(as.character(x))
  }
  
  # Si no se ha seleccionado vía, devolver NA
  if (is.null(input_list$LEAD_DIR_VIA) || input_list$LEAD_DIR_VIA %in% c("", NA)) return(NA)
  
  if (input_list$LEAD_DIR_VIA == "KILOMETRO") {
    # Formato: KILOMETRO 23 VIA MEDELLIN
    km_num <- safe(input_list$LEAD_DIR_KM_NUM)
    km_comp <- safe(input_list$LEAD_DIR_KM_COMP)
    
    if (is.null(km_num)) return(NA)
    
    direccion <- paste(input_list$LEAD_DIR_VIA, km_num)
    if (!is.null(km_comp)) {
      direccion <- paste(direccion, km_comp)
    }
    
    return(direccion)
    
  } else {
    # Formato: CALLE 63C # 24A 28
    via <- safe(input_list$LEAD_DIR_VIA)
    via_num <- safe(input_list$LEAD_DIR_VIA_NUM)
    via_let <- safe(input_list$LEAD_DIR_VIA_LET)
    bis <- safe(input_list$LEAD_DIR_BIS)
    via_com <- safe(input_list$LEAD_DIR_VIA_COM)
    com_num <- safe(input_list$LEAD_DIR_COM_NUM)
    com_let <- safe(input_list$LEAD_DIR_COM_LET)
    com2_num <- safe(input_list$LEAD_DIR_COM2_NUM)
    com2_card <- safe(input_list$LEAD_DIR_COM2_CARD)
    
    if (is.null(via) || is.null(via_num) || is.null(com_num) || is.null(com2_num)) return(NA)
    
    # Construir primera parte: VIA + NUMERO + LETRA
    primera_parte <- paste0(via, " ", via_num)
    if (!is.null(via_let)) {
      primera_parte <- paste0(primera_parte, via_let)
    }
    
    # Agregar BIS si existe
    if (!is.null(bis)) {
      primera_parte <- paste(primera_parte, bis)
    }
    
    # Agregar complemento de vía si existe
    if (!is.null(via_com)) {
      primera_parte <- paste(primera_parte, via_com)
    }
    
    # Construir segunda parte: # + NUMERO + LETRA
    segunda_parte <- paste0("# ", com_num)
    if (!is.null(com_let)) {
      segunda_parte <- paste0(segunda_parte, com_let)
    }
    
    # Construir tercera parte: - + NUMERO + CARDINALIDAD
    tercera_parte <- paste0("- ", com2_num)
    if (!is.null(com2_card)) {
      tercera_parte <- paste(tercera_parte, com2_card)
    }
    
    # Unir todas las partes
    direccion <- paste(primera_parte, segunda_parte, tercera_parte)
    
    return(direccion)
  }
}
# Función para determinar CodLinNegocio y LinNegocio
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

# Modulo -----
FormularioLeadsUI <- function(id, modo) {
  ns <- NS(id) 
  tagList(
    useShinyjs(),
    uiOutput(ns("Titulo")),
    # 1. DATOS BÁSICOS
    box(
      title = "Datos Básicos",
      width = 12,
      collapsible = FALSE,
      fluidRow(
        column(12, 
               ListaDesplegable(ns("LEAD_Asesor"), label = h6("Asesor"), choices = Choices()$personas,  selected = NULL, multiple = FALSE, fem = FALSE)
        )
      ),
      textAreaInput(ns("LEAD_Comentarios"), h6("Comentarios"), "", 
                    placeholder="Ingrese comentarios", width = "100%", height = "60px")
    ),
    # 2. CONTACTO
    box(
      title = "Contacto",
      width = 12,
      collapsible = FALSE,
      ListaDesplegable(ns("LEAD_AutDatos"), label = Obligatorio("Autoriza Tratamiento de Datos"), 
                       choices = c("","SI", "NO"),  selected = NULL, multiple = FALSE),
      uiOutput(ns("LEAD_Contactos")),
      div(style = "text-align: right; margin-top: 10px;",
          fluidRow(
            column(6),
            column(6,
                   div(style = "display: flex; justify-content: flex-end; gap: 10px;",
                       # Botón para eliminar contacto - siempre visible
                       actionBttn(inputId = ns("LEAD_RemoveContacto"), label = NULL, 
                                  style = "material-circle", color = "warning", 
                                  icon = icon("minus"), size = "xs"),
                       # Botón para añadir contacto - siempre visible
                       actionBttn(inputId = ns("LEAD_AddContacto"), label = NULL, 
                                  style = "material-circle", color = "danger", 
                                  icon = icon("plus"), size = "xs")
                   )
            )
          )
      )
    ),
    # EL RESTO DEL FORMULARIO SOLO SE MUESTRA SI AUTORIZA DATOS
    conditionalPanel(
      condition = paste0("input['", ns("LEAD_AutDatos"), "'] == 'SI'"),
      # 3. IDENTIFICACIÓN
      box(
        title = "Identificación",
        width = 12,
        collapsible = FALSE,
        fluidRow(
          column(6, textInput(ns("LEAD_RazSoc"), label = Obligatorio("Razón Social"), width = "100%")),
          column(6, textInput(ns("LEAD_NIT"), label = h6("NIT"), placeholder = "Sin dígito de verificación", width = "100%"))
        ),
        fluidRow(
          column(6, uiOutput(ns("LEAD_RazSoc_VAL"))),
          column(6, uiOutput(ns("LEAD_NIT_VAL")))
        ),
        ListaDesplegable(ns("LEAD_Origen"),label = h6("Origen del Lead"), 
                         choices = Choices()$origen,  selected = NULL, multiple = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("LEAD_Origen"), "'] == 'DIGITAL' || input['", ns("LEAD_Origen"), "'] == 'PRESENCIAL'"),
          ListaDesplegable(ns("LEAD_Origen_Med"), label = Obligatorio("Detalle del Origen"), 
                      choices = "",  selected = "", multiple = FALSE)
        )
      ),
      # 4. UBICACIÓN
      box(
        title = "Ubicación",
        width = 12,
        collapsible = FALSE,
        fluidRow(
          column(6, ListaDesplegable(ns("LEAD_Pais"), label = h6("País"), 
                                choices = Choices()$paises,  selected = "COLOMBIA", multiple = FALSE)
                 ),
          column(6, ListaDesplegable(ns("LEAD_Depto"),label = h6("Departamento"), 
                                choices = Choices()$deptos,  selected = NULL, multiple = FALSE)
                 )
          ),
        ListaDesplegable(ns("LEAD_Mpio"),label = h6("Ciudad/Municipio"),
                         choices = "",  selected = "", multiple = FALSE),
        ListaDesplegable(ns("LEAD_DIR_VIA"), label = h6("Vía"),
                         choices = Choices()$direccion_via,  selected = NULL, multiple = FALSE),
        conditionalPanel(condition = paste0("input['", ns("LEAD_DIR_VIA"), "'] !== '' && ",
                                            "input['", ns("LEAD_DIR_VIA"), "'] !== null && ",
                                            "input['", ns("LEAD_DIR_VIA"), "'] !== 'KILOMETRO'"),
                         fluidRow(
                           column(6,
                                  textInput(ns("LEAD_DIR_VIA_NUM"), label = Obligatorio("Número"), width = "100%"),
                                  uiOutput(ns("LEAD_DIR_VIA_NUM_VAL"))
                           ),
                           column(6,
                                  textInput(ns("LEAD_DIR_VIA_LET"), label = h6("Letra"), width = "100%"),
                                  uiOutput(ns("LEAD_DIR_VIA_LET_VAL"))
                           )
                         )
        ),
        conditionalPanel(condition = paste0("input['", ns("LEAD_DIR_VIA"), "'] == 'KILOMETRO'"),
                         fluidRow(
                           column(6, 
                                  textInput(ns("LEAD_DIR_KM_NUM"), label = Obligatorio("Número"), width = "100%"),
                                  uiOutput(ns("LEAD_DIR_KM_NUM_VAL"))
                           ),
                           column(6,
                                  textInput(ns("LEAD_DIR_KM_COMP"), label = Obligatorio("Complemento"), width = "100%")
                           )
                         )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("LEAD_DIR_VIA"), "'] !== '' && ",
                             "input['", ns("LEAD_DIR_VIA"), "'] !== null && ",
                             "input['", ns("LEAD_DIR_VIA"), "'] !== 'KILOMETRO'"),
          fluidRow(
            column(4,
                   ListaDesplegable(ns("LEAD_DIR_BIS"), label = h6("BIS"), 
                                    choices = c("","BIS"),  selected = NULL, multiple = FALSE
                                    )
                   ),
            column(4,
                   textInput(ns("LEAD_DIR_VIA_COM"), label = h6("Complemento"), width = "100%"),
                   uiOutput(ns("LEAD_DIR_VIA_COM_VAL"))
            ),
            column(4,
                   ListaDesplegable(ns("LEAD_DIR_VIA_CARD"), label = h6("Cardinalidad"), 
                                    choices = Choices()$direccion_cardinalidad,  selected = NULL, multiple = FALSE
                                    )
            )
          ),
          fluidRow(
            column(4, br(),
                   tags$div(style = "text-align: center;",
                            FormatearTexto("#", tamano_pct = 1.2, alineacion = "center"))
            ),
            column(4,
                   textInput(ns("LEAD_DIR_COM_NUM"), label = Obligatorio("Número"), width = "100%"),
                   uiOutput(ns("LEAD_DIR_COM_NUM_VAL"))
            ),
            column(4,
                   textInput(ns("LEAD_DIR_COM_LET"), label = h6("Letra"), width = "100%"),
                   uiOutput(ns("LEAD_DIR_COM_LET_VAL"))
            )
          ),
          fluidRow(
            column(4, br(),
                   tags$div(style = "text-align: center;",
                            FormatearTexto("-", tamano_pct = 1.2, alineacion = "center"))
            ),
            column(4,
                   textInput(ns("LEAD_DIR_COM2_NUM"), label = Obligatorio("Número"), width = "100%"),
                   uiOutput(ns("LEAD_DIR_COM2_NUM_VAL"))
            ),
            column(4,
                   ListaDesplegable(ns("LEAD_DIR_COM2_CARD"), label = h6("Cardinalidad"),
                                    choices = Choices()$direccion_cardinalidad,  selected = NULL, multiple = FALSE
                                    )
                   )
            ),
          fluidRow(
            column(12, textInput(ns("LEAD_DIR_DETALLE"), label = h6("Detalle"), width = "100%"))
          )
        ),
        uiOutput(ns("LEAD_DIR_Completa"))
      ),
      # 5. NEGOCIO
      box(title = "Negocio", width = 12, collapsible = FALSE,
          fluidRow(
            column(6, ListaDesplegable(ns("LEAD_SegmentoPrincipal"), label = h6("Línea de Negocio"), 
                                       choices = Choices()$linneg, 
                                       selected = NULL,
                                       multiple = FALSE
                                       )
                   ),
            column(6, ListaDesplegable(ns("LEAD_Segmento"),label = h6("Segmento"), 
                                       choices = Choices()$segmento,  selected = NULL, multiple = FALSE)
                   )
            ),
          ListaDesplegable(ns("LEAD_Aliado"),label = h6("Aliado"), 
                           choices = Choices()$aliado,  selected = NULL, multiple = FALSE
                           ),
          ListaDesplegable(ns("LEAD_EstadoCuenta"),label = h6("Estado de Cuenta"),
                           choices = Choices()$estadocuenta,  selected = NULL, multiple = FALSE
                           ),
          ListaDesplegable(ns("LEAD_RAZ_INTERES"), label = h6("¿Por qué nos interesa?"), 
                           choices = Choices()$raz_interes,  selected = NULL, multiple = FALSE
                           )
          ),
      # 6. GESTIÓN DE DESCARTE
      box(title = "Descartar", width = 12, collapsible = FALSE,
        ListaDesplegable( ns("LEAD_Descartada"), label = h6("¿Lead Descartado?"), choices = c("", "SI", "NO"),
                          selected = "NO", multiple = FALSE),
        conditionalPanel( condition = paste0("input['", ns("LEAD_Descartada"), "'] == 'SI'"),
          # RAZÓN PRINCIPAL
          fluidRow(
            column(12,
                   ListaDesplegable(inputId  = ns("LEAD_RazonDescartada"), label    = Obligatorio("Razón para descartar el lead"),
                                    choices  = Choices()$raz_descarte, selected = NULL, multiple = FALSE, ns = ns)
                   )
            ),
          # OPCIÓN CALIDAD
          conditionalPanel(condition = paste0("input['", ns("LEAD_RazonDescartada"), "'] == 'CALIDAD'"),
                           fluidRow(
                             column(12,
                                    ListaDesplegable(inputId = ns("LEAD_DescarteCalidad"), 
                                                     label = Obligatorio("Calidad por la que se Descarta"),
                                                     choices = Choices()$categoria,
                                                     selected = NULL, multiple = FALSE, ns = ns)
                                    )
                             )
                           ),
          # OPCIÓN PRECIO
          conditionalPanel(
            condition = paste0("input['", ns("LEAD_RazonDescartada"), "'] == 'PRECIO'"),
            fluidRow(
              column(12, Saltos(),
                     InputNumerico(ns("LEAD_DescartadoPrecio"), label = "Precio por el que se Descarta",
                                   value = NA, type  = "dinero")
                     )
              )
            )
          )
      ),
      # BOTÓN DE GUARDAR (solo aparece si autoriza datos)
      div(style = "text-align: right; margin-top: 10px;",
          div(style = "display: inline-block; width: 20%;",
              if (modo == "crear") {
                actionBttn(inputId = ns("LEAD_Crear"), label = "Crear Lead",
                           style = "unite", color = "danger", size = "xs",
                           icon = icon("save"), block = TRUE)
              } else {
                actionBttn(inputId = ns("LEAD_Editar"), label = "Guardar Cambios",
                           style = "unite", color = "danger", size = "xs",
                           icon = icon("edit"), block = TRUE)
              }
          )
      )
    )
  )
}
FormularioLeads <-  function(id, rv, usr, tit) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    ret <- reactiveVal(0)
    contact_data <- reactiveValues(num_contacts = 1, data = list())
    
    # DATOS REACTIVOS ----
    data_lead_edit <- reactive({
      req(tit())
      CargarDatos("CRMNALLEAD") %>% 
        mutate(FechaHoraModi = as_datetime(FechaHoraModi)) %>% 
        filter(PerRazSoc == tit()) %>% 
        arrange(desc(FechaHoraModi)) %>% 
        filter(row_number() == 1)
    }) 
    
    # OUTPUTS ----
    
    output$Titulo <- renderUI({
      h4(data_lead_edit()$PerRazSoc)
    })
    output$LEAD_Contactos <- renderUI({
      
      if (tit() == "") {
        num <- contact_data$num_contacts
        con <- paste0("LEAD_CON_", 1:num)
        lapply(seq_along(con), function(i){
          x <- con[i]
          
          # Recuperar datos guardados si existen
          saved_data <- contact_data$data[[x]]
          nom_val <- if (!is.null(saved_data)) saved_data$nom else ""
          cargo_val <- if (!is.null(saved_data)) saved_data$cargo else ""
          tel_val <- if (!is.null(saved_data)) saved_data$tel else ""
          mail_val <- if (!is.null(saved_data)) saved_data$mail else ""
          
          fluidRow(
            tags$hr(style = "border-color: grey;"),
            column(6,
                   textInput(ns(paste0(x,"_Nom")), label = h6("Nombre"), width = "100%", value = nom_val)
            ),
            column(6,
                   textInput(ns(paste0(x,"_Cargo")), label = h6("Cargo"), width = "100%", value = cargo_val)
            ),
            column(6,
                   textInput(ns(paste0(x,"_Tel")), label = h6("Teléfono"), width = "100%", value = tel_val),
                   uiOutput(ns(paste0(x,"_Tel_Val")))
            ),
            column(6,
                   textInput(ns(paste0(x,"_Mail")), label = h6("Mail"), width = "100%", value = mail_val),
                   uiOutput(ns(paste0(x,"_Mail_Val")))
            )
          )
        })
      }
      else {
        num <- max(str_count(data_lead_edit()$Nombres, "\\|") + 1, contact_data$num_contacts, na.rm = T)
        con <- paste0("LEAD_CON_", 1:num)
        lapply(seq_along(con), function(i){
          
          x = con[i]
          nom  = str_split(data_lead_edit()$Nombres, "\\|", simplify = T)[i]
          cargo = str_split(data_lead_edit()$Cargos, "\\|", simplify = T)[i]
          tel = str_split(data_lead_edit()$Telefonos, "\\|", simplify = T)[i]
          mail =  str_split(data_lead_edit()$Mails, "\\|", simplify = T)[i]
          
          fluidRow(
            tags$hr(style = "border-color: grey;"),
            column(6,
                   textInput(ns(paste0(x,"_Nom")), label = h6("Nombre"), width = "100%", value = nom)
            ),
            column(6,
                   textInput(ns(paste0(x,"_Cargo")), label = h6("Cargo"), width = "100%", value = cargo)
            ),
            column(6,
                   textInput(ns(paste0(x,"_Tel")), label = h6("Teléfono"), width = "100%", value = tel),
                   uiOutput(ns(paste0(x,"_Tel_Val")))
            ),
            column(6,
                   textInput(ns(paste0(x,"_Mail")), label = h6("Mail"), width = "100%", value = mail),
                   uiOutput(ns(paste0(x,"_Mail_Val")))
            )
          )
        })
      }
    })
    output$LEAD_RazSoc_VAL <- renderUI({
      
      if (tit() =="") {
        razon_social_input <- str_to_upper(input$LEAD_RazSoc)
        
        # Validar si ya existe en CRMNALLEAD
        cond_lead <- razon_social_input %in% CargarDatos("CRMNALLEAD")$PerRazSoc
        
        # Validar si existe en NCLIENTE
        cond_ncliente <- razon_social_input %in% str_to_upper(NCLIENTE$PerRazSoc)
        
        # Validar si existe en FACT
        cond_fact <- razon_social_input %in% str_to_upper(FACT$PerRazSoc)
        
        # Buscar nombres similares mientras se digita (si hay al menos 3 caracteres)
        similares <- NULL
        if (!is.null(razon_social_input) && nchar(razon_social_input) >= 3) {
          # Buscar en NCLIENTE
          ncliente_similares <- NCLIENTE %>%
            filter(grepl(razon_social_input, str_to_upper(PerRazSoc), fixed = TRUE)) %>%
            pull(PerRazSoc) %>%
            str_to_upper() %>%
            unique()
          
          # Buscar en FACT
          fact_similares <- FACT %>%
            filter(grepl(razon_social_input, str_to_upper(PerRazSoc), fixed = TRUE)) %>%
            pull(PerRazSoc) %>%
            str_to_upper() %>%
            unique()
          
          # Combinar y limitar a 5 resultados
          similares <- unique(c(ncliente_similares, fact_similares))
          if (length(similares) > 5) similares <- similares[1:5]
        }
        
        # Generar mensajes de error
        if (cond_lead) {
          tex <- FormatearTexto("* La razón social ya existe como lead", negrita = T,
                                color = "red", tamano_pct = 0.75)
        } else if (cond_ncliente) {
          tex <- FormatearTexto("* La razón social ya existe en NCLIENTE", negrita = T,
                                color = "red", tamano_pct = 0.75)
        } else if (cond_fact) {
          tex <- FormatearTexto("* La razón social ya existe en FACT", negrita = T,
                                color = "red", tamano_pct = 0.75)
        } else if (!is.null(similares) && length(similares) > 0) {
          tex <- tagList(
            FormatearTexto("Nombres similares encontrados:", negrita = T,
                           color = "orange", tamano_pct = 0.75),
            tags$ul(
              lapply(similares, function(x) tags$li(x))
            )
          )
        } else {
          tex <- NULL
        }
        
        return(tex)
      }
      
    })
    
    output$LEAD_NIT_VAL <- renderUI({
      validar_nit(input$LEAD_NIT)
    })
    
    output$LEAD_DIR_VIA_NUM_VAL <- renderUI({
      validar_numerico(input$LEAD_DIR_VIA_NUM)
    })
    
    output$LEAD_DIR_KM_NUM_VAL <- renderUI({
      validar_numerico(input$LEAD_DIR_KM_NUM)
    })
    
    output$LEAD_DIR_VIA_LET_VAL <- renderUI({
      validar_letra(input$LEAD_DIR_VIA_LET)
    })
    
    output$LEAD_DIR_VIA_COM_VAL <- renderUI({
      validar_letra(input$LEAD_DIR_VIA_COM)
    })
    
    output$LEAD_DIR_COM_NUM_VAL <- renderUI({
      validar_entero_positivo(input$LEAD_DIR_COM_NUM)
    })
    
    output$LEAD_DIR_COM_LET_VAL <- renderUI({
      validar_letra(input$LEAD_DIR_COM_LET)
    })
    
    output$LEAD_DIR_COM2_NUM_VAL <- renderUI({
      validar_entero_positivo(input$LEAD_DIR_COM2_NUM)
    })
    
    output$LEAD_DIR_Completa <- renderUI({
      direccion <- construir_direccion_texto(reactiveValuesToList(input))
      if(is.na(direccion)) "" else direccion
    })
    
    # GESTIÓN DE CONTACTOS ----
    
    
    observe({
      # Botón de añadir: deshabilitar si hay 3 o más contactos
      if (contact_data$num_contacts >= 3) {
        shinyjs::disable("LEAD_AddContacto")
      } else {
        shinyjs::enable("LEAD_AddContacto")
      }
      
      # Botón de eliminar: deshabilitar si hay solo 1 contacto
      if (contact_data$num_contacts <= 1) {
        shinyjs::disable("LEAD_RemoveContacto")
      } else {
        shinyjs::enable("LEAD_RemoveContacto")
      }
    })
    
    save_contact_data <- function() {
      for (i in 1:contact_data$num_contacts) {
        contact_id <- paste0("LEAD_CON_", i)
        contact_data$data[[contact_id]] <- list(
          nom = input[[paste0(contact_id, "_Nom")]] %||% "",
          cargo = input[[paste0(contact_id, "_Cargo")]] %||% "",
          tel = input[[paste0(contact_id, "_Tel")]] %||% "",
          mail = input[[paste0(contact_id, "_Mail")]] %||% ""
        )
      }
    }
    
    observeEvent(input$LEAD_AddContacto, {
      save_contact_data()  # Guardar datos actuales antes de añadir
      if (contact_data$num_contacts < 3) {
        contact_data$num_contacts <- contact_data$num_contacts + 1
      }
    })
    
    observeEvent(input$LEAD_RemoveContacto, {
      save_contact_data()  # Guardar datos actuales antes de eliminar
      if (contact_data$num_contacts > 1) {
        # Eliminar el último contacto de los datos guardados
        last_contact <- paste0("LEAD_CON_", contact_data$num_contacts)
        contact_data$data[[last_contact]] <- NULL
        contact_data$num_contacts <- contact_data$num_contacts - 1
      }
    })
    
    # VALIDACIÓN DE CONTACTOS -----
    observe({
      num <- contact_data$num_contacts
      con_tel <- paste0("LEAD_CON_", 1:num, "_Tel")
      con_mail <- paste0("LEAD_CON_", 1:num, "_Mail")
      
      sapply(con_tel, function(x){
        tel_val <- input[[x]]
        cond <- !EsNumTelefono(tel_val) & !(tel_val %in% c("", NA))
        
        output[[paste0(x, "_Val")]] <- renderUI({
          
          if (cond) {
            tex <- FormatearTexto("* Teléfono no válido. Debe iniciar con 3 o 6 y tener 10 dígitos.", negrita = TRUE,
                                  color = "red", tamano_pct = 0.75)
          } else {
            tex <- NULL
          }
          return(tex)
        })
      })
      
      sapply(con_mail, function(x){
        
        mail_val <- input[[x]]
        cond <- !EsEmail(mail_val) & !(mail_val %in% c("", NA))
        
        output[[paste0(x, "_Val")]] <- renderUI({
          
          if (cond) {
            tex <- FormatearTexto("* Correo no válido. Debe tener un dominio válido", negrita = TRUE,
                                  color = "red", tamano_pct = 0.75)
          } else {
            tex <- NULL
          }
          return(tex)
        })
      })
    })
    
    # CARGA DE DATOS EN MODO EDICIÓN ----
    observeEvent(data_lead_edit(), {
      if (nrow(data_lead_edit()) > 0) {
        
        updatePickerInput(session = session, inputId = "LEAD_Asesor", selected = data_lead_edit()$Asesor)
        updateTextAreaInput(session = session, inputId = "LEAD_Comentarios", value = data_lead_edit()$Comentarios)
        updateTextInput(session = session, inputId = "LEAD_RazSoc", value = data_lead_edit()$PerRazSoc)
        updateTextInput(session = session, inputId = "LEAD_NIT", value = data_lead_edit()$PerCod)
        updatePickerInput(session = session, inputId = "LEAD_AutDatos", selected = data_lead_edit()$AutorizaTD)
        
        updatePickerInput(session = session, inputId = "LEAD_Origen", selected = data_lead_edit()$Origen)
        
        updatePickerInput(session = session, inputId = "LEAD_Pais", selected = data_lead_edit()$Pais)
        updatePickerInput(session = session, inputId = "LEAD_Depto", selected = data_lead_edit()$Depto)
        observeEvent(input$LEAD_Depto, {
          updatePickerInput(session = session, inputId = "LEAD_Mpio", selected = data_lead_edit()$Mpio)
        })
        
        updatePickerInput(session = session, inputId = "LEAD_DIR_VIA", selected = data_lead_edit()$via)
        updateTextInput(session = session, inputId = "LEAD_DIR_VIA_NUM", value = data_lead_edit()$via_num)
        updateTextInput(session = session, inputId = "LEAD_DIR_KM_NUM", value = data_lead_edit()$km_num)
        updateTextInput(session = session, inputId = "LEAD_DIR_KM_COMP", value = data_lead_edit()$km_com)
        updateTextInput(session = session, inputId = "LEAD_DIR_VIA_LET", value = data_lead_edit()$via_let)
        updatePickerInput(session = session, inputId = "LEAD_DIR_BIS", selected = data_lead_edit()$bis)
        updateTextInput(session = session, inputId = "LEAD_DIR_VIA_COM", value = data_lead_edit()$complemento)
        updatePickerInput(session = session, inputId = "LEAD_DIR_VIA_CARD", selected = data_lead_edit()$cardinalidad)
        updateTextInput(session = session, inputId = "LEAD_DIR_COM_NUM", value = data_lead_edit()$com_num)
        updateTextInput(session = session, inputId = "LEAD_DIR_COM_LET", value = data_lead_edit()$com_let)
        updateTextInput(session = session, inputId = "LEAD_DIR_COM2_NUM", value = data_lead_edit()$com2_num)
        updatePickerInput(session = session, inputId = "LEAD_DIR_COM2_CARD", selected = data_lead_edit()$com2_card)
        updateTextInput(session = session, inputId = "LEAD_DIR_DETALLE", value = data_lead_edit()$compl)
        
        updatePickerInput(session = session, inputId = "LEAD_SegmentoPrincipal", selected = data_lead_edit()$CodLinNegocio)
        updatePickerInput(session = session, inputId = "LEAD_Segmento", selected = data_lead_edit()$Segmento)
        
        updatePickerInput(session = session, inputId = "LEAD_Aliado", selected = data_lead_edit()$Aliado)
        updatePickerInput(session = session, inputId = "LEAD_EstadoCuenta", selected = data_lead_edit()$EstadoCuenta)
        updatePickerInput(session = session, inputId = "LEAD_RAZ_INTERES", selected = data_lead_edit()$RazonInteres)
        
        disable("LEAD_RazSoc")
        
        # Configurar número de contactos basado en datos existentes
        if (!is.na(data_lead_edit()$Nombres) && data_lead_edit()$Nombres != "") {
          contact_data$num_contacts <- str_count(data_lead_edit()$Nombres, "\\|") + 1
        }
      }
    })
    
    # ACTUALIZACIÓN DINÁMICA DE CAMPOS -----
    observeEvent(input$LEAD_Origen, {
      req(input$LEAD_Origen)
      
      t1 <- CargarDatos("CRMNALORILEAD") %>% 
        filter(Estado == "A", 
               Origen == input$LEAD_Origen)
      

      cho <- if(input$LEAD_Origen == "PRESENCIAL") Unicos(t1$Detalle) else if (input$LEAD_Origen == "DIGITAL") Unicos(t1$Detalle) else ""
      updatePickerInput(session, "LEAD_Origen_Med", choices = cho, selected = NULL)
      
    })
    
    observeEvent(input$LEAD_Depto, {
      req(input$LEAD_Depto)
      cho <-  c("", Unicos(CargarDatos("ANDIVIPOLA")%>% filter(NomDep == input$LEAD_Depto) %>% pull(Mun)))
      sel = ifelse(tit() == "", cho[1], data_lead_edit()$Mpio)
      updatePickerInput(session, "LEAD_Mpio", choices = cho, selected = sel)
      
    })
    
    observeEvent(input$LEAD_Pais, {
      req(input$LEAD_Pais)
      if (input$LEAD_Pais != "COLOMBIA") {
        disable("LEAD_Depto")
        disable("LEAD_Mpio")
      }
      
    })
    
    # GUARDAR LEAD (CREAR) -----
    observeEvent(input$LEAD_Crear, {
      
      # Validaciones de contacto
      num <- contact_data$num_contacts
      con_tel <- paste0("LEAD_CON_", 1:num, "_Tel")
      con_mail <- paste0("LEAD_CON_", 1:num, "_Mail")
      
      tel <- any(sapply(con_tel, function(x){
        tel_val <- input[[x]]
        !EsNumTelefono(tel_val) & !(tel_val %in% c("", NA))
      }))
      
      mail <- any(sapply(con_mail, function(x){
        mail_val <- input[[x]]
        !EsEmail(mail_val) & !(mail_val %in% c("", NA))
      }))
      
      # Validación de dirección
      dir <- !(input$LEAD_DIR_VIA %in% c("",NA, "KILOMETRO")) &
        any(sapply(c("LEAD_DIR_VIA_NUM", "LEAD_DIR_COM_NUM", "LEAD_DIR_COM2_NUM"),
                   function(x) EsVacio(input[[x]]))) || 
        (input$LEAD_DIR_VIA == "KILOMETRO" &
           any(sapply(c("LEAD_DIR_KM_NUM", "LEAD_DIR_KM_COMP"), function(x) EsVacio(input[[x]]))))
      
      # Validaciones adicionales para NIT y Razón Social
      razon_social_input <- str_to_upper(input$LEAD_RazSoc)
      nit_input <- input$LEAD_NIT
      
      cond <- c(
        "La razón social ya existe como lead" = razon_social_input %in% CargarDatos("CRMNALLEAD")$PerRazSoc,
        "La razón social ya existe en clientes" = razon_social_input %in% str_to_upper(NCLIENTE$PerRazSoc),
        "La razón social ya existe en facturaciones" = razon_social_input %in% str_to_upper(FACT$PerRazSoc),
        "El NIT ya existe en pyg anteriores" = !EsVacio(nit_input) && nit_input %in% CargarDatos("CRMNALMARLOT")$PerCod,
        "El NIT ya existe en facturaciones" = !EsVacio(nit_input) && nit_input %in% FACT$PerCod,
        "El campo Razon Social es obligatorio" = EsVacio(input$LEAD_RazSoc),
        "El campo Autoriza tratamiento de datos es obligatorio" = EsVacio(input$LEAD_AutDatos), 
        "El campo detallle del origen del Lead es obligatorio" =  input$LEAD_Origen %in% c("DIGITAL", "PRESENCIAL") & EsVacio(input$LEAD_Origen_Med),
        "Valores inválidos en teléfono de Contacto" = tel,
        "Valores inválidos en Email de Contacto" = mail,
        "El municipio es obligatorio" = !EsVacio(input$LEAD_Depto) & EsVacio(input$LEAD_Mpio),
        "Dirección incompleta o invalida" = dir
      )
      
      if (any(cond)) {
        
        sapply(names(cond), function(x) {
          val <- cond[[x]]
          if (val) {
            showNotification(x, duration = 4, type = "error")
          }
        })
        
      } else{
        
        reac_val <- reactiveValuesToList(input)
        reac_val <- reac_val[names(reac_val) %in% grep("^LEAD_", names(reac_val), value = TRUE) &
                               !grepl("^LEAD_GUARDAR$", names(reac_val)) &
                               !grepl("^LEAD_AddContacto$", names(reac_val)) &
                               !grepl("^LEAD_RemoveContacto$", names(reac_val))]
        
        # Extraer vectores de contactos
        nom_vec   <- reac_val[grepl("^LEAD_CON_\\d+_Nom$",   names(reac_val))] %>% unlist()
        cargo_vec <- reac_val[grepl("^LEAD_CON_\\d+_Cargo$", names(reac_val))] %>% unlist()
        tel_vec   <- reac_val[grepl("^LEAD_CON_\\d+_Tel$",   names(reac_val))] %>% unlist()
        mail_vec  <- reac_val[grepl("^LEAD_CON_\\d+_Mail$",  names(reac_val))] %>% unlist()
        
        # Colapsar contactos (siempre devuelve longitud 1)
        lis_nom  <- colapsar_contactos(nom_vec)
        lis_car  <- colapsar_contactos(cargo_vec)
        lis_tel  <- colapsar_contactos(tel_vec)
        lis_mail <- colapsar_contactos(mail_vec)
        
        # Determinar CodLinNegocio y LinNegocio basado en la selección
        linea_neg <- determinar_linea_negocio(input$LEAD_SegmentoPrincipal)
        descartar_flag <- ifelse(input$LEAD_Descartada == "SI", 1, 0)
        
        razon_desc <- if (descartar_flag == 1) input$LEAD_RazonDescartada else NA
        calidad_desc <- if (descartar_flag == 1 && input$LEAD_RazonDescartada == "CALIDAD") input$LEAD_DescarteCalidad else NA
        precio_desc <- if (descartar_flag == 1 && input$LEAD_RazonDescartada == "PRECIO") as.numeric(input$LEAD_DescartadoPrecio) else NA
        
        usuario_desc <- if (descartar_flag == 1) usr() else NA
        fecha_desc <- if (descartar_flag == 1) as.character(Sys.time()) else NA
        
        aux1 <- data.frame(
          UsuarioCrea      = usr(),
          FechaHoraCrea    = Sys.time(),
          UsuarioMod       = usr(),
          FechaHoraModi    = Sys.time(),
          SegmentoRacafe   = "LEAD",
          Asesor           = input$LEAD_Asesor,
          PerRazSoc        = input$LEAD_RazSoc,
          PerCod           = input$LEAD_NIT,
          AutorizaTD       = input$LEAD_AutDatos,
          Origen           = input$LEAD_Origen,
          DetOrigen        = input$LEAD_Origen_Med,
          CodLinNegocio    = linea_neg$cod,
          LinNegocio       = linea_neg$nombre,
          Segmento         = input$LEAD_Segmento,
          Aliado           = input$LEAD_Aliado,
          EstadoCuenta     = input$LEAD_EstadoCuenta,
          RazonInteres     = input$LEAD_RAZ_INTERES,
          Pais             = input$LEAD_Pais,
          Depto            = input$LEAD_Depto,
          Mpio             = ifelse(is.null(input$LEAD_Mpio), NA_character_, input$LEAD_Mpio),
          via              = input$LEAD_DIR_VIA,
          via_num          = input$LEAD_DIR_VIA_NUM,
          km_num           = input$LEAD_DIR_KM_NUM,
          km_com           = input$LEAD_DIR_KM_COMP,
          via_let          = input$LEAD_DIR_VIA_LET,
          bis              = input$LEAD_DIR_BIS,
          complemento      = input$LEAD_DIR_VIA_COM,
          cardinalidad     = input$LEAD_DIR_VIA_CARD,
          com_num          = input$LEAD_DIR_COM_NUM,
          com_let          = input$LEAD_DIR_COM_LET,
          com2_num         = input$LEAD_DIR_COM2_NUM,
          com2_card        = input$LEAD_DIR_COM2_CARD,
          compl            = input$LEAD_DIR_DETALLE,
          lng              = NA_real_,
          lat              = NA_real_,
          Comentarios      = input$LEAD_Comentarios,
          Nombres          = lis_nom,
          Cargos           = lis_car,
          Telefonos        = lis_tel,
          Mails            = lis_mail,
          Descartada        = descartar_flag,
          UsuarioDescarte   = usuario_desc,
          FechaHoraDescarte = fecha_desc,
          RazonDescartado   = razon_desc,
          DescartadoPrecio  = precio_desc,
          DescartadoCalidad = calidad_desc,
          stringsAsFactors = FALSE) %>%
          mutate(across(where(is.character), ~ str_to_upper(ifelse(. == "", NA_character_, .))))
        
        AgregarDatos(aux1, "CRMNALLEAD")
        showNotification("Lead registrado exitosamente", duration = 4, type = "message")
        
        # Limpiar datos de contacto
        contact_data$data <- list()
        contact_data$num_contacts <- 1
        
        lapply(names(reac_val), reset)
        
        rv$btn <- input$LEAD_Crear
        ret(ret()+1)
        return(list(n = reactive(ret)))
      }
    })
    
    # GUARDAR LEAD (EDITAR) ----
    observeEvent(input$LEAD_Editar, {
      req(input$LEAD_Editar)
      
      # Validaciones de contacto
      num <- contact_data$num_contacts
      con_tel <- paste0("LEAD_CON_", 1:num, "_Tel")
      con_mail <- paste0("LEAD_CON_", 1:num, "_Mail")
      
      tel <- any(sapply(con_tel, function(x){
        tel_val <- input[[x]]
        !EsNumTelefono(tel_val) & !(tel_val %in% c("", NA))
      }))
      
      mail <- any(sapply(con_mail, function(x){
        mail_val <- input[[x]]
        !EsEmail(mail_val) & !(mail_val %in% c("", NA))
      }))
      
      # Validación de dirección
      dir <- !(input$LEAD_DIR_VIA %in% c("",NA, "KILOMETRO")) &
        any(sapply(c("LEAD_DIR_VIA_NUM", "LEAD_DIR_COM_NUM", "LEAD_DIR_COM2_NUM"),
                   function(x) EsVacio(input[[x]]))) || 
        (input$LEAD_DIR_VIA == "KILOMETRO" &
           any(sapply(c("LEAD_DIR_KM_NUM", "LEAD_DIR_KM_COMP"), function(x) EsVacio(input[[x]]))))
      
      # Validaciones adicionales para NIT (solo si se está modificando)
      nit_input <- input$LEAD_NIT
      nit_original <- data_lead_edit()$PerCod
      nit_cambio <- !EsVacio(nit_input) && nit_input != nit_original
      
      cond <- c(
        "El NIT ya existe en CRMNALMARLOT" = nit_cambio && nit_input %in% CargarDatos("CRMNALMARLOT")$PerCod,
        "El NIT ya existe en FACT" = nit_cambio && nit_input %in% FACT$PerCod,
        "El campo Razon Social es obligatorio" = EsVacio(input$LEAD_RazSoc),
        "El campo detallle del origen del Lead es obligatorio" =  input$LEAD_Origen %in% c("DIGITAL", "PRESENCIAL") & EsVacio(input$LEAD_Origen_Med),
        "El campo Autoriza tratamiento de datos es obligatorio" = EsVacio(input$LEAD_AutDatos), 
        "Valores inválidos en teléfono de Contacto" = tel,
        "Valores inválidos en Email de Contacto" = mail,
        "El campo municipio es obligatorio" = !EsVacio(input$LEAD_Depto) & EsVacio(input$LEAD_Mpio),
        "Dirección incompleta o inválida" = dir
      )
      
      if (any(cond)) {
        
        sapply(names(cond), function(x) {
          val <- cond[[x]]
          if (val) {
            showNotification(x, duration = 4, type = "error")
          }
        })
        
      } else{
        
        reac_val <- reactiveValuesToList(input)
        
        # Extraer vectores de contactos
        nom_vec   <- reac_val[grepl("^LEAD_CON_\\d+_Nom$",   names(reac_val))] %>% unlist()
        cargo_vec <- reac_val[grepl("^LEAD_CON_\\d+_Cargo$", names(reac_val))] %>% unlist()
        tel_vec   <- reac_val[grepl("^LEAD_CON_\\d+_Tel$",   names(reac_val))] %>% unlist()
        mail_vec  <- reac_val[grepl("^LEAD_CON_\\d+_Mail$",  names(reac_val))] %>% unlist()
        
        # Colapsar contactos (siempre devuelve longitud 1)
        lis_nom  <- colapsar_contactos(nom_vec)
        lis_car  <- colapsar_contactos(cargo_vec)
        lis_tel  <- colapsar_contactos(tel_vec)
        lis_mail <- colapsar_contactos(mail_vec)
        
        # Generar valores de descarte
        descartar_flag <- ifelse(input$LEAD_Descartada == "SI", 1, 0)
        
        razon_desc <- if (descartar_flag == 1) input$LEAD_RazonDescartada else NA
        calidad_desc <- if (descartar_flag == 1 && input$LEAD_RazonDescartada == "CALIDAD") input$LEAD_DescarteCalidad else NA
        precio_desc <- if (descartar_flag == 1 && input$LEAD_RazonDescartada == "PRECIO") as.numeric(input$LEAD_DescartadoPrecio) else NA
        
        usuario_desc <- if (descartar_flag == 1) usr() else data_lead_edit()$UsuarioDescarte
        fecha_desc <- if (descartar_flag == 1) as.character(Sys.time()) else data_lead_edit()$FechaHoraDescarte
        
        aux1 <- data.frame(
          UsuarioCrea      = data_lead_edit()$UsuarioCrea,
          FechaHoraCrea    = data_lead_edit()$FechaHoraCrea,
          UsuarioMod       = usr(),
          FechaHoraModi    = Sys.time(),
          SegmentoRacafe   = "LEAD",
          Asesor           = input$LEAD_Asesor,
          PerRazSoc        = input$LEAD_RazSoc,
          PerCod           = input$LEAD_NIT,
          AutorizaTD       = input$LEAD_AutDatos,
          Origen           = input$LEAD_Origen,
          DetOrigen        = input$LEAD_Origen_Med,
          CodLinNegocio    = linea_neg$cod,
          LinNegocio       = linea_neg$nombre,
          Segmento         = input$LEAD_Segmento,
          Aliado           = input$LEAD_Aliado,
          EstadoCuenta     = input$LEAD_EstadoCuenta,
          RazonInteres     = input$LEAD_RAZ_INTERES,
          Pais             = input$LEAD_Pais,
          Depto            = input$LEAD_Depto,
          Mpio             = ifelse(is.null(input$LEAD_Mpio), NA_character_, input$LEAD_Mpio),
          via              = input$LEAD_DIR_VIA,
          via_num          = input$LEAD_DIR_VIA_NUM,
          km_num           = input$LEAD_DIR_KM_NUM,
          km_com           = input$LEAD_DIR_KM_COMP,
          via_let          = input$LEAD_DIR_VIA_LET,
          bis              = input$LEAD_DIR_BIS,
          complemento      = input$LEAD_DIR_VIA_COM,
          cardinalidad     = input$LEAD_DIR_VIA_CARD,
          com_num          = input$LEAD_DIR_COM_NUM,
          com_let          = input$LEAD_DIR_COM_LET,
          com2_num         = input$LEAD_DIR_COM2_NUM,
          com2_card        = input$LEAD_DIR_COM2_CARD,
          compl            = input$LEAD_DIR_DETALLE,
          lng              = data_lead_edit()$lng,
          lat              = data_lead_edit()$lat,
          Comentarios      = input$LEAD_Comentarios,
          Nombres          = lis_nom,
          Cargos           = lis_car,
          Telefonos        = lis_tel,
          Mails            = lis_mail,
          Descartada        = descartar_flag,
          UsuarioDescarte   = usuario_desc,
          FechaHoraDescarte = fecha_desc,
          RazonDescartado   = razon_desc,
          DescartadoPrecio  = precio_desc,
          DescartadoCalidad = calidad_desc,
          
          stringsAsFactors = FALSE
        ) %>%
          mutate(across(where(is.character), ~ str_to_upper(ifelse(. == "", NA_character_, .))))
        
        
        
        AgregarDatos(aux1, "CRMNALLEAD")
        showNotification("Lead modificado exitosamente", duration = 4, type = "message")
        
        # Limpiar datos de contacto
        contact_data$data <- list()
        
        lapply(names(reac_val), reset)
        rm(aux1)
        
        rv$btn <- input$LEAD_Editar
        ret(ret()+1)
        return(list(n = reactive(ret)))
        keypress::press("escape")
        
      }
    })
    
  })
}

# APP DE PRUEBA ----

ui <- bs4DashPage(
  title = "Prueba Formulario Leads",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    # tit = "" ⇒ modo CREAR
    FormularioLeadsUI("lead_test", modo = "crear")
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(btn = NULL)
  
  FormularioLeads(
    id  = "lead_test",
    rv  = rv,
    usr = reactive("CMEDINA"),
    tit = reactive("")
  )
}

shinyApp(ui, server)
# Constantes de configuracion ----
.COT_RMD_PATH <- file.path(
  Sys.getenv("CRM_BASE_PATH",
             "/home/htamara/6_IndustriaNacional/CRM Cliente Nacional/CRMNacional"),
  "cotizacion.Rmd"
)


# Datos de referencia ----

# Tabla estatica de vendedores
VENDEDORES_DATA <- data.frame(
  Asesor = c("CMEDINA", "JGCANON", "LABOYACA", "GACORREDOR"),
  Nombre = c("Carlos Medina",
             "Jonathan Cañon",
             "Luis Boyacá",
             "Gustavo Corredor"),
  Cargo = c("Coordinador Negocios Industria Nacional",
            "Jefe Negocios Industria Nacional",
            "Asesor Comercial",
            "Asesor Comercial"),
  Email = c("cmedina@racafe.com",
            "jgcanon@racafe.com",
            "laboyaca@racafe.com",
            "gacorredor@racafe.com"),
  stringsAsFactors = FALSE
  )

.choices_static <- Choices()

.leads_startup <- CargarDatos("CRMNALLEAD")


# Helpers globales ----

# Helper para valores seguros
safe_val <- function(x) {
  if (EsVacio(x)) "" else as.character(x[[1]])
}

# Helper para conversion numerica segura
safe_num <- function(x) {
  if (EsVacio(x)) 0 else as.numeric(x[[1]])
}

# Helper para formato monetario
fmt_pesos <- function(x) {
  paste0("$", FormatearNumero(x, formato = "numero"))
}


# Producto ----

# UI del producto
ProductoUI <- function(id, num, choices) {
  ns <- NS(id)
  
  div(
    id = ns("wrapper"),
    bs4Dash::bs4Card(title = paste("Producto", num), status = "white", solidHeader = TRUE,
                     width = 12, collapsible = FALSE,
                     fluidRow(column(6,
                                     ListaDesplegable(ns("LinNeg"), label = Obligatorio("Línea de Negocio"), 
                                                      choices = choices$linneg, selected = NULL, multiple = FALSE)
                                     ),
                              column(6, 
                                     ListaDesplegable(ns("Categoria"), label = Obligatorio("Categoría"), 
                                                      choices = c("", choices$categoria), selected = "", 
                                                      multiple = FALSE)
                                     )
                              ),
                     fluidRow(column(6,
                                     pickerInput(ns("Producto"), label = Obligatorio("Producto"), width = "100%",
                                                 choices = "", options = pick_opt(NULL))
                                     ),
                              column(6,
                                     autonumericInput(ns("Cantidad"), label = Obligatorio("Cantidad (Kilos)"), value = NULL,
                                                      decimalPlaces = 1, width = "100%", minimumValue = 1, currencySymbol = " kilos",
                                                      currencySymbolPlacement = "s",
                                                      style = "height: 25px !important; font-size: 14px;")
                                     )
                              ),
                     fluidRow(column(6,
                                     ListaDesplegable(ns("Presentacion"), label = Obligatorio("Presentación"), 
                                                      choices = c("", "Sacos de 70kgs",  "Sacos de 62.5Kgs", "Sacos de 35Kgs", "Grainpro 70kgs"),
                                                      selected = NULL, multiple = FALSE)
                                     ),
                              column(6,
                                     ListaDesplegable(ns("Empaque"), label = Obligatorio("Empaque"), 
                                                      choices = c("", "Blanco #6", "Premarcado #7", "Premarcado #7 Arte del cliente"),
                                                      selected = NULL, multiple = FALSE
                                                      )
                                     )
                              ),
                     fluidRow(column(6,
                                     autonumericInput(ns("Precio"), label = Obligatorio("Precio por Kilo"), value = NULL,
                                                      decimalPlaces = 0, currencySymbol = "$",  width = "100%", 
                                                      style = "height: 25px !important; font-size: 14px;")
                                     ),
                              column(6,
                                     div(style = "margin-top: 25px;",
                                         h6("Total: ", 
                                            span(id = ns("Total"), "$0", style = "font-weight: bold; color: #000;")
                                            )
                                         )
                                     )
                              )
                     )
    )
}
Producto <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    
    # Actualizacion de categorias
    observeEvent(input$LinNeg, ignoreNULL = TRUE, ignoreInit = TRUE, {
      cho_cat <- dat() %>%
        filter(CLLinNegNo == input$LinNeg) %>%
        mutate(Categoria = ifelse(Categoria == "BLEND", "FUERA DE NORMA", Categoria)) %>%
        pull(Categoria) %>%
        Unicos()
        
        updatePickerInput(session, "Categoria", choices = c("", cho_cat), selected = "")
        updatePickerInput(session, "Producto", choices = "", selected = "")
        })
    
    # Actualizacion de productos
    observeEvent(input$Categoria, ignoreNULL = TRUE, ignoreInit = TRUE, {
      
      if (!nzchar(trimws(input$Categoria %||% ""))) {
          
          updatePickerInput(session, "Producto", choices = "", selected = "", options = pick_opt(NULL))
          return(NULL)
        }
        
        lin_neg_actual <- isolate(input$LinNeg %||% "")
        cat_filtro <- ifelse(input$Categoria == "FUERA DE NORMA", "BLEND", input$Categoria)
        
        cho_prod <- tryCatch({
          
          dat() %>%
            filter(CLLinNegNo == lin_neg_actual,
                   Categoria == cat_filtro) %>%
            pull(Producto) %>%
            Unicos()
          
        }, error = function(e) {
          
          character(0)
        })
        
        updatePickerInput(session, "Producto", choices = c("", cho_prod), selected = "", options = pick_opt(NULL))
      })
    
    # Calculo del total del producto
    total_producto <- reactive({
      
      cant <- safe_num(input$Cantidad)
      
      precio <- safe_num(input$Precio)
      
      if (cant > 0 && precio > 0) {
        cant * precio
      } else {
        0
      }
    })
    
    # Actualizacion visual del total
    observeEvent(total_producto(), {
      shinyjs::html("Total", fmt_pesos(total_producto()))
    })
    
    # Retorno reactivo del modulo
    return(
      reactive({
        
        list(LinNeg = input$LinNeg %||% "",
             Categoria = input$Categoria %||% "",
             Producto = input$Producto %||% "",
             Presentacion = input$Presentacion %||% "",
             Empaque = input$Empaque %||% "",
             Cantidad = safe_num(input$Cantidad),
             Precio = safe_num(input$Precio),
             Total = total_producto(),
             Valido = all(nzchar(input$LinNeg %||% ""),
                          nzchar(input$Categoria %||% ""),
                          nzchar(input$Producto %||% ""),
                          nzchar(input$Presentacion %||% ""),
                          nzchar(input$Empaque %||% ""),
                          safe_num(input$Cantidad) > 0,
                          safe_num(input$Precio) > 0
                          )
             )
        })
    )
  })
}


# Modulo cotizacion ----
CotizacionUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      fluidRow(column(12,uiOutput(ns("ui_cliente"))
                      )),
      # Rango de vigencia
      fluidRow(column(6,
                      airDatepickerInput(ns("COT_FechaIni"), label = Obligatorio("Efectiva Desde:"), timepicker = TRUE,
                                         value = Sys.time(), width = "100%")
                      ),
               column(6,
                      airDatepickerInput(ns("COT_FechaFin"), label = Obligatorio("Efectiva Hasta:"), timepicker = TRUE,
                                         value = as.POSIXct(paste(Sys.Date(), "16:00:00")),
                                         width = "100%"
                                         )
                      )
               ),
      # Modalidad del responsable
      fluidRow(
        column(6,
               materialSwitch(inputId = ns("COT_NombrePropio"), label = FormatearTexto("Cotización genérica", tamano_pct = 0.8),
                              value = TRUE, status = "danger", width = "100%"
                              )
               ),
        column(6,
               hidden(
                 div(id = ns("div_responsable"),
                   ListaDesplegable(ns("COT_Responsable"), label = h6("Responsable"), choices = .choices_static$personas,
                                    selected = NULL, multiple = FALSE)
                   )
                 )
               )
        ),
      
      # Condiciones comerciales
      fluidRow(
        column(6,
               ListaDesplegable(
                 ns("COT_Divisa"),
                 Obligatorio("Divisa"),
                 choices = c(
                   "Peso Colombiano",
                   "Dólares"
                 ),
                 selected = "Peso Colombiano",
                 multiple = FALSE
               )
        ),
        
        column(6,
               ListaDesplegable(
                 ns("COT_FPago"),
                 Obligatorio("Forma de Pago"),
                 choices = .choices_static$formapago,
                 selected = "PAGO ANTICIPADO",
                 multiple = FALSE
               )
        )
      ),
      
      tags$hr(),
      
      # Contenedor dinamico de productos
      div(id = ns("productos_container")),
      
      # Controles de productos
      div(
        style = paste0(
          "display: flex; justify-content: flex-end; ",
          "gap: 10px; margin-bottom: 20px;"
        ),
        
        actionBttn(
          inputId = ns("COT_RemoveProducto"),
          label = NULL,
          style = "material-circle",
          color = "warning",
          icon = icon("minus"),
          size = "xs"
        ),
        
        actionBttn(
          inputId = ns("COT_AddProducto"),
          label = NULL,
          style = "material-circle",
          color = "danger",
          icon = icon("plus"),
          size = "xs"
        )
      ),
      
      # Total consolidado
      fluidRow(
        column(12,
               div(
                 style = "text-align: right;",
                 
                 h4(
                   icon("calculator"),
                   "Total General: ",
                   
                   span(
                     id = ns("total_general"),
                     "$0",
                     style = "font-weight: bold; color: #000;"
                   )
                 )
               )
        )
      ),
      
      tags$hr(),
      
      # Boton de descarga
      div(
        style = "text-align:center; margin-top:10px;",
        
        div(
          id = ns("wrapper_btn_crear"),
          
          downloadButton(
            outputId = ns("COT_Crear"),
            label = "Generar Cotización PDF",
            class = "btn btn-danger btn-block disabled"
          )
        )
      )
    )
  )
}
Cotizacion <- function(id, usr, dat, persona_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render reactivo del selector de clientes ----
    output$ui_cliente <- renderUI({
      ListaDesplegable(
        ns("COT_Cliente"), label = Obligatorio("Cliente"),
        choices = persona_r(), selected = NULL, multiple = FALSE
      )
    })
    
    # Cache de forma de pago por NIT ----
    .cache_fpg <- new.env(parent = emptyenv())
    
    obtener_fpg_cliente <- function(nit) {
      clave <- as.character(nit)
      if (exists(clave, envir = .cache_fpg)) return(get(clave, envir = .cache_fpg))
      resultado <- tryCatch(
        ConsultaSistema("syscafe", sprintf(
          "SELECT f.ForPagNom
           FROM   NCLIENT5 c
           LEFT JOIN NFORPAG f ON c.ForPagCod = f.ForPagCod
           WHERE  c.CliNit = '%s'",
          nit
        )) %>% pull(ForPagNom),
        error = function(e) "CONTADO CONTRA ENTREGA"
      )
      assign(clave, resultado, envir = .cache_fpg)
      resultado
    }
    
    # Directorio de clientes precalculado por sesión ----
    .directorio_clientes <- bind_rows_na(
      NCLIENTE %>%
        select(PerRazSoc, CliNitPpal, CliDir, CliDir1, CliTel),
      .leads_startup %>%
        rename(CliNitPpal = CLCliNit) %>%
        select(PerRazSoc, CliNitPpal)
    )
    
    # Helpers de obtención de datos ----
    
    # Máximo consecutivo cacheado; solo consulta BD si aún no se ha inicializado.
    .cache_max_cot <- reactiveVal(NULL)
    
    obtener_max_cotizacion <- function() {
      if (!is.null(.cache_max_cot())) return(.cache_max_cot())
      aux <- CargarDatos("CRMCOTIZACION")
      val <- if (nrow(aux) == 0) 0L else max(aux$ConsCot, na.rm = TRUE)
      .cache_max_cot(val)
      val
    }
    
    obtener_datos_vendedor <- function(asesor, nombre_propio) {
      if (isTRUE(nombre_propio)) {
        return(list(
          Nombre = "Área Comercial Industria Nacional",
          Cargo  = "Área Comercial Industria Nacional",
          Email  = "comercial@racafe.com"
        ))
      }
      vendedor <- VENDEDORES_DATA[VENDEDORES_DATA$Asesor == asesor, ]
      if (nrow(vendedor) == 0) return(NULL)
      list(Nombre = vendedor$Nombre, Cargo = vendedor$Cargo, Email = vendedor$Email)
    }
    
    obtener_datos_cliente <- function(cliente) {
      aux <- .directorio_clientes %>%
        filter(PerRazSoc == cliente) %>%
        slice(1)
      if (nrow(aux) != 1) return(NULL)
      list(
        NIT  = safe_val(aux$CliNitPpal),
        DIR  = safe_val(aux$CliDir),
        DIR1 = safe_val(aux$CliDir1),
        TEL  = safe_val(aux$CliTel),
        FPG  = safe_val(obtener_fpg_cliente(aux$CliNitPpal))
      )
    }
    
    # Manejo de productos dinámicos ----
    productos_modulos  <- reactiveVal(list())
    contador_productos <- reactiveVal(0)
    
    crear_nuevo_producto <- function() {
      contador_productos(contador_productos() + 1)
      nuevo_id <- paste0("producto_", contador_productos())
      
      # UI: ns() correcto → DOM id = "cotizador-producto_1-LinNeg"
      insertUI(
        selector = paste0("#", ns("productos_container")),
        where    = "beforeEnd",
        ui       = ProductoUI(ns(nuevo_id), contador_productos(), .choices_static)
      )
      
      nuevo_modulo <- Producto(nuevo_id, dat)
      
      modulos_actuales <- productos_modulos()
      modulos_actuales[[nuevo_id]] <- nuevo_modulo
      productos_modulos(modulos_actuales)
      
      invisible(nuevo_id)
    }
    
    observe({
      if (contador_productos() == 0) crear_nuevo_producto()
    }) %>% bindEvent(TRUE, once = TRUE)
    
    observeEvent(input$COT_AddProducto, {
      crear_nuevo_producto()
    })
    
    observeEvent(input$COT_RemoveProducto, {
      modulos_actuales <- productos_modulos()
      if (length(modulos_actuales) > 1) {
        ultimo_id <- names(modulos_actuales)[length(modulos_actuales)]
        # M3: removeUI por id rastreado; no selector CSS especulativo.
        removeUI(selector = paste0("#", ns(ultimo_id), "-wrapper"), immediate = TRUE)
        modulos_actuales[[ultimo_id]] <- NULL
        productos_modulos(modulos_actuales)
      }
    })
    
    # Total consolidado ----
    total_general <- reactive({
      modulos <- productos_modulos()
      if (length(modulos) == 0) return(0)
      sum(vapply(modulos, function(m) safe_num(m()$Total), numeric(1)))
    })
    
    observeEvent(total_general(), {
      shinyjs::html("total_general", fmt_pesos(total_general()))
    })
    
    # Validación global del formulario ----
    campos_ok <- reactive({
      basicos_ok <- all(
        nzchar(input$COT_Cliente %||% ""),
        !is.null(input$COT_FechaIni),
        !is.null(input$COT_FechaFin)
      )
      modulos <- productos_modulos()
      if (length(modulos) == 0) return(FALSE)
      productos_ok <- all(vapply(modulos, function(m) isTRUE(m()$Valido), logical(1)))
      basicos_ok && productos_ok
    })
    
    observe({
      selector_a <- paste0("#", ns("wrapper_btn_crear"), " a")
      if (campos_ok()) {
        shinyjs::removeCssClass(selector = selector_a, class = "disabled")
      } else {
        shinyjs::addCssClass(selector = selector_a, class = "disabled")
      }
    })
    
    observe({
      if (isTRUE(input$COT_NombrePropio)) shinyjs::hide(ns("div_responsable"))
      else                                 shinyjs::show(ns("div_responsable"))
    })
    
    # Helpers de construcción de tablas para el RMarkdown ----
    
    # Estructura declarativa fila a fila; sin append() posicional.
    crear_tabla_encabezado <- function(data_cot, data_ven, data_cli) {
      num_cot <- sprintf("%05d", data_cot + 1)
      filas <- list(
        c("Numero Cotización:", num_cot,                               "",              ""),
        c("Efectiva Desde:",    format(input$COT_FechaIni, "%d/%m/%Y %H:%M:%S"),
          "Efectiva Hasta:",    format(input$COT_FechaFin, "%d/%m/%Y %H:%M:%S")),
        c("",                   "",                                    "",              ""),
        c("De:",                data_ven$Nombre %||% "",               "Email:",        data_ven$Email %||% "")
      )
      
      # Campo Cargo solo en cotizaciones no genéricas
      if (!isTRUE(input$COT_NombrePropio)) {
        filas <- c(filas, list(c("Cargo:", data_ven$Cargo %||% "", "", "")))
      }
      
      filas <- c(filas, list(
        c("",              "",                         "",        ""),
        c("Razón Social:", input$COT_Cliente %||% "",  "NIT:",    data_cli$NIT  %||% ""),
        c("Dirección:",    data_cli$DIR       %||% "",  "Tel:",    data_cli$TEL  %||% ""),
        c("País:",         "COLOMBIA",                  "Ciudad:", data_cli$DIR1 %||% "")
      ))
      
      do.call(rbind, lapply(filas, function(f) {
        data.frame(
          Campo1 = f[1], Valor1 = f[2],
          Campo2 = f[3], Valor2 = f[4],
          stringsAsFactors = FALSE
        )
      }))
    }
    
    # Extrae datos crudos de todos los módulos activos; reutilizado por PDF y BD.
    obtener_datos_productos <- function() lapply(productos_modulos(), function(m) m())
    
    # RA3: LimpiarCadena() de racafe reemplaza str_to_upper() + gsub() manuales.
    # rem_numeros/caresp/acentos en FALSE preservan el contenido de la descripción.
    crear_tabla_pdf <- function(datos_lista) {
      productos_df <- do.call(rbind, lapply(datos_lista, function(d) {
        descripcion <- LimpiarCadena(
          paste(
            trimws(safe_val(d$Producto)), "en presentacion de",
            trimws(safe_val(d$Presentacion)), "en empaque",
            trimws(safe_val(d$Empaque))
          ),
          rem_numeros = FALSE,
          rem_caresp  = FALSE,
          rem_acentos = FALSE
        )
        data.frame(
          Descripcion = descripcion,
          Kilos       = safe_num(d$Cantidad),
          Precio_Kilo = safe_num(d$Precio),
          Total       = safe_num(d$Total),
          stringsAsFactors = FALSE
        )
      }))
      rownames(productos_df) <- NULL
      productos_df
    }
    
    # Construye el data.frame completo para un solo AgregarDatos (N productos → 1 insert).
    crear_df_bd <- function(datos_lista, num_consecutivo) {
      do.call(rbind, lapply(datos_lista, function(d) {
        data.frame(
          ConsCot       = num_consecutivo,
          UsuarioCrea   = usr(),
          FechaHoraCrea = Sys.time(),
          Cliente       = input$COT_Cliente     %||% "",
          FechaIni      = as.POSIXct(input$COT_FechaIni),
          FechaFin      = as.POSIXct(input$COT_FechaFin),
          LineaNegocio  = safe_val(d$LinNeg),
          Responsable   = input$COT_Responsable %||% "",
          Categoria     = safe_val(d$Categoria),
          Producto      = safe_val(d$Producto),
          Presentacion  = safe_val(d$Presentacion),
          Empaque       = safe_val(d$Empaque),
          Cantidad      = safe_num(d$Cantidad),
          PrecioKilo    = safe_num(d$Precio),
          TotalProducto = safe_num(d$Total),
          stringsAsFactors = FALSE
        )
      }))
    }
    
    # Generación del PDF y persistencia ----
    output$COT_Crear <- downloadHandler(
      filename = function() {
        cliente_clean <- gsub("[^A-Za-z0-9_]", "_", input$COT_Cliente %||% "cliente")
        paste0("Cotizacion_", cliente_clean, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        # L3: on.exit garantiza waiter_hide en cualquier path de salida (éxito, error o return).
        waiter_show(html = preloader_calculando$html, color = preloader_calculando$color)
        on.exit(waiter_hide(), add = TRUE)
        
        # Guard real del botón; la clase CSS es solo visual.
        if (!campos_ok()) {
          showNotification("Por favor complete todos los campos obligatorios", type = "error")
          return()
        }
        
        tryCatch({
          num_consecutivo  <- obtener_max_cotizacion() + 1L
          data_ven         <- obtener_datos_vendedor(input$COT_Responsable, input$COT_NombrePropio)
          data_cli         <- obtener_datos_cliente(input$COT_Cliente)
          
          if (is.null(data_ven) || is.null(data_cli)) {
            showNotification("Error al obtener datos del vendedor o cliente", type = "error")
            return()
          }
          
          datos_productos  <- obtener_datos_productos()
          tabla_encabezado <- crear_tabla_encabezado(num_consecutivo - 1L, data_ven, data_cli)
          tabla_productos  <- crear_tabla_pdf(datos_productos)
          
          params <- list(
            encabezado    = tabla_encabezado,
            productos     = tabla_productos,
            forma_pago    = input$COT_FPago %||% "PAGO ANTICIPADO",
            total_general = total_general(),
            divisa        = str_to_upper(input$COT_Divisa %||% "PESO COLOMBIANO")
          )
          
          # M1: .COT_RMD_PATH en constante; sin ruta hardcodeada en el código.
          rmarkdown::render(
            input       = .COT_RMD_PATH,
            output_file = file,
            params      = params,
            envir       = new.env(parent = globalenv()),
            quiet       = TRUE
          )
          
          # Un solo insert para todos los productos (N round-trips → 1).
          df_bd <- crear_df_bd(datos_productos, num_consecutivo)
          AgregarDatos(df_bd, "CRMCOTIZACION")
          
          # Actualizar cache solo tras insert exitoso; evita gap de consecutivo.
          .cache_max_cot(num_consecutivo)
          
          showNotification("Cotización generada exitosamente", type = "message")
          limpiar_formulario()
          
        }, error = function(e) {
          showNotification(paste("Error al generar cotización:", e$message), type = "error")
        })
      }
    )
    
    # Limpieza del formulario ----
    # M3: removeUI por id rastreado; sin selector CSS especulativo.
    limpiar_formulario <- function() {
      ids_actuales <- names(productos_modulos())
      for (mid in ids_actuales) {
        removeUI(selector = paste0("#", ns(mid), "-wrapper"), immediate = TRUE)
      }
      productos_modulos(list())
      contador_productos(0)
      
      updatePickerInput(session, "COT_Cliente",         selected = "")
      updatePickerInput(session, "COT_Divisa",          selected = "Peso Colombiano")
      updatePickerInput(session, "COT_FPago",           selected = "PAGO ANTICIPADO")
      updateAirDateInput(session, "COT_FechaIni",       value    = Sys.time())
      updateAirDateInput(session, "COT_FechaFin",       value    = as.POSIXct(paste(Sys.Date(), "16:00:00")))
      updateMaterialSwitch(session, "COT_NombrePropio", value    = TRUE)
      
      crear_nuevo_producto()
    }
  })
}

# App ----

# UI principal de prueba
ui <- bs4DashPage(
  title = "Cotizador — Prueba",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  
  body = bs4DashBody(
    useShinyjs(),
    CotizacionUI("cotizador")
  )
)


# Server principal de prueba
server <- function(input, output, session) {
  
  # Reactivo de clientes disponibles
  persona_r <- reactive({
    Unicos(c(data$PerRazSoc, .leads_startup$PerRazSoc))
  })
  
  # Inicializacion del modulo de cotizacion
  Cotizacion(
    "cotizador",
    reactive("HCYATE"),
    reactive(BaseDatos_c),
    persona_r
  )
}


# Inicializacion de la aplicacion
shinyApp(ui, server)
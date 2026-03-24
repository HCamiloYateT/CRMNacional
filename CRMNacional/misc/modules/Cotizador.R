# Datos previos ----
VENDEDORES_DATA <- data.frame(
  Asesor = c("CMEDINA", "JGCANON", "LABOYACA", "GACORREDOR"),
  Nombre = c("Carlos Medina", "Jonathan Cañon", "Luis Boyacá", "Gustavo Corredor"),
  Cargo = c("Coordinador Negocios Industria Nacional", "Jefe Negocios Industria Nacional", 
            "Asesor Comercial", "Asesor Comercial"),
  Email = c("cmedina@racafe.com", "jgcanon@racafe.com", "laboyaca@racafe.com", "gacorredor@racafe.com"),
  stringsAsFactors = FALSE
)

persona <- c(
  Unicos(data$PerRazSoc),
  Unicos(CargarDatos("CRMNALLEAD") %>% pull(PerRazSoc))
)

# Modulo Producto ----
ProductoUI <- function(id, num, dat) {
  ns <- NS(id)
  div(
    id = ns("wrapper"), 
    bs4Dash::bs4Card(title = paste("Producto", num), status = "white", solidHeader = TRUE, 
                     width = 12, collapsible = FALSE,
                     fluidRow(
                       column(6,
                              ListaDesplegable(ns("LinNeg"), label = Obligatorio("Línea de Negocio"),
                                               choices = Choices()$linneg, selected = NULL, multiple = FALSE)
                              ),
                       column(6,
                              ListaDesplegable(ns("Categoria"), label = Obligatorio("Categoría"),
                                               choices = Choices()$categoria, selected = NULL, multiple = FALSE)
                              )
                       ),
      fluidRow(
        column(6,
               pickerInput(
                 ns("Producto"), label = Obligatorio("Producto"), width = "100%",
                 choices = "", options = pick_opt(NULL)
                 )
               ),
        column(6,
               autonumericInput(
                 ns("Cantidad"), label = Obligatorio("Cantidad (Kilos)"), value = NULL,
                 decimalPlaces = 1, width = "100%", minimumValue = 1, 
                 currencySymbol = " kilos", currencySymbolPlacement = "s", 
                 style = "height: 25px !important; font-size: 14px;"
                 )
               )
        ),
      fluidRow(
        column(6, 
               ListaDesplegable(ns("Presentacion"), label = Obligatorio("Presentacion"), 
                                choices = c("","Sacos de 70kgs", "Sacos de 62.5Kgs", "Sacos de 35Kgs", "Grainpro 70kgs"),
                                selected = NULL, multiple = FALSE)
               ),
        column(6,
               ListaDesplegable(ns("Empaque"), label = Obligatorio("Empaque"),
                                choices = c("","Blanco #6", "Premarcado #7", "Premarcado #7 Arte del cliente"),
                                selected = NULL, multiple = FALSE)
               )
        ),
      fluidRow(
        column(6, 
               autonumericInput(
                 ns("Precio"), label = Obligatorio("Precio por Kilo"), value = NULL,
                 decimalPlaces = 0, currencySymbol = "$", width = "100%",
                 style = "height: 25px !important; font-size: 14px;"
                 )
               ),
        column(6,
               div(style = "margin-top: 25px;",
                   h6("Total: ", 
                      span(id = ns("Total"), "$0", 
                           style = "font-weight: bold; color: #000;"))
                   )
               )
        )
      )
    )
  }
Producto <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Actualizar Categoria y Producto ----
    observeEvent(input$LinNeg, {
      req(input$LinNeg)
      cho_cat <- c("", 
                   dat %>%
                     filter(CLLinNegNo == input$LinNeg) %>%
                     mutate(Categoria = ifelse(Categoria == "BLEND", "FUERA DE NORMA", Categoria)) %>% 
                     pull(Categoria) %>%
                     Unicos())
      
      updatePickerInput(session, "Categoria", choices = cho_cat, selected = NULL)
    })
    observeEvent(input$Categoria, {
      req(input$Categoria)
      
      cat <- ifelse(input$Categoria == "FUERA DE NORMA", "BLEND", input$Categoria)
      
      cho_prod <- c("", 
                    dat %>%  
                      filter(Categoria == cat) %>%
                      pull(Producto) %>%
                      Unicos())
      
      updatePickerInput(session, "Producto", choices = cho_prod, selected = NULL)
    })
    
    # Cálculo de total del producto ----
    total_producto <- reactive({
      if (!is.null(input$Cantidad) && !is.null(input$Precio) && 
          input$Cantidad > 0 && input$Precio > 0) {
        as.numeric(input$Cantidad) * as.numeric(input$Precio)
      } else {
        0
      }
    })
    observe({
      total <- total_producto()
      total_formateado <- paste0("$", format(total, big.mark = ".", decimal.mark = ",", scientific = FALSE))
      shinyjs::html("Total", total_formateado)
    })
    
    # Validación de campos del producto ----
    producto_valido <- reactive({
      all(
        nzchar(input$LinNeg %||% ""),
        nzchar(input$Categoria %||% ""),
        nzchar(input$Producto %||% ""),
        nzchar(input$Presentacion %||% ""),
        nzchar(input$Empaque %||% ""),
        !is.null(input$Cantidad) && input$Cantidad > 0,
        !is.null(input$Precio) && input$Precio > 0
      )
    })
    
    # Retornar valores reactivos del producto
    return(reactive({
      list(
        LinNeg = input$LinNeg %||% "",
        Categoria = input$Categoria %||% "",
        Producto = input$Producto %||% "",
        Presentacion = input$Presentacion %||% "",
        Empaque = input$Empaque %||% "",
        Cantidad = input$Cantidad %||% 0,
        Precio = input$Precio %||% 0,
        Total = total_producto(),
        Valido = producto_valido()
      )
    }))
  })
}

# Modulo Cotizador ----
safe_val <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("")
  as.character(x)
}
CotizacionUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      # Cliente
      fluidRow(
        column(12,
               ListaDesplegable(ns("COT_Cliente"), label = Obligatorio("Cliente"), 
                                choices = persona, selected = NULL, multiple = FALSE)
               )
        ),
      # Fechas
      fluidRow(
        column(6, airDatepickerInput(
          ns("COT_FechaIni"), label = Obligatorio("Efectiva Desde:"), 
          timepicker = TRUE, value = Sys.time(), width = "100%"
        )),
        column(6, airDatepickerInput(
          ns("COT_FechaFin"), label = Obligatorio("Efectiva Hasta:"), 
          timepicker = TRUE, value = as.POSIXct(paste(Sys.Date(), "16:00:00")), 
          width = "100%"
        ))
      ),
      # Switch y responsable
      fluidRow(
        column(6,
               materialSwitch(
                 inputId = ns("COT_NombrePropio"),
                 label = FormatearTexto("Cotización genérica", tamano_pct = 0.8),
                 value = TRUE, status = "danger", width = "100%"
               )
        ),
        column(6,
               hidden(
                 div(id = ns("div_responsable"),
                     ListaDesplegable(ns("COT_Responsable"), label = h6("Responsable"), 
                                      choices = Choices()$personas, selected = NULL, multiple = FALSE
                                      )
                     )
                 )
               )
        ),
      # Divisa y forma de pago
      fluidRow(
        column(6,
               ListaDesplegable(ns("COT_Divisa"), Obligatorio("Divisa"),
                                choices = c("Peso Colombiano", "Dólares"), 
                                selected = "Peso Colombiano", multiple = FALSE)
        ),
        column(6,
               ListaDesplegable(ns("COT_FPago"), Obligatorio("Forma de Pago"),
                                choices = Choices()$formapago , selected = "PAGO ANTICIPADO",
                                multiple = FALSE)
               )
        ),
      tags$hr(),
      # Contenedor dinámico para productos
      div(id = ns("productos_container")),
      # Botones para agregar/eliminar productos
      div(style = "display: flex; justify-content: flex-end; gap: 10px; margin-bottom: 20px;",
          actionBttn(inputId = ns("COT_RemoveProducto"), label = NULL, 
                     style = "material-circle", color = "warning", 
                     icon = icon("minus"), size = "xs"),
          actionBttn(inputId = ns("COT_AddProducto"), label = NULL, 
                     style = "material-circle", color = "danger", 
                     icon = icon("plus"), size = "xs")
      ),
      # Total general
      fluidRow(
        column(12,
               div(style = "text-align: right;",
                   h4(icon("calculator"), "Total General: ", 
                      span(id = ns("total_general"), "$0", 
                           style = "font-weight: bold; color: #000;"))
               )
        )
      ),
      tags$hr(),
      # Botón de generación
      div(
        style = "text-align:center; margin-top:10px;",
        downloadButton(
          outputId = ns("COT_Crear"), 
          label = "Generar Cotización PDF",
          class = "btn btn-danger btn-block"
        )
      )
    )
  )
}
Cotizacion <- function(id, usr, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Funciones ----
    obtener_datos_cotizacion <- function() {
      aux1 <- CargarDatos("CRMCOTIZACION")
      if (nrow(aux1) == 0) {
        list(Num = 0)
      } else {
        list(Num = max(aux1$ConsCot, na.rm = TRUE))
      }
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
      list(
        Nombre = vendedor$Nombre,
        Cargo  = vendedor$Cargo,
        Email  = vendedor$Email
      )
    }
    obtener_datos_cliente <- function(cliente) {
      
      aux1 <- bind_rows(NCLIENTE %>% select(PerRazSoc, CliNitPpal, CliDir, CliDir1, CliTel),
                        CargarDatos("CRMNALLEAD") %>% select(PerRazSoc = PerRazSoc, CliNitPpal = PerCod, 
                                                             CliDir = Depto, CliDir1 = Mpio, CliTel = Telefonos)
                        ) %>% 
        filter(PerRazSoc == cliente) %>% 
        select(PerRazSoc, CliNitPpal, CliDir, CliDir1, CliTel) %>% 
        filter(row_number() == 1)
      
      if (nrow(aux1) != 1) return(NULL)
      
      fpg_query <- sprintf(
        "SELECT f.ForPagNom FROM NCLIENT5 c 
         LEFT JOIN NFORPAG f ON c.ForPagCod = f.ForPagCod
         WHERE c.CliNit = '%s'", aux1$CliNitPpal
      )
      
      fpg <- tryCatch({
        ConsultaSistema("syscafe", fpg_query) %>% pull(ForPagNom)
      }, error = function(e) "CONTADO CONTRA ENTREGA")
      
      list(
        NIT = safe_val(aux1$CliNitPpal),
        DIR = safe_val(aux1$CliDir),
        DIR1 = safe_val(aux1$CliDir1),
        TEL = safe_val(aux1$CliTel),
        FPG = fpg
      )
    }
    
    # Manejo de Productos ----
    productos_modulos <- reactiveVal(list())
    contador_productos <- reactiveVal(0)
    crear_nuevo_producto <- function() {
      contador_productos(contador_productos() + 1)
      nuevo_id <- paste0("producto_", contador_productos())
      
      # Insertar UI del producto
      insertUI(
        selector = paste0("#", ns("productos_container")),
        ui = ProductoUI(ns(nuevo_id), contador_productos(), dat())
      )
      
      # Crear módulo del producto
      nuevo_modulo <- Producto(nuevo_id, dat())
      
      # Agregar a la lista de módulos
      modulos_actuales <- productos_modulos()
      modulos_actuales[[nuevo_id]] <- nuevo_modulo
      productos_modulos(modulos_actuales)
      
      return(nuevo_id)
    }
    
    # Insertar primer producto al inicializar
    observeEvent(TRUE, {
      if (contador_productos() == 0) {
        crear_nuevo_producto()
      }
    }, once = TRUE)
    # Agregar producto
    observeEvent(input$COT_AddProducto, {
      crear_nuevo_producto()
    })
    # Eliminar producto
    observeEvent(input$COT_RemoveProducto, {
      modulos_actuales <- productos_modulos()
      if (length(modulos_actuales) > 1) {
        ultimo_id <- names(modulos_actuales)[length(modulos_actuales)]
        removeUI(selector = paste0("#", ns(ultimo_id), "-wrapper"), immediate = TRUE)
        modulos_actuales[[ultimo_id]] <- NULL
        productos_modulos(modulos_actuales)
      }
    })
    
    # Calculo del Total  ----
    total_general <- reactive({
      modulos <- productos_modulos()
      if (length(modulos) == 0) return(0)
      totales <- sapply(modulos, function(mod) {
        datos <- mod()
        datos$Total
      })
      sum(totales, na.rm = TRUE)
    })
    observe({
      total <- total_general()
      total_formateado <- paste0("$", format(total, big.mark = ".", decimal.mark = ",", scientific = FALSE))
      shinyjs::html("total_general", total_formateado)
    })
    
    # Validación de campos ----
    campos_ok <- reactive({
      basicos_ok <- all(
        nzchar(input$COT_Cliente %||% ""),
        !is.null(input$COT_FechaIni),
        !is.null(input$COT_FechaFin)
      )

      modulos <- productos_modulos()
      if (length(modulos) == 0) return(FALSE)
      
      productos_ok <- all(sapply(modulos, function(mod) {
        datos <- mod()
        datos$Valido
      }))
      
      return(basicos_ok && productos_ok)
    })
    observe({
      if (campos_ok()) {
        shinyjs::enable("COT_Crear")
      } else {
        shinyjs::disable("COT_Crear")
      }
    })
    observe({
      if (isTRUE(input$COT_NombrePropio)) {
        shinyjs::hide("div_responsable")
      } else {
        shinyjs::show("div_responsable")
      }
    })
    
    # Generar PDF de cotizacion -----
    crear_tabla_encabezado <- function(input, data_cot, data_ven, data_cli) {
      campos_base <- c(
        "Numero Cotización:", "",
        "Efectiva Desde:", "Efectiva Hasta:",
        "", "",
        "De:", "Email:",
        "", "",
        "Razón Social:", "NIT:", 
        "Dirección:", "Tel:", 
        "País:", "Ciudad:"
      )
      
      valores_base <- c(
        paste(sprintf("%05d", data_cot$Num + 1)),
        "",
        format(input$COT_FechaIni, "%d/%m/%Y %H:%M:%S"),
        format(input$COT_FechaFin, "%d/%m/%Y %H:%M:%S"),
        "","",
        data_ven$Nombre %||% "",
        data_ven$Email %||% "",
        "", "",
        input$COT_Cliente %||% "",
        data_cli$NIT %||% "",
        data_cli$DIR %||% "",
        data_cli$TEL %||% "",
        "COLOMBIA",
        data_cli$DIR1 %||% ""
      )
      
      # Si NO es cotización genérica, insertar el campo Cargo
      if (!isTRUE(input$COT_NombrePropio)) {
        pos_cargo <- which(campos_base == "De:")
        campos_base <- append(campos_base, "Cargo:", after = pos_cargo)
        valores_base <- append(valores_base, data_ven$Cargo %||% "", after = pos_cargo)
      }
      
      # Asegurar número par de elementos
      if (length(campos_base) %% 2 != 0) {
        campos_base <- c(campos_base, "")
        valores_base <- c(valores_base, "")
      }
      
      data.frame(
        Campo1 = campos_base[seq(1, length(campos_base), 2)],
        Valor1 = valores_base[seq(1, length(valores_base), 2)],
        Campo2 = campos_base[seq(2, length(campos_base), 2)],
        Valor2 = valores_base[seq(2, length(valores_base), 2)],
        stringsAsFactors = FALSE
      )
    }
    crear_tabla_productos <- function() {
      modulos <- productos_modulos()
      if (length(modulos) == 0) return(data.frame())
      
      # Función auxiliar para valores seguros
      safe_val <- function(x) ifelse(is.null(x) || length(x) == 0, "", as.character(x))
      safe_num <- function(x) ifelse(is.null(x) || length(x) == 0, 0, as.numeric(x))
      
      productos_list <- lapply(modulos, function(mod) {
        datos <- mod()
        
        # Crear descripción concatenada con valores seguros
        descripcion <- str_to_upper(
          paste(trimws(safe_val(datos$Producto)), "en presentacion de ",
                trimws(safe_val(datos$Presentacion)), "en empaque ",
                trimws(safe_val(datos$Empaque)),
                sep = " ")
        )
        # Limpiar guiones extras
        descripcion <- gsub("\\s*-\\s*-\\s*", " - ", descripcion)
        descripcion <- gsub("^\\s*-\\s*|\\s*-\\s*$", "", descripcion)
        
        data.frame(
          Descripcion = descripcion,
          Kilos = safe_num(datos$Cantidad),
          Precio_Kilo = safe_num(datos$Precio),
          Total = safe_num(datos$Total),
          stringsAsFactors = FALSE
        )
      })
      
      # Unir todos los productos y eliminar rownames
      resultado <- do.call(rbind, productos_list)
      rownames(resultado) <- NULL  # <<<< ELIMINA LOS ROWNAMES
      
      return(resultado)
    }
    output$COT_Crear <- downloadHandler(
      filename = function() {
        cliente_clean <- gsub("[^A-Za-z0-9_]", "_", input$COT_Cliente)
        paste0("Cotizacion_", cliente_clean, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        # Mostrar preloader
        waiter_show(html = preloader2$html, color = preloader2$color)
        if (!campos_ok()) {
          waiter_hide()
          showNotification("Por favor complete todos los campos obligatorios", type = "error")
          return()
        }
        
        tryCatch({
          # Obtener datos
          data_cot <- obtener_datos_cotizacion()
          data_ven <- obtener_datos_vendedor(input$COT_Responsable, input$COT_NombrePropio)
          data_cli <- obtener_datos_cliente(input$COT_Cliente)
          
          # Validar que se obtuvieron los datos
          if (is.null(data_ven) || is.null(data_cli)) {
            waiter_hide()
            showNotification("Error al obtener datos del vendedor o cliente", type = "error")
            return()
          }
          
          # Crear dataframes para el RMarkdown
          tabla_encabezado <- crear_tabla_encabezado(input, data_cot, data_ven, data_cli)
          tabla_productos <- crear_tabla_productos()
          tabla_forma_pago <- data.frame(Forma_Pago = input$COT_FPago %||% "Contado", stringsAsFactors = FALSE)
          
          # Calcular total general
          total_general_val <- total_general()
          
          # Parámetros para el RMarkdown
          params <- list(
            encabezado = tabla_encabezado,
            productos = tabla_productos,
            forma_pago = tabla_forma_pago,
            total_general = total_general_val,
            divisa = str_to_upper(input$COT_Divisa) %||% "PESO COLOMBIANO"
          )
          
          # Generar PDF
          rmd_path <- file.path(getwd(), "cotizacion.Rmd")
          
          rmarkdown::render(
            input = rmd_path,
            output_file = file,
            params = params,
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )
          
          # Guardar registro en base de datos
          modulos <- productos_modulos()
          for (i in seq_along(modulos)) {
            datos <- modulos[[i]]()
            
            df_cotizacion <- data.frame(
              ConsCot = data_cot$Num + 1,
              UsuarioCrea = usr(),
              FechaHoraCrea = Sys.time(),
              Cliente = input$COT_Cliente,
              FechaIni = as.POSIXct(input$COT_FechaIni),
              FechaFin = as.POSIXct(input$COT_FechaFin),
              LineaNegocio = datos$LinNeg,
              Responsable = input$COT_Responsable,
              Categoria = datos$Categoria,
              Producto = datos$Producto,
              Presentacion = datos$Presentacion,
              Empaque = datos$Empaque,
              Cantidad = as.numeric(datos$Cantidad),
              PrecioKilo = as.numeric(datos$Precio),
              TotalProducto = datos$Total,
              stringsAsFactors = FALSE
            )
            AgregarDatos(df_cotizacion, "CRMCOTIZACION")
          }
          
          # Ocultar preloader y mostrar éxito
          waiter_hide()
          showNotification("Cotización generada exitosamente", type = "message")
          
          # Limpiar formulario
          limpiar_formulario()
          
        }, error = function(e) {
          waiter_hide()
          showNotification(paste("Error al generar cotización:", e$message), type = "error")
        })
      }
    )
    
    # Limpiar Productos ----
    limpiar_formulario <- function() {
      updatePickerInput(session, "COT_Cliente", selected = "")
      updateAirDateInput(session, "COT_FechaIni", value = Sys.time())
      updateAirDateInput(session, "COT_FechaFin", value = as.POSIXct(paste(Sys.Date(), "16:00:00")))
      updateMaterialSwitch(session, "COT_NombrePropio", value = TRUE)
      updateSelectInput(session, "COT_Divisa", selected = "Peso Colombiano")
      updateSelectInput(session, "COT_FPago", selected = "PAGO ANTICIPADO")
      
      removeUI(selector = paste0("#", ns("productos_container"), " > *"), multiple = TRUE)
      productos_modulos(list())
      contador_productos(0)
      
      crear_nuevo_producto()
    }
  })
}

# App de prueba ----
ui <- bs4DashPage(
  title = "Prueba ResumenTotal",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    CotizacionUI("resumen")
  )
)
server <- function(input, output, session) {
  Cotizacion("resumen", reactive("HCYATE"), reactive(BaseDatos))
}

shinyApp(ui, server)

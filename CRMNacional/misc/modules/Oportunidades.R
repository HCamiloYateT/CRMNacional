# Módulo FormularioOportunidad ----
FormularioOportunidadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             box(title = "Identificación", width = 12, status = "white", solidHeader = TRUE, collapsible = TRUE, 
                 fluidRow(
                   column(6,
                          ListaDesplegable(ns("OP_TipoCliente"), label   = Obligatorio("Tipo de Cliente"),
                                           choices = c("", "CLIENTE", "CLIENTE A RECUPERAR", "LEAD"), selected = "",
                                           multiple = FALSE)),
                   column(6,
                          ListaDesplegable(ns("OP_Cliente"), label = Obligatorio("Cliente"), ns = ns,
                                           choices = c(""), multiple = FALSE, fem = FALSE))
                 ),
                 fluidRow(column(6,
                                 ListaDesplegable(ns("OP_LinNeg"),label   = Obligatorio("Línea de Negocio"), ns = ns,
                                                  choices = c("", "A LA MEDIDA", "CONVENCIONALES"),
                                                  selected = "", multiple = FALSE)),
                          column(6, 
                                 ListaDesplegable(ns("OP_Segmento"), label   = Obligatorio("Segmento"), ns = ns,
                                                  choices = c("", "DETAL", "MEDIANO", "GRANDES"), selected = "",
                                                  multiple = FALSE))
                 )
             )
      )
    ),
    fluidRow(
      column(12,
             box(title = "Información del Producto", width = 12, status = "white", solidHeader = TRUE, collapsible = TRUE,
                 fluidRow(
                   column(6,
                          ListaDesplegable(ns("OP_Categoria"), label   = Obligatorio("Categoría"), ns = ns, 
                                           choices = "", selected = NULL, multiple = FALSE)
                   ),
                   column(6,
                          ListaDesplegable(ns("OP_Producto"), label   = Obligatorio("Producto"), ns = ns, choices = "",
                                           selected = NULL, multiple = FALSE))
                 )
             )
      )
    ),
    fluidRow(
      column(12, 
             box(title = "Detalles de la Oportunidad", width = 12, status = "white", solidHeader = TRUE, collapsible = TRUE,
                 fluidRow(
                   column(6,
                          dateInput(ns("OP_Fecha"), label   = Obligatorio("Fecha de Cumplimiento"), 
                                    value   = Sys.Date() + 7, min = Sys.Date(), language = "es",
                                    width   = "100%")
                   ),
                   column(6, 
                          uiOutput(ns("OP_Sacos_UI")),
                          InputNumerico(ns("OP_Frecuencia"), type  = "numero", label = Obligatorio("Frecuencia (días)"), value = NA, dec = 0),
                          InputNumerico(ns("OP_Margen"), type  = "dinero", label = Obligatorio("Márgen por Kilo"), value = NA, dec = 0),
                          br(),
                          uiOutput(ns("OP_Resumen"))
                   )
                 )
             )
      )
    ),
    fluidRow(
      column(12,
             fluidRow(column(10),
                      column(2,
                             actionBttn(inputId = ns("OP_Crear"), label = "Crear Oportunidad", style = "unite",
                                        color = "danger", size = "xs", icon = icon("save"), block   = TRUE))
             )
      )
    )
  )
}
FormularioOportunidad <- function(id, dd_data = reactive(NULL), dat, usr, trigger_update, tipo_cliente_default = reactive("CLIENTE")) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    # Valida Tipo de Cliente ----
    observeEvent(dd_data(), {
    
      tipo_valido <- if (tipo_cliente_default() %in% c("CLIENTE", "CLIENTE A RECUPERAR", "LEAD")) {
        tipo_cliente_default()
      } else {
        ""
      }
      updatePickerInput(
        session,
        inputId = "OP_TipoCliente",
        selected = tipo_valido
      )
    })
    # Actualizar lista de clientes según tipo de cliente ----
    observeEvent(input$OP_TipoCliente, {
      req(input$OP_TipoCliente)
      
      clientes <- if (is.null(dd_data())) {
        
        switch(input$OP_TipoCliente, 
               "CLIENTE" = {
                 dat() %>%
                   filter(SegmentoRacafe == "CLIENTE") %>%
                   pull(PerRazSoc) %>%
                   Unicos()
                 },
               "CLIENTE A RECUPERAR" = {
                 dat() %>%
                   filter(SegmentoRacafe == "CLIENTE A RECUPERAR") %>%
                   pull(PerRazSoc) %>%
                   Unicos()
                 },
               "LEAD" = {
                 CargarDatos("CRMNALLEAD") %>%
                   filter(!is.na(PerRazSoc)) %>%
                   pull(PerRazSoc) %>%
                   Unicos()
                 },
          character(0)
        )
        
      } else {
      extraer_razon_social(dd_data()$fila_completa$PerRazSoc)
      }
      
      updatePickerInput(
        session,
        "OP_Cliente",
        choices  = c("", clientes),
        selected = if (length(clientes) == 1) clientes else ""
      )
    })
    output$OP_Sacos_UI <- renderUI({
      text <- if (!is.null(input$OP_LinNeg) && input$OP_LinNeg != "") {
        dplyr::case_when(input$OP_LinNeg == "CONVENCIONALES" ~ "Sacos (62.5 kgs)",
                         input$OP_LinNeg == "A LA MEDIDA" ~ "Sacos (70 kgs)",
                         TRUE ~ "Sacos")
        } else {"Sacos"
      }
      
      InputNumerico(ns("OP_Sacos"), type = "numero", label = Obligatorio(text), value = NA, dec = 2)
    })
    # Actualizar categorías según línea de negocio ----
    observeEvent(input$OP_LinNeg, {
      req(input$OP_LinNeg)
      cho_cat <- c("",
                   dat() %>%
                     filter(CLLinNegNo == input$OP_LinNeg) %>%
                     pull(Categoria) %>%
                     Unicos())
      
      text <- dplyr::case_when(input$OP_LinNeg == "CONVENCIONALES" ~ "Sacos (62.5 kgs)",
                               input$OP_LinNeg == "A LA MEDIDA" ~ "Sacos (70 kgs)",
                               TRUE ~ "Sacos")
      
      updatePickerInput(session, "OP_Categoria", choices = cho_cat)
    })
    # Actualizar productos según categoría ----
    observeEvent(input$OP_Categoria, {
      req(input$OP_Categoria)
      cho_prod <- c("",
                    dat() %>%
                      filter(Categoria == input$OP_Categoria) %>%
                      pull(Producto) %>%
                      Unicos())
      updatePickerInput(session, "OP_Producto", choices = cho_prod)
    })
    # Actualizar segmento según cliente y línea de negocio ----
    observeEvent(c(input$OP_Cliente, input$OP_LinNeg), {
      req(input$OP_Cliente, input$OP_LinNeg)
      
      seg <- dat() %>%
        filter(PerRazSoc  == input$OP_Cliente,
               CLLinNegNo == input$OP_LinNeg) %>%
        pull(Segmento) %>%
        Unicos()
      
      updatePickerInput(session, "OP_Segmento", selected = seg)
      
    })
    # Resumen de la oportunidad
    output$OP_Resumen <- renderUI({
      # Pedimos los inputs necesarios
      req(input$OP_Sacos, input$OP_LinNeg, input$OP_Frecuencia, input$OP_Margen)
      
      # Si alguno es NA o vacío, no mostramos nada
      if (is.na(input$OP_Sacos) || is.na(input$OP_Frecuencia) || is.na(input$OP_Margen) ||
          input$OP_Sacos == "" || input$OP_Frecuencia == "" || input$OP_Margen == "") {
        return(NULL)
      }
      
      # Determinar peso por saco según Línea de Negocio
      peso_saco <- dplyr::case_when(input$OP_LinNeg == "CONVENCIONALES" ~ 62.5,
                                    input$OP_LinNeg == "A LA MEDIDA" ~ 70,
                                    TRUE ~ NA_real_)
      
      # Si no podemos determinar el peso, no mostramos resumen
      if (is.na(peso_saco)) return(NULL)
      
      # Cálculos
      sacos <- as.numeric(input$OP_Sacos)
      frecuencia <- as.numeric(input$OP_Frecuencia)
      sacos_mes <- sacos / frecuencia * 30
      margen_kg  <- as.numeric(input$OP_Margen)
      margen_saco <- margen_kg * peso_saco
      margen_total <- margen_saco * sacos
      margen_mes <- margen_total / frecuencia * 30
      
      # Texto final en español
      resumen_text <- paste0("Oportunidad por ",
                             FormatearNumero(sacos, formato = "coma", negrita = TRUE), " sacos de ", 
                             FormatearNumero(peso_saco, formato = "numero", negrita = TRUE), " kgs cada ",
                             FormatearNumero(frecuencia, formato = "coma", negrita = TRUE), " días, es decir, ",
                             FormatearNumero(sacos_mes, formato = "numero", negrita = TRUE), " sacos mensuales, dejando un márgen por kilo de ",
                             FormatearNumero(margen_kg, formato = "dinero", negrita = TRUE), " lo que equivale a ", 
                             FormatearNumero(margen_saco, formato = "dinero", negrita = TRUE), " por saco y un margen total de oportunidad de ",
                             FormatearNumero(margen_total, formato = "dinero", negrita = TRUE), " y un estimado mensual de ",
                             FormatearNumero(margen_mes, formato = "dinero", negrita = TRUE)) %>% HTML
      
      tags$div(style = "margin-bottom: 10px; padding: 8px 12px; border-radius: 4px; background-color: #f8f9fa; 
                        border: 1px solid #dee2e6; text-align: justify;",
               resumen_text)
    })
    
    # Crear oportunidad ----
    observeEvent(input$OP_Crear, {
      inputs <- list("OP_TipoCliente" = input$OP_TipoCliente,
                     "OP_Cliente"     = input$OP_Cliente,
                     "OP_LinNeg"      = input$OP_LinNeg,
                     "OP_Categoria"   = input$OP_Categoria,
                     "OP_Segmento"    = input$OP_Segmento,
                     "OP_Producto"    = input$OP_Producto,
                     "OP_Oportunidad" = NA,
                     "OP_Fecha"       = input$OP_Fecha,
                     "OP_Sacos"       = input$OP_Sacos,
                     "OP_Frecuencia"  = input$OP_Frecuencia,
                     "OP_Margen"      = input$OP_Margen)
      
      cond <- c("El campo Tipo de Cliente es obligatorio"   = EsVacio(input$OP_TipoCliente),
                "El campo Cliente es obligatorio"           = EsVacio(input$OP_Cliente),
                "El campo Línea de Negocio es obligatorio"  = EsVacio(input$OP_LinNeg),
                "El campo Categoría es obligatorio"         = EsVacio(input$OP_Categoria),
                "El campo Segmento es obligatorio"          = EsVacio(input$OP_Segmento),
                "El campo Producto es obligatorio"          = EsVacio(input$OP_Producto),
                "El campo Sacos es obligatorio"             = EsVacio(input$OP_Sacos),
                "El campo Frecuencia es obligatorio"        = EsVacio(input$OP_Frecuencia),
                "El campo Márgen por kilo es obligatorio"   = EsVacio(input$OP_Margen))
      
      if (any(cond)) {
        sapply(names(cond), function(mensaje) {
          if (cond[[mensaje]]) {
            showNotification(mensaje, duration = 3, type = "error")
          }
        })
      } else {
        tipo_cliente_bd <- dat() %>%
          filter(PerRazSoc  == input$OP_Cliente,
                 CLLinNegNo == input$OP_LinNeg) %>%
          pull(SegmentoRacafe) %>%
          Unicos()
        
        if (length(tipo_cliente_bd) == 0) {
          tipo_cliente_bd <- NA
        }
        
        aux1 <- data.frame(Usuario          = usr(),
                           FechaHoraCrea    = Sys.time(),
                           TipoClienteOP    = input$OP_TipoCliente,
                           PerRazSoc        = input$OP_Cliente,
                           LineaNegocio     = input$OP_LinNeg,
                           TipoCliente      = tipo_cliente_bd,
                           Segmento         = input$OP_Segmento,
                           Categoria        = input$OP_Categoria,
                           Producto         = input$OP_Producto,
                           Oportunidad      = NA,
                           FechaCumpOP      = input$OP_Fecha,
                           SacosOP          = input$OP_Sacos,
                           FrecuenciaDias   = input$OP_Frecuencia,
                           MargenOP         = input$OP_Margen,
                           Descartada       = FALSE,
                           RazonDescartado  = NA,
                           UsuarioDescarte  = NA,
                           FechaHoraDescarte= NA)
        
        AgregarDatos(aux1, "CRMNALCLOPT")
        showNotification("Oportunidad registrada exitosamente", duration = 3, type = "message")
        lapply(names(inputs), reset)
        
        # Vuelve a dejar el tipo de cliente por defecto después de resetear
        updatePickerInput(session, inputId  = "OP_TipoCliente",
                          selected = if (tipo_cliente_default() %in% c("CLIENTE", "CLIENTE A RECUPERAR", "LEAD"))
                            tipo_cliente_default else "")
        trigger_update(trigger_update() + 1)
      }
    })
  })
}
## App de prueba
ui <- bs4DashPage(
  title = "Prueba ResumenTotal",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    FormularioOportunidadUI("resumen")
  )
)

server <- function(input, output, session) {
  FormularioOportunidad("resumen", dat = reactive({BaseDatos}), usr = reactive("CMEDINA"), trigger_update = reactive(0))
}

shinyApp(ui, server)





## Módulo Descartar Oportunidad ----
actualizar_descartado <- function(razon_social, linea_negocio, descartada, razon_descartado, usuario_descarte,
                                  descartado_precio  = NA, descartado_calidad = NA,fecha_hora = Sys.time()) {
  
  razon_social     <- gsub("'", "''", razon_social)
  linea_negocio    <- gsub("'", "''", linea_negocio)
  razon_descartado <- gsub("'", "''", razon_descartado)
  usuario_descarte <- gsub("'", "''", usuario_descarte)
  descartado_calidad <- gsub("'", "''", descartado_calidad)
  
  con <- ConectarBD()
  on.exit(DBI::dbDisconnect(con))
  
  query <- glue::glue("UPDATE CRMNALCLOPT
                       SET  Descartada          = {descartada},
                            RazonDescartado     = '{razon_descartado}',
                            DescartadoPrecio    = {ifelse(is.na(descartado_precio), 'NULL', descartado_precio)},
                            DescartadoCalidad   = {ifelse(is.na(descartado_calidad), 'NULL', glue::glue(\"'{descartado_calidad}'\"))},
                            UsuarioDescarte     = '{usuario_descarte}',
                            FechaHoraDescarte   = '{as.character(fecha_hora)}'
                        WHERE PerRazSoc = '{razon_social}' AND LineaNegocio = '{linea_negocio}';")
  
  DBI::dbExecute(con, query)
  invisible(TRUE)
}

DescartarOportunidadUI <- function(id) {
  ns <- NS(id)
  tagList(div(style="width: 80%; margin: 0 auto; padding: 20px;",
              div(class="alert alert-warning",style="display: flex; align-items: center; gap: 10px;",
                  icon("exclamation-triangle"),strong("Advertencia: "),
                  "Esta operación no se puede deshacer."),
              ListaDesplegable(inputId=ns("razon"),label="Razón para descartar la oportunidad:",
                               choices=Choices()$raz_descarte,selected="",multiple=FALSE,ns=ns),
              conditionalPanel(condition=sprintf("input['%s'] == 'PRECIO'",ns("razon")),
                               InputNumerico(id=ns("precio"),label="Precio ofrecido por kilo",
                                             type="dinero",value=NA,dec=0)),
              conditionalPanel(condition=sprintf("input['%s'] == 'CALIDAD'",ns("razon")),
                               ListaDesplegable(inputId=ns("calidad"),label="Motivo asociado a calidad:",
                                                choices=Choices()$producto,selected=NULL,
                                                multiple=FALSE,ns=ns)),
              br(),
              actionButton(ns("confirmar"),"Confirmar Descarte",
                           icon=icon("ban"),class="btn-danger",
                           style="width: 100%;")
  ))
}
DescartarOportunidad <- function(id, dd_data, usr, trigger) {
  moduleServer(id,function(input,output,session) {
    
    observe({
      print("Prueba Descartar")
      print(dd_data())
    })
    
    observeEvent(input$confirmar,{
      req(is.function(dd_data))
      sel <- dd_data()
      req(!is.null(sel))
      
      req(input$razon)
      # VALIDACIONES ----
      cond <- c("La razón de descarte es obligatoria" = is.null(input$razon)|| input$razon=="",
                "El precio es obligatorio cuando la razón de descarte es precio"= (input$razon=="PRECIO" && is.na(input$precio)),
                "El motivo de calidad es obligatorio cuando la razón de descarte es calidad" = (input$razon=="CALIDAD" && (is.null(input$calidad)||input$calidad==""))
      )
      if(any(cond)){
        sapply(names(cond),function(m)
          if(cond[[m]])showNotification(m,duration=3,type="error"))
        return()
      }
      
      req(is.data.frame(sel$data),nrow(sel$data)>=1)
      
      cliente <- extraer_info_cliente(sel$data$PerRazSoc %||% sel$data$Cliente)
      
      res <- try({
        actualizar_descartado(
          razon_social     = cliente$razon_social,
          linea_negocio    = cliente$linea_negocio,
          descartada       = 1,
          razon_descartado = input$razon,
          usuario_descarte = if (is.reactive(usr)) usr() else usr,
          descartado_precio  = if(input$razon=="PRECIO") input$precio else NA,
          descartado_calidad = if(input$razon=="CALIDAD") input$calidad else NA
        )
      },silent=TRUE)
      
      if(inherits(res,"try-error")){
        showNotification("Error al actualizar la oportunidad",type="error")
        return()
      }
      
      if(is.reactive(trigger))trigger(trigger()+1)
      removeModal()
      showNotification("Oportunidad descartada exitosamente",
                       type="message",duration=3)
    })
  })
}

## Módulo Detalle de Oportunidad ----

extraer_oportunidad <- function(base) {
  base %>%
    dplyr::slice(1) %>%
    dplyr::select(
      Usuario, PerRazSoc, LineaNegocio, Segmento, Categoria, Producto,
      FechaHoraCrea, FechaCumpOP, FrecuenciaDias, SacosOP, MargenTotalOP
    )
}
preparar_facturacion <- function(base, fecha_crea, frec_dias) {
  
  frec_dias <- ifelse(is.na(frec_dias) || frec_dias <= 0, NA_real_, frec_dias)
  
  base %>%
    dplyr::filter(!is.na(FecFact)) %>%
    dplyr::group_by(FecFact) %>%
    dplyr::summarise(
      SacFact = sum(as.numeric(SacFact), na.rm = TRUE),
      Margen  = sum(as.numeric(Margen),  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(FecFact) %>%
    dplyr::mutate(
      CumSac  = cumsum(SacFact),
      CumMarg = cumsum(Margen),
      Dias    = as.numeric(FecFact - fecha_crea),
      Tnorm   = Dias / frec_dias
    )
}
calcular_cumplimiento <- function(fac, sacos_obj, margen_obj, frec_dias) {
  fac_h <- fac %>% dplyr::filter(Dias <= frec_dias)
  
  sac <- ifelse(nrow(fac_h) > 0, max(fac_h$CumSac,  na.rm = TRUE), 0)
  mar <- ifelse(nrow(fac_h) > 0, max(fac_h$CumMarg, na.rm = TRUE), 0)
  
  tibble::tibble(
    sacos_cumplidos = sac,
    margen_cumplido = mar,
    pct_sacos       = ifelse(sacos_obj  > 0, sac / sacos_obj,  NA_real_),
    pct_margen      = ifelse(margen_obj > 0, mar / margen_obj, NA_real_)
  )
}
normalizar_30_dias <- function(fac, sacos_op, margen_op, frec_dias) {
  fac %>%
    dplyr::mutate(
      Dia30        = Tnorm * 30,
      PlanSacos30  = sacos_op  / frec_dias * 30,
      PlanMargen30 = margen_op / frec_dias * 30
    )
}
proyectar_lineal <- function(df, x, y, x_max, n = 60) {
  df <- df %>% dplyr::filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
  if (nrow(df) < 2 || length(unique(df[[x]])) < 2) return(NULL)
  
  fit <- stats::lm(df[[y]] ~ df[[x]])
  grid <- data.frame(x = seq(min(df[[x]]), x_max, length.out = n))
  grid$y <- stats::predict(fit, newdata = grid)
  grid
}

DetalleOportunidadUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    # RESUMEN (solo texto) ----
    shiny::fluidRow(
      shiny::column(
        12,
        bs4Dash::box(
          title = "Resumen de la oportunidad",
          width = 12, status = "white", solidHeader = TRUE,
          shiny::uiOutput(ns("card_html"))
        )
      )
    ),
    
    # CUMPLIMIENTO GENERAL ----
    shiny::fluidRow(
      shiny::column(
        12,
        bs4Dash::box(
          title = "Cumplimiento general de la oportunidad",
          width = 12, status = "white", solidHeader = TRUE,
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput(ns("plot_sacos_op"),  height = "320px")),
            shiny::column(6, plotly::plotlyOutput(ns("plot_margen_op"), height = "320px"))
          )
        )
      )
    ),
    
    # NORMALIZADO 30 DÍAS ----
    shiny::fluidRow(
      shiny::column(
        12,
        bs4Dash::box(
          title = "Estandarización a 30 días",
          width = 12, status = "white", solidHeader = TRUE,
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput(ns("plot_sacos_30"),  height = "320px")),
            shiny::column(6, plotly::plotlyOutput(ns("plot_margen_30"), height = "320px"))
          )
        )
      )
    ),
    
    # DETALLE FACTURACIÓN ----
    shiny::fluidRow(
      shiny::column(
        12,
        bs4Dash::box(
          title = "Detalle de facturación",
          width = 12, status = "white", solidHeader = TRUE,
          gt_output(ns("tabla_fact"))
        )
      )
    )
  )
}
DetalleOportunidad <- function(id, dd_data, data_op_completa) {
  shiny::moduleServer(id, function(input, output, session) {
    
    op_data <- shiny::reactive({
      req(is.function(data_op_completa))
      dd <- dd_data()
      req(!is.null(dd), nrow(dd$fila_gt) == 1)
      
      f <- dd$fila_gt
      base <- data_op_completa()
      
      base <- base %>%
        dplyr::filter(
          Categoria == f$Categoria,
          Producto  == f$Producto
        ) %>%
        dplyr::mutate(
          FechaHoraCrea = as.POSIXct(FechaHoraCrea),
          FechaCumpOP   = as.Date(FechaCumpOP),
          FecFact       = as.Date(FecFact)
        )
      
      req(nrow(base) > 0)
      
      op <- extraer_oportunidad(base)
      
      fecha_crea <- as.Date(op$FechaHoraCrea)
      fecha_cump <- as.Date(op$FechaCumpOP)
      frec_dias  <- as.numeric(op$FrecuenciaDias)
      frec_dias  <- ifelse(is.na(frec_dias) || frec_dias <= 0,
                           as.numeric(fecha_cump - fecha_crea), frec_dias)
      
      fac <- preparar_facturacion(base, fecha_crea, frec_dias)
      
      list(
        op           = op,
        fac          = fac,
        fac_30       = normalizar_30_dias(
          fac, op$SacosOP, op$MargenTotalOP, frec_dias
        ),
        cumplimiento = calcular_cumplimiento(
          fac, op$SacosOP, op$MargenTotalOP, frec_dias
        ),
        fecha_crea   = fecha_crea,
        fecha_cump   = fecha_cump
      )
    })
    
    observe({
      print(op_data())
    })
    
    # Resumen oportunidad ----
    output$card_html <- shiny::renderUI({
      x   <- op_data()
      op  <- x$op
      fac <- x$fac
      cum <- x$cumplimiento
      
      # Observado acumulado
      sacos_real  <- ifelse(nrow(fac) > 0, max(fac$CumSac,  na.rm = TRUE), 0)
      margen_real <- ifelse(nrow(fac) > 0, max(fac$CumMarg, na.rm = TRUE), 0)
      
      # Frecuencia y duración real de la oferta
      frecuencia_dias <- as.numeric(op$FrecuenciaDias)
      
      fecha_creacion     <- as.Date(op$FechaHoraCrea)
      fecha_cumplimiento <- as.Date(op$FechaCumpOP)
      
      dias_cumplimiento <- as.numeric(fecha_cumplimiento - fecha_creacion)
      
      # Estandarización mensual (30 días)
      sacos_30  <- NA_real_
      margen_30 <- NA_real_
      
      if (nrow(fac) >= 2 && length(unique(fac$Dias)) > 1) {
        sacos_30  <- coef(stats::lm(CumSac  ~ Dias, data = fac))[2] * 30
        margen_30 <- coef(stats::lm(CumMarg ~ Dias, data = fac))[2] * 30
      }
      
      # Plan mensual implícito
      sacos_mes_plan  <- as.numeric(op$SacosOP)       / frecuencia_dias * 30
      margen_mes_plan <- as.numeric(op$MargenTotalOP) / frecuencia_dias * 30
      
      # Cumplimiento mensual estandarizado (%)
      pct_sacos_30  <- sacos_30  / sacos_mes_plan  * 100
      pct_margen_30 <- margen_30 / margen_mes_plan * 100
      
      shiny::HTML(paste0(
        "<div style='display:flex;gap:24px;flex-wrap:wrap;'>",
        
        "<div>",
        "<b>Cliente:</b> ", op$PerRazSoc, "<br>",
        "<b>Línea:</b> ", op$LineaNegocio, "<br>",
        "<b>Categoría:</b> ", op$Categoria, "<br>",
        "<b>Producto:</b> ", op$Producto, "<br>",
        "<b>Fecha de creación:</b> ", as.character(fecha_creacion), "<br>",
        "<b>Fecha de cumplimiento:</b> ", as.character(fecha_cumplimiento), "<br>",
        "<b>Frecuencia comercial (días):</b> ",
        FormatearNumero(frecuencia_dias, formato = "numero"), "<br>",
        "<b>Días para cumplir la oferta:</b> ",
        FormatearNumero(dias_cumplimiento, formato = "numero"),
        "</div>",
        
        "<div>",
        "<b>Meta sacos:</b> ",
        FormatearNumero(op$SacosOP, formato = "numero"), "<br>",
        "<b>Meta margen:</b> ",
        FormatearNumero(op$MargenTotalOP, formato = "dinero"), "<br><br>",
        
        "<b>Cumplimiento sacos (total):</b> ",
        FormatearNumero(cum$pct_sacos * 100, formato = "porcentaje"), "%<br>",
        "<b>Cumplimiento margen (total):</b> ",
        FormatearNumero(cum$pct_margen * 100, formato = "porcentaje"), "%<br><br>",
        
        "<b>Sacos estandarizados / 30 días:</b> ",
        FormatearNumero(sacos_30, formato = "numero"), " (",
        FormatearNumero(pct_sacos_30, formato = "porcentaje"), "%)<br>",
        
        "<b>Margen estandarizado / 30 días:</b> ",
        FormatearNumero(margen_30, formato = "dinero"), " (",
        FormatearNumero(pct_margen_30, formato = "porcentaje"), "%)",
        "</div>",
        
        "</div>"
      ))
    })
    
    
    # Plots oportunidad Total ----
    output$plot_sacos_op <- plotly::renderPlotly({
      x   <- op_data()
      fac <- x$fac
      op  <- x$op
      
      fmt_num <- FormatoD3("numero")
      
      p <- plotly::plot_ly() %>%
        plotly::add_lines(
          data = fac,
          x = ~FecFact,
          y = ~CumSac,
          mode = "lines+markers",
          name = "Sacos facturados",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_num, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::add_lines(
          x = c(min(fac$FecFact), max(fac$FecFact)),
          y = c(op$SacosOP, op$SacosOP),
          name = "Meta de sacos",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_num, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::layout(
          title = "Cumplimiento de sacos",
          xaxis = list(
            title = "Fecha de facturación",
            tickformat = "%b %Y"
          ),
          yaxis = list(
            title = "Sacos",
            tickformat = fmt_num
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          hovermode = "x unified"
        )
      
      plotly::config(
        p,
        displayModeBar = FALSE,
        locale = "es"
      )
    })
    output$plot_margen_op <- plotly::renderPlotly({
      x   <- op_data()
      fac <- x$fac
      op  <- x$op
      
      fmt_dinero <- FormatoD3("dinero")
      
      p <- plotly::plot_ly() %>%
        plotly::add_lines(
          data = fac,
          x = ~FecFact,
          y = ~CumMarg,
          mode = "lines+markers",
          name = "Margen facturado",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_dinero, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::add_lines(
          x = c(min(fac$FecFact), max(fac$FecFact)),
          y = c(op$MargenTotalOP, op$MargenTotalOP),
          name = "Meta de margen",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_dinero, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::layout(
          title = "Cumplimiento de margen ($)",
          xaxis = list(
            title = "Fecha de facturación",
            tickformat = "%b %Y"
          ),
          yaxis = list(
            title = "Margen ($)",
            tickformat = fmt_dinero
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          hovermode = "x unified"
        )
      
      plotly::config(
        p,
        displayModeBar = FALSE,
        locale = "es"
      )
    })
    
    # Plots oportunidad mensualizada ----
    output$plot_sacos_30 <- plotly::renderPlotly({
      fac <- op_data()$fac_30
      fmt_num <- FormatoD3("numero")
      
      p <- plotly::plot_ly() %>%
        plotly::add_bars(
          data = fac,
          x = ~FecFact,
          y = ~CumSac,
          name = "Sacos facturados (equivalente 30 días)",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_num, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::add_lines(
          x = c(min(fac$FecFact), max(fac$FecFact)),
          y = c(fac$PlanSacos30[1], fac$PlanSacos30[1]),
          name = "Meta estandarizada (30 días)",
          hovertemplate = paste0(
            "Meta 30 días<br>",
            "%{y:", fmt_num, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::layout(
          title = "Estandarización a 30 días – Sacos",
          xaxis = list(
            title = "Mes de facturación",
            tickformat = "%b %Y"
          ),
          yaxis = list(
            title = "Sacos",
            tickformat = fmt_num
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          hovermode = "x unified",
          barmode = "group"
        )
      
      plotly::config(p, displayModeBar = FALSE, locale = "es")
    })
    output$plot_margen_30 <- plotly::renderPlotly({
      fac <- op_data()$fac_30
      fmt_dinero <- FormatoD3("dinero")
      
      p <- plotly::plot_ly() %>%
        plotly::add_bars(
          data = fac,
          x = ~FecFact,
          y = ~CumMarg,
          name = "Margen facturado (equivalente 30 días)",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_dinero, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::add_lines(
          x = c(min(fac$FecFact), max(fac$FecFact)),
          y = c(fac$PlanMargen30[1], fac$PlanMargen30[1]),
          name = "Meta estandarizada (30 días)",
          hovertemplate = paste0(
            "Meta 30 días<br>",
            "%{y:", fmt_dinero, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::layout(
          title = "Estandarización a 30 días – Margen ($)",
          xaxis = list(
            title = "Mes de facturación",
            tickformat = "%b %Y"
          ),
          yaxis = list(
            title = "Margen ($)",
            tickformat = fmt_dinero
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          hovermode = "x unified",
          barmode = "group"
        )
      
      plotly::config(p, displayModeBar = FALSE, locale = "es")
    })
    
    # Tabla de Facturación ----
    output$tabla_fact <- gt::render_gt({
      fac <- op_data()$fac
      req(nrow(fac) > 0)
      
      fac %>%
        dplyr::select(
          FecFact,
          SacFact,
          CumSac,
          Margen,
          CumMarg
        ) %>%
        dplyr::mutate(
          FecFact = as.Date(FecFact)
        ) %>%
        gt::gt() %>%
        gt::cols_label(
          FecFact = "Fecha de facturación",
          SacFact = "Sacos facturados",
          CumSac  = "Sacos acumulados",
          Margen  = "Margen facturado",
          CumMarg = "Margen acumulado"
        ) %>%
        gt::fmt_number(
          columns = c(SacFact, CumSac),
          decimals = 0
        ) %>%
        gt::fmt_currency(
          columns = c(Margen, CumMarg),
          currency = "COP",
          decimals = 0
        ) %>%
        gt_minimal_style()
    })
    
    
    invisible(NULL)
  })
}


# Módulo Tabla Oportunidades ----
TablaOportunidadesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             box(title = "Filtros de Cumplimiento", width = 12, status = "white",
                 solidHeader = TRUE, collapsible = TRUE,
                 fluidRow(
                   tags$div(style = "width:20%; float:left;",
                            materialSwitch(
                              inputId = ns("OP_CumplidoSacosTOT"),
                              label = FormatearTexto("Cumplidas Sacos (Total)", 0.9),
                              value = FALSE, status = "danger", width = "100%")),
                   tags$div(style = "width:20%; float:left;",
                            materialSwitch(
                              inputId = ns("OP_CumplidoMargenTOT"),
                              label = FormatearTexto("Cumplidas Márgen (Total)", 0.9),
                              value = FALSE, status = "danger", width = "100%")),
                   tags$div(style = "width:20%; float:left;",
                            materialSwitch(
                              inputId = ns("OP_CumplidoSacosPER"),
                              label = FormatearTexto("Cumplidas Sacos (Periódica)", 0.9),
                              value = FALSE, status = "danger", width = "100%")),
                   tags$div(style = "width:20%; float:left;",
                            materialSwitch(
                              inputId = ns("OP_CumplidoMargenPER"),
                              label = FormatearTexto("Cumplidas Márgen (Periódica)", 0.9),
                              value = FALSE, status = "danger", width = "100%"))
                 )
             )
      )
    ),
    fluidRow(
      column(12,
             box(title = "Oportunidades Activas", width = 12, status = "white",
                 solidHeader = TRUE, collapsible = TRUE,
                 GTBotonesUI(ns("tabla_activas"))
             )
      )
    ),
    fluidRow(
      column(12,
             box(title = "Oportunidades Descartadas", width = 12, status = "white",
                 solidHeader = TRUE, collapsible = TRUE,
                 gt_output(ns("tabla_descartadas"))
             )
      )
    )
  )
}
TablaOportunidades <- function(id, data_op, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    
    data_op_completa <- reactive({
      trigger_update()
      req(data_op())
      
      data_op() %>%
        group_by(Usuario, FechaHoraCrea, TipoClienteOP, PerRazSoc, LineaNegocio, TipoCliente, Segmento, 
                 Categoria, Producto, FechaCumpOP, Descartada, Cliente) %>%
        summarise(SacosOP        = max(SacosOP),
                  Sacos70        = max(Sacos70),
                  Kilos          = max(Kilos),
                  MargenTotalOP  = max(MargenTotalOP),
                  MargenKiloOP   = max(MargenOP),
                  FrecuenciaOP   = max(FrecuenciaDias),
                  SacosTOT       = sum(if_else(is.na(FecFact) | (FecFact >= FechaHoraCrea & FecFact <= FechaCumpOP),
                                               SacFact, 0), na.rm = TRUE),
                  CumpSacosTOT   = SiError_0(SacosTOT / SacosOP),
                  PendSacosTOT   = CumpSacosTOT < 1,
                  MargenTOT      = sum(if_else(is.na(FecFact) | (FecFact >= FechaHoraCrea & FecFact <= FechaCumpOP),
                                               Margen, 0),na.rm = TRUE),
                  CumpMargenTOT  = SiError_0(MargenTOT / MargenTotalOP),
                  PendMargenTOT  = CumpMargenTOT < 1,
                  MargenKiloTOT  = SiError_0(MargenTOT / (SacosTOT * Kilos)),
                  CumpMargenKiloTOT = SiError_0(MargenKiloTOT / MargenKiloOP),
                  SacosMes       = max(SacosMes),
                  MargenMes      = max(MargenMes),
                  MargenKiloMes  = SiError_0(MargenMes / (SacosMes * Kilos)),
                  MesMin = suppressWarnings(min(if_else(is.na(FecFact) | FecFact >= FechaHoraCrea, 
                                                        FecFact, as.Date(NA)), na.rm = TRUE)),
                  MesMax = suppressWarnings(max(if_else(is.na(FecFact) | FecFact >= FechaHoraCrea,
                                                        FecFact, as.Date(NA)),na.rm = TRUE)),
                  NumMeses = SiError_0(round(as.numeric(difftime(MesMax, MesMin, units = "days")) / 30)),
                  SacosPER = SiError_0(SacosTOT / NumMeses),
                  CumpSacosPER = SiError_0(SacosPER / SacosMes),
                  PendSacosPER = CumpSacosPER < 1,
                  MargenPER = SiError_0(MargenTOT / NumMeses),
                  CumpMargenPER = SiError_0(MargenPER / MargenMes),
                  PendMargenPER = CumpMargenPER < 1,
                  MargenKiloPER = SiError_0(MargenPER / (SacosMes * Kilos)),
                  CumpMargenKiloPER = SiError_0(MargenKiloPER / MargenKiloMes),
                  .groups = "drop") %>% 
        mutate(Descartada = if_else(is.na(Descartada), FALSE, Descartada))
    })
    data_activas <- reactive({
      data_op_completa() %>% filter(Descartada == FALSE)
    })
    data_descartadas <- reactive({
      data_op_completa() %>% filter(Descartada == TRUE)
    })
    make_gt <- function(df) {
      if (nrow(df) == 0)
        return(gt_mensaje_vacio("No hay oportunidades para los filtros seleccionados"))
      
      aux <- df %>%
        select(Cliente, LineaNegocio, Categoria, Producto, Descartada, FechaCumpOP,
               SacosOP, Sacos70, MargenTotalOP, FrecuenciaOP,
               CumpSacosTOT, CumpMargenTOT, CumpMargenKiloTOT,
               CumpSacosPER, CumpMargenPER, CumpMargenKiloPER) %>%
        mutate(Descartada = ifelse(Descartada == 1, "SI", "NO"))
      
      gt::gt(aux) %>%
        gt::tab_spanner(label = "Oportunidad de Negocio", columns = 1:9) %>%
        gt::tab_spanner(label = "Cumplimiento Total", columns = 10:12) %>%
        gt::tab_spanner(label = "Cumplimiento Periódico", columns = 13:15) %>%
        gt::tab_header(
          title = "Oportunidades de Negocio",
          subtitle = paste("Total Oportunidades:", nrow(aux))
        ) %>%
        gt::cols_label(
          Cliente            = "Cliente",
          LineaNegocio       = "Línea de Negocio",
          Categoria          = "Categoría",
          Producto           = "Producto",
          FechaCumpOP        = "Fecha Cump. Oportunidad",
          SacosOP            = "Sacos Oportunidad",
          Sacos70            = "Sacos (70Kgs)",
          MargenTotalOP      = "Márgen Total Oportunidad",
          FrecuenciaOP       = "Frecuencia (días)",
          CumpSacosTOT       = "Cumpl. Sacos (%)",
          CumpMargenTOT      = "Cumpl. Márgen (%)",
          CumpMargenKiloTOT  = "Cumpl. Márgen/Kilo (%)",
          CumpSacosPER       = "Cumpl. Sacos Mes (%)",
          CumpMargenPER      = "Cumpl. Márgen Mes (%)",
          CumpMargenKiloPER  = "Cumpl. Márgen/Kilo Mes (%)"
        ) %>%
        gt::fmt_number(columns = c(SacosOP, FrecuenciaOP), decimals = 0) %>%
        gt::fmt_number(columns = c(Sacos70), decimals = 2) %>%
        gt::fmt_currency(columns = c(MargenTotalOP), currency = "COP", decimals = 0) %>%
        gt::fmt_percent(
          columns = c(
            CumpSacosTOT, CumpMargenTOT, CumpMargenKiloTOT,
            CumpSacosPER, CumpMargenPER, CumpMargenKiloPER
          ),
          decimals = 2
        ) %>%
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(columns = c(CumpSacosTOT, CumpMargenTOT))
        ) %>%
        gt::tab_style(
          style = gt::cell_text(color = "red", weight = "bold"),
          locations = gt::cells_body(columns = FechaCumpOP, rows = FechaCumpOP < Sys.Date())
        ) %>%
        gt::fmt_markdown(columns = Cliente) %>%  
        gt_minimal_style() %>%
        gt_pct_style(
          CumpSacosTOT, CumpMargenTOT, CumpMargenKiloTOT,
          CumpSacosPER, CumpMargenPER, CumpMargenKiloPER
        ) %>%
        gt::opt_interactive(use_pagination = FALSE, use_filters = FALSE)
    }
    
    # ACTIVAS -----
    # Configuración de botones ----
    botones_config <- crear_config_botones(
      detalle = list(titulo        = "Ver Detalle",
                     tit_modal     = "Detalle Oportunidad -",
                     module_ui     = "DetalleOportunidadUI",
                     module_server = "DetalleOportunidad",
                     module_id     = "mod_detalle",
                     posicion      = "inicio",
                     extra_params  = list(data_op_completa = data_op)),
      descartar = list(titulo        = "Descartar Oportunidad",
                       tit_modal     = "Descartar Oportunidad -",
                       module_ui     = "DescartarOportunidadUI",
                       module_server = "DescartarOportunidad",
                       module_id     = "mod_descartar",
                       posicion      = "inicio",
                       extra_params  = list(usr = usr,
                                            trigger = trigger_update))
      )
    
    # # Reactivos de drilldown para los sub-módulos ----
    # dd <- setNames(
    #   lapply(names(botones_config), function(x) reactiveVal(NULL)),
    #   paste0(names(botones_config), "_dd")
    # )
    # Llamada de la parte UI de los sub-módulos ----
    modulos_ui <- setNames(
      lapply(botones_config, function(cfg) {
        ui_fun <- get(cfg$module_ui)
        function(ns_parent) ui_fun(ns_parent(cfg$module_id))
      }),
      paste0(names(botones_config), "_module")
    )

    # Llamado módulo de tabla gt ----
    resultado <- GTBotones(id             = "tabla_activas",
                           gt_table       = reactive(make_gt(data_activas())),
                           data           = data_activas,
                           botones_config = botones_config,
                           nombre_col     = c("Cliente")
                           )
    
    # Asignación de datos según click ----
    # observe({
    #   req(resultado$dd_data())
    #   data_sel <- resultado$dd_data()
    #   dd_name <- paste0(data_sel$accion, "_dd")
    #   if (dd_name %in% names(dd)) {
    #     dd[[dd_name]](data_sel)
    #   }
    # })


    # DESCARTADAS -----
    output$tabla_descartadas <- render_gt({
      make_gt(data_descartadas())
    })

  })
}

## App de prueba
ui <- bs4DashPage(
  title = "Prueba ResumenTotal",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    TablaOportunidadesUI("resumen")
  )
)

server <- function(input, output, session) {
  TablaOportunidades("resumen", reactive({BaseOportunidades}), reactive("CMEDINA"), trigger_update = reactive(0))
}

shinyApp(ui, server)


# Tablero de oportunidades -----
DashboardOportunidadesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4,
             valueBoxOutput(ns("total_op"), width = 12)
      ),
      column(4,
             valueBoxOutput(ns("op_activas"), width = 12)
      ),
      column(4,
             valueBoxOutput(ns("op_descartadas"), width = 12)
      )
    ),
    
    # CUMPLIMIENTO FACTURACIÓN (TOTAL) ----
    fluidRow(
      column(12,
             box(title = "Cumplimiento total (facturación vs oportunidad)",
                 status = "white", solidHeader = FALSE, width = 12,
                 fluidRow(
                   column(6,
                          plotlyOutput(ns("plot_sacos_tot"), height = "320px")
                   ),
                   column(6,
                          plotlyOutput(ns("plot_margen_tot"), height = "320px")
                   )
                 )
             )
      )
    ),
    
    # NORMALIZADO 30 DÍAS (TOTAL) ----
    fluidRow(
      column(12,
             box(title = "Estandarización a 30 días (total)",
                 status = "white", solidHeader = FALSE, width = 12,
                 fluidRow(
                   column(6,
                          plotlyOutput(ns("plot_sacos_30"), height = "320px")
                   ),
                   column(6,
                          plotlyOutput(ns("plot_margen_30"), height = "320px")
                   )
                 )
             )
      )
    ),
    
    fluidRow(
      column(12,
             box(title = "Oportunidades por Asesor", status = "white", solidHeader = FALSE,
                 width = 12,
                 fluidRow(
                   column(6,
                          plotlyOutput(ns("grafico_asesor"), height = "300px")
                   ),
                   column(6,
                          gt_output(ns("tabla_asesor"))
                   )
                 )
             )
      )
    ),
    
    fluidRow(
      column(12,
             box(title = "Segmentación de Oportunidades", status = "white", solidHeader = FALSE,
                 width = 12,
                 fluidRow(
                   column(4,
                          plotlyOutput(ns("grafico_tipo_cliente"), height = "260px")
                   ),
                   column(4,
                          plotlyOutput(ns("grafico_linea"), height = "260px")
                   ),
                   column(4,
                          plotlyOutput(ns("grafico_segmento"), height = "260px")
                   )
                 )
             )
      )
    ),
    
    fluidRow(
      column(12,
             box(title = "Resumen Económico", status = "white", solidHeader = FALSE,
                 width = 12,
                 fluidRow(
                   column(6,
                          gt_output(ns("tabla_margen"))
                   ),
                   column(6,
                          gt_output(ns("tabla_sacos"))
                   )
                 )
             )
      )
    ),
    
    fluidRow(
      column(12,
             box(title = "Oportunidades Descartadas", status = "white", solidHeader = FALSE,
                 width = 12, collapsible = TRUE, collapsed = TRUE,
                 gt_output(ns("tabla_descartes"))
             )
      )
    )
  )
}
DashboardOportunidades <- function(id, datos_op, usr) {
  moduleServer(id, function(input, output, session) {
    
    # Datos reactivos ----
    datos <- reactive({
      req(datos_op())
      
      df <- datos_op() %>%
        dplyr::mutate(dplyr::across(where(is.character), ~ ifelse(is.na(.) | . == "", "SIN DATO", trimws(.))),
                      FechaCumpOP = as.Date(FechaCumpOP),
                      FechaHoraCrea = as.POSIXct(FechaHoraCrea),
                      FecFact = as.Date(FecFact),
                      dias_urgencia = as.numeric(FechaCumpOP - Sys.Date()),
                      factor_urgencia = dplyr::case_when(is.na(dias_urgencia) ~ 1.0,
                                                         dias_urgencia <= 7  ~ 1.5,
                                                         dias_urgencia <= 30 ~ 1.2,
                                                         TRUE                ~ 1.0),
                      ScoreRaw = SacosOP * MargenOP * factor_urgencia)
      
      if (all(is.na(df$ScoreRaw)) || length(unique(na.omit(df$ScoreRaw))) <= 1) {
        df$ScoreOP <- 0
      } else {
        df$ScoreOP <- scales::rescale(df$ScoreRaw, to = c(0, 100), na.rm = TRUE)
      }
      
      df %>%
        dplyr::select(-ScoreRaw)
    })
    
    # Paleta ----
    pal_rg <- c("#7a1f1f", "#9e9e9e", "#bdbdbd", "#d32f2f", "#757575")
    
    # Cumplimiento facturación (TOTAL) ----
    cumplimiento_total <- reactive({
      df <- datos()
      req(nrow(df) > 0)
      
      # Definición de oportunidad (misma granularidad del módulo detalle)
      keys <- c("PerRazSoc", "LineaNegocio", "Categoria", "Producto")
      
      op_tot <- df %>%
        dplyr::distinct(dplyr::across(dplyr::all_of(keys)),
                        SacosOP, MargenTotalOP, FrecuenciaDias, FechaHoraCrea, FechaCumpOP) %>%
        dplyr::mutate(
          FechaHoraCrea = as.POSIXct(FechaHoraCrea),
          FechaCumpOP   = as.Date(FechaCumpOP)
        )
      
      sacos_obj  <- sum(as.numeric(op_tot$SacosOP), na.rm = TRUE)
      margen_obj <- sum(as.numeric(op_tot$MargenTotalOP), na.rm = TRUE)
      
      fecha_crea <- suppressWarnings(min(as.Date(op_tot$FechaHoraCrea), na.rm = TRUE))
      fecha_cump <- suppressWarnings(max(as.Date(op_tot$FechaCumpOP),   na.rm = TRUE))
      
      frec_dias <- suppressWarnings(stats::weighted.mean(
        x = as.numeric(op_tot$FrecuenciaDias),
        w = as.numeric(op_tot$SacosOP),
        na.rm = TRUE
      ))
      
      if (is.na(frec_dias) || frec_dias <= 0) {
        frec_dias <- as.numeric(fecha_cump - fecha_crea)
      }
      
      fac <- preparar_facturacion(df, fecha_crea, frec_dias)
      
      list(
        op = tibble::tibble(SacosOP = sacos_obj, MargenTotalOP = margen_obj),
        fac = fac,
        fac_30 = normalizar_30_dias(fac, sacos_obj, margen_obj, frec_dias),
        cumplimiento = calcular_cumplimiento(fac, sacos_obj, margen_obj, frec_dias),
        fecha_crea = fecha_crea,
        fecha_cump = fecha_cump,
        frec_dias  = frec_dias
      )
    })
    
    # KPIs ----
    output$total_op <- renderbs4ValueBox({
      total <- nrow(datos() %>% select(PerRazSoc, LineaNegocio) %>% distinct())
      CajaValor(total, "coma", "Total oportunidades", "bullseye", mostrar_boton = FALSE)
    })
    output$op_activas <- renderbs4ValueBox({
      activas <- datos() %>% dplyr::filter(Descartada == 0) %>% select(PerRazSoc, LineaNegocio) %>% distinct() %>% nrow()
      CajaValor(activas, "coma", "Oportunidades activas", "check-circle",
                mostrar_boton = FALSE)
    })
    output$op_descartadas <- renderbs4ValueBox({
      disc <- datos() %>% dplyr::filter(Descartada == 1) %>% nrow()
      CajaValor(disc, "coma", "Oportunidades descartadas", "ban",
                mostrar_boton = FALSE)
    })
    
    # Plots cumplimiento total ----
    output$plot_sacos_tot <- plotly::renderPlotly({
      x <- cumplimiento_total()
      fac <- x$fac
      op  <- x$op
      req(nrow(fac) > 0)
      
      fmt_num <- FormatoD3("numero")
      
      p <- plotly::plot_ly() %>%
        plotly::add_lines(
          data = fac,
          x = ~FecFact,
          y = ~CumSac,
          mode = "lines+markers",
          name = "Sacos facturados (acum)",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_num, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::add_lines(
          x = c(min(fac$FecFact), max(fac$FecFact)),
          y = c(op$SacosOP, op$SacosOP),
          name = "Meta sacos (total)",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_num, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::layout(
          title = "Cumplimiento total – Sacos",
          xaxis = list(title = "Fecha de facturación", tickformat = "%b %Y"),
          yaxis = list(title = "Sacos", tickformat = fmt_num),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
          hovermode = "x unified"
        )
      
      plotly::config(p, displayModeBar = FALSE, locale = "es")
    })
    
    output$plot_margen_tot <- plotly::renderPlotly({
      x <- cumplimiento_total()
      fac <- x$fac
      op  <- x$op
      req(nrow(fac) > 0)
      
      fmt_dinero <- FormatoD3("dinero")
      
      p <- plotly::plot_ly() %>%
        plotly::add_lines(
          data = fac,
          x = ~FecFact,
          y = ~CumMarg,
          mode = "lines+markers",
          name = "Margen facturado (acum)",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_dinero, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::add_lines(
          x = c(min(fac$FecFact), max(fac$FecFact)),
          y = c(op$MargenTotalOP, op$MargenTotalOP),
          name = "Meta margen (total)",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_dinero, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::layout(
          title = "Cumplimiento total – Margen ($)",
          xaxis = list(title = "Fecha de facturación", tickformat = "%b %Y"),
          yaxis = list(title = "Margen ($)", tickformat = fmt_dinero),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
          hovermode = "x unified"
        )
      
      plotly::config(p, displayModeBar = FALSE, locale = "es")
    })
    
    # Plots estandarización 30 días (total) ----
    output$plot_sacos_30 <- plotly::renderPlotly({
      x <- cumplimiento_total()
      fac <- x$fac_30
      req(nrow(fac) > 0)
      
      fmt_num <- FormatoD3("numero")
      
      p <- plotly::plot_ly() %>%
        plotly::add_bars(
          data = fac,
          x = ~FecFact,
          y = ~CumSac,
          name = "Sacos (equivalente 30 días)",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_num, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::add_lines(
          x = c(min(fac$FecFact), max(fac$FecFact)),
          y = c(fac$PlanSacos30[1], fac$PlanSacos30[1]),
          name = "Meta 30 días (total)",
          hovertemplate = paste0(
            "Meta 30 días<br>",
            "%{y:", fmt_num, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::layout(
          title = "Estandarización a 30 días – Sacos (total)",
          xaxis = list(title = "Mes de facturación", tickformat = "%b %Y"),
          yaxis = list(title = "Sacos", tickformat = fmt_num),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
          hovermode = "x unified",
          barmode = "group"
        )
      
      plotly::config(p, displayModeBar = FALSE, locale = "es")
    })
    
    output$plot_margen_30 <- plotly::renderPlotly({
      x <- cumplimiento_total()
      fac <- x$fac_30
      req(nrow(fac) > 0)
      
      fmt_dinero <- FormatoD3("dinero")
      
      p <- plotly::plot_ly() %>%
        plotly::add_bars(
          data = fac,
          x = ~FecFact,
          y = ~CumMarg,
          name = "Margen (equivalente 30 días)",
          hovertemplate = paste0(
            "%{x|%b %Y}<br>",
            "%{y:", fmt_dinero, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::add_lines(
          x = c(min(fac$FecFact), max(fac$FecFact)),
          y = c(fac$PlanMargen30[1], fac$PlanMargen30[1]),
          name = "Meta 30 días (total)",
          hovertemplate = paste0(
            "Meta 30 días<br>",
            "%{y:", fmt_dinero, "}",
            "<extra></extra>"
          )
        ) %>%
        plotly::layout(
          title = "Estandarización a 30 días – Margen ($) (total)",
          xaxis = list(title = "Mes de facturación", tickformat = "%b %Y"),
          yaxis = list(title = "Margen ($)", tickformat = fmt_dinero),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
          hovermode = "x unified",
          barmode = "group"
        )
      
      plotly::config(p, displayModeBar = FALSE, locale = "es")
    })
    
    # Gráficos ----
    output$grafico_asesor <- plotly::renderPlotly({
      df <- datos() %>%
        dplyr::count(Usuario, name = "Total") %>%
        dplyr::arrange(desc(Total))
      
      plotly::plot_ly(
        df, x = ~Usuario, y = ~Total,
        type = "bar",
        marker = list(color = pal_rg[1]),
        text = ~Total, textposition = "outside"
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    output$grafico_tipo_cliente <- plotly::renderPlotly({
      df <- datos() %>% dplyr::count(TipoClienteOP, name = "Total")
      
      plotly::plot_ly(
        df,
        labels = ~TipoClienteOP,
        values = ~Total,
        type = "pie",
        hole = 0.5,
        marker = list(colors = pal_rg)
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    output$grafico_linea <- plotly::renderPlotly({
      df <- datos() %>% dplyr::count(LineaNegocio, name = "Total")
      
      plotly::plot_ly(
        df,
        labels = ~LineaNegocio,
        values = ~Total,
        type = "pie",
        hole = 0.5,
        marker = list(colors = pal_rg)
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    output$grafico_segmento <- plotly::renderPlotly({
      df <- datos() %>% dplyr::count(Segmento, name = "Total")
      
      plotly::plot_ly(
        df,
        labels = ~Segmento,
        values = ~Total,
        type = "pie",
        hole = 0.5,
        marker = list(colors = pal_rg)
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Tablas ----
    output$tabla_asesor <- gt::render_gt({
      datos() %>%
        dplyr::group_by(Usuario) %>%
        dplyr::summarise(
          Oportunidades = n(),
          Sacos = sum(SacosOP, na.rm = TRUE),
          Margen = sum(SacosOP * MargenOP, na.rm = TRUE),
          Score = mean(ScoreOP, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        gt::gt() %>%
        gt::fmt_currency(columns = Margen, currency = "COP", decimals = 0) %>%
        gt::fmt_number(columns = Score, decimals = 1) %>%
        gt_minimal_style() %>%
        gt::opt_interactive(
          use_pagination = FALSE,
          use_filters = FALSE,
          use_resizers = TRUE
        )
    })
    output$tabla_margen <- gt::render_gt({
      datos() %>%
        dplyr::group_by(LineaNegocio) %>%
        dplyr::summarise(
          Sacos = sum(SacosOP, na.rm = TRUE),
          MargenTotal = sum(SacosOP * MargenOP, na.rm = TRUE),
          ScoreTotal = mean(ScoreOP, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        gt::gt() %>%
        gt::fmt_currency(columns = MargenTotal, currency = "COP", decimals = 0) %>%
        gt::fmt_number(columns = ScoreTotal, decimals = 1) %>%
        gt_minimal_style() %>%
        gt::opt_interactive(
          use_pagination = FALSE,
          use_filters = FALSE,
          use_resizers = TRUE
        )
    })
    output$tabla_sacos <- gt::render_gt({
      datos() %>%
        dplyr::group_by(Categoria) %>%
        dplyr::summarise(
          Sacos = sum(SacosOP, na.rm = TRUE),
          Margen = sum(SacosOP * MargenOP, na.rm = TRUE),
          Score = mean(ScoreOP, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        gt::gt() %>%
        gt::fmt_currency(columns = Margen, currency = "COP", decimals = 0) %>%
        gt::fmt_number(columns = Score, decimals = 1) %>%
        gt_minimal_style() %>%
        gt::opt_interactive(
          use_pagination = FALSE,
          use_filters = FALSE,
          use_resizers = TRUE
        )
    })
    
    output$tabla_descartes <- gt::render_gt({
      datos() %>%
        dplyr::filter(Descartada == 1) %>%
        dplyr::group_by(RazonDescartado) %>%
        dplyr::summarise(
          Oportunidades = n(),
          Sacos = sum(SacosOP, na.rm = TRUE),
          Margen = sum(SacosOP * MargenOP, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        gt::gt() %>%
        gt::fmt_currency(columns = Margen, currency = "COP", decimals = 0) %>%
        gt_minimal_style() %>%
        gt::opt_interactive(
          use_pagination = FALSE,
          use_filters = FALSE,
          use_resizers = TRUE
        )
    })
    
  })
}


ui <- bs4DashPage(
  title = "Prueba ResumenTotal",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    DashboardOportunidadesUI("resumen")
  )
)

server <- function(input, output, session) {
  DashboardOportunidades("resumen", reactive(BaseOportunidades), reactive("CMEDINA"))
}

shinyApp(ui, server)

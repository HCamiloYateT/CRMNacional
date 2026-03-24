choi_seg <-  c("CAMPEONES", "CLIENTES LEALES", "POTENCIALES LEALES",
               "NUEVOS CLIENTES", "PROMETEDORES", "NECESITAN ATENCIÓN",
               "A PUNTO DE DORMIR", "EN RIESGO", "NO PODEMOS PERDERLOS",
               "HIBERNANDO", "PERDIDOS", "NUEVOS", "OTROS")

RFMUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(9, 
             plotlyOutput(ns("DistrSegmentoRFM"), width = "100%", height = "500px")),
      column(3, 
             uiOutput(ns("DescSegmento"))
      )
    ),
    br(),h5("Detalle por Segmento RFM"),
    fluidRow(
      column(4,
             ListaDesplegable(ns("RFM_Seg"), label = h6("Segmento RFM"), 
                              choices =choi_seg,  selected = choi_seg,
                              multiple = TRUE, fem = FALSE)
      )
    ),
    fluidRow(
      column(4,
             uiOutput(ns("NumClientesRFM")),
             plotlyOutput(ns("DistrRecencia"), width = "100%", height = "280px"),
             plotlyOutput(ns("DistrFrecuencia"), width = "100%", height = "280px"),
             plotlyOutput(ns("DistrMonto"), width = "100%", height = "280px")
      ),
      column(8,
             gt_output(ns("TablaRFM"))
      )
    )
  )
}
RFM <- function(id, dat, titmonto = "Sacos (70 kgs)") {
  moduleServer(id, function(input, output, session) {
    
    # Distribucion ----
    output$DistrSegmentoRFM <- renderPlotly({
      # Optimización: calcular fechas una sola vez
      fecha_actual <- Sys.Date()
      primer_dia_mes_actual <- PrimerDia(fecha_actual)
      anho_actual <- year(fecha_actual)
      
      tot <- n_distinct(dat()$CliNitPpal, dat()$LinNegCod)
      
      t1 <- dat() %>% 
        group_by(SegmentoAnalitica, parents = "") %>% 
        summarise(Clientes = n_distinct(CliNitPpal),
                  PctClientes = Clientes / tot, 
                  SacosMes = sum(if_else(PrimerDia(FecFact) == primer_dia_mes_actual, Kilos/70, 0), na.rm = T),
                  SacosPromMes = sum(Kilos/70, na.rm = T) / n_distinct(PrimerDia(FecFact)),
                  SacosAnho = sum(if_else(year(FecFact) == anho_actual, Kilos/70, 0), na.rm = T),
                  MargenMes = sum(if_else(PrimerDia(FecFact) == primer_dia_mes_actual, Margen, 0), na.rm = T),
                  MargenPromMes = sum(Margen, na.rm = T) / n_distinct(PrimerDia(FecFact)),
                  MargenAnho = sum(if_else(year(FecFact) == anho_actual, Margen, 0), na.rm = T),
                  .groups = "drop") %>% 
        # Optimización: usar mutate vectorizado en lugar de rowwise
        mutate(Texto = paste0("<b>Segmento: </b>", SegmentoAnalitica,
                              "<b><br>Clientes: </b>", comma(Clientes),
                              "<b><br>Porcentaje: </b>", FormatearNumero(PctClientes, "porcentaje",  negrita = FALSE),
                              "<br><br><b>Sacos Acum Mes: </b>", FormatearNumero(SacosMes, "numero",  negrita = FALSE),
                              "<br><b>Sacos Acum Año: </b>", FormatearNumero(SacosAnho, "numero",  negrita = FALSE),
                              "<br><b>Sacos Prom. Mes: </b>", FormatearNumero(SacosPromMes, "numero",  negrita = FALSE),
                              "<br><br><b>Márgen Acum Mes: </b>", FormatearNumero(MargenMes, "dinero",  negrita = FALSE),
                              "<br><b>Márgen Acum Año: </b>", FormatearNumero(MargenAnho, "dinero",  negrita = FALSE),
                              "<br><b>Márgen Prom. Mes: </b>", FormatearNumero(MargenPromMes, "dinero",  negrita = FALSE))
        )
      
      if (nrow(t1) > 0) {
        p <- plot_ly(t1, type = "treemap", labels = ~Texto, parents = ~parents,
                     customdata = t1$SegmentoAnalitica, 
                     values = ~Clientes, textinfo = "label", source = session$ns("rfm")
        ) %>% 
          layout(margin = list(t = 0, l = 0, r = 0, b = 0),
                 clickmode = "event+select") %>% 
          config(locale = "es", displayModeBar = F)
        p <- event_register(p, "plotly_click")
        return(p)
      } else {
        # Retorna un plotly vacío pero con el evento registrado
        p <- plot_ly(
          type = "treemap",
          labels = character(0),
          parents = character(0),
          values = numeric(0),
          source = session$ns("rfm")
        ) %>%
          layout(margin = list(t = 0, l = 0, r = 0, b = 0),
                 clickmode = "event+select") %>%
          config(locale = "es", displayModeBar = F)
        p <- event_register(p, "plotly_click")
        return(p)
      }
    })
    
    selected_value <- reactiveVal(NULL)
    
    observeEvent(event_data("plotly_click", source = session$ns("rfm")), {
      click_data <- event_data("plotly_click", source = session$ns("rfm"))
      selected_value(click_data$customdata)
      updatePickerInput(session, "RFM_Seg", selected = click_data$customdata)
    })
    
    data_desc_seg <- reactive({
      req(selected_value())
      # Optimización: eliminar variable no utilizada
      data.frame(
        Segmento_de_Cliente = c("CAMPEONES", "CLIENTES LEALES", "POTENCIALES LEALES",
                                "NUEVOS CLIENTES", "PROMETEDORES", "NECESITAN ATENCIÓN",
                                "A PUNTO DE DORMIR", "EN RIESGO", "NO PODEMOS PERDERLOS",
                                "HIBERNANDO", "PERDIDOS", "NUEVOS", "OTROS"),
        Actividad = c(
          "¡Compran recientemente, compran con frecuencia y gastan más!",
          "Gastan una buena cantidad con nosotros con frecuencia. Son sensibles a promociones.",
          "Clientes recientes, pero gastaron una buena cantidad y compraron más de una vez.",
          "Compraron más recientemente, pero no con frecuencia.",
          "Compradores recientes, pero no gastaron mucho.",
          "Valores de recencia, frecuencia y monto por encima del promedio. Tal vez no hayan comprado muy recientemente.",
          "Valores por debajo del promedio en recencia, frecuencia y monto. Se perderán si no se reactiva.",
          "Gastaron mucho dinero y compraron con frecuencia, pero hace tiempo. ¡Es necesario traerlos de vuelta!",
          "Realizaron las compras más grandes y con frecuencia, pero no han regresado en mucho tiempo.",
          "Su última compra fue hace mucho tiempo, gastan poco y compran poco.",
          "Los puntajes más bajos en recencia, frecuencia y monto (puntaje RFM).",
          "Clientes que hicieron su primera transacción en el mes vigente",
          "Clientes con comportamiento en zonas grises"
        ),
        Sugerencia_Accionable = c(
          "Recompensarlos. Pueden ser adoptadores tempranos de nuevos productos. Promoverán tu marca.",
          "Vender productos de mayor valor. Pedir reseñas. Involucrarlos más.",
          "Ofrecer programas de membresía/loyalty, recomendar otros productos.",
          "Proporcionar soporte inicial, brindarles éxito temprano, comenzar a construir la relación.",
          "Crear conciencia de marca, ofrecer pruebas gratuitas.",
          "Hacer ofertas por tiempo limitado, recomendar productos basados en compras pasadas. Reactivarlos.",
          "Compartir recursos valiosos, recomendar productos populares o renovaciones con descuento, reconectar con ellos.",
          "Enviar correos electrónicos personalizados para reconectar, ofrecer renovaciones, proporcionar recursos útiles.",
          "Recuperarlos mediante renovaciones o productos más nuevos, no dejarlos ir a la competencia, hablar con ellos.",
          "Ofrecer otros productos relevantes y descuentos especiales. Recrear el valor de la marca.",
          "Revivir el interés con una campaña de alcance, ignorarlos de lo contrario.",
          "Monitoreo de nuevas transacciones, el siguiente mes deberá cambiar de segmento",
          "Monitoreo de clientes, suelen cambiar fácilmente de segmento"
        ),
        stringsAsFactors = FALSE
      ) %>%
        filter(Segmento_de_Cliente == selected_value())
    })
    
    output$DescSegmento <- renderUI({
      req(data_desc_seg())
      paste0(
        FormatearTexto(selected_value(), tamano_pct = 2), Saltos(3),
        FormatearTexto("Descripción: ", tamano_pct = 1.5), br(),
        FormatearTexto(data_desc_seg()$Actividad, negrita = F), br(), br(),
        FormatearTexto("Acciones: ", tamano_pct = 1.5), br(),
        FormatearTexto(data_desc_seg()$Sugerencia_Accionable, negrita = F)
      ) %>% HTML
    })
    
    # Detalle Segmento ----
    det_seg_f <- reactive({
      dat() %>%
        filter(SegmentoAnalitica %in% input$RFM_Seg)
    })
    
    tmp_rfm_data <- reactive({
      det_seg_f() %>%
        select(CliNitPpal, recency_days, transaction_count, amount) %>%
        distinct()
    })
    
    output$NumClientesRFM <- renderUI({
      tmp1 <- n_distinct(det_seg_f()$CliNitPpal, det_seg_f()$LinNegCod)
      
      tags$div(
        style = "text-align:center",
        paste0(
          FormatearTexto(comma(tmp1), tamano_pct = 3),
          FormatearTexto(" Clientes")
        ) %>% HTML
      )
    })
    
    output$DistrRecencia <- renderPlotly({
      datos <- tmp_rfm_data()$recency_days
      datos <- datos[datos > 0 & !is.na(datos)] # Solo positivos y no NA
      if (length(datos) > 1) {
        ImprimirDensidad(data.frame(recency_days = datos), "recency_days", "Recencia (días)")
      }
    })
    output$DistrFrecuencia <- renderPlotly({
      datos <- tmp_rfm_data()$transaction_count
      datos <- datos[datos > 0 & !is.na(datos)] # Solo positivos y no NA
      if (length(datos) > 1) {
        ImprimirDensidad(data.frame(transaction_count = datos), "transaction_count", "Frecuencia")
      }
    })
    output$DistrMonto <- renderPlotly({
      datos <- tmp_rfm_data()$amount
      datos <- datos[datos > 0 & !is.na(datos)] # Solo positivos y no NA
      if (length(datos) > 1) {
        ImprimirDensidad(data.frame(amount = datos), "amount", titmonto)
      }
    })
    
    output$TablaRFM <- render_gt({
      # Validación: verificar que hay datos
      req(nrow(det_seg_f()) > 0)
      
      # Optimización: calcular fechas una sola vez
      fecha_actual <- Sys.Date()
      primer_dia_mes_actual <- PrimerDia(fecha_actual)
      anho_actual <- year(fecha_actual)
      
      aux1 <- det_seg_f() %>%
        crear_link_cliente(col_razsoc = "PerRazSoc", col_linneg = "CLLinNegNo") %>% 
        group_by(Cliente = PerRazSoc, LineaNegocio = CLLinNegNo, Segmento) |> 
        summarise(DiasUltDesp = min(recency_days, na.rm = T),
                  SacosMes = sum(if_else(PrimerDia(FecFact) == primer_dia_mes_actual, Kilos/70, 0), na.rm = T),
                  SacosAnho = sum(if_else(year(FecFact) == anho_actual, Kilos/70, 0), na.rm = T),
                  MargenMes = sum(if_else(PrimerDia(FecFact) == primer_dia_mes_actual, Margen, 0), na.rm = T),
                  MargenAnho = sum(if_else(year(FecFact) == anho_actual, Margen, 0), na.rm = T),
                  .groups = "drop"
        )
      
      aux1 <- bind_rows(
        aux1,
        aux1 %>% 
          summarise(Cliente = "TOTAL", 
                    LineaNegocio = "", 
                    Segmento = "",
                    DiasUltDesp = min(DiasUltDesp, na.rm = T),
                    SacosMes = sum(SacosMes, na.rm = T),
                    SacosAnho = sum(SacosAnho, na.rm = T),
                    MargenMes = sum(MargenMes, na.rm = T),
                    MargenAnho = sum(MargenAnho, na.rm = T)
          )
      )
      
      aux1 %>% 
        gt() %>%
        tab_header(title = "RFM de Clientes",
                   subtitle = paste("Total registros:", nrow(aux1) - 1)) %>%
        cols_label(Cliente = "Cliente", 
                   LineaNegocio = "Línea de negocio",
                   Segmento = "Segmento", 
                   DiasUltDesp = "Días desde último despacho",
                   SacosMes = "Sacos mes", 
                   SacosAnho = "Sacos año", 
                   MargenMes = "Margen mes (COP)",
                   MargenAnho = "Margen año (COP)") %>%  
        fmt_number(columns = c(DiasUltDesp, SacosMes, SacosAnho), decimals = 0) %>%
        fmt_currency(columns = c(MargenMes, MargenAnho), currency = "COP", decimals = 0) %>%
        fmt_url(columns = Cliente, 
                label = function(x) extraer_razon_social(x),
                target = "_blank", 
                color = "black", 
                show_underline = TRUE) %>%
        gt_minimal_style() 
    })
    
  })
}
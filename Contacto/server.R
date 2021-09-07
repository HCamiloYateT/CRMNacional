function(input, output, session) { 
  
  ### Configuracion Logins ----
  
  insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(
      class="nav navbar-nav navbar-right",
      tags$li(
        div(
          style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
          shinyauthr::logoutUI("logout", label = "Salir", icon =icon("sign-out-alt"), 
                               style = "background-color: #CB4335 !important; border: 0;
                               font-weight: bold; margin:5px; padding: 10px;")
        )
      )
    )
  )
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = users,
    user_col = "User",
    pwd_col = "Pass",
    sodium_hashed = F,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  observeEvent(credentials()$user_auth, {
    
    if (credentials()$user_auth) { 
      
      removeTab("tabs", "login")
      appendTab("tabs", tab_registro, select = TRUE)
      appendTab("tabs", tab_analisis)
    }
  })
  
  ### Actualizacion de Inputs de Registro ----
  
  user_data <- reactive({
    req(credentials()$user_auth)
    aux <- prods %>% 
      filter(Tipo == input$Tipo)
    
    prods <- unique(aux$Calidad)
  })
  
  observe({
    if (credentials()$user_auth) { 
      req(input$Tipo)
      updateSelectInput(session, "Calidad", choices = c("", user_data()), selected = "")
      }
  })
  
  ### Validacion de Campos Obligatorios ----
  
  observe({
    req(credentials()$user_auth)
    mandatoryFilled <-
      vapply(obligatorios,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "Registrar", condition = mandatoryFilled)
  })
  
  ### Interactividad del Formulario ----
  
  registro <- reactive({
    req(credentials()$user_auth)
    
    aux <- data.frame(
      Usuario = credentials()$info$User,
      TimeStamp = as.POSIXct(Sys.Date()),
      Cliente = input$Cliente,
      Tipo = input$Tipo,
      Calidad = input$Calidad,
      Venta = input$Venta,
      Commentario = input$caption
    )
    
  })
  observeEvent(input$Registrar, {
    saveData(registro())
    
    aux <- loadData()
    
    clientes <- aux %>% 
      select(cliente = Cliente) %>% 
      distinct() %>% 
      bind_rows(readRDS("data/clientes.rds"))
    
    
    prods <- aux %>% 
      select(Tipo, Calidad) %>% 
      distinct() %>% 
      bind_rows(readRDS("data/prods.rds"))
    
    customers <- c("", sort(unique(clientes$cliente)))
    tipos <- c("", sort(unique(prods$Tipo)))
    
    updateSelectInput(session, "Cliente", choices = customers, selected = "")
    updateSelectInput(session, "Tipo", choices = tipos, selected = "")
    updateSelectInput(session, "Calidad", choices = "", selected = "")
    updatePrettyToggle(session, "Venta", value = FALSE)
    
    sendSweetAlert(
      session = session,
      title = "",
      text = "Registro guardado con éxito",
      type = "success",
      closeOnClickOutside = T
    )
    
  })
  
  ### Carga de Datos Persistente -----
  
  bd_registros <- eventReactive(input$Registrar, {
    req(credentials()$user_auth)
    loadData()
  }, ignoreNULL = FALSE)

  ### Consulta Individual
  
  bd_consulta <- reactive({
    req(credentials()$user_auth)
    bd_registros() %>% 
      filter(Cliente == input$ClienteConsulta)
  })
  
  ### Consulta General
  bd_general <- reactive({
    req(credentials()$user_auth)
    bd_registros() %>% 
      left_join(users, by= c("Usuario"="User")) %>% 
      arrange(Cliente, desc(Timestamp)) %>% 
      group_by(Cliente) %>% 
      filter(row_number()==1) %>% 
      ungroup() %>% 
      mutate(Dias = as.numeric(Sys.Date() - as.Date(Timestamp)),
             RangoDias = case_when(Dias <= 7 ~ "De 0 a 7 días",
                                   Dias <= 15 ~ "De 8 a 15 días",
                                   Dias <= 30 ~ "De 16 a 30 días",
                                   Dias <= 90 ~ "De 31 a  90 días",
                                   Dias > 90 ~ "Más de 90 días",
             )
      )
  })
  
  ### Consula Fechas ----
  
  bd_fecha <- eventReactive(input$Refresh, {
    req(credentials()$user_auth)
    loadData() %>% 
      left_join(users, by= c("Usuario"="User")) %>% 
      mutate(Fecha = as.Date(Timestamp)) %>% 
      filter(Fecha == input$FechaAnalisis) %>% 
      select(Fecha, Cliente, Nombre, Tipo, Calidad, Comentarios)
  }, ignoreNULL = FALSE)
  
  
  ### Reporte ----
  #### Individual ----
  ##### Cajas ----
  
  ###### Contacto ----
  output$VB_UltimoContacto <- renderInfoBox({
    req(input$ClienteConsulta)
    aux1 <- as.numeric(abs(as.Date(max(bd_consulta()$Timestamp))-Sys.Date()))
    aux2 <- format(as.Date(max(bd_consulta()$Timestamp)), "%d de %B del %Y")

    if(!is.na(aux2)){

      infoBox(
        value = paste0("Días desde el último contacto: ", aux1),
        title = "",
        subtitle = paste0("Último contacto: ", aux2),
        icon = icon("calendar"),
        fill = T,
        color = "red"
        )
      }
  })
  output$VB_ContactoUsuario <- renderInfoBox({
    req(input$ClienteConsulta)

    aux0 <- format(as.Date(max(bd_consulta()$Timestamp)), "%d de %B del %Y")

    aux1 <- bd_consulta() %>%
      filter(Timestamp == max(bd_consulta()$Timestamp)) %>%
      select(Usuario) %>%
      as.character()

    if(!is.na(aux0)){

      infoBox(
        value = users[users$User == aux1 ,"Nombre"],
        title = "",
        subtitle = "Usuario de último contacto",
        icon = icon("user-check"),
        fill = T,
        color = "red"
      )
    }
  })
  output$VB_ContactoTipo <- renderInfoBox({
    
    aux0 <- format(as.Date(max(bd_consulta()$Timestamp)), "%d de %B del %Y")
    req(input$ClienteConsulta)
    aux1 <- bd_consulta() %>% 
      filter(Timestamp == max(bd_consulta()$Timestamp)) %>% 
      select(Tipo) %>% 
      as.character()
    
    if(!is.na(aux0)){
      
      infoBox(
        value = aux1,
        title = "",
        subtitle = "Último Tipo de Producto",
        icon = icon("file-alt"),
        fill = T,
        color = "red"
      )
    }
  })
  output$VB_ContactoProducto <- renderInfoBox({
    req(input$ClienteConsulta)
    aux0 <- format(as.Date(max(bd_consulta()$Timestamp)), "%d de %B del %Y")

    aux1 <- bd_consulta() %>%
      filter(Timestamp == max(bd_consulta()$Timestamp)) %>%
      select(Calidad) %>%
      as.character()

    if(!is.na(aux0)){

      apputils::infoBox(
        value = aux1,
        title = "",
        subtitle = "Último Producto",
        icon = ico_cafe,
        fill = T,
        color = "red"
      )
    }
  })
  
  ###### Efectiva ----
  output$VB_UltimoVenta <- renderInfoBox({
    req(input$ClienteConsulta)
    df <- bd_consulta() %>% 
      filter(Venta == "VERDADERO")
    
    aux1 <- as.numeric(abs(as.Date(max(df$Timestamp))-Sys.Date()))
    aux2 <- format(as.Date(max(df$Timestamp)), "%d de %B del %Y")
    
    if(!is.na(aux2)){
      
      infoBox(
        value = paste0("Días desde el último contacto efectivo: ", aux1),
        title = "",
        subtitle = paste0("Última venta: ", aux2),
        icon = icon("calendar"),
        fill = T,
        color = "red"
      )
    }
  })
  output$VB_VentaUsuario <- renderInfoBox({
    req(input$ClienteConsulta)
    df <- bd_consulta() %>% 
      filter(Venta == "VERDADERO")
    
    aux0 <- format(as.Date(max(df$Timestamp)), "%d de %B del %Y")
    
    aux1 <- df %>%
      filter(Timestamp == max(df$Timestamp)) %>%
      select(Usuario) %>%
      as.character()
    
    if(!is.na(aux0)){
      
      infoBox(
        value = users[users$User == aux1 ,"Nombre"],
        title = "",
        subtitle = "Usuario de último contacto efectivo",
        icon = icon("user-check"),
        fill = T,
        color = "red"
      )
    }
  })
  output$VB_VentaTipo <- renderInfoBox({
    req(input$ClienteConsulta)
    df <- bd_consulta() %>% 
      filter(Venta == "VERDADERO")
    
    aux0 <- format(as.Date(max(df$Timestamp)), "%d de %B del %Y")
    
    aux1 <- df %>% 
      filter(Timestamp == max(df$Timestamp)) %>% 
      select(Tipo) %>% 
      as.character()
    
    if(!is.na(aux0)){
      
      infoBox(
        value = aux1,
        title = "",
        subtitle = "Último Tipo de Producto",
        icon = icon("file-alt"),
        fill = T,
        color = "red"
      )
    }
  })
  output$VB_VentaProducto <- renderInfoBox({
    req(input$ClienteConsulta)
    df <- bd_consulta() %>% 
      filter(Venta == "VERDADERO")
    
    aux0 <- format(as.Date(max(df$Timestamp)), "%d de %B del %Y")
    
    aux1 <- df %>%
      filter(Timestamp == max(df$Timestamp)) %>%
      select(Calidad) %>%
      as.character()
    
    if(!is.na(aux0)){
      
      apputils::infoBox(
        value = aux1,
        title = "",
        subtitle = "Último Producto",
        icon = ico_cafe,
        fill = T,
        color = "red"
      )
    }
  })
  
  #### Graficos -----
  
  output$SerieContacto <- renderPlotly({
    if (input$ClienteConsulta != ""){
      aux1 <- bd_consulta() %>% 
        left_join(users, by= c("Usuario"="User")) %>% 
        complete(Timestamp = seq.Date(min(as.Date(Timestamp)), max(as.Date(Timestamp)), by="month")) %>%
        mutate(Mes = format(as.Date(Timestamp), "%b%y"),
               Mes = factor(Mes, levels = unique(Mes)),
               Aux = ifelse(is.na(Usuario), 0, 1)) %>%
        group_by(Mes) %>%
        summarise(Registros = sum(Aux))
      
      plot_ly(data=aux1, x=~Mes, y=~Registros, type = 'scatter', mode = 'linesmarkers', 
              line = list(color = '#515A5A', width = 2),
              marker = list(size = 3, color = '#515A5A'),
              textposition = 'auto', hoverinfo = "text", text = ~comma(Registros, accuracy = 1), 
              hovertext = paste("Fecha:", aux1$Mes, 
                                "<br>Contactos :", comma(aux1$Registros, accuracy = 1))) %>% 
        layout(xaxis = list(gridcolor="#F7F9F9", 
                            tickfont= list(family = "Arial, sans-serif",size = 14,color = "lightgrey"),
                            rangeslider = list(visible = T),
                            rangeselector=list(
                              buttons=list(
                                list(count=1, label="1m", step="month", stepmode="backward"),
                                list(count=6, label="6m", step="month", stepmode="backward"),
                                list(count=1, label="YTD", step="year", stepmode="todate"),
                                list(count=1, label="1y", step="year", stepmode="backward"),
                                list(step="Todos")
                                )
                              )
                            ),
               yaxis = list(tickformat = ",", title="Contactos", gridcolor="#F7F9F9",
                            tickfont= list(family = "Arial, sans-serif",size = 14,color = "lightgrey")),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)') %>% 
        config(displayModeBar=F)
    }
    
  })
  output$Productos <- renderPlotly({
    
    aux1 <- bd_consulta()
    
    aux2 <- bind_rows(
      aux1 %>% group_by(labels = "Contacto", parents = "") %>% summarise(values = n()),
      aux1 %>% group_by(labels = Tipo, parents = "Contacto") %>% summarise(values = n()),
      aux1 %>% group_by(labels = Calidad, parents = Tipo) %>% summarise(values = n())
    )
    
    plot_ly(aux2, labels = ~labels, parents = ~ parents, values = ~values,
            type = 'sunburst',
            branchvalues = 'total',
            textinfo = "label+value",
            hoverinfo = "label+percent entry") %>% 
      layout(colorway = c("#3498DB", "#EC7063", "#73C6B6", "#BFC9CA")) %>% 
      config(displayModeBar=F)
    
  })

  #### Fechas ----
  
  output$ClientesDia = renderDataTable({
    
    datatable(bd_fecha(), options = list(dom = 'tp', searching= T, scrollY = "800px",
                                           language = list(paginate = list('next'="Siguiente", previous="Anterior"))), rownames=F,
              colnames = c("Fecha", "Cliente", "Trader","Tipo", "Calidad", "Comentarios")
    )
    
  })
  
  #### General ----
  ##### Diagrama de Barras -----
  
  RangoDias <- reactiveVal()
  observeEvent(event_data("plotly_click", source = "Dias"), {
    RangoDias(event_data("plotly_click", source = "Dias")$y)
  })
  output$RangosDias <- renderPlotly({
    
    if (is.null(RangoDias())) {
      aux1 <- bd_general() %>% 
        group_by(RangoDias) %>% 
        summarise(Freq=n()) %>% 
        mutate(current_color = "#566573")
    } else {
      aux1 <- bd_general() %>% 
        group_by(RangoDias) %>% 
        summarise(Freq=n()) %>% 
        mutate(current_color = if_else(RangoDias %in% RangoDias(), "#AAB7B8", "#566573"))
    }
    
    aux1$RangoDias <- factor(aux1$RangoDias, levels = c("Más de 90 días", "De 31 a  90 días", "De 16 a 30 días", "De 8 a 15 días", "De 0 a 7 días"))
    
    plot_ly(data=aux1, x=~Freq, y=~RangoDias, type = 'bar', source = "Dias", 
            marker = list(color = ~current_color, line = list(color = "#17202A", width = 1.5)),
            textposition = 'auto', hoverinfo = "text", text = ~comma(Freq, accuracy = 1),
            hovertext = paste(aux1$RangoDias,
                              "<br>", "Clientes: ", comma(aux1$Freq, accuracy = 1))) %>%
      layout(xaxis = list(gridcolor="#F7F9F9", title="Clientes",
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "lightgrey")),
             yaxis = list(tickformat = ",", title="", gridcolor="#F7F9F9",
                          tickfont= list(family = "Arial, sans-serif",size = 14)),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>%
      config(displayModeBar=F) %>% 
      event_register("plotly_click")
    
  })
  
  ##### Tabla de Resumen -----
  
  output$click <- renderText({
    filtro <- event_data("plotly_click", source = "Dias")$y
    ifelse(is.null(filtro), "Todos los Clientes", paste("Clientes con contacto ", filtro))
  })
  
  data_print <- reactive({
    
    filtro <- event_data("plotly_click", source = "Dias", priority = "event")$y
    
    if (is.null(filtro)) {
      aux1 <- bd_general() %>%
        mutate(UltimoContacto = format(as.Date(Timestamp), "%d de %B del %Y")) %>% 
        select(Cliente, UltimoContacto, Dias, Nombre) %>% 
        arrange(desc(Dias))
    } else{
      aux1 <- bd_general() %>%
        filter(RangoDias == filtro) %>% 
        mutate(UltimoContacto = format(as.Date(Timestamp), "%d de %B del %Y")) %>% 
        select(Cliente, UltimoContacto, Dias, Nombre) %>% 
        arrange(desc(Dias))
    }
    
  })
  
  output$DetalleDias = renderDataTable({
    
    datatable(data_print(), options = list(dom = 'tp', searching= T, scrollY = "300px",
                                           language = list(paginate = list('next'="Siguiente", previous="Anterior"))), rownames=F, 
              colnames = c("Cliente", "Fecha de último contacto", "Días desde último contacto", "Última persona de contacto")
    )
    
  })

  ##### Boton de Descarga -----
    
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Clientes.xlsx")
    },
    content = function(file) {
      write_xlsx(data_print(), file)
    }
  )
  
}
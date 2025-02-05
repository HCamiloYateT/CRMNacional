function(input, output, session) { 
  
  # Bases Reactivas -----
  gasto_f <- reactive({
    
    gastos %>% 
      filter(proveedor %in% input$Proveedor,
             concepto %in% input$Concepto,
             between(fechafct, input$Fecha[1], input$Fecha[2]))
    
  })
  gasto_f_alm <- reactive({
    
    gastos %>% 
      filter(proveedor %in% input$Proveedor,
             between(fechafct, input$Fecha[1], input$Fecha[2]))
    
  })
  
  embarque_f <- reactive({
    
    embarques %>%
      filter(puertoorigen %in% input$Puerto,
             terminalmaritimo %in% input$Terminal,
             linea %in% input$Linea
             )
  })
  
  # Embarque -----
  
  ### Generalidades ----
  #### Cajas -----
  
  output$VB_EmbarquesEmbarques <- renderInfoBox({
    
    infoBox(
      value = comma(nrow(embarque_f())),
      title = "Embarques",
      subtitle = "Número de Embarques",
      icon = icon("ship"),
      fill = T,
      color = "navy"
    )
  }) 
  output$VB_EmbarquesReservas <- renderInfoBox({
    
    aux1 <- embarque_f() %>% 
      summarise(val = n_distinct(reserva))
    
    infoBox(
      value = comma(aux1$val),
      title = "Reservas",
      subtitle = "Número de Reservas",
      icon = icon("book-open"),
      fill = T,
      color = "navy"
    )
  }) 
  output$VB_EmbarquesSacos <- renderInfoBox({
    
    aux1 <- embarque_f() %>% 
      summarise(val = sum(sacos))
    
    infoBox(
      value = comma(aux1$val),
      title = "Sacos",
      subtitle = "Número de Sacos",
      icon = icon("th"),
      fill = T,
      color = "navy"
    )
  }) 
  output$VB_EmbarquesKilos <- renderInfoBox({
    aux1 <- embarque_f() %>% 
      summarise(val = sum(Kilos))
    
    infoBox(
      value = comma(aux1$val),
      title = "Kilos",
      subtitle = "Número de Kilos",
      icon = icon("weight"),
      fill = T,
      color = "navy"
    )
  }) 
  output$VB_EmbarquesDiasProm <- renderInfoBox({
    aux1 <- embarque_f() %>% 
      summarise(val = mean(DiasPuertoZarpe, na.rm = T))
    
    infoBox(
      value = comma(aux1$val, accuracy = 0.1),
      title = "Días Hábiles",
      subtitle = "Promedio",
      icon = icon("calendar-day"),
      fill = T,
      color = "navy"
    )
  }) 
  output$VB_EmbarquesDiasPromPond <- renderInfoBox({
    aux1 <- embarque_f() %>% 
      summarise(val = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T))
    
    infoBox(
      value = comma(aux1$val, accuracy = 0.1),
      title = "Días Hábiles",
      subtitle = "Promedio Ponderado",
      icon = icon("calendar-day"),
      fill = T,
      color = "navy"
    )
  }) 
  
  #### Graficas -----
  output$EmbarquesSucursal <- renderPlotly({
    
    aux1 <- embarque_f() %>% 
      filter(!is.na(sucursal)) %>% 
      group_by(sucursal) %>% 
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      ) %>% 
      mutate_("Dim" = "sucursal",
              "Var" = input$Medida) %>% 
      arrange(desc(Var))
    
    aux1$Dim <- sapply(aux1$Dim, 
                       FUN = function(x) {paste(strwrap(x, width = 25), collapse = "<br>")})
    
    aux1$Dim <- factor(aux1$Dim, 
                       levels=c("Otras", unique(aux1$Dim[aux1$Dim != "Otras"])[order(aux1$Var[aux1$Dim != "Otras"], decreasing = F)])
    )
    
    digitos = ifelse(input$Medida %in% c("Embarques", "Reservas", "Sacos", "Kilos", "DiasPuertoZarpeTot"), 1, 0.1)
    promedio = mean(aux1$Var, na.rm = T)
    textos = case_when(input$Medida == "Embarques" ~ "Número de Embarques",
                       input$Medida == "Reservas" ~ "Número de Reservas",
                       input$Medida == "Sacos" ~ "Número de Sacos",
                       input$Medida == "Kilos" ~ "Kilos",
                       input$Medida == "DiasPuertoZarpeTot" ~ "Días de Almacenamiento Totales",
                       input$Medida == "DiasPuertoZarpeProm" ~ "Días de Almacenamiento Promedio",
                       input$Medida == "DiasPuertoZarpePromPond" ~ "Días de Almacenamiento Promedio Ponderado"
                       )
    
    plot_ly(data=aux1, x=~Var, y=~Dim, type = 'bar', orientation = 'h', textposition = 'auto', 
            hoverinfo = "text", text = ~comma(Var, accuracy = digitos), 
            marker = list(color = "#717D7E"),
            hovertext = paste("<b>Sucursal: </b>", aux1$Dim, 
                              "<br><b>Embarques :</b>", comma(aux1$Embarques, accuracy = 1),
                              "<br><b>Reservas :</b>", comma(aux1$Reservas, accuracy = 1),
                              "<br><b>Sacos :</b>", comma(aux1$Sacos, accuracy = 1),
                              "<br><b>Kilos :</b>", comma(aux1$Kilos, accuracy = 1),
                              "<br><b>Días de Almacenaje Totales :</b>", comma(aux1$DiasPuertoZarpeTot, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Promedio  :</b>", comma(aux1$DiasPuertoZarpeProm, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Ponderados por Sacos  :</b>", comma(aux1$DiasPuertoZarpePromPond, accuracy = 0.1))
            ) %>% 
      layout(title = paste("Distribución de", textos, "por Sucursal"),
             xaxis = list(tickformat = ",", title=textos),
             yaxis = list(title=""),
             shapes = list(vline(promedio)),
             annotations = list(text = paste("<b>Promedio: </b>", comma(promedio, accuracy = 0.1)),  
                                x = promedio, showarrow=F, xanchor = 'left',
                                bgcolor="#AAB7B8"),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)'
             )  %>% 
      config(displayModeBar=F)
    
    
  })
  output$EmbarquesPuertoOrigen <- renderPlotly({
    
    aux1 <- embarque_f() %>% 
      filter(!is.na(puertoorigen)) %>% 
      group_by(puertoorigen) %>% 
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      ) %>% 
      mutate_("Dim" = "puertoorigen",
              "Var" = input$Medida) %>% 
      arrange(desc(Var))
    
    aux1$Dim <- sapply(aux1$Dim, 
                       FUN = function(x) {paste(strwrap(x, width = 25), collapse = "<br>")})
    
    aux1$Dim <- factor(aux1$Dim, 
                       levels=c("Otras", unique(aux1$Dim[aux1$Dim != "Otras"])[order(aux1$Var[aux1$Dim != "Otras"], decreasing = F)])
    )
    
    digitos = ifelse(input$Medida %in% c("Embarques", "Reservas", "Sacos", "Kilos", "DiasPuertoZarpeTot"), 1, 0.1)
    promedio = mean(aux1$Var, na.rm = T)
    textos = case_when(input$Medida == "Embarques" ~ "Número de Embarques",
                       input$Medida == "Reservas" ~ "Número de Reservas",
                       input$Medida == "Sacos" ~ "Número de Sacos",
                       input$Medida == "Kilos" ~ "Kilos",
                       input$Medida == "DiasPuertoZarpeTot" ~ "Días de Almacenamiento Totales",
                       input$Medida == "DiasPuertoZarpeProm" ~ "Días de Almacenamiento Promedio",
                       input$Medida == "DiasPuertoZarpePromPond" ~ "Días de Almacenamiento Promedio Ponderado"
    )
    
    plot_ly(data=aux1, x=~Var, y=~Dim, type = 'bar', orientation = 'h', textposition = 'auto', 
            hoverinfo = "text", text = ~comma(Var, accuracy = digitos), 
            marker = list(color = "#717D7E"),
            hovertext = paste("<b>Sucursal: </b>", aux1$Dim, 
                              "<br><b>Embarques :</b>", comma(aux1$Embarques, accuracy = 1),
                              "<br><b>Reservas :</b>", comma(aux1$Reservas, accuracy = 1),
                              "<br><b>Sacos :</b>", comma(aux1$Sacos, accuracy = 1),
                              "<br><b>Kilos :</b>", comma(aux1$Kilos, accuracy = 1),
                              "<br><b>Días de Almacenaje Totales :</b>", comma(aux1$DiasPuertoZarpeTot, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Promedio  :</b>", comma(aux1$DiasPuertoZarpeProm, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Ponderados por Sacos  :</b>", comma(aux1$DiasPuertoZarpePromPond, accuracy = 0.1))
    ) %>% 
      layout(title = paste("Distribución de", textos, "por Puerto de Origen"),
             xaxis = list(tickformat = ",", title=textos),
             yaxis = list(title=""),
             shapes = list(vline(promedio)),
             annotations = list(text = paste("<b>Promedio: </b>", comma(promedio, accuracy = 0.1)),  
                                x = promedio, showarrow=F, xanchor = 'left',
                                bgcolor="#AAB7B8"),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)'
      )  %>% 
      config(displayModeBar=F)
    
    
  })
  output$EmbarquesTerminal <- renderPlotly({
    
    aux1 <- embarque_f() %>% 
      filter(!is.na(terminalmaritimo)) %>% 
      group_by(terminalmaritimo) %>% 
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      ) %>% 
      mutate_("Dim" = "terminalmaritimo",
              "Var" = input$Medida) %>% 
      arrange(desc(Var))
    
    aux1$Dim <- sapply(aux1$Dim, 
                       FUN = function(x) {paste(strwrap(x, width = 25), collapse = "<br>")})
    
    aux1$Dim <- factor(aux1$Dim, 
                       levels=c("Otras", unique(aux1$Dim[aux1$Dim != "Otras"])[order(aux1$Var[aux1$Dim != "Otras"], decreasing = F)])
    )
    
    digitos = ifelse(input$Medida %in% c("Embarques", "Reservas", "Sacos", "Kilos", "DiasPuertoZarpeTot"), 1, 0.1)
    promedio = mean(aux1$Var, na.rm = T)
    textos = case_when(input$Medida == "Embarques" ~ "Número de Embarques",
                       input$Medida == "Reservas" ~ "Número de Reservas",
                       input$Medida == "Sacos" ~ "Número de Sacos",
                       input$Medida == "Kilos" ~ "Kilos",
                       input$Medida == "DiasPuertoZarpeTot" ~ "Días de Almacenamiento Totales",
                       input$Medida == "DiasPuertoZarpeProm" ~ "Días de Almacenamiento Promedio",
                       input$Medida == "DiasPuertoZarpePromPond" ~ "Días de Almacenamiento Promedio Ponderado"
    )
    
    plot_ly(data=aux1, x=~Var, y=~Dim, type = 'bar', orientation = 'h', textposition = 'auto', 
            hoverinfo = "text", text = ~comma(Var, accuracy = digitos), 
            marker = list(color = "#717D7E"),
            hovertext = paste("<b>Sucursal: </b>", aux1$Dim, 
                              "<br><b>Embarques :</b>", comma(aux1$Embarques, accuracy = 1),
                              "<br><b>Reservas :</b>", comma(aux1$Reservas, accuracy = 1),
                              "<br><b>Sacos :</b>", comma(aux1$Sacos, accuracy = 1),
                              "<br><b>Kilos :</b>", comma(aux1$Kilos, accuracy = 1),
                              "<br><b>Días de Almacenaje Totales :</b>", comma(aux1$DiasPuertoZarpeTot, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Promedio  :</b>", comma(aux1$DiasPuertoZarpeProm, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Ponderados por Sacos  :</b>", comma(aux1$DiasPuertoZarpePromPond, accuracy = 0.1))
    ) %>% 
      layout(title = paste("Distribución de", textos, "por Terminal Maritimo"),
             xaxis = list(tickformat = ",", title=textos),
             yaxis = list(title=""),
             shapes = list(vline(promedio)),
             annotations = list(text = paste("<b>Promedio: </b>", comma(promedio, accuracy = 0.1)),  
                                x = promedio, showarrow=F, xanchor = 'left',
                                bgcolor="#AAB7B8"),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)'
      )  %>% 
      config(displayModeBar=F)
    
    
  })
  output$EmbarquesNaviera <- renderPlotly({
    
    aux1 <- embarque_f() %>% 
      filter(!is.na(linea)) %>% 
      group_by(linea) %>% 
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      ) %>% 
      mutate_("Dim" = "linea",
              "Var" = input$Medida) %>% 
      arrange(desc(Var))
    
    aux1$Dim <- sapply(aux1$Dim, 
                       FUN = function(x) {paste(strwrap(x, width = 25), collapse = "<br>")})
    
    aux1$Dim <- factor(aux1$Dim, 
                       levels=c("Otras", unique(aux1$Dim[aux1$Dim != "Otras"])[order(aux1$Var[aux1$Dim != "Otras"], decreasing = F)])
    )
    
    digitos = ifelse(input$Medida %in% c("Embarques", "Reservas", "Sacos", "Kilos", "DiasPuertoZarpeTot"), 1, 0.1)
    promedio = mean(aux1$Var, na.rm = T)
    textos = case_when(input$Medida == "Embarques" ~ "Número de Embarques",
                       input$Medida == "Reservas" ~ "Número de Reservas",
                       input$Medida == "Sacos" ~ "Número de Sacos",
                       input$Medida == "Kilos" ~ "Kilos",
                       input$Medida == "DiasPuertoZarpeTot" ~ "Días de Almacenamiento Totales",
                       input$Medida == "DiasPuertoZarpeProm" ~ "Días de Almacenamiento Promedio",
                       input$Medida == "DiasPuertoZarpePromPond" ~ "Días de Almacenamiento Promedio Ponderado"
    )
    
    plot_ly(data=aux1, x=~Var, y=~Dim, type = 'bar', orientation = 'h', textposition = 'auto', 
            hoverinfo = "text", text = ~comma(Var, accuracy = digitos), 
            marker = list(color = "#717D7E"),
            hovertext = paste("<b>Sucursal: </b>", aux1$Dim, 
                              "<br><b>Embarques :</b>", comma(aux1$Embarques, accuracy = 1),
                              "<br><b>Reservas :</b>", comma(aux1$Reservas, accuracy = 1),
                              "<br><b>Sacos :</b>", comma(aux1$Sacos, accuracy = 1),
                              "<br><b>Kilos :</b>", comma(aux1$Kilos, accuracy = 1),
                              "<br><b>Días de Almacenaje Totales :</b>", comma(aux1$DiasPuertoZarpeTot, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Promedio  :</b>", comma(aux1$DiasPuertoZarpeProm, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Ponderados por Sacos  :</b>", comma(aux1$DiasPuertoZarpePromPond, accuracy = 0.1))
    ) %>% 
      layout(title = paste("Distribución de", textos, "por Naviera"),
             xaxis = list(tickformat = ",", title=textos),
             yaxis = list(title=""),
             shapes = list(vline(promedio)),
             annotations = list(text = paste("<b>Promedio: </b>", comma(promedio, accuracy = 0.1)),  
                                x = promedio, showarrow=F, xanchor = 'left',
                                bgcolor="#AAB7B8"),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)'
      )  %>% 
      config(displayModeBar=F)
    
    
  })
  output$EmbarquesPaisDestino <- renderPlotly({
    
    aux0 <- embarque_f() %>% 
      filter(!is.na(paisdestino)) %>% 
      group_by(paisdestino) %>% 
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      ) %>% 
      mutate_("Dim" = "paisdestino",
              "Var" = "Embarques") %>% 
      arrange(desc(Var)) %>% 
      top_n(10, Var) %>% 
      select(paisdestino)
    
    aux1 <- embarque_f() %>% 
      filter(!is.na(paisdestino)) %>% 
      mutate(Pais = ifelse(paisdestino %in% aux0$paisdestino, paisdestino, "OTROS")) %>% 
      group_by(Pais) %>% 
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      ) %>% 
      mutate_("Dim" = "Pais",
              "Var" = input$Medida) %>% 
      arrange(desc(Var))
    
    aux1$Dim <- sapply(aux1$Dim, 
                       FUN = function(x) {paste(strwrap(x, width = 25), collapse = "<br>")})
    
    aux1$Dim <- factor(aux1$Dim, 
                       levels=c("OTROS", unique(aux1$Dim[aux1$Dim != "OTROS"])[order(aux1$Var[aux1$Dim != "OTROS"], decreasing = F)])
    )
    
    digitos = ifelse(input$Medida %in% c("Embarques", "Reservas", "Sacos", "Kilos", "DiasPuertoZarpeTot"), 1, 0.1)
    promedio = mean(aux1$Var, na.rm = T)
    textos = case_when(input$Medida == "Embarques" ~ "Número de Embarques",
                       input$Medida == "Reservas" ~ "Número de Reservas",
                       input$Medida == "Sacos" ~ "Número de Sacos",
                       input$Medida == "Kilos" ~ "Kilos",
                       input$Medida == "DiasPuertoZarpeTot" ~ "Días de Almacenamiento Totales",
                       input$Medida == "DiasPuertoZarpeProm" ~ "Días de Almacenamiento Promedio",
                       input$Medida == "DiasPuertoZarpePromPond" ~ "Días de Almacenamiento Promedio Ponderado"
    )
    
    plot_ly(data=aux1, x=~Var, y=~Dim, type = 'bar', orientation = 'h', textposition = 'auto', 
            hoverinfo = "text", text = ~comma(Var, accuracy = digitos), 
            marker = list(color = "#717D7E"),
            hovertext = paste("<b>Sucursal: </b>", aux1$Dim, 
                              "<br><b>Embarques :</b>", comma(aux1$Embarques, accuracy = 1),
                              "<br><b>Reservas :</b>", comma(aux1$Reservas, accuracy = 1),
                              "<br><b>Sacos :</b>", comma(aux1$Sacos, accuracy = 1),
                              "<br><b>Kilos :</b>", comma(aux1$Kilos, accuracy = 1),
                              "<br><b>Días de Almacenaje Totales :</b>", comma(aux1$DiasPuertoZarpeTot, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Promedio  :</b>", comma(aux1$DiasPuertoZarpeProm, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Ponderados por Sacos  :</b>", comma(aux1$DiasPuertoZarpePromPond, accuracy = 0.1))
    ) %>% 
      layout(title = paste("Distribución de", textos, "por País de Destino (Top 10)"),
             xaxis = list(tickformat = ",", title=textos),
             yaxis = list(title=""),
             shapes = list(vline(promedio)),
             annotations = list(text = paste("<b>Promedio: </b>", comma(promedio, accuracy = 0.1)),  
                                x = promedio, showarrow=F, xanchor = 'left',
                                bgcolor="#AAB7B8"),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)'
      )  %>% 
      config(displayModeBar=F)
    
    
  })
  output$EmbarquesCliente <- renderPlotly({
    
    aux0 <- embarque_f() %>% 
      filter(!is.na(cliente)) %>% 
      group_by(cliente) %>% 
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      ) %>% 
      mutate_("Dim" = "cliente",
              "Var" = "Embarques") %>% 
      arrange(desc(Var)) %>% 
      top_n(10, Var) %>% 
      select(cliente)
    
    aux1 <- embarque_f() %>% 
      filter(!is.na(cliente)) %>% 
      mutate(Cliente = ifelse(cliente %in% aux0$cliente, cliente, "OTROS")) %>% 
      group_by(Cliente) %>% 
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      ) %>% 
      mutate_("Dim" = "Cliente",
              "Var" = input$Medida) %>% 
      arrange(desc(Var))
    
    aux1$Dim <- sapply(aux1$Dim, 
                       FUN = function(x) {paste(strwrap(x, width = 25), collapse = "<br>")})
    
    aux1$Dim <- factor(aux1$Dim, 
                       levels=c("OTROS", unique(aux1$Dim[aux1$Dim != "OTROS"])[order(aux1$Var[aux1$Dim != "OTROS"], decreasing = F)])
    )
    
    digitos = ifelse(input$Medida %in% c("Embarques", "Reservas", "Sacos", "Kilos", "DiasPuertoZarpeTot"), 1, 0.1)
    promedio = mean(aux1$Var, na.rm = T)
    textos = case_when(input$Medida == "Embarques" ~ "Número de Embarques",
                       input$Medida == "Reservas" ~ "Número de Reservas",
                       input$Medida == "Sacos" ~ "Número de Sacos",
                       input$Medida == "Kilos" ~ "Kilos",
                       input$Medida == "DiasPuertoZarpeTot" ~ "Días de Almacenamiento Totales",
                       input$Medida == "DiasPuertoZarpeProm" ~ "Días de Almacenamiento Promedio",
                       input$Medida == "DiasPuertoZarpePromPond" ~ "Días de Almacenamiento Promedio Ponderado"
    )
    
    plot_ly(data=aux1, x=~Var, y=~Dim, type = 'bar', orientation = 'h', textposition = 'auto', 
            hoverinfo = "text", text = ~comma(Var, accuracy = digitos), 
            marker = list(color = "#717D7E"),
            hovertext = paste("<b>Sucursal: </b>", aux1$Dim, 
                              "<br><b>Embarques :</b>", comma(aux1$Embarques, accuracy = 1),
                              "<br><b>Reservas :</b>", comma(aux1$Reservas, accuracy = 1),
                              "<br><b>Sacos :</b>", comma(aux1$Sacos, accuracy = 1),
                              "<br><b>Kilos :</b>", comma(aux1$Kilos, accuracy = 1),
                              "<br><b>Días de Almacenaje Totales :</b>", comma(aux1$DiasPuertoZarpeTot, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Promedio  :</b>", comma(aux1$DiasPuertoZarpeProm, accuracy = 0.1),
                              "<br><b>Días de Almacenaje Ponderados por Sacos  :</b>", comma(aux1$DiasPuertoZarpePromPond, accuracy = 0.1))
    ) %>% 
      layout(title = paste("Distribución de", textos, "por Cliente (Top 10)"),
             xaxis = list(tickformat = ",", title=textos),
             yaxis = list(title=""),
             shapes = list(vline(promedio)),
             annotations = list(text = paste("<b>Promedio: </b>", comma(promedio, accuracy = 0.1)),  
                                x = promedio, showarrow=F, xanchor = 'left',
                                bgcolor="#AAB7B8"),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)'
      )  %>% 
      config(displayModeBar=F)
    
    
  })
  
  ### Flujos ----
  
  output$EmbarquesSankey <- renderPlotly({
    
    n_ent =  input$NumEntradas
    n_sal =  input$NumSalidas
    
    aux0 <- embarque_f() %>% 
      mutate_("Entrada" = input$NodoInicial,
              "Salida" = input$NodoFinal
              )
    
    aux1 <- aux0 %>%
      group_by(Entrada) %>%
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      )  %>% 
      mutate_("Var" = input$Medida) %>% 
      arrange(desc(Var)) %>%
      top_n(n_ent, Var) %>%
      mutate(Origen=Entrada) %>%
      select(Origen, Entrada)
    
    aux2 <- aux0 %>%
      group_by(Salida) %>%
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      )  %>% 
      mutate_("Var" = input$Medida) %>% 
      arrange(desc(Var)) %>%
      top_n(n_sal, Var) %>%
      mutate(Destino=Salida) %>%
      select(Destino, Salida)
    
    aux3 <- aux0 %>%
      left_join(aux1, by = c("Entrada")) %>%
      left_join(aux2, by = c("Salida")) %>%
      mutate(Origen = ifelse(is.na(Origen), "OTROS ORIGENES", Origen),
             Destino = ifelse(is.na(Destino), "OTROS DESTINOS", Destino)
      ) %>%
      group_by(Origen, Destino) %>%
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      )  %>% 
      mutate_("Var" = input$Medida) %>% 
      ungroup()
    
    n_seq <- length(c(unique(aux3$Origen), unique(aux3$Destino)))
    
    Nodes <- bind_cols(node=seq(0,n_seq-1), 
                       bind_rows(
                         aux3 %>% 
                           mutate(color=ifelse(Origen == "RACAFE & CIA S C A", "firebrick", "gray")) %>% 
                           select(name=Origen, color) %>% 
                           distinct(),
                         aux3 %>% 
                           mutate(color=ifelse(Destino == "RACAFE & CIA S C A", "firebrick", "gray")) %>% 
                           select(name=Destino) %>% 
                           distinct()
                       )
    ) %>% 
      data.frame()
    
    Links <- bind_rows(aux3) %>% 
      left_join(Nodes, by=c("Origen"="name")) %>% 
      left_join(Nodes, by=c("Destino"="name")) %>% 
      mutate(LinkColor=ifelse(Origen == "RACAFE & CIA S C A", "firebrick", "seashell")) %>% 
      select(source=node.x, target=node.y, value=Var, LinkColor) 
    
    fig <- plot_ly( type = "sankey", orientation = "h", valuesuffix = paste(" ",input$Medida), 
                    source = "Sankey1",arrangement='fixed',
                    node = list(
                      label = Nodes$name,
                      color = Nodes$color,
                      pad = 15,
                      thickness = 20,
                      hovertemplate="%{label}"
                    ),
                    link = list(
                      source = Links$source,
                      target = Links$target,
                      value = Links$value
                      # color = Links$LinkColor
                    )
    ) %>% 
      layout(title = "Flujo de Distribucion de Embarques",
             font = list(size = 10)         
      ) %>%
      config(displayModeBar=F) %>% 
      event_register("plotly_click");fig
    
    
    
  })
  output$EmbarqueSankeyDetalle <- DT::renderDataTable({
    
    n_ent =  input$NumEntradas
    n_sal =  input$NumSalidas
    
    aux0 <- embarque_f() %>% 
      mutate_("Entrada" = input$NodoInicial,
              "Salida" = input$NodoFinal
      )
    
    aux1 <- aux0 %>%
      group_by(Entrada) %>%
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      )  %>% 
      mutate_("Var" = input$Medida) %>% 
      arrange(desc(Var)) %>%
      top_n(n_ent, Var) %>%
      mutate(Origen=Entrada) %>%
      select(Origen, Entrada)
    
    aux2 <- aux0 %>%
      group_by(Salida) %>%
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
                )  %>% 
      mutate_("Var" = input$Medida) %>% 
      arrange(desc(Var)) %>%
      top_n(n_sal, Var) %>%
      mutate(Destino=Salida) %>%
      select(Destino, Salida)
    
    aux3 <- aux0 %>%
      left_join(aux1, by = c("Entrada")) %>%
      left_join(aux2, by = c("Salida")) %>%
      mutate(Origen = ifelse(is.na(Origen), "OTROS ORIGENES", Origen),
             Destino = ifelse(is.na(Destino), "OTROS DESTINOS", Destino)
      ) %>%
      group_by(Origen, Destino) %>%
      summarise(Embarques = n(),
                Reservas = n_distinct(reserva),
                Sacos = sum(sacos, na.rm = T),
                Kilos = sum(Kilos, na.rm = T),
                DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
      )  %>% 
      mutate_("Var" = input$Medida) %>% 
      ungroup()
    
    n_seq <- length(c(unique(aux3$Origen), unique(aux3$Destino)))
    
    Nodes <- bind_cols(node=seq(0,n_seq-1), 
                       bind_rows(
                         aux3 %>% 
                           mutate(color=ifelse(Origen == "RACAFE & CIA S C A", "firebrick", "gray")) %>% 
                           select(name=Origen, color) %>% 
                           distinct(),
                         aux3 %>% 
                           mutate(color=ifelse(Destino == "RACAFE & CIA S C A", "firebrick", "gray")) %>% 
                           select(name=Destino) %>% 
                           distinct()
                       )
    ) %>% 
      data.frame()
    
    valor <- Nodes %>% 
      filter(node==event_data("plotly_click", source = "Sankey1")$pointNumber) %>% 
      select(name) %>%
      as.character()

    resultado <- if(valor %in% c("OTROS ORIGENES")){
      aux0 %>%
        anti_join(Nodes, by=c("Entrada"="name")) %>%
        group_by(Entrada, Salida) %>%
        summarise(Embarques = n(),
                  Reservas = n_distinct(reserva),
                  Sacos = sum(sacos, na.rm = T),
                  Kilos = sum(Kilos, na.rm = T),
                  DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                  DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                  DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
                  )  %>%
        mutate_("Var" = input$Medida) %>%
        arrange(desc(Var))
    }
    else if (event_data("plotly_click", source = "Sankey1")$pointNumber <= n_ent){
      aux0 %>%
        filter(Entrada==valor) %>%
        group_by(Entrada, Salida) %>%
        summarise(Embarques = n(),
                  Reservas = n_distinct(reserva),
                  Sacos = sum(sacos, na.rm = T),
                  Kilos = sum(Kilos, na.rm = T),
                  DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                  DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                  DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
        )  %>%
        mutate_("Var" = input$Medida) %>%
        arrange(desc(Var))
    }
    else if (valor %in% c("OTROS DESTINOS")){
      aux0 %>%
        anti_join(Nodes, by=c("Salida"="name")) %>%
        group_by(Entrada, Salida) %>%
        summarise(Embarques = n(),
                  Reservas = n_distinct(reserva),
                  Sacos = sum(sacos, na.rm = T),
                  Kilos = sum(Kilos, na.rm = T),
                  DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                  DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                  DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
        )  %>%
        mutate_("Var" = input$Medida) %>%
        arrange(desc(Var))
    }
    else if (event_data("plotly_click", source = "Sankey1")$pointNumber <= n_ent+n_sal+1){
      aux0 %>%
        filter(Salida==valor) %>%
        group_by(Entrada, Salida) %>%
        summarise(Embarques = n(),
                  Reservas = n_distinct(reserva),
                  Sacos = sum(sacos, na.rm = T),
                  Kilos = sum(Kilos, na.rm = T),
                  DiasPuertoZarpeTot = sum(DiasPuertoZarpe, na.rm = T),
                  DiasPuertoZarpeProm = mean(DiasPuertoZarpe, na.rm = T),
                  DiasPuertoZarpePromPond = weighted.mean(DiasPuertoZarpe, sacos , na.rm = T)
        )  %>%
        mutate_("Var" = input$Medida) %>%
        arrange(desc(Var))
    }
    
    datatable(resultado %>% select(-Var), options=list(pageLength =15, dom = 'tsp', 
                                                       searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c(input$NodoInicial, input$NodoFinal, "Embarques", "Reservas", "Sacos",
                           "Kilos", "Total Días de Almacenamiento", "Promedio Días de Almacenamiento",
                           "Promedio Ponderado Dias de Almacenamiento")) %>%
      formatRound(c("Embarques", "Reservas","Sacos", "Kilos", "DiasPuertoZarpeTot"), digits = 0) %>% 
      formatRound(c("DiasPuertoZarpeProm", "DiasPuertoZarpePromPond"), digits = 2)

  })
  

  ## Gasto ----
  
  ### Generalidades ----
  
  #### Cajas ----
  output$IB_Registros <- renderInfoBox({
    infoBox(
      value = comma(nrow(gasto_f())),
      title = "",
      subtitle = "Registros con Gasto",
      icon = icon("bars"),
      fill = F,
      color = "blue"
    )
  }) 
  output$IB_Lotes <- renderInfoBox({
    infoBox(
      value = comma(length(unique(gasto_f()$lote))),
      title = "",
      subtitle = "Lotes con Gasto",
      icon = icon("truck-loading"),
      fill = F,
      color = "blue"
    )
  }) 
  output$IB_Sacos <- renderInfoBox({
    infoBox(
      value = comma(sum(gasto_f()$sacosgto)),
      title = "",
      subtitle = "Sacos con Gasto",
      icon = icon("box"),
      fill = F,
      color = "blue"
    )
  }) 
  output$IB_Gasto <- renderInfoBox({
    infoBox(
      value = dollar(sum(gasto_f()$valorgto)),
      title = "",
      subtitle = "Gasto Total",
      icon = icon("hand-holding-usd"),
      fill = F,
      color = "blue"
    )
  }) 
  
  #### General ----
  output$Serie1 <- renderPlotly({
    
    aux1 <- gasto_f() %>% 
      group_by(Fecha= format(fechafct, "%Y-%m")) %>%
      summarise(Gastos = sum(valorgto, na.rm = T)) 
    
    
    plot_ly(data=aux1, x=~Fecha, y=~Gastos, type = 'scatter', mode = 'linesmarkers', 
            textposition = 'auto', hoverinfo = "text", text = ~dollar(Gastos), 
            hovertext = paste("Mes:", aux1$Fecha, 
                              "<br>Gasto Total :", dollar(aux1$Gastos))) %>% 
      layout(yaxis = list(tickformat = "$,", title="Gasto")) %>% 
      config(displayModeBar=F)
    
  })
  output$Concepto1 <- renderPlotly({
    
    aux1 <- gasto_f() %>% 
      mutate(Total=sum(valorgto, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(concepto) %>%
      summarise(Gastos = sum(valorgto, na.rm = T),
                Pct = sum(valorgto, na.rm = T)/max(Total)) %>% 
      arrange(desc(Gastos))
    
    aux1$concepto <- factor(aux1$concepto, levels = unique(aux1$concepto)[order(aux1$Gastos, decreasing = F)])
    
    plot_ly(data=aux1, x=~Gastos, y=~concepto, type = 'bar', orientation = 'h', textposition = 'auto', hoverinfo = "text", text = ~dollar(Gastos), 
            hovertext = paste("Concepto:", aux1$concepto, 
                              "<br>Gasto Total :", dollar(aux1$Gastos),
                              "<br>Porcentaje :", percent(aux1$Pct, accuracy = 0.1))
    ) %>% 
      layout(xaxis = list(tickformat = "$,", title="Gasto"),
             yaxis = list(title="")) %>% 
      config(displayModeBar=F)
    
  })

  #### Almacenaje ----
  output$Tiempos1 <- renderPlotly({
    
    aux1 <- gasto_f_alm() %>% 
      filter(concepto == "ALMACENAJE") %>% 
      select(conceptoacenaje, diasalm)
    
    plot_ly(data=aux1, y=~conceptoacenaje, x=~diasalm, type = "box") %>% 
      layout(xaxis = list(tickformat = ",", title="Dias de Almacenaje"),
             yaxis = list(title="")) %>% 
      config(displayModeBar=F)
    
  })
  output$Almacenamiento1 <- renderPlotly({
    
    aux1 <- gasto_f_alm() %>% 
      filter(concepto == "ALMACENAJE") %>% 
      mutate(Total=sum(valorgto, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(conceptoacenaje) %>%
      summarise(Gastos = sum(valorgto, na.rm = T),
                Lotes = n_distinct(lote),
                Registros= n(),
                Pct = sum(valorgto, na.rm = T)/max(Total)) %>% 
      arrange(desc(Gastos))
    
    aux1$conceptoacenaje <- factor(aux1$conceptoacenaje, levels = unique(aux1$conceptoacenaje)[order(aux1$Gastos, decreasing = F)])
    
    plot_ly(data=aux1, x=~Gastos, y=~conceptoacenaje, type = 'bar', orientation = 'h', textposition = 'auto', hoverinfo = "text", text = ~dollar(Gastos), 
            hovertext = paste("Concepto Almacenaje:", aux1$conceptoacenaje, 
                              "<br>Gasto Total :", dollar(aux1$Gastos),
                              "<br>Porcentaje :", percent(aux1$Pct, accuracy = 0.1),
                              "<br>Lotes :", comma(aux1$Lotes, accuracy = 1))
    ) %>% 
      layout(xaxis = list(tickformat = "$,", title="Gasto"),
             yaxis = list(title="")) %>% 
      config(displayModeBar=F)
    
  })
  
  #### Otros Gastos ----
  output$Otros1 <- renderPlotly({
    
    aux1 <- gasto_f_alm() %>% 
      filter(concepto == "OTROS GASTOS") %>% 
      mutate(Total=sum(valorgto, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(otrosgastos) %>%
      summarise(Gastos = sum(valorgto, na.rm = T),
                Lotes = n_distinct(lote),
                Registros= n(),
                Pct = sum(valorgto, na.rm = T)/max(Total)) %>% 
      arrange(desc(Gastos))
    
    aux1$otrosgastos <- factor(aux1$otrosgastos, levels = unique(aux1$otrosgastos)[order(aux1$Gastos, decreasing = F)])
    
    plot_ly(data=aux1, x=~Gastos, y=~otrosgastos, type = 'bar', orientation = 'h', textposition = 'auto', hoverinfo = "text", text = ~dollar(Gastos), 
            hovertext = paste("Otros Gastos:", aux1$otrosgastos, 
                              "<br>Gasto Total :", dollar(aux1$Gastos),
                              "<br>Porcentaje :", percent(aux1$Pct, accuracy = 0.1),
                              "<br>Lotes :", comma(aux1$Lotes, accuracy = 1))
    ) %>% 
      layout(xaxis = list(tickformat = "$,", title="Gasto"),
             yaxis = list(title="")) %>% 
      config(displayModeBar=F)
    
  })
  
}



function(input, output, session) { 
  
  data_f <- reactive({
    
    data %>% 
      dplyr::filter(fechafct >= input$FechaFactura[1], fechafct<= input$FechaFactura[2]) %>% 
      select(fechafct:code, contains(input$dias))
    
  })
  
  #### Ciclo 1 ----
  
  ### Value boxes ----
  
  output$C1_transacciones <- renderValueBox({

    valueBox(

      comma(nrow(data_f())),
      "Transacciones",
      color = "yellow",
      icon = icon("file-invoice")

    )

  })
  output$C1_SacosDespachados <- renderValueBox({
    
    aux <- data_f() %>% summarise(SacosDespachados= sum(sacosdsp, na.rm = T)) %>% as.numeric()
    
    valueBox(
      
      comma(aux),
      "Sacos Despachados",
      color = "yellow",
      icon = icon("dolly-flatbed")
      
    )
    
  })
  output$C1_dias_promedio <- renderValueBox({
    
    aux <- data_f() %>%
      rename_at(vars(contains("Ciclo1", )), ~paste("Variable")) %>% 
      summarise(Promedio = mean(Variable, na.rm = T)) %>% 
      as.numeric()
    
    valueBox(
      
      comma(aux, accuracy = 0.01),
      "Promedio de dias",
      color = "yellow",
      icon = icon("calendar")
      
    )
    
  })
  output$C1_promedio_ponderado <- renderValueBox({
    
    aux <- data_f() %>%
      rename_at(vars(contains("Ciclo1", )), ~paste("Variable")) %>% 
      summarise(Promedio = weighted.mean(Variable, sacosdsp, na.rm = T)) %>% 
      as.numeric()
    
    valueBox(
      
      comma(aux, accuracy = 0.01),
      "Promedio de dias Ponderado por Sacos",
      color = "yellow",
      icon = icon("calendar-check")
      
    )
    
  })
  
  ### Fila de Dias Simples ----
  
  output$C1_BarrasTrilladoraDIa <- renderPlotly({
    
    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo1")), ~paste("Variable")) %>% 
      group_by(trilladora) %>% 
      summarise(Promedio = mean(Variable, na.rm = T)) %>% 
      mutate(Color = ifelse(Promedio > input$C1_MetaDias, "No Cumple", "Cumple"))
    
    
    plot_ly(aux1, y=~trilladora, x=~Promedio, type = "bar", hoverinfo = "text", color = ~Color,
            textposition = 'auto', text = ~comma(Promedio, accuracy = 0.01),
            hovertext = paste("Trilladora :", aux1$trilladora, 
                              "<br>", "Promedio de Días: ", comma(aux1$Promedio))) %>%
      layout() %>% 
      layout(shapes = list(vline(input$C1_MetaDias) ),
             title= paste("Tiempo promedio en dias", input$dias),
             yaxis= list(tickformat="", title=""),
             xaxis= list(tickformat=",", title="Promedio de Días")) %>%
      config(displayModeBar=F)
    
  })
  output$C1_TablaDias <- DT::renderDataTable({
    
    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo1")), ~paste("Variable")) %>% 
      group_by(trilladora) %>% 
      summarise(Promedio = mean(Variable, na.rm = T)) %>% 
      mutate(Diferencia = Promedio - input$C1_MetaDias)
    
    datatable(aux1, options=list(pageLength =10, dom = 'tpl'), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Trilladora", "Promedio en Días", "Diferencia")) %>%
      formatRound(c("Promedio", "Diferencia"), digits = 2)
    
  })

  ### Fila de Promedio Ponderado ----
  
  output$C1_BarrasTrilladoraPromedio <- renderPlotly({
    
    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo1")), ~paste("Variable")) %>% 
      group_by(trilladora) %>% 
      summarise(Promedio =weighted.mean(Variable, sacosdsp, na.rm = T)) %>% 
      mutate(Color = ifelse(Promedio > input$C1_MetaPromedio, "No Cumple", "Cumple"))
    
    
    plot_ly(aux1, y=~trilladora, x=~Promedio, type = "bar", hoverinfo = "text", color = ~Color,
            textposition = 'auto', text = ~comma(Promedio, accuracy = 0.01),
            hovertext = paste("Trilladora :", aux1$trilladora, 
                              "<br>", "Promedio de Días: ", comma(aux1$Promedio))) %>%
      layout() %>% 
      layout(shapes = list(vline(input$C1_MetaPromedio) ),
             title= paste("Tiempo Promedio en Días", input$dias),
             yaxis= list(tickformat="", title=""),
             xaxis= list(tickformat=",", title="Promedio de Días")) %>%
      config(displayModeBar=F)
    
  })
  output$C1_Tablapromedio <- DT::renderDataTable({
    
    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo1")), ~paste("Variable")) %>% 
      group_by(trilladora) %>% 
      summarise(Promedio =weighted.mean(Variable, sacosdsp, na.rm = T)) %>% 
      mutate(Diferencia = Promedio - input$C1_MetaPromedio)
    
    datatable(aux1, options=list(pageLength =10, dom = 'tpl'), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Trilladora", "Promedio en Días", "Diferencia")) %>%
      formatRound(c("Promedio", "Diferencia"), digits = 2)
    
  })
  
  
  #### Ciclo 2 ----

  ### Medicion por Naviera ----

  ## Value boxes ----

  output$C2_transacciones <- renderValueBox({

    valueBox(

      comma(nrow(data_f())),
      "Transacciones",
      color = "yellow",
      icon = icon("file-invoice")

    )

  })
  output$C2_SacosDespachados <- renderValueBox({

    aux <- data_f() %>% summarise(SacosDespachados= sum(sacosdsp, na.rm = T)) %>% as.numeric()

    valueBox(

      comma(aux),
      "Sacos Despachados",
      color = "yellow",
      icon = icon("dolly-flatbed")

    )

  })
  output$C2_dias_promedio <- renderValueBox({

    aux <- data_f() %>%
      rename_at(vars(contains("Ciclo2", )), ~paste("Variable")) %>%
      summarise(Promedio = mean(Variable, na.rm = T)) %>%
      as.numeric()

    valueBox(

      comma(aux, accuracy = 0.01),
      "Promedio de dias",
      color = "yellow",
      icon = icon("calendar")

    )

  })
  output$C2_promedio_ponderado <- renderValueBox({

    aux <- data_f() %>%
      rename_at(vars(contains("Ciclo2", )), ~paste("Variable")) %>%
      summarise(Promedio = weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      as.numeric()

    valueBox(

      comma(aux, accuracy = 0.01),
      "Promedio de dias Ponderado por Sacos",
      color = "yellow",
      icon = icon("calendar-check")

    )

  })

  ## Fila de Dias Simples ----

  output$C2_BarrasTrilladoraDIa <- renderPlotly({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo2")), ~paste("Variable")) %>%
      group_by(lineamaritima) %>%
      summarise(Promedio = mean(Variable, na.rm = T)) %>%
      mutate(Color = ifelse(Promedio > input$C2_MetaDias, "No Cumple", "Cumple"))


    plot_ly(aux1, y=~lineamaritima, x=~Promedio, type = "bar", hoverinfo = "text", color = ~Color,
            textposition = 'auto', text = ~comma(Promedio, accuracy = 0.01),
            hovertext = paste("Naviera :", aux1$lineamaritima,
                              "<br>", "Promedio de Días: ", comma(aux1$Promedio))) %>%
      layout() %>%
      layout(shapes = list(vline(input$C2_MetaDias) ),
             title= paste("Tiempo promedio en dias", input$dias),
             yaxis= list(tickformat="", title=""),
             xaxis= list(tickformat=",", title="Promedio de Días")) %>%
      config(displayModeBar=F)

  })
  output$C2_TablaDias <- DT::renderDataTable({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo2")), ~paste("Variable")) %>%
      group_by(lineamaritima) %>%
      summarise(Promedio = mean(Variable, na.rm = T)) %>%
      mutate(Diferencia = Promedio - input$C2_MetaDias)

    datatable(aux1, options=list(pageLength =10, dom = 'tpl'), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Naviera", "Promedio en Días", "Diferencia")) %>%
      formatRound(c("Promedio", "Diferencia"), digits = 2)

  })

  ## Fila de Promedio Ponderado ----

  output$C2_BarrasPromedio <- renderPlotly({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo2")), ~paste("Variable")) %>%
      group_by(lineamaritima) %>%
      summarise(Promedio =weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      mutate(Color = ifelse(Promedio > input$C2_MetaPromedio, "No Cumple", "Cumple"))


    plot_ly(aux1, y=~lineamaritima, x=~Promedio, type = "bar", hoverinfo = "text", color = ~Color,
            textposition = 'auto', text = ~comma(Promedio, accuracy = 0.01),
            hovertext = paste("Naviera :", aux1$lineamaritima,
                              "<br>", "Promedio de Días: ", comma(aux1$Promedio))) %>%
      layout() %>%
      layout(shapes = list(vline(input$C2_MetaPromedio) ),
             title= paste("Tiempo Promedio en Días", input$dias),
             yaxis= list(tickformat="", title=""),
             xaxis= list(tickformat=",", title="Promedio de Días")) %>%
      config(displayModeBar=F)

  })
  output$C2_Tablapromedio <- DT::renderDataTable({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo2")), ~paste("Variable")) %>%
      group_by(lineamaritima) %>%
      summarise(Promedio =weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      mutate(Diferencia = Promedio - input$C2_MetaPromedio)

    datatable(aux1, options=list(pageLength =10, dom = 'tpl'), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Naviera", "Promedio en Días", "Diferencia")) %>%
      formatRound(c("Promedio", "Diferencia"), digits = 2)

  })



  ### Medicion por destino ----

  output$MapaMundial <- renderPlotly({

    l <- list(color = toRGB("grey"), width = 0.5)

    g <- list(
      showframe = FALSE,
      fitbounds = "locations",
      showcoastlines = T,
      projection = list(type = 'Mercator')
    )

    aux <- data_f() %>% ungroup() %>%
      rename_at(vars(contains("Ciclo2")), ~paste("Variable")) %>%
      group_by(paisdestino, code) %>%
      summarise(Transacciones=n(),
                Sacos=sum(sacosdsp, na.rm = T),
                Dias=mean(Variable),
                Promedio=weighted.mean(Variable, sacosdsp, na.rm = T)
                ) %>%
      ungroup()

    plot_ly(aux, z = ~Promedio, locations = ~code, type = 'choropleth', source = "Mapa",
            color = ~Promedio, colors = 'Blues', marker = list(line = l),
            colorbar = list(tickprefix = '', title = 'PP Dias'),
            hoverinfo = "text",
            hovertext = paste("Pais de Destino:", aux$paisdestino,
                              "<br>Transacciones :", comma(aux$Transacciones, accuracy = 1),
                              "<br>Sacos :", comma(aux$Sacos, accuracy = 1),
                              "<br>Dias Promedio :", comma(aux$Dias, accuracy = 0.01),
                              "<br>Dias Promedio Ponderado :", comma(aux$Promedio, accuracy = 0.01))
            ) %>%
      layout(title = 'Promedio de dias en Ciclo 2 (ponderado por sacos despachados)',
             geo = g) %>%
      config(displayModeBar=F) %>%
      event_register("plotly_click")

  })
  output$DetalleMapa <- DT::renderDataTable({

    exp <- event_data("plotly_click", source = "Mapa")$pointNumber

    aux <- data_f() %>% ungroup() %>%
      rename_at(vars(contains("Ciclo2")), ~paste("Variable")) %>%
      group_by(paisdestino, code) %>%
      summarise(Transacciones=n(),
                Sacos=sum(sacosdsp, na.rm = T),
                Dias=mean(Variable),
                Promedio=weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      ungroup() %>%
      mutate(id = seq(0,n()-1)) %>%
      filter(id == exp) %>%
      select(paisdestino) %>%
      as.character()

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo2")), ~paste("Variable")) %>%
      filter(paisdestino == aux) %>%
      group_by(paisdestino, puertodestino) %>%
      summarise(Transacciones=n(),
                Sacos=sum(sacosdsp, na.rm = T),
                Dias=mean(Variable),
                Promedio=weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      arrange(desc(Promedio))

    DT::datatable(aux1, options=list(pageLength =10, dom = 'tp', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F,
                  filter = list(position = 'top', clear = FALSE) ,
                  colnames = c("País de Destino", "Puerto de Destino" ,"Transacciones", "Sacos", "Dias (promedio simple)",
                               "Dias (promedio ponderado)")) %>%
      formatRound(c("Transacciones", "Sacos"), digits = 0) %>%
      formatRound(c("Dias", "Promedio"), digits = 2)

  })


  #### Ciclo 3 ----
  ## Value boxes ----
  output$C3_transacciones <- renderValueBox({

    valueBox(

      comma(nrow(data_f())),
      "Transacciones",
      color = "yellow",
      icon = icon("file-invoice")

    )

  })
  output$C3_SacosDespachados <- renderValueBox({

    aux <- data_f() %>% summarise(SacosDespachados= sum(sacosdsp, na.rm = T)) %>% as.numeric()

    valueBox(

      comma(aux),
      "Sacos Despachados",
      color = "yellow",
      icon = icon("dolly-flatbed")

    )

  })
  output$C3_dias_promedio <- renderValueBox({

    aux <- data_f() %>%
      rename_at(vars(contains("Ciclo3", )), ~paste("Variable")) %>%
      summarise(Promedio = mean(Variable, na.rm = T)) %>%
      as.numeric()

    valueBox(

      comma(aux, accuracy = 0.01),
      "Promedio de dias",
      color = "yellow",
      icon = icon("calendar")

    )

  })
  output$C3_promedio_ponderado <- renderValueBox({

    aux <- data_f() %>%
      rename_at(vars(contains("Ciclo3", )), ~paste("Variable")) %>%
      summarise(Promedio = weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      as.numeric()

    valueBox(

      comma(aux, accuracy = 0.01),
      "Promedio de dias Ponderado por Sacos",
      color = "yellow",
      icon = icon("calendar-check")

    )

  })

  output$C3_BarrasTiempos <- renderPlotly({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo3")), ~paste("Variable")) %>%
      mutate(RangoDias = case_when(Variable <=3 ~ "De 0 a 3 días",
                                   Variable <=9 ~ "De 4 a 9 días",
                                   Variable <=11 ~ "De 10 a 11 días",
                                   Variable >11 ~ "Más de 11 días")) %>%
      group_by(RangoDias) %>%
      summarise(Freq=n())

    aux1$RangoDias <- factor(aux1$RangoDias, levels = c("Más de 11 días", "De 10 a 11 días", "De 4 a 9 días", "De 0 a 3 días"), ordered = T)

    plot_ly(aux1, y=~RangoDias, x=~Freq, type = "bar", hoverinfo = "text", source = "Grupos",
            textposition = 'auto', text = ~comma(Freq, accuracy = 1),
            hovertext = paste("Número de Días :", aux1$RangoDias,
                              "<br>", "Número de Transacciones: ", comma(aux1$Freq))) %>%
      layout(title= paste("Duracion del ciclo"),
             yaxis= list(tickformat="", title=""),
             xaxis= list(tickformat=",", title="Número de Transacciones")) %>%
      config(displayModeBar=F) %>%
      event_register("plotly_click")
  })
  output$Mapa2 <- renderPlotly({

    l <- list(color = toRGB("grey"), width = 0.5)

    g <- list(
      showframe = FALSE,
      fitbounds = "locations",
      showcoastlines = T,
      projection = list(type = 'Mercator')
    )

    filtro <- event_data("plotly_click", source = "Grupos")$y

    aux <- data_f() %>% ungroup() %>%
      rename_at(vars(contains("Ciclo2")), ~paste("Variable")) %>%
      mutate(RangoDias = case_when(Variable <=3 ~ "De 0 a 3 días",
                                   Variable <=9 ~ "De 4 a 9 días",
                                   Variable <=11 ~ "De 10 a 11 días",
                                   Variable >11 ~ "Más de 11 días")) %>%
      filter(RangoDias == filtro) %>%
      group_by(paisdestino, code) %>%
      summarise(Transacciones=n(),
                Sacos=sum(sacosdsp, na.rm = T),
                Dias=mean(Variable),
                Promedio=weighted.mean(Variable, sacosdsp, na.rm = T)
      ) %>%
      ungroup()

    plot_ly(aux, z = ~Promedio, locations = ~code, type = 'choropleth', source = "Mapa",
            color = ~Promedio, colors = 'Blues', marker = list(line = l),
            colorbar = list(tickprefix = '', title = 'PP Dias'),
            hoverinfo = "text",
            hovertext = paste("Pais de Destino:", aux$paisdestino,
                              "<br>Transacciones :", comma(aux$Transacciones, accuracy = 1),
                              "<br>Sacos :", comma(aux$Sacos, accuracy = 1),
                              "<br>Dias Promedio :", comma(aux$Dias, accuracy = 0.01),
                              "<br>Dias Promedio Ponderado :", comma(aux$Promedio, accuracy = 0.01))
    ) %>%
      layout(title = paste("Duracion del ciclo", filtro),
             geo = g) %>%
      config(displayModeBar=F) %>%
      event_register("plotly_click")

  })

  output$C3_Detalle <- DT::renderDataTable({

    filtro <- event_data("plotly_click", source = "Grupos")$y

    aux1 <- data_f() %>% ungroup() %>%
      rename_at(vars(contains("Ciclo2")), ~paste("Variable")) %>%
      mutate(RangoDias = case_when(Variable <=3 ~ "De 0 a 3 días",
                                   Variable <=9 ~ "De 4 a 9 días",
                                   Variable <=11 ~ "De 10 a 11 días",
                                   Variable >11 ~ "Más de 11 días")) %>%
      filter(RangoDias == filtro) %>%
      group_by(cliente, paisdestino) %>%
      summarise(Transacciones=n(),
                Sacos=sum(sacosdsp, na.rm = T),
                Dias=mean(Variable),
                Promedio=weighted.mean(Variable, sacosdsp, na.rm = T)
      ) %>%
      ungroup() %>%
      arrange(desc(Promedio))

    DT::datatable(aux1, options=list(pageLength =10, dom = 'tp', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F,
                  filter = list(position = 'top', clear = FALSE) ,
                  colnames = c("Cliente","País de Destino", "Transacciones", "Sacos", "Dias (promedio simple)",
                               "Dias (promedio ponderado)")) %>%
      formatRound(c("Transacciones", "Sacos"), digits = 0) %>%
      formatRound(c("Dias", "Promedio"), digits = 2)

  })


  #### Ciclo 4 ----

  ### Value boxes ----
  output$C4_transacciones <- renderValueBox({

    valueBox(

      comma(nrow(data_f())),
      "Transacciones",
      color = "yellow",
      icon = icon("file-invoice")

    )

  })
  output$C4_SacosDespachados <- renderValueBox({

    aux <- data_f() %>% summarise(SacosDespachados= sum(sacosdsp, na.rm = T)) %>% as.numeric()

    valueBox(

      comma(aux),
      "Sacos Despachados",
      color = "yellow",
      icon = icon("dolly-flatbed")

    )

  })
  output$C4_dias_promedio <- renderValueBox({

    aux <- data_f() %>%
      rename_at(vars(contains("Ciclo4", )), ~paste("Variable")) %>%
      summarise(Promedio = mean(Variable, na.rm = T)) %>%
      as.numeric()

    valueBox(

      comma(aux, accuracy = 0.01),
      "Promedio de dias",
      color = "yellow",
      icon = icon("calendar")

    )

  })
  output$C4_promedio_ponderado <- renderValueBox({

    aux <- data_f() %>%
      rename_at(vars(contains("Ciclo4", )), ~paste("Variable")) %>%
      summarise(Promedio = weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      as.numeric()

    valueBox(

      comma(aux, accuracy = 0.01),
      "Promedio de dias Ponderado por Sacos",
      color = "yellow",
      icon = icon("calendar-check")

    )

  })

  ### Fila de Dias Simples ----
  output$C4_BarrasTrilladoraDIa <- renderPlotly({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo4")), ~paste("Variable")) %>%
      group_by(cliente) %>%
      summarise(Promedio = mean(Variable, na.rm = T)) %>%
      arrange(desc(Promedio)) %>%
      top_n(input$C4_Top, Promedio)


    plot_ly(aux1, y=~cliente, x=~Promedio, type = "bar", hoverinfo = "text",
            textposition = 'auto', text = ~comma(Promedio, accuracy = 0.01),
            hovertext = paste("Cliente :", aux1$cliente,
                              "<br>", "Promedio de Días: ", comma(aux1$Promedio))) %>%
      layout() %>%
      layout(title= paste("Tiempo promedio en dias", input$dias),
             yaxis= list(tickformat="", title=""),
             xaxis= list(tickformat=",", title="Promedio de Días")) %>%
      config(displayModeBar=F)

  })
  output$C4_TablaDias <- DT::renderDataTable({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo4")), ~paste("Variable")) %>%
      group_by(cliente, paisdestino) %>%
      summarise(Promedio = mean(Variable, na.rm = T)) %>%
      arrange(desc(Promedio))

    datatable(aux1, options=list(pageLength = input$C4_Top, dom = 'tpl'), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Cliente","Destino", "Promedio en Días")) %>%
      formatRound(c("Promedio"), digits = 2)

  })

  ### Fila de Promedio Ponderado ----
  output$C4_BarrasTrilladoraPromedio <- renderPlotly({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo4")), ~paste("Variable")) %>%
      group_by(cliente) %>%
      summarise(Promedio =weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      arrange(desc(Promedio)) %>%
      top_n(input$C4_Top, Promedio)


    plot_ly(aux1, y=~cliente, x=~Promedio, type = "bar", hoverinfo = "text",
            textposition = 'auto', text = ~comma(Promedio, accuracy = 0.01),
            hovertext = paste("Cliente :", aux1$cliente,
                              "<br>", "Promedio de Días: ", comma(aux1$Promedio))) %>%
      layout() %>%
      layout(title= paste("Tiempo promedio en dias", input$dias),
             yaxis= list(tickformat="", title=""),
             xaxis= list(tickformat=",", title="Promedio de Días")) %>%
      config(displayModeBar=F)

  })
  output$C4_Tablapromedio <- DT::renderDataTable({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo4")), ~paste("Variable")) %>%
      group_by(cliente, paisdestino) %>%
      summarise(Promedio =weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      arrange(desc(Promedio))

    datatable(aux1, options=list(pageLength = input$C4_Top, dom = 'tpl'), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Cliente","Destino", "Promedio en Días")) %>%
      formatRound(c("Promedio"), digits = 2)

  })

  #### Ciclo 5 ----

  ### Value boxes ----
  output$C5_transacciones <- renderValueBox({

    valueBox(

      comma(nrow(data_f())),
      "Transacciones",
      color = "yellow",
      icon = icon("file-invoice")

    )

  })
  output$C5_SacosDespachados <- renderValueBox({

    aux <- data_f() %>% summarise(SacosDespachados= sum(sacosdsp, na.rm = T)) %>% as.numeric()

    valueBox(

      comma(aux),
      "Sacos Despachados",
      color = "yellow",
      icon = icon("dolly-flatbed")

    )

  })
  output$C5_dias_promedio <- renderValueBox({

    aux <- data_f() %>%
      rename_at(vars(contains("Ciclo5", )), ~paste("Variable")) %>%
      summarise(Promedio = mean(Variable, na.rm = T)) %>%
      as.numeric()

    valueBox(

      comma(aux, accuracy = 0.01),
      "Promedio de dias",
      color = "yellow",
      icon = icon("calendar")

    )

  })
  output$C5_promedio_ponderado <- renderValueBox({

    aux <- data_f() %>%
      rename_at(vars(contains("Ciclo5", )), ~paste("Variable")) %>%
      summarise(Promedio = weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      as.numeric()

    valueBox(

      comma(aux, accuracy = 0.01),
      "Promedio de dias Ponderado por Sacos",
      color = "yellow",
      icon = icon("calendar-check")

    )

  })

  ### Fila de Dias Simples ----
  output$C5_BarrasTrilladoraDIa <- renderPlotly({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo5")), ~paste("Variable")) %>%
      group_by(cliente) %>%
      summarise(Promedio = mean(Variable, na.rm = T)) %>%
      arrange(desc(Promedio)) %>%
      top_n(input$C5_Top, Promedio)


    plot_ly(aux1, y=~cliente, x=~Promedio, type = "bar", hoverinfo = "text",
            textposition = 'auto', text = ~comma(Promedio, accuracy = 0.01),
            hovertext = paste("Cliente :", aux1$cliente,
                              "<br>", "Promedio de Días: ", comma(aux1$Promedio))) %>%
      layout() %>%
      layout(title= paste("Tiempo promedio en dias", input$dias),
             yaxis= list(tickformat="", title=""),
             xaxis= list(tickformat=",", title="Promedio de Días")) %>%
      config(displayModeBar=F)

  })
  output$C5_TablaDias <- DT::renderDataTable({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo5")), ~paste("Variable")) %>%
      group_by(cliente, paisdestino) %>%
      summarise(Promedio = mean(Variable, na.rm = T)) %>%
      arrange(desc(Promedio))

    datatable(aux1, options=list(pageLength = input$C5_Top, dom = 'tpl'), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Cliente","Destino", "Promedio en Días")) %>%
      formatRound(c("Promedio"), digits = 2)

  })

  ### Fila de Promedio Ponderado ----
  output$C5_BarrasTrilladoraPromedio <- renderPlotly({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo5")), ~paste("Variable")) %>%
      group_by(cliente) %>%
      summarise(Promedio =weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      arrange(desc(Promedio)) %>%
      top_n(input$C5_Top, Promedio)


    plot_ly(aux1, y=~cliente, x=~Promedio, type = "bar", hoverinfo = "text",
            textposition = 'auto', text = ~comma(Promedio, accuracy = 0.01),
            hovertext = paste("Cliente :", aux1$cliente,
                              "<br>", "Promedio de Días: ", comma(aux1$Promedio))) %>%
      layout() %>%
      layout(title= paste("Tiempo promedio en dias", input$dias),
             yaxis= list(tickformat="", title=""),
             xaxis= list(tickformat=",", title="Promedio de Días")) %>%
      config(displayModeBar=F)

  })
  output$C5_Tablapromedio <- DT::renderDataTable({

    aux1 <- data_f() %>%
      rename_at(vars(contains("Ciclo5")), ~paste("Variable")) %>%
      group_by(cliente, paisdestino) %>%
      summarise(Promedio =weighted.mean(Variable, sacosdsp, na.rm = T)) %>%
      arrange(desc(Promedio))

    datatable(aux1, options=list(pageLength = input$C5_Top, dom = 'tpl'), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Cliente","Destino", "Promedio en Días")) %>%
      formatRound(c("Promedio"), digits = 2)

  })


}
function(input, output, session) {
  
  ## Manual ----
  
  
  output$Variables <- DT::renderDataTable({
    
    datatable(vars, options=list(pageLength =80, dom = 't', scrollY = "400px"), rownames=F,
              filter = list(position = 'top', clear = FALSE))
    
  })
  
  ## Cargue de Datos ----
  data1 <- reactive({
    req(input$sicex)
    
    noms <- c('Ano', 'NumConocimiento', 'TipoConocimiento1', 'EstadoConocimiento', 'Fecha', 'Periodo', 'NITEmpColombiana', 'DigitoEmpColombiana', 
              'NombreEmpColombiana', 'CiudadEmpColombiana', 'DepartamentoEmpColombiana', 'TelefonoEmpColombiana', 'RepresentanteEmpColombiana', 
              'DireccionEmpColombiana', 'FaxEmpColombiana', 'EmailEmpColombiana', 'EmpresaExtranjera', 'Naviera', 'AgenteNaviero', 'Motonave', 
              'Consolidador', 'ConsolidadorCargaNieto', 'TipoConsolidacion', 'OperadorTransitoMultimodal', 'PuertoColombiano', 'Muelle', 
              'PaisExtranjero', 'PuertoExtranjero', 'PuertoTransbordo', 'CiudadEmpresaNotificada', 'DepartamentoEmpresaNotificada', 
              'DescripcionBL', 'CodSubPartida', 'SubPartida', 'CodCapitulo', 'Capitulo', 'Cantidad', 'UnidadComercial', 'NumeroEstibas', 
              'TipoEstiba', 'Kilos', 'Sacos70', 'ContenedoresTEUS20', 'ContenedoresFEUS40', 'TotalTEUS', 'TotalFEUS', 'ContendoresTEUS20FCL', 
              'ContendoresTEUS40FCL', 'TotalTEUSFCL', 'ContendoresTEUS20LCL', 'ContendoresTEUS40LCL', 'TotalTEUSLCL', 'EmpresaNotificada', 
              'NumConocimientoNieto', 'TipoTransito', 'CiudadDestino', 'PaisDestino', 'EmpresaSIA', 'DepositoAduanero', 'ZonaFranca', 
              'FormaPago', 'TipoConocimiento2', 'NumSobordo', 'ConsecutivoRegistro', 'RevistaDiaria', 'RevistaSemanal', 'CIIU', 'COUODE', 
              'RORO', 'Fondeo', 'MotonaveViaje', 'Cubicaje')
    
    inFile <- input$sicex
    
    data <- read_excel(inFile$datapath, col_names = noms, skip = 1) %>% 
      mutate(Sacos70=Kilos/70,
             Fecha=as.Date(Fecha),
             Importadora= case_when(
               grepl(pattern='STARBUCKS', EmpresaExtranjera) ~ 'STARBUCKS COFFEE TRADING COMPANY',
               grepl(pattern='OLAM', EmpresaExtranjera) ~ 'OLAM INC',
               grepl(pattern='ROTHFOS', EmpresaExtranjera) ~ 'ROTHFOS',
               grepl(pattern='RGC COFFEE', EmpresaExtranjera) ~ 'RGC COFFEE',
               grepl(pattern='FOLGER', EmpresaExtranjera) ~ 'THE FOLGER COFFEE COMPANY',
               grepl(pattern='MCT USA INC', EmpresaExtranjera) ~ 'MCT USA INC',
               grepl(pattern='MITSUI', EmpresaExtranjera) ~ 'MITSUI Y CO LTD',
               grepl(pattern='MITSU Y CO LTD', EmpresaExtranjera) ~ 'MITSUI Y CO LTD',
               grepl(pattern='KONINKLIJKE', EmpresaExtranjera) ~ 'KONINKLIJKE DOUWE EGBERTS BV',
               grepl(pattern='VOLCAFE', EmpresaExtranjera) ~ 'VOLCAFE LTDA',
               grepl(pattern='LUIGI LAVAZZA SPA', EmpresaExtranjera) ~ 'LUIGI LAVAZZA SPA',
               grepl(pattern='SUCAFINA', EmpresaExtranjera) ~ 'SUCAFINA SA',
               grepl(pattern='NESTLE', EmpresaExtranjera) ~ 'NESTLE NESPRESSO SA',
               grepl(pattern='AMERICAN COFFEE CORP', EmpresaExtranjera) ~ 'AMERICAN COFFEE CORP',
               grepl(pattern='DONGSUH FOOD CO', EmpresaExtranjera) ~ 'DONGSUH FOOD CO',
               grepl(pattern='COFFEE SOURCE', EmpresaExtranjera) ~ 'THE COFFEE SOURCE INC',
               grepl(pattern='TCHIBO', EmpresaExtranjera) ~ 'TCHIBO GMBH',
               grepl(pattern='MARUBENI', EmpresaExtranjera) ~ 'MARUBENI CORP',
               grepl(pattern='COEX', EmpresaExtranjera) ~ 'COEX COFFEE INTL',
               grepl(pattern='MCC LOGISTICS KOREA LTD', EmpresaExtranjera) ~ 'MCC LOGISTICS KOREA LTD',
               grepl(pattern='HARRISON COFFEE', EmpresaExtranjera) ~ 'HARRISON COFFEE',
               grepl(pattern='SHAREKAT OLD CITY', EmpresaExtranjera) ~ 'SHAREKAT OLD CITY',
               grepl(pattern='SOBHY', EmpresaExtranjera) ~ 'SOBHY NACHLY AND SONS',
               grepl(pattern='ITOCHU', EmpresaExtranjera) ~ 'ITOCHU CORP',
               T ~ EmpresaExtranjera)
             )
    
    return(data)
    
  })
  
  # Conteos de subida
  output$vb_registros <- renderInfoBox({
    
    infoBox(title = "Registros",
            subtitle = "Registros Cargados",
            value = comma(nrow(data1())),
            color = "red",
            icon = icon("bars")
    )
  })
  output$vb_fecha <- renderInfoBox({
    
    val = paste("De", format(min(data1()$Fecha), "%B"), "a", format(max(data1()$Fecha), "%B"))
    
    infoBox(title = "Fecha",
            subtitle = "Exportaciones del Periodo",
            value = val,
            color = "red",
            icon = icon("history")
    )
  })
  output$vb_sacos70 <- renderInfoBox({
    
    val <- sum(data1()$Sacos70)
    
    infoBox(title = "Sacos Exportados",
            subtitle = "Total de Sacos Exportados (70kg)",
            value = comma(val),
            color = "red",
            icon = icon("ship")
    )
  })
  
  # Modificaion de Cadenas
  
  output$Cambios <- DT::renderDataTable({
    
    aux <- data1() %>%
      select(EmpresaExtranjera, Importadora) %>% 
      filter(Importadora != EmpresaExtranjera) %>%
      distinct()
    
    datatable(aux, options=list(pageLength =20, dom = 'tlsp', searching= T, scrollX = TRUE, scrollY = "600px"), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Nombre Original", "Nombre Modificado"))
  })
  

  
  ## Actualizacion de Filtros y Filtro de Base -----
  
  observe({
    updateDateRangeInput(session, "Fecha",
                         start = min(data1()$Fecha, na.rm = T),
                         end = max(data1()$Fecha, na.rm = T))
    })
  
  data_f <- reactive({
    
    data1() %>% 
      filter(Fecha >= input$Fecha[1] & Fecha <=input$Fecha[2])
    
  })
    
    
  ## Analisis Descriptivo -----
  ### Serie de Tiempo ----
  
  output$Serie1 <- renderDygraph({
    
    aux <- data_f() %>% 
      group_by(Fecha) %>% 
      summarise(Sacos70=round(sum(Sacos70)),
                Kilos=round(sum(Kilos))) %>% 
      mutate_("Var"=input$Medida1) %>% 
      select(Fecha, Var)
    
    FUNC_JSFormatNumber <- "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"
    
    dygraph(apply.monthly(as.xts(aux$Var, order.by = aux$Fecha), FUN=sum), main = paste("Exportación medida en", gsub("\\d", "", input$Medida1))) %>% 
      dyAxis("y", label = "", axisLabelFormatter=JS(FUNC_JSFormatNumber), valueFormatter=JS(FUNC_JSFormatNumber) )%>% 
      dyRangeSelector() %>% 
      dyOptions(includeZero = T)
  })
  output$BarrasSerie <- renderPlotly({
    
    aux <- data_f() %>% 
      dplyr::group_by(Periodo) %>% 
      summarise(Sacos70=round(sum(Sacos70)),
                Kilos=round(sum(Kilos))) %>% 
      mutate_("Var"=input$Medida1) %>% 
      select(Periodo, Var)
    
    plot_ly(aux, y=~Var, x=~Periodo, type = "bar", hoverinfo = "text", textposition = 'auto', text = ~comma(Var),
            hovertext = paste("Periodo:", aux$Periodo,
                              "<br>", gsub("\\d", "", input$Medida1), " :", comma(aux$Var))
    ) %>% 
      layout(title= paste("Exportación medida en", gsub("\\d", "", input$Medida1)), 
             yaxis= list(tickformat=",", title="")) %>% 
      config(displayModeBar=F)
    
  })
  
  ### Pestaña Exportadores ----
  
  output$Exportador1 <- renderPlotly({
    
    aux <- data_f() %>% 
      dplyr::group_by(NombreEmpColombiana) %>% 
      summarise(Sacos70=round(sum(Sacos70))) %>% 
      top_n(input$top1, wt = Sacos70) %>% 
      mutate(Color=ifelse(NombreEmpColombiana == "RACAFE & CIA S C A", "Racafé", "Otros"))
    
    aux$NombreEmpColombiana <- factor(aux$NombreEmpColombiana, levels = unique(aux$NombreEmpColombiana)[order(aux$Sacos70, decreasing = F)])
    
    p<- plot_ly(aux, y=~NombreEmpColombiana, x=~Sacos70, type = "bar", hoverinfo = "text", color = ~Color, source = "Exportador1",
            colors = c("gray", "firebrick"), textposition = 'auto', text = ~comma(Sacos70), 
            hovertext = paste("Exportadora:", aux$NombreEmpColombiana,
                              "<br> Sacos 70kg :", comma(aux$Sacos70))) %>% 
      layout(title = paste("Exportación en Sacos de 70kg"), 
             xaxis = list(tickformat=",", title=""),
             yaxis = list(title=""),
             legend = list(orientation = "h",   xanchor = "center",  x = 0.5)) %>% 
      config(displayModeBar=F) %>% 
      event_register("plotly_click");p

  })
  output$Exportador2 <- DT::renderDataTable({

    exp <- event_data("plotly_click", source = "Exportador1")$y
    
    aux <- data_f() %>% 
      filter(NombreEmpColombiana == exp) %>% 
      mutate(Total = sum(Sacos70)) %>% 
      dplyr::group_by(Importadora) %>% 
      summarise(Sacos70=round(sum(Sacos70)),
                Pct=round(sum(Sacos70)/max(Total),2)) %>% 
      arrange(desc(Sacos70))
    
    datatable(aux, options=list(pageLength =10, dom = 'tsp', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Importadora", "Sacos 70kg", "Porcentaje")) %>% 
      formatRound("Sacos70", digits = 0) %>% 
      formatPercentage("Pct", digits = 2)

  })
  
  ### Pestaña Importadores ----
  
  output$Importador1 <- renderPlotly({

    aux <- data_f()  %>%
      dplyr::group_by(Importadora) %>%
      summarise(Sacos70=round(sum(Sacos70))) %>%
      top_n(input$top2, wt = Sacos70)

    aux$Importadora <- factor(aux$Importadora, levels = unique(aux$Importadora)[order(aux$Sacos70, decreasing = F)])

    p<- plot_ly(aux, y=~Importadora, x=~Sacos70, type = "bar", hoverinfo = "text", source = "Importador1",
                textposition = 'auto', text = ~comma(Sacos70),
                hovertext = paste("Importadora:", aux$Importadora,
                                  "<br> Sacos 70kg :", comma(aux$Sacos70))) %>%
      layout(title = paste("Exportación en Sacos de 70kg"),
             xaxis = list(tickformat=",", title=""),
             yaxis = list(title=""),
             legend = list(orientation = "h",   xanchor = "center",  x = 0.5)) %>%
      config(displayModeBar=F) %>%
      event_register("plotly_click");p

  })
  
  output$Importador2 <- DT::renderDataTable({

    exp <- event_data("plotly_click", source = "Importador1")$y

    aux <- data_f() %>%
      filter(Importadora == exp) %>%
      mutate(Total = sum(Sacos70)) %>%
      dplyr::group_by(NombreEmpColombiana) %>%
      summarise(Sacos70=round(sum(Sacos70)),
                Pct=round(sum(Sacos70)/max(Total),2)) %>%
      arrange(desc(Sacos70))

    datatable(aux, options=list(pageLength =10, dom = 'tsp', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Exportadora", "Sacos 70kg", "Porcentaje")) %>%
      formatRound("Sacos70", digits = 0) %>%
      formatPercentage("Pct", digits = 2)

  })

  ### Diagrama de Sankey Exportador-Importador ----
  
  output$Sankey1 <- renderPlot({
    
    aux1 <- data_f() %>% 
      group_by(NombreEmpColombiana) %>% 
      summarise(Sacos70=round(sum(Sacos70))) %>% 
      arrange(desc(Sacos70)) %>% 
      top_n(10) %>% 
      mutate(Cons=row_number(),
             Origen=NombreEmpColombiana) %>% 
      select(-Sacos70)
    
    aux2 <- data_f() %>% 
      group_by(Importadora) %>% 
      summarise(Sacos70=round(sum(Sacos70))) %>% 
      arrange(desc(Sacos70)) %>% 
      top_n(15) %>% 
      mutate(Cons2=row_number()+11,
             Destino=Importadora) %>% 
      select(-Sacos70)
    
    test1 <- data_f() %>% 
      select(NombreEmpColombiana, Importadora, Sacos70) %>% 
      left_join(aux1, by = c("NombreEmpColombiana")) %>% 
      left_join(aux2, by = c("Importadora")) %>% 
      mutate(
        Origen = ifelse(is.na(Origen), "OTROS EXPORTADORES", Origen),
        Cons = ifelse(is.na(Cons), 11, Cons),
        Destino = ifelse(is.na(Destino), "OTROS IMPORTADORES", Destino),
        Cons2 = ifelse(is.na(Cons2), 22, Cons2)
      ) %>% 
      group_by(Cons, Origen, Cons2 , Destino) %>% 
      summarise(Sacos70=round(sum(Sacos70))) %>% 
      ungroup()
    
    test2 <- test1 %>% select(-c(Cons, Cons2)) %>% 
      mutate(Color=ifelse(Origen == "RACAFE & CIA S C A", "Racafé", "Otros"))
    
    ggplot(data = test2, aes(axis1 = Origen, axis2 = Destino, y = Sacos70)) +
      scale_x_discrete(limits = c("Origen", "Destino"), expand = c(.2, .05)) +
      xlab("") +
      geom_alluvium(aes(fill = Color)) +
      geom_stratum() +
      geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
      theme_minimal() +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      labs(fill = "") +
      scale_fill_manual(values=c("gray", "firebrick")) +
      ggtitle("Distribución de Exportaciones (Total Sacos de 70kg)",
              "Top 10 de Exportadores vs Top 15 de Importadores")
    
    
  })
  
  ### Pestaña Paises ----
  
  output$Pais1 <- renderPlotly({
    
    aux <- data_f()  %>%
      dplyr::group_by(PaisExtranjero) %>%
      summarise(Sacos70=round(sum(Sacos70))) %>%
      top_n(input$top3, wt = Sacos70)
    
    aux$PaisExtranjero <- factor(aux$PaisExtranjero, levels = unique(aux$PaisExtranjero)[order(aux$Sacos70, decreasing = F)])
    
    p<- plot_ly(aux, y=~PaisExtranjero, x=~Sacos70, type = "bar", hoverinfo = "text", source = "Pais1",
                textposition = 'auto', text = ~comma(Sacos70),
                hovertext = paste("País de Destino:", aux$PaisExtranjero,
                                  "<br> Sacos 70kg :", comma(aux$Sacos70))) %>%
      layout(title = paste("Exportación en Sacos de 70kg"),
             xaxis = list(tickformat=",", title=""),
             yaxis = list(title=""),
             legend = list(orientation = "h",   xanchor = "center",  x = 0.5)) %>%
      config(displayModeBar=F) %>%
      event_register("plotly_click");p
    
  })
  output$Pais2 <- DT::renderDataTable({
    
    exp <- event_data("plotly_click", source = "Pais1")$y
    
    aux <- data_f() %>%
      filter(PaisExtranjero == exp) %>%
      mutate(Total = sum(Sacos70)) %>%
      dplyr::group_by(NombreEmpColombiana) %>%
      summarise(Sacos70=round(sum(Sacos70)),
                Pct=round(sum(Sacos70)/max(Total),2)) %>%
      arrange(desc(Sacos70))
    
    datatable(aux, options=list(pageLength =10, dom = 'tsp', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F,
              filter = list(position = 'top', clear = FALSE) ,
              colnames = c("Exportadora", "Sacos 70kg", "Porcentaje")) %>%
      formatRound("Sacos70", digits = 0) %>%
      formatPercentage("Pct", digits = 2)
    
  })
  
  ### Exportacion por Pais -----
  
  output$Sankey2 <- renderPlot({
    
    aux1 <- data_f() %>% 
      group_by(NombreEmpColombiana) %>% 
      summarise(Sacos70=round(sum(Sacos70))) %>% 
      arrange(desc(Sacos70)) %>% 
      top_n(10) %>% 
      mutate(Cons=row_number(),
             Origen=NombreEmpColombiana) %>% 
      select(-Sacos70)
    
    aux2 <- data_f() %>% 
      group_by(PaisExtranjero) %>% 
      summarise(Sacos70=round(sum(Sacos70))) %>% 
      arrange(desc(Sacos70)) %>% 
      top_n(10) %>% 
      mutate(Cons2=row_number()+11,
             Destino=PaisExtranjero) %>% 
      select(-Sacos70)
    
    test1 <- data_f() %>% 
      select(NombreEmpColombiana, PaisExtranjero, Sacos70) %>% 
      left_join(aux1, by = c("NombreEmpColombiana")) %>% 
      left_join(aux2, by = c("PaisExtranjero")) %>% 
      mutate(
        Origen = ifelse(is.na(Origen), "OTROS EXPORTADORES", Origen),
        Cons = ifelse(is.na(Cons), 11, Cons),
        Destino = ifelse(is.na(Destino), "OTROS PAISES", Destino),
        Cons2 = ifelse(is.na(Cons2), 22, Cons2)
      ) %>% 
      group_by(Cons, Origen, Cons2 , Destino) %>% 
      summarise(Sacos70=round(sum(Sacos70))) %>% 
      ungroup()
    
    test2 <- test1 %>% select(-c(Cons, Cons2)) %>% 
      mutate(Color=ifelse(Origen == "RACAFE & CIA S C A", "Racafé", "Otros"))
    
    ggplot(data = test2, aes(axis1 = Origen, axis2 = Destino, y = Sacos70)) +
      scale_x_discrete(limits = c("Origen", "Destino"), expand = c(.2, .05)) +
      xlab("") +
      geom_alluvium(aes(fill = Color)) +
      geom_stratum() +
      geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
      theme_minimal() +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      labs(fill = "") +
      scale_fill_manual(values=c("gray", "firebrick")) +
      ggtitle("Distribución de Exportaciones (Total Sacos de 70kg)",
              "Top 10 de Exportadores y de Países de Destino")
    
    
  })
}
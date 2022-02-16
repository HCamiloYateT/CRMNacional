function(input, output, session) { 
  
  # Datos Reactivos ----
  
  fecha_focal <- reactive({
    max(input$Fecha)
  })
  data_trilladora <- reactive({
    
    data %>% 
      filter(Sucursal %in% input$Sucursal)
    
  })
  data_f <- reactive({
    
    if(length(input$Fecha) == 1) {
      data %>% 
        filter(Sucursal == input$Sucursal,
               floor_date(Fecha, unit = "month") == floor_date(input$Fecha, unit = "month")
        )
    } else {
      data %>% 
        filter(Sucursal == input$Sucursal,
               dplyr::between(floor_date(Fecha, unit = "month"), floor_date(input$Fecha[1], unit = "month"), 
                              floor_date(input$Fecha[2], unit = "month")))
    }
  })
  data_per_ant_f <- reactive({
    
    if(length(input$Fecha) == 1) {
      data %>% 
        filter(Sucursal == input$Sucursal,
               floor_date(Fecha, unit = "month") == floor_date(input$Fecha, unit = "month") - months(1)
        )
    } else {
      
      n_mes = interval(floor_date(input$Fecha[1], unit = "month"), floor_date(input$Fecha[2], unit = "month")) %/% months(1) + 1
      
      data %>% 
        filter(Sucursal == input$Sucursal,
               dplyr::between(floor_date(Fecha, unit = "month"), floor_date(input$Fecha[1], unit = "month") - months(n_mes), 
                              floor_date(input$Fecha[2], unit = "month") - months(n_mes) ))
    }
  })
  data_an_ant_f <- reactive({
    
    if(length(input$Fecha) == 1) {
      data %>% 
        filter(Sucursal == input$Sucursal,
               floor_date(Fecha, unit = "month") == floor_date(input$Fecha, unit = "month") - years(1)
        )
    } else {
      data %>% 
        filter(Sucursal == input$Sucursal,
               dplyr::between(floor_date(Fecha, unit = "month"), floor_date(input$Fecha[1], unit = "month") - years(1), 
                              floor_date(input$Fecha[2], unit = "month") - years(1) ))
    }
  })
  
  
  # Indicadores de Gestión -----
  
  output$MesAnalisis <- renderUI({
    if(length(input$Fecha) == 1) h4(paste("Indicadores de gestion de la trilladora", input$Sucursal, "en", format(input$Fecha, "%B %Y")))
    else h4(paste("Indicadores de gestion de la trilladora", input$Sucursal, "entre", 
                  format(input$Fecha[1], "%B %Y"), "y", format(input$Fecha[2], "%B %Y")))
    })
  
  ## Costo Estándar -----
  
  output$BarrasCostoEstandar <- renderPlotly({
    
    aux1 <- data_f() %>%
      filter(Seccion == "Costo Estandar") %>% 
      select(Item, Valor) %>% 
      distinct()
    
    aux1$Item <- factor(aux1$Item, levels = rev(c('Estándar','Supremos y Dunkin Donuts','Convencionales Trazables',
                                                  'Diferenciados','Micro lotes','A la medida','Consumos y Pasillas Sacos de 70kg')), 
                        ordered = T)
    
    plot_ly(data = aux1, x=~Valor, y=~Item, type = "bar", text = ~Item,
            marker = list(color = "#707B7C",
                          line = list(color = "black", width = 1.5)),
            hoverinfo = "text", hoverlabel = list(align = "left"),
            hovertext = paste0("<b>", aux1$Item, 
                               "<br><b>Costo Estándar: </b>", dollar(aux1$Valor))) %>% 
      layout(title = list(text=paste(""), 
                          font=list(family = "Arial, sans-serif",size = 16,color = "black")),
             xaxis = list(tickformat = ",", fixedrange = TRUE, 
                          title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "black")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
             ),
             yaxis = list(tickformat = ",", visible=F, fixedrange = TRUE,
                          title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "black")), 
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.07, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "black"))) %>% 
      config(displayModeBar=F)
    
  })
  output$TablaCostoEstandar <- renderTable({
    aux1 <- data_f() %>%
      filter(Seccion == "Costo Estandar") %>% 
      select(Item, Valor) %>% 
      distinct() %>% 
      mutate(Valor = dollar(Valor, accuracy = 1)) %>% 
      rename(`Costo Estandar` = Valor)
    
  }, spacing = 'xs', width = "100%", height = "100%", digits = 0)
  
  ## Compras ----
  
  output$ResumenCompras <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "Compras realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "Compras realizadas") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "Compras realizadas") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(comma(aux1, accuracy = 1), "Kg")),
      title = "Compras:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", comma(aux2), "Kg", br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", comma(aux3), "Kg", br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1)
                            )),
      icon = icon("cash-register"),
      fill = T,
      color = "teal"
    )
  }) 
  output$DetallesCompras <- renderUI({
    
    mes <- data_f() %>% filter(Item == "Compras realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "Compras realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "Compras realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',comma(mes_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm) , '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',comma(an_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam) , '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$GaugeCumpCompras <- renderPlotly({
    
    pres <- data_f() %>% filter(Item == "Presupuesto de Compras") %>% summarise(sum(Valor)) %>% as.numeric()
    mes <- data_f() %>% filter(Item == "Compras realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    cump <- mes/pres
    
    meta = 1 * 1
    
    lim1 = 0.8 * 1
    lim2 = 1 * 1
    lim3 = 1.7 * 1
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = cump *1,
      title = list(text = ""),
      type = "indicator",
      mode = "gauge+number",
      number = list(valueformat = ",.2%"),
      gauge = list(
        axis =list(range = list(NULL, lim3), tickformat = ",.0%"),
        bar = list(color = "black"),
        steps = list(
          list(range = c(lim2, lim3), color = "#45B39D"),
          list(range = c(lim1, lim2), color = "#EB984E "),
          list(range = c(0, lim1), color = "#CD6155")
        ))) %>% 
      layout(autosize = T, margin = m, 
             xaxis = list(tickformat = "%", title=""),
             yaxis = list(tickformat = "%", title="")) %>% 
      config(displayModeBar=F)
    
    
    
  })
  output$DetallGaugeCompras <- renderUI({ 
    
    pres <- data_f() %>% filter(Item == "Presupuesto de Compras") %>% summarise(sum(Valor)) %>% as.numeric()
    mes  <- data_f() %>% filter(Item == "Compras realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    cump <- mes/pres
    
    pre_mes <- data_per_ant_f() %>% filter(Item == "Presupuesto de Compras") %>% summarise(sum(Valor)) %>% as.numeric()
    mes_ant <- data_per_ant_f() %>% filter(Item == "Compras realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    cum_mes <- mes_ant/pre_mes
    tmm <- 1- (cum_mes/cump)
    
    pre_an <- data_an_ant_f() %>% filter(Item == "Presupuesto de Compras") %>% summarise(sum(Valor)) %>% as.numeric()
    an_ant <- data_an_ant_f() %>% filter(Item == "Compras realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    cum_an <- an_ant/pre_an
    tam <- 1- (cum_an/cump)
    
    per <- ifelse(length(input$Fecha) == 1, format(max(data_f()$Fecha), "%b%y"),
                  paste(format(min(data_f()$Fecha), "%b%y"), "a", format(max(data_f()$Fecha), "%b%Y")))
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<strong><span style="font-size: 16px;">Presupuesto : ',comma(pres, accuracy = 1),' kg </span></strong>',
                  '<br><strong><span style="font-size: 15px;"> ',per_mes,': </span></strong>',percent(cum_mes, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 15px;"> ',per_an,': </span></strong>',percent(cum_an, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$AcumCompras <- renderPlotly({
    
    item = "Compras realizadas"
    meta = 12000000
    
    aux1 <- data_trilladora() %>% 
      filter(year(Fecha) >= anho_ant, Item == item) %>% 
      mutate(Mes = month(Fecha)) %>% 
      group_by(Periodo, Mes) %>% 
      summarise(Valor= sum(Valor)) %>% 
      group_by(Periodo) %>% 
      mutate(Acum = ifelse(Valor ==0, 0, cumsum(Valor)),
             Cump = Acum/meta)
    
    aux2 <- aux1 %>% 
      filter(Periodo == anho , Valor != 0)
    
    max_mes <- max(aux2$Mes)
    
    if (nrow(aux2) == 1){
      pron <- data.frame(Mes=1:12,
                         Pron = 1:12 * aux2$Valor)
    } else{
      pron <- data.frame(Mes=1:12)
      pron$Pron <- predict(lm(data = aux2, Acum ~ Mes), pron)
    }
    
    pron$Cump = pron$Pron/meta
    pron <- pron %>% filter(Mes > max_mes)
    
    plot_ly(data = aux1, x = ~Mes, y = ~Acum, type = 'bar', color = ~factor(Periodo), colors = c("2021"="#2E4053", "2022"="#2980B9"),
            hoverinfo = "text", hovertext = paste0("<b>", aux1$Periodo*100+aux1$Mes, "</b>",
                                                   "<br>Despachos :", comma(aux1$Acum, accuracy = 1),
                                                   "<br>Cumplimiento :", percent(aux1$Cump, accuracy = 0.1))) %>% 
      add_bars(data=pron, x= ~Mes, y = ~Pron, inherit = F, name="Estimación 2022",
               marker = list(color = "#707B7C", line = list(color = "black", width = 1)),
               hoverinfo = "text", hovertext = paste0("<b>", 2021*100+pron$Mes, "</b>",
                                                      "<br>Despachos :", comma(pron$Pron, accuracy = 1),
                                                      "<br>Cumplimiento :", percent(pron$Cump, accuracy = 0.1))) %>% 
      layout(shapes = hline(meta),
             hoverlabel = list(align ="left"),
             title = paste(""),
             xaxis = list(title="Mes",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             yaxis = list(title="Compras (Kg)" , tickformat = "s",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>% 
      config(displayModeBar=F)
    
  })
  output$SerieCompras <- renderPlotly({
    
    var_plot = "Compras realizadas"
    
    aux1 <- data_trilladora() %>%  filter(Item ==	var_plot, Fecha <= fecha_focal())
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    aux4 <- data_trilladora() %>%  
      filter(Item %in%	c("Presupuesto de Compras", "Compras realizadas"), 
             Fecha <= fecha_focal()) %>% 
      select(Item, Fecha, Valor) %>% 
      pivot_wider(names_from = Item , values_from = Valor) %>% 
      rename(Compras = `Compras realizadas`, Presupuesto = `Presupuesto de Compras`) %>% 
      mutate(Cumplimiento = Compras/Presupuesto)
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Compras",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(var_plot))," :", comma(aux2$Valor, accuracy = 1))
    ) %>% 
      add_trace(data = aux4, x=~Fecha, y=~Presupuesto, type = "scatter", mode="lines+markers", 
                line = list(width = 2, color = "#196F3D "), marker = list(size = 5, color = "#196F3D "), name = "Presupuesto",
                hoverlabel = list(align = "left"), hoverinfo = "text", inherit = F,
                hovertext = paste0("<b>", str_to_title(format(aux4$Fecha, "%B %Y")), "</b>",
                                   "<br>", "Presupuesto: ", comma(aux4$Presupuesto, accuracy = 1),
                                   "<br>", "Cumplimiento: ", percent(aux4$Cumplimiento, accuracy = 0.1))
      ) %>%  
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", comma(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", comma(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Kilos (Forecast): ", comma(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "s", 
                          title= list(text= "Compras (Kg)", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  
  ## Entradas ----
  
  output$ResumenEntradas <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "Entradas  realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "Entradas  realizadas") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "Entradas  realizadas") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(comma(aux1, accuracy = 1), "Kg")),
      title = "Entradas:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", comma(aux2), "Kg", br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", comma(aux3), "Kg", br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("truck"),
      fill = T,
      color = "teal"
    )
  }) 
  output$DetallesEntradas <- renderUI({
    
    mes <- data_f() %>% filter(Item == "Entradas  realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "Entradas  realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <-(mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "Entradas  realizadas") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1

    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',comma(mes_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',comma(an_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$SerieEntradas <- renderPlotly({
    
    var_plot = "Entradas  realizadas"
    
    aux1 <- data_trilladora() %>%  filter(Item ==	var_plot, Fecha <= fecha_focal())
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Observado",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(var_plot))," :", comma(aux2$Valor, accuracy = 1))
    ) %>% 
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", comma(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", comma(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Kilos (Forecast): ", comma(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "s", 
                          title=list(text="Entradas (Kg)", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  
  ## Produccion ----
  
  output$ResumenProduccion <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "TOTAL  PRODUCCIÓN") %>% summarise(sum(Valor)) %>% as.numeric()
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()),
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL  PRODUCCIÓN") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL  PRODUCCIÓN") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(comma(aux1, accuracy = 1), "Sacos")),
      title = "Producción:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", comma(aux2), "Sacos", br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", comma(aux3), "Sacos", br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("industry"),
      fill = T,
      color = "teal"
    )
  }) 
  output$DetallesProduccion <- renderUI({
    
    mes <- data_f() %>% filter(Item == "TOTAL  PRODUCCIÓN") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "TOTAL  PRODUCCIÓN") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <-(mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "TOTAL  PRODUCCIÓN") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1

    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',comma(mes_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',comma(an_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$AcumProduccion <- renderPlotly({
    
    item = "TOTAL  PRODUCCIÓN"
    meta = 120000
    
    aux1 <- data_trilladora() %>% 
      filter(year(Fecha) >= anho_ant, Item == item) %>% 
      mutate(Mes = month(Fecha)) %>% 
      group_by(Periodo, Mes) %>% 
      summarise(Valor= sum(Valor)) %>% 
      group_by(Periodo) %>% 
      mutate(Acum = ifelse(Valor ==0, 0, cumsum(Valor)),
             Cump = Acum/meta)
    
    aux2 <- aux1 %>% 
      filter(Periodo == anho , Valor != 0)
    
    max_mes <- max(aux2$Mes)
    
    if (nrow(aux2) == 1){
      pron <- data.frame(Mes=1:12,
                         Pron = 1:12 * aux2$Valor)
    } else{
      pron <- data.frame(Mes=1:12)
      pron$Pron <- predict(lm(data = aux2, Acum ~ Mes), pron)
    }
    
    pron$Cump = pron$Pron/meta
    pron <- pron %>% filter(Mes > max_mes)
    
    plot_ly(data = aux1, x = ~Mes, y = ~Acum, type = 'bar', color = ~factor(Periodo), colors = c("2021"="#2E4053", "2022"="#2980B9"),
            hoverinfo = "text", hovertext = paste0("<b>", aux1$Periodo*100+aux1$Mes, "</b>",
                                                   "<br>Despachos :", comma(aux1$Acum, accuracy = 1),
                                                   "<br>Cumplimiento :", percent(aux1$Cump, accuracy = 0.1))) %>% 
      add_bars(data=pron, x= ~Mes, y = ~Pron, inherit = F, name="Estimación 2022",
               marker = list(color = "#707B7C", line = list(color = "black", width = 1)),
               hoverinfo = "text", hovertext = paste0("<b>", 2021*100+pron$Mes, "</b>",
                                                      "<br>Despachos :", comma(pron$Pron, accuracy = 1),
                                                      "<br>Cumplimiento :", percent(pron$Cump, accuracy = 0.1))) %>% 
      layout(shapes = hline(meta),
             hoverlabel = list(align ="left"),
             title = paste(""),
             xaxis = list(title="Mes",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             yaxis = list(title="Sacos Producidos" , tickformat = "s",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>% 
      config(displayModeBar=F)
    
  })
  output$SB_Produccion <- renderPlotly({
    
    aux1 <- data_f() %>% 
      filter(Seccion == "Produccion", !grepl("TOTAL", Item)) %>% 
      group_by(Item) %>% 
      summarise(Valor =round(sum(Valor, na.rm = T))) %>% 
      ungroup() %>% 
      mutate(Tipo = case_when(Item == "Supremos y Dunkin Donuts"~ "Supremos y Dunkin Donuts", 
                              Item == "Consumos y Pasillas Sacos de 70kg" ~ "Coproductos",
                              T ~ "Excelsos"))
    
    aux2 <- bind_rows(
      aux1 %>% group_by(labels = "Producción", parents="") %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Tipo, parents="Producción") %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Item, parents=Tipo) %>% summarise(values = sum(Valor))
    ) %>% 
      filter(parents !=  "Supremos y Dunkin Donuts") %>% 
      mutate(Texto = case_when(parents == "" ~ "<b>%{label}</b><br>%{value:,} sacos<extra></extra>",
                               parents == "Producción" ~ "<b>%{label}</b><br>%{value:,} sacos<br>%{percentParent:.2%} de %{parent}<extra></extra>",
                               T ~ "<b>%{label}</b><br>%{value:,} sacos<br>%{percentParent:.2%} de %{parent}<br>%{percentEntry:.2%} de Producción<extra></extra>"
      )
      )
    
    plot_ly(aux2, labels = ~labels, parents = ~ parents, values = ~values, type = 'sunburst',  branchvalues = 'total',
            text = ~str_wrap(paste0("<b>", labels), width = 15), 
            textinfo = "text",
            hoverlabel = list(align = "left"),
            hovertemplate= aux2$Texto
    ) %>% 
      layout(colorway = c("Excelsos"="#C4E4E3","Supremos y Dunkin Donuts"="#FFF7EA", "Coproductos"= "#D5F5E3"), 
             margin = m) %>% 
      config(displayModeBar=F)
    
  })
  output$SerieProduccion <- renderPlotly({
    
    var_plot = "TOTAL  PRODUCCIÓN"
    
    aux1 <- data_trilladora() %>%  filter(Item ==	var_plot, Fecha <= fecha_focal())
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Observado",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(var_plot))," :", comma(aux2$Valor, accuracy = 1))
    ) %>% 
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", comma(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", comma(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Kilos (Forecast): ", comma(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = ",", 
                          title=list(text="Sacos Producidos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  
  ## Despachos ----
  
  output$ResumenDespachos <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "TOTAL  DESPACHOS") %>% summarise(sum(Valor)) %>% as.numeric()
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()),
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL  DESPACHOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL  DESPACHOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(comma(aux1, accuracy = 1), "Sacos")),
      title = "Despachos:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", comma(aux2), "Kg", br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", comma(aux3), "Kg", br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("shipping-fast"),
      fill = T,
      color = "teal"
    )
  }) 
  output$DetallesDespachos <- renderUI({
    
    mes <- data_f() %>% filter(Item == "TOTAL  DESPACHOS") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "TOTAL  DESPACHOS") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <-(mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "TOTAL  DESPACHOS") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',comma(mes_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',comma(an_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$AcumDespachos <- renderPlotly({
    
    item = "TOTAL  DESPACHOS"
    meta = 150000
    
    aux1 <- data_trilladora() %>% 
      filter(year(Fecha) >= anho_ant, Item == item) %>% 
      mutate(Mes = month(Fecha)) %>% 
      group_by(Periodo, Mes) %>% 
      summarise(Valor= sum(Valor)) %>% 
      group_by(Periodo) %>% 
      mutate(Acum = ifelse(Valor ==0, 0, cumsum(Valor)),
             Cump = Acum/meta)
    
    aux2 <- aux1 %>% 
      filter(Periodo == anho , Valor != 0)
    
    max_mes <- max(aux2$Mes)
    
    if (nrow(aux2) == 1){
      pron <- data.frame(Mes=1:12,
                         Pron = 1:12 * aux2$Valor)
    } else{
      pron <- data.frame(Mes=1:12)
      pron$Pron <- predict(lm(data = aux2, Acum ~ Mes), pron)
    }
    
    pron$Cump = pron$Pron/meta
    pron <- pron %>% filter(Mes > max_mes)
    
    plot_ly(data = aux1, x = ~Mes, y = ~Acum, type = 'bar', color = ~factor(Periodo), colors = c("2021"="#2E4053", "2022"="#2980B9"),
            hoverinfo = "text", hovertext = paste0("<b>", aux1$Periodo*100+aux1$Mes, "</b>",
                                                   "<br>Despachos :", comma(aux1$Acum, accuracy = 1),
                                                   "<br>Cumplimiento :", percent(aux1$Cump, accuracy = 0.1))) %>% 
      add_bars(data=pron, x= ~Mes, y = ~Pron, inherit = F, name="Estimación 2022",
               marker = list(color = "#707B7C", line = list(color = "black", width = 1)),
               hoverinfo = "text", hovertext = paste0("<b>", 2021*100+pron$Mes, "</b>",
                                                      "<br>Despachos :", comma(pron$Pron, accuracy = 1),
                                                      "<br>Cumplimiento :", percent(pron$Cump, accuracy = 0.1))) %>% 
      layout(shapes = hline(meta),
             hoverlabel = list(align ="left"),
             title = paste(""),
             xaxis = list(title="Mes",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             yaxis = list(title="Sacos Despachados" , tickformat = "s",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>% 
      config(displayModeBar=F)
    
  })
  output$SB_Despachos <- renderPlotly({
    
    aux1 <- data_f() %>% 
      filter(Subseccion == "Despachos (Sacos De 70 Kilos)", !grepl("TOTAL", Item)) %>% 
      group_by(Item) %>% 
      summarise(Valor =sum(Valor, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(Tipo = case_when(Item == "Supremos y Dunkin Donuts"~ "Supremos y Dunkin Donuts", 
                       Item == "Consumos y Pasillas Sacos de 70kg" ~ "Coproductos",
                       T ~ "Excelsos")
             )
    
    aux2 <- bind_rows(
      aux1 %>% group_by(labels = "Despachos", parents="") %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Tipo, parents="Despachos") %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Item, parents=Tipo) %>% summarise(values = sum(Valor))
    ) %>% 
      filter(parents !=  "Supremos y Dunkin Donuts") %>% 
      mutate(Texto = case_when(parents == "" ~ "<b>%{label}</b><br>%{value:,} sacos<extra></extra>",
                               parents == "Despachos" ~ "<b>%{label}</b><br>%{value:,} sacos<br>%{percentParent:.2%} de %{parent}<extra></extra>",
                               T ~ "<b>%{label}</b><br>%{value:,} sacos<br>%{percentParent:.2%} de %{parent}<br>%{percentEntry:.2%} de Producción<extra></extra>"
      )
      )
    
    plot_ly(aux2, labels = ~labels, parents = ~ parents, values = ~values, type = 'sunburst',  branchvalues = 'total',
            text = ~str_wrap(paste0("<b>", labels), width = 15), 
            textinfo = "text",
            hoverlabel = list(align = "left"),
            hovertemplate= aux2$Texto
    ) %>% 
      layout(colorway = c("Excelsos"="#C4E4E3","Supremos y Dunkin Donuts"="#FFF7EA", "Coproductos"= "#D5F5E3"), 
             margin = m) %>% 
      config(displayModeBar=F)
    
  })
  output$SerieDespachos <- renderPlotly({
    
    var_plot = "TOTAL  DESPACHOS"
    
    aux1 <- data_trilladora() %>%  filter(Item ==	var_plot, Fecha <= fecha_focal())
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Observado",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(var_plot))," :", comma(aux2$Valor, accuracy = 1))
    ) %>% 
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", comma(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", comma(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Kilos (Forecast): ", comma(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = ",", 
                          title=list(text="Sacos Despachados", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  
  ## Rechazos ----
  
  output$ResumenRechazos <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "TOTAL  RECHAZOS") %>% summarise(sum(Valor)) %>% as.numeric()
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL  RECHAZOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL  RECHAZOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    variacion <- (aux2/aux3)-1
    variacion <- ifelse(is.na(variacion), 0, variacion)
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(comma(aux1, accuracy = 1), "Sacos")),
      title = "Rechazos:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", comma(aux2), "Sacos", br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", comma(aux3), "Sacos", br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("ban"),
      fill = T,
      color = "teal"
    )
  }) 
  output$DetallesRechazos <- renderUI({
    
    mes <- data_f() %>% filter(Item == "TOTAL  RECHAZOS") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "TOTAL  RECHAZOS") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <- (mes/mes_ant) -1
    tmm <- ifelse(is.na(tmm), 0, tmm)
    
    an_ant <- data_an_ant_f() %>% filter(Item == "TOTAL  RECHAZOS") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1
    tam <- ifelse(is.na(tam), 0, tam)
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',comma(mes_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm, F),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',comma(an_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam, F),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$AcumRechazos <- renderPlotly({
    
    item = "TOTAL  RECHAZOS"
    meta = 0
    
    aux1 <- data_trilladora() %>% 
      filter(year(Fecha) >= anho_ant, Item == item) %>% 
      mutate(Mes = month(Fecha)) %>% 
      group_by(Periodo, Mes) %>% 
      summarise(Valor= sum(Valor)) %>% 
      group_by(Periodo) %>% 
      mutate(Acum = cumsum(Valor),
             Cump = Acum/meta)
    
    aux2 <- aux1 %>% 
      filter(Periodo == anho)
    
    max_mes <- max(aux2$Mes)
    
    if (nrow(aux2) == 1){
      pron <- data.frame(Mes=1:12,
                         Pron = 1:12 * 0)
    } else{
      pron <- data.frame(Mes=1:12)
      pron$Pron <- predict(lm(data = aux2, Acum ~ Mes), pron)
    }
    
    pron$Cump = pron$Pron/meta
    pron <- pron %>% filter(Mes > max_mes)
    
    plot_ly(data = aux1, x = ~Mes, y = ~Acum, type = 'bar', color = ~factor(Periodo), colors = c("2021"="#2E4053", "2022"="#2980B9"),
            hoverinfo = "text", hovertext = paste0("<b>", aux1$Periodo*100+aux1$Mes, "</b>",
                                                   "<br>Despachos :", comma(aux1$Acum, accuracy = 1),
                                                   "<br>Cumplimiento :", percent(aux1$Cump, accuracy = 0.1))) %>% 
      add_bars(data=pron, x= ~Mes, y = ~Pron, inherit = F, name="Estimación 2022",
               marker = list(color = "#707B7C", line = list(color = "black", width = 1)),
               hoverinfo = "text", hovertext = paste0("<b>", 2021*100+pron$Mes, "</b>",
                                                      "<br>Despachos :", comma(pron$Pron, accuracy = 1),
                                                      "<br>Cumplimiento :", percent(pron$Cump, accuracy = 0.1))) %>% 
      layout(shapes = hline(meta),
             hoverlabel = list(align ="left"),
             title = paste(""),
             xaxis = list(title="Mes",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             yaxis = list(title="Sacos Rechazados" , tickformat = "s",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>% 
      config(displayModeBar=F)
    
  })
  output$SB_Rechazos <- renderPlotly({
    
    aux1 <- data_f() %>% 
      filter(Subseccion == "Rechazos (Sacos De 70 Kilos)", !grepl("TOTAL", Item)) %>% 
      group_by(Item) %>% 
      summarise(Valor =sum(Valor, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(Tipo = case_when(Item == "Supremos y Dunkin Donuts"~ "Supremos y Dunkin Donuts", 
                              Item == "Consumos y Pasillas Sacos de 70kg" ~ "Coproductos",
                              T ~ "Excelsos"))
    
    aux2 <- bind_rows(
      aux1 %>% group_by(labels = "Rechazos", parents="") %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Tipo, parents="Rechazos") %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Item, parents=Tipo) %>% summarise(values = sum(Valor))
    ) %>% 
      filter(parents !=  "Supremos y Dunkin Donuts") %>% 
      mutate(Texto = case_when(parents == "" ~ "<b>%{label}</b><br>%{value:,} sacos<extra></extra>",
                               parents == "Rechazos" ~ "<b>%{label}</b><br>%{value:,} sacos<br>%{percentParent:.2%} de %{parent}<extra></extra>",
                               T ~ "<b>%{label}</b><br>%{value:,} sacos<br>%{percentParent:.2%} de %{parent}<br>%{percentEntry:.2%} de Producción<extra></extra>"
      )
      )
    
    plot_ly(aux2, labels = ~labels, parents = ~ parents, values = ~values, type = 'sunburst',  branchvalues = 'total',
            text = ~str_wrap(paste0("<b>", labels), width = 15), 
            textinfo = "text",
            hoverlabel = list(align = "left"),
            hovertemplate= aux2$Texto
    ) %>% 
      layout(colorway = c("#C4E4E3","#FFF7EA"), 
             margin = m) %>% 
      config(displayModeBar=F)
    
  })
  output$SerieRechazos <- renderPlotly({
    
    var_plot = "TOTAL  RECHAZOS"
    
    aux1 <- data_trilladora() %>%  filter(Item ==	var_plot, Fecha <= fecha_focal())
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Observado",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(var_plot))," :", comma(aux2$Valor, accuracy = 1))
    ) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = ",", 
                          title=list(text="Sacos Rechazados", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  
  ## PyG ----
  ### Ingresos ----

  output$ResumenIngresos <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "TOTAL INGRESOS") %>% summarise(sum(Valor)) %>% as.numeric() *1000
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL INGRESOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()*1000
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL INGRESOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(dollar(aux1, accuracy = 1), "")),
      title = "Ingresos Sobre Despachos:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", dollar(aux2, scale = 1/1e6, suffix = " MM"), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", dollar(aux3, scale = 1/1e6, suffix = " MM"), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("money-bill-wave"),
      fill = T,
      color = "teal"
    )
  }) 
  output$DetallesIngresos <- renderUI({
    
    mes <- data_f() %>% filter(Item == "TOTAL INGRESOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "TOTAL INGRESOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "TOTAL INGRESOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>', dollar(mes_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>', dollar(an_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$ResumenOtrosIngresos <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "TOTAL OTROS INGRESOS") %>% summarise(sum(Valor)) %>% as.numeric() *1000
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL OTROS INGRESOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()*1000
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL OTROS INGRESOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(dollar(aux1, accuracy = 1), "")),
      title = "Otros Ingresos:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", dollar(aux2, scale = 1/1e6, suffix = " MM"), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", dollar(aux3, scale = 1/1e6, suffix = " MM"), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("money-bill-wave"),
      fill = T,
      color = "teal"
    )
  }) 
  output$DetallesOtrosIngresos <- renderUI({
    
    mes <- data_f() %>% filter(Item == "TOTAL OTROS INGRESOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "TOTAL OTROS INGRESOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "TOTAL OTROS INGRESOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>', dollar(mes_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>', dollar(an_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$SB_Ingresos <- renderPlotly({
    
    aux1 <- data_f() %>% 
      filter(Subseccion %in% c("Ingresos Sobre Despachos  Miles De Pesos", 
                               "Otros Ingresos Miles De Pesos"), !grepl("TOTAL", Item)) %>%
      group_by(Subseccion, Item) %>% 
      summarise(Valor =sum(Valor, na.rm = T)*1000) %>% 
      ungroup() %>% 
      mutate(Tipo = case_when(Item == "Supremos y Dunkin Donuts"~ "Supremos y Dunkin Donuts", 
                              Item == "Consumos y Pasillas Sacos de 70kg" ~ "Coproductos",
                              grepl("Recuperación", Item, ignore.case = T) ~ "Otros",
                              T ~ "Excelsos"),
             Subseccion = ifelse(Subseccion == "Ingresos Sobre Despachos  Miles De Pesos", "Sobre despachos",
                                 "Otros Ingresos"))
    
    aux2 <- bind_rows(
      aux1 %>% group_by(labels = "Ingresos", parents="") %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Subseccion, parents="Ingresos") %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Tipo, parents=Subseccion) %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Item, parents=Tipo) %>% summarise(values = sum(Valor))
    ) %>% 
      filter(parents !=  "Supremos y Dunkin Donuts",
             labels != "Otros") %>% 
      mutate(parents = ifelse(parents == "Otros", "Otros Ingresos", parents)) %>% 
      mutate(Texto = case_when(parents == "" ~ "<b>%{label}</b><br>%{value:$,} <extra></extra>",
                               parents == "Despachos" ~ "<b>%{label}</b><br>%{value:$,} <br>%{percentParent:.2%} de %{parent}<extra></extra>",
                               T ~ "<b>%{label}</b><br>%{value:$,} <br>%{percentParent:.2%} de %{parent}<br>%{percentEntry:.2%} de Ingresos<extra></extra>"
      )
      )
    
    plot_ly(aux2, labels = ~labels, parents = ~ parents, values = ~values, type = 'sunburst',  branchvalues = 'total',
            text = ~str_wrap(paste0("<b>", labels), width = 15), 
            textinfo = "text",
            hoverlabel = list(align = "left"),
            hovertemplate= aux2$Texto
    ) %>% 
      layout(colorway = c("#C4E4E3","#FFF7EA"), 
             margin = m) %>% 
      config(displayModeBar=F)
    
  })
  output$AcumIngresos <- renderPlotly({
    
    item = "TOTAL INGRESOS"
    meta = 1500000000
    
    aux1 <- data_trilladora() %>% 
      filter(year(Fecha) >= anho_ant, Item == item) %>% 
      mutate(Mes = month(Fecha)) %>% 
      group_by(Periodo, Mes) %>% 
      summarise(Valor= sum(Valor)*1000) %>% 
      group_by(Periodo) %>% 
      mutate(Acum = ifelse(Valor ==0, 0, cumsum(Valor)),
             Cump = Acum/meta)
    
    aux2 <- aux1 %>% 
      filter(Periodo == anho , Valor != 0)
    
    max_mes <- max(aux2$Mes)
    
    if (nrow(aux2) == 1){
      pron <- data.frame(Mes=1:12,
                         Pron = 1:12 * aux2$Valor)
    } else{
      pron <- data.frame(Mes=1:12)
      pron$Pron <- predict(lm(data = aux2, Acum ~ Mes), pron)
    }
    
    pron$Cump = pron$Pron/meta
    pron <- pron %>% filter(Mes > max_mes)
    
    plot_ly(data = aux1, x = ~Mes, y = ~Acum, type = 'bar', color = ~factor(Periodo), colors = c("2021"="#2E4053", "2022"="#2980B9"),
            hoverinfo = "text", hovertext = paste0("<b>", aux1$Periodo*100+aux1$Mes, "</b>",
                                                   "<br>Despachos :", comma(aux1$Acum, accuracy = 1),
                                                   "<br>Cumplimiento :", percent(aux1$Cump, accuracy = 0.1))) %>% 
      add_bars(data=pron, x= ~Mes, y = ~Pron, inherit = F, name="Estimación 2022",
               marker = list(color = "#707B7C", line = list(color = "black", width = 1)),
               hoverinfo = "text", hovertext = paste0("<b>", 2021*100+pron$Mes, "</b>",
                                                      "<br>Despachos :", comma(pron$Pron, accuracy = 1),
                                                      "<br>Cumplimiento :", percent(pron$Cump, accuracy = 0.1))) %>% 
      layout(shapes = hline(meta),
             hoverlabel = list(align ="left"),
             title = paste(""),
             xaxis = list(title="Mes",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             yaxis = list(title="Ingresos" , tickformat = "s",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>% 
      config(displayModeBar=F)
    
  })
  output$AcumOtrosIngresos <- renderPlotly({
    
    item = "TOTAL OTROS INGRESOS"
    meta = 80000000
    
    aux1 <- data_trilladora() %>% 
      filter(year(Fecha) >= anho_ant, Item == item) %>% 
      mutate(Mes = month(Fecha)) %>% 
      group_by(Periodo, Mes) %>% 
      summarise(Valor= sum(Valor)*1000) %>% 
      group_by(Periodo) %>% 
      mutate(Acum = ifelse(Valor ==0, 0, cumsum(Valor)),
             Cump = Acum/meta)
    
    aux2 <- aux1 %>% 
      filter(Periodo == anho , Valor != 0)
    
    max_mes <- max(aux2$Mes)
    
    if (nrow(aux2) == 1){
      pron <- data.frame(Mes=1:12,
                         Pron = 1:12 * aux2$Valor)
    } else{
      pron <- data.frame(Mes=1:12)
      pron$Pron <- predict(lm(data = aux2, Acum ~ Mes), pron)
    }
    
    pron$Cump = pron$Pron/meta
    pron <- pron %>% filter(Mes > max_mes)
    
    plot_ly(data = aux1, x = ~Mes, y = ~Acum, type = 'bar', color = ~factor(Periodo), colors = c("2021"="#2E4053", "2022"="#2980B9"),
            hoverinfo = "text", hovertext = paste0("<b>", aux1$Periodo*100+aux1$Mes, "</b>",
                                                   "<br>Despachos :", comma(aux1$Acum, accuracy = 1),
                                                   "<br>Cumplimiento :", percent(aux1$Cump, accuracy = 0.1))) %>% 
      add_bars(data=pron, x= ~Mes, y = ~Pron, inherit = F, name="Estimación 2022",
               marker = list(color = "#707B7C", line = list(color = "black", width = 1)),
               hoverinfo = "text", hovertext = paste0("<b>", 2021*100+pron$Mes, "</b>",
                                                      "<br>Despachos :", comma(pron$Pron, accuracy = 1),
                                                      "<br>Cumplimiento :", percent(pron$Cump, accuracy = 0.1))) %>% 
      layout(shapes = hline(meta),
             hoverlabel = list(align ="left"),
             title = paste(""),
             xaxis = list(title="Mes",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             yaxis = list(title="Otros Ingresos" , tickformat = "s",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>% 
      config(displayModeBar=F)
    
  })
  output$SerieIngresosTotal <- renderPlotly({

    aux1 <- data_trilladora() %>%  
      filter(Subseccion %in% c("Ingresos Sobre Despachos  Miles De Pesos", 
                               "Otros Ingresos Miles De Pesos"), !grepl("TOTAL", Item)) %>%
      filter(Fecha <= fecha_focal()) %>% 
      mutate(Valor = Valor *1000) %>% 
      group_by(Fecha) %>% 
      summarise(Valor = sum(Valor))
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Observado",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws("Ingresos"))," :", dollar(aux2$Valor, accuracy = 1))
    ) %>% 
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", dollar(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", dollar(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Ingresos (Forecast): ", dollar(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,.2s", 
                          title=list(text="Ingresos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
  })
  output$SerieIngresosDet1 <- renderPlotly({
    
    aux1 <- data_trilladora() %>%  
      filter(Subseccion %in% c("Ingresos Sobre Despachos  Miles De Pesos", 
                               "Otros Ingresos Miles De Pesos"), !grepl("TOTAL", Item)) %>%
      filter(Fecha <= fecha_focal()) %>% mutate(Valor = Valor *1000) %>% 
      group_by(Fecha, Subseccion) %>% 
      summarise(Valor = sum(Valor)) %>% 
      mutate(Subseccion = ifelse(Subseccion == "Ingresos Sobre Despachos  Miles De Pesos", "Sobre despachos",
                                 "Otros Ingresos"))
    
    plot_ly(data= aux1, x = ~Fecha , y= ~Valor, mode="line", color = ~Subseccion,
            line = list(width = 2), marker = list(size = 5), 
            colors = c("Otros Ingresos"="#34495E", "Sobre despachos"="#2471A3"),
            textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(aux1$Subseccion))," :", dollar(aux1$Valor, accuracy = 1))
    ) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,.2s", 
                          title=list(text="Ingresos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
  })
  output$SerieIngresosDet2 <- renderPlotly({
    
    aux1 <- data_trilladora() %>%  
      filter(Subseccion %in% c("Ingresos Sobre Despachos  Miles De Pesos", 
                               "Otros Ingresos Miles De Pesos"), !grepl("TOTAL", Item)) %>%
      filter(Fecha <= fecha_focal()) %>% mutate(Valor = Valor *1000) %>% 
      mutate(Tipo = case_when(Item == "Supremos y Dunkin Donuts" ~"Supremos y Dunkin Donuts",
                              grepl("Recuperación", Item, ignore.case = T) ~ "Otros Ingresos",
                              T ~ "Excelsos"),
             Subseccion = ifelse(Subseccion == "Ingresos Sobre Despachos  Miles De Pesos", "Sobre despachos",
                                 "Otros Ingresos")) %>% 
      group_by(Fecha, Tipo) %>% 
      summarise(Valor = sum(Valor))
    
    aux1$Tipo <- factor(aux1$Tipo, levels = c("Otros Ingresos", "Excelsos", "Supremos y Dunkin Donuts"), ordered = T)
    
    plot_ly(data= aux1, x = aux1$Fecha , y= aux1$Valor, type = "scatter", mode="line", color = ~Tipo,
            line = list(width = 2), marker = list(size = 5), 
            colors = c("Otros Ingresos"="#34495E", "Excelsos"="#2471A3", "Supremos y Dunkin Donuts" = "#16A085"),
            textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(aux1$Tipo))," :", dollar(aux1$Valor, accuracy = 1))
    ) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,.2s", 
                          title=list(text="Ingresos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
  })
  output$SerieIngresosDet3 <- renderPlotly({
    
    aux1 <- data_trilladora() %>%  
      filter(Subseccion %in% c("Ingresos Sobre Despachos  Miles De Pesos", 
                               "Otros Ingresos Miles De Pesos"), !grepl("TOTAL", Item)) %>%
      filter(Fecha <= fecha_focal()) %>% mutate(Valor = Valor *1000) %>% 
      mutate(Tipo = case_when(Item == "Supremos y Dunkin Donuts" ~"Supremos y Dunkin Donuts",
                              grepl("Recuperación", Item, ignore.case = T) ~ "Otros Ingresos",
                              T ~ "Excelsos"),
             Subseccion = ifelse(Subseccion == "Ingresos Sobre Despachos  Miles De Pesos", "Sobre despachos",
                                 "Otros Ingresos")) %>% 
      group_by(Fecha, Item) %>% 
      summarise(Valor = sum(Valor))
    
    plot_ly(data= aux1, x = aux1$Fecha , y= aux1$Valor, type = "scatter", mode="line", color = ~Item,
            line = list(width = 2), marker = list(size = 5),  colors = "Paired",
            textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(aux1$Item))," :", dollar(aux1$Valor, accuracy = 1))
    ) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,.2s", 
                          title=list(text="Ingresos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  
  ### Costos ----

  output$ResumenCostos <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "TOTAL COSTOS") %>% summarise(sum(Valor)) %>% as.numeric() %>% abs() *1000
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL COSTOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() %>% abs()*1000
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL COSTOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() %>% abs() *1000
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(dollar(aux1, accuracy = 1), "")),
      title = "Costos:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", dollar(aux2, scale = 1/1e6, suffix = " MM"), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", dollar(aux3, scale = 1/1e6, suffix = " MM"), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("money-bill-wave"),
      fill = T,
      color = "teal"
    )
  }) 
  output$DetallesCostos <- renderUI({
    
    mes <- data_f() %>% filter(Item == "TOTAL COSTOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() %>% abs()*1000
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "TOTAL COSTOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() %>% abs()*1000
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "TOTAL COSTOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() %>% abs()*1000
    tam <- (mes/an_ant) -1
    
    col_tmm = ifelse(tmm >=0, "#943126", "#0B5345")
    chr_tmm = ifelse(tmm <=0, "▼", "▲")
    
    col_tam = ifelse(tam >=0, "#943126", "#0B5345")
    chr_tam = ifelse(tam >=0, "▼", "▲")
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>', dollar(mes_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm, F),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>', dollar(an_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam, F),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$ResumenOtrosCostos <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "TOTAL OTROS  COSTOS") %>% summarise(sum(Valor)) %>% as.numeric() %>% abs() *1000
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL OTROS  COSTOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() %>% abs()*1000
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "TOTAL OTROS  COSTOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() %>% abs() *1000
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(dollar(aux1, accuracy = 1), "")),
      title = "Otros Costos:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", dollar(aux2, scale = 1/1e6, suffix = " MM"), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", dollar(aux3, scale = 1/1e6, suffix = " MM"), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("money-bill-wave"),
      fill = T,
      color = "teal"
    )
  }) 
  output$DetallesOtrosCostos <- renderUI({
    
    mes <- data_f() %>% filter(Item == "TOTAL OTROS  COSTOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() %>% abs()*1000
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "TOTAL OTROS  COSTOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() %>% abs()*1000
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "TOTAL OTROS  COSTOS") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() %>% abs()*1000
    tam <- (mes/an_ant) -1
    
    col_tmm = ifelse(tmm <=0, "#943126", "#0B5345")
    chr_tmm = ifelse(tmm <=0, "▼", "▲")
    
    col_tam = ifelse(tam <=0, "#943126", "#0B5345")
    chr_tam = ifelse(tam <=0, "▼", "▲")
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>', dollar(mes_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm, F),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>', dollar(an_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam, F),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$SB_Costos <- renderPlotly({
    
    aux1 <- data_f() %>% 
      filter(Subseccion %in% c("Costos  Miles De Pesos", 
                               "Otros Costos Miles De Pesos"), !grepl("TOTAL", Item),
             Item != "Funcionamiento") %>%
      mutate(Tipo = case_when( Item %in% c("Vigilancia", "Costos de viaje", 
                                           "Costos legales", "Servicios publicos",
                                           "Otros Funcionamiento") ~ "Funcionamiento",
                               Subseccion == "Otros Costos Miles De Pesos" ~ "Otros",
                               T ~ Item),
      Subseccion = ifelse(Subseccion == "Costos  Miles De Pesos", "Costos", "Otros Costos")
      ) %>% 
      group_by(Subseccion, Tipo, Item) %>% 
      summarise(Valor =sum(Valor, na.rm = T)*-1000)
    
    aux2 <- bind_rows(
      aux1 %>% group_by(labels = "Costo", parents="") %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Subseccion, parents="Costo") %>% summarise(values = sum(Valor)),
      aux1 %>% group_by(labels = Tipo, parents=Subseccion) %>% summarise(values = sum(Valor)),
      aux1 %>% filter(Item != Tipo) %>% group_by(labels = Item, parents=Tipo) %>% summarise(values = sum(Valor))
    ) %>%
      mutate(parents = ifelse(parents == "Otros", "Otros Costos", parents)) %>% 
      filter(labels != "Otros") %>% 
      mutate(Texto = case_when(parents == "" ~ "<b>%{label}</b><br>%{value:,} sacos<extra></extra>",
                               parents == "Despachos" ~ "<b>%{label}</b><br>%{value:,} sacos<br>%{percentParent:.2%} de %{parent}<extra></extra>",
                               T ~ "<b>%{label}</b><br>%{value:,} sacos<br>%{percentParent:.2%} de %{parent}<br>%{percentEntry:.2%} de Producción<extra></extra>"
      )
      )
    
    plot_ly(aux2, labels = ~labels, parents = ~ parents, values = ~values, type = 'sunburst',  branchvalues = 'total',
            text = ~str_wrap(paste0("<b>", labels), width = 15), 
            textinfo = "text",
            hoverlabel = list(align = "left"),
            hovertemplate= aux2$Texto
    ) %>% 
      layout(colorway = c("#C4E4E3","#FFF7EA"), 
             margin = m) %>% 
      config(displayModeBar=F)
    
  })
  output$AcumCostos <- renderPlotly({
    
    item = "TOTAL COSTOS"
    meta = 1500000000
    
    aux1 <- data_trilladora() %>% 
      filter(year(Fecha) >= anho_ant, Item == item) %>% 
      mutate(Mes = month(Fecha)) %>% 
      group_by(Periodo, Mes) %>% 
      summarise(Valor= sum(Valor)*-1000) %>% 
      group_by(Periodo) %>% 
      mutate(Acum = ifelse(Valor ==0, 0, cumsum(Valor)),
             Cump = Acum/meta)
    
    aux2 <- aux1 %>% 
      filter(Periodo == anho , Valor != 0)
    
    max_mes <- max(aux2$Mes)
    
    if (nrow(aux2) == 1){
      pron <- data.frame(Mes=1:12,
                         Pron = 1:12 * aux2$Valor)
    } else{
      pron <- data.frame(Mes=1:12)
      pron$Pron <- predict(lm(data = aux2, Acum ~ Mes), pron)
    }
    
    pron$Cump = pron$Pron/meta
    pron <- pron %>% filter(Mes > max_mes)
    
    plot_ly(data = aux1, x = ~Mes, y = ~Acum, type = 'bar', color = ~factor(Periodo), colors = c("2021"="#2E4053", "2022"="#2980B9"),
            hoverinfo = "text", hovertext = paste0("<b>", aux1$Periodo*100+aux1$Mes, "</b>",
                                                   "<br>Despachos :", comma(aux1$Acum, accuracy = 1),
                                                   "<br>Cumplimiento :", percent(aux1$Cump, accuracy = 0.1))) %>% 
      add_bars(data=pron, x= ~Mes, y = ~Pron, inherit = F, name="Estimación 2022",
               marker = list(color = "#707B7C", line = list(color = "black", width = 1)),
               hoverinfo = "text", hovertext = paste0("<b>", 2021*100+pron$Mes, "</b>",
                                                      "<br>Despachos :", comma(pron$Pron, accuracy = 1),
                                                      "<br>Cumplimiento :", percent(pron$Cump, accuracy = 0.1))) %>% 
      layout(shapes = hline(meta),
             hoverlabel = list(align ="left"),
             title = paste(""),
             xaxis = list(title="Mes",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             yaxis = list(title="Costos" , tickformat = "s",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>% 
      config(displayModeBar=F)
    
  })
  output$AcumOtrosCostos <- renderPlotly({
    
    item = "TOTAL OTROS  COSTOS"
    meta = 80000000
    
    aux1 <- data_trilladora() %>% 
      filter(year(Fecha) >= anho_ant, Item == item) %>% 
      mutate(Mes = month(Fecha)) %>% 
      group_by(Periodo, Mes) %>% 
      summarise(Valor= sum(Valor)*-1000) %>% 
      group_by(Periodo) %>% 
      mutate(Acum = ifelse(Valor ==0, 0, cumsum(Valor)),
             Cump = Acum/meta)
    
    aux2 <- aux1 %>% 
      filter(Periodo == anho , Valor != 0)
    
    max_mes <- max(aux2$Mes)
    
    if (nrow(aux2) == 1){
      pron <- data.frame(Mes=1:12,
                         Pron = 1:12 * aux2$Valor)
    } else{
      pron <- data.frame(Mes=1:12)
      pron$Pron <- predict(lm(data = aux2, Acum ~ Mes), pron)
    }
    
    pron$Cump = pron$Pron/meta
    pron <- pron %>% filter(Mes > max_mes)
    
    plot_ly(data = aux1, x = ~Mes, y = ~Acum, type = 'bar', color = ~factor(Periodo), colors = c("2021"="#2E4053", "2022"="#2980B9"),
            hoverinfo = "text", hovertext = paste0("<b>", aux1$Periodo*100+aux1$Mes, "</b>",
                                                   "<br>Despachos :", comma(aux1$Acum, accuracy = 1),
                                                   "<br>Cumplimiento :", percent(aux1$Cump, accuracy = 0.1))) %>% 
      add_bars(data=pron, x= ~Mes, y = ~Pron, inherit = F, name="Estimación 2022",
               marker = list(color = "#707B7C", line = list(color = "black", width = 1)),
               hoverinfo = "text", hovertext = paste0("<b>", 2021*100+pron$Mes, "</b>",
                                                      "<br>Despachos :", comma(pron$Pron, accuracy = 1),
                                                      "<br>Cumplimiento :", percent(pron$Cump, accuracy = 0.1))) %>% 
      layout(shapes = hline(meta),
             hoverlabel = list(align ="left"),
             title = paste(""),
             xaxis = list(title="Mes",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             yaxis = list(title="Otros Costos" , tickformat = "s",
                          tickfont= list(family = "Arial, sans-serif",size = 12)),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>% 
      config(displayModeBar=F)
    
  })
  output$SerieCostosTotal <- renderPlotly({
    
    aux1 <- data_trilladora() %>%  
      filter(Subseccion %in% c("Costos  Miles De Pesos", 
                               "Otros Costos Miles De Pesos"), !grepl("TOTAL", Item),
             Item != "Funcionamiento") %>%
      filter(Fecha <= fecha_min) %>% 
      mutate(Valor = Valor *-1000) %>% 
      group_by(Fecha) %>% 
      summarise(Valor = sum(Valor))
    
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Observado",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence("Costos")," :", dollar(aux2$Valor, accuracy = 1))
    ) %>% 
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", dollar(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", dollar(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Ingresos (Forecast): ", dollar(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,.2s", 
                          title=list(text="Costos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
  })
  output$SerieCostosDet1 <- renderPlotly({
    
    aux1 <- data_trilladora() %>%  
      filter(Subseccion %in% c("Costos  Miles De Pesos", 
                                                            "Otros Costos Miles De Pesos"), !grepl("TOTAL", Item),
                                          Item != "Funcionamiento") %>%
      filter(Fecha <= fecha_min) %>% 
      mutate(Valor = Valor *-1000,
             Subseccion = ifelse(Subseccion == "Costos  Miles De Pesos", "Costos", "Otros Costos")) %>% 
      group_by(Fecha, Subseccion) %>% 
      summarise(Valor = sum(Valor))
    
    plot_ly(data= aux1, x = ~Fecha , y= ~Valor, type = "scatter", mode="line+markers", color = ~Subseccion,
            line = list(width = 2), marker = list(size = 5), 
            colors = c("Otros Costos"="#34495E", "Costos"="#2471A3"),
            textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(aux1$Subseccion)," :", dollar(aux2$Valor, accuracy = 1))
    ) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,.2s", 
                          title=list(text="Costos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.18, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  output$SerieCostosDet2 <- renderPlotly({
    
    aux1 <- data_trilladora() %>%  
      filter(Subseccion %in% c("Costos  Miles De Pesos", 
                               "Otros Costos Miles De Pesos"), !grepl("TOTAL", Item),
             Item != "Funcionamiento") %>%
      filter(Fecha <= fecha_min) %>% 
      mutate(Valor = Valor *-1000,
             Subseccion = ifelse(Subseccion == "Costos  Miles De Pesos", "Costos", "Otros Costos"),
             Tipo = case_when( Item %in% c("Vigilancia", "Costos de viaje", 
                                           "Costos legales", "Servicios publicos",
                                           "Otros Funcionamiento") ~ "Funcionamiento",
                               Subseccion == "Otros Costos Miles De Pesos" ~ "Otros",
                               T ~ Item)) %>% 
      group_by(Fecha, Tipo) %>% 
      summarise(Valor = sum(Valor))
    
    plot_ly(data= aux1, x = ~Fecha , y= ~Valor, type = "scatter", mode="line+markers", color = ~Tipo,
            line = list(width = 2), marker = list(size = 5), 
            colors = "Paired",
            textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(aux1$Tipo)," :", dollar(aux2$Valor, accuracy = 1))
    ) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,.2s", 
                          title=list(text="Costos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.2, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
  })
  output$SerieCostosDet3 <- renderPlotly({
    
    aux1 <- data_trilladora() %>%  
      filter(Subseccion %in% c("Costos  Miles De Pesos", 
                               "Otros Costos Miles De Pesos"), !grepl("TOTAL", Item),
             Item != "Funcionamiento") %>%
      filter(Fecha <= fecha_min) %>% 
      mutate(Valor = Valor *-1000,
             Subseccion = ifelse(Subseccion == "Costos  Miles De Pesos", "Costos", "Otros Costos"),
             Tipo = case_when( Item %in% c("Vigilancia", "Costos de viaje", 
                                           "Costos legales", "Servicios publicos",
                                           "Otros Funcionamiento") ~ "Funcionamiento",
                               Subseccion == "Otros Costos Miles De Pesos" ~ "Otros",
                               T ~ Item)) %>% 
      group_by(Fecha, Item) %>% 
      summarise(Valor = sum(Valor))
    
    plot_ly(data= aux1, x = ~Fecha , y= ~Valor, type = "scatter", mode="line+markers", color = ~Item,
            line = list(width = 2), marker = list(size = 5), 
            colors = "Paired",
            textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(aux1$Item)," :", dollar(aux2$Valor, accuracy = 1))
    ) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,.2s", 
                          title=list(text="Costos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.20, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
  })
  
  ### Utilidades ----
  
  output$ResumenUtOperacional <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) OPERACIONAL") %>% summarise(sum(Valor)) %>% as.numeric() *1000
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), Item == "UTILIDAD O (PÉRDIDA) OPERACIONAL") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()*1000
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "UTILIDAD O (PÉRDIDA) OPERACIONAL") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    col <- ifelse(aux1 <= 0, "red", "olive")
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(dollar(aux1, accuracy = 1), "")),
      title = "Utilidad Operacional:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", dollar(aux2, scale = 1/1e6, suffix = " MM"), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", dollar(aux3, scale = 1/1e6, suffix = " MM"), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("hand-holding-usd"),
      fill = T,
      color = col
    )
  }) 
  output$DetallesUtOperacional <- renderUI({
    
    mes <- data_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) OPERACIONAL") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) OPERACIONAL") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) OPERACIONAL") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    tam <- (mes/an_ant) -1
    
    col_tmm = ifelse(tmm <=0, "#943126", "#0B5345")
    chr_tmm = ifelse(tmm <=0, "▼", "▲")
    
    col_tam = ifelse(tam <=0, "#943126", "#0B5345")
    chr_tam = ifelse(tam <=0, "▼", "▲")
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>', dollar(mes_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>', dollar(an_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  
  output$ResumenUtCorte <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) EN EL CORTE") %>% summarise(sum(Valor)) %>% as.numeric() *1000
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), Item == "UTILIDAD O (PÉRDIDA) EN EL CORTE") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()*1000
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "UTILIDAD O (PÉRDIDA) EN EL CORTE") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    col <- ifelse(aux1 <= 0, "red", "olive")
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(dollar(aux1, accuracy = 1), "")),
      title = "Utilidad en el Corte:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", dollar(aux2, scale = 1/1e6, suffix = " MM"), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", dollar(aux3, scale = 1/1e6, suffix = " MM"), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("hand-holding-usd"),
      fill = T,
      color = col
    )
  }) 
  output$DetallesUtCorte <- renderUI({
    
    mes <- data_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) EN EL CORTE") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) EN EL CORTE") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) EN EL CORTE") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    tam <- (mes/an_ant) -1
    
    col_tmm = ifelse(tmm <=0, "#943126", "#0B5345")
    chr_tmm = ifelse(tmm <=0, "▼", "▲")
    
    col_tam = ifelse(tam <=0, "#943126", "#0B5345")
    chr_tam = ifelse(tam <=0, "▼", "▲")
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>', dollar(mes_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>', dollar(an_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  
  output$ResumenUtNeta <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) NETA") %>% summarise(sum(Valor)) %>% as.numeric() *1000
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), Item == "UTILIDAD O (PÉRDIDA) NETA") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()*1000
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "UTILIDAD O (PÉRDIDA) NETA") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    col <- ifelse(aux1 <= 0, "red", "olive")
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(dollar(aux1, accuracy = 1), "")),
      title = "Utilidad Neta:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", dollar(aux2, scale = 1/1e6, suffix = " MM"), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", dollar(aux3, scale = 1/1e6, suffix = " MM"), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("hand-holding-usd"),
      fill = T,
      color = col
    )
  }) 
  output$DetallesUtNeta <- renderUI({
    
    mes <- data_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) NETA") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) NETA") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "UTILIDAD O (PÉRDIDA) NETA") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    tam <- (mes/an_ant) -1
    
    col_tmm = ifelse(tmm <=0, "#943126", "#0B5345")
    chr_tmm = ifelse(tmm <=0, "▼", "▲")
    
    col_tam = ifelse(tam <=0, "#943126", "#0B5345")
    chr_tam = ifelse(tam <=0, "▼", "▲")
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>', dollar(mes_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>', dollar(an_ant),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  
  output$SerieUtilidades <- renderPlotly({
    
    var_plot = "UTILIDAD O (PÉRDIDA) OPERACIONAL"
    var_plot2 = "UTILIDAD O (PÉRDIDA) EN EL CORTE"
    var_plot3 = "UTILIDAD O (PÉRDIDA) NETA"
    
    aux1 <- data_trilladora() %>%  mutate(Valor = Valor*1000) %>% filter(!is.na(Fecha), Item ==	var_plot, Fecha <= fecha_focal())
    aux2 <- data_trilladora() %>%  mutate(Valor = Valor*1000) %>% filter(!is.na(Fecha), Item ==	var_plot2, Fecha <= fecha_focal())
    aux3 <- data_trilladora() %>%  mutate(Valor = Valor*1000) %>% filter(!is.na(Fecha), Item ==	var_plot3, Fecha <= fecha_focal())
    
    plot_ly(data= aux1, x = ~Fecha , y= ~Valor, type = "scatter", mode="lines",
            line = list(width = 2, color = "#212F3D"), name = "Operacional",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1),
            hoverinfo = "text", hoverlabel = list(align = "left"),
            hovertext = paste0("<b>", format(aux1$Fecha, "%B %Y"), "</b>", 
                               "<br><b>Utilidad Operacional: ", dollar(aux1$Valor))) %>% 
      add_lines(data= aux2, x = ~Fecha , y= ~Valor, color = I("#138D75"), type = "scatter", mode="lines+markers",
                name = "En el Corte", inherit = F,
                hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", format(aux2$Fecha, "%B %Y"), "</b>", 
                                   "<br><b>Utilidad Operacional: ", dollar(aux2$Valor))) %>% 
      add_lines(data= aux3, x = ~Fecha , y= ~Valor, color = I("#D35400"), type = "scatter", mode="lines+markers",
                name = "Neta", inherit = F,
                hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", format(aux2$Fecha, "%B %Y"), "</b>", 
                                   "<br><b>Utilidad Operacional: ", dollar(aux2$Valor))) %>% 
      layout(title = list(text="Utilidades", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,.2s", 
                          title=list(text="Utilidad", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.18, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
  })
  
  
  ## Indicadores ----
  
  ### Proveedores ----
  #### Este indidicador necesita mas trabajo para las agregaciones
  output$BarrasProveedores <- renderPlotly({
    
    aux1 <- data_f() %>% 
      mutate(Periodo = "Periodo Vigente") %>% 
      filter(Item %in% c("Cantidad Proveedores", "Pareto Proveedores")) 
    
    p1<- plot_ly(data = aux1, x=~Periodo, y=~Valor, color = ~Item, type = "bar", 
                 colors = c("Cantidad Proveedores"="#2471A3", "Pareto Proveedores"="#34495E"),
                 textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1), 
                 hoverlabel = list(align = "left"), hoverinfo = "text",
                 hovertext = paste0("<b>", "Mes Vigente", "</b>",
                                    "<br>",str_to_sentence(trimws(aux1$Item))," :", comma(aux1$Valor, accuracy = 1))) %>% 
      layout(barmode = "group",
             title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = ",", 
                          title=list(text="Proveedores", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.05, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
    aux1 <- data_trilladora() %>% 
      filter(Periodo == year(fecha_min), is.na(Fecha)) %>% 
      filter(Item %in% c("Cantidad Proveedores", "Pareto Proveedores")) %>% 
      mutate(Periodo = "Acumulado")
    
    p2<- plot_ly(data = aux1, x=~Periodo, y=~Valor, color = ~Item, type = "bar",  showlegend = FALSE,
                 colors = c("Cantidad Proveedores"="#2471A3", "Pareto Proveedores"="#34495E"),
                 textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1), 
                 hoverlabel = list(align = "left"), hoverinfo = "text",
                 hovertext = paste0("<b>", "Mes Vigente", "</b>",
                                    "<br>",str_to_sentence(trimws(aux1$Item))," :", comma(aux1$Valor, accuracy = 1))) %>% 
      layout(barmode = "group",
             title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = ",", 
                          title=list(text="Proveedores", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>% 
      config(displayModeBar=F)
    
    
    subplot(p1, p2, shareY = T)
    
    
  })
  output$GaugeProveedoresPeriodo <- renderPlotly({
    
    mes <- data_f() %>% 
      filter(Item == "Proveedor con mayor concentración %") %>% summarise(sum(Valor)) %>% as.numeric()
    
    meta = 0.13
    
    lim1 = 0.13
    lim2 = 0.20
    lim3 = 0.40
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = mes *1,
      title = list(text = ""),
      type = "indicator",
      mode = "gauge+number",
      number = list(valueformat = ",.2%"),
      gauge = list(
        axis =list(range = list(NULL, lim3), tickformat = ",.0%"),
        bar = list(color = "black"),
        steps = list(
          list(range = c(0, lim1), color = "#45B39D"),
          list(range = c(lim1, lim2), color = "#EB984E "),
          list(range = c(lim2, lim3), color = "#CD6155")
        ))) %>% 
      layout(autosize = T, margin = m, 
             xaxis = list(tickformat = "%", title=""),
             yaxis = list(tickformat = "%", title="")) %>% 
      config(displayModeBar=F)
    
  })
  output$DetalleGaugeProveedoresPeriodo <- renderUI({
    
    mes <- data_f() %>% filter(Item == "Proveedor con mayor concentración %") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "Proveedor con mayor concentración %") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "Proveedor con mayor concentración %") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',percent(mes_ant, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',percent(an_ant, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  ### Proceso ----
  
  output$GaugeDifSobreDespachos <- renderPlotly({
    
    mes <- data_f() %>% 
      filter(Item == "% Cafés Diferenciados y a la medida sobre total de despachos") %>% 
      summarise(sum(Valor)) %>% as.numeric()
    
    meta = 0.50
    
    lim1 = 0.40
    lim2 = 0.80
    lim3 = 1
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = mes *1,
      title = list(text = ""),
      type = "indicator",
      mode = "gauge+number",
      number = list(valueformat = ",.2%"),
      gauge = list(
        axis =list(range = list(NULL, lim3), tickformat = ",.0%"),
        bar = list(color = "black"),
        steps = list(
          list(range = c(lim2, lim3), color = "#45B39D"),
          list(range = c(lim1, lim2), color = "#EB984E "),
          list(range = c(0, lim1), color = "#CD6155")
        ))) %>% 
      layout(autosize = T, margin = m, 
             xaxis = list(tickformat = "%", title=""),
             yaxis = list(tickformat = "%", title="")) %>% 
      config(displayModeBar=F)
    
  })
  output$DetalleGaugeDifSobreDespachos <- renderUI({
    
    mes <- data_f() %>% filter(Item == "% Cafés Diferenciados y a la medida sobre total de despachos") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "% Cafés Diferenciados y a la medida sobre total de despachos") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "% Cafés Diferenciados y a la medida sobre total de despachos") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',percent(mes_ant, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',percent(an_ant, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  
  output$SerieKWXSaco <- renderPlotly({
    
    var_plot = "KW POR SACO"
    
    aux1 <- data_trilladora() %>%  filter(Item ==	var_plot, Fecha <= fecha_focal())
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    aux4 <- data_trilladora() %>%  
      filter(Item %in%	c("Presupuesto de Compras", "Compras realizadas"), 
             Fecha <= fecha_focal()) %>% 
      select(Item, Fecha, Valor) %>% 
      pivot_wider(names_from = Item , values_from = Valor) %>% 
      rename(Compras = `Compras realizadas`, Presupuesto = `Presupuesto de Compras`) %>% 
      mutate(Cumplimiento = Compras/Presupuesto)
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Compras",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(var_plot))," :", comma(aux2$Valor, accuracy = 0.01))
    ) %>% 
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", comma(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", comma(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Kilos (Forecast): ", comma(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(shapes = hline(3.2, color = "steelblue"),
             title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "s", 
                          title= list(text= "Compras (Kg)", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  output$GaugeKWXSaco <- renderPlotly({
    
    mes <- data_f() %>% 
      filter(Item == "KW POR SACO") %>% 
      summarise(sum(Valor)) %>% as.numeric()
    
    meta = 3.2
    
    lim1 = 2.5
    lim2 = 3.5
    lim3 = 4
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = mes *1,
      title = list(text = ""),
      type = "indicator",
      mode = "gauge+number",
      number = list(valueformat = ",.4"),
      gauge = list(
        axis =list(range = list(NULL, lim3), tickformat = ",.2"),
        bar = list(color = "black"),
        steps = list(
          list(range = c(0, lim1), color = "#45B39D"),
          list(range = c(lim1, lim2), color = "#EB984E "),
          list(range = c(lim2, lim3), color = "#CD6155")
        ))) %>% 
      layout(autosize = T, margin = m, 
             xaxis = list(tickformat = "%", title=""),
             yaxis = list(tickformat = "%", title="")) %>% 
      config(displayModeBar=F)
    
  })
  output$DetalleGaugeKWXSaco <- renderUI({
    
    mes <- data_f() %>% filter(Item == "KW POR SACO") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "KW POR SACO") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "KW POR SACO") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',comma(mes_ant, accuracy = 0.01),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',comma(an_ant, accuracy = 0.01),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  
  ### Calidad ----
  output$GaugeSupExcelsos <- renderPlotly({
    
    mes <- data_f() %>% 
      filter(Item == "% Supremos /Total excelsos") %>% 
      summarise(sum(Valor)) %>% as.numeric()
    
    meta = 0.50
    
    lim1 = 0.40
    lim2 = 0.80
    lim3 = 1
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = mes *1,
      title = list(text = ""),
      type = "indicator",
      mode = "gauge+number",
      number = list(valueformat = ",.2%"),
      gauge = list(
        axis =list(range = list(NULL, lim3), tickformat = ",.0%"),
        bar = list(color = "black"),
        steps = list(
          list(range = c(lim2, lim3), color = "#45B39D"),
          list(range = c(lim1, lim2), color = "#EB984E "),
          list(range = c(0, lim1), color = "#CD6155")
        ))) %>% 
      layout(autosize = T, margin = m, 
             xaxis = list(tickformat = "%", title=""),
             yaxis = list(tickformat = "%", title="")) %>% 
      config(displayModeBar=F)
    
  })
  output$DetalleGaugeSupExcelsos <- renderUI({
    
    mes <- data_f() %>% filter(Item == "% Supremos /Total excelsos") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "% Supremos /Total excelsos") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "% Supremos /Total excelsos") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',percent(mes_ant, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',percent(an_ant, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$SerieSupExcelsos <- renderPlotly({
    
    var_plot = "% Supremos /Total excelsos"
    
    aux1 <- data_trilladora() %>%  filter(Item ==	var_plot, Fecha <= fecha_focal())
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    aux4 <- data_trilladora() %>%  
      filter(Item %in%	c("Presupuesto de Compras", "Compras realizadas"), 
             Fecha <= fecha_focal()) %>% 
      select(Item, Fecha, Valor) %>% 
      pivot_wider(names_from = Item , values_from = Valor) %>% 
      rename(Compras = `Compras realizadas`, Presupuesto = `Presupuesto de Compras`) %>% 
      mutate(Cumplimiento = Compras/Presupuesto)
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Compras",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(var_plot))," :", comma(aux2$Valor, accuracy = 0.01))
    ) %>% 
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", comma(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", comma(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Kilos (Forecast): ", comma(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(shapes = hline(0.33, color = "steelblue"),
             title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = ",.0%", 
                          title= list(text= "Participacion de Supremos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  
  ### Operacion Logistica
  
  output$GaugeEficienciaFlete <- renderPlotly({
    
    mes <- data_f() %>% 
      filter(Item == "% Eficiencia en flete") %>% 
      summarise(sum(Valor)) %>% as.numeric()
    
    meta = 0.93
    
    lim1 = 0.93
    lim2 = 1.2
    lim3 = 1.5
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = mes *1,
      title = list(text = ""),
      type = "indicator",
      mode = "gauge+number",
      number = list(valueformat = ",.2%"),
      gauge = list(
        axis =list(range = list(NULL, lim3), tickformat = ",.0%"),
        bar = list(color = "black"),
        steps = list(
          list(range = c(lim2, lim3), color = "#45B39D"),
          list(range = c(lim1, lim2), color = "#EB984E "),
          list(range = c(0, lim1), color = "#CD6155")
        ))) %>% 
      layout(autosize = T, margin = m, 
             xaxis = list(tickformat = "%", title=""),
             yaxis = list(tickformat = "%", title="")) %>% 
      config(displayModeBar=F)
    
  })
  output$DetalleGaugeEficienciaFlete <- renderUI({
    
    mes <- data_f() %>% filter(Item == "% Eficiencia en flete") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "% Eficiencia en flete") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "% Eficiencia en flete") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',percent(mes_ant, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',percent(an_ant, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$SerieEficienciaFlete <- renderPlotly({
    
    var_plot = "% Eficiencia en flete"
    
    aux1 <- data_trilladora() %>%  filter(Item ==	var_plot, Fecha <= fecha_focal())
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    aux4 <- data_trilladora() %>%  
      filter(Item %in%	c("Presupuesto de Compras", "Compras realizadas"), 
             Fecha <= fecha_focal()) %>% 
      select(Item, Fecha, Valor) %>% 
      pivot_wider(names_from = Item , values_from = Valor) %>% 
      rename(Compras = `Compras realizadas`, Presupuesto = `Presupuesto de Compras`) %>% 
      mutate(Cumplimiento = Compras/Presupuesto)
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Compras",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(var_plot))," :", percent(aux2$Valor, accuracy = 0.01))
    ) %>% 
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", percent(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", percent(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Kilos (Forecast): ", percent(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(shapes = hline(0.93, color = "steelblue"),
             title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = ",.0%", 
                          title= list(text= "Eficiencia en flete", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  
  output$GaugeOpLogistica <- renderPlotly({
    
    mes <- data_f() %>% 
      filter(Item == "% Oportunidad Logística") %>% 
      summarise(sum(Valor)) %>% as.numeric()
    
    meta = 0.96
    
    lim1 = 0.93
    lim2 = 1.2
    lim3 = 1.5
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = mes *1,
      title = list(text = ""),
      type = "indicator",
      mode = "gauge+number",
      number = list(valueformat = ",.2%"),
      gauge = list(
        axis =list(range = list(NULL, lim3), tickformat = ",.0%"),
        bar = list(color = "black"),
        steps = list(
          list(range = c(lim2, lim3), color = "#45B39D"),
          list(range = c(lim1, lim2), color = "#EB984E "),
          list(range = c(0, lim1), color = "#CD6155")
        ))) %>% 
      layout(autosize = T, margin = m, 
             xaxis = list(tickformat = "%", title=""),
             yaxis = list(tickformat = "%", title="")) %>% 
      config(displayModeBar=F)
    
  })
  output$DetalleGaugeOpLogistica <- renderUI({
    
    mes <- data_f() %>% filter(Item == "% Oportunidad Logística") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "% Oportunidad Logística") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "% Oportunidad Logística") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',percent(mes_ant, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',percent(an_ant, accuracy = 0.1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$SerieOpLogistica <- renderPlotly({
    
    var_plot = "% Oportunidad Logística"
    
    aux1 <- data_trilladora() %>%  filter(Item ==	var_plot, Fecha <= fecha_focal())
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Compras",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(var_plot))," :", percent(aux2$Valor, accuracy = 0.01))
    ) %>% 
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", percent(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", percent(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Kilos (Forecast): ", percent(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(shapes = hline(0.96, color = "steelblue"),
             title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = ",.0%", 
                          title= list(text= "Oportunidad Logística", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  
  ### Talento Humanos ----
  
  output$GaugeMovXSaco <- renderPlotly({
    
    mes <- data_f() %>% 
      filter(Item == "Costo de movilización por saco") %>% 
      summarise(sum(Valor)) %>% as.numeric() %>% round()
    
    meta = 1300
    
    lim1 = 1300
    lim2 = 1500
    lim3 = 2000
    
    fig <- plot_ly(
      domain = list(x = c(0, 3), y = c(0, 1)),
      value = mes ,
      title = list(text = ""),
      type = "indicator",
      mode = "gauge+number",
      number = list(valueformat = "$,"),
      gauge = list(
        axis =list(range = list(NULL, lim3), tickformat = "$,"),
        bar = list(color = "black"),
        steps = list(
          list(range = c(0, lim1), color = "#45B39D"),
          list(range = c(lim1, lim2), color = "#EB984E "),
          list(range = c(lim2, lim3), color = "#CD6155")
        ))) %>% 
      layout(autosize = T, margin = m, 
             xaxis = list(tickformat = "%", title=""),
             yaxis = list(tickformat = "%", title="")) %>% 
      config(displayModeBar=F)
    
  })
  output$DetalleGaugeMovXSaco <- renderUI({
    
    mes <- data_f() %>% filter(Item == "Costo de movilización por saco") %>% summarise(sum(Valor)) %>% as.numeric()
    
    mes_ant <- data_per_ant_f() %>% filter(Item == "Costo de movilización por saco") %>% summarise(sum(Valor)) %>% as.numeric()
    tmm <- (mes/mes_ant) -1
    
    an_ant <- data_an_ant_f() %>% filter(Item == "Costo de movilización por saco") %>% summarise(sum(Valor)) %>% as.numeric()
    tam <- (mes/an_ant) -1
    
    per_mes <- ifelse(length(input$Fecha) == 1, format(max(data_per_ant_f()$Fecha), "%b%y"),
                      paste(format(min(data_per_ant_f()$Fecha), "%b%y"), "a", format(max(data_per_ant_f()$Fecha), "%b%Y")))
    per_an <- ifelse(length(input$Fecha) == 1, format(max(data_an_ant_f()$Fecha), "%b%y"),
                     paste(format(min(data_an_ant_f()$Fecha), "%b%y"), "a", format(max(data_an_ant_f()$Fecha), "%b%Y")))
    
    
    out <- paste0('<p style="text-align: center;">',
                  '<br><strong><span style="font-size: 16px;"> Periodo anterior (',per_mes,'): </span></strong>',dollar(mes_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tmm, F),';">&nbsp','', chr_kpi(tmm), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tmm, accuracy = 0.1),'</span></strong>',
                  '<br><strong><span style="font-size: 16px;"> Periodo año ant. (',per_an,'): </span></strong>',dollar(an_ant, accuracy = 1),'</span></strong>',
                  '<strong><span style="font-size: 16px;color:',col_kpi(tam, F),';">&nbsp', chr_kpi(tam), '</span></strong>',
                  '<strong><span style="font-size: 16px;">  ', percent(tam, accuracy = 0.1),'</span></strong>','</p>')
    
    HTML(out)
    
    
  })
  output$SerieMovXSaco <- renderPlotly({
    
    var_plot = "Costo de movilización por saco"
    
    aux1 <- data_trilladora() %>%  filter(Item ==	var_plot, Fecha <= fecha_focal())
    
    m <- aux1 %>% 
      select(ds = Fecha, y = Valor) %>% 
      prophet(interval.width = 0.6)
    
    future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)        
    forecast <- predict(m, future)
    
    aux2 <- aux1 %>% 
      full_join(forecast, by=c("Fecha"="ds"))
    
    aux3 <- aux2 %>% 
      filter(is.na(Valor))
    
    plot_ly(data= aux2, x = aux2$Fecha , y= aux2$Valor, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Compras",
            textposition = 'bottom', text = ~comma(aux2$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux2$Fecha, "%B %Y")), "</b>",
                               "<br>",str_to_sentence(trimws(var_plot))," :", percent(aux2$Valor, accuracy = 0.01))
    ) %>% 
      add_ribbons(data= aux3 , x = ~Fecha, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  color = I("#AAB7B8"), name = "IC", inherit = F, hoverinfo = "text", 
                  hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                     "<br>Limite Inferior IC: ", percent(aux3$yhat_lower, accuracy = 1),
                                     "<br>Limite Superior IC: ", percent(aux3$yhat_upper, accuracy = 1)
                  )) %>% 
      add_lines(data= aux3, x = ~Fecha, y= ~yhat, color = I("#AAB7B8"), 
                name = "Forecast", mode = 'lines', inherit = F, hoverinfo = "text", hoverlabel = list(align = "left"),
                hovertext = paste0("<b>", str_to_title(format(aux3$Fecha, "%B %Y")), "</b>",
                                   "<br>Kilos (Forecast): ", percent(aux3$yhat, accuracy = 1)
                )) %>% 
      layout(shapes = hline(1300, color = "steelblue"),
             title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,", 
                          title= list(text= "Costo de movilización por saco", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  
  output$BarrasPersonal <- renderPlotly({
    
    aux1 <- data_f() %>% 
      mutate(Auxiliar = "Personal") %>% 
      filter(Item %in% c("Planta de personal Total", "Movilizadores")) 
    
    plot_ly(data = aux1, x=~Auxiliar, y=~Valor, color = ~Item, type = "bar", 
            colors = c("Planta de personal Total"="#2471A3", "Movilizadores"="#34495E"),
            textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", "Mes Vigente", "</b>",
                               "<br>",str_to_sentence(trimws(aux1$Item))," :", comma(aux1$Valor, accuracy = 1))) %>% 
      layout(barmode = "group",
             title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = ",", 
                          title=list(text="Personal", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.05, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)  
    
  })
  output$SerieHorasExtras <- renderPlotly({
    
    aux1 <- data_trilladora() %>%  
      filter(Item %in%	c("Horas Extras", "Horas Extras - Cantidad empleados", "Costo $ horas extras"), Fecha <= fecha_focal()) %>% 
      mutate(Item = case_when(Item == "Horas Extras" ~ "Horas",
                              Item == "Horas Extras - Cantidad empleados" ~ "Empleados",
                              Item == "Costo $ horas extras" ~ "Costo")
      ) %>% 
      pivot_wider(id_cols = Fecha, names_from = Item, values_from = Valor) %>% 
      mutate(Costo = Costo*1000)
    
    plot_ly(data= aux1, x = aux1$Fecha , y= aux1$Costo, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Costo Horas Extras",
            textposition = 'bottom', text = ~comma(aux1$Costo, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
                               "<br>","Número de Horas Extras :", comma(aux1$Horas, accuracy = 1),
                               "<br>","Cantidad de Empleados :", comma(aux1$Empleados, accuracy = 1),
                               "<br>","Costo Horas Extras :", dollar(aux1$Costo, accuracy = 1)
            )
    ) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = "$,.2s", 
                          title= list(text= "Costo Horas Extras", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })
  output$VB_HorasExtras <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "Horas Extras") %>% summarise(sum(Valor)) %>% as.numeric()
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "Horas Extras") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "Horas Extras") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() 
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(comma(aux1, accuracy = 1), "")),
      title = "Horas Extras:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", comma(aux2), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", comma(aux3), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("calendar"),
      fill = T,
      color = "teal"
    )
  }) 
  output$VB_HorasExtrasCostos <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "Costo $ horas extras") %>% summarise(sum(Valor)) %>% as.numeric() *1000
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "Costo $ horas extras") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()*1000
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "Costo $ horas extras") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() *1000
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(dollar(aux1, accuracy = 1), "")),
      title = "Costo de Horas Extras:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", dollar(aux2), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", dollar(aux3), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("dollar-sign"),
      fill = T,
      color = "teal"
    )
  }) 
  
  output$VB_NumAccidentes <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "Numero de accidentes") %>% summarise(sum(Valor)) %>% as.numeric()
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "Numero de accidentes") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "Numero de accidentes") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() 
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(comma(aux1, accuracy = 1), "")),
      title = "Número de Accidentes:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", comma(aux2), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", comma(aux3), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("user-injured"),
      fill = T,
      color = "teal"
    )
  }) 
  output$VB_DiasAusencia <- renderValueBox({
    
    aux1 <- data_f() %>% filter(Item == "Cantidad en días de ausencia - accidentes") %>% summarise(sum(Valor)) %>% as.numeric()
    aux2 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal()), 
                                         month(Fecha) <= month(fecha_focal()),
                                         Item == "Cantidad en días de ausencia - accidentes") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric()
    
    aux3 <- data_trilladora() %>% filter(year(Fecha) == year(fecha_focal() - years(1)) , month(Fecha) <= month(fecha_focal()),
                                         Item == "Cantidad en días de ausencia - accidentes") %>% summarise(sum(Valor, na.rm = T)) %>% as.numeric() 
    
    variacion <- (aux2/aux3)-1
    
    infoBox(
      value = tags$p(style = "font-size: 24px;", paste(comma(aux1, accuracy = 1), "")),
      title = "Número de dias de Ausencia:",
      subtitle = HTML(paste("Acumulado a", format(fecha_focal(), "%b %Y"), ":", comma(aux2), br(), 
                            "Acumulado a", format(fecha_focal() - years(1) , "%b %Y"), ":", comma(aux3), br(),
                            "Variación", chr_kpi(variacion), percent(variacion, accuracy = 0.1))),
      icon = icon("calendar"),
      fill = T,
      color = "teal"
    )
  }) 
  output$SerieAccidentes <- renderPlotly({
    
    aux1 <- data_trilladora() %>%  
      filter(Item %in%	c("Numero de accidentes", "Cantidad en días de ausencia - accidentes"), Fecha <= fecha_focal()) %>% 
      mutate(Item = case_when(Item == "Numero de accidentes" ~ "Accidentes",
                              Item == "Cantidad en días de ausencia - accidentes" ~ "Ausencias")
      ) %>% 
      pivot_wider(id_cols = Fecha, names_from = Item, values_from = Valor) 
    
    plot_ly(data= aux1, x = aux1$Fecha , y= aux1$Accidentes, type = "scatter", mode="lines+markers",
            line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Accidentes",
            textposition = 'bottom', text = ~comma(aux1$Accidentes, accuracy = 1), 
            hoverlabel = list(align = "left"), hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
                               "<br>","Accidentes :", comma(aux1$Accidentes, accuracy = 1),
                               "<br>","Días de Ausencia por Accidentes :", comma(aux1$Ausencias, accuracy = 1)
            )
    ) %>% 
      layout(title = list(text="", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(gridcolor="#CCD1D1", 
                          title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             yaxis = list(tickformat = ",", 
                          title= list(text= "Accidentes", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22, 
                           font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>% 
      config(displayModeBar=F)
    
    
  })

}
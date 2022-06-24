function(input, output, session)
{
  ##Datos Reactivos ----
  data_informe_f <- reactive({
    
    tabla_informe %>% 
      filter(Fecha_Factura>=input$Fecha_Factura[1], Fecha_Factura<=input$Fecha_Factura[2],
             Sucursal %in% input$Sucursal)
  })
  
  #Informe Comercial ----
  
  ####Título----
  output$TextoEntradas<-renderUI({
    h3(paste("Entradas Facturadas entre el",
             format(input$Fecha_Factura[1], "%d de %B del %Y"), "y el",
             format(input$Fecha_Factura[2], "%d de %B del %Y")))
  })
  
  ####Calidades ----
  output$pie_Calidades<-renderPlotly({
    
    aux1<- data_informe_f() %>%
      mutate(Calidad = case_when(
        Calidad == "PERGAMINO SECO"~"PERGAMINO",
        Calidad == "EXCELSO" ~ "EXCELSO",
        Calidad == "CONSUMO" | Calidad == "MOLIDOS" |Calidad == "SOLUBLE"| Calidad == "RIPIO" ~ "COPRODUCTO"),
        Calidad = ifelse(is.na(Calidad),"DIFERENCIADOS",Calidad)) %>% 
      group_by(Calidad = Calidad) %>%
      summarise(Participacion = sum(Kilos_Netos, na.rm = T)/sum(data_informe_f()$Kilos_Netos, na.rm = T))
    
    colors = colorRampPalette(brewer.pal(4, "Blues"))(100)[100*aux1$Participacion]
    
    plot_ly(data= aux1, labels = ~ Calidad , values= ~Participacion, 
            marker =list(colors=colors), type = "pie",
            textposition='', textinfo='percent', hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(aux1$Calidad), "</b>",
                               "<br>Participación: ", percent(aux1$Participacion, accuracy = 0.1))) %>% 
      layout(title = "Kilos Netos por Calidad",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             showlegend = F)  %>%
      config(displayModeBar=F)
    
  })
  
  output$tb_Calidades<-DT::renderDataTable({
    
    aux1 <- data_informe_f() %>%
      group_by(Calidad = Calidad) %>%
      summarise(`Kilos Brutos` = sum(Kilos_Brutos, na.rm = T),
                `Kilos Netos` = sum(Kilos_Netos, na.rm = T),
                `Promedio Factor Rto`= mean(PorRendimiento[PorRendimiento<=100]),
                `Valor Compra` =sum(Valor_Compra, na.rm = T),
                `Número Entradas` = n()) %>%
      arrange(desc(`Kilos Netos`)) %>%
      mutate(`Kilos Brutos` = comma(`Kilos Brutos`, accuracy = 1),
             `Kilos Netos` = comma(`Kilos Netos`, accuracy = 1),
             `Promedio Factor Rto`= comma(`Promedio Factor Rto`, accuracy = 0.1),
             `Valor Compra` =dollar(`Valor Compra`, accuracy = 1),
             `Número Entradas` = comma(`Número Entradas`, accuracy = 1))
    
  datatable(aux1, options=list(pageLength =12, dom = 'tp', searching= T, scrollX = TRUE, 
                                 scrollY = "440px"), rownames=F, 
              filter = list(position = 'none', clear = FALSE))}
  )
  
  output$pie_Proveedor<-renderPlotly({
    
    aux1<- data_informe_f() %>% 
      group_by(`Clase Proveedor` = Clase_Proveedor) %>% 
      summarise(Participacion = sum(Kilos_Netos, na.rm = T)/sum(data_informe_f()$Kilos_Netos, na.rm = T))
    
    colors = colorRampPalette(brewer.pal(9, "Blues"))(100)[100*aux1$Participacion]
    
    plot_ly(data= aux1, labels = ~`Clase Proveedor` , values= ~Participacion, 
            marker = list(colors=colors), type = "pie",
            textposition='', textinfo='percent', hoverinfo = "text",
            hovertext = paste0("<b>", str_to_title(aux1$`Clase Proveedor`), "</b>",
                               "<br>Participación: ", percent(aux1$Participacion, accuracy = 0.1))) %>% 
      layout(title = "Kilos Netos por Clase Proveedor",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             showlegend = F)  %>%
      config(displayModeBar=F)
    
  })
  
  output$tb_Proveedor<-DT::renderDataTable({
    aux1 <- data_informe_f() %>% 
      mutate(Proveedor = ifelse(is.na(Asociado), Proveedor, Asociado)) %>% 
      group_by(Proveedor,
               Departamento = Departamento_Proveedor,
               Ciudad = Ciudad_Proveedor) %>% 
      summarise(`Kilos Brutos` = sum(Kilos_Brutos, na.rm = T),
                `Kilos Netos` = sum(Kilos_Netos, na.rm = T),
                `Promedio Factor Rto`= mean(PorRendimiento[PorRendimiento<=100]),
                `Valor Compra` = sum(Valor_Compra, na.rm = T),
                `Número Entradas` = n()) %>% 
      arrange(desc(`Kilos Netos`)) %>% 
      head(., 15) %>% 
      mutate(`Kilos Brutos` = comma(`Kilos Brutos`, accuracy = 1),
             `Kilos Netos` = comma(`Kilos Netos`, accuracy = 1),
             `Promedio Factor Rto`= comma(`Promedio Factor Rto`, accuracy = 0.1),
             `Valor Compra` =dollar(`Valor Compra`, accuracy = 1),
             `Número Entradas` = comma(`Número Entradas`, accuracy = 1))
    
    datatable(aux1, options=list(pageLength =12, dom = 'tp', searching= T, scrollX = TRUE, 
                                 scrollY = "440px"), rownames=F, 
              filter = list(position = 'none', clear = FALSE))
    })
  
  output$gra_Ubicacion<-renderPlotly({
    
  })
  
  output$tb_Ubicacion<-DT::renderDataTable({
    
    aux1 <- data_informe_f() %>% 
      group_by(Departamento, Ciudad) %>% 
      summarise(`Kilos Brutos` = sum(Kilos_Brutos, na.rm = T),
                `Kilos Netos` = sum(Kilos_Netos, na.rm = T),
                `Promedio Factor Rto`= mean(PorRendimiento[PorRendimiento<=100]),
                `Valor Compra` = sum(Valor_Compra, na.rm = T),
                `Número Entradas` = n()) %>% 
      arrange(desc(`Kilos Netos`)) %>% 
      mutate(`Kilos Brutos` = comma(`Kilos Brutos`, accuracy = 1),
             `Kilos Netos` = comma(`Kilos Netos`, accuracy = 1),
             `Promedio Factor Rto`= comma(`Promedio Factor Rto`, accuracy = 0.1),
             `Valor Compra` =dollar(`Valor Compra`, accuracy = 1),
             `Número Entradas` = comma(`Número Entradas`, accuracy = 1))
    
    datatable(aux1, options=list(pageLength =12, dom = 'tp', searching= T, scrollX = TRUE, 
                                 scrollY = "440px"), rownames=F, 
              filter = list(position = 'none', clear = FALSE))
})
  
  output$tb_Proveedor2 <- DT::renderDataTable({
    aux1 <- data_informe_f() %>% 
      mutate(Proveedor = ifelse(is.na(Asociado), Proveedor, Asociado)) %>% 
      group_by(Proveedor,
               Departamento = Departamento_Proveedor,
               Ciudad = Ciudad_Proveedor) %>% 
      summarise(`Kilos Brutos` = sum(Kilos_Brutos, na.rm = T),
                `Kilos Netos` = sum(Kilos_Netos, na.rm = T),
                `Promedio Factor Rto`= mean(PorRendimiento[PorRendimiento<=100]),
                `Valor Compra` = sum(Valor_Compra, na.rm = T),
                `Número Entradas` = n()) %>% 
      arrange(desc(`Kilos Netos`)) %>% 
      mutate(`Kilos Brutos` = comma(`Kilos Brutos`, accuracy = 1),
             `Kilos Netos` = comma(`Kilos Netos`, accuracy = 1),
             `Promedio Factor Rto`= comma(`Promedio Factor Rto`, accuracy = 0.1),
             `Valor Compra` =dollar(`Valor Compra`, accuracy = 1),
             `Número Entradas` = comma(`Número Entradas`, accuracy = 1))
    
    datatable(aux1, options=list(pageLength =12, dom = 'tp', searching= T, scrollX = TRUE, 
                                 scrollY = "440px"), rownames=F, 
              filter = list(position = 'none', clear = FALSE))
    })
  
  
  
}
function(input, output, session){
  output$FechaAnalisis <- renderUI({
    h4(paste("Posición de Dólares y Café al", format(as.Date(fecha_consulta, '%Y-%m-%d'),'%d de %B del %Y')
             ))
    
  })
  
  ### Sacos por fijar ----
  output$VB_SacosXFijar<- renderValueBox({
    aux1 <- sum(cafexasignar$Total_Sacos)
    color<- case_when(aux1>limites_cafe$SUPERIOR ~"yellow",
                      aux1<limites_cafe$INFERIOR~"yellow",
                      TRUE ~ "black")
      
    apputils::infoBox(
      title = "Sacos por Fijar",
      value = comma(aux1, accuracy = 0.01), 
      subtitle = "Sacos de cafe",
      icon = ico_saco,
      color = color,
      fill = T
      )
    })
  
  output$DetalleSacosXFijar <- renderTable({
    aux1 <- cafexasignar%>% 
      mutate(Total_Sacos = comma(Total_Sacos, accuracy = 1)) %>% 
      mutate_at(c("PrecioxCarga", "PrecioCarga", "DiferenciaPrecio", "DiferenciaHoy"), dollar) %>% 
      rename('Tipo de Negocio' = Tipo_Negocio,
             'Sacos' = Total_Sacos,
             'Precio por Carga' = PrecioxCarga,
             'Promedio Ultima Compra' = PrecioCarga,
             'Diferencia de Precio' = DiferenciaPrecio,
             'Diferencia a Hoy' = DiferenciaHoy)
      }, spacing = 'xs', width = "100%", 
    height="100%", digits = 0, na ="")
  
  ## Cuenta Cafe----
  
  #output$Ajustes_Cafe <- renderRHandsontable({})
  output$Cuenta_Cafe <- renderTable({
    aux1 <- cuenta_cafe}, 
    spacing = 'xs', width = "100%", height="100%", digits = 1, na ="")
   
  output$Ajustes_Cafe<- renderTable({
    aux1 <- Ajustes_Cafe %>% 
      mutate_at(c("Detalle"), dollar)},
    spacing = 'xs', width = "100%", height="100%", digits = 1, na ="")
  
  
  output$VB_VerificacionCafe<- renderValueBox({
    aux1  <- as.numeric(cuenta_cafe1$`Diferencia respecto al calculado`)
    color <- ifelse( abs(aux1) < 1,  "black", "yellow") %>% as.character()
    icono <- ifelse( abs(aux1) < 1,  "check", "exclamation-circle") %>% as.character()
    
    apputils::infoBox(
      title = "Sacos de Diferencia",
      value = paste(comma(aux1, accuracy = 0.01)), 
      subtitle = "Sacos de Café",
      icon = icon(icono),
      color = color,
      fill = T
    )
    
    })
  
  


  ###Dolares por Fijar ----
  output$VB_SaldoDLRS <- renderValueBox({
    aux1 <- total_dolares
    aux1 <- ifelse(is.na(aux1), 0, aux1)
    color<- case_when(aux1>limites_dolares$SUPERIOR ~"yellow",
                      aux1<limites_dolares$INFERIOR~"yellow",
                      TRUE ~ "black")
    
    apputils::infoBox(
      title = "DÓLARES POR ASIGNAR",
      value = comma(aux1, accuracy = 0.01), 
      subtitle = "Dólares",
      icon = icon("comment-dollar"),
      color = color,
      fill = T
    )

  })
  
  output$VB_TasaPromedio <- renderValueBox({
    aux1 <- TRM_Promedio
    aux1 <- ifelse(is.na(aux1), 0, aux1)
    
    apputils::infoBox(
      title = "TRM PROMEDIO",
      value = comma(aux1, accuracy = 0.01), 
      subtitle = "TRM",
      icon = icon("dollar-sign"),
      color = "black",
      fill = T
    )
  })

  output$DetalleDolaresXAsignar <- renderTable({
    aux1 <- dolaresxAsignar %>% 
      select(-DxAFch) %>% 
      mutate_at(c("DxADolar", "DxAFrwTasa"), dollar) %>% 
      mutate(DxAFrwFec = format(DxAFrwFec, "%d/%m/%Y")) %>% 
      rename('Pedido/Forward' = FrwNum,
             'Cliente' = DxAFrwOpe,
             'Saldo USD' = DxADolar,
             'Fecha Short' = DxAFrwFec,
             'Tasa'= DxAFrwTasa)}, spacing = 'xs', width = "100%", height = "100%", digits = 0, na = "")
  
  output$FijadoxComprar <- renderTable({
    aux1 <- dolares_faltantes %>%
      mutate_at(c("Dolares_Faltantes", "Tasa_Promedio"), dollar) %>% 
      rename("Pedido/Forward" = Codigo_Pedido,
            "Dolares Faltantes" = Dolares_Faltantes,
            "Tasa Promedio" = Tasa_Promedio)
  })
  

  output$Cuenta_Dolares <- renderTable({
    aux1 <- cuenta_dolares})
  
  output$Ajustes_Dolares <- renderTable({
    aux1 <- Ajustes_Dolares %>% 
      mutate_at(c("Detalle"), dollar)},
    spacing = 'xs', width = "100%", height="100%", digits = 1, na ="")
  
  output$VB_VerificacionDolares <- renderValueBox({
    aux1  <- as.numeric(diferencia_dolares)
    color <- ifelse( abs(aux1) < 1,  "black", "yellow") %>% as.character()
    icono <- ifelse( abs(aux1) < 1,  "check", "exclamation-circle") %>% as.character()
    
    apputils::infoBox(
      title = "Dólares de Diferencia",
      value = paste(comma(aux1, accuracy = 0.01)), 
      subtitle = "Dólares",
      icon = icon(icono),
      color = color,
      fill = T
    )
  })

  
  
  
  RangoDias <- reactiveVal()
  observeEvent(event_data("plotly_click", source = "BarrasContratos"), {
    RangoDias(event_data("plotly_click", source = "BarrasContratos")$x)
  })
  
  
  output$BarrasContratos <- renderPlotly({
    
    if (is.null(RangoDias())) {
      aux1 <- contratos %>% 
        group_by(MesEmbarque = floor_date(FechaEmbarque, unit = "month")) %>% 
        summarise(Contratos = n(), Sacos =  sum(Sacos)) %>% 
        mutate(current_color = "#566573")
    } else {
      aux1 <- contratos %>% 
        group_by(MesEmbarque = floor_date(FechaEmbarque, unit = "month")) %>% 
        summarise(Contratos = n(), Sacos =  sum(Sacos)) %>% 
        mutate(current_color = if_else(MesEmbarque == RangoDias(), "#AAB7B8", "#566573"))
    }
    
    plot_ly(aux1, x=~MesEmbarque, y=~Contratos, type="bar", hoverinfo = "text", source = "BarrasContratos",
            marker = list(color = ~current_color, line = list(color = "#17202A", width = 1.5)),
            hovertext = paste0("<b>", str_to_sentence(format(aux1$MesEmbarque, "%B del %Y")), "</b>",
                               "<br>Numero de Contratos: ", comma(aux1$Contratos, accuracy = 1),
                               "<br>Numero de Sacos: ", comma(aux1$Sacos, accuracy = 1))) %>% 
      layout(title = list(text="Distribucion Contratos por Mes de Embarque", 
                          font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
             xaxis = list(tickformat = "%b <br>%Y",
                          title=list(text="Mes de Embarque", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
                          tickfont= list(family = "Arial, sans-serif",size = 10,color = "#17202A"),
                          dtick = "M1"
                          ),
             yaxis = list(tickformat = ",", 
                          title=list(text="Contratos", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")), 
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
             ),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)') %>% 
      config(displayModeBar=F) %>%
      event_register("plotly_click")
    
  })
  
  
  
  output$DetalleContratos <- DT::renderDataTable({
    if(is.null(RangoDias())) {
      aux1 <-contratos %>% 
        arrange(desc(Sacos)) %>% 
        rename("FECHA EMBARQUE" = FechaEmbarque, 
               "CODIGO PEDIDO" = Codigo_Pedido,
               "DIFERENCIAL VENTA" = Diferencial_Venta,
               "EXTRA COSTO" = Extra_Costo,
               "DIFERENCIAL NETO" = Diferencial_Neto)
    } else{
      aux1 <- contratos %>% 
        filter(floor_date(FechaEmbarque, unit = "month") == RangoDias()) %>% 
        arrange(desc(Sacos)) %>% 
        rename("FECHA EMBARQUE" = FechaEmbarque, 
               "CODIGO PEDIDO" = Codigo_Pedido,
               "DIFERENCIAL VENTA" = Diferencial_Venta,
               "EXTRA COSTO" = Extra_Costo,
               "DIFERENCIAL NETO" = Diferencial_Neto)
    }
    
    datatable(aux1, options=list(pageLength =12, dom = 'tp', searching= T, scrollX = TRUE, 
                                 scrollY = "440px"), rownames=F,
              filter = list(position = 'none', clear = FALSE)) 
    
  })
  
  output$VB_ContratosXFijar<- renderValueBox({
      aux1 <- contratos %>% summarise(sum(Sacos)) %>% as.numeric()
    
      apputils::infoBox(
      title = "NÚMERO SACOS CONTRATOS POR FIJAR",
      value = comma(aux1, accuracy = 0.01), 
      subtitle = "Sacos",
      icon = icon("file-contract"),
      color = "black",
      fill = T
    )
  })
  
  
  output$DetalleCMes <- renderTable({
    aux1 <- resumen_contratos %>% 
      mutate(FechaEmbarque = format(as.Date(FechaEmbarque, '%Y-%m-%d')),
             Total_Sacos = comma(Total_Sacos, accuracy = 0.01)) %>%
      mutate_at(c("Total_DV", "Total_EC", "Total_DN"), dollar) %>% 
      rename("Mes Embarque" = FechaEmbarque, 
             "Sacos" = Total_Sacos,
             "Diferencial Venta" = Total_DV,
             "Extracostos" = Total_EC,
             "Diferencial Neto" = Total_DN)
  })
  
  output$ResumenContratos <- renderTable({
    aux1 <- saldo_contratos %>% 
      mutate(Valor = comma(Valor, accuracy = 1))

  })
  
  output$VB_Corto3m<- renderValueBox({
    aux1 <- corto_3m
    
    apputils::infoBox(
      title = "CORTO ÚLTIMOS 3 MESES",
      value = comma(aux1, accuracy = 0.01), 
      subtitle = "Sacos",
      icon = ico_saco,
      color = "black",
      fill = T
    )
    })
    
    
    output$VB_pCorto3m <- renderValueBox(
      { aux1 <- pcorto_3m
      
      apputils::infoBox(
        title = "% CORTO ÚLTIMOS 3 MESES",
        value = aux1,
        color = "black",
        fill = T)
      })
  
    ## Saldo Diferencial + KKMM----
    
    output$SaldoDiferencial <- renderTable({
      aux1 <- Saldo_Diferencial %>% 
        mutate_at(c("US$", "TASA PROM CONTABILIDAD NIIF", "PESOS CONTABILIDAD NIIF"), dollar)
    })
    
    output$SaldoDiferencial1 <- renderTable({
      aux1 <- Saldo_Diferencial1 %>% 
        mutate_at(c("US$"), dollar)
    })
    
    
    output$VB_SubtotalSldoD <- renderValueBox({
      aux1 <- Subtotal_SldoD
      
      apputils::infoBox(
        title = "",
        value = comma(aux1, accuracy = 0.01), 
        subtitle = "Saldo Diferencial Dólares",
        icon = icon("comment-dollar"),
        color = "black",
        fill = T
      )
      })
    
    
    output$VB_SubtotalSldoDTRM <- renderValueBox({
      aux1 <- Subtotal_SldoDTRM
      
        apputils::infoBox(
        title = "",
        value = comma(aux1, accuracy = 0.01), 
        subtitle = "Saldo Diferencial TRM",
        icon = icon("dollar-sign"),
        color = "black",
        fill = T
      )
    })
      
      output$VB_SubtotalSldoDPe <- renderValueBox({
        aux1 <- Subtotal_SldoD*Subtotal_SldoDTRM
        
          apputils::infoBox(
          title = "",
          value = comma(aux1, accuracy = 0.01), 
          subtitle = "Pesos Contabilidad NIIF",
          icon = icon("dollar-sign"),
          color = "black",
          fill = T
        )
        
      })
      
      output$VB_TotalSldoD <- renderValueBox({
        aux1 <- SdloD_Total
        
        apputils::infoBox(
          title = "Total",
          value = comma(aux1, accuracy = 0.01), 
          subtitle = "Diferencial+KKMM Dólares",
          icon = icon("comment-dollar"),
          color = "blue",
          fill = T
        )
        
        
      })
      
      
      ##Capital Cuenta Futuros ----
      output$Capital_CtaF <- renderTable({
        aux1 <- Cta_Futuros %>% 
          mutate_at(c("US$", "PESOS", "TASA"), dollar)
        
      })
      
      output$VB_saldoCAdm <- renderValueBox({
        
        aux1 <- Saldo_CtaCAdm
        color <- ifelse( aux1 <limite_Comisionista,  "black", "yellow") %>% as.character()
        icono <- ifelse( aux1 <limite_Comisionista,  "check", "exclamation-circle") %>% as.character()
        
          apputils::infoBox(
          title = "Saldo en ADM",
          value = comma(aux1, accuracy = 0.01), 
          subtitle = "Dólares",
          icon = icon(icono),
          color = color,
          fill = T)
        
      })
      
      
      output$VB_SaldoCStonex <- renderValueBox({
        aux1 <- Saldo_CtaCStonex
        color <- ifelse( aux1 <limite_Comisionista,  "black", "yellow") %>% as.character()
        icono <- ifelse( aux1 <limite_Comisionista,  "check", "exclamation-circle") %>% as.character()
        
        apputils::infoBox(
          title = "Saldo en STONEX",
          value = comma(aux1, accuracy = 0.01), 
          subtitle = "Dólares",
          icon = icon(icono),
          color = color,
          fill = T)
        
      })
      
      
      
      output$VB_SubCapitalCtaF<- renderValueBox({
      aux1 <- Subtotal_CtaF
        
        apputils::infoBox(
        title = "",
        value = comma(aux1, accuracy = 0.01), 
        subtitle = "Total Dólares",
        icon = icon("comment-dollar"),
        color = "black",
        fill = T)
      
          })
    
    output$VB_SubCapitalCtaFPesos <- renderValueBox({
      aux1 <- Subtotal_CtaFPe
      
      
        apputils::infoBox(
        title = "",
        value = comma(aux1, accuracy = 0.01), 
        subtitle = "Total Pesos",
        icon = icon("dollar-sign"),
        color = "black",
        fill = T
      )
      
    })
    
    
    output$VB_SubCapitalCtaFTRM <- renderValueBox({
      aux1 <- Subtotal_CtaFTRM
      
        apputils::infoBox(
        title = "",
        value = comma(aux1, accuracy = 0.01), 
        subtitle = "TRM Promedio",
        icon = icon("dollar-sign"),
        color = "black",
        fill = T
      )
    })
    
    
    ## Saldo Cuenta Compensación ----
    
    output$Cuenta_Compensacion <- renderTable({
      aux1 <- Saldo_Cta_Compensacion %>% 
        mutate_at(c("SALDOS USD", "ENTREGA"), dollar)
    })
    
    
    output$VB_SldoDisPref <- renderValueBox({
      aux1 <- Sldo_DPref
      
      
        apputils::infoBox(
        title = "Saldo Disponible Pref",
        value = comma(aux1, accuracy = 0.01), 
        subtitle = "Dólares",
        icon = icon("comment-dollar"),
        color = "black",
        fill = T
      )
    })
    
    output$VB_Total_CtaCompensacion <- renderValueBox({
      aux1 <- Total_Cta_Comp
      
        apputils::infoBox(
        title = "Total cta Compensación",
        value = comma(aux1, accuracy = 0.01), 
        subtitle = "Dólares",
        icon = icon("comment-dollar"),
        color = "blue",
        fill = T
      )
    })
    

  
  
}

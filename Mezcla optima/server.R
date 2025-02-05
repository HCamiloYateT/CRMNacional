function(input, output, session) {
  
  ### Filtros de base de datos ----
  espec_f <- reactive({
    espec %>% 
      filter(Nombre==input$Prep)
  })
  
  ### Ouput de Filtros -----
  output$Nombre <- renderText({
    espec_f() %>% 
      select(Nombre) %>% 
      unique() %>% 
      as.character()
  })
  output$Especs <- renderText({
    espec_f() %>% 
      select(Especificacion) %>% 
      unique() %>% 
      as.character()
  })
  output$Faltas <- renderText({
    espec_f() %>% 
      select(Faltas) %>% 
      unique() %>% 
      as.character()
  })
  
  ### Captura de inputs ----
  costos <- reactive({
    c(input$C_Malla13, input$C_Malla14, input$C_Malla15, input$C_Malla16, input$C_Malla17, input$C_Malla18)
  })
  
  inventario <- reactive({
    c(input$I_Malla13, input$I_Malla14, input$I_Malla15, input$I_Malla16, input$I_Malla17, input$I_Malla18)
  })
  
  fallas <- reactive({
    c(input$F_Malla13, input$F_Malla14, input$F_Malla15, input$F_Malla16, input$F_Malla17, input$F_Malla18)
  })
  
  ### Solver ----
  
  optimo <- reactive({
    
    k = input$Kilos
    
    dire <- espec_f() %>% 
      select(Cond) %>% 
      as.matrix() %>% 
      t() %>% 
      as.vector()
    
    derecha <-  espec_f() %>% 
      select(Pct) %>% 
      as.matrix() %>% 
      t() %>% 
      as.vector()
    
    # Restricciones ----
    cumplimiento <- c(1,1,1,1,1,1)
    
    preparacion <- espec_f() %>%  
      select(M13:M18) %>% 
      as.matrix() %>% 
      t() %>% 
      as.vector()
    
    inventarios <- c(1,0,0,0,0,0,
                     0,1,0,0,0,0,
                     0,0,1,0,0,0,
                     0,0,0,1,0,0,
                     0,0,0,0,1,0,
                     0,0,0,0,0,1)
    
    nonegatividad <- c(1,0,0,0,0,0, 
                       0,1,0,0,0,0,
                       0,0,1,0,0,0,
                       0,0,0,1,0,0,
                       0,0,0,0,1,0,
                       0,0,0,0,0,1)
    
    # Definiciones ----
    f.obj <- costos()
    f.con <- matrix(c(cumplimiento,
                      preparacion,
                      fallas(),
                      inventarios,
                      nonegatividad),
                    nrow = 14+length(preparacion)/6,
                    byrow = T)
    
    f.dir <- c("==",
               dire,
               "<=",
               "<=","<=","<=","<=","<=","<=",
               ">=",">=",">=",">=",">=",">=")
    
    f.rhs <- c(k, 
               derecha*k,
               k*50, 
               inventario(), 
               0,0,0,0,0,0)
    
    solucion <- lp("min", f.obj, f.con, f.dir, f.rhs)
    
    aux <- data.frame(Malla=paste("Malla", seq(13,18)),
                      Optimo=solucion$solution) %>% 
      mutate(Porcentaje = Optimo/k)
    
    return(aux)
  })
  
  ### Resultados -----
  
  output$resultados <- DT::renderDataTable({
    
    aux <- optimo()
    
    datatable(aux, rownames = F, options = list(pageLength=6, dom="t")) %>% 
      formatRound("Optimo", digits = 2) %>% 
      formatPercentage("Porcentaje", digits = 2)
    
  })
  
  output$faltas <- DT::renderDataTable({
    
    aux <- optimo() %>% 
      bind_cols(fallas()) 
    
    names(aux) <- c("Malla","Optimo","Pct","Fallas")
    
    aux <- aux %>% 
      mutate(FallasEstimadas=Optimo*Fallas) %>% 
      select(Malla, FallasEstimadas)
    
    datatable(aux, rownames = F, options = list(pageLength=6, dom="t")) %>% 
      formatRound("FallasEstimadas", digits = 2)
    
  })
  
  output$vb_faltastotal <- renderInfoBox({
    
    aux <- optimo() %>% 
      bind_cols(fallas()) 
    
    names(aux) <- c("Malla","Optimo","Pct","Fallas")
    
    aux <- aux %>% 
      mutate(FallasEstimadas=Optimo*Fallas) %>% 
      summarise(sum(FallasEstimadas)) %>% 
      as.numeric()
    
    infoBox(title = "",
            subtitle = "Faltas Totales Estimadas",
            value = comma(aux),
            color = "red",
            icon = icon("bars")
    )
  })
  
  output$vb_faltasKg <- renderInfoBox({
    
    aux <- optimo() %>% 
      bind_cols(fallas()) 
    
    names(aux) <- c("Malla","Optimo","Pct","Fallas")
    
    aux <- aux %>% 
      mutate(FallasEstimadas=Optimo*Fallas) %>% 
      summarise(sum(FallasEstimadas)) %>% 
      as.numeric()
    
    infoBox(title = "",
            subtitle = "Faltas Estimadas Por Kilogramo",
            value = comma(aux/input$Kilos),
            color = "red",
            icon = icon("bars")
    )
  })
  
  
  output$vb_faltas300 <- renderInfoBox({
    
    aux <- optimo() %>% 
      bind_cols(fallas()) 
    
    names(aux) <- c("Malla","Optimo","Pct","Fallas")
    
    aux <- aux %>% 
      mutate(FallasEstimadas=Optimo*Fallas) %>% 
      summarise(sum(FallasEstimadas)) %>% 
      as.numeric()
    
    infoBox(title = "",
            subtitle = "Faltas Estimadas Por cada 300gr",
            value = comma((aux/input$Kilos)*(3/10)),
            color = "red",
            icon = icon("bars")
    )
  })
  
}
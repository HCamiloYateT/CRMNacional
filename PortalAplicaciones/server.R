function(input, output, session) {
  
  data <- reactive({
    
    apps_ver <-  CargarExcelOneDrive("hcyate", "Analitica", "AppsControl.xlsx") %>% 
      openxlsx2::read_xlsx() %>% 
      select(guid) %>%
      distinct() %>% 
      mutate(Req=1) %>% 
      left_join(
        CargarDatos("ANCTRLAPPS") %>% 
          mutate(Fecha = as.Date(Fecha),
                 FechaHora = as_datetime(FechaHora)) %>% 
          filter(Fecha == Sys.Date()),
        by = join_by(guid)
      ) %>% 
      rename(IdApp  =guid)
    
    
    aux1 <- CargarDatos("ANPORTROLES") %>% 
      mutate(updated_time = as_datetime(updated_time)) %>%
      left_join(apps_ver, by = join_by(IdApp)) %>% 
      filter(username == session$user) %>%
      # filter(username =="juliana.franco") %>%
      filter(grepl(input$Buscar, title, ignore.case = T) | grepl(input$Buscar, description, ignore.case = T),
             grepl(paste(input$Area, collapse = "|"), Area, ignore.case = T),
             grepl(paste(input$Periodicidad, collapse = "|"), Periodicidad, ignore.case = T)) %>%
      rowwise() %>% 
      mutate(Verificacion = case_when(Req  == 1 & valid %in% c(NA, 0) ~ FormatearTexto(paste0("Aplicación no verificada"), color = "firebrick"),
                                      Req  == 1 & valid %in% c(1) ~ FormatearTexto(paste("Aplicación verificada por:", usr, "el", 
                                                                                         format(FechaHora, "%d de %b del %Y %H:%M")
                                                                                         ), color = "darkolivegreen"),
                                      )
             )
    
    return(aux1)
  })
  
  output$Tabla <- renderDataTable({
    
    aux1 <- data() %>%
      mutate(Imagen = paste0("<a href='", url, "'><img src='", img,"' alt='",title,"'style='width:60px;height:60px;'></a>"),
             title = paste0("<a href='",url,"'>",title,"</a>")) %>%
      select(Imagen, title, description, Area, Periodicidad, Verificacion, updated_time)

    datatable(aux1, escape=F, rownames = F, style = "default",
              colnames = c("","Aplicación","Descripción","Áreas", "Periodicidad","Verificado", "Hora de Actualización"),
              selection="none",
              options = list(pageLength = 15, ordering=F, dom="tp",
                             language = lang)) %>%
      formatDate("updated_time", "toLocaleString" )

  })
  output$Lista <- renderUI({

    if (nrow(data())>0) {
      lista <- data() %>% 
        split(seq(nrow(.)))
      
      do.call("paste0",lapply(lista, function(x){
        
        Tarjeta(tit = x$title, desc = x$description, url = x$url,
                img = x$img, act = x$updated_time, ver = x$Verificacion,
                per = x$Periodicidad, are = x$Area)}
      )) %>% HTML
    }
    

  })
  
}

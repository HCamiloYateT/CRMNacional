function(input, output, session) {
  
  
  
  output$Usuarios <- renderDT({
    
    aux1 <- data %>% 
      filter(!is.na(Nombre)) %>% 
      group_by(Nombre, Grupo, Fecha = as.Date(started)) %>% 
      summarise(Horas = sum(as.numeric(difftime(ended, started, units = "hours"))),
                Conexiones = n()) %>% 
      ungroup() %>% 
      group_by(Nombre, Grupo) %>% 
      summarise(Conexiones = sum(Conexiones, na.rm = T),
                Horas = sum(Horas, na.rm = T),
                TiempoPromedio = Horas/Conexiones,
                UltimaConexion = max(Fecha)
                ) %>% 
      arrange(desc(Horas))
    
    datatable(aux1, escape = F, rownames=F, class = "compact", style="default",
              colnames =  c("Usuario", "Grupo", "Núm. Conexiones", "Horas en el Servidor", "Tiempo Promedio", "Última Conexión"),
              selection= "none",  extensions = c("FixedColumns", "FixedHeader", "Buttons"),
              options = list(pageLength =nrow(aux1), ordering=T, dom="tB", scrollY = paste0("350px"),
                             columnDefs = list(list(className = 'dt-right', targets = 5),
                                               list(width = "20%", targets =c(0:1))),
                             buttons = list(
                               list(extend = "excel", text = 'Descargar <span class="glyphicon glyphicon-download"> </span>',
                                    filename=paste0("Usuarios", format(Sys.Date(), "%d%m%Y")), className='copyButton')
                               ),
                             language = lang)) %>% 
      formatRound(c("Conexiones"), digits = 0) %>% 
      formatRound(c("Horas", "TiempoPromedio"), digits = 2)
    
    
    
    
  })
  output$Apps <- renderDT({

    aux1 <- data %>%
      group_by(App, created_time = as.Date(created_time), format( started, "%B%Y")) %>%
      summarise(Horas = sum(as.numeric(difftime(ended, started, units = "hours"))),
                Conexiones = n(),
                Usuarios = n_distinct(Nombre),
                UltConexion = max(as.Date(ended), na.rm = T)) %>%
      group_by(App, created_time) %>%
      summarise(Conexiones = sum(Conexiones, na.rm = T),
                HorasTot = sum(Horas, na.rm = T),
                HorasProm = mean(Horas, na.rm = T),
                UsuariosProm = mean(Usuarios, na.rm = T),
                UltimaConexion = max(UltConexion, na.rm = T)
      ) %>%
      arrange(desc(HorasProm))

    datatable(aux1, escape = F, rownames=F, class = "compact", style="default",
              colnames =  c("Aplicacion", "Fecha de Publicación", "Núm. Conexiones", "Horas en el Servidor", "Horas Prom. Mes", "Usuarios Prom. Mes", "Últ. Conexión"),
              selection= "none",  extensions = c("FixedColumns", "FixedHeader", "Buttons"),
              options = list(pageLength =nrow(aux1), ordering=T, dom="tB", scrollY = paste0("350px"),
                             columnDefs = list(list(className = 'dt-right', targets = 6),
                                               list(width = "40%", targets =c(0))),

                             buttons = list(
                               list(extend = "excel", text = 'Descargar <span class="glyphicon glyphicon-download"> </span>',
                                    filename=paste0("Usuarios", format(Sys.Date(), "%d%m%Y")), className='copyButton')
                             ),
                             language = lang)) %>%
      formatRound(c("Conexiones"), digits = 0) %>%
      formatRound(c("HorasTot", "HorasProm", "UsuariosProm"), digits = 2)




  })
  
  data_f_usr <- reactive({
    req(input$Usuario)
    data %>% 
      filter(Nombre == input$Usuario)
  })
  
  output$UsuarioApps <- renderDT({
    
    if (data_f_usr() %>% nrow > 0) {
      aux1 <- data_f_usr() %>%
        group_by(App, created_time = as.Date(created_time), format( started, "%B%Y")) %>%
        summarise(Horas = sum(as.numeric(difftime(ended, started, units = "hours"))),
                  Conexiones = n(),
                  Usuarios = n_distinct(Nombre),
                  UltConexion = max(as.Date(ended), na.rm = T)) %>%
        group_by(App, created_time) %>%
        summarise(Conexiones = sum(Conexiones, na.rm = T),
                  HorasProm = mean(Horas, na.rm = T),
                  UltimaConexion = max(UltConexion, na.rm = T)
        ) %>%
        arrange(desc(HorasProm))
      
      datatable(aux1, escape = F, rownames=F, class = "compact", style="default",
                colnames =  c("Aplicacion", "Fecha de Publicación", "Núm. Conexiones", "Horas Prom. Mes", "Últ. Conexión"),
                selection= "none",  extensions = c("FixedColumns", "FixedHeader", "Buttons"),
                options = list(pageLength =nrow(aux1), ordering=T, dom="tB", scrollY = paste0("350px"),
                               columnDefs = list(list(className = 'dt-right', targets = 4),
                                                 list(width = "40%", targets =c(0))),

                               buttons = list(
                                 list(extend = "excel", text = 'Descargar <span class="glyphicon glyphicon-download"> </span>',
                                      filename=paste0("Usuarios", format(Sys.Date(), "%d%m%Y")), className='copyButton')
                               ),
                               language = lang)) %>%
        formatRound(c("Conexiones"), digits = 0) %>%
        formatRound(c("HorasProm"), digits = 2)
      
    }
    
    
  })
  output$CalUsuario <- renderPlot({
    
    if (data_f_usr() %>% nrow > 0) {
      dtData = data_f_usr() %>% 
        group_by(Fecha = as.Date(started)) %>% 
        summarise(Horas = sum(as.numeric(difftime(ended, started, units = "hours")))) %>% 
        ungroup() %>% 
        complete(Fecha = seq.Date(min(Fecha), Sys.Date(), "days"), fill = list(Horas=0))
      
      
      p1 <- ggplot_calendar_heatmap(dtData, 'Fecha', 'Horas',
                                    dayBorderSize = 0.2, 
                                    dayBorderColour = "grey",
                                    monthBorderSize = 1,
                                    monthBorderColour = "white",
                                    monthBorderLineEnd = "round")  +
        theme_minimal() +
        scale_fill_viridis_c()+
        theme(axis.title.y = element_text(colour = "grey30", size = 10, face = "plain"))+
        labs(x = "",
             y = "Tiempo en Aplicaciones (h)",
             fill = "") +
        facet_wrap(~Year, ncol = 1)  +
        guides(colour = guide_colorbar(barwidth = 25)) +
        theme(legend.position = "bottom",
              legend.key.width = unit(2, 'cm'))
      
      p1
    }
  })
  
  data_f_app <- reactive({
    req(input$App)
    data %>% 
      filter(App == input$App)
  })
  
  output$AppsUsuarios <- renderDT({
    
    if (data_f_app() %>% nrow > 0) {
      aux1 <- data_f_app() %>%
        group_by(Nombre, created_time = as.Date(created_time), format( started, "%B%Y")) %>%
        summarise(Horas = sum(as.numeric(difftime(ended, started, units = "hours"))),
                  Conexiones = n(),
                  Usuarios = n_distinct(Nombre),
                  UltConexion = max(as.Date(ended), na.rm = T)) %>%
        group_by(Nombre, created_time) %>%
        summarise(Conexiones = sum(Conexiones, na.rm = T),
                  HorasProm = mean(Horas, na.rm = T),
                  UltimaConexion = max(UltConexion, na.rm = T)
        ) %>%
        arrange(desc(HorasProm))
      
      datatable(aux1, escape = F, rownames=F, class = "compact", style="default",
                colnames =  c("Aplicacion", "Fecha de Publicación", "Núm. Conexiones", "Horas Prom. Mes", "Últ. Conexión"),
                selection= "none",  extensions = c("FixedColumns", "FixedHeader", "Buttons"),
                options = list(pageLength =nrow(aux1), ordering=T, dom="tB", scrollY = paste0("350px"),
                               columnDefs = list(list(className = 'dt-right', targets = 4),
                                                 list(width = "40%", targets =c(0))),
                               
                               buttons = list(
                                 list(extend = "excel", text = 'Descargar <span class="glyphicon glyphicon-download"> </span>',
                                      filename=paste0("Usuarios", format(Sys.Date(), "%d%m%Y")), className='copyButton')
                               ),
                               language = lang)) %>%
        formatRound(c("Conexiones"), digits = 0) %>%
        formatRound(c("HorasProm"), digits = 2)
      
    }
    
    
  })
  output$CalApps <- renderPlot({
    
    if (data_f_app() %>% nrow > 0) {
      dtData = data_f_app() %>% 
        group_by(Fecha = as.Date(started)) %>% 
        summarise(Horas = sum(as.numeric(difftime(ended, started, units = "hours")))) %>% 
        ungroup() %>% 
        complete(Fecha = seq.Date(min(Fecha), Sys.Date(), "days"), fill = list(Horas=0))
      
      
      p1 <- ggplot_calendar_heatmap(dtData, 'Fecha', 'Horas',
                                    dayBorderSize = 0.2, 
                                    dayBorderColour = "grey",
                                    monthBorderSize = 1,
                                    monthBorderColour = "white",
                                    monthBorderLineEnd = "round")  +
        theme_minimal() +
        scale_fill_viridis_c()+
        theme(axis.title.y = element_text(colour = "grey30", size = 10, face = "plain"))+
        labs(x = "",
             y = "Tiempo en Aplicaciones (h)",
             fill = "") +
        facet_wrap(~Year, ncol = 1)  +
        guides(colour = guide_colorbar(barwidth = 25)) +
        theme(legend.position = "bottom",
              legend.key.width = unit(2, 'cm'))
      
      p1
    }
  })
  
  
}


function(input, output, session) {
  
  ## Maker ----
  data_f <- reactive({
    
    conn_cafe <- dbConnect(odbc::odbc(),
                           Driver = "ODBC Driver 18 for SQL Server",
                           Server = "172.16.19.21",
                           Database = "ContabRacafe",
                           uid = "AppQlikRcaLee",
                           pwd = "Rac@CafQlikS2022*",
                           port = 1433,
                           TrustServerCertificate="yes")
    
    venc <- dbGetQuery(conn_cafe, "select AnoBol, MesBolCod, MesBolLTD from NMEANB1")
    corr <-  dbGetQuery(conn_cafe, "select TipCFCod, TipCFNom from EXPCTAFU") %>% 
      mutate_if(is.character, LimpiarCadena)
    
    banrep <- dbGetQuery(conn_cafe, as.is = T, paste("select * from EXPACTR1 where MovBolFec ='", format(input$Fecha, "%Y-%d-%m") ,"'")) %>% 
      left_join(venc, by = c("AnoBol", "MesBolCod")) %>% 
      left_join(corr, by = "TipCFCod") %>% 
      arrange(MovBolCBR) %>% 
      mutate(Consecutivo = MovBolCBR,
             TipTransaccion = ifelse(MovBolCanL == 0, "V", "C"),
             TipProducto = "Futuro",
             MonedaRef = "USD     dólar de U.S.A.",
             FechaNegociacion = as.Date(MovBolFec),
             FecVencimiento = as.Date(MesBolLTD),
             MontoMonedaRef =  (MovBolCanL+MovBolCanS)*as.numeric(MovBolVr)*375,
             ProductoSub = "Café",
             Contraparte = TipCFNom,
             PaisContraparte = "Estados Unidos",
             TipoContraparte = "K      ENTIDAD NO RESIDENTE",
             TasaContado=NA,
             Precio = MovBolVr,
             ModCumplimiento = "DF",
             OpcionesCallPut = "",
             OctionesTipo = "",
             OpPrecioReferencia=NA,
             SwapTasaReferencia=NA,
             SwapPeriodicidad = "",
             TipoNovedad = "I",
             Comentarios = paste0("Precio dado en centavos de dólar por libra. Número de contratos: ", (MovBolCanL+MovBolCanS)),
             OpRealizadaMatriz = "NO") %>% 
      select(Consecutivo:OpRealizadaMatriz)
    
    dbDisconnect(conn_cafe)
    
    return(banrep)
    
  })
  
  observe({
    
    if (sum(data_f()$Consecutivo==0) > 0 | data_f() %>% nrow == 0) {
      shinyjs::disable("Enviar")
    } else {
      shinyjs::enable("Enviar")
    }
    
  })
  output$Titulo <- renderUI({
    
    fest <- FormatearTexto(ifelse(input$Fecha %in% festivos$Fecha, paste("( FESTIVO EN", festivos %>% filter(Fecha == input$Fecha) %>% .$Lugar, ")"), ""), 
                           color = "red", negrita = T, transform = "capitalize")
    
    h4(paste("Reporte de derivados productos básicos: ", 
             FechaTexto(input$Fecha, dia_nombre = T, dia_nom_abr = F, mes_abr = F, anho_abr = F), fest) %>% HTML)
  })
  output$Informe <- renderDataTable({

  noms <- c('Número Consecutivo','Tipo de  Transacción ','Tipo Producto',
            'Moneda Referencia','Fecha de Negociación','Fecha de Vencimiento',
            'Monto en Moneda de Referencia','Producto Subyacente',
            'Nombre de la  Contraparte','País de la Contraparte',
            'Tipo Contraparte','Tasa de Contado en Moneda de Referencia',
            'Tasa Pactada (Precio)','Modalidad de Cumplimiento',
            'Opciones Call/Put','Opciones Tipo Opc.',
            'Opciones Precio Ejercicio en Moneda de Referencia',
            'Swap Tasa de Referencia','Swap Periodicidad','Tipo de Novedad',
            'Comentarios ','Operación realizada por  Matriz/ Controlante')

  datatable(data_f(), escape = F, rownames=F, colnames = noms,
            extensions = c("FixedColumns", "FixedHeader"),
            class = "compact", selection=list(target='row', mode = "single"),
            options = list(pageLength =nrow(data_f()), ordering=F, dom="t",
                           autoWidth = TRUE,
                           columnDefs = list(list(className = 'dt-center', width = '220', targets="_all")),
                           scrollX = T,
                           language = lang)) %>% 
    formatStyle(c(1:22), textAlign = 'center')

})
  output$Descargar <- downloadHandler(
    filename = function() {
      paste0(toupper(paste0("reporte de derivados productos básicos ", FechaTexto(input$Fecha, dia_nombre = F, anho_abr = F, mes_abr = F))), '.xlsx')
    },
    content = function(con) {
      
      aux <- data_f()
      wb <- wb_load("data/Plantilla.xlsx")
      write_data(wb, "productos básicos", x = aux, startCol = 1, startRow = 13, colNames = F, rowNames = F, na.strings = "")
      tit <- paste0(toupper(paste0("reporte de derivados productos básicos ", FechaTexto(input$Fecha, dia_nombre = F, anho_abr = F, mes_abr = F))), '.xlsx')
      wb_save(wb, con)
    }
  )
  
  observeEvent(input$Enviar, {
    
    aux1 <- CargarDatos("ANINFDER") %>%
      filter(FechaReporte == input$Fecha)
    
    if (length(as.Date(aux1$FechaReporte)) == 0){
      
      confirmSweetAlert(
        inputId = "myconfirmation",
        title = "¿Enviar reporte?",
        btn_labels = c("Cancelar", "Enviar"),
        btn_colors = c("gray", "red"),
        closeOnClickOutside = T, 
        showCloseButton = T
      )
      
    } else {
      
      sendSweetAlert(title = "El correo ya fue enviado", type = "warning",
                     text = paste("El informe de derivados del", 
                                  str_to_lower(FechaTexto(input$Fecha, dia_nombre = T, dia_nom_abr = F, anho_abr = F, mes_abr = F)),
                                  "fue enviado por: ", aux1$Usuario, "el",
                                  str_to_lower(FechaTexto(as.Date(aux1$FechaEnvio), dia_nombre = T, dia_nom_abr = F, anho_abr = F, mes_abr = F)),
                                  "a las",
                                  format(as_datetime(aux1$FechaEnvio), "%I:%M:%S %p"))
      )
      
    }
    
    
  })
  observeEvent(input$myconfirmation, {
    if (isTRUE(input$myconfirmation)) {
      
      # Preparacion Archivo
      aux <- data_f()
      wb <- wb_load("data/Plantilla.xlsx")
      write_data(wb, "productos básicos", x = aux, startCol = 1, startRow = 13, colNames = F, rowNames = F, na.strings = "")
      tit <- paste0(toupper(paste0("reporte de derivados productos básicos ", FechaTexto(input$Fecha, dia_nombre = F, anho_abr = F, mes_abr = F))), '.xlsx')
      wb_save(wb, tit)
      
      # Envio del Correo 
      # to <- c("hcyate@racafe.com")
      to <- c("derivados@banrep.gov.co")
      
      cco <- c("dsrosas@racafe.com", "jagallo@racafe.com", "cavillarraga@racafe.com")
      
      subj <- gsub(".xlsx","",tit)
      
      img_string_2 <- add_image(file = "www/logo2.png", align = "center", width = 50)
      bl_body <- paste0("Buenos días,","<br><br>", 
                        "Adjunto encontrará el ", str_to_lower(subj),".<br><br><br><br>",
                        "<img src='www/firma.png' width='250' height='90'>")
      
      compose_email(body=md(bl_body),footer=md(img_string_2)) %>% 
        add_attachment(tit) %>%
        smtp_send(to, "reportesanalitica@racafe.com", bcc = cco,
                  subject = subj, 
                  credentials = creds_file(file = "cred"), 
                  verbose = F)
      
      # Actualizacion en Log de envio.
      log1 <- data.frame(FechaReporte=input$Fecha, FechaEnvio=Sys.time(), Usuario = session$user)
      EscribirDatos("ANINFDER", log1)
      
      #Limpieza
      sendSweetAlert(title = "Correo enviado", type = "success", text = "Correo enviado con éxito")
      rm(wb)
      unlink(tit)
      
    }
  }, ignoreNULL = TRUE)
  
  ## Checker ----
  
  data_c <- reactive({
    
   CargarDatos("ANINFDER") %>% 
      arrange(desc(FechaReporte)) %>%
      mutate(FechaReporte = ymd(FechaReporte),
             FechaEnvio = as_datetime(FechaEnvio)) %>% 
      left_join(festivos, by=c("FechaReporte"="Fecha")) %>% 
      mutate(HoraEnvio = format(FechaEnvio, "%H:%M:%S"),
             FechaEnvio = as_date(FechaEnvio),
             Usuario = str_to_upper(Usuario)) %>% 
      relocate(Lugar, .after = FechaReporte) %>% 
      relocate(HoraEnvio, .after = FechaEnvio) 
    
    
  }) 
  output$Control <- renderDataTable({
    
    noms <- c("Fecha de Reporte", "Festivo", "Fecha de Envío", "Hora de envío", "Usario que Envió")
    
    DT::datatable(data_c(), escape = F, rownames=F, colnames = noms,
              extensions =  c('Buttons'),
              class = "compact", selection="none",
              options = list(pageLength = 15, ordering=F, dom="tpB",
                             buttons = list(list(extend = "excel", text = 'Descargar <span class="glyphicon glyphicon-download"> </span>', 
                                                 filename=paste0("InformeDerivados"), className='copyButton')),
                             language = lang))
    
  })
  
}

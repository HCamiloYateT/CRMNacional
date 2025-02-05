function(input, output, session) {
  
  data <- eventReactive(input$Crear, {
    
    data.frame(NFirst= input$NFirst,
               NLast = input$NLast,
               FN = paste(input$NFirst, input$NLast),
               TITLE = input$TITLE,
               TEL = input$TEL,
               EMAIL = input$EMAIL)
    
  })
  
  output$CodigoQR <- renderPlot({

      text <- sprintf(temp, 
                      data()$NLast,
                      data()$NFirst,
                      data()$FN,
                      data()$TITLE,
                      data()$TEL,
                      data()$EMAIL)
      qr_code(text) %>% plot
      
    
  })
  
  # output$ImagenQR <- renderUI({
  # 
  #   text <- sprintf(temp, 
  #                   data()$NLast,
  #                   data()$NFirst,
  #                   data()$FN,
  #                   data()$TITLE,
  #                   data()$TEL,
  #                   data()$EMAIL) %>% URLencode()
  # 
  #   res <- paste0("https://quickchart.io/qr?text=", text)
  # 
  #   tags$iframe(style="border:none;",
  #               src = res)
  # 
  # })


  
}

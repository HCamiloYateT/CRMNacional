function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = users,
    user_col = "User",
    pwd_col = "Pass",
    sodium_hashed = F,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )

  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  observeEvent(credentials()$user_auth, {

    if (credentials()$user_auth) {

      sendSweetAlert(
        session = session,
        title = "",
        text = h4(paste(ifelse(user_info()$Genero == "F", "Bienvenida ", "Bienvenido "), user_info()$Nombre)),
        type = "success", closeOnClickOutside = T, showCloseButton = T, width = "300px"
      )
      removeTab("tabs", "login")
      appendTab("tabs", tab_apps, select = TRUE)

    }
  })

  user_info <- reactive({credentials()$info})

  lista_apps <- reactive({
    roles %>% 
      filter(IdUsuario == user_info()$User) %>% 
      left_join(apps, by = "AppId")
  })
  
  output$board <- renderUI(
    div(
      class = "board",
      board_ui("board", lista_apps())
      )
  )

}

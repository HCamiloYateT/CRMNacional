tagList(
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo2.png")),
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  tags$head(tags$style(type = 'text/css','.navbar-brand{display:none;}')),
  shinyjs::useShinyjs(),
  navbarPage(
    title = NULL,
    windowTitle="Portal de Apliaciones",
    theme = bs_theme(bootswatch = "lux"),
    collapsible=TRUE,
    id = "tabs",
    login_tab
    ),
  HTML('<center><img src="logo2.png" width="90" height="90"></center>'),
  tags$footer(HTML("                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <div class='footer-copyright text-center py-3'>© 2022 Copyright:
                           <a href='https://racafe.com.co/es/'> Racafé</a>
                           </div>
                           <!-- Copyright -->

                           </footer>
                           <!-- Footer -->"))
  )
tagList(
  ## CSS y HTML ----
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo2.png")),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  # tagList(tags$head(tags$style(type = 'text/css','.navbar-brand{display:none;}'))),
  tags$style(type = 'text/css', '.bg-aqua {background-color: #707B7C!important; }'),
  tags$style(type = 'text/css', '.bg-teal {background-color: #121110!important; }'),
  tags$style(type = 'text/css', '.bg-olive {background-color: #317A5A!important; }'),
  ## Herencia de Funciones ----
  useShinyjs(),
  useShinydashboard(),
  ### Navegacion ----
  navbarPage( 
    title = "Informe de Gestión",
    windowTitle="Gestion de Trilladoras",
    theme = bs_theme(bootswatch = "lux"),
    id = "tabs",
    collapsible = T,
    login_tab
    ),
  ### Footer ----
  br(),br(),br(),
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
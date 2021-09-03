tagList(
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo2.png")),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  useSweetAlert(),
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  navbarPage(  
    title = "Registro de Contacto",
    windowTitle="Registro de Contacto",
    theme = bs_theme(bootswatch = "lux"),
    id = "tabs",
    collapsible = TRUE,
    login_tab
    ),
  tags$footer(HTML("
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <div class='footer-copyright text-center py-3'>© 2021 Copyright:
                           <a href='https://racafe.com.co/en//'> Racafé</a>
                           </div>
                           <!-- Copyright -->

                           </footer>
                           <!-- Footer -->"))
)


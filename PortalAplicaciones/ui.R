tagList(
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo2.png")),
  # tags$head(tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")),
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
  shinyjs::useShinyjs(),
  shinybusy::add_busy_bar(color = "#FF0000"),
  navbarPage(
    title = HTML("<center><img src='logo.png' width='150' height='60'></center>"),
    windowTitle="Portal de Aplicaciones",
    theme = bs_theme(bootswatch = "lux"),
    id = "tabs",
    portal
  ),
  tags$footer(HTML(paste0("<footer class='page-footer font-large indigo'>
                           <center>
                           <img src='logo2.png' width='90' height='90'>
                           </center>
                           <div class='footer-copyright text-center py-3'>",format(Sys.Date(), "%Y"), " Copyright:
                           <a href='https://racafe.com.co/es/'> Racafé</a>
                           </div>
                          </footer>")
                   )
              )
)
tagList(
  ## CSS y HTML ----
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo2.png")),
  includeCSS("www/style.css"),
  ## Herencia de Funciones ----
  useShinyjs(),
  useShinydashboard(),
  add_busy_bar(color = "#FF0000"),
  ### Navegacion ----
  navbarPage( 
    title = HTML("<a href='https://racafe.shinyapps.io/PortalAplicaciones/'><center><img src='logo.png' width='150' height='60'></center></a>"),
    windowTitle="Informe Banco de la República ",
    theme = bs_theme(bootswatch = "lux"),
    id = "tabs",
    maker_tab,
    checker_tab
    ),
  ### Footer ----
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
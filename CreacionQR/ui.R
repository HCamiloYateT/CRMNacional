tagList(
  ## CSS y HTML ----
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo2.png")),
  includeCSS("www/style.css"),
  ## Herencia de Funciones ----
  useShinyjs(),
  add_busy_bar(color = "#FF0000"),
  ### Navegacion ----
  navbarPage( 
    title = HTML("<a href='https://analitica.racafe.com/'><center><img src='logo.png' width='150' height='60'></center></a>"),
    windowTitle="Creación QR",
    theme = bs_theme(bootswatch = "lux"),
    id = "tabs",
    tabPanel("",
             fluidRow(
               column(1),
               column(6,
                      textInput("NFirst", label = h6("Nombre"), value = "", placeholder = "Ingrese el nombre de contacto", width = "100%"),
                      textInput("NLast", label = h6("Apellido"), value = "", placeholder = "Ingrese el apellido del contacto", width = "100%"),
                      textInput("TITLE", label = h6("Cargo"), value = "", placeholder = "Ingrese el cargo del contacto", width = "100%"),
                      textInput("TEL", label = h6("Teléfono  (+57)"), value = "", placeholder = "Ingrese el número de telefono", width = "100%"),
                      textInput("EMAIL", label = h6("E-mail"), value = "", placeholder = "Ingrese el correo electrónico", width = "100%"),
                      div(style="text-align: center;",
                          actionBttn(inputId = "Crear", label = "Crear QR", style = "unite", color = "danger", icon = icon("qrcode"))
                      )
                      ),
               column(4,
                      br(),br(),
                      plotOutput("CodigoQR", width = "100%")
                      # uiOutput("ImagenQR")
                      ),
               column(1)
               )
             )
    ),
  ### Footer ----
  tags$footer(HTML("<footer class='page-footer font-large indigo'>
                    <center><img src='logo2.png' width='90' height='90'></center>
                    <div class='footer-copyright text-center py-3'>©", format(Sys.Date(), "%Y"), " Copyright:
                    <a href='https://racafe.com.co/es/'> Racafé</a>
                    </div></footer>"))
)
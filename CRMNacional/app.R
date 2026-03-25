# Punto de entrada consolidado de la aplicación
source("global.R", local = FALSE)
source("ui.R", local = FALSE)
source("server.R", local = FALSE)

shinyApp(ui = ui, server = server)

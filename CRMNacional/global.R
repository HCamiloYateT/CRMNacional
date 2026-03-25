# Configuración del sistema
Sys.setlocale("LC_TIME", "es_ES.UTF-8")
options(
  dplyr.summarise.inform = FALSE,
  repos = c(CRAN = "https://cloud.r-project.org")
)

tit_app = "CRM Nacional"

# Credenciales
uid = Sys.getenv("SYS_UID")
pwd = Sys.getenv("SYS_PWD")

# Librerías consolidadas
required_packages <- c("shiny", "bs4Dash", "shinyBS", "shinyjs", "shinytoastr", "shinyWidgets", 
                       "shinybusy", "shinyGizmo", "DBI", "tidyverse", "lubridate", "DT", 
                       "rvest", "phosphoricons", "racafe", "scales", "plotly", "colorspace", 
                       "rlang", "rhandsontable", "waiter", "gt", "blastula", "tsibble",
                       "fabletools", "tsibble", "forecast", "prophet", "fable", "racafeModulos")
racafe::Loadpkg(required_packages)

# Carga de datos y funciones
load("data/data.RData")
.fecha_min   <- min(data$FecFact, na.rm = TRUE)
.fecha_max   <- max(data$FecFact, na.rm = TRUE)
source("shared/functions.R")
source("shared/values.R")
.app_choices <- Choices()
source("shared/filters.R")




source("core/services.R")
load_modules <- function() {
  module_dirs <- c("modules", "misc/modules")
  ui_dirs <- c("ui", "misc/ui")

  modules <- unlist(lapply(module_dirs, function(path) {
    if (!dir.exists(path)) return(character(0))
    list.files(path, pattern = "\\.R$", full.names = TRUE)
  }))

  ui_components <- unlist(lapply(ui_dirs, function(path) {
    if (!dir.exists(path)) return(character(0))
    list.files(path, pattern = "\\.R$", full.names = TRUE)
  }))

  pres <- modules[grepl("/Presupuesto\\.R$", modules)]
  botones <- modules[grepl("/GTBotones\\.R$", modules)]
  modules_rest <- setdiff(modules, c(pres, botones))

  ordered <- c(botones, pres, sort(modules_rest), sort(ui_components))

  message("Cargando archivos en orden:")
  purrr::walk(ordered, ~ message(" - ", .x))
  purrr::walk(ordered, ~ sys.source(.x, envir = globalenv()))
}
load_modules()

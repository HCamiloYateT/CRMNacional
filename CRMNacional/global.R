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
source("misc/functions.R")
source("misc/values.R")
.app_choices <- Choices()
source("misc/filters.R")




load_modules <- function() {
  path <- "misc"
  all_files <- list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  
  helpers <- all_files[basename(all_files) %in% c("functions.R", "filters.R", "values.R")]
  
  modules <- all_files[grepl("/modules/", all_files)]
  
  pres <- modules[grepl("modules/Presupuesto\\.R$", modules)]
  botones <- modules[grepl("modules/GTBotones\\.R$", modules)]
  modules_rest <- setdiff(modules, c(pres, botones))
  
  modules <- c(botones, pres, sort(modules_rest))
  
  ui <- all_files[grepl("/ui/", all_files)]
  
  ordered <- c(helpers, modules, ui)
  
  message("Cargando archivos en orden:")
  purrr::walk(ordered, ~ message(" - ", .x))
  purrr::walk(ordered, ~ sys.source(.x, envir = globalenv()))
}
load_modules()
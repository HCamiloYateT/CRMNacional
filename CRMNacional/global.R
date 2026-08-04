# Configuración del sistema
Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setenv(LANG = "es_CO.UTF-8")
options(
  dplyr.summarise.inform = FALSE,
  repos = c(CRAN = "https://cloud.r-project.org")
)
options(sass.cache = FALSE)

tit_app = "CRM Nacional"

# Credenciales
uid = Sys.getenv("SYS_UID")
pwd = Sys.getenv("SYS_PWD")

# Librerías consolidadas
required_packages <- c("shiny", "bs4Dash", "shinyBS", "shinyjs", "shinytoastr", "shinyWidgets", 
                       "shinybusy", "shinyGizmo", "DBI", "tidyverse", "lubridate", "DT", 
                       "rvest", "phosphoricons", "racafe", "scales", "plotly", "colorspace", 
                       "rlang", "rhandsontable", "waiter", "gt", "blastula", "tsibble",
                       "fabletools", "tsibble", "forecast", "prophet", "fable", "racafeModulos", 
                       "reactable", "racafeForecast", "wordcloud2")
racafe::Loadpkg(required_packages)

# Carga de scripts utilitarios
load("data/data.RData")
.fecha_min   <- min(data$FecFact, na.rm = TRUE)
.fecha_max   <- max(data$FecFact, na.rm = TRUE)

racafeCore::load_modules(verbose = FALSE)

.app_choices <- Choices()
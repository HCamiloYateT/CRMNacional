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
                       "reactable")
racafe::Loadpkg(required_packages)

# Carga de scripts utilitarios
load("data/data.RData")
.fecha_min   <- min(data$FecFact, na.rm = TRUE)
.fecha_max   <- max(data$FecFact, na.rm = TRUE)

load_modules <- function(path = "misc", verbose = FALSE, progress = TRUE) {
  
  # Verificar dependencias internas antes de ejecutar
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("[load_modules] stringr no disponible. Debe cargarse antes de invocar load_modules.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("[load_modules] purrr no disponible. Debe cargarse antes de invocar load_modules.")
  }
  
  # Recolectar y normalizar rutas, excluir global.R
  files <- list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  files <- unique(normalizePath(files, winslash = "/", mustWork = TRUE))
  files <- files[!grepl("/global\\.R$", files, ignore.case = TRUE)]
  
  # Salida temprana si no hay archivos en el path indicado
  if (length(files) == 0L) {
    message(sprintf("[WARN] No se encontraron archivos .R en '%s'", path))
    return(invisible(list(ok = 0L, fallidos = character(0), errores = list())))
  }
  
  # Ordenar por profundidad ascendente: menos separadores = mas cerca a raiz
  depth   <- stringr::str_count(files, "/")
  files   <- files[order(depth, files)]
  n_total <- length(files)
  
  if (verbose) {
    message(sprintf("[INFO] %d archivos encontrados en '%s':", n_total, path))
    purrr::walk(files, ~ message(sprintf("  [depth=%d] %s", stringr::str_count(.x, "/"), .x)))
  }
  
  # Inicializar barra de progreso si se solicita y no esta en modo verbose
  use_pb <- progress && !verbose && n_total > 0L
  pb <- if (use_pb) {
    txtProgressBar(min = 0L, max = n_total, style = 3, width = 60, char = "=")
  } else {
    NULL
  }
  
  # Estado de carga acumulado entre pasadas
  pendientes  <- files
  errores     <- list()
  cargados    <- character(0)
  pasada      <- 1L
  n_procesado <- 0L
  
  while (length(pendientes) > 0L) {
    fallidos_pasada <- character(0)
    
    for (f in pendientes) {
      resultado <- tryCatch(
        {
          sys.source(f, envir = globalenv())
          "ok"
        },
        error = function(e) e$message
      )
      
      if (identical(resultado, "ok")) {
        cargados    <- c(cargados, f)
        n_procesado <- n_procesado + 1L
        # Actualizar barra o imprimir OK segun modo activo
        if (use_pb) {
          setTxtProgressBar(pb, n_procesado)
        } else {
          message(sprintf("  [OK] %s", f))
        }
      } else {
        fallidos_pasada <- c(fallidos_pasada, f)
        errores[[f]]    <- resultado
        if (!use_pb) message(sprintf("  [FAIL] %s\n         -> %s", f, resultado))
      }
    }
    
    # Sin progreso: ninguno de los pendientes pudo cargarse en esta pasada
    if (length(fallidos_pasada) == length(pendientes)) {
      # Cerrar barra antes de imprimir errores para no mezclar output
      if (use_pb) { close(pb); use_pb <- FALSE; cat("\n") }
      message(sprintf("\n[ERROR] Pasada %d sin progreso. Archivos irresolubles:", pasada))
      purrr::walk(fallidos_pasada, function(f) {
        message(sprintf("  [FAIL] %s\n         -> %s", f, errores[[f]]))
      })
      break
    }
    
    # Hay progreso pero quedan fallidos: reintentar en siguiente pasada
    if (length(fallidos_pasada) > 0L && length(fallidos_pasada) < length(pendientes)) {
      # Hay progreso pero quedan fallidos: reportar siempre, con el error de cada uno
      message(sprintf("[RETRY] Pasada %d: %d archivo(s) con error, reintentando:",
                      pasada, length(fallidos_pasada)))
      if (use_pb) {
        # En modo pb los fallos no se imprimieron inline: mostrarlos aquí
        purrr::walk(fallidos_pasada, ~ message(sprintf("  [FAIL] %s\n         -> %s", .x, errores[[.x]])))
      }
    }
    
    pendientes <- fallidos_pasada
    pasada     <- pasada + 1L
  }
  
  # Cerrar barra si todos los archivos cargaron correctamente
  if (use_pb) { close(pb); cat("\n") }
  
  # Resumen final siempre visible independiente de verbose
  message(sprintf("[DONE] Modulos: %d cargados | %d fallidos de %d totales",
                  length(cargados), length(errores), n_total))
  
  invisible(list(ok = length(cargados), fallidos = names(errores), errores = errores))
}
load_modules(verbose = FALSE)

.app_choices <- Choices()
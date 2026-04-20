tictoc::tic("CRM NACIONAL")
print(paste0("***********",Sys.time(), "***********"))
setwd("/home/htamara/6_IndustriaNacional/CRM Cliente Nacional")
options(scipen = 99999)

# Funciones ----
SubirDatos <- function(df, db){
  con <- dbConnect(RMySQL::MySQL(), dbname = "Analitica", host = "localhost",
                   port = 3306, user = 'datos', password = 'REMOVED', DBMSencoding = "UTF-8")
  DBI::dbWriteTable(con, db, df, row.names = F ,overwrite = T, encoding = "latin1")
  dbDisconnect(con)
}
get_access_token <- function() {
  url <- paste0("https://login.microsoftonline.com/", tenant_id, "/oauth2/v2.0/token")
  payload <- list(
    grant_type = 'client_credentials',
    client_id = client_id,
    REMOVED = REMOVED,
    scope = 'https://graph.microsoft.com/.default'
  )
  response <- POST(url, body = payload, encode = "form")
  response_data <- content(response, as = "parsed", type = "application/json")
  access_token <- response_data$access_token
  return(access_token)
}
get_drive_id <- function(usr){
  
  access_token <- get_access_token()
  headers <- add_headers(
    Authorization = paste("Bearer", access_token),
    "Content-Type" = "application/json"
  )
  
  url <- paste0("https://graph.microsoft.com/v1.0/users/", usr, "@racafe.com/drive")
  response <- GET(url, headers)
  id <- content(response, as = "parsed", type = "application/json")
  return(id$id)
  
}
CargarExcelOneDrive <- function(usr, path, file) {
  
  access_token <- get_access_token()
  headers <- add_headers(
    Authorization = paste("Bearer", access_token),
    "Content-Type" = "application/json"
  )
  
  drive <- get_drive_id(usr)
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive, "/root:/", path, "/", file, ":/content") %>% URLencode
  response <- GET(url, headers)
  
  
  if (status_code(response) == 200) {
    writeBin(content(response, "raw"), "Tmp.xlsx")
    wb <- openxlsx2::wb_load("Tmp.xlsx")
    file.remove("Tmp.xlsx")
    return(wb)
  } else {
    cat("Error: No se puede descargar el archivo\n")
    cat("Status code:", status_code(response), "\n")
    cat(content(response, "text"), "\n")
  }
  
  
}
DescargarExcelOneDrive <- function(usr, path, file, nom) {
  
  access_token <- get_access_token()
  headers <- add_headers(
    Authorization = paste("Bearer", access_token),
    "Content-Type" = "application/json"
  )
  
  drive <- get_drive_id(usr)
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive, "/root:/", path, "/", file, ":/content") %>% URLencode
  response <- GET(url, headers)
  
  
  if (status_code(response) == 200) {
    writeBin(content(response, "raw"), paste0(nom, ".xlsx"))
  } else {
    cat("Error: No se puede descargar el archivo\n")
    cat("Status code:", status_code(response), "\n")
    cat(content(response, "text"), "\n")
  }
  
  
}
list_folders <- function(user) {
  access_token <- get_access_token()
  drive_id <- get_drive_id(user)
  
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/root/children")
  
  headers <- add_headers(
    Authorization = paste("Bearer", access_token),
    "Content-Type" = "application/json"
  )
  
  response <- GET(url, headers)
  
  if (http_status(response)$category != "Success") {
    stop("Error al obtener las carpetas: ", content(response, as = "text"))
  }
  
  content_data <- content(response, as = "parsed", type = "application/json")
  
  folders <- content_data$value
  folder_names <- sapply(folders, function(item) {
    if (!is.null(item$folder)) {
      return(item$name)
    }
    return(NULL)
  })
  
  # Eliminar valores NULL y devolver los nombres de las carpetas
  folder_names <- folder_names[!is.null(folder_names)]
  return(folder_names)
}
get_folder_id <- function(user, folder_name) {

  access_token <- get_access_token()
  drive_id <- get_drive_id(user)
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/root/children")
  
  headers <- add_headers(
    Authorization = paste("Bearer", access_token),
    "Content-Type" = "application/json"
  )
  
  response <- GET(url, headers)
  
  if (http_status(response)$category != "Success") {
    stop("Error al obtener las carpetas: ", content(response, as = "text"))
  }
  
  content_data <- content(response, as = "parsed", type = "application/json")
  
  folder_id <- NULL
  for (item in content_data$value) {
    if (!is.null(item$folder) && item$name == folder_name) {
      folder_id <- item$id
      break
    }
  }
  
  if (is.null(folder_id)) {
    stop(paste("Carpeta", folder_name, "no encontrada."))
  }
  
  return(folder_id)
}
list_folder_contents_name <- function(user, folder_name) {
  
  folder_id <- get_folder_id(user, folder_name)
  access_token <- get_access_token()
  
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", get_drive_id(user), "/items/", folder_id, "/children")
  
  headers <- add_headers(
    Authorization = paste("Bearer", access_token),
    "Content-Type" = "application/json"
  )
  
  response <- GET(url, headers)
  
  if (http_status(response)$category != "Success") {
    stop("Error al listar los elementos de la carpeta: ", content(response, as = "text"))
  }
  
  content_data <- content(response, as = "parsed", type = "application/json")
  
  elements <- lapply(content_data$value, function(item) {
    list(
      name = item$name,
      type = if (!is.null(item$folder)) "Folder" else "File",
      id = item$id
    )
  })
  
  elements_df <- do.call(rbind, lapply(elements, as.data.frame))
  return(elements_df)
}
list_folder_contents_id <- function(user, folder_id) {
  
  access_token <- get_access_token()
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", get_drive_id(user), "/items/", folder_id, "/children")
  
  headers <- add_headers(
    Authorization = paste("Bearer", access_token),
    "Content-Type" = "application/json"
  )
  
  response <- GET(url, headers)
  
  if (http_status(response)$category != "Success") {
    stop("Error al listar los elementos de la carpeta: ", content(response, as = "text"))
  }
  
  content_data <- content(response, as = "parsed", type = "application/json")
  
  elements <- lapply(content_data$value, function(item) {
    list(
      name = item$name,
      type = if (!is.null(item$folder)) "Folder" else "File",
      id = item$id
    )
  })
  
  elements_df <- do.call(rbind, lapply(elements, as.data.frame))
  return(elements_df)
}
list_folder_contents_recursive <- function(user, folder_id, parent_path = "") {
  access_token <- get_access_token()
  drive_id <- get_drive_id(user)
  
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/items/", folder_id, "/children")
  
  headers <- add_headers(
    Authorization = paste("Bearer", access_token),
    "Content-Type" = "application/json"
  )
  
  response <- GET(url, headers)
  
  if (http_status(response)$category != "Success") {
    stop("Error al listar los elementos de la carpeta: ", content(response, as = "text"))
  }
  
  content_data <- content(response, as = "parsed", type = "application/json")
  
  elements <- list()
  
  for (item in content_data$value) {
    # Ruta completa del archivo/carpeta
    item_path <- file.path(parent_path, item$name)
    
    if (!is.null(item$folder)) {
      # Si es una carpeta, explorar recursivamente
      subfolder_elements <- list_folder_contents_recursive(user, item$id, item_path)
      elements <- append(elements, subfolder_elements)
    } else {
      # Si es un archivo, agregarlo al listado
      elements <- append(elements, list(list(
        name = item$name,
        type = "File",
        id = item$id,
        path = item_path
      )))
    }
  }
  
  return(elements)
}
list_all_folder_contents <- function(user, folder_id) {
  elements <- list_folder_contents_recursive(user, folder_id)
  
  elements_df <- do.call(rbind, lapply(elements, as.data.frame))
  return(elements_df)
}
get_file_content <- function(file_id, user) {
  access_token <- get_access_token()
  drive_id <- get_drive_id(user)
  
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/items/", file_id, "/content")
  
  headers <- add_headers(
    Authorization = paste("Bearer", access_token)
  )
  
  response <- GET(url, headers, write_disk("temp_file.xlsx", overwrite = TRUE))
  
  if (http_status(response)$category != "Success") {
    stop("Error al descargar el archivo: ", content(response, as = "text"))
  }
  
  message("Archivo descargado exitosamente.")
  return("temp_file.xlsx")
}
read_excel_sheets_from_onedrive <- function(file_id, user) {
  file_path <- get_file_content(file_id, user)
  hojas <- excel_sheets(file_path)
  return(hojas)
}
read_excel_from_onedrive <- function(file_id, user, ...) {
  # Obtener el contenido del archivo
  file_path <- get_file_content(file_id, user)
  data <- read_excel(file_path, ...)
  file.remove(file_path)
  return(data)
}


# Valores ----
client_id ='750565f3-da81-4f91-a8c2-fa861ab456f6'
REMOVED = 'REMOVED'
tenant_id ='71271bb0-c004-4e2b-bdd1-0e57372c6a35'
uid = "AppQlikRcaLee"
pwd = "REMOVED"

# librerias ----
pkgs <- c("racafe", "tidyverse", "DBI", "lubridate", "httr", "readxl")
racafe::Loadpkg(pkgs)


# Lectura de Datos ----
## Marcacion de clientes ----

clientes <- CargarExcelOneDrive("hcyate", 
                                "2024/Industria Nacional/CRM/Tablas de Homologación",
                                "Personas.xlsx") %>% 
  openxlsx2::read_xlsx(sheet = "BD") %>% 
  mutate(FecProceso = Sys.Date(),
         Usr = "hcyate",
         CliNitPpal = ifelse(is.na(CliNitPpal), CLCliNit, CliNitPpal),
         DescartadoPrecio = DescartadoPrecio,
         DescartadoCalidad = DescartadoCalidad
         ) %>% 
  select(FecProceso, Usr, LinNegCod, LinNegocio = LinNeg, CLCliNit, CliNitPpal,
         Asesor, Responsable =  `RESPONSABLE PPT`, starts_with("Segmento2"), contains("Ppto"), 
         SegmentoRacafe, EstadoCliente, EstadoNegocio, 
         RazonDescartado, DescartadoPrecio, DescartadoCalidad,
         FrecuenciaDias, NumMesesRecuperar) 
  
SubirDatos(clientes, "CRMNALCLIENTE")

## Margenes ----

usr <- "wmunozs"

meses_es <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
              "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")

anios <- list(list(file = "014N3L2U25IZAT3WYZGVFJGCZIDNSKRNAZ", sheet = "facturas detalle lote"),
              list(file = "014N3L2UYF5REA56C4FJBJIDVGVTL7Q5KC", sheet = "BASE"),
              list(file = "014N3L2U3KMRY62PJEAVAKCINWACPKEEN2", sheet = "BASE A DIC"),
              list(file = "014N3L2U3FF242O4SCCZFKZQI2WLPFYWUZ", sheet = "BASE A DIC 2019"),
              list(file = "014N3L2U2UNNJZCOXBT5G3BTW5FT2X5NUB", sheet = "BASE"),
              list(file = "014N3L2UYEG2B4LQ5CLFE2UNXX7LS5F2KZ", sheet = "BASE"),
              list(file = "014N3L2U7OUDR3SIVD35FIN7VKHPGQO4JI", sheet = "BASE"),
              list(file = "014N3L2U6GFS3QX3SFQJGKHMAE4XRQ6DUP", sheet = "BASE"),
              list(file = "014N3L2U3M7ICAP6MGRBAKFYFORYW4GFYS", sheet = "BASE"),
              list(file = "014N3L2U4TDA25KCDBNNFZO2JIUO4YKQCB", sheet = "Base"),
              list(file = "014N3L2U4AMBYV3AFVCRGKG443DJDMRZHY", sheet = "BASE"),
              list(file = "014N3L2U2ZDHOZ6MIMVREZ5G7WYU3HGKD2", sheet = "Base"))

margenes <- map_dfr(anios, function(a) {
  
  skip_rows <- ifelse(
    grepl("2019", a$sheet, ignore.case = TRUE),
    2,
    0
  )
  
  df <- suppressWarnings(
    read_excel_from_onedrive(
      a$file,
      usr,
      sheet = a$sheet,
      skip  = skip_rows
    )
  )
  
  names_df <- names(df)
  
  sacos_col <- names_df[
    grepl("SS 70|SACOS70|SACOS 70",
          names_df,
          ignore.case = TRUE)
  ]
  
  if (length(sacos_col) == 0)
    sacos_col <- names_df[
      grepl("SACOS", names_df,
            ignore.case = TRUE)
    ][1]
  
  if (length(sacos_col) == 0 ||
      is.na(sacos_col))
    sacos_col <- NULL
  
  cols_base <- c(
    "SUCURSAL", "TIP", "MARCA",
    "FACTURA", "LOTE",
    "CLIENTE", "KILOS", "MES"
  )
  
  df <- df %>%
    select(
      any_of(cols_base),
      LINNEG = any_of(
        c("LIN.NEG", "LIN NEG")
      ),
      FECFACTURA = any_of(
        c("FEC FACTURA",
          "FECHA FACTURA")
      ),
      PRODUCTO = any_of(
        c("PRODUCTO",
          "PRODUCTO SEGUN MARCA")
      ),
      MARGEN = any_of(
        c("MNFCC$", "MARGEN NETO")
      ),
      any_of(sacos_col)
    )
  
  if (!is.null(sacos_col) &&
      sacos_col %in% names(df))
    names(df)[
      names(df) == sacos_col
    ] <- "SACOS70"
  
  if (!"SACOS70" %in% names(df))
    df$SACOS70 <- NA
  
  if (!"FECFACTURA" %in% names(df))
    df$FECFACTURA <- NA
  
  if (!"MES" %in% names(df))
    df$MES <- NA
  
  df <- df %>%
    mutate(
      FECFACTURA =
        suppressWarnings(
          as.Date(
            FECFACTURA,
            origin = "1900-01-01"
          )
        ),
      MES = ifelse(
        is.na(MES),
        NA_character_,
        ifelse(
          is.numeric(MES),
          toupper(meses_es[MES]),
          toupper(MES)
        )
      )
    )
  
  gc()
  df
}) %>%
  filter(!is.na(LOTE) &
           LOTE != 0)

margenes %>% mutate(Fecha = year(FECFACTURA)) %>% select(Fecha) %>% Unicos()

SubirDatos(margenes, "CRMNALMARLOT")
rm(margenes)
gc()

## Productos ----

productos <- CargarExcelOneDrive("hcyate", 
                                 "2024/Industria Nacional/CRM/Tablas de Homologación",
                                 "Productos.xlsx") %>% 
  openxlsx2::read_xlsx() 

SubirDatos(productos, "CRMNALPRODS")

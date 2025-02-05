Sys.setlocale("LC_TIME", "es_ES.UTF-8")

## Librerias ----
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(bslib)
library(connectwidgets)
library(DT)
library(DBI)
library(httr)
library(racafe)

get_access_token <- function() {
  url <- paste0("https://login.microsoftonline.com/", tenant_id, "/oauth2/v2.0/token")
  payload <- list(
    grant_type = 'client_credentials',
    client_id = client_id,
    client_secret = client_secret,
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

### Tabs ----
source("tabs/portal.R", encoding = "UTF-8")

### Valores ----
Tarjeta <- function(tit, desc, url, img, act, ver, per, are){
  
  f_act <- format(act, "%d/%m/%Y, %H:%M:%S", tz="America/Bogota")
  
  ver=ifelse(is.na(ver), "", ver)
  
  out <- paste0(fluidRow(style = "border-style:solid;border-width: 1px;border-color: #E5E8E8 ;border-radius: 2px",
                         column(2, 
                                paste0("<center><a href ='", url, "'><img height='100%' width='100%' src='", img, "'/></a></center>" )%>% HTML
                         ),
                         column(10, 
                                tags$span(
                                  paste(
                                    "<a href='", url, "'><h2 style='color=#3498DB';>", tit, "</h2></a>",
                                    tags$p(style = "font-weight:bold;font-size:90%", desc),
                                    tags$p(style = "font-weight:bold;font-size:100%", ver %>% HTML),
                                    tags$p(style = "font-weight:bold;font-size:70%color:black", paste0("Reporte actualizado el ", f_act)),
                                    br(),
                                    tags$div(
                                      lapply(str_split(are, ",", simplify = T), function(x){
                                        paste0("<span style='display: inline-block;padding: 0.35em 0.65em;font-size: 70%; background-color: #007bff;
                                                font-weight: 600;color: white;text-align: center;border-radius: 0.25rem;'>", x, "</span>")}) %>% 
                                        unlist() %>% HTML
                                    ),
                                    tags$div(
                                      lapply(str_split(per, ",", simplify = T), function(x){
                                        paste0("<span style='display: inline-block;padding: 0.35em 0.65em;font-size: 70%; background-color: #000000;
                                                font-weight: 600;color: white;text-align: center;border-radius: 0.25rem;'>", x, "</span>")}) %>% 
                                        unlist() %>% HTML
                                      ),
                                    br()
                                    ) %>% 
                                    HTML
                                  )
                                )
                         ), br()
                ) %>% HTML
  
  return(out)
}
lang <- list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/es-ES.json')

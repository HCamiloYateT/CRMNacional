tictoc::tic("Transaccionalidad Clientes")
print(paste0("***********", Sys.time(), "***********"))
setwd("/home/htamara/6_IndustriaNacional/CRM Cliente Nacional")
options(scipen = 99999)

# librerias ----
pkgs <- c("racafe", "tidyverse", "DBI", "lubridate", "rfm", "CLVTools")
racafe::Loadpkg(pkgs)
uid = "AppQlikRcaLee"
pwd = "Rac@CafQlikS2022*"

# Funciones -----
SubirDatos <- function(df, db){
  con <- dbConnect(RMySQL::MySQL(), dbname = "Analitica", host = "localhost",
                   port = 3306, user = 'datos', password = 'R4c4f3*1', DBMSencoding = "UTF-8")
  DBI::dbWriteTable(con, db, df, row.names = F , append = T, encoding = "latin1")
  dbDisconnect(con)
}
SubirDatos2 <- function(df, db){
  con <- dbConnect(RMySQL::MySQL(), dbname = "Analitica", host = "localhost",
                   port = 3306, user = 'datos', password = 'R4c4f3*1', DBMSencoding = "UTF-8")
  DBI::dbWriteTable(con, db, df, row.names = F ,overwrite = T, encoding = "latin1")
  dbDisconnect(con)
}
CargarDatos <- function(tabla){
  # Establece una conexiÃ³n a la base de datos MySQL
  con <- dbConnect(RMySQL::MySQL(), dbname = "Analitica", host = "localhost",
                   port = 3306, user = 'datos', password = 'R4c4f3*1', DBMSencoding = "UTF-8")
  
  # Establece el juego de caracteres UTF-8 para la conexión
  dbGetQuery(con, "SET NAMES 'utf8'") 
  
  # Ejecuta una consulta SQL para cargar los datos de la tabla especificada
  aux1 <- dbGetQuery(con, paste("select *  from", tabla))
  
  # Cierra la conexiÃ³n a la base de datos
  dbDisconnect(con)
  
  # Devuelve los datos cargados en el objeto aux1
  return(aux1)
}



# Lectura de Datos ----
print("**** Cargar Datos ****")
load("CRMNacional/data/data.RData")
## Auxiliares ----

personas <- CargarDatos("CRMNALCLIENTE") %>% 
  mutate(FecProceso = as.Date(FecProceso)) %>% 
  arrange(CliNitPpal) %>% 
  group_by(LinNegCod, CliNitPpal) %>% 
  filter(FecProceso == max(FecProceso),
         row_number() == 1)
  
# Clientes nuevos.
pers_nuevas <- data |> 
  select(CliNitPpal, LinNegCod, Usuario) |> 
  distinct() |> 
  anti_join(
    CargarDatos("CRMNALCLIENTE")  |> 
      select(CliNitPpal, LinNegCod) |> 
      distinct(),
    by = join_by(CliNitPpal, LinNegCod)
    )


# Cliente a Recuperar (Regla general de despachos) ----
print("Segmento Racafé")

ana_dates <- seq.Date(as.Date("2020-02-01"), PrimerDia(Sys.Date()), by = "month")

seg <- lapply(ana_dates, function(x){
  data %>%
    filter(FecFact < x) %>%
    left_join(personas %>% 
                select(LinNegCod, CliNitPpal, NumMesesRecuperar) %>% 
                distinct(),
              by = join_by(CliNitPpal, LinNegCod)
              ) %>% 
    group_by(LinNegCod, CliNitPpal, NumMesesRecuperar) %>%
    summarise(FecUltDesp = max(FecFact, na.rm = T),
              .groups = "drop") %>%
    mutate(Meses = ifelse(is.na(NumMesesRecuperar), 4, NumMesesRecuperar),
           SegmentoRacafe = ifelse(FecUltDesp >= PrimerDia(x)- months(Meses), "CLIENTE", "CLIENTE A RECUPERAR"),
           FecProceso = x) %>%
    select(-FecUltDesp, -NumMesesRecuperar)
  }) %>% bind_rows 

data |> 
  group_by(CliNitPpal, LinNegCod, Usuario) |> 
  summarise(FecFact = max(FecFact)) |> 
  distinct() |> 
  anti_join(
    seg  |> 
      select(CliNitPpal, LinNegCod) |> 
      distinct(),
    by = join_by(CliNitPpal, LinNegCod)
  )

SubirDatos2(seg, "CRMNALSEGR")


# RFM VOLUMEN ----
print("RFM Volumen")

data_rfm <- data %>%
  left_join(personas %>% 
              select(LinNegCod, CliNitPpal, starts_with("Segmento2")),
            by = join_by(LinNegCod, CliNitPpal)) %>% 
  filter(!is.na(FecFact),
         FecFact < PrimerDia(Sys.Date()),
         FecFact >= PrimerDia(Sys.Date()) - years(1)
         ) %>% 
  select(customer_id = CliNitPpal, starts_with("Segmento2"),
         LinNegCod, order_date = FecFact,
         revenue = SacDesp)

ana_dates <- seq.Date(as.Date("2020-02-01"), 
                      PrimerDia(Sys.Date()) , 
                      by = "month") - days(1)

rfm_s <- do.call("bind_rows", lapply(ana_dates, function(x) {
  
  aux0 <- data_rfm %>% 
    mutate(FecProceso = x + 1,
           Segmento = ifelse(year(FecProceso) <= 2024, Segmento2024,  Segmento2025)) %>%
    left_join(CargarDatos("CRMNALSEGR") %>% 
                mutate(FecProceso = as.Date(FecProceso)) %>% 
                select(LinNegCod, CliNitPpal, FecProceso, SegmentoRacafe), 
              by = c("LinNegCod"="LinNegCod","customer_id" = "CliNitPpal", "FecProceso" = "FecProceso")) %>%
    filter(order_date <= x) %>% 
    select(- starts_with("Segmento2"))
  
  segs <- c("DETAL","MEDIANO", "GRANDES")
  segs2 <- c("CLIENTE A RECUPERAR","CLIENTE")
  segs3 <- c(10000, 21000)
  
  t1 <- do.call("bind_rows", lapply(segs, function(y) {
    do.call("bind_rows", lapply(segs2, function(z) {
      do.call("bind_rows", lapply(segs3, function(xx) {
        
        df <- aux0 %>% 
          filter(Segmento == y,
                 SegmentoRacafe == z,
                 LinNegCod == xx)
        
        if (nrow(df) > 0) {
          aux1 <- rfm_table_order(df, customer_id, order_date, revenue, x)
          
          segment_names <- c("CAMPEONES", "CLIENTES LEALES", "POTENCIALES LEALES",
                             "NUEVOS CLIENTES", "PROMETEDORES", "NECESITAN ATENCIÓN",
                             "A PUNTO DE DORMIR", "EN RIESGO", "NO PODEMOS PERDERLOS",
                             "HIBERNANDO", "PERDIDOS")
          
          recency_lower   <- c(4, 2, 3, 4, 3, 3, 2, 1, 1, 2, 1)
          recency_upper   <- c(5, 4, 5, 5, 4, 4, 3, 2, 1, 3, 1)
          frequency_lower <- c(4, 3, 1, 1, 1, 3, 1, 2, 4, 2, 1)
          frequency_upper <- c(5, 4, 3, 1, 1, 4, 2, 5, 5, 3, 1)
          monetary_lower  <- c(4, 4, 1, 1, 1, 3, 1, 2, 4, 2, 1)
          monetary_upper  <- c(5, 5, 3, 1, 1, 4, 2, 5, 5, 3, 1)
          
          segments <- rfm_segment(aux1, segment_names, recency_lower,
                                  recency_upper, frequency_lower, frequency_upper,
                                  monetary_lower, monetary_upper)
          
          res <- segments %>%
            mutate(FecProceso = x + days(1),
                   segment = ifelse(segment == "Others", "OTROS", segment)) %>%
            rename(SegmentoAnalitica = segment,
                   CliNitPpal = customer_id)
          return(res)
          
        }
        
        }))
      }))
    }))
  }))

data |> 
  group_by(CliNitPpal, LinNegCod, Usuario) |> 
  summarise(FecFact = max(FecFact)) |> 
  distinct() |> 
  anti_join(
    rfm_s  |> 
      select(CliNitPpal, LinNegCod) |> 
      distinct(),
    by = join_by(CliNitPpal, LinNegCod)
  )

SubirDatos2(rfm_s, "CRMNALRFM")



# RFM MARGEN ----
print("RFM Márgen")

data_rfm <- data %>%
  left_join(personas %>% 
              select(LinNegCod, CliNitPpal, starts_with("Segmento2")),
            by = join_by(LinNegCod, CliNitPpal)) %>% 
  filter(!is.na(FecFact),
         FecFact < PrimerDia(Sys.Date()),
         FecFact >= PrimerDia(Sys.Date()) - years(1),
         !is.na(Margen)
         ) %>% 
  select(customer_id = CliNitPpal, starts_with("Segmento2"),
         LinNegCod, order_date = FecFact,
         revenue = Margen)

ana_dates <- seq.Date(as.Date("2020-02-01"), 
                      PrimerDia(Sys.Date()) , 
                      by = "month") - days(1)

rfm_m <- do.call("bind_rows", lapply(ana_dates, function(x) {
  
  aux0 <- data_rfm %>% 
    mutate(FecProceso = x + 1,
           Segmento = ifelse(year(FecProceso) <= 2024, Segmento2024,  Segmento2025)) %>%
    left_join(CargarDatos("CRMNALSEGR") %>% 
                mutate(FecProceso = as.Date(FecProceso)) %>% 
                select(LinNegCod, CliNitPpal, FecProceso, SegmentoRacafe), 
              by = c("LinNegCod"="LinNegCod","customer_id" = "CliNitPpal", "FecProceso" = "FecProceso")) %>%
    filter(order_date <= x) %>% 
    select(- starts_with("Segmento2"))
  
  segs <- c("DETAL","MEDIANO", "GRANDES")
  segs2 <- c("CLIENTE A RECUPERAR","CLIENTE")
  segs3 <- c(10000, 21000)
  
  t1 <- do.call("bind_rows", lapply(segs, function(y) {
    do.call("bind_rows", lapply(segs2, function(z) {
      do.call("bind_rows", lapply(segs3, function(xx) {
        
        df <- aux0 %>% 
          filter(Segmento == y,
                 SegmentoRacafe == z,
                 LinNegCod == xx)
        
        if (nrow(df) > 0) {
          aux1 <- rfm_table_order(df, customer_id, order_date, revenue, x)
          
          segment_names <- c("CAMPEONES", "CLIENTES LEALES", "POTENCIALES LEALES",
                             "NUEVOS CLIENTES", "PROMETEDORES", "NECESITAN ATENCIÓN",
                             "A PUNTO DE DORMIR", "EN RIESGO", "NO PODEMOS PERDERLOS",
                             "HIBERNANDO", "PERDIDOS")
          
          recency_lower   <- c(4, 2, 3, 4, 3, 3, 2, 1, 1, 2, 1)
          recency_upper   <- c(5, 4, 5, 5, 4, 4, 3, 2, 1, 3, 1)
          frequency_lower <- c(4, 3, 1, 1, 1, 3, 1, 2, 4, 2, 1)
          frequency_upper <- c(5, 4, 3, 1, 1, 4, 2, 5, 5, 3, 1)
          monetary_lower  <- c(4, 4, 1, 1, 1, 3, 1, 2, 4, 2, 1)
          monetary_upper  <- c(5, 5, 3, 1, 1, 4, 2, 5, 5, 3, 1)
          
          segments <- rfm_segment(aux1, segment_names, recency_lower,
                                  recency_upper, frequency_lower, frequency_upper,
                                  monetary_lower, monetary_upper)
          
          res <- segments %>%
            mutate(FecProceso = x + days(1),
                   segment = ifelse(segment == "Others", "OTROS", segment)) %>%
            rename(SegmentoAnalitica = segment,
                   CliNitPpal = customer_id)
          return(res)
          
        }
        
      }))
    }))
  }))
}))

data |> 
  group_by(CliNitPpal, LinNegCod, Usuario) |> 
  summarise(FecFact = max(FecFact)) |> 
  distinct() |> 
  anti_join(
    rfm_m  |> 
      select(CliNitPpal, LinNegCod) |> 
      distinct(),
    by = join_by(CliNitPpal, LinNegCod)
  )

SubirDatos2(rfm_m, "CRMNALRFMM")

# CLV ----
print("CLV")
ana_dates <- c(seq.Date(as.Date("2020-01-01"), PrimerDia(Sys.Date()), by = "month") - days(1))
lapply(ana_dates, function(x) {
  unidad <- "weeks"
  
  apparelTrans <- data %>%
    filter(FecFact < x,
           !is.na(FecFact),
           year(FecFact) >= 2020) %>%
    group_by(Id = CLCliNit, Date = PrimerDia(FecFact, uni = unidad)) %>%
    summarise(Price = sum(SacDesp), .groups = "drop")

  # Definir el corte de estimación (1 mes antes de la última transacción)
  fec_split <- PrimerDia(max(apparelTrans$Date)) - months(1) - days(1)

  # Filtrar transacciones para clientes con primeras transacciones dentro del período válido
  apparelTrans_filtered <- apparelTrans %>%
    group_by(Id) %>%
    mutate(first_transaction = min(Date)) %>%
    filter(first_transaction <= fec_split) %>%
    ungroup()

  results <- tryCatch({
    clv.apparel <- CLVTools::clvdata(apparelTrans_filtered,
                                      date.format = "ymd",
                                      time.unit = unidad,
                                      name.id = "Id",
                                      name.date = "Date",
                                      name.price = "Price",
                                      estimation.split = fec_split)

    # Estimar el modelo Pareto/NBD
    est.pnbd <- CLVTools::pnbd(clv.data = clv.apparel)

    CLVTools::predict(est.pnbd, prediction.end = PrimerDia(x) + months(1) - days(1)) %>%
      mutate(SacosPred = pmax(0, round(predicted.total.spending)),
             FecProceso = x + days(1),
             Churn = 1 - PAlive) %>%
      select(FecProceso, CliNitPpal = Id, Churn, SacosPred)

  }, error = function(e) {
    message(paste("Error procesando la fecha", x, ":", e$message))
    results <- data.frame()
  })

  return(results)
  }) %>% 
  bind_rows() %>% 
  SubirDatos2("CRMNALCLV")

# limpieza ----
rm(list = ls())
gc()
tictoc::toc()
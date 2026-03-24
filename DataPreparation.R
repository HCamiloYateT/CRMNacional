# Configuración inicial y carga de librerías
tictoc::tic("CRM NACIONAL")
print(paste0("*********** ", Sys.time(), " ***********"))
setwd("/home/htamara/6_IndustriaNacional/CRM Cliente Nacional")
options(scipen = 99999)

# Librerias --------------------------------------------------------------------
required_packages <- c("racafe", "tidyverse", "DBI", "lubridate", "httr", "readxl", 
                       "RMySQL", "openxlsx2", "rfm", "CLVTools", "fuzzyjoin")
racafe::Loadpkg(required_packages)

# Funciones ----
# Leer hojas de un archivo Excel desde OneDrive
# Obtener contenido de un archivo
get_file_content <- function(file_id, user) {
  access_token <- racafe::ObtenerTokenAcceso()
  drive_id <- racafe::ObtenerIdDrive(user)
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/items/", file_id, "/content")
  headers <- add_headers(Authorization = paste("Bearer", access_token))
  temp_file <- "temp_file.xlsx"
  response <- GET(url, headers, write_disk(temp_file, overwrite = TRUE))
  if (http_status(response)$category != "Success") {
    stop("Error al descargar archivo: ", content(response, as = "text"))
    }
  return(temp_file)
  }
read_excel_sheets_from_onedrive <- function(file_id, user) {
  file_path <- get_file_content(file_id, user)
  on.exit(file.remove(file_path))
  return(excel_sheets(file_path))
}
CargarExcelPorId <- function(id_archivo, hoja, usuario) {
  stopifnot(is.character(id_archivo), is.character(hoja), is.character(usuario))
  
  headers <- CabecerasGraph()
  drive_id <- ObtenerIdDrive(usuario)
  
  url <- paste0(
    "https://graph.microsoft.com/v1.0/drives/",
    drive_id,
    "/items/",
    id_archivo,
    "/content"
  )
  
  resp <- httr::GET(url, headers)
  
  if (httr::status_code(resp) == 200) {
    tmp <- tempfile(fileext = ".xlsx")
    
    on.exit({
      if (file.exists(tmp)) file.remove(tmp)
    }, add = TRUE)
    
    writeBin(httr::content(resp, "raw"), tmp)
    
    datos <- openxlsx2::read_xlsx(tmp, sheet = hoja)
    return(datos)
    
  } else {
    stop(
      "Error al descargar archivo: ",
      httr::status_code(resp), 
      " - ",
      httr::content(resp, "text")
    )
  }
}

# Procesamiento principal ------------------------------------------------------
## 1. Cargar datos de clientes ----
# Ya se lee directo desde la BDD: "CRMNALCLIENTE"
## 2. Cargar datos de leads ----
print("Cargando datos de leads...")
leads <- CargarExcelDesdeOneDrive("hcyate", 
                                  "2024/Industria Nacional/CRM/Tablas de Homologación",
                                  "Leads.xlsx") %>% 
  openxlsx2::read_xlsx(sheet = "BD LEADS") %>% 
  mutate(Descartada        = as.logical(NA),
         UsuarioDescarte   = as.character(NA),
         FechaHoraDescarte = as.POSIXct(NA),
         across(where(is.character), racafe::LimpiarNombres)
         ) %>% 
  relocate(`RAZON DESCARTADO`, `PRECIO DESCARTADO`, `CALIDAD DESCARTADO`, .after = last_col())

names(leads) <- c('UsuarioCrea','FechaHoraCrea','UsuarioMod','FechaHoraModi','SegmentoRacafe','Asesor','PerRazSoc','PerCod','AutorizaTD',
                  'Origen','DetOrigen','CodLinNegocio','LinNegocio','Segmento','Aliado','EstadoCuenta',
                  'RazonInteres','Pais','Depto','Mpio','via','via_num','km_num','km_com','via_let','bis','complemento',
                  'cardinalidad','com_num','com_let','com2_num','com2_card','compl','lng','lat','Comentarios','Nombres','Cargos','Telefonos',
                  'Mails', 'Descartada', 'UsuarioDescarte', 'FechaHoraDescarte', 'RazonDescartado','DescartadoPrecio', 'DescartadoCalidad')

# Guardar datos de clientes en la base de datos
EscribirDatos(leads, "CRMNALLEAD")

## 3. Cargar datos de márgenes ----
print("Cargando datos de márgenes...")

# Función para cargar datos de márgenes
load_margin_data <- function(usr, file_id, sheet_name) {
  CargarExcelPorId(id_archivo = file_id, hoja = sheet_name, usuario = usr) %>%
    select(SUCURSAL, TIP, LINNEG = LIN.NEG, MARCA, FACTURA, FECFACTURA = `FEC FACTURA`, LOTE, CLIENTE, 
           PRODUCTO =`PRODUCTO SEGUN MARCA`, SACOS70 = `SS 70 KLS` ,KILOS, MARGEN = `MNFCC$`,
           MES) %>%  
    mutate(FECFACTURA = as.Date(FECFACTURA, origin = "1900-01-01"))
}
usr <- "wmunozs"

# Obtener la carpeta del año más reciente
anho <- ListarContenidoCarpetaId(usr, "014N3L2U2EH7DJE5B6V5AK6USIEW5IJH2N") %>% # Informes Café
  filter(str_detect(name, "^AÑO\\s\\d{4}$")) %>%
  mutate(year_num = as.numeric(gsub("^AÑO (\\d{4})$", "\\1", name))) %>%
  slice_max(year_num, n = 1) %>%  
  select(name, id)

# Si no hay carpeta de año, salir
if (nrow(anho) == 0) {
  message("No se encontró carpeta de año")
  return(NULL)
}

# Obtener la carpeta del mes más reciente
mes <- ListarContenidoCarpetaId(usr, anho$id) %>%
  filter(grepl("^\\d{2}\\.", name)) %>%
  mutate(month_num = as.numeric(sub("^(\\d{2})\\. .*$", "\\1", name))) %>%
  slice_max(month_num, n = 1) %>%  
  select(name, id)

# Si no hay carpeta de mes, salir
if (nrow(mes) == 0) {
  message("No se encontró carpeta de mes")
  return(NULL)
}

# Buscar archivos PYG
pyg_file <- ListarContenidoCarpetaId(usr, mes$id) %>%
  filter(grepl("^\\d{2}\\. PYG POR LOTE AÑO \\d{4}.*(COPRODUCTOS|A LA MEDIDA)\\.xlsx$", 
               name, ignore.case = TRUE)) %>%
  select(name, id, type)

# Procesar archivos si existen
if (nrow(pyg_file) > 0) {
  # Obtener datos históricos
  all_margin_data <- tryCatch({
    Consulta("SELECT * FROM CRMNALMARLOT WHERE LOTE != 0") %>% 
      mutate(FECFACTURA = as.Date(FECFACTURA)) %>% 
      filter(year(FECFACTURA) != year(Sys.Date()))
  }, error = function(e) {
    message("Error al obtener datos históricos: ", e$message)
    return(data.frame())
  })
  
  # Procesar cada archivo
  for (i in 1:nrow(pyg_file)) {
    current_file <- pyg_file[i, ]
    
    # Obtener hojas del archivo actual
    sheets <- tryCatch({
      read_excel_sheets_from_onedrive(current_file$id, usr)
    }, error = function(e) {
      message("Error al leer hojas de ", current_file$name, ": ", e$message)
      return(character(0))
    })
    
    # Filtrar hojas BASE
    base_sheets <- sheets[grepl("^BASE( \\d{4})?$", sheets, ignore.case = TRUE)]
    
    if (length(base_sheets) > 0) {
      # Si hay hojas con año, usar la más reciente
      if (any(grepl("\\d{4}", base_sheets))) {
        year_numbers <- as.numeric(gsub(".*?(\\d{4}).*", "\\1", 
                                        base_sheets, perl = TRUE))
        year_numbers[is.na(year_numbers)] <- 0
        selected_sheet <- base_sheets[which.max(year_numbers)]
      } else {
        # Si no hay años, usar la primera hoja BASE
        selected_sheet <- base_sheets[1]
      }
      
      # Cargar datos del archivo
      tryCatch({
        margin_data <- load_margin_data(usr, current_file$id, selected_sheet) %>% 
          filter(LOTE != 0)
        
        # Combinar con datos existentes
        all_margin_data <- bind_rows(all_margin_data, margin_data)
        message("Procesado archivo: ", current_file$name, ", hoja: ", selected_sheet)
      }, error = function(e) {
        message("Error al cargar datos de ", current_file$name, 
                ", hoja ", selected_sheet, ": ", e$message)
      })
    } else {
      message("No se encontraron hojas BASE en ", current_file$name)
    }
  }
  
  # Guardar datos si hay algo que guardar
  if (nrow(all_margin_data) > 0) {
    tryCatch({
      EscribirDatos(all_margin_data, "CRMNALMARLOT")
      message("Datos guardados correctamente en CRMNALMARLOT")
    }, error = function(e) {
      message("Error al guardar datos: ", e$message)
    })
  } else {
    message("No hay datos para guardar")
  }
} else {
  message("No se encontraron archivos PYG")
}


# Cargar datos de márgenes completos para procesamiento
margenes <- Consulta("SELECT * FROM CRMNALMARLOT WHERE LOTE != 0")

## 4. Cargar datos de productos ----
print("Cargando datos de productos...")
productos <- CargarExcelDesdeOneDrive("hcyate", 
                                      "2024/Industria Nacional/CRM/Tablas de Homologación",
                                      "Productos.xlsx") %>% 
  openxlsx2::read_xlsx() %>% 
  mutate(Usr = "HCYATE", 
         across(where(is.character), \(x) replace_na(x, "SIN DATO"))) %>% 
  pivot_longer(cols = matches("^Categoria|^Producto"), names_to = c(".value", "Anho"), names_pattern = "(Categoria|Producto)(20\\d{2})") %>%
  mutate(FecProceso = as.Date(paste0(Anho, "-01-01"))) %>%
  select(FecProceso, Usr,LinNegCod, LinNeg, LinProCod, LinProNom,
         MCCod, MCNom, MrcCod, Marca, Excluir, Categoria, Producto) %>% 
  mutate(across(where(is.character), racafe::LimpiarNombres))

EscribirDatos(productos, "CRMNALPRODS")

## 5. Cargar datos principales ----
print("Cargando datos principales...")

# Cargar sucursales
sucs <- data.frame(
  SucCod = c(12, 15, 20, 26, 30, 32, 35, 50, 55),
  Sucursal = c("Trilladora 12","Bachué", "Medellín", "Popayán", "Armenia", "Arenales", "Pereira", "Bucaramanga", "Huila")) %>% 
  mutate(across(where(is.character), LimpiarNombres))

# Consulta SQL para obtener NIT principal
NITPPAL <- Consulta("SELECT DISTINCT FecProceso, CLCliNit AS PerCod, CliNitPpal FROM CRMNALCLIENTE") %>% 
  mutate(FecProceso = as.Date(FecProceso)) %>% 
  group_by(PerCod) %>% 
  filter(FecProceso == max(FecProceso)) %>% 
  select(-FecProceso)

# Consulta SQL para obtener datos de clientes NITPPAL
NCLIENTE <- ConsultaSistema("syscafe", 
                            query = "SELECT c.CliNit AS PerCod, c.CliCont, c.CliDir, 
                                            c.CliDir1, c.CliTel, c.CliConCom, c.CliTelCom, 
                                            c.CliEmlCom, c.CiuExtCod, c.CliFPagDbl,
                                            ce.CiuExtNom, p.PerRazSoc
                                     FROM NCLIENTE c
                                     LEFT JOIN NCIUEXT ce ON c.CiuExtCod = ce.CiuExtCod
                                     LEFT JOIN NPERSONA p ON c.CliNit = p.PerCod") %>%
  left_join(NITPPAL, by = "PerCod") %>%
  mutate(CliNitPpal = ifelse(is.na(CliNitPpal), PerCod, CliNitPpal),
         across(where(is.character), \(x) ifelse(x %in% c("","."), NA, x)))

# Consulta SQL para obtener unidades de medida
NUNIMEDI <- ConsultaSistema("syscafe", 
                            query =  "SELECT UMeCod, UMeNom AS UnidadMedida, UMeFac, UMeSgl
                                      FROM NUNIMEDI")

# Consulta SQL para obtener datos de pedidos

ConsultaSistema("syscafe", "select top 10 * from EXPPEDID where PdcCod = 44423") %>% glimpse()

ped <- ConsultaSistema("syscafe", 
                       query = "SELECT p1.PdcCod, pd.PdcCntCli as PdcRefCli, pd.PdcFecCre, p1.PdcLin, p1.PdcCan, pd.PdcUsu AS Usuario, 
                                       um.UMeFac, pd.PdcTipCaf as TipCaf, pd.PdcPrePes as PdcPrecioKilo
                                       FROM EXPPEDI1 p1
                                       INNER JOIN EXPPEDID pd ON p1.PdcCod = pd.PdcCod
                                       LEFT JOIN NUNIMEDI um ON pd.UMeCod = um.UMeCod
                                       WHERE p1.CiaCod = 10 AND pd.CiaCod = 10 AND 
                                             pd.CliNit <> 32 AND pd.PdcEst = 'A' AND 
                                             pd.PdcVtaNal = 1") 

# Margenes y Fecha de Factura
fact <- ConsultaSistema("syscafe",
                        query = "SELECT F1.FcnLot AS CLLotCod, F2.FcnTip,
                                        MAX(F2.FcnFec) AS FecFact, 
                                        SUM(F1. FcnKilLot) AS KilosFact, 
                                        SUM(F1.FcnSacLot) AS SacosFact,
                                        SUM(F1. FcnKilLot / 70) AS SacFact70
                                 FROM FCTFACN1 F1
                                 LEFT JOIN FCTFACNA F2 ON F1.FcnNum = F2.FcnNum
                                 WHERE F1.CiaCod = 10 AND F2.CiaCod = 10 AND F2.FcnEtd = 'C'
                                GROUP BY F1.FcnLot, F2.FcnTip") %>% 
  left_join(Consulta("SELECT LOTE AS CLLotCod, SUM(MARGEN) AS Margen, sum(KILOS/70) as SacosPYG  
                            FROM CRMNALMARLOT
                            WHERE TIP = 'NAL'
                            GROUP BY LOTE"),
             by = join_by(CLLotCod)) %>% 
  filter(FecFact >= as.Date("2020-01-01"))

# Facturas Historicas
FACT <- ConsultaSistema("syscafe", 
                        query = "SELECT F2.FctNit,
                                        MAX(F2.FcnFec) AS MinFecFact, 
                                        MAX(F2.FcnFec) AS UltFecFact, 
                                        SUM(F1.FcnKilLot) AS KilosFact, 
                                        SUM(F1.FcnSacLot) AS SacosFact,
                                        SUM(F1.FcnKilLot / 70) AS SacFact70
                                 FROM FCTFACN1 F1
                                 LEFT JOIN FCTFACNA F2 
                                        ON F1.FcnNum = F2.FcnNum
                                 WHERE F1.CiaCod = 10 AND F2.CiaCod = 10 AND F2.FcnEtd = 'C'
                                 GROUP BY F2.FctNit, F1.FcnLot, F2.FcnTip") %>% 
  left_join(NCLIENTE %>% select(PerCod, PerRazSoc) %>% distinct(),
            by = c("FctNit"="PerCod"))
# Produccion y Despachos
prod <- ConsultaSistema("cafesys",
                        query = "SELECT ExcCod, ExcFch AS FecDesp, ExcSacD + ExcSacF AS SacProd 
                                 FROM EXCMVI 
                                 WHERE MTipCod = 2")

desp <- ConsultaSistema("cafesys",
                        query = "SELECT ExcCod, ExcFch AS FecProd, ExcSacD + ExcSacF AS SacProd 
                                 FROM EXCMVI 
                                 WHERE MTipCod = 3")

# Consulta SQL cuadro de lotes
data <- ConsultaSistema("syscafe",
                        query = "SELECT CLSucCod, CLLotCod, CLPdcCod, CLPdcLin, 
                                        CLPdcAnoEm*100 + CLPdcMesEm AS Periodo,
                                        CLPdcCntCl, CLLotCan AS SacLote, CLLotFec AS FecAsignLote,
                                        CLotrCod AS CodOrdTril, CLOTrFec AS FecOrdTril,
                                        CLLotSacPr AS SacProd, CLLotFecPr AS FecProd,
                                        CLIDeCod AS CodDesp, CLIDeFec AS FecDesp, CLLotSacDe AS SacDesp,
                                        CLPdcFctAD, CLLotSacFa as SacFact, CLPdcCanFa, CLLinNegCo, 
                                        CLLotSacXP, CLLotPenXD, CLLotDesXF, CLCliNit, CLCliRazSo,
                                        CLLinNegCo AS LinNegCod, CLLinNegNo, CLLinProCo AS LinProCod, 
                                        CLLinProNo, CLMCCod AS MCCod, CLMCNom, CLMrcCod AS MrcCod, CLMrcNom
                                  FROM EXPCUALO 
                                  WHERE CiaCod = 10 AND CLPdcVtaNa = 1 AND CLCliNit <> 32 AND 
                                  CLLinNegCo IN (10000, 21000) AND CLLotCan > 0") %>%
  left_join(sucs, by = c("CLSucCod" = "SucCod")) %>%
  left_join(NITPPAL, by = c("CLCliNit" = "PerCod")) %>%
  mutate(CliNitPpal = ifelse(is.na(CliNitPpal), CLCliNit, CliNitPpal)) %>%
  left_join(NCLIENTE %>% filter(PerCod == CliNitPpal) %>% select(-PerCod), by = "CliNitPpal") %>%
  left_join(ped, by = c("CLPdcCod" = "PdcCod", "CLPdcLin" = "PdcLin")) %>% 
  left_join(fact, by = c("CLLotCod")) %>%
  filter(LinNegCod == 21000 | (LinNegCod == 10000 & TipCaf != "E")) %>% 
  mutate(MarKilo = Margen / KilosFact) %>%
  group_by(CLCliNit, CLLinNegNo, LinProCod, MCCod, MrcCod) %>% 
  mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = T), MarKilo)) %>% 
  group_by(CLLinNegNo, LinProCod, MCCod, MrcCod) %>% 
  mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = T), MarKilo)) %>% 
  group_by(CLLinNegNo, LinProCod) %>% 
  mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = T), MarKilo)) %>% 
  group_by(CLLinNegNo) %>% 
  mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = T), MarKilo)) %>% 
  ungroup() %>% 
  mutate(Margen = ifelse(is.na(Margen), MarKilo * SacLote * ifelse(LinNegCod == 10000,62.5, 70), Margen),
         FechaEmbarque = as.Date(paste(Periodo, "01"), "%Y%m%d"),
         across(contains("Fec"), ~ if_else(as.Date(.) == as.Date("1753-01-01"), as.Date(NA), as.Date(.))),
         CliNitPpal = ifelse(is.na(CliNitPpal), CLCliNit, CliNitPpal),
         CLLinNegNo = ifelse(CLLinNegNo == "DIFERENCIADOS", "A LA MEDIDA", CLLinNegNo),
         LinNegCod = ifelse(LinNegCod == 20000, 21000, LinNegCod),
         Kilos = SacDesp * UMeFac) %>%
  group_by(LinNegCod, CLLinProNo) %>%
  mutate(PendProducir = SacLote - SacProd,
         PendDespachar = SacLote - pmax(SacDesp,0),
         PendFacturar = SacLote - pmax(PendDespachar,0) - coalesce(SacosFact, 0)
         ) %>%
  ungroup() %>%
  select(CLSucCod, Sucursal, CLLotCod, FechaEmbarque, CLPdcCod, PdcPrecioKilo, PdcRefCli, PdcFecCre, CLPdcLin, CLPdcCntCl, 
         Usuario, SacLote, FecAsignLote,
         CodOrdTril, FecOrdTril, SacProd, FecProd, CodDesp, FecDesp, SacDesp, 
         FecFact, KilosFact, SacosFact, SacFact70, Margen, SacosPYG, MarKilo, UMeFac, Kilos,
         LinNegCod, CLLinNegNo, LinProCod, MCCod, MrcCod,
         CLPdcFctAD, SacFact, CLPdcCanFa,
         CliNitPpal, CLCliNit, PerRazSoc, CliCont, CliDir, CliDir1,
         CliTel, CliConCom, CliTelCom, CliEmlCom, CiuExtNom,
         CLLotSacXP, CLLotPenXD, CLLotDesXF,
         PendProducir, PendDespachar, PendFacturar
         ) %>%
  mutate(across(where(is.character), ~replace_na(., "SIN DATO")))

## 6. Segmentaciones ----
identificar_clientes_faltantes <- function(df_referencia, df_comparacion) {
  df_referencia %>% 
    group_by(CliNitPpal, LinNegCod, Usuario) %>% 
    summarise(FecFact = max(FecFact), .groups = "drop") %>% 
    distinct() %>% 
    anti_join(
      df_comparacion %>% 
        select(CliNitPpal, LinNegCod) %>% 
        distinct(),
      by = join_by(CliNitPpal, LinNegCod)
    )
}
generar_fechas_analisis <- function(fecha_inicio, fecha_fin = Sys.Date(), por = "month", ajuste = 0) {
  fechas <- seq.Date(as.Date(fecha_inicio), PrimerDia(fecha_fin), by = por)
  if (ajuste != 0) fechas <- fechas + days(ajuste)
  return(fechas)
}

personas <- Consulta("SELECT * FROM CRMNALCLIENTE") %>% 
  mutate(FecProceso = as.Date(FecProceso))

### 6.1. Marcación de cliente o cliente a Recuperar (Regla general de facturacion) ----
print("Segmento Racafé")
ana_dates <- generar_fechas_analisis("2020-02-01")

# Preparar datos de personas para join (solo columnas necesarias)
personas_segmento <- personas %>% 
  select(FecProceso, LinNegCod, CliNitPpal, NumMesesRecuperar) %>% 
  complete(LinNegCod, CliNitPpal, 
           FecProceso = sort(data$FecFact %>% unique())) %>% 
  group_by(LinNegCod, CliNitPpal) %>%
  fill(NumMesesRecuperar, .direction = "updown") %>%
  ungroup() 

seg <- purrr::map_dfr(ana_dates, function(x) {
  data %>%
    filter(FecFact < x) %>%
    left_join(personas_segmento, by = c("CliNitPpal", "LinNegCod", "FecFact"="FecProceso")) %>% 
    group_by(LinNegCod, CliNitPpal, NumMesesRecuperar) %>%
    summarise(FecUltDesp = max(FecFact, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(
      Meses = ifelse(is.na(NumMesesRecuperar), 4, NumMesesRecuperar),
      SegmentoRacafe = ifelse(FecUltDesp >= PrimerDia(x) - months(Meses), "CLIENTE", "CLIENTE A RECUPERAR"),
      FecProceso = x
    ) %>%
    select(-FecUltDesp, -NumMesesRecuperar)
})
rm(personas_segmento)
identificar_clientes_faltantes(data, seg)
EscribirDatos(seg, "CRMNALSEGR")

### 6.2. RFM ----

personas_rfm <- personas %>% 
  select(FecProceso, LinNegCod, CliNitPpal, Segmento) %>% 
  complete(LinNegCod, CliNitPpal, 
           FecProceso = sort(data$FecFact %>% unique())) %>% 
  group_by(LinNegCod, CliNitPpal) %>%
  fill(Segmento, .direction = "updown") %>%
  ungroup()

# Función para procesar RFM
procesar_rfm <- function(data_source, variable_revenue, tabla_destino) {
  print(paste("RFM", variable_revenue))
  
  # Preparar datos para RFM
  data_rfm <- data_source %>%
    left_join(personas_rfm,
              by = c("CliNitPpal", "LinNegCod", "FecFact"="FecProceso")) %>% 
    filter(!is.na(FecFact),
           FecFact < PrimerDia(Sys.Date()),
           !is.na({{ variable_revenue }})
           ) %>% 
    select(customer_id = CliNitPpal, Segmento, LinNegCod, 
           order_date = FecFact, revenue = {{ variable_revenue }}
           ) %>% 
    filter(!is.na(revenue))
  
  # Fechas de análisis
  ana_dates <- generar_fechas_analisis("2020-02-01", ajuste = -1)
  
  # Cargar segmentos Racafé una sola vez
  segmentos_racafe <- Consulta("select * from CRMNALSEGR") %>% 
    mutate(FecProceso = as.Date(FecProceso)) %>% 
    select(LinNegCod, CliNitPpal, FecProceso, SegmentoRacafe)
  
  # Definir segmentos para análisis
  segs <- c("DETAL", "MEDIANO", "GRANDES")
  segs2 <- c("CLIENTE A RECUPERAR", "CLIENTE")
  segs3 <- c(10000, 21000)
  
  # Definir parámetros de segmentación RFM
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
  
  # Procesar RFM para cada fecha
  rfm_results <- purrr::map_dfr(ana_dates, function(x) {
    # Filtrar datos para la fecha actual
    aux0 <- data_rfm %>% 
      mutate(FecProceso = x + 1) %>%
      left_join(segmentos_racafe %>% filter(FecProceso == x + 1),
                by = c("LinNegCod", "customer_id" = "CliNitPpal", "FecProceso")) %>%
      filter(order_date <= x,
             order_date > x - years(1)) 
    
    # Procesar cada combinación de segmentos
    purrr::map_dfr(segs, function(y) {
      purrr::map_dfr(segs2, function(z) {
        purrr::map_dfr(segs3, function(xx) {
          # Filtrar por segmento
          df <- aux0 %>% 
            filter(
              Segmento == y,
              SegmentoRacafe == z,
              LinNegCod == xx
            )
          
          # Procesar RFM si hay datos
          if (nrow(df) > 0) {
            # Calcular tabla RFM
            aux1 <- rfm_table_order(df, customer_id, order_date, revenue, x)
            
            # Segmentar clientes
            segments <- rfm_segment(
              aux1, segment_names, 
              recency_lower, recency_upper, 
              frequency_lower, frequency_upper,
              monetary_lower, monetary_upper
            )
            
            # Formatear resultados
            segments %>%
              mutate(
                FecProceso = x + days(1),
                segment = ifelse(segment == "Others", "OTROS", segment)
              ) %>%
              rename(
                SegmentoAnalitica = segment,
                CliNitPpal = customer_id
              )
          } else {
            # Devolver dataframe vacío si no hay datos
            data.frame()
          }
        })
      })
    })
  })
  
  # Verificar clientes faltantes
  identificar_clientes_faltantes(data, rfm_results)
  
  # Subir resultados a la base de datos
  EscribirDatos(rfm_results, tabla_destino)
  return(rfm_results)
}

rfm_s <- procesar_rfm(data, variable_revenue = "SacFact70", tabla_destino = "CRMNALRFM")
rfm_m <- procesar_rfm(data,  variable_revenue = "Margen", tabla_destino = "CRMNALRFMM")

rm(personas_rfm)

### 6.3. CLV ----

# Función para procesar CLV por fecha
procesar_clv <- function(fecha_corte) {
  unidad <- "weeks"
  
  # Preparar datos de transacciones
  apparelTrans <- data %>%
    filter(
      FecFact < fecha_corte,
      FecFact > fecha_corte - years(1),
      !is.na(FecFact),
      year(FecFact) >= 2020
    ) %>%
    group_by(Id = CLCliNit, Date = PrimerDia(FecFact, uni = unidad)) %>%
    summarise(Price = sum(SacFact70), .groups = "drop")
  
  # Si no hay datos, devolver dataframe vacío
  if (nrow(apparelTrans) == 0) {
    return(data.frame())
  }
  
  # Definir fecha de corte para estimación
  fec_split <- PrimerDia(max(apparelTrans$Date)) - months(1) - days(1)
  
  # Filtrar transacciones
  apparelTrans_filtered <- apparelTrans %>%
    group_by(Id) %>%
    mutate(first_transaction = min(Date)) %>%
    filter(first_transaction <= fec_split) %>%
    ungroup()
  
  # Si no hay datos después del filtro, devolver dataframe vacío
  if (nrow(apparelTrans_filtered) == 0) {
    return(data.frame())
  }
  
  # Procesar CLV con manejo de errores
  tryCatch({
    # Crear objeto clvdata
    clv.apparel <- CLVTools::clvdata(
      apparelTrans_filtered,
      date.format = "ymd",
      time.unit = unidad,
      name.id = "Id",
      name.date = "Date",
      name.price = "Price",
      estimation.split = fec_split
    )
    
    # Estimar modelo Pareto/NBD
    est.pnbd <- CLVTools::pnbd(clv.data = clv.apparel)
    
    # Predecir y formatear resultados
    CLVTools::predict(est.pnbd, prediction.end = PrimerDia(fecha_corte) + months(1) - days(1)) %>%
      mutate(
        SacosPred = pmax(0, round(predicted.total.spending)),
        FecProceso = fecha_corte + days(1),
        Churn = 1 - PAlive
      ) %>%
      select(FecProceso, CliNitPpal = Id, Churn, SacosPred)
    
  }, error = function(e) {
    message(paste("Error procesando la fecha", fecha_corte, ":", e$message))
    data.frame()
  })
}
ana_dates <- generar_fechas_analisis("2020-01-01", ajuste = -1)

# Usar procesamiento paralelo si hay muchas fechas
if (length(ana_dates) > 12) {
  # Configurar procesamiento paralelo
  future::plan(future::multisession, workers = parallel::detectCores() - 1)
  
  # Procesar en paralelo
  clv_results <- furrr::future_map_dfr(ana_dates, procesar_clv, .progress = TRUE)
} else {
  # Procesar secuencialmente
  clv_results <- purrr::map_dfr(ana_dates, procesar_clv)
}

# Subir resultados a la base de datos
EscribirDatos(clv_results, "CRMNALCLV")



## 7. Limpieza ----
gdata::keep(data, NCLIENTE, FACT, sure = T)
save.image("CRMNacional/data/data.RData")

## 8. Publicacion ----
library(connectapi)
client <- connect(
  server  = "http://172.16.19.39:3939",
  api_key = "HayDGkCmpQqmZB1rSkj2300JMDNpA2el")

  if (!file.exists("APP/manifest.json")) {
    rsconnect::writeManifest("/home/htamara/6_IndustriaNacional/CRM Cliente Nacional/CRMNacional/")
  }

bundle <- bundle_dir("/home/htamara/6_IndustriaNacional/CRM Cliente Nacional/CRMNacional/")
content <- client %>%
  deploy(bundle, guid = "f3724248-d5b5-4c28-880c-4d41ba8b5d95") %>%
  poll_task()
rm(bundle, client, content)

gc()
tictoc::toc()


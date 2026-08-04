tictoc::tic("CRM NACIONAL")
print(paste0("*********** ", Sys.time(), " ***********"))
setwd("/home/htamara/6_IndustriaNacional/CRM Cliente Nacional")
Sys.setenv(LANG = "es_CO.UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")
options(dplyr.summarise.inform = FALSE,
        OutDec = ".",
        scipen = 999,
        lubridate.week.start = 1,
        repos = c(CRAN = "https://cloud.r-project.org"))

# Librerias ----
required_packages <- c("racafe", "tidyverse", "lubridate", "httr", "readxl",
                       "openxlsx2", "rfm", "CLVTools")
racafe::Loadpkg(required_packages)


# Funciones: OneDrive ----

# Descarga un archivo de OneDrive por ID, escribe en disco y retorna ruta temp
.onedrive_descargar_raw <- function(file_id, usuario) {
  access_token <- racafe::ObtenerTokenAcceso()
  drive_id     <- racafe::ObtenerIdDrive(usuario)
  url          <- paste0("https://graph.microsoft.com/v1.0/drives/",
                         drive_id, "/items/", file_id, "/content")
  
  headers  <- httr::add_headers(Authorization = paste("Bearer", access_token))
  tmp_path <- tempfile(fileext = ".xlsx")
  resp     <- httr::GET(url, headers, httr::write_disk(tmp_path, overwrite = TRUE))
  if (httr::status_code(resp) != 200L) {
    stop("Error OneDrive [", httr::status_code(resp), "]: ",
         httr::content(resp, "text", encoding = "UTF-8"))
  }
  tmp_path
}
# Lista las hojas de un Excel en OneDrive dado su ID
leer_hojas_onedrive <- function(file_id, usuario) {
  ruta <- .onedrive_descargar_raw(file_id, usuario)
  on.exit(if (file.exists(ruta)) file.remove(ruta), add = TRUE)
  openxlsx2::wb_load(ruta)$sheet_names
}
# Carga una hoja especifica de un Excel en OneDrive dado su ID
CargarExcelPorId <- function(id_archivo, hoja, usuario) {
  stopifnot(is.character(id_archivo), length(id_archivo) == 1L,
            is.character(hoja),       length(hoja) == 1L,
            is.character(usuario),    length(usuario) == 1L)
  ruta <- .onedrive_descargar_raw(id_archivo, usuario)
  on.exit(if (file.exists(ruta)) file.remove(ruta), add = TRUE)
  openxlsx2::read_xlsx(ruta, sheet = hoja)
}


# Funciones: carga de lotes (EXPCUALO) ----------------------------------------

# Carga completa de lotes sin restriccion de estado ni fecha.
# Usada en primera ejecucion (sin cache) o cuando se fuerza recarga total.
.lotes_full_sql <- function() {
  ConsultaSistema(
    "syscafe",
    query = "SELECT CLSucCod, CLLotCod, CLPdcCod, CLPdcLin,
                    CLPdcAnoEm*100 + CLPdcMesEm AS Periodo,
                    CLPdcCntCl, CLLotCan AS SacLote, CLLotFec AS FecAsignLote,
                    CLotrCod AS CodOrdTril, CLOTrFec AS FecOrdTril,
                    CLLotSacPr AS SacProd, CLLotFecPr AS FecProd,
                    CLIDeCod AS CodDesp, CLIDeFec AS FecDesp, CLLotSacDe AS SacDesp,
                    CLPdcFctAD, CLLotSacFa AS SacFact, CLPdcCanFa, CLLinNegCo,
                    CLLotSacXP, CLLotPenXD, CLLotDesXF, CLCliNit, CLCliRazSo,
                    CLLinNegCo AS LinNegCod, CLLinNegNo,
                    CLLinProCo AS LinProCod, CLLinProNo,
                    CLMCCod AS MCCod, CLMCNom, CLMrcCod AS MrcCod, CLMrcNom
             FROM   EXPCUALO
             WHERE  CiaCod = 10
               AND  (CLLinNegCo = 21000 OR
                        (CLLinNegCo = 10000
                           AND CLLinProCo IN (90030, 90040, 40050, 40100, 90025, 40150, 40200, 40010, 90035, 40250)
                        )
                    )
               AND  CLCliNit <> 32
               AND  CLLinNegCo IN (10000, 21000)
               AND  CLLotCan > 0"
  )
}

# Lotes activos por criterio de estado: no facturados completos o recientes
.lotes_activos_sql <- function(anho_corte) {
  ConsultaSistema(
    "syscafe",
    query = sprintf(
      "SELECT CLSucCod, CLLotCod, CLPdcCod, CLPdcLin,
              CLPdcAnoEm*100 + CLPdcMesEm AS Periodo,
              CLPdcCntCl, CLLotCan AS SacLote, CLLotFec AS FecAsignLote,
              CLotrCod AS CodOrdTril, CLOTrFec AS FecOrdTril,
              CLLotSacPr AS SacProd, CLLotFecPr AS FecProd,
              CLIDeCod AS CodDesp, CLIDeFec AS FecDesp, CLLotSacDe AS SacDesp,
              CLPdcFctAD, CLLotSacFa AS SacFact, CLPdcCanFa, CLLinNegCo,
              CLLotSacXP, CLLotPenXD, CLLotDesXF, CLCliNit, CLCliRazSo,
              CLLinNegCo AS LinNegCod, CLLinNegNo,
              CLLinProCo AS LinProCod, CLLinProNo,
              CLMCCod AS MCCod, CLMCNom, CLMrcCod AS MrcCod, CLMrcNom
       FROM   EXPCUALO
       WHERE  CiaCod = 10
         AND  (CLLinNegCo = 21000 OR
                        (CLLinNegCo = 10000
                           AND CLLinProCo IN (90030, 90040, 40050, 40100, 90025, 40150, 40200, 40010, 90035, 40250)
                        )
              )
         AND  CLCliNit <> 32
         AND  CLLinNegCo IN (10000, 21000)
         AND  CLLotCan > 0
         AND  YEAR(CONVERT(date, CLLotFec, 23)) >= %d",
      anho_corte
    )
  )
}

# Combina lotes activos (SQL incremental) con lotes congelados (cache anterior).
# Sin cache ejecuta carga completa como base inicial.
cargar_lotes_incremental <- function(datos_previos = NULL) {
  if (is.null(datos_previos) || nrow(datos_previos) == 0L) {
    message("Sin cache de lotes: ejecutando carga completa...")
    return(.lotes_full_sql())
  }
  anho_corte    <- lubridate::year(Sys.Date()) - 1L
  lotes_activos <- .lotes_activos_sql(anho_corte)
  
  # Congelados: todo lo del cache que la consulta activa no cubre.
  # El anti_join es suficiente: .lotes_activos_sql ya trae todos los lotes
  # con FecAsignLote >= anho_corte, por lo que el complemento es siempre anterior.
  lotes_congelados <- datos_previos %>%
    anti_join(lotes_activos, by = "CLLotCod")
  
  bind_rows(lotes_congelados, lotes_activos)
}

# Funciones: facturacion (FCTFACN1) -------------------------------------------

# Query completa obligatoria: un lote puede tener facturas en multiples anos.
# La particion por corte de ano duplicaria registros al unir al cuadro de lotes
# porque GROUP BY opera sobre el universo completo y colapsa a un registro por
# lote+tipo independientemente del ano de facturacion.
cargar_fact <- function() {
  fact_sys_nal <- ConsultaSistema("syscafe",
                                  query = "SELECT F1.FcnLot      AS CLLotCod,
                                              MIN(F2.FcnFec) AS FecPrimerFact,
                                              MAX(F2.FcnFec) AS FecFact,
                                              SUM(F1.FcnKilLot)         AS KilosFact,
                                              SUM(F1.FcnSacLot)         AS SacosFact,
                                              SUM(F1.FcnKilLot / 70.)    AS SacFact70
                                       FROM   FCTFACN1 F1
                                       LEFT   JOIN FCTFACNA F2 ON F1.FcnNum = F2.FcnNum
                                       WHERE  F1.CiaCod = 10 AND
                                              F2.CiaCod = 10 AND
                                              F2.FcnEtd = 'C'
                                      GROUP  BY F1.FcnLot")
  fact_sys_ext <- ConsultaSistema("syscafe",
                                  query = "SELECT F1.FctLot AS CLLotCod,
                                                   MIN(F2.FctFec)         AS FecPrimerFact,
                                                   MAX(F2.FctFec)         AS FecFact,
                                                   SUM(F1.FctKilLot)      AS KilosFact,
                                                   SUM(F1.FctSacEmb)      AS SacosFact,
                                                   SUM(F1.FctKilLot / 70.) AS SacFact70
                                            FROM FCTFACC2 F1
                                            LEFT JOIN FCTFACCA F2 ON F1.FctNum = F2.FctNum
                                            WHERE F1.CiaCod = 10 AND
                                                  F2.CiaCod = 10 AND
                                                  F1.FctLotAnu <> 'S'
                                            GROUP BY F1.FctLot")
  
  fact_margenes <- Consulta("SELECT LOTE AS CLLotCod,
                                    MAX(FECFACTURA) AS FechaFactura,
                                    SUM(MARGEN)     AS Margen,
                                    SUM(SACOS70)    AS SacosPYG
                            FROM CRMNALMARLOT
                            GROUP BY LOTE") %>%
    mutate(FechaFactura = as.Date(FechaFactura))
  
  bind_rows(fact_sys_nal, fact_sys_ext) %>%
    full_join(fact_margenes, by = "CLLotCod") %>%
    mutate(FecFact = if_else(is.na(FecFact), FechaFactura, FecFact),
           FecPrimerFact = if_else(is.na(FecPrimerFact), FechaFactura, FecPrimerFact)) %>%
    filter(FecPrimerFact >= as.Date("2020-01-01")) %>%
    select(-FechaFactura) %>%
    mutate(Facturado = TRUE, FecFact = as.Date(FecFact))
}

# Funciones: margenes desde OneDrive ------------------------------------------

# Extrae y normaliza columnas de un archivo PYG de margenes
.parsear_hoja_pyg <- function(id_archivo, hoja, usuario) {
  CargarExcelPorId(id_archivo = id_archivo, hoja = hoja, usuario = usuario) %>%
    select(
      SUCURSAL, TIP, LINNEG = `LIN.NEG`, MARCA, FACTURA,
      FECFACTURA = `FEC FACTURA`, LOTE, CLIENTE,
      PRODUCTO   = `PRODUCTO SEGUN MARCA`,
      SACOS70    = `SS 70 KLS`,
      KILOS, MARGEN = `MNFCC$`, MES
    ) %>%
    mutate(FECFACTURA = as.Date(FECFACTURA, origin = "1900-01-01"))
}
# Selecciona la hoja BASE mas reciente de un vector de nombres de hojas
.seleccionar_hoja_base <- function(hojas) {
  bases <- hojas[grepl("^BASE( \\d{4})?$", hojas, ignore.case = TRUE)]
  if (length(bases) == 0L) return(NA_character_)
  if (any(grepl("\\d{4}", bases))) {
    anhos <- as.numeric(gsub(".*?(\\d{4}).*", "\\1", bases, perl = TRUE))
    anhos[is.na(anhos)] <- 0L
    return(bases[which.max(anhos)])
  }
  bases[[1L]]
}
# Navega la estructura de carpetas OneDrive y carga los archivos PYG mas recientes
cargar_margenes_onedrive <- function(usuario, id_carpeta_raiz) {
  # Carpeta del ano mas reciente
  anho <- ListarContenidoCarpetaId(usuario, id_carpeta_raiz) %>%
    filter(str_detect(name, "^A\u00d1O\\s\\d{4}$")) %>%
    mutate(year_num = as.numeric(gsub("^A\u00d1O (\\d{4})$", "\\1", name))) %>%
    slice_max(year_num, n = 1L) %>%
    select(name, id)
  if (nrow(anho) == 0L) { message("No se encontro carpeta de ano"); return(NULL) }
  
  # Carpeta del mes mas reciente
  mes <- ListarContenidoCarpetaId(usuario, anho$id) %>%
    filter(grepl("^\\d{2}\\.", name)) %>%
    mutate(month_num = as.numeric(sub("^(\\d{2})\\. .*$", "\\1", name))) %>%
    slice_max(month_num, n = 1L) %>%
    select(name, id)
  if (nrow(mes) == 0L) { message("No se encontro carpeta de mes"); return(NULL) }
  
  # Archivos PYG disponibles en el mes mas reciente
  pyg_files <- ListarContenidoCarpetaId(usuario, mes$id) %>%
    filter(grepl(
      "^\\d{2}\\. PYG POR LOTE( A\u00d1O)? \\d{4}.*(COPRODUCTOS|A LA MEDIDA).*\\.xlsx$",
      name, ignore.case = TRUE
    )) %>%
    select(name, id)
  if (nrow(pyg_files) == 0L) {
    message("No se encontraron archivos PYG en: ", mes$name)
    return(NULL)
  }
  
  if (nrow(pyg_files) != 2L) {
    stop(sprintf(
      "Se esperaban 2 archivos PYG en '%s' y se encontraron %d",
      mes$name, nrow(pyg_files)
    ))
  }
  
  # Procesar cada archivo y combinar resultados
  res <- purrr::map(seq_len(nrow(pyg_files)), function(i) {
    arch     <- pyg_files[i, ]
    hojas    <- tryCatch(
      leer_hojas_onedrive(arch$id, usuario),
      error = function(e) { message("Error hojas ", arch$name, ": ", e$message); character(0L) }
    )
    hoja_sel <- .seleccionar_hoja_base(hojas)
    if (is.na(hoja_sel)) { message("Sin hoja BASE: ", arch$name); return(NULL) }
    tryCatch(
      .parsear_hoja_pyg(arch$id, hoja_sel, usuario) %>% filter(LOTE != 0L),
      error = function(e) {
        message("Error datos ", arch$name, " / ", hoja_sel, ": ", e$message); NULL
      }
    )
  }) %>%
    purrr::compact() %>%
    bind_rows()
  
  return(res)
}

# Funciones: imputacion en cascada --------------------------------------------

# Imputa una columna numerica en cascada, de mas especifico a mas general.
# 'niveles' es una lista de vectores de nombres de columnas de agrupacion,
# ordenados de mayor a menor especificidad; el ultimo nivel puede ser
# character(0) para un promedio global sin agrupar (ultimo recurso).
# En cada nivel se calcula el promedio SOLO con filas no-NA (via left_join,
# no via group_by/mutate repetido sobre el objeto completo), lo cual evita
# el costo de reagrupar el data frame entero en cada paso de la cascada.
imputar_cascada <- function(df, valor_col, niveles) {
  for (grupo_vars in niveles) {
    
    # Nivel global (sin agrupar): promedio general como ultimo recurso
    if (length(grupo_vars) == 0L) {
      media_global <- mean(df[[valor_col]], na.rm = TRUE)
      df[[valor_col]] <- ifelse(is.na(df[[valor_col]]), media_global, df[[valor_col]])
      next
    }
    
    # Promedio del valor en el nivel de agregacion actual (solo filas validas)
    medias_nivel <- df %>%
      filter(!is.na(.data[[valor_col]])) %>%
      group_by(across(all_of(grupo_vars))) %>%
      summarise(.media_tmp = mean(.data[[valor_col]], na.rm = TRUE), .groups = "drop")
    
    # Union por join: solo se completan los NA restantes en este nivel
    df <- df %>%
      left_join(medias_nivel, by = grupo_vars) %>%
      mutate(across(all_of(valor_col), ~ ifelse(is.na(.), .media_tmp, .))) %>%
      select(-.media_tmp)
  }
  df
}

# Cascada de imputacion de margen por kilo (MarKilo), de mas a menos especifica.
# Los niveles que omiten CLLinNegNo (marcados abajo) permiten que clientes
# nuevos o clientes sin historial en una linea de negocio especifica hereden
# el margen del MISMO producto transado en otra linea, en vez de saltar
# directo a un promedio generico de linea de negocio.
niveles_margen <- list(
  c("CLCliNit", "CLLinNegNo", "LinProCod", "MCCod", "MrcCod"),
  c("CLLinNegNo", "LinProCod", "MCCod", "MrcCod"),
  c("LinProCod", "MCCod", "MrcCod"),          # sin CLLinNegNo: producto exacto, cualquier linea
  c("CLLinNegNo", "LinProCod"),
  c("LinProCod"),                              # sin CLLinNegNo: categoria de producto sola
  c("CLLinNegNo"),
  character(0)                                 # promedio global: ultimo recurso
)

# Funciones: CLV (Pareto/NBD) por CliNitPpal + LinNegCod ----------------------

# Estima Churn (1 - PAlive) y SacosPred para cada combinacion CliNitPpal+LinNegCod
# en una fecha de corte dada. Llave alineada con RFM y con el consumo en la app
# (Individual.R filtra por CliNitPpal + LinNegCod).
procesar_clv <- function(fecha_corte, data_fuente = data, unidad = "weeks") {
  
  base_trans <- data_fuente %>%
    filter(FecFact < fecha_corte,
           !is.na(FecFact),
           year(FecFact) >= 2020,
           !is.na(CliNitPpal), !is.na(LinNegCod)) %>%
    mutate(Id = paste(CliNitPpal, LinNegCod, sep = "_")) %>%
    group_by(Id, CliNitPpal, LinNegCod, Date = PrimerDia(FecFact, uni = unidad)) %>%
    summarise(Price = sum(SacDesp, na.rm = TRUE), .groups = "drop")
  
  if (nrow(base_trans) == 0L) return(data.frame())
  
  ids_map <- base_trans %>%
    select(Id, CliNitPpal, LinNegCod) %>%
    distinct()
  
  apparelTrans <- base_trans %>%
    select(Id, Date, Price)
  
  fec_split <- PrimerDia(max(apparelTrans$Date)) - months(1) - days(1)
  
  apparelTrans_filtered <- apparelTrans %>%
    group_by(Id) %>%
    mutate(first_transaction = min(Date)) %>%
    filter(first_transaction <= fec_split) %>%
    ungroup() %>%
    select(-first_transaction)
  
  if (nrow(apparelTrans_filtered) == 0L) return(data.frame())
  
  resultado <- tryCatch({
    clv_data <- CLVTools::clvdata(apparelTrans_filtered,
                                  date.format        = "ymd",
                                  time.unit          = unidad,
                                  name.id            = "Id",
                                  name.date          = "Date",
                                  name.price         = "Price",
                                  estimation.split   = fec_split)
    
    est_pnbd <- CLVTools::pnbd(clv.data = clv_data)
    
    # FIX: predicted.total.spending NO existe; se usa predicted.period.spending
    pred <- CLVTools::predict(est_pnbd,
                              prediction.end = PrimerDia(fecha_corte) + months(1) - days(1))
    
    col_spending <- if ("predicted.period.spending" %in% names(pred)) {
      pred$predicted.period.spending
    } else {
      pred$CET * pred$predicted.mean.spending
    }
    
    pred %>%
      mutate(SacosPred  = pmax(0, round(col_spending)),
             FecProceso = fecha_corte + days(1),
             Churn      = 1 - PAlive) %>%
      left_join(ids_map, by = "Id") %>%
      select(FecProceso, CliNitPpal, LinNegCod, Churn, SacosPred) %>%
      group_by(FecProceso, CliNitPpal, LinNegCod) %>%
      summarise(Churn = mean(Churn, na.rm = TRUE),
                SacosPred = sum(SacosPred, na.rm = TRUE),
                .groups = "drop")
    
  }, error = function(e) {
    message("Error CLV fecha ", fecha_corte, ": ", e$message)
    data.frame()
  })
  
  resultado
}

# Funciones: utilidades de segmentacion (RFM / Racafe) ------------------------

identificar_clientes_faltantes <- function(df_referencia, df_comparacion) {
  df_referencia %>%
    group_by(CliNitPpal, LinNegCod, Usuario) %>%
    summarise(FecFact = max(FecFact), .groups = "drop") %>%
    distinct() %>%
    anti_join(
      df_comparacion %>% select(CliNitPpal, LinNegCod) %>% distinct(),
      by = join_by(CliNitPpal, LinNegCod)
    )
}
generar_fechas_analisis <- function(fecha_inicio, fecha_fin = Sys.Date(), por = "month", ajuste = 0) {
  fechas <- seq.Date(as.Date(fecha_inicio), PrimerDia(fecha_fin), by = por)
  if (ajuste != 0L) fechas <- fechas + lubridate::days(ajuste)
  fechas
}


# Procesamiento: 1. Margenes ---------------------------------------------------
print("Cargando datos de margenes...")

usr_margenes        <- "wmunozs"
id_carpeta_informes <- "014N3L2U2EH7DJE5B6V5AK6USIEW5IJH2N"

# Datos historicos cacheados (anos anteriores al corriente)
margenes_hist <- tryCatch(
  Consulta("SELECT * FROM CRMNALMARLOT WHERE LOTE != 0") %>%
    mutate(FECFACTURA = as.Date(FECFACTURA)) %>%
    filter(year(FECFACTURA) < year(Sys.Date())),
  error = function(e) { message("Error margenes historicos: ", e$message); data.frame() }
)

# Datos del ano corriente desde OneDrive
margenes_nuevos <- tryCatch(
  cargar_margenes_onedrive(usr_margenes, id_carpeta_informes),
  error = function(e) { message("Error margenes OneDrive: ", e$message); NULL }
)

# Combinar y persistir si hay datos nuevos
if (!is.null(margenes_nuevos) && nrow(margenes_nuevos) > 0L) {
  tryCatch(
    { EscribirDatos(bind_rows(margenes_hist, margenes_nuevos), "CRMNALMARLOT")
      message("Margenes guardados OK") },
    error = function(e) message("Error al guardar margenes: ", e$message)
  )
} else {
  message("Sin datos nuevos de margenes; se conserva lo existente")
}


# Procesamiento: 2. Datos principales ------------------------------------------
print("Cargando datos principales...")

# Tabla maestra de sucursales — con tildes para que LimpiarNombres produzca
sucs <- data.frame(SucCod = c(12, 15, 20, 26, 30, 32, 35, 50, 55),
                   Sucursal = c("Trilladora 12","Bachué", "Medellín", "Popayán", "Armenia",
                                "Arenales", "Pereira", "Bucaramanga", "Huila")) %>%
  mutate(across(where(is.character), LimpiarNombres))

# Snapshot completo de CRMNALCLIENTE, cargado UNA sola vez y reutilizado
# para NITPPAL (NIT principal por cliente, seccion 2) y para los segmentos
# Racafe/RFM (seccion 3). FecProceso se lleva a datetime para que, si hay
# mas de un proceso el mismo dia, siempre se tome el mas reciente.
personas <- Consulta("SELECT * FROM CRMNALCLIENTE") %>%
  mutate(FecProceso = as_datetime(FecProceso))

# NIT principal por cliente (snapshot mas reciente), derivado de 'personas'
# en vez de una segunda consulta a CRMNALCLIENTE.
NITPPAL <- personas %>%
  select(FecProceso, PerCod = CLCliNit, CliNitPpal) %>%
  distinct() %>%
  group_by(PerCod) %>%
  filter(FecProceso == max(FecProceso)) %>%
  ungroup() %>%
  select(-FecProceso)

# Datos maestros de clientes desde sistema transaccional
CREACION <- ConsultaSistema("syscafe", "SELECT DISTINCT PerCod, PerFecCre from NPERSONA")

NCLIENTE <- ConsultaSistema("syscafe",
                            query = "SELECT c.CliNit AS PerCod, c.CliCont, c.CliDir,
                                            c.CliDir1, c.CliTel, c.CliConCom, c.CliTelCom,
                                            c.CliEmlCom, c.CiuExtCod, c.CliFPagDbl,
                                            ce.CiuExtNom, p.PerRazSoc, p.PerFecCre as FecCreacion
                                     FROM   NCLIENTE c
                                     LEFT   JOIN NCIUEXT  ce ON c.CiuExtCod = ce.CiuExtCod
                                     LEFT   JOIN NPERSONA p  ON c.CliNit     = p.PerCod") %>%
  left_join(NITPPAL, by = "PerCod") %>%
  mutate(CliNitPpal = ifelse(is.na(CliNitPpal), PerCod, CliNitPpal),
         across(where(is.character), \(x) ifelse(x %in% c("", "."), NA, x)))

# Pedidos activos de venta nacional
ped <- ConsultaSistema("syscafe",
                       query = "SELECT p1.PdcCod, pd.PdcCntCli AS PdcRefCli, pd.PdcFecCre,
                                        p1.PdcLin, p1.PdcCan, pd.PdcUsu AS Usuario,
                                        um.UMeFac, pd.PdcTipCaf AS TipCaf,
                                        pd.PdcPrePes AS PdcPrecioKilo
                                 FROM   EXPPEDI1 p1
                                 INNER  JOIN EXPPEDID pd ON p1.PdcCod = pd.PdcCod
                                 LEFT   JOIN NUNIMEDI um ON pd.UMeCod  = um.UMeCod
                                 WHERE  p1.CiaCod = 10 AND pd.CiaCod = 10
                                   AND  pd.CliNit <> 32 AND pd.PdcEst = 'A'")

# Cuadro de lotes con carga incremental (activos + congelados del cache)
datos_previos <- tryCatch(
  {
    prev <- readRDS("CRMNacional/data/lotes_raw.rds")
    if (is.data.frame(prev) && nrow(prev) > 0L) prev else NULL
  },
  error = function(e) NULL
)

lotes_raw <- cargar_lotes_incremental(datos_previos)

# Facturacion: query completa garantiza un registro por lote (no particionar)
fact <- cargar_fact()

# NOTA: se mantiene la consulta independiente de FACT (por cliente) tal
# como estaba. Se detecto que esta consulta NO filtra facturas anuladas de
# exportacion (FctLotAnu <> 'S'), a diferencia de cargar_fact() que si lo
# hace. Consolidarla desde 'fact' + el mapeo lote->cliente eliminaria un
# roundtrip completo a FCTFACN1/FCTFACC2, pero cambiaria el resultado por
# esa inconsistencia de filtro. Se deja igual para no alterar comportamiento
# sin confirmacion explicita.
FACT <- bind_rows(ConsultaSistema("syscafe",
                                  query = "SELECT F2.FctNit,
                                                   MIN(F2.FcnFec)          AS MinFecFact,
                                                   MAX(F2.FcnFec)          AS UltFecFact,
                                                   SUM(F1.FcnKilLot)       AS KilosFact,
                                                   SUM(F1.FcnSacLot)       AS SacosFact,
                                                   SUM(F1.FcnKilLot / 70.) AS SacFact70
                                           FROM FCTFACN1 F1
                                           INNER JOIN FCTFACNA F2 ON F1.FcnNum = F2.FcnNum AND
                                                                     F1.CiaCod = F2.CiaCod
                                           WHERE F1.CiaCod = 10 AND
                                                 F2.FcnEtd = 'C'
                                           GROUP BY F2.FctNit"),
                  ConsultaSistema("syscafe",
                                  query = "SELECT F2.FctNit,
                                            MIN(F2.FctFec)          AS MinFecFact,
                                            MAX(F2.FctFec)          AS UltFecFact,
                                            SUM(F1.FctKilLot)       AS KilosFact,
                                            SUM(F1.FctSacEmb)       AS SacosFact,
                                            SUM(F1.FctKilLot / 70.) AS SacFact70
                                     FROM FCTFACC2 F1
                                     INNER JOIN FCTFACCA F2 ON F1.FctNum = F2.FctNum AND
                                                               F1.CiaCod = F2.CiaCod
                                     WHERE F1.CiaCod = 10
                                     GROUP BY F2.FctNit")
) %>%
  group_by(FctNit) %>%
  summarise(MinFecFact = min(MinFecFact, na.rm = TRUE),
            UltFecFact = max(UltFecFact, na.rm = TRUE),
            KilosFact  = sum(KilosFact,  na.rm = TRUE),
            SacosFact  = sum(SacosFact,  na.rm = TRUE),
            SacFact70  = sum(SacFact70,  na.rm = TRUE),
            .groups = "drop") %>%
  left_join(NCLIENTE %>%
              select(PerCod, PerRazSoc) %>%
              distinct(),
            by = c("FctNit" = "PerCod")) %>%
  left_join(CREACION,  by = c("FctNit" = "PerCod") )

# Construccion del cuadro de datos principal

# Adicion de lotes que estan en PYG y no en lotes_raw
lotes_pyg_faltantes <- CargarDatos("CRMNALMARLOT") %>%
  filter(FECFACTURA >= as.Date("2026-01-01")) %>%
  rename(CLLotCod = LOTE) %>%
  anti_join(lotes_raw, by = "CLLotCod") %>%
  mutate(CLLinNegNo = ifelse(LINNEG== 10000, "CONVENCIONALES", "A LA MEDIDA")) %>%
  select(CLSucCod = SUCURSAL,
         CLLotCod,
         LinProCod = LINNEG,
         CLLinNegNo,
         CLCliNit = CLIENTE,
         LinNegCod = LINNEG)

data <- lotes_raw %>%
  bind_rows(lotes_pyg_faltantes) %>%
  left_join(sucs, by = c("CLSucCod" = "SucCod")) %>%
  left_join(NITPPAL, by = c("CLCliNit" = "PerCod")) %>%
  mutate(CliNitPpal = ifelse(is.na(CliNitPpal), CLCliNit, CliNitPpal)) %>%
  left_join(NCLIENTE %>% filter(PerCod == CliNitPpal) %>% select(-PerCod),
            by = "CliNitPpal") %>%
  left_join(NCLIENTE %>% select(PerCod, RazonSocialCliNit = PerRazSoc),
            by = c("CLCliNit" = "PerCod")) %>%
  left_join(ped, by = c("CLPdcCod" = "PdcCod", "CLPdcLin" = "PdcLin")) %>%
  left_join(fact, by = "CLLotCod") %>%
  mutate(MarKilo = Margen / KilosFact) %>%
  # Imputacion en cascada de margen por kilo (7 niveles, ver niveles_margen)
  imputar_cascada(valor_col = "MarKilo", niveles = niveles_margen) %>%
  mutate(SacosPYG = ifelse(is.na(SacosPYG), SacFact70, SacosPYG),
         Margen = ifelse(is.na(Margen),
                         MarKilo * (SacosPYG * 70),
                         Margen),
         FechaEmbarque = as.Date(paste(Periodo, "01"), "%Y%m%d"),
         across(contains("Fec"), ~ if_else(as.Date(.) == as.Date("1753-01-01"), as.Date(NA), as.Date(.))),
         CliNitPpal = ifelse(is.na(CliNitPpal), CLCliNit, CliNitPpal),
         CLLinNegNo = ifelse(CLLinNegNo == "DIFERENCIADOS", "A LA MEDIDA", CLLinNegNo),
         LinNegCod  = ifelse(LinNegCod == 20000L, 21000L, LinNegCod),
         Kilos      = SacDesp * UMeFac) %>%
  group_by(LinNegCod, CLLinProNo) %>%
  mutate(PendProducir  = SacLote - SacProd,
         PendDespachar = SacLote - pmax(SacDesp, 0),
         PendFacturar  = SacLote - pmax(PendDespachar, 0) - coalesce(SacosFact, 0)) %>%
  ungroup() %>%
  select(CLSucCod, Sucursal, CLLotCod, FechaEmbarque, CLPdcCod, PdcPrecioKilo,
         PdcRefCli, PdcFecCre, CLPdcLin, CLPdcCntCl, Usuario, SacLote, FecAsignLote,
         CodOrdTril, FecOrdTril, SacProd, FecProd, CodDesp, FecDesp, SacDesp,
         FecFact = FecPrimerFact, KilosFact, SacosFact, SacFact70, Margen, SacosPYG, MarKilo, UMeFac, Kilos,
         LinNegCod, CLLinNegNo, LinProCod, MCCod, MrcCod, CLPdcFctAD, SacFact, CLPdcCanFa,
         CLCliNit, RazonSocialCliNit, CliNitPpal, PerRazSoc, CliCont, CliDir, CliDir1, CliTel, CliConCom, CliTelCom,
         CliEmlCom, CiuExtNom, CLLotSacXP, CLLotPenXD, CLLotDesXF, PendProducir, PendDespachar, PendFacturar) %>%
  mutate(across(where(is.character), ~ replace_na(., "SIN DATO")))


# Procesamiento: 3. Segmentaciones (solo primer dia del mes) -------------------
es_primer_dia <- lubridate::day(Sys.Date()) == 1L

if (es_primer_dia) {
  
  # Segmento Racafe: marcacion cliente / cliente a recuperar ---
  print("Calculando segmento Racafe...")
  ana_dates <- generar_fechas_analisis("2020-02-01")
  
  personas_segmento <- personas %>%
    select(FecProceso, LinNegCod, CliNitPpal, NumMesesRecuperar) %>%
    complete(LinNegCod, CliNitPpal, FecProceso = sort(unique(data$FecFact))) %>%
    group_by(LinNegCod, CliNitPpal) %>%
    fill(NumMesesRecuperar, .direction = "updown") %>%
    ungroup()
  
  seg <- purrr::map_dfr(ana_dates, function(x) {
    data %>%
      filter(FecFact < x) %>%
      left_join(personas_segmento,
                by = c("CliNitPpal", "LinNegCod", "FecFact" = "FecProceso")) %>%
      group_by(LinNegCod, CliNitPpal, NumMesesRecuperar) %>%
      summarise(FecUltDesp = max(FecFact, na.rm = TRUE), .groups = "drop") %>%
      mutate(Meses          = ifelse(is.na(NumMesesRecuperar), 4L, NumMesesRecuperar),
             SegmentoRacafe = ifelse(FecUltDesp >= PrimerDia(x) - months(Meses), "CLIENTE", "CLIENTE A RECUPERAR"),
             FecProceso = x) %>%
      select(-FecUltDesp, -NumMesesRecuperar)
  })
  
  rm(personas_segmento)
  identificar_clientes_faltantes(data, seg)
  EscribirDatos(seg, "CRMNALSEGR")
  
  # RFM por sacos y por margen ---
  personas_rfm <- personas %>%
    select(FecProceso, LinNegCod, CliNitPpal, Segmento) %>%
    complete(LinNegCod, CliNitPpal,
             FecProceso = sort(data$FecFact %>% unique())) %>%
    group_by(LinNegCod, CliNitPpal) %>%
    fill(Segmento, .direction = "updown") %>%
    ungroup()
  
  # Segmentos Racafe cargados UNA sola vez y compartidos por ambas corridas
  # de procesar_rfm (sacos y margen), en vez de reconsultarse dentro de cada una.
  segmentos_racafe <- Consulta("select * from CRMNALSEGR") %>%
    mutate(FecProceso = as.Date(FecProceso)) %>%
    select(LinNegCod, CliNitPpal, FecProceso, SegmentoRacafe)
  
  # Definir segmentos para analisis (compartidos por ambas corridas de RFM)
  segs  <- c("DETAL", "MEDIANO", "GRANDES")
  segs2 <- c("CLIENTE A RECUPERAR", "CLIENTE")
  segs3 <- c(10000, 21000)
  
  # Definir parametros de segmentacion RFM
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
    
    # Fechas de analisis
    ana_dates <- generar_fechas_analisis("2020-02-01", ajuste = -1)
    
    # Procesar RFM para cada fecha
    rfm_results <- purrr::map_dfr(ana_dates, function(x) {
      # Filtrar datos para la fecha actual, restringido a las combinaciones
      # validas de segmento/segmento-racafe/linea (mismo universo que antes)
      aux0 <- data_rfm %>%
        mutate(FecProceso = x + 1) %>%
        left_join(segmentos_racafe %>% filter(FecProceso == x + 1),
                  by = c("LinNegCod", "customer_id" = "CliNitPpal", "FecProceso")) %>%
        filter(order_date <= x,
               order_date > x - years(1),
               Segmento %in% segs,
               SegmentoRacafe %in% segs2,
               LinNegCod %in% segs3)
      
      if (nrow(aux0) == 0L) return(data.frame())
      
      # Un solo split por grupo (Segmento, SegmentoRacafe, LinNegCod) en vez
      # de 3 bucles anidados de purrr::map_dfr re-filtrando el mismo objeto
      aux0 %>%
        group_by(Segmento, SegmentoRacafe, LinNegCod) %>%
        group_modify(~ {
          aux1 <- rfm_table_order(.x, customer_id, order_date, revenue, x)
          rfm_segment(
            aux1, segment_names,
            recency_lower, recency_upper,
            frequency_lower, frequency_upper,
            monetary_lower, monetary_upper
          ) %>%
            mutate(
              FecProceso = x + days(1),
              segment = ifelse(segment == "Others", "OTROS", segment)
            ) %>%
            rename(
              SegmentoAnalitica = segment,
              CliNitPpal = customer_id
            )
        }) %>%
        ungroup()
    })
    
    # Verificar clientes faltantes
    identificar_clientes_faltantes(data, rfm_results)
    
    # Subir resultados a la base de datos
    EscribirDatos(rfm_results, tabla_destino)
    return(rfm_results)
  }
  
  rfm_s <- procesar_rfm(data, variable_revenue = "SacFact70", tabla_destino = "CRMNALRFM")
  rfm_m <- procesar_rfm(data, variable_revenue = "Margen",    tabla_destino = "CRMNALRFMM")
  
  # CLV ---
  ana_dates_clv <- generar_fechas_analisis("2020-01-01", ajuste = -1L)
  
  if (length(ana_dates_clv) > 12L) {
    future::plan(future::multisession, workers = parallel::detectCores() - 1L)
    clv_results <- furrr::future_map_dfr(ana_dates_clv, procesar_clv, .progress = TRUE)
    future::plan(future::sequential)
  } else {
    clv_results <- purrr::map_dfr(ana_dates_clv, procesar_clv)
  }
  EscribirDatos(clv_results, "CRMNALCLV")
  
  message("Segmentaciones completadas: ", Sys.time())
  
} else {
  message("Segmentaciones omitidas (no es primer dia del mes): ", Sys.Date())
}


# Limpieza y persistencia de cache ---------------------------------------------

# Persistir lotes_raw para cache incremental de la proxima ejecucion
saveRDS(lotes_raw, "CRMNacional/data/lotes_raw.rds")

# Conservar solo los objetos necesarios para la app Shiny
gdata::keep(data, NCLIENTE, FACT, sure = TRUE)
save.image("CRMNacional/data/data.RData")

# Publicacion en Posit Connect -------------------------------------------------
library(connectapi)

client <- connect(server  = "http://172.16.19.39:3939",
                  api_key = Sys.getenv("CONNECT_HCYT_KEY"))

if (!file.exists("APP/manifest.json")) {
  rsconnect::writeManifest("/home/htamara/6_IndustriaNacional/CRM Cliente Nacional/CRMNacional/")
}

bundle  <- bundle_dir("/home/htamara/6_IndustriaNacional/CRM Cliente Nacional/CRMNacional/")
content <- client %>%
  deploy(bundle, guid = "f3724248-d5b5-4c28-880c-4d41ba8b5d95") %>%
  poll_task()

rm(bundle, client, content)
gc()
tictoc::toc()
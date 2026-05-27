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
                                    SUM(MARGEN)     AS Margen,
                                    SUM(SACOS70) AS SacosPYG
                            FROM   CRMNALMARLOT
                            GROUP  BY LOTE")
  
  bind_rows(fact_sys_nal, fact_sys_ext) %>% 
    left_join(fact_margenes, by = "CLLotCod") %>%
    filter(FecFact >= as.Date("2020-01-01")) %>%
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

# NIT principal por cliente (snapshot mas reciente)
NITPPAL <- Consulta("SELECT DISTINCT FecProceso, CLCliNit AS PerCod, CliNitPpal FROM CRMNALCLIENTE") %>%
  mutate(FecProceso = as.Date(FecProceso)) %>%
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

# Facturas historicas consolidadas por cliente (para indicadores globales)
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
data <- lotes_raw %>%
  left_join(sucs, by = c("CLSucCod" = "SucCod")) %>%
  left_join(NITPPAL, by = c("CLCliNit" = "PerCod")) %>%
  mutate(CliNitPpal = ifelse(is.na(CliNitPpal), CLCliNit, CliNitPpal)) %>%
  # Solo NITs principales para evitar duplicados por subsidiarias
  left_join(NCLIENTE %>% filter(PerCod == CliNitPpal) %>% select(-PerCod),
            by = "CliNitPpal") %>%
  left_join(ped, by = c("CLPdcCod" = "PdcCod", "CLPdcLin" = "PdcLin")) %>%
  left_join(fact, by = "CLLotCod") %>%
  mutate(MarKilo = Margen / KilosFact) %>%
  # Imputacion jerarquica de margen por kilo (4 niveles de agregacion)
  group_by(CLCliNit, CLLinNegNo, LinProCod, MCCod, MrcCod) %>%
  mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = TRUE), MarKilo)) %>%
  group_by(CLLinNegNo, LinProCod, MCCod, MrcCod) %>%
  mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = TRUE), MarKilo)) %>%
  group_by(CLLinNegNo, LinProCod) %>%
  mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = TRUE), MarKilo)) %>%
  group_by(CLLinNegNo) %>%
  mutate(MarKilo = ifelse(is.na(MarKilo), mean(MarKilo, na.rm = TRUE), MarKilo)) %>%
  ungroup() %>%
  mutate(Margen = ifelse(is.na(Margen),
                         MarKilo * SacLote * ifelse(LinNegCod == 10000L, 62.5, 70),
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
         FecFact, KilosFact, SacosFact, SacFact70, Margen, SacosPYG, MarKilo, UMeFac, Kilos,
         LinNegCod, CLLinNegNo, LinProCod, MCCod, MrcCod, CLPdcFctAD, SacFact, CLPdcCanFa,
         CliNitPpal, CLCliNit, PerRazSoc, CliCont, CliDir, CliDir1, CliTel, CliConCom, CliTelCom, 
         CliEmlCom, CiuExtNom, CLLotSacXP, CLLotPenXD, CLLotDesXF, PendProducir, PendDespachar, PendFacturar) %>%
  mutate(across(where(is.character), ~ replace_na(., "SIN DATO")))


# Procesamiento: 3. Segmentaciones (solo primer dia del mes) -------------------

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
personas      <- Consulta("SELECT * FROM CRMNALCLIENTE") %>%
  mutate(FecProceso = as.Date(FecProceso))
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
                  api_key = "HayDGkCmpQqmZB1rSkj2300JMDNpA2el")

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
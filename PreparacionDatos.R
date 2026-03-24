tictoc::tic("CRM NACIONAL")
print(paste0("***********", Sys.time(), "***********"))
setwd("/home/htamara/6_IndustriaNacional/CRM Cliente Nacional")
options(scipen = 99999)

# Librerías ----
racafe::Loadpkg(c("racafe", "tidyverse", "DBI", "lubridate"))
# Valores ----
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

# Extracción ----
print("**** Cargar Datos ****")

sucs <- data.frame(
  SucCod = c(15, 20, 26, 30, 32, 35, 50, 55),
  Sucursal = c("Bachué", "Medellín", "Popayán", "Armenia", "Arenales", "Pereira", "Bucaramanga", "Huila")) %>%
  mutate(across(where(is.character), LimpiarNombres))

NITPPAL <- CargarDatos("CRMNALCLIENTE") %>% 
  group_by(CLCliNit) %>% 
  mutate(FecProceso = as.Date(FecProceso)) %>% 
  filter(FecProceso == max(FecProceso)) %>% 
  select(PerCod = CLCliNit, CliNitPpal) %>% 
  distinct()

NCLIENTE <- ConsultaSistema("syscafe",  
                            "SELECT CliNit as PerCod, CliCont,  CliDir, CliDir1, CliTel, CliConCom, 
                                   CliTelCom, CliEmlCom, CiuExtCod
                            FROM NCLIENTE") %>% 
  left_join(ConsultaSistema("syscafe",  
                            "SELECT CiuExtCod, CiuExtNom
                            FROM NCIUEXT"), 
            by = "CiuExtCod") %>% 
  left_join(ConsultaSistema("syscafe",  
                            "select PerCod, PerRazSoc FROM NPERSONA"), 
            by = "PerCod") %>% 
  left_join(NITPPAL, by = join_by(PerCod)) %>%
  mutate(CliNitPpal = ifelse(is.na(CliNitPpal), PerCod, CliNitPpal),
         across(where(is.character), \(x) ifelse(x %in% c("","."), NA, x)))

NUNIMEDI <- ConsultaSistema("syscafe",
                            "SELECT UMeCod, UMeNom as UnidadMedida, UMeFac, UMeSgl 
                            FROM NUNIMEDI")

ped <- ConsultaSistema("syscafe",  
                       "SELECT PdcCod, PdcLin, PdcCan, LinNegCod, LinProCod 
                         FROM EXPPEDI1 
                         WHERE CiaCod =10") %>% 
  inner_join(ConsultaSistema("syscafe",  
                             paste0("SELECT PdcCod, UMeCod, PdcUsu
                                    FROM EXPPEDID 
                                    WHERE CiaCod = 10 AND CliNit <>32 AND
                                    PdcEst = 'A' AND PdcVtaNal = 1")), 
             by = "PdcCod") %>% 
  left_join(NUNIMEDI, by = "UMeCod") %>% 
  select(PdcCod, PdcLin, Usuario = PdcUsu, UMeFac)
rm(NUNIMEDI)

ConsultaSistema("syscafe",
                "select top 10 * from FCTFACNA where FcnNum IN (304361, 304305)") |> glimpse()

ConsultaSistema("syscafe",
                "select top 10 * from FCTFACNA where FcnNum = 1013051") |> glimpse()


fact <- ConsultaSistema("syscafe",
                        "SELECT CLLotCod, MAX(FecFact) AS FecFact
                        FROM (
                          SELECT DISTINCT F1.FcnLot AS CLLotCod, F2.FcnFec AS FecFact
                          FROM FCTFACN1 F1
                          LEFT JOIN FCTFACNA F2 ON F1.FcnNum = F2.FcnNum
                          WHERE F1.CiaCod = 10 AND F2.CiaCod = 10
                        ) AS sbq
                        GROUP BY CLLotCod;") 

fact2 <- ConsultaSistema("syscafe",
                         "SELECT DISTINCT F1.FcnLot AS CLLotCod, F2.FcnFec AS FecFact, F1.FcnNum as NumFact
                          FROM FCTFACN1 F1
                          LEFT JOIN FCTFACNA F2 ON F1.FcnNum = F2.FcnNum
                          WHERE F1.CiaCod = 10 AND F2.CiaCod = 10 and F2.FcnEtd ='C'"
                         )

data <- ConsultaSistema("syscafe", 
                        query = "SELECT CLSucCod, CLLotCod, CLPdcCod, CLPdcLin, CLPdcAnoEm*100 + CLPdcMesEm as Periodo,
                                         CLPdcCntCl, CLLotCan as SacLote, CLLotFec as FecAsignLote,
                                         CLotrCod as CodOrdTril, CLOTrFec as FecOrdTril,
                                         CLLotSacPr as SacProd, CLLotFecPr as FecProd,
                                         CLIDeCod as CodDesp, CLIDeFec as FecDesp, CLLotSacDe as SacDesp,
                                         CLPdcFctAD, CLLotSacFa, CLPdcCanFa,
                                         CLLinNegCo,
                                         CLLotSacXP, CLLotPenXD, CLLotDesXF,
                                         CLCliNit, CLCliRazSo,
                                         CLLinNegCo LinNegCod, CLLinNegNo, CLLinProCo LinProCod, CLLinProNo, 
                                         CLMCCod MCCod, CLMCNom, CLMrcCod MrcCod, CLMrcNom
                                  FROM EXPCUALO 
                                  WHERE CiaCod = 10 AND CLPdcVtaNa = 1 AND CLCliNit <> 32 AND CLLinNegCo <>0 AND CLLotCan >0") %>% 
  left_join(sucs, by = c("CLSucCod"="SucCod")) %>% 
  left_join(NITPPAL, by = c("CLCliNit"="PerCod")) %>%
  mutate(CliNitPpal = ifelse(is.na(CliNitPpal), CLCliNit,CliNitPpal)) %>%
  left_join(NCLIENTE %>% 
              filter(PerCod==CliNitPpal) %>% 
              select(-PerCod),
            by = "CliNitPpal") %>%
  left_join(ped,  by= c("CLPdcCod"="PdcCod", "CLPdcLin"="PdcLin")) %>% 
  left_join(CargarDatos("CRMNALMARLOT") %>% 
              filter(TIP == "NAL") |> 
              group_by(CLLotCod = LOTE, ) |> 
              summarise(Margen = sum(MARGEN),
                        FecFact = as.Date(max(FECFACTURA))) |> 
              bind_rows(
                
                t3 <- fact2 |> 
                  anti_join(CargarDatos("CRMNALMARLOT") |> 
                              rename(CLLotCod = LOTE),
                            by = join_by(CLLotCod)
                            ) |> 
                  group_by(CLLotCod) |> 
                  summarise(FecFact = as.Date(max(FecFact)))
                )
            , by = c("CLLotCod"="CLLotCod")) %>% 
  left_join(CargarDatos("CRMNALPRODS"), by = join_by(LinNegCod, LinProCod, MCCod, MrcCod)) %>% 
  mutate(FechaEmbarque = as.Date(paste(Periodo, "01"), "%Y%m%d"),
         across(contains("Fec"), ~ if_else(as.Date(.) == as.Date("1753-01-01"), as.Date(NA), as.Date(.))),
         CliNitPpal = ifelse(is.na(CliNitPpal), CLCliNit, CliNitPpal),
         CLLinNegNo = ifelse(CLLinNegNo == "DIFERENCIADOS", "A LA MEDIDA", CLLinNegNo),
         LinNegCod = ifelse(LinNegCod == 20000, 21000, LinNegCod),
         Kilos = SacDesp * UMeFac,
         MarKilo = Margen / Kilos) %>% 
  group_by(LinNegCod, CLLinProNo) |> 
  mutate(Margen = ifelse(is.na(Margen), 
                         mean(ifelse(FecDesp >= PrimerDia(Sys.Date()) - months(3), MarKilo,NA), na.rm = T) * Kilos,
                         Margen
                         )   
         ) |> 
  ungroup() |> 
  select(CLSucCod, CLLotCod, FechaEmbarque, CLPdcCod, CLPdcLin, CLPdcCntCl, Usuario,SacLote, FecAsignLote,
         CodOrdTril, FecOrdTril, SacProd, FecProd, CodDesp, FecDesp, SacDesp, FecFact, Margen, MarKilo, UMeFac, Kilos, 
         CLPdcFctAD, CLLotSacFa, CLPdcCanFa,
         LinNegCod, CLLinNegNo, LinProCod, CLLinProNo, MCCod, CLMCNom, MrcCod, CLMrcNom,
         Categoria2024, Producto2024, Categoria2025, Producto2025,
         CliNitPpal, CLCliNit, PerRazSoc, CliCont,CliDir,CliDir1,
         CliTel, CliConCom, CliTelCom, CliEmlCom, CiuExtNom,
         CLLotSacXP, CLLotPenXD, CLLotDesXF)
  
rm(ped, sucs, NITPPAL, fact, fact2)

# Limpieza y exportación ----
save.image("CRMNacional/data/data.RData")
rm(list = ls())
gc()
tictoc::toc()

# Publicacion ----

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

library(tidyverse)
library(readxl)
library(bizdays)
library(data.table)
library(stringi)
library(RODBC)
library(writexl)
library(dplyr)
library(lubridate)
library(Microsoft365R)
library(bizdays)


## BDD----
conn_csys <- odbcConnect("BDD_Cafesys","AppQlikRcaLee","Rac@CafQlikS2022*")

## Tablas maestras ----

tipo_negocio <-  sqlQuery(conn_csys, "select TNeCod, TNeNom AS Tipo_Negocio FROM TNETRN") 

tipo_calidad  <-  sqlQuery(conn_csys, "select TipCaf, CalCod, CalNom AS Calidad FROM CALTRN") 

tipo_cafe <-  sqlQuery(conn_csys, "select TipCaf, TipNom AS Tipo_Cafe FROM TCATRN") 

ciudad <- sqlQuery(conn_csys, "select CiuCod, CiuNom AS Ciudad, DepCod FROM NCIUDAD") %>% 
  left_join(sqlQuery(conn_csys, "select DepCod, DepNom AS Departamento FROM NDEPTOS"), by =c("DepCod")) %>% 
  select(-DepCod) %>% 
  mutate(Ciudad = str_trim(Ciudad),
         Departamento = str_trim(Departamento))

proveedor <- sqlQuery(conn_csys, "select PerCod AS NIT_Proveedor, PerRazSoc AS Proveedor, CiuCodSt2 AS CiuCod FROM NPERSONA") %>% 
  left_join(ciudad, by = c("CiuCod")) %>% 
  select(-CiuCod) %>% 
  rename(Ciudad_Proveedor ="Ciudad",
         Departamento_Proveedor = "Departamento") %>% 
  left_join(sqlQuery(conn_csys, "select PrvNit, PrvCls AS Clase_Proveedor FROM VNPROVEE"), by = c("NIT_Proveedor"="PrvNit"))

asociado <- sqlQuery(conn_csys, "select PerCod AS NIT_Asociado, PerRazSoc AS Asociado FROM NPERSONA")
 
tipo_asociado <- sqlQuery(conn_csys, "select PrvNit AS NIT_Proveedor, AsoCed AS NIT_Asociado, 
                          AsoCls AS Clase_Asociado FROM COOTRN")


facturas <- sqlQuery(conn_csys, "select CiaCod, SucCod, FCoNro, RegNro, FCoFch AS Fecha_Factura, FCoKls, FCoSbt,
                     FCoPrvNit AS NIT_Proveedor,FCoAsoCed AS NIT_Asociado FROM FCOTRN WHERE CiaCod =10")

facturas2 <- sqlQuery(conn_csys, "select CiaCod, SucCod, FCoNro, RegNro, TNeCod, FcoLotCon, FCoCalCod, 
                      FCoTipCaf FROM FCOTRN1 WHERE CiaCod =10")

tabla_facturas <- facturas %>% 
  left_join(facturas2, by = c("CiaCod", "SucCod", "FCoNro", "RegNro")) %>% 
  left_join(proveedor, by = c("NIT_Proveedor")) %>% 
  left_join(asociado, by=c("NIT_Asociado")) %>% 
  left_join(tipo_asociado, by =c("NIT_Proveedor", "NIT_Asociado")) %>% 
  left_join(tipo_negocio, by = c("TNeCod")) %>% 
  left_join(tipo_calidad, by = c("FCoTipCaf" = "TipCaf",
                                 "FCoCalCod"= "CalCod")) %>% 
  left_join(tipo_cafe, by = c("FCoTipCaf" = "TipCaf")) %>% 
  group_by(CiaCod, SucCod, RegNro, Fecha_Factura, Tipo_Negocio, Calidad, Tipo_Cafe, NIT_Proveedor, 
           Proveedor, Clase_Proveedor, Ciudad_Proveedor, Departamento_Proveedor, NIT_Asociado, 
           Asociado, Clase_Asociado) %>% 
  summarise(Kilos_Brutos = sum(FCoKls),
            Kilos_Netos = sum(FcoLotCon),
            Valor_Compra = sum(FCoSbt)) %>% 
  ungroup()
  

entradas <-  sqlQuery(conn_csys, "select CiaCod, SucCod, RegNro, RegCiuC AS CiuCod, EvaPorHum, EvaGrsMue,
                      EvaGrsAlm, EvaGrsMMa, EvaGrsPas, EvaGrsCon, EvaGrsRip, EvaGrsPep 
                      FROM REGTRN WHERE CiaCod = 10") %>% 
  mutate(EvaGrsMer = EvaGrsMue-EvaGrsAlm,
         PorRendimiento = (70*EvaGrsMue)/EvaGrsMMa,
         PorRendimiento = ifelse(is.na(PorRendimiento),0,PorRendimiento)) %>% 
  left_join(ciudad, by = c("CiuCod")) %>% 
  select(-CiuCod)


tabla_informe <- tabla_facturas %>% 
  left_join(entradas, by = c("CiaCod", "SucCod", "RegNro")) %>% 
  filter(Fecha_Factura>=as.Date("2016-01-01") & SucCod!= 12 &  SucCod!=32) %>% 
  mutate(Sucursal = case_when(SucCod == 15 ~ "Bachué",
                              SucCod == 50 ~ "Bucaramanga",
                              SucCod == 55 ~ "Huila",
                              SucCod == 26 ~ "Popayán",
                              SucCod == 30 ~ "Armenia",
                              SucCod == 35 ~ "Pereira",
                              SucCod == 20 ~ "Medellín"),
         Tipo_Negocio = str_trim(Tipo_Negocio),
         Calidad = str_trim(Calidad),
         Tipo_Cafe = str_trim(Tipo_Cafe),
         Proveedor = str_trim(Proveedor),
         Asociado= str_trim(Asociado),
         Clase_Proveedor = ifelse(is.na(Clase_Proveedor) | Clase_Proveedor == 0, "Intermediario",
                                  case_when(Clase_Proveedor == 1 ~ "Productor",
                                            Clase_Proveedor == 2 ~ "Intermediario",
                                            Clase_Proveedor == 3 ~ "Productor e Intermediario")),
         Clase_Asociado = ifelse(is.na(Clase_Asociado) | Clase_Asociado == 0, "",
                                 case_when(Clase_Asociado == 1 ~ "Productor",
                                            Clase_Asociado == 2 ~ "Intermediario",
                                            Clase_Asociado == 3 ~ "Productor e Intermediario"))) %>% 
  rename(PorHumedad = "EvaPorHum",
         GrsMuestra = "EvaGrsMue",
         GrsAlmendra = "EvaGrsAlm",
         GrsMalla14 = "EvaGrsMMa",
         GrsMerma = "EvaGrsMer",
         GrsPasilla = "EvaGrsPas",
         GrsConsumo = "EvaGrsCon",
         GrsRipio = "EvaGrsRip",
         GrsPepas = "EvaGrsPep") %>% 
  select(-c("CiaCod", "SucCod"))
  

aux1 <- sqlQuery(conn_csys, "select CiaCod, SucCod, OfeNro,
                 PrvNit, AsoCed, TipCaf, CalCod, OfeFch,
                 OfeFchVto, OfeNroIni,OfeTotKls, OfeSldKls, 
                 OfeAntTSa, OfeNegKls, OfePrima, OfeRegNro, OfeNroAnt,
                 OfeOriNum,OfeEst, OfeFut FROM OFETrn 
                 WHERE CiaCod=10 AND SucCod  <> 12 AND SucCod <>32 AND OfeFch>='2016-01-01'")


aux2 <- sqlQuery(conn_csys, "select CiaCod, SucCod, FCoNro, RegNro, TNeCod, OfeNro,FCoKls1,
FCoPCaNeg, FCoPKiNeg, FCoVal FROM FCoTrn1 WHERE CiaCod=10 AND SucCod  <> 12 AND SucCod <>32")

aux3 <- sqlQuery(conn_csys, "select * FROM OFETrn WHERE CiaCod=10 AND SucCod  <> 12 AND SucCod <>32 AND OfeFch>='2016-01-01'")


write_xlsx(facturas, "C:/Trabajo/OneDrive - Racafe/Informe Comercial/fact2.xlsx")

table <- sqlTables(conn_csys)


RODBC::odbcCloseAll()


rm(conn_csys, asociado, ciudad, entradas, facturas, facturas2, proveedor, tabla_facturas,
   tipo_asociado, tipo_cafe, tipo_calidad, tipo_negocio)

save.image("data/datos.Rdata")

#setwd("C:/Trabajo/OneDrive - Racafe/Informe Comercial/APP")


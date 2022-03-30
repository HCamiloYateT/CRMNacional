library(apputils)
library(shinydashboard)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(readxl)
library(tidyverse)
library(dplyr)
library(dashboardthemes)
library(DT)
library(scales)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(RODBC)
library(Microsoft365R)
library(bizdays)
library(RQuantLib)
library(tictoc)



## Datos Previos----
tic("Cargue de Datos")

conn <- odbcConnect("BDD_CAFE", "AppQlikRcaLee","Rac@CafQlikS2022*")


festivos <- read_excel("C:/Trabajo/OneDrive - Racafe/Documentos/data/ListadoFestivos.xlsx")
cal <- create.calendar(name = "mycal", weekdays=c("saturday", "sunday"), holidays = festivos$Dia,
                       start.date = as.Date("2000-01-01"), end.date = as.Date("2022-12-31"),
                       adjust.to = adjust.next, adjust.from = adjust.next)

fecha_consulta <- bizdays::offset(dates = Sys.Date(), n = -1, cal = cal)


## Documentos----

od <- get_business_onedrive()

#od$download_file("Compartir/Control Interno/Dolares y Cafe/Informacion.xlsx", 
#                 dest = "data/informacion.xlsx", 
#                 overwrite = T)

od$download_file("Reporte Dolares y Cafe/APP/Informacion.xlsx",
                 dest = "data/informacion.xlsx",
                 overwrite = T)


Saldo_Cta_Compensacion <- read_excel("data/informacion.xlsx", range = "A2:C12")
DolaresxContribuir <- read_excel("data/informacion.xlsx", range = "B15:B16")
limites_cafe <- read_excel("data/informacion.xlsx", range = "A22:B23")
limites_dolares <- read_excel("data/informacion.xlsx", range = "A25:B26")
limite_SldoEC <- read_excel("data/informacion.xlsx", range = "A28:A29")
limite_Corto <- read_excel("data/informacion.xlsx", range = "A31:A32")
Ajustes_Cafe <- read_excel("data/informacion.xlsx", range = "A36:B39")
Ajustes_Dolares <- read_excel("data/informacion.xlsx", range = "A42:B45")
Saldo_Diferencial <- read_excel("data/informacion.xlsx", range = "A48:D52")
Saldo_Diferencial1 <- read_excel("data/informacion.xlsx", range = "A54:B56")
Cta_Futuros <- read_excel("data/informacion.xlsx", range = "A59:D64")

## Cafe x Asignar ----
tabla_CobCafexAsi <- sqlQuery(conn, "select CiaCod, SucCod, OFCodOfe, OFCodFac, CxAKilos, CxAFch, CxAPrexCrg FROM COBCAFXA")
tabla_EXPCOFE1 <- sqlQuery(conn, "select CiaCod, SucCod, OFCodOfe, OFCodFac, OfFDinFac, OfFacCFP FROM EXPCOFE1")
PrecioCarga <- sqlQuery(conn, "SELECT * FROM POSCOBER") %>% 
  filter(PCobFch==fecha_consulta) %>% 
  summarise(Precio = PCobPrmxCr) %>% 
  as.numeric()

cafexasignar <- tabla_CobCafexAsi %>% 
  inner_join(tabla_EXPCOFE1, by = c("CiaCod",
                                    "SucCod",
                                    "OFCodOfe",
                                    "OFCodFac"))%>%
  filter(as.Date(CxAFch)== as.Date(fecha_consulta)) %>% 
  mutate(Sacos = CxAKilos/OfFDinFac, 
         indice = OfFDinFac/96.89, 
         PxCargax96.89=CxAPrexCrg*indice,
         PxSacos= Sacos* PxCargax96.89) %>%
  group_by(OfFacCFP) %>% 
  summarise(Total_Sacos=sum(Sacos), 
            PrecioxCarga =sum(PxSacos)/Total_Sacos
  ) %>% 
  mutate(PrecioCarga = PrecioCarga,
         DiferenciaPrecio = PrecioxCarga -PrecioCarga,
         DiferenciaHoy = (-DiferenciaPrecio/125)*(Total_Sacos*96.89),
         Orden =  case_when(OfFacCFP == "N" ~ 1,
                            OfFacCFP == "R" ~ 2,
                            OfFacCFP == "A" ~ 3,
                            OfFacCFP == "S" ~ 4
         ),
         OfFacCFP =  case_when(OfFacCFP == "N" ~ "Disponible",
                               OfFacCFP == "R" ~ "Reservado Provisión Ofertas",
                               OfFacCFP == "A" ~ "Negocio a la Medida",
                               OfFacCFP == "S" ~ "Comprado en el Exterior"
         )
  ) %>% 
  mutate_at(c("PrecioCarga", "DiferenciaPrecio","DiferenciaHoy"),
            list(~ifelse(OfFacCFP %in% c("Reservado ProvisiÃ³n Ofertas", "Disponible"),.,NA))) %>% 
  arrange(Orden) %>% 
  rename(Tipo_Negocio=OfFacCFP) %>% 
  select(-Orden)




## Cuenta Café ----
SaldoI_Cafe <- sqlQuery(conn, "select * FROM CobCafxA") %>% 
  filter(CxAFch == fecha_consulta-days(1)) %>% 
  summarise(Sacos = sum(CxASacos)) %>% as.numeric()

cafe <- sqlQuery(conn, "Select * FROM PosCober") %>% 
  filter(PCobFch == fecha_consulta)


Liquidaciones <- sqlQuery(conn, "select AsCFch, AsCSacAsg, ASCPosDia from CobAsgCa") %>% 
  filter(AsCFch == fecha_consulta & ASCPosDia == "S") %>% 
  group_by(AsCFch)  %>% 
  summarise(sum(AsCSacAsg)) %>% 
  select(-AsCFch) %>% as.numeric()


Ajustes <- sqlQuery(conn, "select AsCFch, AsCSacAsg, ASCPosDia from CobAsgCa") %>% 
  filter(AsCFch == fecha_consulta & ASCPosDia == "N") %>% 
  group_by(AsCFch)  %>% 
  summarise(sum(AsCSacAsg)) %>% 
  select(-AsCFch) %>% as.numeric()

Total_Ajustes_cuenta_Cafe <- Ajustes_Cafe %>% summarise(sum(Detalle, na.rm = T)) %>% as.numeric()



cuenta_cafe1<-sqlQuery(conn, "select PCobFch, PCobCmpCaf, PCobCanCaf FROM PosCober") %>% 
  filter(PCobFch == fecha_consulta) %>%
  mutate(Saldo_Anterior = SaldoI_Cafe,
         Total_Ajustes = (sum(Ajustes,Total_Ajustes_cuenta_Cafe, na.rm = T)),
         Liquidaciones = -Liquidaciones,
         Total = sum(PCobCmpCaf,PCobCanCaf,SaldoI_Cafe,Total_Ajustes,Liquidaciones, na.rm = T),
         Diferencia_Calculo = Total-sum(cafexasignar$Total_Sacos)) %>%
  rename(Fecha = PCobFch,
         Compras =PCobCmpCaf,
         Cancelaciones = PCobCanCaf,
         "Total Ajustes" = Total_Ajustes) %>% 
  mutate_at(c("Compras", "Cancelaciones", "Total Ajustes", "Liquidaciones", "Saldo_Anterior", 
              "Total", "Diferencia_Calculo"), list(~comma(., accuracy = 0.1))) %>% 
  rename("Saldo Anterior"= Saldo_Anterior,
         "Diferencia respecto al calculado" = Diferencia_Calculo) %>% 
  select(-Fecha)


cuenta_cafe <- select(cuenta_cafe1, `Saldo Anterior`, Compras, Cancelaciones, `Total Ajustes`, Liquidaciones, Total, `Diferencia respecto al calculado`) %>% 
  pivot_longer("Saldo Anterior":"Diferencia respecto al calculado", names_to = "Detalle", values_to = "Valor")

## Dolares x Negociar ----
dolaresxAsignar<- sqlQuery(conn, "SELECT DxAFch, FrwNum, DxAFrwOpe, DxADolar, 
                                 DxAFrwFec, DxAFrwTasa FROM CobDolxA") %>% 
  filter(DxAFch == fecha_consulta) %>% 
  mutate(DxAFrwOpe = case_when(DxAFrwOpe == "S" ~"Spot",
                               DxAFrwOpe == "F" ~"Forward"))


total_dolaresxAsignar <- dolaresxAsignar %>% summarise(sum(DxADolar)) %>% as.numeric()
TRM_PromedioDA <-((dolaresxAsignar %>% mutate(producto = DxADolar*DxAFrwTasa)) %>% 
  summarise(sum(producto)) %>% as.numeric())/total_dolaresxAsignar  


TRM_PromedioDA[is.na(TRM_PromedioDA)]<-0

## Dolares Faltantes ----
table_dxAsignar <- sqlQuery(conn, "select * FROM CobPxAsD") %>% filter(PxADFch == fecha_consulta) %>% 
  left_join(sqlQuery(conn, "select PdcCod, PdcCntCli, CliNit FROM EXPPEDID") %>% 
              left_join(sqlQuery(conn, "select PerCod, PerRazSoc from NPERSONA"), 
                        by = c("CliNit" = "PerCod")),
            by = "PdcCod") %>% mutate(Dolares_Faltantes = -PxADDolPte)

dolares_faltantes<- table_dxAsignar %>% select(PdcCod, PdcCntCli,PerRazSoc,
                                               Dolares_Faltantes, PxADTrmPO) %>% 
  rename(Codigo_Pedido = PdcCod,
         Pedido = PdcCntCli,
         Cliente = PerRazSoc,
         Tasa_Promedio = PxADTrmPO)


total_dolaresF <- dolares_faltantes %>% summarise(sum(Dolares_Faltantes)) %>% as.numeric()
TRM_PromedioDF <- ((dolares_faltantes %>% mutate(producto = Dolares_Faltantes*Tasa_Promedio)) %>% 
                     summarise(sum(producto)) %>% as.numeric())/total_dolaresF
TRM_PromedioDF[is.na(TRM_PromedioDF)]<-0


total_dolares <- total_dolaresxAsignar+total_dolaresF

TRM_Promedio <- case_when(TRM_PromedioDA ==0 ~ TRM_PromedioDF,
                          TRM_PromedioDF ==0 ~ TRM_PromedioDA,
                          TRM_PromedioDA !=0 & TRM_PromedioDF!=0 ~ mean(TRM_PromedioDA,TRM_PromedioDF))


## Cuenta Dolares ----
Total_Ajustes_Cuenta_Dolares <- Ajustes_Dolares %>% summarise(sum(Detalle, na.rm = T)) %>% as.numeric()

cuenta_dolares1<- sqlQuery(conn, "select PCobFch, PCobSIDol, PCobCmpDol, PCobAjuDol, PCobAsgDol FROM PosCober") %>% 
  filter(PCobFch == fecha_consulta) %>% 
  mutate(
         Total_Ajustes = sum(PCobAjuDol, Total_Ajustes_Cuenta_Dolares, na.rm = T), 
         Total = sum(PCobSIDol,PCobCmpDol,Total_Ajustes,PCobAsgDol, na.rm = T),
         Diferencia_Calculo = Total- total_dolares) %>% 
  rename("Saldo Anterior" = PCobSIDol,
         "Dólares del dia" = PCobCmpDol,
         "Total Ajustes" = Total_Ajustes,
         "Liquidaciones" = PCobAsgDol,
         "Diferencia respecto al calculado"= Diferencia_Calculo) %>%
  mutate_at(c("Saldo Anterior", "Dólares del dia", "Total Ajustes", "Liquidaciones", "Total", "Diferencia respecto al calculado"), dollar) %>% 
  select(-PCobFch, -PCobAjuDol)


cuenta_dolares <- cuenta_dolares1 %>% 
  pivot_longer("Saldo Anterior":"Diferencia respecto al calculado", names_to = "Detalle", values_to = "Valor")

diferencia_dolares <- (sqlQuery(conn, "select PCobFch, PCobSIDol, PCobCmpDol, PCobAjuDol, PCobAsgDol FROM PosCober") %>% 
  filter(PCobFch == fecha_consulta) %>% 
  mutate( Total_Ajustes = sum(PCobAjuDol, Total_Ajustes_Cuenta_Dolares, na.rm = T), 
          Total = sum(PCobSIDol,PCobCmpDol,Total_Ajustes,PCobAsgDol, na.rm = T), 
          Diferencia_Calculo = Total- total_dolares)) %>% 
  select(Diferencia_Calculo) %>% as.numeric()


## Contratos x Fijar----

table_contratos <- sqlQuery(conn, "select CiaCod, PxACFch, PdcCod, PxACSacPte from CobPxAsC") %>% 
  filter(PxACFch==fecha_consulta)


table_pedidos <- sqlQuery(conn, "select CiaCod, PdcCod, PdcCntCli, CliNit, PdcSig, PdcPtos, 
                          PdcMesEmb, PdcAnoEmb, PdcDifFij FROM EXPPEDID") %>% 
  mutate(Diferencial_Venta = case_when(PdcSig == "+" ~ PdcPtos,
                                       PdcSig == "-" ~ -PdcPtos,
                                       PdcSig == " " ~ PdcDifFij),
         FechaEmbarque = ymd(paste(PdcAnoEmb, PdcMesEmb, 1, sep = "/"))) %>% 
  left_join(sqlQuery(conn, "select PerCod, PerRazSoc from NPERSONA"), by = c("CliNit" = "PerCod"))



table_extCosto <- sqlQuery(conn, "select CiaCod, PdcCod, PdcExtCvr From EXPEXTCT") %>% 
  group_by(PdcCod) %>% 
  summarise(Extra_Costo = sum(PdcExtCvr, na.rm = T))



table_marcas <- (sqlQuery(conn, "select distinct PdcCod, MrcCod FROM EXPPEDI1") %>% 
                   left_join((sqlQuery(conn, "Select MrcCod, MrcNom FROM NMARCAS")), by = "MrcCod")) %>% 
  group_by(PdcCod) %>% summarise(Marca=paste(trimws(MrcNom), collapse = " / "))


table_contratosxF <- (((table_contratos %>% left_join(table_pedidos, by =  c("CiaCod",
                                                                            "PdcCod"))) %>% 
                        left_join(table_extCosto, by ="PdcCod")) %>% 
  left_join(table_marcas, by = "PdcCod")) %>% 
  mutate_at(c("Extra_Costo"), ~replace(., is.na(.), 0))



contratos<- table_contratosxF %>% select(FechaEmbarque, PdcCod, PdcCntCli, Marca,
                                         PerRazSoc, PxACSacPte, Diferencial_Venta, Extra_Costo) %>% 
  mutate(Diferencial_Neto = Diferencial_Venta-Extra_Costo) %>% 
  rename(Codigo_Pedido = PdcCod,
         Pedido = PdcCntCli,
         Cliente = PerRazSoc,
         Sacos = PxACSacPte)


resumen_contratos1 <- (contratos %>% select(FechaEmbarque, Sacos, Diferencial_Venta, Extra_Costo, Diferencial_Neto) %>% 
                        mutate(productod = Sacos*Diferencial_Venta,
                               productoe = Sacos*Extra_Costo,
                               productodn = Sacos*Diferencial_Neto,
                               sacosex = case_when(productoe ==0 ~ 0,
                                                   productoe != 0 ~ Sacos))) %>% 
                       select(-(Diferencial_Venta), -(Extra_Costo), -(Diferencial_Neto))


resumen_contratos <- resumen_contratos1 %>% 
  group_by(FechaEmbarque) %>% 
  summarise(Total_Sacos = sum(Sacos, na.rm = T),
            Total_SacosE = sum(sacosex, na.rm = T),
            Total_DV = sum(productod, na.rm = T)/Total_Sacos,
            Total_EC = sum(productoe, na.rm = T)/Total_SacosE,
            Total_DN = sum(productodn, na.rm = T)/Total_Sacos) %>%
  mutate_at(c("Total_DV", "Total_EC", "Total_DN"), ~replace(., is.na(.), 0)) %>% 
  select(-Total_SacosE)



total_Sacos_CF <- contratos %>% summarise(-sum(Sacos)) %>% as.numeric()

Open_Temporal <- sqlQuery(conn, "select PCobFch, PCobCafOpe From PosCober") %>% 
  filter(PCobFch == fecha_consulta) %>% select(PCobCafOpe) %>% as.numeric()

total_corto <- sum(total_Sacos_CF, Open_Temporal, na.rm = T)

ventas_sin_Diferencial <- contratos%>%filter(Diferencial_Venta == 0) %>% 
  summarise(sum(Sacos)) %>% as.numeric()

saldo_corto <- sum(total_corto, ventas_sin_Diferencial, na.rm = T)


fecha_3m <- min(resumen_contratos$FechaEmbarque)+days(120)

corto_3m <- contratos %>% filter(Diferencial_Venta != 0 & FechaEmbarque<=fecha_3m) %>% 
  summarise(-sum(Sacos)) %>% as.numeric()

pcorto_3m <-percent(corto_3m/saldo_corto, accuracy = 0.01)


saldo_contratos <- data.frame(c("Total Sacos", "Open Temporal", "Total Corto", "Ventas sin Diferencial", "Saldo Corto"),
                  c(total_Sacos_CF, Open_Temporal, total_corto, ventas_sin_Diferencial, saldo_corto))

names(saldo_contratos) <- c("Detalle", "Valor")

## Saldo Diferencial ----
Subtotal_SldoD <- Saldo_Diferencial %>% summarise(sum(`US$`, na.rm = T)) %>% as.numeric()
Subtotal_SldoDTRM <- (Saldo_Diferencial %>% mutate(producto = `US$`*`TASA PROM CONTABILIDAD NIIF`)) %>% 
  summarise((sum(producto, na.rm = T))/Subtotal_SldoD) %>% as.numeric()
Subtotal_SldoDPe <- Saldo_Diferencial %>% summarise(sum(`PESOS CONTABILIDAD NIIF`, na.rm = T)) %>% as.numeric()
SdloD_Total <- sum((Saldo_Diferencial1 %>% 
                      summarise(sum(`US$`, na.rm = T)) %>% 
                      as.numeric()), Subtotal_SldoD, na.rm = T)


## Cuenta de Futuros ----

  Saldo_CtaCAdm <- Cta_Futuros %>% 
  select("Detalle", "US$") %>% 
  filter(Detalle == "SALDO EN ADM") %>% 
  summarise(sum(`US$`)) %>% as.numeric()

  Saldo_CtaCStonex <- Cta_Futuros %>% 
  select("Detalle", "US$") %>% 
    filter(Detalle == "SALDO EN STONEX") %>% 
    summarise(sum(`US$`)) %>% as.numeric()

  Subtotal_CtaF <- Cta_Futuros %>% 
  filter(Detalle == "CUENTA DE COMPENSACION" |
         Detalle == "MARGENES DE FACTURAS PENDIENTES") %>% 
  summarise(sum(`US$`, na.rm = T)) %>% as.numeric()

Subtotal_CtaFPe <- Cta_Futuros %>% 
  filter(Detalle == "CUENTA DE COMPENSACION" |
           Detalle == "MARGENES DE FACTURAS PENDIENTES") %>% 
  summarise(sum(PESOS, na.rm = T)) %>% as.numeric()

Subtotal_CtaFTRM <- Subtotal_CtaFPe/Subtotal_CtaF

limite_Comisionista <- limite_SldoEC %>% as.numeric()



## Cuenta Compensación ----
Sldo_DPref <- Saldo_Cta_Compensacion %>% filter(DETALLE == "RESOLUCION-PREF") %>% 
  summarise(sum(`SALDOS USD`, -(ENTREGA), na.rm = T)) %>% as.numeric() 
Total_Cta_Comp <- Saldo_Cta_Compensacion %>% summarise(sum(`SALDOS USD`, na.rm = T)) %>%as.numeric()


aux1 <- Saldo_Cta_Compensacion %>% 
  filter(DETALLE == "RESOLUCION-PREF") %>% 
  summarise(sum(ENTREGA)) %>% as.numeric()

## Cerrar BDD ----


RODBC::odbcCloseAll()


# Guardar ----
rm(od, conn)
save.image("data/datos.Rdata")

toc()
library(racafe)
library(tidyverse) 
library(rvest)

uid = Sys.getenv("SYS_UID")
pwd = Sys.getenv("SYS_PWD")

# Función para extraer todos los indicadores
get_fnc_data <- function() {
  tryCatch({
    url <- 'https://federaciondecafeteros.org/wp/'
    contenido <- read_html(url)
    
    precio <- contenido %>%
      html_nodes("ul.lista li[tabindex='1']") %>%
      html_nodes("strong") %>%
      html_text() %>% 
      .[1]
    
    bolsa <- contenido %>%
      html_nodes("ul.lista li[tabindex='2']") %>%
      html_nodes("strong") %>%
      html_text() %>% 
      .[1]
    
    list(
      precio = as.numeric(gsub("\\D", "", precio)),
      bolsa = as.numeric(gsub(",", ".", bolsa))
    )
  }, error = function(e) {
    warning("Error obteniendo datos de FNC: ", e$message)
    # Valores predeterminados en caso de error
    list(precio = NA, bolsa = NA)
  })
}
get_system_data <- function(uid, pwd) {
  tryCatch({
    # Consulta para obtener TRM y precio promedio
    ConsultaSistema("syscafe", "SELECT top 10 *  FROM EXPHOTR2 WHERE HTFec  = (SELECT MAX(HTFec ) FROM EXPHOTRA)")
    
    cons_main <- ConsultaSistema("syscafe",
                                 "SELECT
                                    (SELECT MedTRM FROM EXPMECAF WHERE MedFec = (SELECT MAX(MedFec) FROM EXPMECAF)) AS TRM"
    )
    
    # Contrato "C" de Nueva York de café
    fechai <- as.Date("2025-10-01")
    
    NY <- ConsultaSistema("syscafe", 
                          "WITH Futuros AS (
                                  SELECT 
                                      InFuFch AS Fecha,
                                      DATEFROMPARTS(CAST(AnoBol AS INT), CAST(MesBolCod AS INT), 1) AS Date,
                                      AVG(InFuCieVr) AS InFuCieVr,
                                      ROW_NUMBER() OVER (PARTITION BY InFuFch ORDER BY DATEFROMPARTS(CAST(AnoBol AS INT), CAST(MesBolCod AS INT), 1)) AS Posicion
                                  FROM INFFUT1
                                  WHERE CiaCod = 10 AND TipCFCod = 'A' AND 
                                        InFuFch >= (SELECT MAX(InFuFch) FROM INFFUT1 WHERE CiaCod = 10 AND TipCFCod = 'A')
                                        AND InFuCieVr > 0
                                  GROUP BY InFuFch, AnoBol, MesBolCod
                          )
                          SELECT Fecha, InFuCieVr AS NY
                          FROM Futuros
                          WHERE Posicion = 2")
    
    # Precio de carga
    PC <- ConsultaSistema("syscafe", 
                          "SELECT h2.HTFec,
                                  SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 ELSE h2.HTKilCom END) as KilTo,
                                  SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 
                                      ELSE ((h2.HTPreCom * (f.FactRen/125.0) - (f.FactRen * ((h.HTPreCon * f.PorCon/100.0) + 
                                                           (h.HTPrePas * f.PorPas/100.0) + (h.HTPreRip * f.PorRip/100.0))) +
                                                           (f.FactBas * ((h.HTPreCon * f.PorCon/100.0) + (h.HTPrePas * f.PorPas/100.0) + 
                                                           (h.HTPreRip * f.PorRip/100.0)))) * (125.0/f.FactBas) * h2.HTKilCom)
                                                           END) as PreTo
                          FROM EXPHOTR2 h2
                          LEFT JOIN EXPFACON f ON h2.FactSec = f.FactSec
                          LEFT JOIN EXPHOTRA h ON h2.HTFec = h.HTFec
                          WHERE h2.HTFec = (SELECT MAX(HTFec) FROM EXPHOTR2)
                          GROUP BY h2.HTFec
                          HAVING SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 ELSE h2.HTKilCom END) > 0")
    
    PrecioCarga <- PC %>% 
      group_by(Fecha = as.Date(HTFec)) %>% 
      summarise(PrecioCarga = mean(PreTo / KilTo, na.rm = TRUE), .groups = "drop") 
    
    
    cons_prices <- ConsultaSistema("syscafe",
                                   "SELECT HTPreRip, HTPrePas, HTPreCon,
                                          CASE WHEN HTSigno = '+' THEN HTPtos ELSE -HTPtos END AS Diferencial
                                   FROM EXPHOTRA
                                   WHERE HTFec = (SELECT MAX(HTFec) FROM EXPHOTR2);")
    
    # Compras Arenales
    compras_arenales <- ConsultaSistema("cafesys",
                                        "SELECT r.ResFch, r.TipCaf, r.CalCod, r.TotKls, r.ResVlrNeg, c.CalNom AS Producto 
                                         FROM RESDIA r
                                         LEFT JOIN CALTRN c
                                          ON r.TipCaf = c.TipCaf AND r.CalCod = c.CalCod
                                          WHERE r.CiaCod = 10 AND r.SucCod = 32 AND 
                                                r.ResFch >= DATEADD(month, -1, GETDATE()) AND 
                                                r.TMoDes = 'COMPRAS        '
                                        ") %>% 
      mutate(VlrKilo = ResVlrNeg/TotKls) %>% 
      group_by(Producto) %>% 
      summarise(VlrKilo = weighted.mean(VlrKilo, TotKls)) %>% 
      pivot_wider(names_from = Producto, values_from = VlrKilo)
    
    
    # Combinar resultados
    list(
      trm = cons_main$TRM,
      ny = NY$NY,
      precio_carga = PrecioCarga$PrecioCarga,
      precios_adicionales = cons_prices,
      precios_compras = compras_arenales
    )
  }, error = function(e) {
    warning("Error obteniendo datos del sistema: ", e$message)
    # Valores predeterminados en caso de error
    list(
      trm = NA,
      precio_carga = NA,
      precios_adicionales = data.frame(HTPreRip = NA, HTPrePas = NA, HTPreCon = NA)
    )
  })
}
extraer_indicadores <- function(uid, pwd) {
  # Obtener datos de FNC y sistema
  fnc_data <- get_fnc_data()
  system_data <- get_system_data(uid, pwd)
  
  # Crear dataframe con todos los indicadores
  indicadores_df <- data.frame(
    PrecioBolsa = fnc_data$bolsa,
    TRM = system_data$trm,
    PrecioFNC = fnc_data$precio,
    UGCFNC = (fnc_data$precio/125)/(70/(96.89)),
    PrecioNY = system_data$ny,
    PrecioCarga = system_data$precio_carga,
    Diferencial = system_data$precios_adicionales$Diferencial,
    UGCRacafe = (system_data$precio_carga/125)/(70/(96.89)),
    CALConsumo = system_data$precios_adicionales$HTPreCon,
    COMConsumo = system_data$precios_compras$CONSUMO,
    CALPasilla = system_data$precios_adicionales$HTPrePas,
    COMMolidos = system_data$precios_compras$MOLIDOS,
    COMSoluble = system_data$precios_compras$SOLUBLE,
    CALRipio = system_data$precios_adicionales$HTPreRip,
    COMRipio = pluck(system_data, "precios_compras", "RIPIO", .default = NA),
    COMRobusta = pluck(system_data, "precios_compras", "ROBUSTA", .default = NA),
    FechaActualizacion = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  return(indicadores_df)
}

# Uso:
df_indicadores <- extraer_indicadores(uid, pwd) %>% 
  relocate(FechaActualizacion)

racafe::AgregarDatos(df_indicadores, "CRMINDICADORES")
CargarDatos("CRMINDICADORES")


rm(list = ls(), envir = .GlobalEnv)
gc()

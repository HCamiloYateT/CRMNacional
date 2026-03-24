# Configuración inicial y carga de librerías
tictoc::tic("CRM NACIONAL")
print(paste0("*********** ", Sys.time(), " ***********"))
setwd("/home/htamara/6_IndustriaNacional/CRM Cliente Nacional")
options(scipen = 99999)

# Librerias --------------------------------------------------------------------
required_packages <- c("racafe", "tidyverse", "DBI", "lubridate", "blastula")
racafe::Loadpkg(required_packages)


# Datos ----
# Correos ----
## Correo Clientes sin informacion -----

pers_nuevas <- data %>% 
  select(LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc, Usuario, FecFact) %>% 
  distinct() %>% 
  anti_join(
    execute_query("SELECT DISTINCT CliNitPpal, LinNegCod FROM CRMNALCLIENTE"),
    by = join_by(CliNitPpal, LinNegCod)
  ) %>% 
  group_by(LinNegCod, CLLinNegNo, CliNitPpal, PerRazSoc) %>% 
  summarise(Usuarios = paste(unique(Usuario), collapse = "|"), 
            PrimeraFacturacion = min(FecFact),
            .groups = "drop") %>% 
  arrange(LinNegCod, PerRazSoc)

ppto <- readxl::read_excel("Presupuesto.xlsx", sheet = "Ppto") %>% 
  mutate(FecProceso = PrimerDia(Sys.Date(), "year"),
         Usr = "HCYATE" ) %>% 
  relocate(FecProceso, Usr) %>% 
  glimpse()

t1 <- CargarDatos("CRMNALCLIENTE") %>% 
  mutate(FecProceso = as.Date(FecProceso)) %>% 
  filter(year(FecProceso) != 2026) %>% 
  select(FecProceso:FecLead) %>% 
  bind_rows(ppto) %>% 
  group_by(CliNitPpal) %>% 
  fill(Mpio, Depto, FecLead, .direction = "downup") %>% 
  ungroup() %>% 
  glimpse()

# EscribirDatos(t1, "CRMNALCLIENTE")

# Validacion ----

CargarDatos("CRMNALCLIENTE") %>% 
  mutate(FecProceso = as.Date(FecProceso)) %>% 
  filter(year(FecProceso) == 2026) %>% 
  group_by(LinNegocio, Segmento) %>% 
  summarise(UnidadesComerciales = n_distinct(LinNegCod, CliNitPpal),
            Sacos = sum(SSPpto),
            MNFCCPpto = sum(MNFCCPpto), 
            .groups = "drop") %>% 
  mutate(Segmento = factor(Segmento, levels = c("GRANDES", "MEDIANO", "DETAL"))) %>% 
  arrange(LinNegocio, Segmento) %>% 
  gt::gt() %>% 
  gt::fmt_number(columns = Sacos, decimals = 0, use_seps = TRUE) %>% 
  gt::fmt_currency(columns = MNFCCPpto, currency = "COP",decimals = 0)

CargarDatos("CRMNALCLIENTE") %>% 
  mutate(FecProceso = as.Date(FecProceso)) %>% 
  filter(year(FecProceso) == 2026) %>% 
  group_by(Asesor) %>% 
  summarise(UnidadesComerciales = n_distinct(LinNegCod, CliNitPpal),
            Sacos = sum(SSPpto),
            MNFCCPpto = sum(MNFCCPpto), 
            .groups = "drop") %>% 
  arrange(Asesor) %>% 
  gt::gt() %>% 
  gt::fmt_number(columns = Sacos, decimals = 0, use_seps = TRUE) %>% 
  gt::fmt_currency(columns = MNFCCPpto, currency = "COP",decimals = 0)

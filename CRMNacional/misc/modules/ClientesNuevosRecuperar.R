ClientesNuevosRecuperadosUI <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Clientes Nuevos"),
    DTOutput(ns("ClientesNuevos")),
    br(),
    FormatearTexto("Clientes que no existían en la base de datos", tamano_pct = 0.7),
    br(),
    h5("Clientes Recuperados"),
    DTOutput(ns("ClientesRecuperados")),
    br(),
    FormatearTexto("Clientes marcados como 'Cliente a Recuperar' con facturación", tamano_pct = 0.7)
  )
}
ClientesNuevosRecuperados <- function(id, dat, data_f) {
  moduleServer(id, function(input, output, session) {
    
    # Nuevos Clientes
    output$ClientesNuevos <- renderDT({
      
      aux1 <- CargarDatos("CRMNALSEGR") %>%
        select(LinNegCod, CliNitPpal, SegmentoRacafe, FecProceso) %>%
        mutate(FecProceso = as.Date(FecProceso)) %>%
        filter(FecProceso >= PrimerDia(Sys.Date()) - months(1)) %>%
        group_by(LinNegCod, CliNitPpal) %>%
        tidyr::pivot_wider(names_from = FecProceso, values_from = SegmentoRacafe) %>%
        setNames(c("LinNegCod", "CliNitPpal", "Antes", "Ahora")) %>%
        filter(is.na(Antes), Ahora == "CLIENTE") %>%
        select(LinNegCod, CliNitPpal)
      
      aux2 <- dat %>%
        inner_join(aux1, by = join_by(LinNegCod, CliNitPpal)) %>%
        bind_rows(
          data_f() %>%
            anti_join(CargarDatos("CRMNALSEGR") %>%
                        select(LinNegCod, CliNitPpal),
                      by = join_by(LinNegCod, CliNitPpal))
        ) %>%
        group_by(PerRazSoc, LineaNegocio = CLLinNegNo, Segmento) %>%
        summarise(
          UltDespacho = max(FecFact, na.rm = TRUE),
          SacosMes = sum(if_else(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Kilos/70, 0), na.rm = TRUE),
          SacosAnho = sum(if_else(year(FecFact) == year(Sys.Date()), Kilos/70, 0), na.rm = TRUE),
          MargenMes = sum(if_else(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Margen/70, 0), na.rm = TRUE),
          MargenAnho = sum(if_else(year(FecFact) == year(Sys.Date()), Margen, 0), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        janitor::adorn_totals("row", name = "TOTAL")
      
      ImprimirDTRAzSocLinNeg(
        aux2,
        noms = c("Razón Social", "Línea de Negocio", "Segmento Racafé", "Última Facturación",
                 "Sacos Mes", "Sacos Año", "Márgen Mes", "Márgen Año"),
        formatos = c(NA, NA, NA, NA, "sacos", "sacos", "dinero", "dinero"),
        dom = "Bft", buscar = TRUE, alto = 500
      )
    })
    
    # Clientes Recuperados
    output$ClientesRecuperados <- renderDT({
      aux1 <- CargarDatos("CRMNALSEGR") %>%
        select(LinNegCod, CliNitPpal, SegmentoRacafe, FecProceso) %>%
        mutate(FecProceso = as.Date(FecProceso)) %>%
        filter(FecProceso >= PrimerDia(Sys.Date()) - months(1)) %>%
        group_by(LinNegCod, CliNitPpal) %>%
        tidyr::pivot_wider(names_from = FecProceso, values_from = SegmentoRacafe) %>%
        setNames(c("LinNegCod", "CliNitPpal", "Antes", "Ahora")) %>%
        filter(Antes %in% c("CLIENTE A RECUPERAR", NA) & Ahora == "CLIENTE") %>%
        select(LinNegCod, CliNitPpal)
      
      aux2 <- data_f() %>%
        bind_rows(
          data_f() %>%
            filter(PrimerDia(FecFact) == PrimerDia(Sys.Date()),
                   SegmentoRacafe == "CLIENTE A RECUPERAR")
        ) %>%
        inner_join(aux1, by = join_by(LinNegCod, CliNitPpal)) %>%
        group_by(PerRazSoc, LineaNegocio = CLLinNegNo, Segmento) %>%
        summarise(
          UltDespacho = max(FecFact, na.rm = TRUE),
          SacosMes = sum(if_else(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Kilos/70, 0), na.rm = TRUE),
          SacosAnho = sum(if_else(year(FecFact) == year(Sys.Date()), Kilos/70, 0), na.rm = TRUE),
          MargenMes = sum(if_else(PrimerDia(FecFact) == PrimerDia(Sys.Date()), Margen/70, 0), na.rm = TRUE),
          MargenAnho = sum(if_else(year(FecFact) == year(Sys.Date()), Margen, 0), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        janitor::adorn_totals("row", name = "TOTAL")
      
      ImprimirDTRAzSocLinNeg(
        aux2,
        noms = c("Razón Social", "Línea de Negocio", "Segmento Racafé", "Última Facturación",
                 "Sacos Mes", "Sacos Año", "Márgen Mes", "Márgen Año"),
        formatos = c(NA, NA, NA, NA, "sacos", "sacos", "dinero", "dinero"),
        dom = "Bft", buscar = TRUE, alto = 500
      )
    })
    
  })
}
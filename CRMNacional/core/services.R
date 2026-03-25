# Servicios de dominio / reglas de negocio

compute_pendientes_lote <- function(dat) {
  dat %>%
    mutate(
      PendProducir  = SacLote - coalesce(CLLotSacPr, 0),
      PendDespachar = pmax(SacLote - pmax(coalesce(CLLotSacDe, 0), 0), 0),
      PendFacturar  = SacLote - pmax(PendDespachar, 0) - coalesce(CLLotSacFa, 0)
    ) %>%
    select(-c(CLLotSacPr, CLLotSacDe, CLLotSacFa))
}

apply_cliente_business_rules <- function(dat) {
  dat %>%
    mutate(
      SegmentoAsignadoSistema = is.na(Segmento),
      Excluir     = ifelse(is.na(Excluir)     | Excluir     == "", "NO", Excluir),
      ProdExcluir = ifelse(is.na(ProdExcluir) | ProdExcluir == "", "NO", ProdExcluir),
      Segmento = case_when(
        !is.na(Segmento)                    ~ Segmento,
        LinNegCod == 10000 & SacLote <= 240 ~ "DETAL",
        LinNegCod == 10000 & SacLote >  240 ~ "MEDIANO",
        LinNegCod == 21000 & SacLote <   50 ~ "DETAL",
        LinNegCod == 21000 & SacLote >=  50 ~ "MEDIANO"
      ),
      Asesor = case_when(
        !is.na(Asesor)                      ~ Asesor,
        LinNegCod == 10000 & SacLote <  240 ~ "GACORREDOR",
        LinNegCod == 10000 & SacLote >= 240 ~ "CMEDINA",
        LinNegCod == 21000 & SacLote <   50 ~ "LABOYACA",
        LinNegCod == 21000 & SacLote >=  50 ~ "JGCANON"
      ),
      Responsable = case_when(
        !is.na(Responsable)                    ~ Responsable,
        LinNegCod == 10000 & SacLote <  240    ~ "GACORREDOR",
        LinNegCod == 10000 & SacLote >= 240    ~ "CMEDINA",
        LinNegCod == 21000 & SacLote <   50    ~ "LABOYACA",
        LinNegCod == 21000 & SacLote >=  50    ~ "JGCANON"
      ),
      SegmentoRacafe = ifelse(is.na(SegmentoRacafe) & !is.na(FecFact), "CLIENTE", SegmentoRacafe),
      Asesor      = ifelse(is.na(Asesor)      | Asesor      == "", "SIN DATO", Asesor),
      Responsable = ifelse(is.na(Responsable) | Responsable == "", "SIN DATO", Responsable),
      Segmento    = ifelse(is.na(Segmento)    | Segmento    == "", "SIN DATO", Segmento),
      CLLinNegNo  = ifelse(is.na(CLLinNegNo)  | CLLinNegNo  == "", "SIN DATO", CLLinNegNo),
      Categoria   = ifelse(is.na(Categoria)   | Categoria   == "", "SIN DATO", Categoria),
      Producto    = ifelse(is.na(Producto)    | Producto    == "", "SIN DATO", Producto)
    )
}

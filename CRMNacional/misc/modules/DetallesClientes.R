# Helpers Detalle Clientes ----
# dat()  = dat_c() filtrado por semi_join con NITs del segmento (desde ResumenTotal)
# nits() = tibble derivado de CRMNALSEGR (segr_corte) con columnas:
#          LinNegCod, CliNitPpal, Segmento, Asesor, Presupuestado, UltFecFact, FecProceso
# FUENTE UNICA: conteos de cajas y totales del detalle parten de los mismos nits().

# Helpers de formato ----
.fmt_sacos <- function(v) {
  if (is.na(v) || is.infinite(v)) return("\u2014")
  format(round(v, 1), big.mark = ".", decimal.mark = ",", scientific = FALSE, nsmall = 1)
}
.fmt_margen <- function(v) {
  if (is.na(v) || is.infinite(v)) return("\u2014")
  paste0("$", format(round(v), big.mark = ".", decimal.mark = ",", scientific = FALSE))
}
.fmt_pct <- function(v) {
  if (is.na(v) || is.infinite(v)) return("\u2014")
  paste0(round(v * 100, 1), "%")
}
.style_cumpl <- function(v) {
  if (is.na(v) || is.infinite(v)) return(NULL)
  list(background = dplyr::case_when(v >= 1.00 ~ "#D5F5E3",
                                     v >= 0.80 ~ "#FCF3CF", TRUE ~ "#FADBD8"),
       fontWeight = "600")
}

# Boton oportunidad identico al de dc_coldefs_comunes original
.btn_oportunidad <- function(v) {
  if (is.na(v) || v == "") return("")
  as.character(tags$span(
    style = paste0(
      "display:inline-flex; align-items:center; justify-content:center;",
      "width:28px; height:28px; border-radius:6px;",
      "background:#C11007; color:white; font-size:13px; cursor:pointer;"
    ),
    icon("hand-holding-dollar")
  ))
}

# Ejecucion mensual y YTD desde dat_c() filtrado
dc_ejecucion <- function(dat_df) {
  corte <- PrimerDia(Sys.Date())
  anio  <- year(Sys.Date())
  dat_df %>%
    group_by(LinNegCod, CliNitPpal) %>%
    summarise(
      SacosMes  = sum(ifelse(PrimerDia(FecFact) == corte, SacFact70, 0), na.rm = TRUE),
      MargenMes = sum(ifelse(PrimerDia(FecFact) == corte, Margen,    0), na.rm = TRUE),
      SacosYTD  = sum(ifelse(year(FecFact) == anio,       SacFact70, 0), na.rm = TRUE),
      MargenYTD = sum(ifelse(year(FecFact) == anio,       Margen,    0), na.rm = TRUE),
      .groups   = "drop"
    )
}

# Presupuesto mensualizado — PptoSacos y PptoMargen vienen de ctx (dat_t sin filtro fechas).
# dat_t() contiene el join con CRMNALCLIENTE que aporta las columnas de presupuesto.
dc_ppto <- function(ctx_df) {
  mes <- month(Sys.Date())
  ctx_df %>%
    select(LinNegCod, CliNitPpal, PptoSacos, PptoMargen) %>%
    distinct() %>%
    mutate(
      PptoSacosMes  = PptoSacos  / 12,
      PptoMargenMes = PptoMargen / 12,
      PptoSacosYTD  = PptoSacos  / 12 * mes,
      PptoMargenYTD = PptoMargen / 12 * mes
    )
}

# Mini-tabla resumen HTML con N y porcentaje.
# CORRECCION: ancho de barra proporcional al maximo de la columna (no acumulado).
dc_tabla_resumen <- function(df, columna, titulo, filtro_activo = NULL,
                             niveles = NULL, ns_id) {
  if (nrow(df) == 0)
    return(div(style = "padding:12px;color:#6B7280;font-size:12px;font-style:italic;",
               "Sin datos disponibles."))
  
  datos <- df %>%
    count(Cat = !!sym(columna), name = "N") %>%
    mutate(Pct = round(100 * N / sum(N), 1))
  
  if (!is.null(niveles)) {
    datos <- datos %>%
      mutate(Cat = factor(Cat, levels = niveles, ordered = TRUE)) %>%
      arrange(Cat) %>% mutate(Cat = as.character(Cat))
  } else {
    datos <- datos %>% arrange(desc(N))
  }
  filas <- purrr::map(seq_len(nrow(datos)), function(i) {
    cv  <- datos$Cat[i]; nv  <- datos$N[i]; pv  <- datos$Pct[i]
    act <- !is.null(filtro_activo) && identical(cv, filtro_activo)
    bg  <- if (act) "#FEF2F2" else if (i %% 2 == 0) "#F9FAFB" else "white"
    co  <- if (act) "#C11007" else "#374151"
    fw  <- if (act) "700" else "400"
    tags$tr(
      style   = paste0("background:", bg, "; cursor:pointer; border-bottom:1px solid #F3F4F6;"),
      onclick = paste0("Shiny.setInputValue('", ns_id, "','",
                       gsub("'", "\\\\'", cv), "',{priority:'event'});"),
      tags$td(style = paste0("padding:5px 10px;font-size:12px;color:", co,
                             ";font-weight:", fw, ";white-space:nowrap;",
                             "max-width:180px;overflow:hidden;text-overflow:ellipsis;"),
              if (act) tags$span("\u2022 ", style = "color:#C11007;"), cv),
      tags$td(style = paste0("padding:5px 8px;font-size:12px;color:", co,
                             ";font-weight:", fw, ";text-align:right;",
                             "white-space:nowrap;min-width:40px;"),
              format(nv, big.mark = ".", decimal.mark = ",", scientific = FALSE)),
      tags$td(style = paste0("padding:5px 8px;font-size:12px;color:", co,
                             ";font-weight:", fw, ";text-align:right;white-space:nowrap;"),
              paste0(pv, "%"))
    )
  })
  
  tags$div(
    style = "margin-bottom:4px;",
    tags$div(style = paste0(
      "font-size:11px;font-weight:700;color:#374151;text-transform:uppercase;",
      "letter-spacing:0.05em;margin-bottom:6px;padding-bottom:4px;",
      "border-bottom:2px solid #E5E7EB;"), titulo),
    if (!is.null(filtro_activo))
      tags$div(style = "font-size:10px;color:#C11007;margin-bottom:4px;",
               paste0("Activo: ", filtro_activo, " \u2014 clic para limpiar")),
    tags$div(style = "overflow-x:auto;",
             tags$table(
               style = "width:100%;border-collapse:collapse;font-family:inherit;",
               tags$thead(tags$tr(style = "background:#F8FAFC;",
                                  tags$th(style = "padding:4px 10px;font-size:11px;color:#6B7280;text-align:left;
                           font-weight:600;white-space:nowrap;", "Categoria"),
                                  tags$th(style = "padding:4px 8px;font-size:11px;color:#6B7280;text-align:right;
                           font-weight:600;white-space:nowrap;", "N"),
                                  tags$th(style = "padding:4px 8px;font-size:11px;color:#6B7280;text-align:right;
                           font-weight:600;white-space:nowrap;", "%")
               )),
               tags$tbody(!!!filas)
             ))
  )
}

# UI base: tres resumenes + dos tablas (sacos / margen) + descarga. Sin boton limpiar.
dc_ui_base <- function(ns, titulo_tabla) {
  tagList(
    fluidRow(column(4, uiOutput(ns("resumen_1"))),
             column(4, uiOutput(ns("resumen_2"))),
             column(4, uiOutput(ns("resumen_3")))),
    fluidRow(column(12, div(style = "margin-top:20px;",
                            racafeModulos::TablaReactableUI(
                              ns("TablaSacos"),
                              titulo      = paste0(titulo_tabla, " \u2014 Sacos"),
                              footer      = "Clic en el boton para registrar una oportunidad.",
                              footer_tipo = "info", mostrar_nota = FALSE)))),
    fluidRow(column(12, div(style = "margin-top:20px;",
                            racafeModulos::TablaReactableUI(
                              ns("TablaMargen"),
                              titulo      = paste0(titulo_tabla, " \u2014 Margen"),
                              footer      = "Clic en el boton para registrar una oportunidad.",
                              footer_tipo = "info", mostrar_nota = FALSE)))),
    fluidRow(html(paste0(
      '<div style="text-align:right;width:100%;margin-top:8px;">',
      BotonDescarga("Descargar", size = "md", ns = ns), '</div>'
    )))
  )
}

# Descarga Excel
dc_descarga <- function(data_fn, prefijo) {
  list(
    filename = function() paste0(prefijo, "_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      df <- data_fn()
      if (nrow(df) == 0) { showNotification("Sin datos", type = "warning"); return() }
      tryCatch({
        if (requireNamespace("openxlsx", quietly = TRUE)) openxlsx::write.xlsx(df, file)
        else write.csv(df, file, row.names = FALSE)
        showNotification("Descargado", type = "message")
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    }
  )
}

# ColDefs de identificacion comunes (anchos generosos, sin corte de palabras) ----
.col_oportunidad <- reactable::colDef(
  name = "", minWidth = 52, html = TRUE, sortable = FALSE, cell = .btn_oportunidad
)
.col_razsoc  <- reactable::colDef(name = "Cliente",         minWidth = 260,
                                  html = TRUE, sticky = "left")
.col_linneg  <- reactable::colDef(name = "Linea",           minWidth = 170)
.col_asesor  <- reactable::colDef(name = "Asesor",          minWidth = 160)
.col_ppto    <- reactable::colDef(name = "Presupuestado",   minWidth = 150)
.col_ultfact <- reactable::colDef(
  name = "Ult. Factura", minWidth = 130,
  cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d/%m/%Y")
)
.col_segmento_std <- reactable::colDef(name = "Segmento",   minWidth = 140)

# ColDefs de sacos ----
.cols_sacos <- function() list(
  Oportunidad  = .col_oportunidad,
  PerRazSoc    = .col_razsoc,
  CLLinNegNo   = .col_linneg,
  Segmento     = .col_segmento_std,
  Asesor       = .col_asesor,
  Presupuestado = .col_ppto,
  UltFecFact   = .col_ultfact,
  PptoSacosMes = reactable::colDef(name = "Ppto Sacos Mes",        minWidth = 170,
                                   cell = function(v) .fmt_sacos(v)),
  SacosMes     = reactable::colDef(name = "Sacos Mes",             minWidth = 140,
                                   cell = function(v) .fmt_sacos(v)),
  CumpSacosMes = reactable::colDef(name = "% Cumpl Sacos Mes",     minWidth = 180,
                                   cell  = function(v) .fmt_pct(v),
                                   style = function(v) .style_cumpl(v)),
  PptoSacosYTD = reactable::colDef(name = "Ppto Sacos Ano Corrido",minWidth = 200,
                                   cell = function(v) .fmt_sacos(v)),
  SacosYTD     = reactable::colDef(name = "Sacos Ano Corrido",     minWidth = 170,
                                   cell = function(v) .fmt_sacos(v)),
  CumpSacosYTD = reactable::colDef(name = "% Cumpl Sacos Acum.",   minWidth = 190,
                                   cell  = function(v) .fmt_pct(v),
                                   style = function(v) .style_cumpl(v))
)

# ColDefs de margen ----
.cols_margen <- function() list(
  Oportunidad   = .col_oportunidad,
  PerRazSoc     = .col_razsoc,
  CLLinNegNo    = .col_linneg,
  Segmento      = .col_segmento_std,
  Asesor        = .col_asesor,
  Presupuestado = .col_ppto,
  UltFecFact    = .col_ultfact,
  PptoMargenMes = reactable::colDef(name = "Ppto Margen Mes",        minWidth = 180,
                                    cell = function(v) .fmt_margen(v)),
  MargenMes     = reactable::colDef(name = "Margen Mes",             minWidth = 160,
                                    cell = function(v) .fmt_margen(v)),
  CumpMargenMes = reactable::colDef(name = "% Cumpl Margen Mes",     minWidth = 190,
                                    cell  = function(v) .fmt_pct(v),
                                    style = function(v) .style_cumpl(v)),
  PptoMargenYTD = reactable::colDef(name = "Ppto Margen Ano Corrido",minWidth = 210,
                                    cell = function(v) .fmt_margen(v)),
  MargenYTD     = reactable::colDef(name = "Margen Ano Corrido",     minWidth = 180,
                                    cell = function(v) .fmt_margen(v)),
  CumpMargenYTD = reactable::colDef(name = "% Cumpl Margen Acum.",   minWidth = 200,
                                    cell  = function(v) .fmt_pct(v),
                                    style = function(v) .style_cumpl(v))
)

# Helper: registra FormularioOportunidad y las dos TablaReactable con boton oportunidad
dc_registrar_tablas <- function(ns, data_sacos_r, data_margen_r,
                                col_ss, col_mg, usr, dat, trigger_update,
                                tipo_cliente = "CLIENTE") {
  FormularioOportunidad(
    "mod_formulario", dat = dat, usr = usr, trigger_update = trigger_update,
    tipo_cliente_default = reactive(tipo_cliente)
  )
  racafeModulos::TablaReactable(
    id = "TablaSacos", data = data_sacos_r,
    columnas = names(col_ss), col_specs = col_ss,
    modo_seleccion = "celda", id_col = NULL,
    cols_activos = "Oportunidad", col_header_n = 2L,
    sortable = TRUE, searchable = TRUE, page_size = 15L,
    compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE,
    filas_bloqueadas = "TOTAL",
    modal_icon = "hand-holding-dollar", modal_size = "xl",
    modal_titulo_fn = function(sel) {
      razsoc_raw <- as.character(sel$fila$PerRazSoc[[1]])
      # Extraer texto plano si PerRazSoc contiene HTML de enlace
      razsoc_txt <- gsub("<[^>]+>", "", razsoc_raw)
      paste0("Nueva Oportunidad \u2014 ", razsoc_txt)
    },
    modal_contenido_fn = function(sel) FormularioOportunidadUI(ns("mod_formulario"))
  )
  racafeModulos::TablaReactable(
    id = "TablaMargen", data = data_margen_r,
    columnas = names(col_mg), col_specs = col_mg,
    modo_seleccion = "celda", id_col = NULL,
    cols_activos = "Oportunidad", col_header_n = 2L,
    sortable = TRUE, searchable = TRUE, page_size = 15L,
    compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE,
    filas_bloqueadas = "TOTAL",
    modal_icon = "hand-holding-dollar", modal_size = "xl",
    modal_titulo_fn = function(sel) {
      razsoc_raw <- as.character(sel$fila$PerRazSoc[[1]])
      # Extraer texto plano si PerRazSoc contiene HTML de enlace
      razsoc_txt <- gsub("<[^>]+>", "", razsoc_raw)
      paste0("Nueva Oportunidad \u2014 ", razsoc_txt)
    },
    modal_contenido_fn = function(sel) FormularioOportunidadUI(ns("mod_formulario"))
  )
}

# Helper: fila TOTAL para tabla sacos
.tot_s <- function(df) {
  tibble::tibble(
    Oportunidad = "", PerRazSoc = "TOTAL", CLLinNegNo = "",
    Segmento = "", Asesor = "", Presupuestado = "", UltFecFact = as.Date(NA),
    PptoSacosMes = sum(df$PptoSacosMes, na.rm = TRUE),
    SacosMes     = sum(df$SacosMes,     na.rm = TRUE),
    CumpSacosMes = SiError_0(sum(df$SacosMes, na.rm=TRUE) / sum(df$PptoSacosMes, na.rm=TRUE)),
    PptoSacosYTD = sum(df$PptoSacosYTD, na.rm = TRUE),
    SacosYTD     = sum(df$SacosYTD,     na.rm = TRUE),
    CumpSacosYTD = SiError_0(sum(df$SacosYTD, na.rm=TRUE) / sum(df$PptoSacosYTD, na.rm=TRUE))
  )
}

# Helper: fila TOTAL para tabla margen
.tot_m <- function(df) {
  tibble::tibble(
    Oportunidad = "", PerRazSoc = "TOTAL", CLLinNegNo = "",
    Segmento = "", Asesor = "", Presupuestado = "", UltFecFact = as.Date(NA),
    PptoMargenMes = sum(df$PptoMargenMes, na.rm = TRUE),
    MargenMes     = sum(df$MargenMes,     na.rm = TRUE),
    CumpMargenMes = SiError_0(sum(df$MargenMes,na.rm=TRUE) / sum(df$PptoMargenMes,na.rm=TRUE)),
    PptoMargenYTD = sum(df$PptoMargenYTD, na.rm = TRUE),
    MargenYTD     = sum(df$MargenYTD,     na.rm = TRUE),
    CumpMargenYTD = SiError_0(sum(df$MargenYTD,na.rm=TRUE) / sum(df$PptoMargenYTD,na.rm=TRUE))
  )
}

# Helper: observer de filtro con toggle
.obs_filtro <- function(input_id, filtros_rv, campo) {
  observeEvent(input_id, {
    v <- input_id()
    filtros_rv[[campo]] <- if (!is.null(filtros_rv[[campo]]) &&
                               filtros_rv[[campo]] == v) NULL else v
  }, ignoreNULL = TRUE)
}

# Clientes Activos ----
# Objetivo: RETENCION. Fuente: nits con SegmentoRacafe == "CLIENTE" en CRMNALSEGR.
DetalleClienteUI <- function(id) { ns <- NS(id); dc_ui_base(ns, "Clientes Activos") }

DetalleCliente <- function(id, dat, nits, ctx, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local({
      ids <- c(ns("cr1"), ns("cr2"), ns("cr3"))
      session$userData$.plotlyShinyEventIDs <- unique(
        c(session$userData$.plotlyShinyEventIDs, ids))
    })
    filtros <- reactiveValues(r1 = NULL, r2 = NULL, r3 = NULL)
    observeEvent(input$cr1,{v<-input$cr1;filtros$r1<-if(!is.null(filtros$r1)&&filtros$r1==v)NULL else v},ignoreNULL=TRUE)
    observeEvent(input$cr2,{v<-input$cr2;filtros$r2<-if(!is.null(filtros$r2)&&filtros$r2==v)NULL else v},ignoreNULL=TRUE)
    observeEvent(input$cr3,{v<-input$cr3;filtros$r3<-if(!is.null(filtros$r3)&&filtros$r3==v)NULL else v},ignoreNULL=TRUE)
    
    data_base <- reactive({
      waiter_show(html = preloader_calculando$html, color = preloader_calculando$color)
      on.exit(waiter_hide())
      req(nits(), dat())
      validate(need(nrow(nits()) > 0, "Sin Clientes Activos para los filtros seleccionados."))
      {
        message("[DC_ACTIVOS] nits=", nrow(nits()),
                " dat=", nrow(dat()),
                " ctx=", nrow(ctx()),
                " cols_ctx=", paste(names(ctx()), collapse=","))
        mes <- month(Sys.Date())
        eje <- dc_ejecucion(dat())
        ppo <- dc_ppto(ctx())
        # ctx() viene de dat_t() (sin filtro de fechas) via ResumenTotal.
        # Garantiza que todos los clientes del segmento tengan PerRazSoc, Segmento,
        # Asesor y Presupuestado aunque no hayan facturado en el rango seleccionado.
        # fec: UltFecFact calculada desde dat() (dat_c filtrado) — puede ser NA
        # para clientes que no facturaron en el periodo; es intencional.
        fec <- dat() %>%
          filter(!is.na(FecFact)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(UltFecFact = max(FecFact, na.rm = TRUE), .groups = "drop")
        nits() %>%
          left_join(ctx(), by = c("LinNegCod","CliNitPpal")) %>%
          left_join(fec, by = c("LinNegCod","CliNitPpal")) %>%
          left_join(eje, by = c("LinNegCod","CliNitPpal")) %>%
          left_join(ppo, by = c("LinNegCod","CliNitPpal")) %>%
          mutate(
            across(c(SacosMes,MargenMes,SacosYTD,MargenYTD,
                     PptoSacosMes,PptoMargenMes,PptoSacosYTD,PptoMargenYTD),
                   ~ coalesce(.x, 0)),
            CumpSacosMes  = SiError_0(SacosMes  / PptoSacosMes),
            CumpMargenMes = SiError_0(MargenMes / PptoMargenMes),
            CumpSacosYTD  = SiError_0(SacosYTD  / PptoSacosYTD),
            CumpMargenYTD = SiError_0(MargenYTD / PptoMargenYTD),
            FactMesVigente = ifelse(SacosMes > 0, "CON COMPRA", "SIN COMPRA"),
            MesesSinCompra = {
              m <- ifelse(is.na(UltFecFact), NA_real_,
                          pmax(0, as.numeric(difftime(Sys.Date(), UltFecFact, units="days")) %/% 30))
              dplyr::case_when(is.na(m)~"Sin historial",m==0~"0 meses",
                               m<=2~"1-2 meses",m<=4~"3-4 meses",TRUE~"5+ meses")
            },
            CLLinNegNo = ifelse(LinNegCod == 10000L, "CONVENCIONALES", "A LA MEDIDA"),
            Oportunidad = "Crear"
          )
      }
    })
    
    data_filtrada <- reactive({
      df <- data_base(); if(nrow(df)==0) return(df)
      if(!is.null(filtros$r1)) df <- df %>% filter(FactMesVigente == filtros$r1)
      if(!is.null(filtros$r2)) df <- df %>% filter(Presupuestado  == filtros$r2)
      if(!is.null(filtros$r3)) df <- df %>% filter(MesesSinCompra == filtros$r3)
      df
    })
    
    output$resumen_1 <- renderUI(dc_tabla_resumen(data_base(),"FactMesVigente","Compra en el Mes",filtros$r1,ns_id=ns("cr1")))
    output$resumen_2 <- renderUI(dc_tabla_resumen(data_base(),"Presupuestado","Presupuestado",filtros$r2,ns_id=ns("cr2")))
    output$resumen_3 <- renderUI({
      lvl <- c("Sin historial","0 meses","1-2 meses","3-4 meses","5+ meses")
      dc_tabla_resumen(data_base(),"MesesSinCompra","Riesgo de Perdida",filtros$r3,niveles=lvl,ns_id=ns("cr3"))
    })
    
    .col_fmv <- reactable::colDef(name="Compra Mes",minWidth=150,
                                  style=function(v) list(background=switch(v,"CON COMPRA"="#EDFBF2","SIN COMPRA"="#FEF2F2","white"),
                                                         color=switch(v,"CON COMPRA"="#1E8449","SIN COMPRA"="#943126","#333"),fontWeight="600"))
    .col_msc <- reactable::colDef(name="Meses sin Compra",minWidth=170,
                                  style=function(v) list(
                                    background=switch(v,"Sin historial"="#F3F4F6","0 meses"="#EDFBF2","1-2 meses"="#FFFBEB","3-4 meses"="#FFF7ED","5+ meses"="#FEF2F2","white"),
                                    color=switch(v,"Sin historial"="#6B7280","0 meses"="#1E8449","1-2 meses"="#92400E","3-4 meses"="#9A3412","5+ meses"="#7F1D1D","#333"),
                                    fontWeight="600"))
    
    col_ss <- modifyList(.cols_sacos(), list(FactMesVigente=.col_fmv, MesesSinCompra=.col_msc))
    col_mg <- modifyList(.cols_margen(), list(FactMesVigente=.col_fmv, MesesSinCompra=.col_msc))
    
    .sel_s <- function(df) df %>%
      crear_link_cliente(col_razsoc="PerRazSoc",col_linneg="CLLinNegNo") %>%
      select(Oportunidad,PerRazSoc,CLLinNegNo,FactMesVigente,MesesSinCompra,
             Segmento,Asesor,Presupuestado,UltFecFact,
             PptoSacosMes,SacosMes,CumpSacosMes,PptoSacosYTD,SacosYTD,CumpSacosYTD)
    .sel_m <- function(df) df %>%
      crear_link_cliente(col_razsoc="PerRazSoc",col_linneg="CLLinNegNo") %>%
      select(Oportunidad,PerRazSoc,CLLinNegNo,FactMesVigente,MesesSinCompra,
             Segmento,Asesor,Presupuestado,UltFecFact,
             PptoMargenMes,MargenMes,CumpMargenMes,PptoMargenYTD,MargenYTD,CumpMargenYTD)
    
    .tot_s_act <- function(df) .tot_s(df) %>%
      mutate(FactMesVigente="",MesesSinCompra="")
    .tot_m_act <- function(df) .tot_m(df) %>%
      mutate(FactMesVigente="",MesesSinCompra="")
    
    data_sacos_r  <- reactive({ df<-data_filtrada();req(nrow(df)>0); bind_rows(.sel_s(df),.tot_s_act(df)) })
    data_margen_r <- reactive({ df<-data_filtrada();req(nrow(df)>0); bind_rows(.sel_m(df),.tot_m_act(df)) })
    
    dc_registrar_tablas(ns, data_sacos_r, data_margen_r, col_ss, col_mg,
                        usr, dat, trigger_update, tipo_cliente = "CLIENTE")
    output$Descargar <- do.call(downloadHandler, dc_descarga(data_filtrada,"activos"))
  })
}

# Clientes a Recuperar ----
# Objetivo: RECUPERACION. Fuente: nits con SegmentoRacafe == "CLIENTE A RECUPERAR".
DetalleClienteRecuperarUI <- function(id) { ns<-NS(id); dc_ui_base(ns,"Clientes a Recuperar") }

DetalleClienteRecuperar <- function(id, dat, nits, ctx, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local({
      ids <- c(ns("cr1"),ns("cr2"),ns("cr3"))
      session$userData$.plotlyShinyEventIDs <- unique(c(session$userData$.plotlyShinyEventIDs,ids))
    })
    filtros <- reactiveValues(r1=NULL,r2=NULL,r3=NULL)
    observeEvent(input$cr1,{v<-input$cr1;filtros$r1<-if(!is.null(filtros$r1)&&filtros$r1==v)NULL else v},ignoreNULL=TRUE)
    observeEvent(input$cr2,{v<-input$cr2;filtros$r2<-if(!is.null(filtros$r2)&&filtros$r2==v)NULL else v},ignoreNULL=TRUE)
    observeEvent(input$cr3,{v<-input$cr3;filtros$r3<-if(!is.null(filtros$r3)&&filtros$r3==v)NULL else v},ignoreNULL=TRUE)
    
    data_base <- reactive({
      waiter_show(html=preloader_calculando$html,color=preloader_calculando$color)
      on.exit(waiter_hide())
      req(nits(),dat())
      validate(need(nrow(nits())>0,"Sin Clientes a Recuperar para los filtros seleccionados."))
      {
        eje <- dc_ejecucion(dat()); ppo <- dc_ppto(ctx())
        # ctx() desde dat_t() via ResumenTotal — sin filtro de fechas
        # fec: UltFecFact desde dat() (con filtro de fechas), puede ser NA para inactivos
        fec <- dat() %>%
          filter(!is.na(FecFact)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(UltFecFact = max(FecFact, na.rm = TRUE), .groups = "drop")
        nits() %>%
          left_join(ctx(), by = c("LinNegCod","CliNitPpal")) %>%
          left_join(fec,by=c("LinNegCod","CliNitPpal")) %>%
          left_join(eje,by=c("LinNegCod","CliNitPpal")) %>%
          left_join(ppo,by=c("LinNegCod","CliNitPpal")) %>%
          mutate(
            across(c(SacosMes,MargenMes,SacosYTD,MargenYTD,
                     PptoSacosMes,PptoMargenMes,PptoSacosYTD,PptoMargenYTD),~coalesce(.x,0)),
            CumpSacosMes  = SiError_0(SacosMes  / PptoSacosMes),
            CumpMargenMes = SiError_0(MargenMes / PptoMargenMes),
            CumpSacosYTD  = SiError_0(SacosYTD  / PptoSacosYTD),
            CumpMargenYTD = SiError_0(MargenYTD / PptoMargenYTD),
            MesesSinFacturar = {
              m <- ifelse(is.na(UltFecFact), NA_real_,
                          pmax(0, as.numeric(difftime(Sys.Date(), UltFecFact, units="days")) %/% 30))
              dplyr::case_when(is.na(m)~"Sin historial",m<=3~"Hasta 3 meses",
                               m<=6~"De 4 a 6 meses",TRUE~"Mas de 6 meses")
            },
            ValorEnRiesgo = dplyr::case_when(
              is.na(MargenYTD)|MargenYTD==0~"Sin ejecucion",
              MargenYTD>5e6~"Alto (>5M)",MargenYTD>1e6~"Medio (1-5M)",TRUE~"Bajo (<1M)"),
            CLLinNegNo = ifelse(LinNegCod==10000L,"CONVENCIONALES","A LA MEDIDA"),
            Oportunidad = "Crear"
          )
      }
    })
    
    data_filtrada <- reactive({
      df<-data_base();if(nrow(df)==0) return(df)
      if(!is.null(filtros$r1)) df<-df %>% filter(Segmento==filtros$r1)
      if(!is.null(filtros$r2)) df<-df %>% filter(Presupuestado==filtros$r2)
      if(!is.null(filtros$r3)) df<-df %>% filter(MesesSinFacturar==filtros$r3)
      df
    })
    
    output$resumen_1 <- renderUI(dc_tabla_resumen(data_base(),"Segmento","Segmento",filtros$r1,ns_id=ns("cr1")))
    output$resumen_2 <- renderUI(dc_tabla_resumen(data_base(),"Presupuestado","Presupuestado",filtros$r2,ns_id=ns("cr2")))
    output$resumen_3 <- renderUI({
      lvl <- c("Sin historial","Hasta 3 meses","De 4 a 6 meses","Mas de 6 meses")
      dc_tabla_resumen(data_base(),"MesesSinFacturar","Urgencia de Recuperacion",filtros$r3,niveles=lvl,ns_id=ns("cr3"))
    })
    
    .col_seg_rec <- reactable::colDef(name="Segmento",minWidth=140,
                                      style=function(v) list(background="#FDEDEC",color="#943126",fontWeight="600"))
    .col_urg <- reactable::colDef(name="Urgencia",minWidth=170,
                                  style=function(v) list(
                                    background=switch(v,"Sin historial"="#F3F4F6","Hasta 3 meses"="#FFFBEB","De 4 a 6 meses"="#FFF7ED","Mas de 6 meses"="#FEF2F2","white"),
                                    color=switch(v,"Sin historial"="#6B7280","Hasta 3 meses"="#92400E","De 4 a 6 meses"="#9A3412","Mas de 6 meses"="#7F1D1D","#333"),
                                    fontWeight="600"))
    .col_riesgo <- reactable::colDef(name="Valor en Riesgo",minWidth=170,
                                     style=function(v) list(
                                       background=switch(v,"Alto (>5M)"="#FADBD8","Medio (1-5M)"="#FDEBD0","Bajo (<1M)"="#FCF3CF","Sin ejecucion"="#F8F9F9","white"),
                                       color=switch(v,"Alto (>5M)"="#922B21","Medio (1-5M)"="#784212","Bajo (<1M)"="#7D6608","Sin ejecucion"="#707B7C","#333"),
                                       fontWeight="600"))
    
    col_ss <- modifyList(.cols_sacos(),  list(Segmento=.col_seg_rec,MesesSinFacturar=.col_urg,ValorEnRiesgo=.col_riesgo))
    col_mg <- modifyList(.cols_margen(), list(Segmento=.col_seg_rec,MesesSinFacturar=.col_urg,ValorEnRiesgo=.col_riesgo))
    
    .sel_s_rec <- function(df) df %>%
      crear_link_cliente(col_razsoc="PerRazSoc",col_linneg="CLLinNegNo") %>%
      select(Oportunidad,PerRazSoc,CLLinNegNo,MesesSinFacturar,ValorEnRiesgo,
             Segmento,Asesor,Presupuestado,UltFecFact,
             PptoSacosMes,SacosMes,CumpSacosMes,PptoSacosYTD,SacosYTD,CumpSacosYTD)
    .sel_m_rec <- function(df) df %>%
      crear_link_cliente(col_razsoc="PerRazSoc",col_linneg="CLLinNegNo") %>%
      select(Oportunidad,PerRazSoc,CLLinNegNo,MesesSinFacturar,ValorEnRiesgo,
             Segmento,Asesor,Presupuestado,UltFecFact,
             PptoMargenMes,MargenMes,CumpMargenMes,PptoMargenYTD,MargenYTD,CumpMargenYTD)
    .tot_s_rec <- function(df) .tot_s(df) %>% mutate(MesesSinFacturar="",ValorEnRiesgo="")
    .tot_m_rec <- function(df) .tot_m(df) %>% mutate(MesesSinFacturar="",ValorEnRiesgo="")
    
    data_sacos_r  <- reactive({ df<-data_filtrada();req(nrow(df)>0); bind_rows(.sel_s_rec(df),.tot_s_rec(df)) })
    data_margen_r <- reactive({ df<-data_filtrada();req(nrow(df)>0); bind_rows(.sel_m_rec(df),.tot_m_rec(df)) })
    
    dc_registrar_tablas(ns,data_sacos_r,data_margen_r,col_ss,col_mg,
                        usr,dat,trigger_update,tipo_cliente="CLIENTE A RECUPERAR")
    output$Descargar <- do.call(downloadHandler,dc_descarga(data_filtrada,"recuperar"))
  })
}

# Clientes Nuevos ----
# Objetivo: CONVERSION. Fuente: CLIENTE en CRMNALSEGR mes actual, ausente en mes anterior.
# Sin presupuesto: columnas Ppto ocultas.
DetalleClienteNuevoUI <- function(id) { ns<-NS(id); dc_ui_base(ns,"Clientes Nuevos del Mes") }

DetalleClienteNuevo <- function(id, dat, nits, ctx, usr, trigger_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local({
      ids <- c(ns("cr1"),ns("cr2"),ns("cr3"))
      session$userData$.plotlyShinyEventIDs <- unique(c(session$userData$.plotlyShinyEventIDs,ids))
    })
    filtros <- reactiveValues(r1=NULL,r2=NULL,r3=NULL)
    observeEvent(input$cr1,{v<-input$cr1;filtros$r1<-if(!is.null(filtros$r1)&&filtros$r1==v)NULL else v},ignoreNULL=TRUE)
    observeEvent(input$cr2,{v<-input$cr2;filtros$r2<-if(!is.null(filtros$r2)&&filtros$r2==v)NULL else v},ignoreNULL=TRUE)
    observeEvent(input$cr3,{v<-input$cr3;filtros$r3<-if(!is.null(filtros$r3)&&filtros$r3==v)NULL else v},ignoreNULL=TRUE)
    
    data_base <- reactive({
      waiter_show(html=preloader_calculando$html,color=preloader_calculando$color)
      on.exit(waiter_hide())
      req(nits(),dat())
      validate(need(nrow(nits())>0,"Sin Clientes Nuevos para los filtros seleccionados."))
      {
        eje <- dc_ejecucion(dat())
        # ctx() desde dat_t() via ResumenTotal — sin filtro de fechas
        # fec: UltFecFact desde dat() (con filtro de fechas), puede ser NA para inactivos
        fec <- dat() %>%
          filter(!is.na(FecFact)) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          summarise(UltFecFact = max(FecFact, na.rm = TRUE), .groups = "drop")
        nits() %>%
          left_join(ctx(), by = c("LinNegCod","CliNitPpal")) %>%
          left_join(eje,by=c("LinNegCod","CliNitPpal")) %>%
          left_join(fec,by=c("LinNegCod","CliNitPpal")) %>%
          mutate(
            across(c(SacosMes,MargenMes,SacosYTD,MargenYTD),~coalesce(.x,0)),
            PptoSacosMes=0,PptoMargenMes=0,PptoSacosYTD=0,PptoMargenYTD=0,
            CumpSacosMes=NA_real_,CumpMargenMes=NA_real_,
            CumpSacosYTD=NA_real_,CumpMargenYTD=NA_real_,
            BucketSacos=dplyr::case_when(
              SacosMes>=100~">=100 sacos",SacosMes>=50~"50-99 sacos",
              SacosMes>=10~"10-49 sacos",SacosMes>0~"<10 sacos",TRUE~"Sin compra"),
            CLLinNegNo=ifelse(LinNegCod==10000L,"CONVENCIONALES","A LA MEDIDA"),
            Oportunidad="Crear"
          )
      }
    })
    
    data_filtrada <- reactive({
      df<-data_base();if(nrow(df)==0) return(df)
      if(!is.null(filtros$r1)) df<-df %>% filter(Segmento==filtros$r1)
      if(!is.null(filtros$r2)) df<-df %>% filter(CLLinNegNo==filtros$r2)
      if(!is.null(filtros$r3)) df<-df %>% filter(BucketSacos==filtros$r3)
      df
    })
    
    output$resumen_1 <- renderUI(dc_tabla_resumen(data_base(),"Segmento","Segmento",filtros$r1,ns_id=ns("cr1")))
    output$resumen_2 <- renderUI(dc_tabla_resumen(data_base(),"CLLinNegNo","Linea de Negocio",filtros$r2,ns_id=ns("cr2")))
    output$resumen_3 <- renderUI({
      lvl <- c("Sin compra","<10 sacos","10-49 sacos","50-99 sacos",">=100 sacos")
      dc_tabla_resumen(data_base(),"BucketSacos","Tamano Primera Compra",filtros$r3,niveles=lvl,ns_id=ns("cr3"))
    })
    
    .col_seg_new <- reactable::colDef(name="Segmento",minWidth=140,
                                      style=function(v) list(background="#EDFBF2",color="#1E8449",fontWeight="600"))
    .col_bucket <- reactable::colDef(name="Primera Compra",minWidth=170,
                                     style=function(v) list(
                                       background=switch(v,">=100 sacos"="#D5F5E3","50-99 sacos"="#EAFAF1",
                                                         "10-49 sacos"="#FCF3CF","<10 sacos"="#FEF9E7","Sin compra"="#F8F9F9","white"),
                                       color=switch(v,">=100 sacos"="#1A5226","50-99 sacos"="#1E8449",
                                                    "10-49 sacos"="#7D6608","<10 sacos"="#9A7D0A","Sin compra"="#707B7C","#333"),
                                       fontWeight="600"))
    
    .col_oculto <- reactable::colDef(show=FALSE)
    col_ss <- modifyList(.cols_sacos(), list(
      Segmento=.col_seg_new, BucketSacos=.col_bucket,
      PptoSacosMes=.col_oculto, CumpSacosMes=.col_oculto,
      PptoSacosYTD=.col_oculto, CumpSacosYTD=.col_oculto
    ))
    col_mg <- modifyList(.cols_margen(), list(
      Segmento=.col_seg_new, BucketSacos=.col_bucket,
      PptoMargenMes=.col_oculto, CumpMargenMes=.col_oculto,
      PptoMargenYTD=.col_oculto, CumpMargenYTD=.col_oculto
    ))
    
    .tot_s_new <- function(df) tibble::tibble(
      Oportunidad="",PerRazSoc="TOTAL",CLLinNegNo="",BucketSacos="",Segmento="",
      Asesor="",Presupuestado="",UltFecFact=as.Date(NA),
      PptoSacosMes=0L, SacosMes=sum(df$SacosMes,na.rm=TRUE), CumpSacosMes=NA_real_,
      PptoSacosYTD=0L, SacosYTD=sum(df$SacosYTD,na.rm=TRUE), CumpSacosYTD=NA_real_
    )
    .tot_m_new <- function(df) tibble::tibble(
      Oportunidad="",PerRazSoc="TOTAL",CLLinNegNo="",BucketSacos="",Segmento="",
      Asesor="",Presupuestado="",UltFecFact=as.Date(NA),
      PptoMargenMes=0L, MargenMes=sum(df$MargenMes,na.rm=TRUE), CumpMargenMes=NA_real_,
      PptoMargenYTD=0L, MargenYTD=sum(df$MargenYTD,na.rm=TRUE), CumpMargenYTD=NA_real_
    )
    
    data_sacos_r <- reactive({
      df<-data_filtrada();req(nrow(df)>0)
      base <- df %>%
        crear_link_cliente(col_razsoc="PerRazSoc",col_linneg="CLLinNegNo") %>%
        select(Oportunidad,PerRazSoc,CLLinNegNo,BucketSacos,Segmento,Asesor,
               Presupuestado,UltFecFact,PptoSacosMes,SacosMes,CumpSacosMes,
               PptoSacosYTD,SacosYTD,CumpSacosYTD)
      bind_rows(base,.tot_s_new(df))
    })
    data_margen_r <- reactive({
      df<-data_filtrada();req(nrow(df)>0)
      base <- df %>%
        crear_link_cliente(col_razsoc="PerRazSoc",col_linneg="CLLinNegNo") %>%
        select(Oportunidad,PerRazSoc,CLLinNegNo,BucketSacos,Segmento,Asesor,
               Presupuestado,UltFecFact,PptoMargenMes,MargenMes,CumpMargenMes,
               PptoMargenYTD,MargenYTD,CumpMargenYTD)
      bind_rows(base,.tot_m_new(df))
    })
    
    dc_registrar_tablas(ns,data_sacos_r,data_margen_r,col_ss,col_mg,
                        usr,dat,trigger_update,tipo_cliente="CLIENTE")
    output$Descargar <- do.call(downloadHandler,dc_descarga(data_filtrada,"nuevos"))
  })
}
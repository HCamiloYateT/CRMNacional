# UI ----
customDropdownMenu <-  function(..., badgeStatus = NULL, icon = NULL, 
                                headerText = NULL, .list = NULL, href = NULL,
                                showBadge = TRUE, showHeader = TRUE) {
  
  # Validar badgeStatus si se proporciona
  if (!is.null(badgeStatus)) {
    validateStatus(badgeStatus)
  }
  
  items <- c(list(...), .list)
  
  # Icono por defecto si no se proporciona
  if (is.null(icon)) {
    icon <- shiny::icon("bars")  # Icono genérico por defecto
  }
  
  numItems <- length(items)
  
  # Badge (número) - opcional
  if (is.null(badgeStatus) || !showBadge) {
    badge <- NULL
  } else {
    badge <- shiny::span(
      class = paste0("badge badge-", badgeStatus, " navbar-badge"), 
      numItems
    )
  }
  
  # Header text - completamente personalizable
  if (is.null(headerText)) {
    headerText <- NULL  # Sin header por defecto
  }
  
  # Construir el dropdown
  shiny::tags$li(
    class = "nav-item dropdown",
    shiny::tags$a(
      class = "nav-link",
      `data-toggle` = "dropdown",
      href = "#",
      `aria-expanded` = "false",
      icon,
      badge
    ),
    shiny::tags$div(
      class = "dropdown-menu dropdown-menu-lg custom-dropdown-menu",
      # Header opcional
      if (!is.null(headerText) && showHeader) {
        list(
          shiny::tags$span(
            class = "dropdown-item dropdown-header",
            headerText
          ),
          shiny::tags$div(class = "dropdown-divider")
        )
      },
      # Contenido
      items,
      # Footer opcional
      if (!is.null(href)) {
        shiny::tags$a(
          class = "dropdown-item dropdown-footer",
          href = href,
          target = "_blank",
          "More"
        )
      }
    )
  )
}


# Datos ----
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
    
    # Consulta para obtener precios adicionales
    
    # NY
    # Contrato "C" de Nueva York de café ----
    
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
get_pending_orders <- function(uid, pwd, periodo, asesores, ncliente, crmnalcliente) {
  tryCatch({
    # Consulta principal para obtener datos de la base de datos
    
    pedidos <- ConsultaSistema("syscafe", 
                               paste0("SELECT p1.PdcCod, p1.PdcLin, p1.PdcCan, p1.LinNegCod,
                                           p1.LinProCod, pd.PdcUsu, pd.PdcFecCre, pd.CliNit AS PerCod
                                           FROM EXPPEDI1 p1
                                           INNER JOIN EXPPEDID pd ON p1.PdcCod = pd.PdcCod AND 
                                                        pd.CiaCod = 10 AND pd.CliNit <> 32 AND 
                                                        pd.PdcEst = 'A' AND pd.PdcVtaNal = 1
                                           WHERE p1.CiaCod = 10 AND p1.PdcCan > 0 AND NOT 
                                                 EXISTS (SELECT 1 FROM EXPLOT1 l 
                                                         WHERE l.PdcCod = p1.PdcCod AND l.PdcLin = p1.PdcLin) AND 
                                                               pd.PdcUsu IN ('", paste(asesores, collapse = "','"), "')")
    )
    
    # Si no hay resultados, devolver un dataframe vacío
    if (nrow(pedidos) == 0) {
      return(data.frame(
        PerRazSoc = character(),
        Segmento = character(),
        PdcCod = numeric(),
        PdcCan = numeric(),
        PdcFecCre = as.Date(character()),
        LinNeg = character(),
        LinProNom = character(),
        PdcUsu = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    # Obtener datos adicionales de la base de datos
    lineas_negocio <- ConsultaSistema("syscafe", 
                                      "SELECT LinNegCod, LinNegNom AS LinNeg 
                                       FROM NLINEANE 
                                       WHERE CiaCod = 10")
    tipos_producto <- ConsultaSistema("syscafe", 
                                      "SELECT LinProCod, LinProNom 
                                        FROM NTIPPROD 
                                        WHERE CiaCod = 10")
    
    # Unir con NCLIENTE (objeto en R)
    pedidos <- pedidos %>%
      left_join(NCLIENTE %>% select(PerCod, PerRazSoc), by = "PerCod")
    
    # Unir con CRMNALCLIENTE (objeto en R)
    pedidos <- pedidos %>%
      left_join(
        CargarDatos("CRMNALCLIENTE") %>%
          rowwise() %>%
          mutate(Segmento = ifelse(input$FT_Periodo == 2024, Segmento2024, Segmento2025)) %>%
          select(LinNegCod, PerCod = CLCliNit, Segmento) %>%
          distinct(),
        by = c("PerCod", "LinNegCod")
      )
    
    # Unir con datos de líneas de negocio
    pedidos <- pedidos %>%
      left_join(lineas_negocio, by = "LinNegCod")
    
    # Unir con datos de tipos de producto
    pedidos <- pedidos %>%
      left_join(tipos_producto, by = "LinProCod")
    
    # Seleccionar y transformar columnas finales
    pedidos <- pedidos %>%
      select(PerRazSoc, Segmento, PdcCod, PdcCan, PdcFecCre, LinNeg, LinProNom, PdcUsu) %>%
      mutate(PdcFecCre = as.Date(PdcFecCre))
    
    return(pedidos)
    
  }, error = function(e) {
    warning("Error al obtener pedidos pendientes: ", e$message)
    # Devolver un dataframe vacío con la estructura correcta en caso de error
    return(data.frame(
      PerRazSoc = character(),
      Segmento = character(),
      PdcCod = numeric(),
      PdcCan = numeric(),
      PdcFecCre = as.Date(character()),
      LinNeg = character(),
      LinProNom = character(),
      PdcUsu = character(),
      stringsAsFactors = FALSE
    ))
  })
}

# Impresiones ----
ImprimirDTLead <- function(dat, botones, noms, formatos, dom, buscar) {
  
  # Adiciona botones a los datos
  aux1 <- dat %>% AdicionarBotones(botones)
  
  # Definir dimensiones de la tabla
  cols <- ncol(aux1)
  filas <- 1:nrow(aux1)
  colus <- (cols - length(botones)):(ncol(aux1) - 1)
  mat <- expand.grid(filas, colus) %>% as.matrix()
  
  # Ajustar nombres de columnas
  noms <- c(noms, rep("", cols - length(noms)))
  
  # Identificar variables según su formato
  vars_num <- which(formatos == "numero")
  vars_pct <- which(formatos == "porcentaje")
  vars_din <- which(formatos == "dinero")
  vars_mil <- which(formatos == "miles")
  vars_sac <- which(formatos == "sacos")
  
  # Ajustar la altura de la tabla
  alto <- paste0(min(nrow(aux1) * 45, 500), "px")
  
  # Crear la tabla interactiva
  dt <- datatable(aux1, escape = FALSE, rownames = FALSE, colnames = noms, style = "default",
                  extensions = c('Buttons', 'FixedColumns', 'FixedHeader'),
                  selection = list(target = 'cell', mode = "single", selectable = mat),
                  options = list(pageLength = nrow(aux1), dom = dom, searching = buscar, 
                                 autoWidth = FALSE, filter = 'top',
                                 ordering = TRUE, fixedColumns = list(leftColumns = 1),
                                 columnDefs = list(list(orderable = FALSE, targets = colus),
                                                   list(width = "74%", targets = 0),
                                                   list(width = "1%", targets = colus)),
                                 language = lang, scrollY = alto))
  
  # Formatear columnas según su tipo
  if (length(vars_num) > 0) {
    dt <- dt %>% formatRound(vars_num, digits = 2)
  }
  if (length(vars_pct) > 0) {
    dt <- dt %>% formatPercentage(vars_pct, digits = 2)
  }
  if (length(vars_din) > 0) {
    dt <- dt %>% formatCurrency(vars_din, digits = 0)
  }
  if (length(vars_mil) > 0) {
    dt <- dt %>% formatRound(vars_mil, digits = 0)
  }
  if (length(vars_sac) > 0) {
    dt <- dt %>% formatRound(vars_sac, digits = 1)
  }
  
  
  return(dt)
}
ImprimirDTAux <- function(dat, botones, noms, formatos, dom, buscar, alto=500) {
  
  # Adiciona botones a los datos
  aux1 <- dat %>% AdicionarBotones(botones)
  
  # Definir dimensiones de la tabla
  cols <- ncol(aux1)
  filas <- 1:nrow(aux1)
  colus <- (cols - length(botones)):(ncol(aux1) - 1)
  mat <- expand.grid(filas, colus) %>% as.matrix()
  
  # Ajustar nombres de columnas
  noms <- c(noms, rep("", cols - length(noms)))
  
  # Identificar variables según su formato
  vars_num <- which(formatos == "numero")
  vars_pct <- which(formatos == "porcentaje")
  vars_din <- which(formatos == "dinero")
  vars_mil <- which(formatos == "miles")
  vars_sac <- which(formatos == "sacos")
  
  # Ajustar la altura de la tabla
  alto <- paste0(min(nrow(aux1) * 60, alto), "px")
  
  # Crear la tabla interactiva
  dt <- datatable(aux1, escape = FALSE, rownames = FALSE, colnames = noms, style = "default",
                  extensions = c('Buttons', 'FixedColumns', 'FixedHeader'), class = "tabla_racafe",
                  selection = list(target = 'cell', mode = "single", selectable = mat),
                  options = list(pageLength = nrow(aux1), dom = dom, searching = buscar, 
                                 autoWidth = FALSE, filter = 'top',
                                 ordering = TRUE, fixedColumns = list(leftColumns = 1),
                                 fixedHeader = TRUE,
                                 buttons = list(list(extend = "excel", text = '<i class="fa fa-file-excel"></i> Descargar', className='copyButton',
                                                     filename = paste0("LotesPendientes_", format(Sys.time(), "%d%b%y_%H%M%S")), title = NULL,
                                                     exportOptions = list(modifier = list(page = "all"))
                                                     )
                                                ),
                                 columnDefs = list(list(width = "1%", targets = colus)),
                                 language = lang, scrollY = alto, scrollX = TRUE,
                                 rowCallback = JS(
                                   "function(row, data, index) {",
                                   "  if (data[0] == 'TOTAL') {",
                                   "    $('td', row).css({'font-weight': 'bold'});",
                                   "  }",
                                   "}")
                                 )
                  ) |> 
    
    formatStyle(1:nrow(aux1), backgroundColor =  "#fff")
    
    
  
  # Formatear columnas según su tipo
  if (length(vars_num) > 0) {
    dt <- dt %>% formatRound(vars_num, digits = 2)
  }
  if (length(vars_pct) > 0) {
    dt <- dt %>% formatPercentage(vars_pct, digits = 2)
  }
  if (length(vars_din) > 0) {
    dt <- dt %>% formatCurrency(vars_din, digits = 0)
  }
  if (length(vars_mil) > 0) {
    dt <- dt %>% formatRound(vars_mil, digits = 0)
  }
  if (length(vars_sac) > 0) {
    dt <- dt %>% formatRound(vars_sac, digits = 1)
  }
  
  
  return(dt)
}
ImprimirDTRAzSocLinNeg <- function(dat, noms, formatos, dom, buscar, alto=500) {

  aux1 <- dat %>%
    rowwise() %>%
    mutate(PerRazSoc = ifelse(PerRazSoc == "TOTAL", "TOTAL",
                              paste0('<a href="https://analitica.racafe.com/ClienteNacional/?RazSoc=',
                                     PerRazSoc,
                                     '&LinNeg=', LineaNegocio,
                                     '" target="_blank">',                                     
                                     PerRazSoc,
                                     '</a>') %>% HTML()
                              )
           )

  # Ajustar nombres de columnas
  noms <- c(noms, rep("", ncol(aux1) - length(noms)))

  # Identificar variables según su formato
  vars_num <- which(formatos == "numero")
  vars_pct <- which(formatos == "porcentaje")
  vars_din <- which(formatos == "dinero")
  vars_mil <- which(formatos == "miles")
  vars_sac <- which(formatos == "sacos")

  # Ajustar la altura de la tabla
  alto <- paste0(min(nrow(aux1) * 60, alto), "px")
  cols <- ncol(aux1)

  # Crear la tabla interactiva
  dt <- datatable(aux1, escape = FALSE, rownames = FALSE, colnames = noms, style = "default",
                  extensions = c('Buttons', 'FixedColumns', 'FixedHeader'), class = "tabla_racafe",
                  selection = "none",
                  options = list(pageLength = nrow(aux1), dom = dom, searching = buscar,
                                 autoWidth = FALSE, filter = 'top', ordering = TRUE,
                                 fixedHeader = TRUE,
                                 buttons = list(list(extend = "excel", text = '<i class="fa fa-file-excel"></i> Descargar', className='copyButton',
                                                     filename = paste0("LotesPendientes_", format(Sys.time(), "%d%b%y_%H%M%S")), title = NULL,
                                                     exportOptions = list(modifier = list(page = "all"))
                                                     )),
                                 fixedColumns = list(leftColumns = 2),  fixedHeader = TRUE,
                                 language = lang, scrollY = T, scrollX = alto,
                                 rowCallback = JS(
                                   "function(row, data, index) {",
                                   "  if (data[0] == 'TOTAL') {",
                                   "    $('td', row).css({'font-weight': 'bold'});",
                                   "  }",
                                   "}")
                                 )
                  )  %>%
    formatStyle(1:cols, backgroundColor =  "#FFF", textAlign = 'right') %>%
    formatStyle(1:1, textAlign = 'left', width = "200px")
  

  # Formatear columnas según su tipo
  if (length(vars_num) > 0) {
    dt <- dt %>% formatRound(vars_num, digits = 2)
  }
  if (length(vars_pct) > 0) {
    dt <- dt %>% formatPercentage(vars_pct, digits = 2)
  }
  if (length(vars_din) > 0) {
    dt <- dt %>% formatCurrency(vars_din, digits = 0)
  }
  if (length(vars_mil) > 0) {
    dt <- dt %>% formatRound(vars_mil, digits = 0)
  }
  if (length(vars_sac) > 0) {
    dt <- dt %>% formatRound(vars_sac, digits = 1)
  }


  return(dt)
}
ImprimirDTRazSoc <- function(data) {
  
  aux1 <- data %>%
    group_by(CLCliRazSo) %>% 
    summarise(Sacos = sum(SacDesp, na.rm = TRUE)) %>% 
    arrange(desc(Sacos)) %>% 
    janitor::adorn_totals("row", name = "TOTAL") %>% 
    AdicionarBotones(c("Contacto", "Detalle")) 
  
  noms <- c("Razón Social", "Sacos", "", "")
  
  
  cols <- ncol(aux1)
  filas <- 1:nrow(aux1)
  colus <- (cols - 2):(ncol(aux1) - 1)
  mat <- expand.grid(filas, colus) %>% as.matrix()
  
  alto <- paste0(min(nrow(aux1) * 55, 500), "px")
  
  dt <- datatable(aux1, escape = FALSE, rownames = FALSE, colnames = noms, style = "default",
                  extensions = c('Buttons', 'FixedColumns', 'FixedHeader'),
                  selection = list(target = 'cell', mode = "single", selectable = mat),
                  options = list(pageLength = nrow(aux1), dom = "ft", searching = T, 
                                 autoWidth = FALSE, filter = 'top',
                                 ordering = TRUE, fixedColumns = list(leftColumns = 1),
                                 columnDefs = list(list(orderable = FALSE, targets = colus),
                                                   list(width = "74%", targets = 0),
                                                   list(width = "1%", targets = colus)),
                                 language = lang, scrollY = alto)) %>% 
    formatRound(2, digits = 2) %>% 
    formatCurrency(3, digits = 0) %>% 
    formatStyle('CLCliRazSo', 
                target = 'row',
                fontWeight = styleEqual("TOTAL", "bold"))
  
  return(dt)
}
ImprimirTablaContacto <- function(razsoc){
  
  aux0 <- CargarDatos("CRMNALCONT") %>% 
    mutate(Fecha = as.Date(with_tz(as_datetime(Fecha), "America/Bogota"))) %>% 
    filter(PerRazSoc == razsoc) %>% 
    select(Usuario = Usuario,
           `Fecha` = Fecha,
           `Canal` = Canal,
           `Comentarios` = Comentarios) %>% 
    arrange(desc(Fecha))
  
  alto = paste0(200,"px")
  datatable(aux0, rownames = F, class = "compact",
            options = list(searchHighlight = TRUE, pageLength =5,
                           ordering=F, dom="ft",
                           scrollY = alto, language = lang,
                           columnDefs = list(list(width = '50%', targets = 3),
                                             list(targets = 3, "createdCell" = JS(js))
                           )
            )
  )
  
  
}
ValoresPlots <- function(value){
  
  total = sum(value)
  text <- ifelse(value / total <= 0.05, 'none', 'auto')
  
}
ColoresPlots2 <- function(value){
  
  valores = length(value)
  pal <- colorRampPalette(c("forestgreen", "royalblue4"))
  cols <- pal(valores)
  return(cols)
}
ImprimeSankey2 <- function(dat, vars, fun, var = NULL, colores, source) {
  # Verificar que el número de colores coincida con el número de variables
  if (length(vars) != length(colores)) {
    stop("El número de colores debe coincidir con el número de variables.")
  }
  
  # Identificar valores únicos que aparecen más de una vez
  tb <- table(unlist(sapply(vars, function(x) Unicos(dat[[x]]))) %>% as.character())
  tb <- tb[tb > 1] %>% unlist() %>% names()
  
  # Crear tabla auxiliar según la función especificada
  aux1 <- if (fun == "n") {
    # Si la función es conteo
    dat %>% 
      group_by(Origen = "TOTAL", across(all_of(vars))) %>% 
      summarise(Tot = n(), .groups = "drop") %>% 
      mutate(across(all_of(vars), 
                    ~ifelse(. %in% c(tb, "", NA), 
                            paste0(cur_column(), ., "--AUSENTE--"), 
                            .)))
  } else {
    # Si es otra función agregadora
    dat %>% 
      group_by(Origen = "TOTAL", across(all_of(vars))) %>% 
      summarise(Tot = !!parse_expr(paste0(fun, "(", var, ", na.rm = TRUE)")), 
                .groups = "drop") %>% 
      mutate(across(all_of(vars), 
                    ~ifelse(. %in% c(tb, "", NA), 
                            paste0(cur_column(), ., "--AUSENTE--*"), 
                            .)))
  }
  
  # Crear vectores de nombres y nodos
  vec <- c("Origen", vars)
  nds <- c("TOTAL", unlist(sapply(vars, function(x) Unicos(aux1[[x]]))) %>% as.character())
  nds <- nds[!grepl("--AUSENTE--", nds)]
  
  # Generar colores para cada grupo
  aux_col <- do.call("bind_rows", lapply(vars, function(x) {
    i <- which(vars == x)
    color_base <- colores[i]
    vars_ <- unique(aux1[[x]])
    n_colores <- length(vars_[!grepl("--AUSENTE--", vars_)])
    labs = Unicos(aux1[[x]])
    data.frame(
      label = labs[!grepl("--AUSENTE--", labs)],
      colores = colorRampPalette(c(lighten(color_base, 0.5), 
                                   color_base, 
                                   darken(color_base, 0.5)))(n_colores)
    )
  })) %>% 
    bind_rows(data.frame(label = "TOTAL", colores = "#000000"))
  
  # Generar textos para tooltips
  aux_txt <- do.call("bind_rows", lapply(vec, function(x) {
    tot <- sum(aux1$Tot)
    aux1 %>% 
      group_by(across(all_of(x))) %>% 
      summarise(Tot = sum(Tot),
                Pct = Tot/tot, 
                .groups = "drop") %>% 
      rowwise() %>% 
      mutate(txt = paste0("<b>Clientes: </b>", 
                          comma(Tot, accuracy = 1), 
                          "<br><b>Pct. del Total: </b>",
                          percent(Pct, accuracy = 0.01))) %>% 
      select(label = 1, txt)
  }))
  
  # Crear tabla de variables
  aux_var <- do.call("bind_rows", lapply(vec, function(x) {
    aux1 %>% 
      group_by(across(all_of(x))) %>% 
      summarise(Tot = sum(Tot),
                Pct = Tot/sum(aux1$Tot), 
                .groups = "drop") %>% 
      mutate(Var = x) %>% 
      select(label = 1, Var)
  }))
  
  # Crear tabla de nodos
  nodos <- data.frame(label = nds) %>% 
    left_join(aux_col, by = "label") %>%
    left_join(aux_txt, by = "label") %>% 
    left_join(aux_var, by = "label") %>% 
    mutate(label2 = Reduce(function(x, pattern) gsub(pattern, "", x), 
                           vars, 
                           nds),
           indices = seq_along(nds) - 1,
           texto = paste0("<b>", label2, "</b><br>", txt),
           grupo = Var) 
  
  # Crear enlaces entre nodos
  n <- length(vec) - 1
  aux_lista <- lapply(1:n, function(i) c(vec[i], vec[i + 1]))
  links <- do.call("bind_rows", lapply(aux_lista, function(x) {
    aux1 %>% 
      group_by(across(all_of(x))) %>% 
      summarise(Tot = sum(Tot), .groups = "drop") %>% 
      left_join(nodos %>% 
                  select(label, indices, VarSource = Var) %>% 
                  rename(source = indices),
                by = setNames("label", x[1])) %>% 
      left_join(nodos %>% 
                  select(label, indices, VarTarget = Var) %>% 
                  rename(target = indices),
                by = setNames("label", x[2])) %>% 
      select(source, target, value = Tot, VarSource, VarTarget) %>% 
      na.omit()
  }))
  
  # Calcular totales y porcentajes
  links <- links %>% 
    group_by(VarSource) %>% 
    mutate(value_total = sum(value)) %>% 
    # left_join(total_values, by = "source", suffix = c("", "_total")) %>%
    left_join(nodos %>% select(source = indices, Origen = label2), 
              by = "source") %>%
    left_join(nodos %>% select(target = indices, Destino = label2), 
              by = "target") %>%
    mutate(PctTot = value/sum(aux1$Tot),
           PctSource = value/value_total,
           texto = paste0("<b>Origen: </b>", Origen,
                          "<br><b>Destino: </b>", Destino,
                          "<br><b>Clientes: </b>", comma(value, accuracy = 1),
                          "<br><b>Pct. del Total: </b>", 
                          percent(PctTot, accuracy = 0.01),
                          "<br><b>Pct. del Origen: </b>",
                          percent(PctSource, accuracy = 0.01)))
  
  estandarizar_vector <- function(x, min_diff = 0.05) {
    n <- length(x)
    resultado <- numeric(n)
    resultado[1] <- x[1]  # Mantener el primer valor
    
    for(i in 2:n) {
      # El nuevo valor debe ser al menos min_diff mayor que el anterior
      resultado[i] <- max(resultado[i-1] + min_diff, x[i])
    }
    
    # Si el último valor supera 1, reescalar todo el vector
    if(resultado[n] > 1) {
      resultado <- resultado / max(resultado)
    }
    
    return(resultado)
  }
  calcular_posiciones_y <- function(grupo) {
    n <- sum(nodos$grupo == grupo)
    if(n == 0) return(0.5) 
    if(n == 1) return(0.5)
    else{
      
      pct = as.numeric(unlist(links[links$VarTarget == grupo, "PctSource"]))
      res <- cumsum(pct) - pct + (pct/2)
      res <- estandarizar_vector(res)
      return(res)
    }
    
  }
  
  y_origen <- calcular_posiciones_y("Origen")
  y_niv1 <- calcular_posiciones_y("Niv1")
  y_niv2 <- calcular_posiciones_y("Niv2")
  y_niv3 <- calcular_posiciones_y("Niv3")
  
  posiciones_y <- c(y_origen, y_niv1, y_niv2, y_niv3)
  
  # Crear gráfico Sankey
  sankey <- plot_ly(
    type = "sankey", source = source,
    arrangement = "fixed",
    orientation = "h",
    node = list(
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5),
      label = nodos$label2,
      color = nodos$colores,
      x = c(0,
            rep(0.33, length(unique(aux1 |>
                                      filter(!grepl("--AUSENTE--", Niv1)) |>
                                      na.omit() |>
                                      pull(Niv1))
            )),
            rep(0.66, length(unique(aux1 |>
                                      filter(!grepl("--AUSENTE--", Niv2)) |>
                                      na.omit() |>
                                      pull(Niv2))
            )),
            rep(0.99, length(unique(aux1 |>
                                      filter(!grepl("--AUSENTE--", Niv3)) |>
                                      na.omit() |>
                                      pull(Niv3))
            ))
      ),
      y = posiciones_y,
      hovertemplate = paste0("%{customdata}", "<extra></extra>"),
      customdata = nodos$texto
    ),
    link = list(
      hovertemplate = paste0("%{customdata}", "<extra></extra>"),
      customdata = links$texto,
      source = links$source,
      target = links$target,
      value = links$value,
      color = "rgba(0,0,0,0.3)"
    )
  ) %>%
    layout(clickmode = "event+select",
           title = "",
           annotations = list(
             # Anotaciones con valores para cada grupo
             list(x = 0, y = -0.03, text = "",
                  showarrow = FALSE, font = list(size = 14)),
             list(x = 0.25, y = -0.03, text = "Estado de Cliente",
                  showarrow = FALSE, font = list(size = 14)),
             list(x = 0.66, y = -0.03, text = "Estado de Negocio",
                  showarrow = FALSE, font = list(size = 14)),
             list(x = 1, y = -0.03, text = "Frecuencia / Razón Descartado",
                  showarrow = FALSE, font = list(size = 14))
             )
           ) %>%
    config(locale = "es", displayModeBar = F)
  
  sankey
  
  # Retornar lista con el gráfico y datos
  return(list(plot = sankey, nodos = nodos, arcos = links))
}

# Otras ----
crear_link_cliente <- function(dat, col_razsoc = "PerRazSoc", col_linneg = "LineaNegocio") {
  dat %>%
    rowwise() %>%
    mutate(
      !!col_razsoc := ifelse(
        !!sym(col_razsoc) == "TOTAL", 
        "TOTAL",
        {
          razsoc_val <- as.character(!!sym(col_razsoc))
          linneg_val <- as.character(!!sym(col_linneg))
          
          # URL encode para el href
          razsoc_encoded <- utils::URLencode(razsoc_val, reserved = TRUE)
          linneg_encoded <- utils::URLencode(linneg_val, reserved = TRUE)
          
          # HTML con estilos inline
          paste0(
            '<a href="https://analitica.racafe.com/ClienteNacional/?RazSoc=',
            razsoc_encoded,
            '&LinNeg=', linneg_encoded, 
            '" style="color: #000000; font-weight: bold; text-decoration: underline;" ',
            'onmouseover="this.style.color=\'#ff0000\';" ',
            'onmouseout="this.style.color=\'#000000\';">',
            razsoc_val,
            '</a>'
          )
        }
      )
    ) %>%
    ungroup()
}
extraer_info_cliente <- function(link_html) {
  # Caso 1: NULL o length cero
  if (is.null(link_html) || length(link_html) == 0) {
    return(list(
      razon_social = NA_character_,
      linea_negocio = NA_character_
    ))
  }
  
  # Caso 2: vacío o TOTAL
  if (link_html == "" || link_html == "TOTAL") {
    return(list(
      razon_social = NA_character_,
      linea_negocio = NA_character_
    ))
  }
  
  # Extraer href
  href <- tryCatch(
    sub('.*href="([^"]+)".*', "\\1", link_html),
    error = function(e) NA_character_
  )
  
  if (is.na(href)) {
    return(list(
      razon_social = NA_character_,
      linea_negocio = NA_character_
    ))
  }
  
  # Extraer query string
  query <- sub(".*\\?", "", href)
  
  # Separar pares clave-valor
  pares <- strsplit(query, "&")[[1]]
  
  if (length(pares) < 2) {
    return(list(
      razon_social = NA_character_,
      linea_negocio = NA_character_
    ))
  }
  
  valores <- lapply(pares, function(x) {
    partes <- strsplit(x, "=")[[1]]
    utils::URLdecode(partes[2])
  })
  
  names(valores) <- sapply(pares, function(x) strsplit(x, "=")[[1]][1])
  
  list(
    razon_social  = valores[["RazSoc"]],
    linea_negocio = valores[["LinNeg"]]
  )
}
extraer_razon_social <- function(url_vector) {
  sapply(url_vector, function(url) {
    if (url == "TOTAL") {
      return("TOTAL")
    }
    
    # Extraer el parámetro RazSoc del URL
    razsoc_match <- regexpr("RazSoc=([^&]+)", url)
    if (razsoc_match > 0) {
      razsoc_encoded <- regmatches(url, razsoc_match)
      razsoc_encoded <- gsub("RazSoc=", "", razsoc_encoded)
      # URL decode para obtener el texto original
      utils::URLdecode(razsoc_encoded)
    } else {
      url  # Si no encuentra el parámetro, devuelve el URL original
    }
  }, USE.NAMES = FALSE)
}
createSwitch <- function(inputId, label, value = FALSE, ns) {
  materialSwitch(
    inputId = ns(inputId),
    label = FormatearTexto(label, tamano_pct = 1),
    value = value,
    status = "danger",
    width = "100%"
  )
}

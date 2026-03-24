# Funciones auxiliares ----
crear_config_botones <- function(...) {
  botones  <- list(...)
  defaults <- list(size="xs",color="danger",style="outline",
                   tit_modal="",posicion="final",col_ref=NULL,width="48px")
  iconos   <- c("detalle"="search","historico"="chart-line","dashboard"="tachometer-alt",
                "ver"="eye","editar"="edit","eliminar"="trash","borrar"="trash-alt",
                "descartar"="ban","agregar"="plus","descargar"="download","subir"="upload",
                "configurar"="cog","compartir"="share-alt","copiar"="copy","imprimir"="print",
                "guardar"="save","cancelar"="times","aprobar"="check","rechazar"="times-circle")
  cfg <- lapply(names(botones), function(id) {
    b <- botones[[id]]
    if (is.null(b$module_ui)||is.null(b$module_server))
      stop(sprintf("El botón '%s' debe definir module_ui y module_server.",id))
    ico <- b$icono %||% iconos[[id]] %||% "circle"
    if (!grepl("^fa-",ico)) ico <- paste0("fa-",ico)
    list(nombre=b$nombre %||% tools::toTitleCase(id),
         icono=ico,titulo=b$titulo %||% tools::toTitleCase(id),
         tit_modal=b$tit_modal %||% paste0(tools::toTitleCase(id)," -"),
         input_id=paste0("btn_",id),accion=id,
         size=b$size %||% defaults$size,
         color=b$color %||% defaults$color,
         style=b$style %||% defaults$style,
         posicion=b$posicion %||% defaults$posicion,
         col_ref=b$col_ref %||% defaults$col_ref,
         width=b$width %||% defaults$width,
         module_ui=b$module_ui,module_server=b$module_server,
         module_id=b$module_id %||% paste0(id,"_module"),
         ui_params=b$ui_params %||% list(),
         extra_params=b$extra_params %||% list())
  })
  names(cfg) <- names(botones)
  cfg
}

crear_columnas_botones <- function(datos,botones_config,ns) {
  size_cfg <- list(xs=c("2px 6px","10px"),sm=c("3px 8px","12px"))
  filas <- seq_len(nrow(datos)); out <- list()
  for (id in names(botones_config)) {
    b <- botones_config[[id]]
    cls <- if (b$style=="outline") paste0("btn btn-outline-",b$color) else paste0("btn btn-",b$color)
    sp  <- size_cfg[[b$size %||% "xs"]]
    out[[b$nombre]] <- vapply(filas,function(i)
      gt::html(sprintf(
        '<button id="%s" class="%s btn-sm" style="padding:%s;font-size:%s;border-radius:8px;"
         title="%s" onclick="Shiny.setInputValue(\'%s\',%d,{priority:\'event\'})">
         <i class="fas %s"></i></button>',
        ns(paste0(b$input_id,"_",i)),cls,sp[1],sp[2],b$titulo,
        ns(b$input_id),i,b$icono)),character(1))
  }
  out
}

# Módulo tabla ----
GTBotonesUI <- function(id) {
  ns <- NS(id)
  fluidRow(column(12,gt_output(ns("tabla_gt"))))
}
GTBotones <- function(id, gt_table, data, botones_config, nombre_col=NULL) {
  moduleServer(id,function(input,output,session) {
    ns <- session$ns
    cols_filtro <- if (is.null(nombre_col)) character(0) else nombre_col
    dd_out <- reactiveVal(NULL)
    
    output$tabla_gt <- render_gt({
      tab <- if (is.reactive(gt_table)) gt_table() else gt_table
      dat <- tab[["_data"]]
      cols <- crear_columnas_botones(dat,botones_config,ns)
      for (id in names(botones_config)) {
        cfg <- botones_config[[id]]
        nm  <- cfg$nombre
        if (cfg$posicion %in% c("inicio","final")) {
          tab <- gt::cols_add(tab,!!nm:=cols[[nm]],
                              .before=if (cfg$posicion=="inicio") 1 else NULL)
        } else {
          if (is.null(cfg$col_ref)||!cfg$col_ref %in% names(dat))
            stop(sprintf("col_ref inválida para botón '%s'",id))
          tab <- gt::cols_add(tab,!!nm:=cols[[nm]],.after=cfg$col_ref)
        }
        tab <- tab %>%
          gt::fmt_markdown(columns=nm) %>%
          gt::cols_align("center",nm) %>%
          gt::cols_label(!!nm:="")
        tab <- do.call(
          gt::cols_width,
          list(tab, as.formula(
            paste0("`",nm,"` ~ gt::px(",as.numeric(gsub("px","",cfg$width)),")")
          ))
        )
      }
      tab
    })
    
    dd_vals <- lapply(botones_config,function(x) reactiveVal(NULL))
    names(dd_vals) <- names(botones_config)
    for (nm in names(botones_config)) {
      local({
        nm_ <- nm
        cfg <- botones_config[[nm_]]
        
        do.call(get(cfg$module_server),
                c(list(id=cfg$module_id,dd_data=reactive({dd_vals[[nm_]]()})),
                  cfg$extra_params))
      })
    }
    
    lapply(botones_config,function(cfg) {
      observeEvent(input[[cfg$input_id]],{
        idx <- input[[cfg$input_id]]
        dat <- if (is.reactive(data)) data() else data
        tab <- if (is.reactive(gt_table)) gt_table() else gt_table
        dat_gt <- tab[["_data"]]
        if (is.null(idx)||idx<1||idx>nrow(dat_gt)) return()
        fila_gt <- dat_gt[idx,,drop=FALSE]
        if (length(cols_filtro)>0) {
          cols_ok <- intersect(cols_filtro,names(dat))
          dat_f <- dat
          for (cc in cols_ok) {
            val <- fila_gt[[cc]]
            if (is.null(val) || length(val) != 1) next
            dat_f <- dat_f[dat_f[[cc]] == val, , drop = FALSE]
          }
        } else dat_f <- fila_gt
        id_txt <- NULL
        if (length(cols_filtro)>0) {
          cols_ok <- intersect(cols_filtro,names(fila_gt))
          if (length(cols_ok)>0)
            id_txt <- paste(fila_gt[1,cols_ok,drop=TRUE],collapse=" | ")
        }
        sel <- list(data    = dat_f, fila_gt = fila_gt, accion  = cfg$accion)
        dd_vals[[cfg$accion]](sel); dd_out(sel)
        ui_fun <- get(cfg$module_ui)
        ui_mod <- do.call(ui_fun,c(list(id=ns(cfg$module_id)),cfg$ui_params))
        showModal(modalDialog(title= htmltools::HTML(markdown::markdownToHTML(text = paste(cfg$tit_modal,id_txt),
                                                                              fragment.only = TRUE)),
                              ui_mod,size="xl",easyClose=TRUE,
                              footer=modalButton("Cerrar")))
      },ignoreInit=TRUE)
    })
    list(dd_data=reactive(dd_out()))
  })
}

# Módulos de prueba ----
DetalleUI <- function(id){ns<-NS(id);tagList(h4("Detalle"),verbatimTextOutput(ns("txt")))}
Detalle <- function(id,dd_data){moduleServer(id,function(input,output,session){
  output$txt <- renderPrint(dd_data())})}

EditarUI <- function(id,modo="basico"){ns<-NS(id);tagList(h4(paste("Editar -",modo)),verbatimTextOutput(ns("txt")))}
Editar <- function(id,dd_data){moduleServer(id,function(input,output,session){
  output$txt <- renderPrint(dd_data())})}

EliminarUI <- function(id){ns<-NS(id);tagList(h4("Eliminar"),verbatimTextOutput(ns("txt")))}
Eliminar <- function(id,dd_data){moduleServer(id,function(input,output,session){
  output$txt <- renderPrint(dd_data())})}

# App de prueba ----
ui <- fluidPage(GTBotonesUI("tabla"))
server <- function(input,output,session) {
  datos <- reactive(tibble::tibble(
    Cliente=paste0("Cliente_",1:5),
    Grupo=rep(c("A","B"),length.out=5)))
  
  tabla <- reactive(gt::gt(datos()))
  
  botones <- crear_config_botones(
    detalle=list(module_ui="DetalleUI",
                 module_server="Detalle",
                 posicion="inicio"),
    editar=list(module_ui="EditarUI",
                module_server="Editar",
                posicion="derecha",
                col_ref="Cliente",
                width="52px",
                ui_params=list(modo="PRUEBA"),
                color="primary"),
    eliminar=list(module_ui="EliminarUI",
                  module_server="Eliminar",
                  posicion="final",
                  width="52px",
                  color="danger"))
  
  GTBotones("tabla",tabla,datos,botones,nombre_col=c("Cliente","Grupo"))
}
shinyApp(ui,server)

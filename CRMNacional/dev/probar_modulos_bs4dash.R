# App de prueba rápida de módulos bs4Dash
# Uso:
# 1) Desde la carpeta CRMNacional ejecutar: shiny::runApp("dev/probar_modulos_bs4dash.R")
# 2) En el bloque MODULOS_ACTIVOS comenta/descomenta los módulos que quieres probar.
# 3) Si un módulo requiere parámetros especiales, ajusta su entrada en CATALOGO_MODULOS.

options(stringsAsFactors = FALSE)

library(shiny)
library(bs4Dash)
library(purrr)

.app_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (basename(.app_root) != "CRMNacional") {
  stop("Este script debe ejecutarse desde la carpeta CRMNacional")
}

# Carga utilidades y todos los módulos
source("shared/functions.R")
source("shared/values.R")
source("shared/filters.R")
module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
purrr::walk(sort(module_files), source)

entrada_modulo <- function(key, label, tab, ui_fun, server_fun = NULL, id = NULL,
                           ui_args = list(), server_args = list()) {
  list(
    key = key,
    label = label,
    tab = tab,
    ui_fun = ui_fun,
    server_fun = server_fun,
    id = ifelse(is.null(id), paste0("test_", key), id),
    ui_args = ui_args,
    server_args = server_args
  )
}

# -----------------------------------------------------------------------------
# CATALOGO_MODULOS
# Incluye una lista amplia de módulos para prueba rápida.
# Puedes extender/editar argumentos especiales por módulo.
# -----------------------------------------------------------------------------
CATALOGO_MODULOS <- list(
  entrada_modulo("indicadores", "Indicadores", "tab_indicadores", "IndicadoresUI", "Indicadores", id = "Indicadores"),
  entrada_modulo("comparacion_ind", "Comparación Indicadores", "tab_comp_ind", "ComparacionIndicadoresUI", "ComparacionIndicadores", id = "CompIndicadores"),
  entrada_modulo("calculadora", "Calculadoras", "tab_calculadora", "CalculadoraUI", "Calculadora", id = "Calculadoras"),
  entrada_modulo("presupuesto", "Presupuesto", "tab_presupuesto", "PresupuestoUI", "Presupuesto", id = "Presupuesto"),
  entrada_modulo("pendientes", "Pendientes", "tab_pendientes", "PendientesUI", "Pendientes", id = "Pendientes"),
  entrada_modulo("resumen_total", "Resumen Total", "tab_resumen_total", "ResumenTotalUI", "ResumenTotal", id = "ResumenTotal"),
  entrada_modulo("tabla_oportunidades", "Tabla Oportunidades", "tab_tbl_op", "TablaOportunidadesUI", "TablaOportunidades", id = "Listado"),
  entrada_modulo("dashboard_oportunidades", "Dashboard Oportunidades", "tab_dash_op", "DashboardOportunidadesUI", "DashboardOportunidades", id = "Oportunidades"),
  entrada_modulo("form_oportunidad", "Formulario Oportunidad", "tab_form_op", "FormularioOportunidadUI", "FormularioOportunidad", id = "Formulario"),
  entrada_modulo("detalle_cliente", "Detalle Cliente", "tab_det_cliente", "DetalleClienteUI", "DetalleCliente", id = "ResumenClientes"),
  entrada_modulo("detalle_recuperar", "Detalle Cliente Recuperar", "tab_det_rec", "DetalleClienteRecuperarUI", "DetalleClienteRecuperar", id = "ResumenClientesRecuperar"),
  entrada_modulo("rfm", "RFM", "tab_rfm", "RFMUI", "RFM", id = "RFMClientesSacos"),
  entrada_modulo("tabla_modal_celda", "Tabla Modal Celda", "tab_tabla_modal", "TablaModalCeldaUI", "TablaModalCelda", id = "ResumenLeads"),
  entrada_modulo("sankey", "Embudo Sankey", "tab_sankey", "SankeyTablaUI", "SankeyTabla", id = "EmbudoTest"),
  entrada_modulo("individual", "Consulta Individual", "tab_individual", "IndividualUI", "Individual", id = "ConsultaIndivual"),
  entrada_modulo("competencia", "Competencia", "tab_competencia", "CompetenciaUI", "Competencia", id = "Competencia"),
  entrada_modulo("gestion_producto", "Gestión Producto", "tab_gestion_producto", "GestionProductoUI", "GestionProducto", id = "Productos"),
  entrada_modulo("tareas_creacion", "Creación de Tareas", "tab_tareas_crea", "TaskCreationUI", "TaskCreation", id = "Tareas"),
  entrada_modulo("tareas_lista", "Listado de Tareas", "tab_tareas_list", "noteDisplayUI", "noteDisplay", id = "NotasTareas"),
  entrada_modulo("notificaciones", "Notificaciones", "tab_notificaciones", "NotificacionesUI", "Notificaciones", id = "Notificaciones"),
  entrada_modulo("cotizacion", "Cotizador", "tab_cotizador", "CotizacionUI", "Cotizacion", id = "Cotizador"),
  entrada_modulo("cohortes", "Cohortes", "tab_cohortes", "CohortesUI", "Cohortes", id = "Cohortes"),
  entrada_modulo("clientes_nuevos_rec", "Clientes nuevos/recuperar", "tab_clientes_nuevos", "ClientesNuevosRecuperadosUI", "ClientesNuevosRecuperados", id = "ClientesNuevosRecuperar"),
  entrada_modulo("detalle_leads", "Detalle Leads", "tab_det_leads", "DetalleLeadsUI", "DetalleLeads", id = "DetalleLeads")
)

# -----------------------------------------------------------------------------
# MODULOS_ACTIVOS
# Deja activos solo los módulos que quieras probar (comentar/descomentar).
# -----------------------------------------------------------------------------
MODULOS_ACTIVOS <- c(
  "indicadores"
  # ,"comparacion_ind"
  # ,"calculadora"
  # ,"presupuesto"
  # ,"pendientes"
  # ,"resumen_total"
  # ,"tabla_oportunidades"
  # ,"dashboard_oportunidades"
  # ,"form_oportunidad"
  # ,"detalle_cliente"
  # ,"detalle_recuperar"
  # ,"rfm"
  # ,"tabla_modal_celda"
  # ,"sankey"
  # ,"individual"
  # ,"competencia"
  # ,"gestion_producto"
  # ,"tareas_creacion"
  # ,"tareas_lista"
  # ,"notificaciones"
  # ,"cotizacion"
  # ,"cohortes"
  # ,"clientes_nuevos_rec"
  # ,"detalle_leads"
)

if (length(MODULOS_ACTIVOS) == 0) {
  stop("No hay módulos activos. Descomenta al menos uno en MODULOS_ACTIVOS.")
}

modulos_disponibles <- setNames(CATALOGO_MODULOS, map_chr(CATALOGO_MODULOS, "key"))
modulos_seleccionados <- modulos_disponibles[MODULOS_ACTIVOS]

claves_invalidas <- MODULOS_ACTIVOS[is.na(modulos_seleccionados)]
if (length(claves_invalidas) > 0) {
  stop(sprintf("Módulos no encontrados en CATALOGO_MODULOS: %s", paste(claves_invalidas, collapse = ", ")))
}

ui <- bs4DashPage(
  title = "Test módulos",
  navbar = bs4DashNavbar(
    skin = "light",
    status = "white",
    border = FALSE,
    leftUi = tags$span("App de pruebas de módulos", style = "font-weight:600;")
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    bs4SidebarMenu(
      id = "menu_test",
      !!!purrr::map(modulos_seleccionados, ~ {
        bs4SidebarMenuItem(.x$label, tabName = .x$tab, icon = icon("flask"))
      })
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      !!!purrr::map(modulos_seleccionados, ~ {
        ui_callable <- get(.x$ui_fun, mode = "function", envir = globalenv())
        ui_params <- c(list(id = .x$id), .x$ui_args)
        bs4TabItem(tabName = .x$tab, do.call(ui_callable, ui_params))
      })
    )
  ),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter()
)

server <- function(input, output, session) {
  # Mocks base para cubrir firmas comunes en módulos
  mock_user <- reactive("USUARIO_TEST")
  mock_data <- reactive(data.frame())
  mock_indicadores <- reactive(
    data.frame(
      Item = c("TRM (Hoja de trabajo)", "Precio NYC (HT)"),
      Valor = c(4200, 180),
      last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )
  mock_rv <- reactiveValues(selected = NULL, data = data.frame())
  mock_trigger <- reactiveVal(0)
  mock_notes <- reactive(data.frame())
  mock_fecha_rango <- reactive(c(Sys.Date() - 30, Sys.Date()))

  arg_lookup <- list(
    dat = mock_data,
    data = mock_data,
    data_raw = mock_data,
    data_tx = mock_data,
    data_f = mock_data,
    data_t = mock_data,
    data_c = mock_data,
    data_op = mock_data,
    data_op_completa = mock_data,
    datos_op = mock_data,
    datos_leads = mock_data,
    dat_ped = mock_data,
    dat_leads = mock_data,
    dat_oportunidades = mock_data,
    dat_competencia = mock_data,
    dd_data = mock_data,
    df = mock_data,
    dt = reactive(DT::datatable(data.frame(Mensaje = "Mock DT"))),
    gt_table = reactive(gt::gt(data.frame(Mensaje = "Mock GT"))),
    dat_ind = mock_indicadores,
    data_ind = mock_indicadores,
    usr = mock_user,
    usuario = mock_user,
    rv = mock_rv,
    notes_data = mock_notes,
    trigger = mock_trigger,
    trigger_update = mock_trigger,
    fecha_rango = mock_fecha_rango,
    tit = reactive("PRUEBA"),
    tipo_cliente_default = reactive("CLIENTE"),
    botones_config = reactive(list())
  )

  for (m in modulos_seleccionados) {
    if (is.null(m$server_fun)) next

    server_callable <- get(m$server_fun, mode = "function", envir = globalenv())
    server_formals <- names(formals(server_callable))

    server_params <- list(id = m$id)
    missing_params <- character(0)

    for (param in setdiff(server_formals, "id")) {
      if (param %in% names(m$server_args)) {
        server_params[[param]] <- m$server_args[[param]]
      } else if (param %in% names(arg_lookup)) {
        server_params[[param]] <- arg_lookup[[param]]
      } else if (is.symbol(formals(server_callable)[[param]]) &&
                 identical(formals(server_callable)[[param]], quote(expr = ))) {
        missing_params <- c(missing_params, param)
      }
    }

    if (length(missing_params) > 0) {
      msg <- sprintf("[%s] Faltan argumentos para server_fun '%s': %s",
                     m$key, m$server_fun, paste(missing_params, collapse = ", "))
      message("[WARN] ", msg)
      showNotification(msg, type = "warning", duration = NULL)
      next
    }

    tryCatch(
      do.call(server_callable, server_params),
      error = function(e) {
        message(sprintf("[WARN] No se pudo iniciar '%s': %s", m$label, e$message))
        showNotification(
          paste0("No se pudo iniciar módulo '", m$label, "': ", e$message),
          type = "warning",
          duration = NULL
        )
      }
    )
  }
}

shinyApp(ui, server)

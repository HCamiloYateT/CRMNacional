# CRMNacional

Repositorio con una aplicación **Shiny (bs4Dash)** para gestión comercial de clientes nacionales y varios scripts de preparación/actualización de datos en R.

## Estructura real del repositorio

### 1) Aplicación Shiny
La app vive en `CRMNacional/` y sigue la estructura clásica de Shiny:

- `CRMNacional/app.R`: entrada consolidada (`shinyApp(ui, server)`).
- `CRMNacional/global.R`: configuración global, carga de paquetes, datos, servicios y módulos auxiliares.
- `CRMNacional/ui.R`: definición de la interfaz principal (`bs4DashPage`).
- `CRMNacional/server.R`: lógica de servidor, cachés reactivas e integración de módulos.
- `CRMNacional/modules/`: módulos funcionales de la app (Indicadores, Oportunidades, Leads, RFM, Presupuesto, Cohortes, Pendientes, etc.).
- `CRMNacional/ui/`: componentes de UI (header, sidebar, body, footer, controlbar, preloader).
- `CRMNacional/shared/`: utilidades compartidas (`functions.R`, `filters.R`, `values.R`).
- `CRMNacional/core/services.R`: reglas de negocio y servicios de dominio.
- `CRMNacional/config/`: carpeta reservada para configuración por ambiente.
- `CRMNacional/www/style.css`: estilos CSS de la aplicación.

### 2) Scripts de datos/procesos en la raíz
En la raíz hay scripts operativos para extracción, transformación, indicadores y comunicaciones:

- `DataPreparation.R`: integración y preparación de datos (incluye lecturas desde OneDrive/Graph y procesamiento principal).
- `PreparacionDatos.R`: pipeline de extracción y cargue a base de datos para entidades CRM.
- `Transaccionalidad.R`: cálculo de transaccionalidad, segmentación y métricas históricas de clientes.
- `Indicadores.R`: consulta y consolidación de indicadores (internos y externos como FNC).
- `Comunicaciones.R`: armado de información para comunicaciones/correos.
- `Funciones.r`: funciones utilitarias generales reutilizables.

## Requerimientos generales

- R (versión compatible con `shiny`, `tidyverse`, `DBI`, `bs4Dash` y paquetes de analítica usados por el proyecto).
- Acceso a fuentes de datos internas (consultas a tablas CRM y de sistema).
- Variables de entorno para credenciales cuando aplique (`SYS_UID`, `SYS_PWD`).

## Ejecución de la app

Desde la raíz del repositorio:

```r
shiny::runApp("CRMNacional")
```

## Notas

- El repositorio contiene tanto la capa de visualización (app Shiny) como procesos batch/ETL en scripts separados.
- Varias rutinas dependen de conexiones internas y datos no versionados (por ejemplo `CRMNacional/data/data.RData`).

## App de prueba de módulos (desarrollo)

Para probar módulos de forma aislada en una app `bs4Dash`, se agregó el script:

- `CRMNacional/dev/probar_modulos_bs4dash.R`

Uso rápido:

```r
setwd("CRMNacional")
shiny::runApp("dev/probar_modulos_bs4dash.R")
```

En ese archivo hay un bloque `MODULOS_ACTIVOS` donde puedes **comentar/descomentar** módulos para activar solo los que quieras validar, y un `CATALOGO_MODULOS` amplio con opciones de prueba para la mayoría de módulos actuales.

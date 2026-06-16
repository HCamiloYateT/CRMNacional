# CRMNacional

Repositorio con una aplicación **Shiny (bs4Dash)** para gestión comercial de clientes nacionales y varios scripts de preparación/actualización de datos en R.

## Estructura real del repositorio

### 1) Aplicación Shiny
La app vive en `CRMNacional/` y sigue la estructura clásica de Shiny:

- `CRMNacional/global.R`: configuración global, carga de paquetes, carga inicial de datos y de módulos auxiliares.
- `CRMNacional/ui.R`: definición de la interfaz principal (`bs4DashPage`).
- `CRMNacional/server.R`: lógica de servidor, cachés reactivas, integración de datos y módulos.
- `CRMNacional/misc/modules/`: módulos funcionales de la app (Indicadores, Oportunidades, Leads, RFM, Presupuesto, Cohortes, Pendientes, etc.).
- `CRMNacional/misc/ui/`: componentes de UI (header, sidebar, body, footer, controlbar, preloader).
- `CRMNacional/misc/functions.R`, `CRMNacional/misc/filters.R`, `CRMNacional/misc/values.R`: utilidades compartidas.
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


## Propuesta de estructura concreta (1 archivo por módulo)

Si quieren dejar la app con una estructura más mantenible y con **un archivo por módulo**, propongo esta organización objetivo:

```text
CRMNacional/
├─ app.R                         # punto de entrada (alternativa a ui.R + server.R)
├─ global.R
├─ config/
│  ├─ packages.R                 # carga de librerías
│  ├─ options.R                  # opciones globales
│  └─ constants.R                # constantes de negocio
├─ core/
│  ├─ data_sources.R             # conexiones y lecturas
│  ├─ repositories.R             # acceso a datos (queries/fuentes)
│  ├─ services.R                 # reglas de negocio reutilizables
│  ├─ reactive_store.R           # reactives/global state
│  └─ routing.R                  # registro/ensamble de módulos
├─ modules/
│  ├─ dashboard_leads.R
│  ├─ dashboard_oportunidades.R
│  ├─ leads.R
│  ├─ detalle_leads.R
│  ├─ formulario_leads.R
│  ├─ oportunidades.R
│  ├─ cotizador.R
│  ├─ presupuesto.R
│  ├─ cohortes.R
│  ├─ rfm.R
│  ├─ indicadores.R
│  ├─ pendientes.R
│  ├─ tareas.R
│  ├─ notificaciones.R
│  └─ ...                        # 1 archivo por módulo funcional
├─ ui/
│  ├─ page.R                     # bs4DashPage
│  ├─ header.R
│  ├─ sidebar.R
│  ├─ body.R
│  ├─ controlbar.R
│  └─ footer.R
├─ shared/
│  ├─ helpers.R                  # utilidades transversales
│  ├─ filters.R                  # filtros comunes
│  ├─ validators.R               # validaciones
│  └─ formatters.R               # formateadores
├─ www/
│  ├─ style.css
│  └─ img/
├─ tests/
│  ├─ testthat/
│  └─ shinytest2/
└─ dev/
   └─ probar_modulos_bs4dash.R
```

### Convenciones recomendadas por módulo

Cada archivo en `modules/` debería exponer siempre la misma interfaz:

- `mod_<nombre>_ui(id)`
- `mod_<nombre>_server(id, rv, data, ...)`

Ejemplo: `modules/leads.R` contiene únicamente `mod_leads_ui()` y `mod_leads_server()` (más helpers privados internos del módulo).

### Mapeo sugerido desde la estructura actual

- `CRMNacional/misc/modules/*.R` → `CRMNacional/modules/*.R`
- `CRMNacional/misc/ui/*.R` → `CRMNacional/ui/*.R`
- `CRMNacional/misc/functions.R`, `filters.R`, `values.R` → `CRMNacional/shared/*`
- `CRMNacional/ui.R` y `CRMNacional/server.R` pueden mantenerse inicialmente y migrar a `app.R` al final.

### Plan de migración en 4 pasos

1. Crear carpetas nuevas (`modules`, `ui`, `shared`, `core`, `config`) sin romper la app actual.
2. Migrar módulos uno a uno (`misc/modules` → `modules`) respetando la firma `mod_*_ui/server`.
3. Centralizar utilidades comunes en `shared/` y reglas de negocio en `core/services.R`.
4. Cuando todo funcione, consolidar entrada en `app.R` y dejar `ui.R/server.R` como wrappers o retirarlos.

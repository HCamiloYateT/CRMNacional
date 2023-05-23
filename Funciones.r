# Funciones ----

## Paquetes ----
Loadpkg <- function(pkg) {
# Descripción: Carga los paquetes especificados en el entorno de trabajo.
# Parámetros:
#   - pkg: Un vector de caracteres que contiene los nombres de los paquetes que se desean cargar.
# Valor de retorno:
#   - Ninguno.

  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

## Cadenas de Caracteres ----
LimpiarCadena <- function(x, rem_espacios = T, rem_numeros=T, rem_caresp =T, rem_acentos = T){
  # Descripción: Limpia una cadena de texto aplicando diferentes transformaciones.
  # Parámetros:
  #   - x: La cadena de texto a limpiar.
  #   - rem_espacios: (Opcional) Un valor lógico que indica si se deben eliminar los espacios en blanco (predeterminado: TRUE).
  #   - rem_numeros: (Opcional) Un valor lógico que indica si se deben eliminar los números (predeterminado: TRUE).
  #   - rem_caresp: (Opcional) Un valor lógico que indica si se deben eliminar los caracteres especiales (predeterminado: TRUE).
  #   - rem_acentos: (Opcional) Un valor lógico que indica si se deben eliminar los acentos (predeterminado: TRUE).
  # Valor de retorno:
  #   - La cadena de texto resultante después de aplicar las transformaciones especificadas.
  
  x <- trimws(str_to_upper(gsub("([\\s])\\1+","\\1", x, perl=T)))
  x <- ifelse(rem_espacios, gsub("\\s", "",x), x)
  x <- ifelse(rem_numeros, gsub("\\d", "",x), x)
  x <- ifelse(rem_caresp, gsub("[^[:alnum:]]", "",x), x)
  x <- ifelse(rem_caresp, iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT'), x)
  return(x)
}
UnirCadenas <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  # Descripción: Une múltiples cadenas de texto en una sola cadena. Difiere de `paste` ya que permite omitir valores ausentes
  # Parámetros:
  #   - ...: Las cadenas de texto a unir.
  #   - sep: (Opcional) El separador a utilizar entre las cadenas (predeterminado: " ").
  #   - collapse: (Opcional) El separador a utilizar entre las cadenas resultantes (predeterminado: NULL).
  #   - na.rm: (Opcional) Un valor lógico que indica si se deben eliminar los valores NA (predeterminado: FALSE).
  # Valor de retorno:
  #   - La cadena de texto resultante después de unir las cadenas especificadas.
  
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}

## Números ----
SiError_0 <- function(x){
  # Descripción: Reemplaza los valores NaN, Inf y -Inf en un vector numérico por 0.
  # Parámetros:
  #   - x: El vector numérico en el que se buscarán los valores NaN, Inf y -Inf.
  # Valor de retorno:
  #   - Un nuevo vector numérico con los valores NaN, Inf y -Inf reemplazados por 0.
  
  
  ifelse(x %in% c(NaN, Inf, -Inf),  0, x)
}
Variacion <- function(ini, fin){
  # Descripción: Calcula la variación porcentual entre un número inicial y uno final.
  # Parámetros:
  #   - ini: El número inicial.
  #   - fin: El número final.
  # Valor de retorno:
  #   - La variación porcentual entre el número inicial y el número final.
  
  var  <-  if(ini == 0 & fin == 0){
    0
  } else if (ini == 0 ){
    0
  } else if (ini * fin >= 0 ){
    (fin - ini) / ini
  } else if (ini * fin < 0){
    (fin - ini) / abs(ini)
  }
  return(var)
}
Moda <- function(x, na.rm = TURE) {
  # Descripción: Calcula la moda de un vector numérico o categórico.
  # Parámetros:
  #   - x: El vector numérico o categórico del cual se desea calcular la moda.
  #   - na.rm: (Opcional) Un valor lógico que indica si se deben excluir los valores NA en el cálculo de la moda (predeterminado: TRUE).
  # Valor de retorno:
  #   - La moda del vector, es decir, el valor que aparece con mayor frecuencia.
  
  
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

## Fechas ----
PrimerDia <- function(x){
  # Descripción: Calcula el primer día del mes de una fecha dada.
  # Parámetros:
  #   - x: La fecha de la cual se desea obtener el primer día del mes.
  # Valor de retorno:
  #   - La fecha correspondiente al primer día del mes de la fecha dada.
  
  require(lubridate)
  x <- lubridate::floor_date(as.Date(x), unit = "month")
}
FechaTexto <- function(x, dia = T, dia_nombre = T, dia_nom_abr = T, mes = T, mes_abr = T, anho = T, anho_abr = T, sep_texto=T){
  # Descripción: Convierte una fecha en formato de texto personalizado.
  # Parámetros:
  #   - x: La fecha que se desea convertir.
  #   - dia: (Opcional) Un valor lógico que indica si se debe incluir el día (predeterminado: TRUE).
  #   - dia_nombre: (Opcional) Un valor lógico que indica si se debe incluir el nombre completo del día (predeterminado: TRUE).
  #   - dia_nom_abr: (Opcional) Un valor lógico que indica si se debe incluir el nombre abreviado del día (predeterminado: TRUE).
  #   - mes: (Opcional) Un valor lógico que indica si se debe incluir el mes (predeterminado: TRUE).
  #   - mes_abr: (Opcional) Un valor lógico que indica si se debe incluir el nombre abreviado del mes (predeterminado: TRUE).
  #   - anho: (Opcional) Un valor lógico que indica si se debe incluir el año (predeterminado: TRUE).
  #   - anho_abr: (Opcional) Un valor lógico que indica si se debe incluir el año abreviado (predeterminado: TRUE).
  #   - sep_texto: (Opcional) Un valor lógico que indica si se debe incluir un separador de texto entre las partes de la fecha (predeterminado: TRUE).
  # Valor de retorno:
  #   - La fecha convertida en formato de texto personalizado.
  
  require(lubridate)
  require(stringr)
  d <- ifelse(dia, day(x), NA)
  dia_l <- c('Lunes','Martes','Miércoles','Jueves','Viernes','Sábado','Domingo')
  dia_c <- c('lun','mar','mié','jue','vie','sáb','dom')
  dn <-ifelse(dia_nombre, ifelse(dia_nom_abr, dia_c[wday(x, week_start = 1)], dia_l[wday(x, week_start = 1)]), NA)
  
  mes_l <- c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre')
  mes_c <- c('Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic')
  m <- ifelse(mes, ifelse(mes_abr, mes_c[month(x)], mes_l[month(x)]), NA)
  y <- ifelse(anho, ifelse(anho_abr, format(x, "%y"), format(x, "%Y")),NA)
  
  
  res <- UnirCadenas(dn,  UnirCadenas(d,m,y, sep = ifelse(sep_texto, " de ", ""), na.rm = T), sep=", ", na.rm = T)
  
  return(res)
}
EdadCumplida <- function(from, to) {
  # Descripción: Calcula la edad en años entre dos fechas.
  # Parámetros:
  #   - from: La fecha de inicio (fecha de nacimiento).
  #   - to: La fecha final (fecha actual).
  # Valor de retorno:
  #   - La edad en años entre las dos fechas.
  
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

## Manejo de Datos ----
TopAbsoluto <- function(data, var_recode, var_top, fun_Top, n=10, nom_var, lab_recodificar = "OTROS"){
  # Descripción: Recodifica las categorías menos frecuentes de una variable según su valor absoluto o una función de resumen y las agrupa en una nueva categoría.
  # Parámetros:
  #   - data: El conjunto de datos en el cual se encuentra la variable a recodificar.
  #   - var_recode: El nombre de la variable que se desea recodificar.
  #   - var_top: El nombre de la variable a partir de la cual se calcularán las frecuencias o la función de resumen.
  #   - fun_Top: La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
  #   - n: (Opcional) El número máximo de categorías principales a conservar (predeterminado: 10).
  #   - nom_var: El nombre para la nueva variable recodificada.
  #   - lab_recodificar: (Opcional) El nombre o etiqueta para las categorías recodificadas (predeterminado: "OTROS").
  # Valor de retorno:
  #   - El conjunto de datos con la variable recodificada según las categorías principales y las categorías recodificadas.
  
  require(rlang)
  require(forcats)
  datos = data
  
  if (fun_Top == "n"){
    aux1 <- datos %>% 
      mutate(Tot = n()) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var= n(),
                Pct = Var/unique(Tot))
  } 
  else{
    aux1 <- datos %>% 
      mutate(Tot= !!parse_expr(paste(fun_Top, "(", var_top,", na.rm = T)"))) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var= !!parse_expr(paste0(fun_Top, "(", var_top,", na.rm = T)")),
                Pct = Var/unique(Tot))
  }

  aux2 <- aux1 %>%
    arrange(desc(Var)) %>%
    mutate(Seq = row_number(),
           !!nom_var := !!parse_expr(paste0("ifelse(Seq<=n, as.character(",
                                            var_recode, "), '",
                                            lab_recodificar, "'", ")"))) %>%
    select(all_of(var_recode), all_of(nom_var))

  data <- datos %>%
    left_join(aux2, by = var_recode) %>% 
    mutate(!!nom_var := !!parse_expr(paste0("factor(", nom_var, ", levels = unique(aux2$",nom_var ,"), ordered = T)")),
           !!nom_var := !!parse_expr(paste0("fct_relevel(", nom_var,", 'OTROS', after = Inf)"))
           )
  return(data)
}
TopRelativo <- function(data, var_recode, var_top, fun_Top, pct_min=0.05, nom_var, lab_recodificar = "OTROS"){
  # Descripción: Recodifica las categorías menos frecuentes de una variable según su valor relativo o una función de resumen y las agrupa en una nueva categoría.
  # Parámetros:
  #   - data: El conjunto de datos en el cual se encuentra la variable a recodificar.
  #   - var_recode: El nombre de la variable que se desea recodificar.
  #   - var_top: El nombre de la variable a partir de la cual se calcularán las frecuencias o la función de resumen.
  #   - fun_Top: La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
  #   - pct_min: (Opcional) El porcentaje mínimo necesario para considerar una categoría principal (predeterminado: 0.05).
  #   - nom_var: El nombre para la nueva variable recodificada.
  #   - lab_recodificar: (Opcional) El nombre o etiqueta para las categorías recodificadas (predeterminado: "OTROS").
  # Valor de retorno:
  #   - El conjunto de datos con la variable recodificada según las categorías principales y las categorías recodificadas.
  
  datos = data
  if (fun_Top == "n"){
    aux1 <- datos %>% 
      mutate(Tot = n()) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var= n(),
                Pct = Var/unique(Tot))
  } 
  else{
    aux1 <- datos %>% 
      mutate(Tot= !!parse_expr(paste(fun_Top, "(", var_top,", na.rm = T)"))) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var= !!parse_expr(paste0(fun_Top, "(", var_top,", na.rm = T)")),
                Pct = Var/unique(Tot))
  }
  
  aux2 <- aux1 %>% 
    arrange(desc(Var)) %>% 
    mutate(Seq = row_number(),
           !!nom_var := !!parse_expr(paste0("ifelse(Pct>pct_min, as.character(", 
                                            var_recode, "), '", 
                                            lab_recodificar, "'", ")"))) %>% 
    select(all_of(var_recode), all_of(nom_var))
  
  data <- datos %>%
    left_join(aux2, by = var_recode) %>%
    mutate(!!nom_var := !!parse_expr(paste0("factor(", nom_var, ", levels = unique(aux2$",nom_var ,"), ordered = T)")),
           !!nom_var := !!parse_expr(paste0("fct_relevel(", nom_var,", 'OTROS', after = Inf)"))
           )
  return(data)
}


## Graficos ----
vline <- function(x = 0, color = "red") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}
hline <- function(y = 0, color = "#ff3a21") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color)
  )
}
ValoresPlots <- function(value){
  
  total = sum(value)
  text <- ifelse(value / total <= 0.05, 'none', 'auto')
  
}
ColoresPlots <- function(value){
  
  valores = length(value)
  pal <- colorRampPalette(c("gray90", "gray20"))
  cols <- pal(valores)
  text <- ifelse(value == "RACAFE & CIA S C A", 'firebrick',cols)
  return(text)
}
ColoresPlots2 <- function(value){
  
  valores = length(value)
  pal <- colorRampPalette(c("forestgreen", "royalblue4"))
  cols <- pal(valores)
  return(cols)
}

## Formatos ----
Negrita <- function(x, color="#55595c"){
  # Pone un texto dado en negritas en codigo HTML
  paste0("<strong><span style='color: ",color,"';>", x, "</span></strong>") %>% HTML
  
}
DefinirFormato <- function(formato){
  # Define el formato para un numero segun su tipo: coma, numero, dinero, porcentaje
  formato <- if(formato == "coma"){
    label_number(accuracy = 1,scale = 1,suffix = "",prefix = "", big.mark = ",")
  } else if(formato == "numero"){
    label_number(accuracy = 0.01,scale = 1,suffix = "",prefix = "", big.mark = ",")
  } else if(formato == "dinero"){
    label_number(accuracy = 1,scale = 1,suffix = "",prefix = "$", big.mark = ",")
  } else if(formato == "miles"){
    label_number(accuracy = 1,scale = 1/1000, suffix = "",prefix = "$", big.mark = ",")
  }else if(formato == "porcentaje"){
    label_number(accuracy = 0.1,scale = 100, suffix = "%",prefix = "", big.mark = ",")
  }
  return(formato)
  
}
Formatod3 <- function(formato){
  # Define el formato para un numero segun su tipo: coma, numero, dinero, porcentaje
  formato <- if(formato == "coma"){
    ","
  } else if(formato == "numero"){
    ",.3"
  } else if(formato == "dinero"){
    "$,"
  } else if(formato == "porcentaje"){
    ",.0%"
  }
  return(formato)
}
FormatoHOT <- function(formato){
  # Define el formato para un numero segun su tipo: coma, numero, dinero, porcentaje
  formato <- if(formato == "coma"){
    "0,0"
  } else if(formato == "numero"){
    "0,0.00"
  } else if(formato == "dinero"){
    "$0,0.00"
  } else if(formato == "porcentaje"){
    "%0,0.00"
  }
  return(formato)
}  
FormatoJS <- function(formato){
  # Define el formato para un numero segun su tipo: coma, numero, dinero, porcentaje
  formato <- if(formato == "coma"){
    'function(d){return d.toFixed(0).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  } else if(formato == "numero"){
    "function(d){return d.toFixed(2)}"
  } else if(formato == "dinero"){
    'function(d){return "$" + d.toFixed(0).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  } else if(formato == "dinero"){
    'function(d){m=d/1000; return "$" + m.toFixed(0).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  } else if(formato == "porcentaje"){
    "function(d){return (d*100).toFixed(1) + '%'}"
  }
  return(formato)
}

## Condiciones ----
labelObligatorio <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

## Impresion ----
### Funciones para el UI ----
### Funciones para el server ----
ImprimirGauge <- function(val, limites, rango, Directo, formato){
  
  Form = Formatod3(formato)
  limites <- if(Directo) limites else rev(limites)
  rango <- if(Directo) rango else rev(rango)
  
  plot_ly(
    value = val ,
    title = list(text = ""),
    type = "indicator",
    mode = "gauge+number",
    number = list(valueformat = Form),
    gauge = list(
      axis =list(range = list(NULL, max(rango)), 
                 tickformat = Form),
      bar = list(color = "black"),
      steps = list(
        list(range = c(rango[1], limites[1]), color = "#45B39D"),
        list(range = c(limites[1], limites[2]), color = "#EB984E "),
        list(range = c(limites[2], rango[2]), color = "#CD6155"))
    )) %>% 
    layout(autosize = T, margin = m, 
           xaxis = list(tickformat = Form, title=""),
           yaxis = list(tickformat = Form, title="")) %>% 
    config(displayModeBar=F)
}
ImprimirAnillo <- function(data, var_label, var_medida, funcion = "sum", fun_color){
  
  if (funcion == "n"){
    aux1 <- data %>% 
      group_by_at(var_label) %>% 
      summarise(Var = n()) %>% 
      mutate(Lab = !!parse_expr(var_label))
  } 
  else {
    aux1 <- data %>% 
      group_by_at(var_label) %>% 
      summarise(Var = !!parse_expr(paste0(funcion, "(", 
                                          var_medida,", na.rm = T)")))%>% 
      mutate(Lab = !!parse_expr(var_label))
  }
  
  aux1$Lab <- sapply(aux1$Lab, 
                     FUN = function(x) {paste(strwrap(x, width = 30), collapse = "<br>")})
  aux1$Lab <- fct_reorder(aux1$Lab, aux1$Var, max)
  
  if(fun_color == "ColoresPlots"){
    plot_ly(aux1) %>%
      add_pie(labels = ~Lab, values = ~Var, sort = T,
              type = 'pie', hole = 0.4, textposition = ~ValoresPlots(Var),
              marker = list(line = list(width = 2),
                            colors = ~ColoresPlots(Lab)))  %>%
      layout(margin = m,
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.07, 
                           font=list(size = 9,color = "black"))) %>%
      config(displayModeBar=F)
    
  } else if (fun_color == "ColoresPlots2"){
    plot_ly(aux1) %>%
      add_pie(labels = ~Lab, values = ~Var, sort = T,
              type = 'pie', hole = 0.4, textposition = ~ValoresPlots(Var),
              marker = list(line = list(width = 2),
                            colors = ~ColoresPlots2(Lab)))  %>%
      layout(margin = m,
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.07, 
                           font=list(size = 9,color = "black"))) %>%
      config(displayModeBar=F)
    
  }
  
}
ImprimirSerieDG <- function(data, vars, fecha, formato, titulo, titeje, colores, meta=NULL){
  
  require(xts)
  require(dygraphs)
  Form <- FormatoJS(formato)
  
  aux1 <- data %>% 
    mutate(Fecha = !!parse_expr(fecha)) %>% 
    complete(Fecha = seq.Date(as.Date(min(Fecha)), as.Date(max(Fecha)), by="month")) %>% 
    mutate_at(vars, list(~ifelse(is.na(.), 0, .))) %>% 
    select_at(c("Fecha", vars))
  
  don <- xts(x = aux1[,vars], order.by = aux1$Fecha)
  names(don) <- vars
  
  dygraph(don, main = titulo) %>%
    dyLimit(meta, label = "meta", labelLoc = "right", color = "steelblue",
            strokePattern = "solid") %>%
    dyAxis("x", drawGrid = FALSE,
           valueFormatter = 'function(d) { return moment(d).format("MMM-YY");}',
           axisLabelFormatter = 'function(d) { return moment(d).format("MMM-YY");}') %>%
    dyAxis("y", label = titeje, axisLabelWidth = 90,
           valueFormatter = Form, axisLabelFormatter = Form) %>%
    dyOptions(drawPoints = TRUE, pointSize = 2,strokeWidth = 2,
              digitsAfterDecimal = 0,
              colors = colores,
              gridLineColor = "#CCD1D1") %>%
    dyLegend(show = "follow", labelsSeparateLines = F) %>%
    dyRangeSelector(height = 50, strokeColor = "") %>%
    dyHighlight(highlightCircleSize = 3,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = T)
}
ImprimirProphet <- function(data, var, fecha, IC = 0.8, formato, titeje){
  require(xts)
  require(dygraphs)
  require(prophet)
  Form <- FormatoJS(formato)
  
  m <- data %>%
    select(ds =!!parse_expr(fecha),
           y = !!parse_expr(var)) %>%
    prophet(interval.width = IC, weekly.seasonality=TRUE, daily.seasonality=TRUE)
  
  future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)
  forecast <- predict(m, future)
  
  dyplot.prophet(m, forecast) %>% 
    dyAxis("x", drawGrid = FALSE,
           valueFormatter = 'function(d) { return moment(d).format("MMM-YY");}', 
           axisLabelFormatter = 'function(d) { return moment(d).format("MMM-YY");}') %>% 
    dyAxis("y", label = titeje, axisLabelWidth = 90, 
           valueFormatter = Form, axisLabelFormatter = Form) %>% 
    dyOptions(drawPoints = TRUE, pointSize = 2,strokeWidth = 2, 
              digitsAfterDecimal = 0,
              colors = c("black", "steelblue"),
              gridLineColor = "#CCD1D1") %>% 
    dyLegend(show = "follow", labelsSeparateLines = F) %>%
    dyRangeSelector(height = 50, strokeColor = "")
}


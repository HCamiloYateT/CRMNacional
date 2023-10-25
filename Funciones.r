# Funciones ----

## Paquetes ----
Loadpkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = TRUE)
}

## Cadenas de Caracteres ----

LimpiarNombres <- function(s){
  x <- trimws(str_to_upper(gsub("([\\s])\\1+","\\1",s, perl=T)))
  return(x)
}

LimpiarCadena <- function(x, rem_espacios = F, rem_numeros=T, rem_caresp =T, rem_acentos = T){
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
  x <- if(rem_espacios) gsub("\\s", "",x) else x
  x <- if(rem_numeros) gsub("\\d", "",x) else x
  x <- if(rem_caresp) gsub("[^[:alnum:][:space:]]", "",x) else x
  x <- if(rem_acentos) iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT') else x
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
                   
Saltos <- function(n=1){
  strrep('<br/>', n)
}
Espacios <- function(n=1){
  strrep('&emsp;', n)
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
  return(x)
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
  # Descripción: Crea una representación de una línea vertical en un gráfico.
  # Parámetros:
  #   - x: La posición en el eje x donde se desea trazar la línea vertical (por defecto: 0).
  #   - color: El color de la línea (por defecto: "red").
  # Valor de retorno:
  #   - Una lista con las propiedades necesarias para dibujar la línea vertical en un gráfico.
  
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
  # Descripción: Crea una representación de una línea horizontal en un gráfico.
  # Parámetros:
  #   - y: La posición en el eje y donde se desea trazar la línea horizontal (por defecto: 0).
  #   - color: El color de la línea (por defecto: "#ff3a21").
  # Valor de retorno:
  #   - Una lista con las propiedades necesarias para dibujar la línea horizontal en un gráfico.
  
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

DefinirFormato <- function(formato){
  require(scales)
  # Descripción: Define una función de  formato específico para los números.
  # Parámetros:
  #   - formato: El formato deseado para los números ("coma", "numero", "dinero", "miles" o "porcentaje").
  # Valor de retorno:
  #   - El objeto que define el formato para los números.
  
  formato <- if(formato == "coma"){
    label_number(accuracy = 1,scale = 1,suffix = "",prefix = "", big.mark = ",")
  } else if(formato == "numero"){
    label_number(accuracy = 0.01,scale = 1,suffix = "",prefix = "", big.mark = ",")
  } else if(formato == "dinero"){
    label_number(accuracy = 1,scale = 1,suffix = "",prefix = "$", big.mark = ",")
  } else if(formato == "miles"){
    label_number(accuracy = 1,scale = 1/1000, suffix = "",prefix = "$", big.mark = ",")
  }else if(formato == "porcentaje"){
    label_number(accuracy = 0.01,scale = 100, suffix = "%",prefix = "", big.mark = ",")
  }
  return(formato)
}
FormatoD3 <- function(formato){
  # Descripción: Define el formato de visualización de números en una gráfica utilizando la biblioteca D3.js.
  # Parámetros:
  #   - formato: El formato deseado para visualizar los números en la gráfica ("coma", "numero", "dinero" o "porcentaje").
  # Valor de retorno:
  #   - El formato correspondiente para visualizar los números en la gráfica utilizando D3.js.
  
  formato <- if(formato == "coma"){
    ",.0f"
  } else if(formato == "numero"){
    ",.2f"
  } else if(formato == "dinero"){
    "$,.0f"
  } else if(formato == "porcentaje"){
    ",.2%"
  }
  return(formato)
}
FormatoJS <- function(formato){
  # Descripción: Define el formato de visualización de números en JavaScript.
  # Parámetros:
  #   - formato: El formato deseado para visualizar los números ("coma", "numero", "dinero" o "porcentaje").
  # Valor de retorno:
  #   - El formato correspondiente en JavaScript para visualizar los números.
  
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
FormatoHOT <- function(formato){
  # Descripción: Define el formato para un número según su tipo: coma, número, dinero, porcentaje, para su uso en Handsontable (HOT).
  # Parámetros:
  #   - formato: El tipo de formato deseado. Puede ser "coma", "numero", "dinero" o "porcentaje".
  # Valor de retorno:
  #   - Una cadena de texto que representa el formato deseado para el número en Handsontable (HOT).
  
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
FormatearNumero <- function(x, formato, negrita = T, color = "#000000", meta=NA, prop=T) {
  # Descripción: Formatea un número según un formato específico, con opciones para resaltar en negrita y aplicar un color en función de una meta.
  # Parámetros:
  #   - x: El número a formatear.
  #   - formato: El formato deseado para el número ("coma", "numero", "dinero", "miles" o "porcentaje").
  #   - negrita: Un valor lógico que indica si se debe resaltar el número en negrita (por defecto es TRUE).
  #   - color: El color en formato hexadecimal para aplicar al número (por defecto es "#000000").
  #   - meta: Un vector numérico que representa la meta o umbral para aplicar un color diferente (opcional).
  #   - prop: Un valor lógico que indica si se deben generar colores proporcionales en función de la posición del número con respecto a la meta (por defecto es TRUE).
  # Valor de retorno:
  #   - El número formateado con las opciones seleccionadas y el formato especificado.
  
  require(RColorBrewer)
  require(tidyverse)
  require(shiny)
  
  form = DefinirFormato(formato)
  
  meta2 = c(-Inf, meta, Inf)
  pal <- ifelse(prop,
                colorRampPalette(c("#CB4335", "orange", "#138D75")),
                colorRampPalette(c("#138D75", "orange", "#CB4335"))
  )
  
  n = length(meta) + 1
  colors <- pal(n)
  col <- ifelse(is.null(meta), color, colors[sum(!x < meta2)])
  
  res <- ifelse(negrita, 
                paste0("<span style='font-weight: bold;color:", col,"'>", form(x), "</span>") %>% HTML,
                paste0("<span style='color:", col,"'>", form(x), "</span>") %>% HTML)
  
  return(res)
}
FormatearTexto <- function(x, negrita = T, color = "#000000", tamano_pct = 1, alineacion="left", transform="capitalize") {
  # Descripción: Formatea un texto con opciones para resaltar en negrita, aplicar un color, ajustar el tamaño de fuente, alinear el texto y transformar el texto en mayúsculas o minúsculas.
  # Parámetros:
  #   - x: El texto a formatear.
  #   - negrita: Un valor lógico que indica si se debe resaltar el texto en negrita (por defecto es TRUE).
  #   - color: El color en formato hexadecimal para aplicar al texto (por defecto es "#000000").
  #   - tamano_pct: El tamaño de la fuente en porcentaje (por defecto es 1, es decir, tamaño normal).
  #   - alineacion: La alineación del texto ("left", "center" o "right", por defecto es "left").
  #   - transform: La transformación del texto ("capitalize", "uppercase" o "lowercase", por defecto es "capitalize").
  # Valor de retorno:
  #   - El texto formateado con las opciones seleccionadas.
  
  require(shiny)
  neg <- paste0("font-weight:", ifelse(negrita, "bold", "normal"), ";")
  col <- paste0("color:", color, ";")
  tam <- paste0("font-size:", tamano_pct * 100, "%;")
  ali <- paste0("text-align:", alineacion, ";")
  tra <- paste0("text-transform:", transform, ";")
  
  res <- paste0("<span style='", neg, col, tam, ali, tra, "'>", x, "</span>") %>% HTML
  return(res)
}
col_kpi <- function(x, prop = T){
  # Descripción: Devuelve el color para un indicador según si es proporcional o no.
  # Parámetros:
  #   - x: El valor del indicador.
  #   - prop: Un valor lógico que indica si el indicador es proporcional (TRUE) o no (FALSE).
  # Valor de retorno:
  #   - El color correspondiente al indicador.
  
  if(prop){
    col = case_when(x == 0 ~ "#000000", 
                    x < 0 ~ "#943126",
                    x > 0 ~ "#0B5345"
    )} else {
      col = case_when(x == 0 ~ "#000000", 
                      x < 0 ~  "#0B5345",
                      x > 0 ~  "#943126", 
      )}
  return(col)
}
chr_kpi <- function(x){
  # Descripción: Retorna un carácter de crecimiento de un número dado.
  # Parámetros:
  #   - x: El número para el cual se desea obtener el carácter de crecimiento.
  # Valor de retorno:
  #   - Un carácter que representa el crecimiento del número dado.
  
  case_when(x == 0 ~ "▬",
            x < 0 ~ "▼",
            x > 0 ~ "▲")
}

## Condiciones ----
labelObligatorio <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

## Impresion ----
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

ImprimirSerieTiempo <- function(data, fecha, vars, pronostico, IC, colores, formato, titulo, titeje){
  
  # data: dataframe con la informacion a graficar
  # fecha: variable de fecha, ingresar en comillas
  # vars : columnas a graficar
  # Pronostico: indicador si se ejecuta o no forecast con prophet
  # IC: Intervalo de confianza de la esimacion de prophet
  # colores: vector de colores a graficar,k debe coincidir en tamaño con el numeor de columnas a graficar 
  # formato: formato del eje y de los textos
  # titutlo: titulo del gráfico
  # titeje: titulo del eje Y
  
  require(plotly)
  require(prophet)
  require(rlang)
  require(scales)
  
  aux1 <- data %>% 
    mutate(Fecha = !!parse_expr(fecha)) %>% 
    complete(Fecha = seq.Date(as.Date(min(Fecha)), as.Date(max(Fecha)), by="month")) %>% 
    mutate_at(vars, list(~ifelse(is.na(.), 0, .))) %>% 
    select_at(c("Fecha", vars))
  
  names(colores) = vars
  
  form1 = DefinirFormato(formato)
  form2 = Formatod3(formato)
  
  if(pronostico){
    
    lista <- names(aux1)[-1]
    pron <- do.call("bind_rows", lapply(lista, function(var){
      
      aux2 <- aux1 %>%
        select(ds = Fecha,
               y = all_of(var))
      
      m <- prophet(aux2, interval.width = IC, weekly.seasonality=F, daily.seasonality=F)
      future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)
      
      forecast <- predict(m, future) %>%
        mutate(Source = var) %>%
        left_join(aux2, by = "ds") %>%
        mutate(yhat = ifelse(is.na(y), yhat, NA),
               yhat_lower = ifelse(is.na(y), yhat_lower, NA),
               yhat_upper = ifelse(is.na(y), yhat_upper, NA)) %>% 
        select(Fecha = ds, Source, Real = y, LI = yhat_lower, Pronostico = yhat, LS = yhat_upper)
      
      return(forecast)
      
    }))
    
    plot_ly(pron, x=~Fecha) %>% 
      add_trace(y=~Real, color = ~Source, type = "scatter", mode="lines+markers",
                marker = list(size = 5), colors=colores,
                hoverinfo = "text", hoverlabel = list(pron = "left"),
                hovertext = paste0("<b>", FechaLarga(pron$Fecha), "</b>",
                                   "<br>", str_to_sentence(pron$Source), ": ", form1(pron$Real))) %>% 
      add_ribbons(ymin = ~LI, ymax = ~LS, color = ~Source, showlegend = F, alpha = 0.3,
                  hoverinfo = "text", hoverlabel = list(align = "left"),
                  hovertext = paste0("<b>", FechaLarga(pron$Fecha), "</b>",
                                     "<br>", "Límite inferior:  ", form1(pron$LI),
                                     "<br>", "Estimado:         ", form1(pron$Pronostico),
                                     "<br>", "Límite superior: ", form1(pron$LS))) %>% 
      add_lines(y=~Pronostico, color = ~Source, type = "scatter", mode="lines", name="Forecast",
                hoverinfo = "none", showlegend = F) %>% 
      layout(title = list(text=titulo, 
                          font=list(family = "Arial, sans-serif",size = 16,color = "black")),
             xaxis = list(title = ""),
             yaxis = list(title=titeje, tickformat = form2, visible=T, rangemode = "tozero",
                          title=list(text="", font= list(family = "Arial, sans-serif",
                                                         size = 16,color = "black")), 
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.05)) %>% 
      config(locale = "es",displayModeBar=F)
  } else {
    
    pron <- aux1 %>% 
      pivot_longer(vars, names_to = "Source", values_to = "Real")
    
    plot_ly(pron, x=~Fecha) %>% 
      add_trace(y=~Real, color = ~Source, type = "scatter", mode="lines+markers",
                marker = list(size = 5), colors=colores,
                hoverinfo = "text", hoverlabel = list(pron = "left"),
                hovertext = paste0("<b>", FechaLarga(pron$Fecha), "</b>",
                                   "<br>", str_to_sentence(pron$Source), ": ", form1(pron$Real))) %>% 
      layout(title = list(text=titulo, 
                          font=list(family = "Arial, sans-serif",size = 16,color = "black")),
             xaxis = list(title = ""),
             yaxis = list(title=titeje, tickformat = form2, visible=T, rangemode = "tozero",
                          title=list(text="", font= list(family = "Arial, sans-serif",
                                                         size = 16,color = "black")), 
                          tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.05)) %>% 
      config(locale = "es", displayModeBar=F)
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

#' CajaIco - Genera una caja con un ícono y texto en un entorno de Shiny
#'
#' Esta función genera una caja con un ícono y texto en un entorno de Shiny.
#'
#' @param texto El texto que se mostrará en la caja.
#' @param icono El ícono que se mostrará en la caja.
#' @param col_fondo El color de fondo de la caja.
#' @param col_letra (por defecto #17202A) El color del texto.
#' @param col_icono (por defecto #000000) El color del ícono.
#' @return Una caja con el ícono y el texto especificados.
#'
#' @examples
#' CajaIco("Ejemplo de caja con ícono", "fas fa-chart-bar", "#3498DB", "#FFFFFF", "#FFFFFF")
#'
CajaIco <- function(texto, icono, col_fondo, col_letra = "#17202A", col_icono = "#000000") {
  # Cargando la librería colorspace
  require(colorspace)
  
  # Definición de estilos para la caja
  s_caj <- paste0("display:block;background:", col_fondo, ";min-height:120px;width:100%;border-radius:10px;box-shadow:1px 1px 2px", darken(col_fondo, 0.1), ";")
  
  # Definición de estilos para el ícono
  s_ico <- paste0("position:absolute;display:block;float:left;height:100%;width:100%;text-align:left; font-size:80px;color:", adjust_transparency(col_icono, 0.08), ";background:transparent;z-index:1")
  
  # Definición de estilos para el contenido
  s_con <- "position:absolute;z-index:3;margin:0px;padding:5px 10px;margin-top:20px;margin-left:20px;background:transparent;"
  
  # Definición de estilos para el texto
  s_tex <- paste0("color:", col_letra, ";text-align:left;vertical-align:text-top;font-size:20px;font-weight:bold;")
  
  # Generación de la caja utilizando las especificaciones de estilo definidas
  column(4,
         div(style = s_caj,
             column(6, style = s_ico, icon(icono)), # Incorporando el ícono a la caja
             column(6, style = s_con,
                    tags$span(style = s_tex, texto)) # Incorporando el texto a la caja
         )
  )
}

#' CajaIma - Genera una caja con una imagen y texto en un entorno de Shiny
#'
#' Esta función genera una caja con una imagen y texto en un entorno de Shiny.
#'
#' @param texto El texto que se mostrará en la caja.
#' @param imagen La ruta de la imagen que se mostrará en la caja.
#' @param col_fondo El color de fondo de la caja.
#' @param col_letra (por defecto #17202A) El color del texto.
#' @param col_icono (por defecto #000000) El color de la imagen.
#' @return Una caja con la imagen y el texto especificados.
#'
#' @examples
#' CajaIma("Ejemplo de caja con imagen", "ruta_de_la_imagen.jpg", "#3498DB", "#FFFFFF", "#FFFFFF")
#'
CajaIma <- function(texto, imagen, col_fondo, col_letra = "#17202A", col_icono = "#000000") {
  # Cargando la librería colorspace
  require(colorspace)
  
  # Definición de estilos para la caja
  s_caj <- paste0("display:block;background:", col_fondo, ";min-height:120px;width:100%;border-radius:10px;box-shadow:1px 1px 2px", darken(col_fondo, 0.1), ";")
  
  # Definición de estilos para la imagen
  s_ima <- "position:absolute;display:block;z-index:1;opacity: 0.15;"
  
  # Definición de estilos para el contenido
  s_con <- "position:absolute;z-index:3;margin:0px;padding:5px 10px;margin-top:20px;margin-left:20px;background:transparent;"
  
  # Definición de estilos para el texto
  s_tex <- paste0("color:", col_letra, ";text-align:left;vertical-align:text-top;font-size:20px;font-weight:bold;")
  
  # Generación de la caja utilizando las especificaciones de estilo definidas
  column(4,
         div(style = s_caj,
             column(6, style = s_ima, 
                    HTML(paste("<img src=", imagen, "alt='Imagen'>"))
             ),
             column(6, style = s_con,
                    tags$span(style = s_tex, texto)
             )
         )
  )
}

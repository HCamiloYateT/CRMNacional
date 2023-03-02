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
  x<-trimws(str_to_upper(gsub("([\\s])\\1+","\\1",s, perl=T)))
  return(x)
}
Unir.Cadenas <- function(..., sep = " ", collapse = NULL, na.rm = F) {
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

## Numeros ----
sierror_0 <- function(x){
  ifelse(x %in% c(NaN, Inf, -Inf),  0, x)
}
Variacion <- function(ini, fin){
  # Calcula la variacion porcentual entre un nuero inicial y uno final
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
Moda <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

## Fechas ----
PrimerDia <- function(x){
  # Retorna el primer dia del mes de una fecha data
  x <- lubridate::floor_date(as.Date(x), unit = "month")
}
FechaLarga <- function(x){
  require(lubridate)
  require(stringr)
  # Retorna una fecha en formato mes completo y año en tipo titulo
  
  mes <- case_when(month(x)==1 ~ "Enero",
                   month(x)==2 ~ "Febrero",
                   month(x)==3 ~ "Marzo",
                   month(x)==4 ~ "Abril",
                   month(x)==5 ~ "Mayo",
                   month(x)==6 ~ "Junio",
                   month(x)==7 ~ "Julio",
                   month(x)==8 ~ "Agosto",
                   month(x)==9 ~ "Septiembre",
                   month(x)==10 ~ "Octubre",
                   month(x)==11 ~ "Noviembre",
                   month(x)==12 ~ "Diciembre",
  )
  x <- str_to_title(paste(mes, year(x)))
  return(x)
}
FechaCorta <- function(x){
  require(lubridate)
  require(stringr)
  # Retorna una fecha en formato mes abreviado y año en tipo titulo
  
  mes <- case_when(month(x)==1 ~ "Ene",
                   month(x)==2 ~ "Feb",
                   month(x)==3 ~ "Mar",
                   month(x)==4 ~ "Abr",
                   month(x)==5 ~ "May",
                   month(x)==6 ~ "Jun",
                   month(x)==7 ~ "Jul",
                   month(x)==8 ~ "Ago",
                   month(x)==9 ~ "Sep",
                   month(x)==10 ~ "Oct",
                   month(x)==11 ~ "Nov",
                   month(x)==12 ~ "Dic",
  )
  
  x <- str_to_title(paste(mes, format(x, "%y")))
  return(x)
}
Edad = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

## Manejo de Datos ----
TopAbsoluto <- function(data, var_recode, var_top, fun_Top, n=10, 
                        nom_var, lab_recodificar = "OTROS"){
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
           !!nom_var := !!parse_expr(paste0("ifelse(Seq<=n, ", 
                                            var_recode, ", '", 
                                            lab_recodificar, "'", ")"))) %>% 
    select(all_of(var_recode), all_of(nom_var))
  
  data <- datos %>%
    left_join(aux2, by = var_recode)
  return(data)
}
TopRelativo <- function(data, var_recode, var_top, fun_Top, pct_min=0.05, 
                        nom_var, lab_recodificar = "OTROS"){
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
           !!nom_var := !!parse_expr(paste0("ifelse(Pct>pct_min, ", 
                                            var_recode, ", '", 
                                            lab_recodificar, "'", ")"))) %>% 
    select(all_of(var_recode), all_of(nom_var))
  
  nivs <- c(aux2[,1], aux2[,2]) %>% unlist() %>% unique()
  
  data <- datos %>%
    left_join(aux2, by = var_recode) 
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
    ",.0"
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


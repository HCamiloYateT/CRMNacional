PreparaTabla <- function(tabla, dim, val, levs){
  require(rlang)
  require(tidyverse)
  aux1 <- CargarDatos(tabla) %>% 
    select(Fecha, !!parse_expr(dim), !!parse_expr(val)) %>% 
    mutate(across("Fecha", ymd),
           across(where(is.character), \(x) LimpiarCadena(x, rem_caresp = F, rem_acentos = F)),
           {{dim}} := !!parse_expr(paste0("factor(", dim, ")"))
    ) %>% 
    complete(!!parse_expr(dim), Fecha = seq.Date(floor_date(min(as.Date(Fecha)), unit = "year"), 
                                                 ceiling_date(fecha, unit = "year")-1, 
                                                 by = "month")) %>% 
    filter(year(Fecha) == year(fecha)) %>% 
    pivot_wider(names_from = all_of(dim), values_from = all_of(val)) %>% 
    select("Fecha", all_of(levs))
  return(aux1)
}
ImprimirHOT <- function(tabla, form){
  
  formato <- FormatoHOT(form)
  cols = ncol(tabla)
  tabla <- tabla %>% 
    mutate(Fecha = str_to_sentence(format(Fecha, '%B %Y')))
  
  aux1 <- tabla %>% 
    rhandsontable(rowHeaders = NULL, rowHeaderWidth = 200, stretchH = "all",   
                  width = "100%") %>% 
    hot_col(col = 1:1, readOnly = TRUE) %>% 
    hot_col(col = 2:cols, format = formato) %>% 
    hot_cols(fixedColumnsLeft = 1) %>%
    hot_rows(fixedRowsTop = 1)
  return(aux1)
  
}
ConvertirFecha <- function(texto){
  s_fecha <- paste(gsub("\\D", "", texto),
                   case_when(grepl("Enero", texto, ignore.case = T) ~ "01",
                             grepl("Febrero", texto, ignore.case = T) ~ "02",
                             grepl("Marzo", texto, ignore.case = T) ~ "03",
                             grepl("Abril", texto, ignore.case = T) ~ "04",
                             grepl("Mayo", texto, ignore.case = T) ~ "05",
                             grepl("Junio", texto, ignore.case = T) ~ "06",
                             grepl("Julio", texto, ignore.case = T) ~ "07",
                             grepl("Agosto", texto, ignore.case = T) ~ "08",
                             grepl("Septiembre", texto, ignore.case = T) ~ "09",
                             grepl("Octubre", texto, ignore.case = T) ~ "10",
                             grepl("Noviembre", texto, ignore.case = T) ~ "11",
                             grepl("Diciembre", texto, ignore.case = T) ~ "12"
                             ),
                   "01", sep="-"
                   ) %>% 
    as.Date
  return(s_fecha)
}
ConvertirHOT <- function(dat, dim, val, tabla){
  
  aux0 <- CargarDatos(tabla) %>% 
    mutate(Fecha = as.Date(Fecha)) %>% 
    rename(old = !!parse_expr(val)) %>% 
    full_join(dat %>%
                mutate(Fecha = ConvertirFecha(Fecha)) %>%
                pivot_longer(2:ncol(.), names_to = dim, values_to = "new") %>%
                filter(!is.na(new)),
              by = c("Fecha", dim)) %>% 
   mutate({{val}} := case_when(!is.na(new) ~ new, new != old ~ new,  T ~ old)) %>%
    select(-c(new, old))
  
  return(aux0)
  
}

## Modulos ----
CajaHOTableUI <- function(id){
  ns <- NS(id)
  tagList(
        fluidRow(column(1),
                 column(10, rHandsontableOutput(ns("Tabla"))),
                 column(1, actionBttn(inputId = ns("Guardar"), label = "Guardar",
                                      style = "unite", color = "danger",
                                      icon = icon("save")))
                 ),
        DTOutput(ns("Test"))
        )
}
CajaHOTable <- function(id, tabla, dim, val, levs, form, msj){
  moduleServer(id, function(input, output, session) {
    
    output$Tabla <- renderRHandsontable({
      PreparaTabla(tabla, dim, val, levs) %>%
        ImprimirHOT(form)
    })

    # output$Test <- renderDataTable({
    #   req(input$Tabla)
    #   hot_to_r(input$Tabla) %>%
    #     ConvertirHOT(dim, val, tabla)
    # })

    observeEvent(input$Guardar, {
      req(input$Tabla)
      hot_to_r(input$Tabla) %>% 
        ConvertirHOT(dim, val, tabla) %>% 
        SubirDatos(tabla)
      
      toastr_success(paste(str_to_sentence(msj), "actualizado exitosamente"))

    })
    
    
    
  })
}




  
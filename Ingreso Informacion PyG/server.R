function(input, output, session) { 
  
  ## Captura de Usuario ----
  output$Seccion <- reactive({
    # usuario <- session$user
    usuario = "hcyate"
    grupo <- Users[Users$username == usuario,]$NomGrupo
    returnedValue = case_when("Analítica" %in% grupo   ~ "Todas",
                              "Control Interno" %in%  grupo ~ "Todas",
                              "Talento Humano" %in%  grupo ~ "Talento Humano",
                              "Calidad" %in%  grupo ~ "Calidades",
                              "Mesa de Negocios" %in% grupo ~ "Todas",
                              )
    return(returnedValue)
  })
  outputOptions(output, "Seccion", suspendWhenHidden = FALSE)
  
  ## Informacion Financiera ----
  
  CajaHOTable("Margen", tabla = "ANPYGMARIN", dim = "Cliente", val = "ValMargen", 
              levs = c("TOTAL", "GRANDES", "MEDIANOS", "AL DETAL"), form = "dinero", 
              "Márgen de Industria Nacional")
  
  output$ClasProductos <- renderRHandsontable({
    
    aux1 <- CargarDatos("ANPYGCLPRO") %>%
      mutate(LinProCod = as.character(LinProCod)) %>% 
      arrange(LinProCod)
    
    rhandsontable(aux1, rowHeaders = NULL, rowHeaderWidth = 200, stretchH = "all",   
                  width = "100%", height = 300,
                  colHeaders = c("Id Línea de Producto", "Línea de Producto", 
                                 "Clasifiación PyG")) %>%
      hot_col(col = 1:2, readOnly = TRUE) %>%
      hot_cols(columnSorting = T, fixedColumnsLeft = 2) 
    
    
  })
  observeEvent(input$GuardarClasProductos, {
    req(input$ClasProductos)
    
    aux0 <- CargarDatos("ANPYGCLPRO") %>% rename(old = ProductoPyG) %>%  mutate(LinProCod = as.character(LinProCod))
    aux1 <- hot_to_r(input$ClasProductos) %>% rename(new = ProductoPyG)
    
    aux2 <- aux0 %>% full_join(aux1, by = c("LinProCod", "LinProNom")) %>%
      mutate(ProductoPyG = case_when(!is.na(new) ~ new,
                                     new != old ~ new,
                                     T ~ old)) %>%
      select(-c(old, new)) %>%
      SubirDatos("ANPYGCLPRO")
    
    toastr_success(paste("Clasificación de productos actualizada exitosamente"))
    
  })
  
  output$ValoresXsaco <- renderRHandsontable({
    
   aux1 <- CargarDatos("ANPYGCOSES") %>%
     filter(Fecha == input$FechaValores) %>%
     mutate(Producto = factor(Producto, c('EXCELSOS','SUPREMOS, DUNKIN Y MILLSTONE','PRODUCTO DE COLOMBIA',
                                  'PRACTICES','4C Y CRECER','CERTIFICADOS','REGIONALES','PERFIL DE TAZA',
                                  'FINCA','A LA MEDIDA','CONSUMOS Y PASILLAS SACOS DE 70KG', 'TRILLAS PARA TERCEROS'), ordered = T),
            Fecha = as.character(Fecha)) %>% 
      arrange(Producto)
    
    rhandsontable(aux1, rowHeaders = NULL, rowHeaderWidth = 200, stretchH = "all",  width = "100%",
                  colHeaders = c("Fecha", "Producto", 'BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA')) %>%
      hot_col(col = 1:2, readOnly = TRUE) %>%
      hot_col(col = 3:9, format = "$0,0") %>%
      hot_cols(fixedColumnsLeft = 1)
    
    
  })
  observeEvent(input$GuardarValoresXsaco, {
    req(input$ValoresXsaco)
    
    aux0 <- CargarDatos("ANPYGCOSES") 
    aux1 <- hot_to_r(input$ValoresXsaco) %>% 
      rename(VlrReco = ValorReconocimiento,
             VlrRech = CostoRechazo)
    
    aux2 <- aux0 %>% full_join(aux1, by = c("Fecha", "Item")) %>%
      mutate(ValorReconocimiento = case_when(!is.na(VlrReco) ~ VlrReco,
                                             VlrReco != ValorReconocimiento ~ VlrReco,
                                             T ~ ValorReconocimiento),
             CostoRechazo = case_when(!is.na(VlrRech) ~ VlrRech,
                                      VlrRech != CostoRechazo ~ VlrRech,
                                      T ~ CostoRechazo)
             ) %>%
      select(-c(VlrReco, VlrRech)) %>%
      SubirDatos("ANPYGCOSES")

    toastr_success(paste("Costo estándar actualizado exitosamente"))
    
  })
  
  output$MetaDespachos <- renderRHandsontable({
    
    levs_suc <- c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA', 'TOTAL')
    
    aux1 <- CargarDatos("ANPYGMETAS") %>%
      mutate(across(where(is.character), \(x) LimpiarCadena(x, rem_caresp = F, rem_acentos = F)),
             Sucursal = factor(Sucursal, levels = levs_suc, ordered = T)
      )
    
    aux2 <- aux1 %>%
      pivot_wider(names_from = Sucursal, values_from = Meta) %>%
      filter(Fecha == input$FechaMetas)
    
    rhandsontable(aux2, rowHeaders = NULL, rowHeaderWidth = 200, stretchH = "all") %>%
      hot_col(col = 1:2, readOnly = TRUE) %>%
      hot_col(col = 3:10, format = FormatoHOT("coma")) %>%
      hot_col(col = 1:1, format = "0", halign='htLeft') %>%
      hot_cols(fixedColumnsLeft = 2) %>%
      hot_rows(fixedRowsTop = 1)
    
  })
  observeEvent(input$GuardarMetaDespachos, {
    req(input$MetaDespachos)
    
    aux0 <- CargarDatos("ANPYGMETAS") %>%
      rename(old = Meta)
    
    aux1 <- hot_to_r(input$MetaDespachos) %>%
      pivot_longer(3:ncol(.), names_to = "Sucursal", values_to = "new") %>%
      filter(!is.na(new))
    
    aux2 <- aux0 %>% full_join(aux1, by = c("Fecha", "Item", "Sucursal")) %>%
      mutate(Meta = case_when(!is.na(new) ~ new, new != old ~ new,  T ~ old)) %>%
      select(-c(new, old)) %>%
      SubirDatos("ANPYGMETAS")
    
    toastr_success(paste("Metas actualizadas exitosamente"))
    
  })
  
  CajaHOTable("PresupuestoCompras", tabla = "ANPYGPTCOM", dim = "Sucursal", val = "ValPtoCompras",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
              form = "coma", "Presupuesto de Compras")
  
  CajaHOTable("KilosPuntoCompra", tabla = "ANPYGKLPC", dim = "Sucursal", val = "Kilos",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
              form = "coma", "Kilos Comprados en Punto de Compra")
  
  CajaHOTable("Extraordinarios", tabla = "ANPYGEXTRA", dim = "Sucursal", val = "Extraordinarios",
            levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
            form = "dinero", "Extraordinarios")
  
  CajaHOTable("OtrosCostos", tabla = "ANPYGOTCOS", dim = "Sucursal", val = "OtrosCostos",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
              form = "dinero", "Otros Costos")
  
  CajaHOTable("TrillasTerceros", tabla = "ANPYGTRTER", dim = "Sucursal", val = "ValTrillaTer",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
              form = "coma", "Trillas para Terceros - Excelsos")
  
  CajaHOTable("TrillasTercerosCop", tabla = "ANPYGTRTEC", dim = "Sucursal", val = "ValTrillaTer",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
              form = "coma", "Trillas para Terceros - Coproductos")
  
  CajaHOTable("TrasladosTrll", tabla = "ANPYGTRTRL", dim = "Sucursal", val = "Sacos",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
              form = "coma", "Sacos Trasladados")
  
  CajaHOTable("OpLogistica", tabla = "ANPYGOPLOG", dim = "Sucursal", val = "OpLogistica",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
              form = "porcentaje", "Indicador de Opotunidad Logística")
  
  CajaHOTable("OpLogisticaAcum", tabla = "ANPYGOLAC", dim = "Sucursal", val = "OpLogistica",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
              form = "porcentaje", "Indicador de Opotunidad Logística Acumulada")
  
  CajaHOTable("CostosViajes", tabla = "ANPYGCVIAJ", dim = "Sucursal", val = "ValViajes",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
              form = "dinero", "Costo de viaje")
  
  CajaHOTable("ConsumoEnergia", tabla = "ANPYGENERG", dim = "Sucursal", val = "KWEnergia",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA', 'ARENALES'),
              form = "coma", "Consumo de energía")
  
  CajaHOTable("CostoEnergia", tabla = "ANPYGENERG", dim = "Sucursal", val = "ValEnergia",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA', 'ARENALES'),
              form = "dinero", "Costo de Energía")
  
  CajaHOTable("CostoMovilizacion", tabla = "ANPYGMOVIL", dim = "Sucursal", val = "ValMovilizadores",
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'),
              form = "dinero", "Costo de Movilización")

  CajaHOTable("FinancierosArenales", tabla = "ANPYGAJFIN", dim = "Sucursal", val = "Ajuste",
              levs = c("ARENALES"),
              form = "dinero", "Financieros Arenales")

  output$Prespuestos <- renderRHandsontable({

    levs_suc <- c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA')
    
    aux1 <- CargarDatos("ANPYGPTCOS") %>%
      mutate(across(where(is.character), \(x) LimpiarCadena(x, rem_caresp = F, rem_acentos = F)),
             Sucursal = factor(Sucursal, levels = levs_suc, ordered = T)
             ) %>%
      arrange(Subgrupo)

    aux2 <- aux1 %>%
      pivot_wider(names_from = Sucursal, values_from = ValPtoCostos) %>% 
      filter(Fecha == input$FechaPtoCosto,
             Grupo == input$GrupoPtoCosto) %>% 
      arrange(Tipo, Subgrupo)

    rhandsontable(aux2, rowHeaders = NULL, rowHeaderWidth = 200, width = "100%", stretchH = "all") %>%
      hot_col(col = 1:3, readOnly = TRUE) %>%
      hot_col(col = 1:1, format = "0", halign='htLeft') %>%
      hot_col(col = 5:11, format = FormatoHOT("dinero")) %>%
      hot_cols(fixedColumnsLeft = 3) %>%
      hot_rows(fixedRowsTop = 1)
  })
  observeEvent(input$GuardarPrespuestos, {
    req(input$Prespuestos)

    aux0 <- CargarDatos("ANPYGPTCOS")

    aux1 <- hot_to_r(input$Prespuestos) %>%
      pivot_longer(5:ncol(.), names_to = "Sucursal", values_to = "new") %>%
      rename(TipoNew = Tipo) %>% 
      filter(!is.na(new))
    
    aux2 <- aux0 %>% full_join(aux1, by = c("Fecha", "Grupo", "Subgrupo", "Sucursal")) %>% 
      mutate(ValPtoCostos = case_when(!is.na(new) ~ new, new != ValPtoCostos ~ new,  T ~ ValPtoCostos),
             Tipo = case_when(!is.na(TipoNew) ~ TipoNew, TipoNew != Tipo ~ TipoNew,  T ~ Tipo)) %>% 
      select(-c(TipoNew, new)) %>%
      SubirDatos("ANPYGPTCOS")
      
   toastr_success(paste("Presupuesto actualizado exitosamente"))

  })

  
  ## Calidades ----

  output$Rechazos <- renderRHandsontable({
    
    levs_suc <- c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA')
    
    aux1 <- CargarDatos("ANPYGCALTR") %>%
      filter(Item != "TOTAL") %>% 
      select(-NumReclamos) %>% 
      mutate(across("Fecha", ymd),
             across(where(is.character), \(x) LimpiarCadena(x, rem_caresp = F, rem_acentos = F, rem_numeros = F))) %>% 
      complete(Sucursal, Item, Fecha = seq.Date(floor_date(min(as.Date(Fecha)), unit = "year"), 
                                                   ceiling_date(fecha, unit = "year")-1, 
                                                   by = "month"))
    
    aux2 <- aux1 %>% 
      pivot_wider(names_from = Sucursal, values_from = NumRechazos) %>% 
      filter(year(Fecha) == year(input$FechaRechazos), month(Fecha) == month(input$FechaRechazos)) %>% 
      mutate(Fecha = str_to_sentence(format(Fecha, '%B %Y')),
             Item = factor(Item, c('EXCELSOS','SUPREMOS, DUNKIN Y MILLSTONES','PRODUCTO DE COLOMBIA',
                                   'PRACTICES','4C Y CRECER','CERTIFICADOS', 'REGIONALES','PERFIL DE TAZA',
                                   'FINCA','A LA MEDIDA','CONSUMOS Y PASILLAS SACOS DE 70KG','TRILLAS A TERCEROS'), ordered = T)) %>% 
      select(Fecha, Item, all_of(levs_suc)) %>% 
      arrange(Item)
    
    
    rhandsontable(aux2, rowHeaders = NULL, rowHeaderWidth = 200, stretchH = "all") %>% 
      hot_col(col = 1:2, readOnly = TRUE) %>% 
      hot_col(col = 3:ncol(aux2), format = FormatoHOT("coma")) %>% 
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_rows(fixedRowsTop = 1)
    
  })
  output$RechazosArenales <- renderRHandsontable({
    
    levs_suc <- c('ARENALES')
    
    aux1 <- CargarDatos("ANPYGCALAR") %>% 
      filter(Item != "TOTAL") %>% 
      mutate(across("Fecha", ymd),
             across(where(is.character), \(x) LimpiarCadena(x, rem_caresp = F, rem_acentos = F)),
             Sucursal = factor(Sucursal, levels = levs_suc, ordered = T)
      ) %>% 
      select(-NumReclamos) %>% 
      pivot_wider(names_from = Sucursal, values_from = NumRechazos) %>% 
      complete(Item, Fecha = seq.Date(floor_date(min(as.Date(Fecha)), unit = "year"), 
                                                ceiling_date(fecha, unit = "year")-1, 
                                                by = "month"))
    
    aux2 <- aux1 %>% 
      filter(year(Fecha) == year(input$FechaRechazos), month(Fecha) == month(input$FechaRechazos)) %>% 
      mutate(Fecha = str_to_sentence(format(Fecha, '%B %Y')),
             Item = factor(Item, c("CONSUMO", "CONSUMO MEZCLA", "PASILLA MOLIDO", "PASILLA SOLUBLE",
                                   "RIPIO","TOTAL"), ordered = T)) %>% 
      relocate(Fecha) %>% 
      arrange(Item)
    
    rhandsontable(aux2, rowHeaders = NULL, rowHeaderWidth = 200, stretchH = "all") %>% 
      hot_col(col = 1:2, readOnly = TRUE) %>% 
      hot_col(col = 3:2, format = FormatoHOT("coma")) %>% 
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_rows(fixedRowsTop = 1)
    
  })
  observeEvent(input$GuardarRechazos, {
    
    req(input$Rechazos)
    
    aux0 <- CargarDatos("ANPYGCALTR") %>% 
      mutate(Fecha = as.Date(Fecha)) %>% 
      rename(old = NumRechazos)
    
    aux1 <- hot_to_r(input$Rechazos) %>% 
      mutate(Fecha = ConvertirFecha(Fecha)) %>%
      pivot_longer(3:ncol(.), names_to = "Sucursal", values_to = "new") %>%
      filter(!is.na(new))
    
    aux2 <- aux0 %>% full_join(aux1, by = c("Fecha", "Item", "Sucursal")) %>% 
      mutate(NumRechazos = case_when(!is.na(new) ~ new, new != old ~ new,  T ~ old)) %>%
      select(-c(new, old))
    
    SubirDatos(aux2, "ANPYGCALTR")
    
    aux0 <- CargarDatos("ANPYGCALAR") %>%
      mutate(Fecha = as.Date(Fecha)) %>%
      rename(old = NumRechazos)

    aux1 <- hot_to_r(input$RechazosArenales) %>%
      mutate(Fecha = ConvertirFecha(Fecha)) %>%
      pivot_longer(3:ncol(.), names_to = "Sucursal", values_to = "new") %>%
      filter(!is.na(new))

    aux0 %>% full_join(aux1, by = c("Fecha", "Item", "Sucursal")) %>%
      mutate(NumRechazos = case_when(!is.na(new) ~ new, new != old ~ new,  T ~ old)) %>%
      select(-c(new, old)) %>%
      SubirDatos("ANPYGCALAR")
    
    toastr_success(paste("Rechazos actualizados exitosamente"))
    
    
  })
  
  output$Reclamos <- renderRHandsontable({
    
    levs_suc <- c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA')
    
    aux1 <- CargarDatos("ANPYGCALTR") %>%
      filter(Item != "TOTAL") %>% 
      select(-NumRechazos) %>% 
      mutate(across("Fecha", ymd),
             across(where(is.character), \(x) LimpiarCadena(x, rem_caresp = F, rem_acentos = F, rem_numeros = F))) %>% 
      complete(Sucursal, Item, Fecha = seq.Date(floor_date(min(as.Date(Fecha)), unit = "year"), 
                                                ceiling_date(fecha, unit = "year")-1, 
                                                by = "month"))

    aux2 <- aux1 %>% 
      pivot_wider(names_from = Sucursal, values_from = NumReclamos) %>% 
      filter(year(Fecha) == year(input$FechaReclamos), month(Fecha) == month(input$FechaReclamos)) %>% 
      mutate(Fecha = str_to_sentence(format(Fecha, '%B %Y')),
             Item = factor(Item, c('EXCELSOS','SUPREMOS, DUNKIN Y MILLSTONES','PRODUCTO DE COLOMBIA',
                                   'PRACTICES','4C Y CRECER','CERTIFICADOS', 'REGIONALES','PERFIL DE TAZA',
                                   'FINCA','A LA MEDIDA','CONSUMOS Y PASILLAS SACOS DE 70KG','TRILLAS A TERCEROS'), ordered = T)) %>% 
      select(Fecha, Item, all_of(levs_suc)) %>% 
      arrange(Item)
    
    
    rhandsontable(aux2, rowHeaders = NULL, rowHeaderWidth = 200, stretchH = "all") %>% 
      hot_col(col = 1:2, readOnly = TRUE) %>% 
      hot_col(col = 3:ncol(aux2), format = FormatoHOT("coma")) %>% 
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_rows(fixedRowsTop = 1)
    
  })
  output$ReclamosArenales <- renderRHandsontable({
    
    levs_suc <- c('ARENALES')
    
    aux1 <- CargarDatos("ANPYGCALAR") %>% 
      filter(Item != "TOTAL") %>% 
      mutate(across("Fecha", ymd),
             across(where(is.character), \(x) LimpiarCadena(x, rem_caresp = F, rem_acentos = F)),
             Sucursal = factor(Sucursal, levels = levs_suc, ordered = T)
      ) %>% 
      select(-NumReclamos) %>% 
      pivot_wider(names_from = Sucursal, values_from = NumRechazos) %>% 
      complete(Item, Fecha = seq.Date(floor_date(min(as.Date(Fecha)), unit = "year"), 
                                      ceiling_date(fecha, unit = "year")-1, 
                                      by = "month"))

    
    aux2 <- aux1 %>% 
      filter(year(Fecha) == year(input$FechaReclamos), month(Fecha) == month(input$FechaReclamos)) %>% 
      mutate(Fecha = str_to_sentence(format(Fecha, '%B %Y')),
             Item = factor(Item, c("CONSUMO", "CONSUMO MEZCLA", "PASILLA MOLIDO", "PASILLA SOLUBLE",
                                   "RIPIO","TOTAL"), ordered = T)) %>% 
      relocate(Fecha) %>% 
      arrange(Item)
    
    
    rhandsontable(aux2, rowHeaders = NULL, rowHeaderWidth = 200, stretchH = "all") %>% 
      hot_col(col = 1:2, readOnly = TRUE) %>% 
      hot_col(col = 3:2, format = FormatoHOT("coma")) %>% 
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_rows(fixedRowsTop = 1)
    
  })
  observeEvent(input$GuardarReclamos, {
    
    req(input$ReclamosArenales)
    
    aux0 <- CargarDatos("ANPYGCALTR") %>% 
      mutate(Fecha = as.Date(Fecha)) %>% 
      rename(old = NumReclamos)
    
    aux1 <- hot_to_r(input$Reclamos) %>% 
      mutate(Fecha = ConvertirFecha(Fecha)) %>%
      pivot_longer(3:ncol(.), names_to = "Sucursal", values_to = "new") %>%
      filter(!is.na(new))
    
    aux2 <- aux0 %>% full_join(aux1, by = c("Fecha", "Item", "Sucursal")) %>% 
      mutate(NumReclamos = case_when(!is.na(new) ~ new, new != old ~ new,  T ~ old)) %>%
      select(-c(new, old))
    
    SubirDatos(aux2, "ANPYGCALTR")
    
    aux0 <- CargarDatos("ANPYGCALAR") %>%
      mutate(Fecha = as.Date(Fecha)) %>%
      rename(old = NumReclamos)
    
    aux1 <- hot_to_r(input$ReclamosArenales) %>%
      mutate(Fecha = ConvertirFecha(Fecha)) %>%
      pivot_longer(3:ncol(.), names_to = "Sucursal", values_to = "new") %>%
      filter(!is.na(new))
    
    aux0 %>% full_join(aux1, by = c("Fecha", "Item", "Sucursal")) %>%
      mutate(NumReclamos = case_when(!is.na(new) ~ new, new != old ~ new,  T ~ old)) %>%
      select(-c(new, old)) %>%
      SubirDatos("ANPYGCALAR")
    
    toastr_success(paste("Reclamos actualizados exitosamente"))
    
  })
  
  
  ## Talento Humano ----
  
  CajaHOTable("NumMovilizadores", tabla = "ANPYGMOVIL", dim = "Sucursal", val = "NumMovilizadores", 
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA', 'ARENALES'), 
              form = "coma", "Número de Movilizadores")
  
  CajaHOTable("PlantaPersonal", tabla = "ANPYGPERSO", dim = "Sucursal", val = "NumPersonal",  
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'), 
              form = "coma", "Número de planta de personal")
  
  CajaHOTable("NumHorasExtra", tabla = "ANPYGHOREX", dim = "Sucursal", val = "NumHorasExtra",  
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'), 
              form = "coma", "Número de horas extra")
  
  CajaHOTable("EmpHorasExtras", tabla = "ANPYGHOREX", dim = "Sucursal", val = "EmpHorasExtra",  
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'), 
              form = "coma", "Empleados con horas extra")
  
  CajaHOTable("ValHorasExtras", tabla = "ANPYGHOREX", dim = "Sucursal", val = "ValHorasExtra",  
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'), 
              form = "coma", "Valor de horas extras")
  
  CajaHOTable("NumAccidentes", tabla = "ANPYGACCID", dim = "Sucursal", val = "NumAccidentes",  
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'), 
              form = "coma", "Número de accidentes")
  
  CajaHOTable("DiasAccidentes", tabla = "ANPYGACCID", dim = "Sucursal", val = "DiasAccidentes",  
              levs = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA','PEREIRA','BUCARAMANGA','HUILA'), 
              form = "coma", "Días de ausencia por accidentes")


}

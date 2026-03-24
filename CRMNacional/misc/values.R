## Choices Listas Desplegables ------
Choices <- function() {
  
  clientes_data <- CargarDatos("CRMNALCLIENTE")
  productos <- CargarDatos("CRMNALPRODS") %>% 
    mutate(Categoria  =  ifelse(Categoria == "BLEND", "FUERA DE NORMA", Categoria))  
  origen_lead <- CargarDatos("CRMNALORILEAD") %>% filter(Estado== "A")
  aliados <- CargarDatos("CRMNALALIADOS") %>% filter(Estado== "A")
  estados <- CargarDatos("CRMNALESTCUENTA") %>% filter(Estado== "A")
  raz_interes <- CargarDatos("CRMNALRAZINTERES") %>% filter(Estado== "A")
  raz_descarte <- CargarDatos("CRMNALDESCARTE") %>% filter(Estado== "A")
  formapago <- ConsultaSistema("syscafe", "select distinct ForPagNom from NFORPAG")
  
  pais <- c("", read.csv("https://gist.githubusercontent.com/kalinchernev/486393efcca01623b18d/raw/daa24c9fea66afb7d68f8d69f0c4b8eeb9406e83/countries",
                             header = FALSE, col.names = "Pais") %>% pull(Pais) %>% str_to_upper())
  depto <- c("", Unicos(CargarDatos("ANDIVIPOLA") %>% pull(NomDep)))
  direccion_via <- c("", "AVENIDA", "CALLE", "CARRERA", "TRANSVERSAL", "DIAGONAL", "CIRCULAR", "CIRCUNVALAR", "KILOMETRO")
  direccion_cardinalidad <- c("","NORTE","SUR","ESTE","OESTE")
  
  list(
    personas = Unicos(clientes_data$Asesor),
    segmento = Unicos(clientes_data$Segmento),
    linneg = Unicos(productos$LinNeg),
    categoria = Unicos(c(productos$Categoria)),
    producto = Unicos(c(productos$Producto)),
    aliado = Unicos(aliados$Aliado),
    estadocuenta = Unicos(estados$EstCuenta),
    estadonegocio = Unicos(estados$EstNegocio),
    origen = Unicos(origen_lead$Origen),
    raz_interes = Unicos(raz_interes$RazInteres),
    raz_descarte = Unicos(raz_descarte$Razon),
    formapago = Unicos(formapago$ForPagNom),
    paises = pais,
    deptos = depto,
    direccion_via = direccion_via,
    direccion_cardinalidad = direccion_cardinalidad
    )
}

## Márgenes Plotly ----
m <- list(l = 50,r = 50,b = 5,t = 20)

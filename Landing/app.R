library(readxl)
library(tidyverse)

TarjetaArticuloNuevo <- function(x){
  
  img <- x$NomImg
  tit <- x$`Nombre App`
  des <- x$Descripción
  eti <- x$Tags
  
  out <- paste0("<div class='card-home card-reports-news'>",
                "<article class='card-home__wrapper card-reports-news__wrapper'>",
                "<figure class='card-home__image card-reports-news__image'>",
                "<img src='", img,"' alt='", tit,"' class='w-100'>",
                "</figure>",
                "<div class='card-home__copntent card-reports-news__content'>",
                "<p class='card-home__paragraph card-reports-news__paragraph'>",
                des,
                "</p>",
                "<h3 class='card-home__title card-reports-news__title'>",
                eti,
                "</h3>",
                "</div>",
                "</article>",
                "</div>"
  ) %>% HTML
  
  return (out)
  
  
  
}
ListaArticuloProx <- function(x){
  
  tit <- x$`Nombre App`
  eti <- x$Tags
  
  out <- paste0("<div class='card-home card-reports-next'>",
                "<article class='card-home__wrapper card-reports-next__wrapper d-flex'>",
                "<span class='card-home__icon card-reports-next__icon'></span>",
                "<div class='card-home__content card-reports-next__content'>",
                "<h2 class='card-home__title card-reports-next__title'>",
                tit,
                "</h2>",
                "<p class='card-home__subtitle card-reports-next__subtitle'>",
                eti,
                "</p>",
                "</div>",
                "</article>",
                "</div>") %>% HTML
  return (out)
}
ArticuloNoticia <- function(x){
  
  img <- x$NomImg
  tit <- x$Titulo
  con <- x$Contenido
  
  out <- paste0("<div class='card-home card-news col-sm-6 col-md-4'>",
                "<article class='card-home__wrapper card-news__wrapper'>",
                "<figure class='card-home__image card-news__image'>",
                "<img src='", img, "' alt='", tit, "' class='w-100'>",
                "</figure>",
                "<div class='card-home__content card-news__content'>",
                "<h3 class='card-home__title card-news__title'>",
                tit,
                "</h3>",
                "<p class='card-home__paragraph card-news__paragraph' style='text-align: justify;'>",
                con,
                "</p>",
                "</div>",
                "</article>",
                "</div>") %>% HTML
  return (out)
}

## Apps
data <- read_excel("Aplicaciones.xlsx")
# Nuevas
new_apps <- data %>% 
  filter(Estado == "Nuevo") %>% 
  mutate(NomImg = paste0("./assets/img-dist/", NomImg, ".png")) %>% 
  split(seq(nrow(.)))
# Proximas
next_apps <- data %>% 
  filter(Estado == "Próximo") %>% 
  mutate(NomImg = paste0("./assets/img-dist/", NomImg, ".png")) %>% 
  split(seq(nrow(.)))
izq <- next_apps[c(seq(1, length(next_apps), 2))] 
der <- next_apps[c(seq(2, length(next_apps), 2))] 

## Noticias
noticias <- read_excel("Noticias.xlsx") %>% 
  filter(Incluir == 1) %>% 
  mutate(NomImg = paste0("./assets/img-dist/", Imagen, ".png"),
         Contenido = gsub("\\. ", ".<br><br>", Contenido)) %>% 
  split(seq(nrow(.)))

# APP
shinyApp(
  ui = tagList(
    tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo2.png")),
    htmlTemplate("index.html",
                 news = lapply(new_apps, function(x) TarjetaArticuloNuevo(x)),
                 prox_col_1 = lapply(izq, function(x) ListaArticuloProx(x)),
                 prox_col_2 = lapply(der, function(x) ListaArticuloProx(x)),
                 noticias = lapply(noticias, function(x) ArticuloNoticia(x))
                 )
    ),
  server = function(input, output){}
)
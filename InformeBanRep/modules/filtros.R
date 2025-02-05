# Filtos ----

festivos <- CargarDatos("ANFESTIVOS") %>% 
  mutate(Fecha = ymd(Fecha)) %>% 
  group_by(Fecha) %>% 
  summarise(Lugar = paste(Lugar, collapse = ","))

cal <-  create.calendar(name = "mycal", weekdays=c("saturday", "sunday"), holidays = festivos$Fecha, 
                        start.date = as.Date("2000-01-01"), end.date = as.Date("2025-12-31"), 
                        adjust.to = adjust.next, adjust.from = adjust.next)

fecha_ej <- bizdays::offset(Sys.Date(), -1, cal) %>% as.Date()

filtros <- fixedPanel(top = 20, left = "auto", right = 20, bottom = "auto",
                      shinyWidgets::dropdown(
                        dateInput("Fecha", "Seleccione la Fecha", 
                                  value = fecha_ej, min = as.Date("2022-01-01"),
                                  max = Sys.Date(), language = "es",
                                  format = "dd - MM - yyyy"
                                  ),style = "pill", icon = icon("filter"), size = "lg",
                        status = "danger", width = "300px",right = T, height = "600px",
                        animate = animateOptions(
                          enter = animations$fading_entrances$fadeInRight,
                          exit = animations$fading_exits$fadeOutRightBig)),
                      style = "z-index: 10;"
                      )

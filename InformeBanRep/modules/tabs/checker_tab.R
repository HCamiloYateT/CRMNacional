checker_tab <- tabPanel("Control de Envio de Informe",
                        h4("Control de envio del Informe"),
                        fluidRow(
                          column(12, dataTableOutput("Control", width = "100%"))
                          ),
                        h6(paste0("*Fechas en formato: AAAA-MM-DD", br(), " Tiempo en formato HH:MM:SS (24 horas)") %>% HTML)
                        )
source("misc/info_fina.R", encoding = "UTF-8")
source("misc/calidades.R", encoding = "UTF-8")
source("misc/tal_humano.R", encoding = "UTF-8")

body_tab <- tabPanel("",
                     conditionalPanel(
                       condition = "output.Seccion == 'Todas'",
                       info_fina
                       ),
                     conditionalPanel(
                       condition = "output.Seccion ==  'Calidades' || output.Seccion ==  'Todas'",
                       calidades
                       ),
                     conditionalPanel(
                       condition = "output.Seccion ==  'Talento Humano' || output.Seccion ==  'Todas'",
                       tal_humano
                       )
                     )

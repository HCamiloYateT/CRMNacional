footer <- bs4DashFooter(
  left = div(
    style = "display:flex; align-items:center; margin:0;",
    tags$span(style = "margin-left:5px;", uiOutput("last_update_info"))
  ),
  right = div(
    style = "display:flex; align-items:center; margin:0;",
    FormatearTexto(
      str_to_title(paste(tit_app, "Versión 2.0")),
      tamano_pct = 0.6
    )
  ),
  fixed = TRUE
)
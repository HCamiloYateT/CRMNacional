footer <- bs4DashFooter(
  left = div(
    tags$a(style = paste("display:flex;align-items:center;", "gap:14px;padding:8px 12px;cursor:default;"),
           tags$span(style = "margin-left:5px;", 
                     uiOutput("last_update_info")
                     )
           )
  ),
  right = tags$img(
    src = "https://raw.githubusercontent.com/HCamiloYateT/Compartido/main/img/logo.png",
    style = "height:30px;"
    ),
  fixed = TRUE
)
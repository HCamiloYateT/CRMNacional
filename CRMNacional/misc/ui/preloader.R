preloader <- list(
  html = tagList(    
    tags$div(
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100vh;",
      tags$img(src = "logo2.png", width = "80px", style = "margin-bottom: 20px;"),
      tags$h3("Cargando...",
              style = "color: black; font-size: 10pt; font-weight: bold;"),
      tags$div(class = "spinner-border text-danger", role = "status")
    )
  ),
  color = "#e5e7e9"
)

preloader2 <- list(
  html = tagList(    
    tags$div(
      style ="
        width: 300px;
        height: 200px;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        background: white;
        border-radius: 16px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.15);
        z-index: 9999;
      ",
      tags$img(src = "logo2.png", width = "80px", style = "margin-bottom: 20px;"),
      tags$h3("Actualizando...",
              style = "color: black; font-size: 10pt; font-weight: bold;"),
      tags$div(class = "spinner-border text-danger", role = "status")
    )
  ),
  color = "transparent"
)
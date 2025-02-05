## Estilos Datatables ----
js_estilo <- JS(
  "function(settings, json) {",
  "$('th').css({'padding': '1px', 'color': 'black', 'border-top':'2px solid #000', 
  'border-bottom':'2px solid #000', 'fontWeight':'900', 'font-size': '11px'});",
  "$('td').css({'padding': '1px', 'border-style':'none', 'font-size': '13px'});",
  "}")

js_rows <- JS("
          function( row, data, index ) {
            if (data[0].toUpperCase() === 'TOTAL' && data[0].match(/[a-z]/i)) {
              $(row).css({'background-color':'#EBEDEF', 
              'fontWeight':'900', 'color':'#000'});
            } else {
            $(row).css({'background-color':'#FDFEFE', 
              'color':'#1B2631'});
            }
          }")
js_callback <- JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Filtrar" )')
lang <- list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/es-ES.json')

## Márgenes Plotly ----
m <- list(l = 50,r = 50,b = 5,t = 20)
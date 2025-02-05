Sys.setlocale("LC_TIME", "es_ES.UTF-8")
# Librerias ----

library(shiny)
library(bslib)
library(shinyBS)
library(shinyjs)
library(shinybusy)
library(shinyWidgets)
library(colourpicker)
library(qrcode)
library(dplyr)

temp <- "
BEGIN:VCARD
VERSION:3.0
N:%s;%s;;;
FN:%s - Racafé
ORG: Racafé
TITLE:%s
TEL;WORK;VOICE:+57 %s
EMAIL;WORK;INTERNET:%s
END:VCARD
"


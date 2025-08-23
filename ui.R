library(tidyverse)
library(readxl)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shiny)
library(shinyFeedback)
library(DT)
library(shinyfullscreen)
library(shinyauthr)
library(shinymanager)
library(shinythemes)
library(tools)
library(patchwork)
library(scales)

source('header.R')
source('sidebar.R')
source('body.R')

# secure_app(
#   theme    = shinytheme("flatly"),
#   language = "es",
#   id       = "auth",
#
#   tags_top = tags$div(
#     tags$h4("DIIE app", style = "align:center;color:#1e4a75;font-weight:bold;"),
#     tags$img(src = "logo.png", width = 250)
#   ),
#
#   tags_bottom = tags$div(
#     tags$p("Cualquier pregunta, por favor contacte al ",
#       tags$a(
#         href   = str_c("mailto:", contact_email, "?Subject=DIIE%20app%20autenticación"),
#         target = "_top",
#         "administrator"
#       )
#     )
#   ),
#   lan = use_language("es"),

  dashboardPage(header, sidebar, body, title = 'DIIE app')
# )

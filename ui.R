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


source('contact/contact.R')
source('modules/CNGAE_2024.R')
source('modules/CNGAE_2025.R')

# secure_app(
#   theme    = shinytheme("flatly"),
#   language = "es",
#   id       = "auth",
#   tags_top = tags$div(
#     tags$h4(
#       "DIIE app",
#       style = "align:center;color:#1e4a75;font-weight:bold;"
#     ),
#     tags$img(
#       src = "logo.png", width = 250
#     )
#   ),
#   tags_bottom = tags$div(
#     tags$p(
#       "Cualquier pregunta, por favor contacte al ",
#       tags$a(
#         href = str_c(
#           "mailto:", contact_email, "?Subject=DIIE%20app%20autenticación"
#         ),
#         target ="_top", "administrator"
#       )
#     )
#   ),
  # lan = use_language("es"),
  dashboardPage(


# Header --------------------------------------------------------

    dashboardHeader(
      title = tags$div(
        style = "
        display: flex;
        align-items: center;
        padding-left: 17px;
        ",
        tags$img(
          src    = "logo_dashboardheader.png",
          height = "40px",
          style  = "margin-right: 10px;"
        ),
        tags$span(
          "DIIE app",
          style = "
          font-size: 16px;
          font-weight: bold;
          "
        ),
        tags$head(
          tags$title("DIIE app")
        )
      ),
      titleWidth = 200,
      tags$li(
        class = "dropdown",
        style = "display: flex; align-items: center; height: 50px;",
        actionBttn("page_full", style = "stretch", icon("maximize"))
      ),
      dropdownMenu(
        type        = "messages",
        badgeStatus = NULL,
        headerText  = "Administrador",
        messageItem(
          from    = contact_name,
          message = contact_email,
          icon    = icon("user-gear"),
          href    = str_c("mailto:", contact_email, "?Subject=DIIE%20app")
        )
      )
    ),


# Sidebar -------------------------------------------------------

    dashboardSidebar(
      width = 150,
      HTML(str_c("<br><br><br><br><br><br><br>")),
      sidebarMenu(
        menuItem(
          "Analítica",
          tabName = "analysis",
          icon = icon("chart-simple"),
          menuItem("CNGAE 2025", tabName = "CNGAE_analytics_2025")
          # menuItem("CNGAE 2024", tabName = "CNGAE_analytics_2024")
        )
      )
    ),


# Body ----------------------------------------------------------

    dashboardBody(
      tags$head(tags$style(HTML(
        ".content-wrapper {
        background-color: #FFFFFF;
        }
      /* Delete focus */
      :focus {
        outline: 0 !important;
        box-shadow: none !important;
      }
      .nav-tabs-custom {
        box-shadow: none !important;
        border: none !important;
      }
      .btn_custom_interno_1 {
        background-color: #3c8dbc;
        color: white;
      }
      .btn_custom_interno_2 {
        background-color: navy;
        color: white;
      }
      .skin-blue .main-header .logo {
          background-color: #3c8dbc !important;
          color: white !important;
        }
        "
      ))),
      fullscreen_all(click_id = "page_full"),
      # Analítica
      tabItems(
        CNGAE_2024_UI('id_CNGAE_2024'),
        CNGAE_2025_UI('id_CNGAE_2025')
      )
    )
  )
# )

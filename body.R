# shiny dashboardBody
source('modules/CNGAE_2024.R')
source('modules/CNGAE_2025.R')

body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),

  fullscreen_all(click_id = "page_full"),

  tabItems(
    CNGAE_2024_UI('id_CNGAE_2024'),
    CNGAE_2025_UI('id_CNGAE_2025')
  )
)

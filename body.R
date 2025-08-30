# shiny dashboardBody
source('modules/CNGAE_2024.R')
source('modules/CNGAE_2025.R')

body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),

  useShinyjs(),

  tags$script(HTML("
      Shiny.addCustomMessageHandler('exitFullScreen', function(message) {
        if (document.fullscreenElement) {
          document.exitFullscreen();
        }
      });
    ")),


  tabItems(
    CNGAE_2024_UI('id_CNGAE_2024'),
    CNGAE_2025_UI('id_CNGAE_2025')
  )
)

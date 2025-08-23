# shiny dashboardSidebar
sidebar <- dashboardSidebar(
  width = 150,
  HTML(str_c("<br><br><br><br><br><br><br>")),

  sidebarMenu(
    menuItem(
      "Analítica",
      tabName       = "analysis",
      icon          = icon("chart-simple"),
      startExpanded = TRUE,

      menuItem("CNGAE 2024", tabName = "CNGAE_analytics_2024"),
      menuItem("CNGAE 2025", tabName = "CNGAE_analytics_2025")
    )
  )
)

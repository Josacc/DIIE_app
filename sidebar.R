# shiny dashboardSidebar

source('modules/evaluation/module_evaluation.R')


sidebar <- dashboardSidebar(
  width = 160,
  HTML(str_c("<br><br><br><br><br><br><br>")),

  sidebarMenu(
    menuItem(
      "Analítica",
      tabName       = "analysis",
      icon          = icon("chart-simple"),
      startExpanded = TRUE,

      menuItem("Integración 2024", tabName = "CNGAE_analytics_2024"),

      menuItem("Integración 2025", tabName = "CNGAE_analytics_2025")
    )
  ),
  br(),

  sidebarMenu(
    menuItem(
      'Evaluación',
      tabName       = 'CNGAE_operative_2023',
      icon          = icon('chart-simple'),
      startExpanded = TRUE,

      menuItem('Citas & Evolución',   tabName = 'CNGAE_operative_citas_2023'),

      menuItem('Recuperación',        tabName = 'CNGAE_operative_recupera_2023'),

      menuItem('Etapas & Revisiones', tabName = 'CNGAE_operative_revision_2023')
    )
  )

)

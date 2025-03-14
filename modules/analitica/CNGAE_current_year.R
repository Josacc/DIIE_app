# 'CNGAE_current_year' module ---------------------------------------------

source('modules/analitica/CNGAE_current_year/top_ten.R')

CNGAE_current_year_UI <- function(id) {
  tabItem(
    tabName = "projects",
    navbarPage(
      title       = "Projects",
      selected    = "Text mining",
      collapsible = T,

      # "Info" module
      info_projects_UI(NS(id, "id_module_info_projects")),
      # "DIIE app" module
      text_mining_UI(NS(id, "id_module_text_mining")),
      # "One-way ANOVA" module
      one_way_anova_UI(NS(id, "id_module_one_way_anova")),
      # "Statistical analysis" module
      pharmacokinetics_UI(NS(id, "id_module_pharmacokinetics")),
      # "Markov process" module
      markov_process_UI(NS(id, "id_module_markov_process"))
    )
  )
}

CNGAE_current_year_Server <- function(id) {
  moduleServer(id, function(input, output, session) {

    info_projects_Server("id_module_info_projects")
    one_way_anova_Server("id_module_one_way_anova")
    text_mining_Server("id_module_text_mining")
    pharmacokinetics_Server("id_module_pharmacokinetics")
    markov_process_Server("id_module_markov_process")

  })
}










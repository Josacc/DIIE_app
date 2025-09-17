# 'CNGAE_current_year' module ---------------------------------------------

module_files_integration_2025 <- list.files(path = 'modules/integration/CNGAE_2025', pattern = 'module(.+)\\.R$', full.names = TRUE)
invisible(map(module_files_integration_2025, source))

source('modules/integration/functions/others_functions.R')
source("data/data_2025_year.R")


CNGAE_2025_UI <- function(id) {

  ns <- NS(id)

  tabItem(
    "CNGAE_analytics_2025",

    fluidRow(
      box(
        solidHeader = TRUE,
        width       = 12,

        tabsetPanel(
          id       = ns('id_navbar_analytics'),
          selected = "Observaciones",

          tabPanel(
            title = tags$b('Análisis y seguimiento'),
            br(),
            info_analitica_UI(ns('id_info_analitica'))
          ),

          tabPanel(
            title = "Observaciones",
            icon  = icon("square-poll-vertical"),
            br(),
            top_ten_UI(ns('id_top_ten'))
          ),

          tabPanel(
            title = "Revisión OC",
            icon  = icon("square-poll-vertical"),
            br(),
            revision_oc_UI(ns('id_revision_oc'))
          ),

          tabPanel(
            title = "Firma y sello (1)",
            icon  = icon("square-poll-vertical"),
            br(),
            proceso_firma_sello1_UI(ns('id_proceso_firma_sello1'))
          ),

          tabPanel(
            title = "Interno",
            icon  = icon("square-poll-vertical"),
            br(),
            interno_UI(ns('id_module_interno'))
          )
        )
      )
    )
  )
}


CNGAE_2025_Server <- function(id) {
  moduleServer(id, function(input, output, session) {


    data_and_update <- mod_integration$data_and_update
    team_data       <- mod_integration$team_data


    data <- reactive({

      data_path    <- 'historial_seguimiento/xIktan_20250511091935479_reporteSegumiento.xlsx'
      raw_data     <- data_and_update(data_path)
      database     <- raw_data$data_base
      update       <- raw_data$update
      database_obs <- team_data(reviewer_team, database) %>% filter(`Cantidad de obs` > 0)

      return(list(database, database_obs, update))

    })

    info_analitica_Server('id_info_analitica')

    top_ten_Server('id_top_ten', data)

    revision_oc_Server('id_revision_oc', data)

    proceso_firma_sello1_Server('id_proceso_firma_sello1', data)

    interno_Server('id_module_interno', data)

  })

}

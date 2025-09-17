# 'CNGAE_current_year' module ---------------------------------------------

module_files_integration_2024 <- list.files(path = 'modules/integration/CNGAE_2024', pattern = 'module(.+)\\.R$', full.names = TRUE)
invisible(map(module_files_integration_2024, source))

source("data/data_2024_year.R")


mod_integration <- env()
function_file_integration <- list.files('modules/integration/functions/', pattern = '\\.R$', full.names = TRUE)

map(function_file_integration, ~sys.source(., envir = mod_integration))


CNGAE_2024_UI <- function(id) {

  ns <- NS(id)

  tabItem(
    "CNGAE_analytics_2024",

    fluidRow(
      box(
        width       = 12,
        solidHeader = TRUE,

        tabsetPanel(
          id       = ns('id_navbar_analytics'),
          selected = 'Observaciones',

          tabPanel(
            title = tags$b('Análisis y seguimiento'),
            br(),
            info_analitica_UI_2024(ns('id_info_analitica'))
          ),

          tabPanel(
            title = "Observaciones",
            icon  = icon("square-poll-vertical"),
            br(),
            top_ten_UI_2024(ns('id_top_ten'))
          ),

          tabPanel(
            title = "Revisión OC",
            icon  = icon("square-poll-vertical"),
            br(),
            revision_oc_UI_2024(ns('id_revision_oc'))
          ),

          tabPanel(
            title = "Firma y sello (1)",
            icon  = icon("square-poll-vertical"),
            br(),
            proceso_firma_sello1_UI_2024(ns('id_proceso_firma_sello1'))
          ),

          tabPanel(
            title = "Interno",
            icon = icon("square-poll-vertical"),
            br(),
            interno_UI_2024(ns('id_module_interno'))
          )
        )
      )
    )
  )
}

CNGAE_2024_Server <- function(id) {
  moduleServer(id, function(input, output, session) {


    data_and_update <- mod_integration$data_and_update


    data <- reactive({

      data_path    <- 'historial_seguimiento/xIktan_20241119120144449_reporteSegumiento.xlsx'
      raw_data     <- data_and_update(data_path, year = 2024)
      database     <- raw_data$data_base
      update       <- raw_data$update
      database_obs <- team_data(reviewer_team, database) %>% filter(`Cantidad de obs` > 0)

      return(list(database, database_obs, update))

    })

    info_analitica_Server_2024('id_info_analitica')

    top_ten_Server_2024('id_top_ten', data)

    revision_oc_Server_2024('id_revision_oc', data)

    proceso_firma_sello1_Server_2024('id_proceso_firma_sello1', data)

    interno_Server_2024('id_module_interno', data)

  })
}

# 'CNGAE_current_year' module ---------------------------------------------

source('modules/CNGAE_2024/module_info_analitica.R')
source('modules/CNGAE_2024/module_top_ten.R')
source('modules/CNGAE_2024/module_revision_oc.R')
source('modules/CNGAE_2024/module_proceso_firma_sello1.R')
source('modules/CNGAE_2024/module_interno.R')
source('modules/CNGAE_2024/module_actualizacion.R')
source("data/data_2024_year.R")

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

    data <- reactive({
      data_path <- 'historial_seguimiento/xIktan_20241119120144449_reporteSegumiento.xlsx'
      raw_data  <- data_and_update(data_path, year = 2024)
      database     <- raw_data[[1]]
      update       <- raw_data[[2]]
      database_obs <- team_data(reviewer_team, database) %>%
        filter(`Cantidad de obs` > 0)

      return(list(database, database_obs, update))
    })

    info_analitica_Server_2024('id_info_analitica')
    top_ten_Server_2024('id_top_ten', data)
    revision_oc_Server_2024('id_revision_oc', data)
    proceso_firma_sello1_Server_2024('id_proceso_firma_sello1', data)
    interno_Server_2024('id_module_interno', data)

  })
}

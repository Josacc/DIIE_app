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
        solidHeader = TRUE,
        width       = 12,
        tabBox(
          id    = ns('id_navbar_analytics'),
          title = tags$p('Análisis y seguimiento', style = 'color: #3c8dbc;'),
          width = 12,
          tabPanel(
            title = "Info",
            icon  = icon("info-circle"),
            info_analitica_UI(ns('id_info_analitica'))
          ),
          tabPanel(
            title = "Observaciones",
            icon  = icon("square-poll-vertical"),
            top_ten_UI(ns('id_top_ten'))
          ),
          tabPanel(
            title = "Revisión OC",
            icon  = icon("square-poll-vertical"),
            revision_oc_UI(ns('id_revision_oc'))
          ),
          tabPanel(
            title = "Firma y sello (1)",
            icon  = icon("square-poll-vertical"),
            proceso_firma_sello1_UI(ns('id_proceso_firma_sello1'))
          ),
          tabPanel(
            title = "Interno",
            icon = icon("square-poll-vertical"),
            interno_UI(ns('id_module_interno'))
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
      raw_data  <- data_and_update(data_path)
      database     <- raw_data[[1]]
      update       <- raw_data[[2]]
      database_obs <- team_data(reviewer_team, database) %>%
        filter(`Cantidad de obs` > 0)

      return(list(database, database_obs, update))
    })

    # info
    info_analitica_Server('id_info_analitica')
    # observaciones
    top_ten_Server('id_top_ten', data)
    # revisión oc
    revision_oc_Server('id_revision_oc', data)
    # en proceso de firma y sello (1)
    proceso_firma_sello1_Server('id_proceso_firma_sello1', data)
    # interno
    interno_Server('id_module_interno', data)
    # actualización
    actualizacion_Server("id_module_actualizacion", data)

  })
}

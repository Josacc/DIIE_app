# 'CNGAE_current_year' module ---------------------------------------------

source('modules/CNGAE_2025/module_info_analitica.R')
source('modules/CNGAE_2025/module_top_ten.R')
source('modules/CNGAE_2025/module_revision_oc.R')
source('modules/CNGAE_2025/module_proceso_firma_sello1.R')
source('modules/CNGAE_2025/module_interno.R')
source('modules/CNGAE_2025/module_actualizacion.R')
source("data/data_2025_year.R")

CNGAE_2025_UI <- function(id) {
  tabItem(
    "CNGAE_analytics_2025",
    navbarPage(
      title       = "Análisis y seguimiento",
      selected    = "Top 10",
      collapsible = TRUE,
      # 'Info analitica' module
      info_analitica_UI(NS(id, 'id_info_analitica')),
      # Observaciones
      navbarMenu(
        "Observaciones",
        icon = icon("square-poll-vertical"),
        # 'top ten' module
        top_ten_UI(NS(id, 'id_top_ten'))
      ),
      # Cuestionarios
      navbarMenu(
        "Cuestionarios",
        icon = icon("square-poll-vertical"),
        # 'revisión oc' module
        revision_oc_UI(NS(id, 'id_revision_oc')),
        # 'en proceso de firma y sello (1)' module
        proceso_firma_sello1_UI(NS(id, 'id_proceso_firma_sello1'))
      ),
      # Interno
      interno_UI(NS(id, 'id_module_interno')),
      # Actualización
      actualizacion_UI(NS(id, 'id_module_actualizacion'))
    )
  )
}

CNGAE_2025_Server <- function(id) {
  moduleServer(id, function(input, output, session) {

    data <- reactive({
      data_path <- 'historial_seguimiento/xIktan_20250425020837567_reporteSegumiento.xlsx'
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

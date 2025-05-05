# 'CNGAE_current_year' module ---------------------------------------------

source('modules/CNGAE_2025/module_info_analitica.R')
source('modules/CNGAE_2025/module_upload_file.R')
source('modules/CNGAE_2025/module_top_ten.R')
source('modules/CNGAE_2025/module_revision_oc.R')
source('modules/CNGAE_2025/module_proceso_firma_sello1.R')
source('modules/CNGAE_2025/module_interno.R')
source('modules/CNGAE_2025/module_actualizacion.R')
source("data/data_2025_year.R")

CNGAE_2025_UI <- function(id) {
  ns <- NS(id)
  tabItem(
    "CNGAE_analytics_2025",
    navbarPage(
      title       = "Análisis y seguimiento",
      id          = ns('id_navbar_analytics'),
      selected    = 'Cargar archivo',
      collapsible = TRUE,
      # 'Info analitica' module
      info_analitica_UI(ns('id_info_analitica')),
      # 'upload file' module
      upload_file_UI(ns('id_upload_file')),
      # Observaciones
      navbarMenu(
        "Observaciones",
        icon = icon("square-poll-vertical"),
        # 'top ten' module
        top_ten_UI(ns('id_top_ten'))
      ),
      # Cuestionarios
      navbarMenu(
        "Cuestionarios",
        icon = icon("square-poll-vertical"),
        # 'revisión oc' module
        revision_oc_UI(ns('id_revision_oc')),
        # 'en proceso de firma y sello (1)' module
        proceso_firma_sello1_UI(ns('id_proceso_firma_sello1'))
      ),
      # Interno
      interno_UI(ns('id_module_interno')),
      # Actualización
      actualizacion_UI(ns('id_module_actualizacion'))
    )
  )
}

CNGAE_2025_Server <- function(id) {
  moduleServer(id, function(input, output, session) {

    hideTab(inputId = "id_navbar_analytics", target = "Observaciones")
    hideTab(inputId = "id_navbar_analytics", target = "Cuestionarios")
    hideTab(inputId = "id_navbar_analytics", target = "Interno")

    observeEvent(is_null(data()), {
      hideTab(inputId = "id_navbar_analytics", target = "Observaciones")
      hideTab(inputId = "id_navbar_analytics", target = "Cuestionarios")
      hideTab(inputId = "id_navbar_analytics", target = "Interno")
    })

    observeEvent(data(), {
      showTab(inputId = "id_navbar_analytics", target = "Observaciones")
      showTab(inputId = "id_navbar_analytics", target = "Cuestionarios")
      showTab(inputId = "id_navbar_analytics", target = "Interno")
    })
    # info
    info_analitica_Server('id_info_analitica')
    # upload file
    # upload_file_Server('id_upload_file')
    data <- upload_file_Server('id_upload_file')
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

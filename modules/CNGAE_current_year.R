# 'CNGAE_current_year' module ---------------------------------------------

source('modules/CNGAE_current_year/module_info_analitica.R')
source('modules/CNGAE_current_year/module_upload_file.R')
source('modules/CNGAE_current_year/module_top_ten.R')
source('modules/CNGAE_current_year/module_revision_oc.R')
source('modules/CNGAE_current_year/module_proceso_firma_sello1.R')
source('modules/CNGAE_current_year/module_interno.R')
source('modules/CNGAE_current_year/module_actualizacion.R')

CNGAE_current_year_UI <- function(id) {
  tabItem(
    "CNGAE_analysis",
    navbarPage(
      title       = "Análisis y seguimiento",
      id          = NS(id, "id_navbar_current_year"), #NS(id) for collapse panels
      selected    = "Cargar archivo",
      collapsible = TRUE,
      # 'info analitica' module
      info_analitica_UI(NS(id, 'id_info_analitica')),
      # 'upload file' module
      upload_file_UI(NS(id, 'id_upload_file')),
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

CNGAE_current_year_Server <- function(id) {
  moduleServer(id, function(input, output, session) {

    hideTab(inputId = "id_navbar_current_year", target = "Observaciones")
    hideTab(inputId = "id_navbar_current_year", target = "Cuestionarios")
    hideTab(inputId = "id_navbar_current_year", target = "Interno")

    observeEvent(is_null(data()), {
      hideTab(inputId = "id_navbar_current_year", target = "Observaciones")
      hideTab(inputId = "id_navbar_current_year", target = "Cuestionarios")
      hideTab(inputId = "id_navbar_current_year", target = "Interno")
    })

    observeEvent(data(), {
      showTab(inputId = "id_navbar_current_year", target = "Observaciones")
      showTab(inputId = "id_navbar_current_year", target = "Cuestionarios")
      showTab(inputId = "id_navbar_current_year", target = "Interno")
    })

    # info
    info_analitica_Server('id_info_analitica')
    # upload file
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

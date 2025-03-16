# 'CNGAE_current_year' module ---------------------------------------------

source('modules/analitica/CNGAE_current_year/module_info_analitica.R')
source('modules/analitica/CNGAE_current_year/module_top_ten.R')


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


# Observaciones -----------------------------------------------------------

      navbarMenu(
        "Observaciones",
        icon = icon("square-poll-vertical"),
        # 'top ten' module
        top_ten_UI(NS(id, 'id_top_ten'))
      ),


# Cuestionarios -----------------------------------------------------------

      navbarMenu(
        "Cuestionarios",
        icon = icon("square-poll-vertical"),
        # 'revisión oc' module
        revision_oc_UI(NS(id, 'id_revision_oc')),
        # 'en proceso de firma y sello (1)' module

      ),


      # Interno -----------------------------------------------------------------

      tabPanel(
        "Interno",
        icon = icon("square-poll-vertical"),
        div(
          class = "pull-right",
          logoutUI(
            id = "logout",
            label = "",
            icon = icon("sign-out-alt")
          )
        ),
        loginUI(
          id = "login",
          title = "",
          user_title = "Usuario",
          pass_title = "Contraseña",
          login_title = "Iniciar sesión",
          error_message = "¡Usuario o contraseña no válidos!",
        ),
        uiOutput("diie_interno")
      ),


      # Actualización -----------------------------------------------------------

      actualizacion_UI("id_module_actualizacion")
    )
  )
}

CNGAE_current_year_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    info_analitica_Server('id_info_analitica')
    upload_file_Server('id_upload_file')
    # observaciones
    top_ten_Server('id_top_ten')
    # revisión oc
    revision_oc_Server('id_revision_oc')

  })
}

source("functions/datatable_something.R")

function(input, output, session) {

  res_auth <- secure_server(
    check_credentials = check_credentials(application_user_base),
    timeout           = 20
  )

  auth <- callModule(
    module = auth_server,
    id     = "auth",
    check_credentials = check_credentials(application_user_base)
  )


  fullscreen_mode <- reactiveVal(FALSE)

  observeEvent(input$page_toggle, {
    if (!fullscreen_mode()) {

      runjs("document.documentElement.requestFullscreen();")
      updateActionButton(session, "page_toggle", icon = icon("minimize"))
      fullscreen_mode(TRUE)
    } else {

      session$sendCustomMessage("exitFullScreen", list())
      updateActionButton(session, "page_toggle", icon = icon("maximize"))
      fullscreen_mode(FALSE)
    }
  })


  CNGAE_2024_Server('id_CNGAE_2024')

  CNGAE_2025_Server('id_CNGAE_2025')

  CNGAE_operative_2023_Server('id_CNGAE_operative_2023')

}

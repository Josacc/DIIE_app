# "Actualización" module

# module UI
actualizacion_UI <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = strong(
      textOutput(ns('update')),
      style = "color: #3c8dbc; font-size: 12px;"
    ),
    h4(strong("Historial de seguimiento")),
    br(),
    DTOutput(ns('database_original')),
    br(), br(), br(), br()
  )
}

# module server
actualizacion_Server <- function(id, data) {
  # stopifnot(is.reactive(data()))  # check this line!!
  moduleServer(id, function(input, output, session) {
    output$update <- renderText({
      str_c("Actualización ", (data())[[3]])
    })

    output$database_original <- renderDT({
      datatable_actualizacion((data())[[1]])
    })
  })
}

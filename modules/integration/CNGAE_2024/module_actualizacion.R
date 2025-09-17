# "Actualización" module

# module UI
actualizacion_UI <- function(id) {
  tabPanel(
    title = strong(
      textOutput(NS(id, "update")),
      style = "color: #3c8dbc; font-size: 12px;"
    ),
    h4(strong("Historial de seguimiento")),
    br(),
    DTOutput(NS(id, "database_original")),
    br(), br(), br(), br()
  )
}

# module server
actualizacion_Server <- function(id, data) {
  stopifnot(is.reactive(data))
  moduleServer(id, function(input, output, session) {


    datatable_actualizacion <- mod_integration$datatable_actualizacion


    output$update <- renderText({
      req(data())
      str_c("Actualización ", (data())[[3]])
    })

    output$database_original <- renderDT({
      datatable_actualizacion((data())[[1]])
    })

  })

}

# Actualización module
actualizacion_UI <- function(id) {
  tabPanel(
    title = strong(
      uiOutput(NS(id, "update")),
      style = "color: #3c8dbc;font-size: 12px;"
    ),
    h4(strong("Historial de seguimiento")),
    br(),
    br(),
    DTOutput(NS(id, "database_original"))
  )
}

actualizacion_Server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$update <- renderUI({
      req(data())
      str_c("Actualización")# ", data()[[3]])
    })

    output$database_original <- renderDT({
      data()[[1]] %>%
        datatable(
        rownames   = FALSE,
        filter     = "top",
        options    = list(
          pageLength  = 5,
          language    = list(
            url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
          )
        )
      )
    })

  })
}

# 'upload file' module ----------------------------------------------------

nameUI <- function(id) {

  tabPanel(
    "Cargar archivo",
    icon = icon("upload"),
    br(),
    fluidRow(
      column(
        width = 4,
        useShinyFeedback(),
        fileInput(
          NS(id, "file_upload"),
          'Historial de seguimiento con extensión "xlsx"',
          accept = c(".xlsx"), width = "450px",
          buttonLabel =  "Buscar", placeholder = "Sin archivo"
        )
      ),
      column(
        width = 1,
        actionBttn(
          inputId = NS(id, "info_button_file_upload"),
          label   = "",
          icon    = icon("info-circle"),
          style   = "jelly"
        )
      )
    )
  )
}

nameServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # hasta aquí


  })
}

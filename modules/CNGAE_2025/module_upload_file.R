# 'upload file' module ----------------------------------------------------

upload_file_UI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Cargar archivo",
    icon = icon("upload"),
    br(),
    fluidRow(
      column(
        width = 4,
        useShinyFeedback(),
        fileInput(
          ns("file_upload"),
          'Carga el historial de seguimiento con extensión "xlsx"',
          accept      = c(".xlsx"),
          width       = "450px",
          buttonLabel =  "Buscar",
          placeholder = "Sin archivo"
        )
      ),
      column(
        width = 1,
        actionBttn(
          inputId = ns("info_button_file_upload"),
          label   = "",
          icon    = icon("info-circle"),
          style   = "jelly"
        )
      )
    )
  )
}

upload_file_Server <- function(id) {
  moduleServer(id, function(input, output, session) {

    data <- reactive({
      req(input$file_upload)
      ext <- file_ext(input$file_upload$datapath)
      feedbackDanger("file_upload", ext != "xlsx", "Extensión desconocida")

      if (!identical(ext, "xlsx")) {
        return(NULL)
      }

      req(identical(ext, "xlsx"))
      pre_data <- read_xlsx(input$file_upload$datapath)
      condition <- identical(names(pre_data)[2], "INSTITUTO NACIONAL DE ESTADÍSTICA Y GEOGRAFÍA") &&
        identical(pre_data[[7,1]],  "Folio") &&
        identical(pre_data[[7,3]],  "Entidad") &&
        identical(pre_data[[7,5]],  "Usuario") &&
        identical(pre_data[[7,6]],  "Perfil") &&
        identical(pre_data[[7,8]],  "Registro") &&
        identical(pre_data[[7,10]], "Estatus") &&
        identical(pre_data[[7,12]], "Observación") &&
        identical(pre_data[[7,15]], "Contador de días")
      feedbackDanger("file_upload", !condition, "Archivo desconocido")

      if (!condition) {
        return(NULL)
      }

      req(condition)
      id <- showNotification(strong("Leyendo...",
                                    style = "color: #0323f5;font-size: 15px;font-style: italic;"),
                             type = "message", duration = NULL)
      on.exit(removeNotification(id), add = TRUE)

      database     <- data_and_update(input$file_upload$datapath)[[1]]
      update       <- data_and_update(input$file_upload$datapath)[[2]]
      database_obs <- team_data(reviewer_team, database) %>%
        filter(`Cantidad de obs` > 0)

      return(list(database, database_obs, update))
    })

    # Info upload "Historial de seguimiento con extensión 'xlsx'".
    observeEvent(input$info_button_file_upload, {
      show_alert(
        session = session,
        title   = "",
        text    = tags$div(
          tags$h3("Información",
                  style = "color: #0076C8; font-weight: bold; text-align: center"),
          tags$br(), tags$br(),
          tags$h4('Recuerda cargar el historial de seguimiento en formato “xlsx”',
                  style = "font-weight: bold; text-align: center"),
          tags$br(),
          style = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
          'El reporte',
          tags$b('Historial de seguimiento', style = "color: #0076C8"),
          'lo puedes descargar desde la página de IKTAN siguiendo estos pasos:',
          tags$br(), tags$br(),
          tags$ol(
            tags$li('Ingresa tus credenciales.'),
            tags$li('Selecciona tu perfil de acceso.'),
            tags$li('Selecciona la ventana “Reportes”.'),
            tags$br(),
            tags$img(src = "ventana_reportes.png" ,
                     `style` = "display: block;
                                                             margin-left: auto;
                                                             margin-right: auto;
                                                             width: 25%;"
            ),
            tags$br(),
            tags$li('Selecciona el recuadro “Reporte”.'),
            tags$li('Elige el formato “XLSX”.'),
            tags$li('Elige la opción “Historial de seguimiento”.'),
            tags$li('Presiona el botón aceptar.'),
            tags$li('Da clic en el archivo generado para descargarlo.'),
            tags$br(),
            tags$img(src = "select_reportes.png" ,
                     `style` = "display: block;
                                                             margin-left:  -4.3rem;
                                                             margin-right: auto;
                                                             width: 105%;"
            ),
            tags$br(),
            tags$li('En tu explorador de archivos localiza dónde se descargó
        la carpeta comprimida del reporte de seguimiento, generalmente
        la podrás encontrar en la carpeta de descargas.'),
            tags$li('Extrae el archivo de la carpeta comprimida.'),
            tags$li('¡Listo! Ya tienes el archivo que debes cargar en la aplicación.')
          )
        ),
        html  = TRUE,
        width = "55%"
      )
    })

    return(data)
  })
}

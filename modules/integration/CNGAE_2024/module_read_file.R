# 'read file' module ------------------------------------------------------

read_file_UI <- function(id) {

}

read_file_Server <- function(id) {
  moduleServer(id, function(input, output, session) {

    data <- reactive({
      data_path <- 'historial_seguimiento/xIktan_20241024044202298_reporteSegumiento.xlsx'
      ext <- file_ext(data_path)

      if (!identical(ext, "xlsx")) {
        return(NULL)
      }

      req(identical(ext, "xlsx"))
      raw_data <- read_xlsx(data_path)
      condition <- identical(names(raw_data)[2], "INSTITUTO NACIONAL DE ESTADÍSTICA Y GEOGRAFÍA") &&
        identical(raw_data[[3,2]],  "CNGAE 2024") &&
        identical(raw_data[[7,1]],  "Folio") &&
        identical(raw_data[[7,3]],  "Entidad") &&
        identical(raw_data[[7,5]],  "Usuario") &&
        identical(raw_data[[7,6]],  "Perfil") &&
        identical(raw_data[[7,8]],  "Registro") &&
        identical(raw_data[[7,10]], "Estatus") &&
        identical(raw_data[[7,12]], "Observación") &&
        identical(raw_data[[7,15]], "Contador de días")

      if (!condition) {
        return(NULL)
      }

      req(condition)
      data <- data_and_update(data_path)
      database     <- data[[1]]
      update       <- data[[2]]
      database_obs <- team_data(reviewer_team, database) %>%
        filter(`Cantidad de obs` > 0)

      return(list(database, database_obs, update))
    })

    return(data)
  })
}


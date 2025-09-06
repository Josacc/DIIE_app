# 'Evaluación' module ---------------------------------------------

mod_evaluation <- env()
function_file  <- list.files('modules/evaluation/functions/', pattern = '\\.R$', full.names = TRUE)
map(function_file, ~sys.source(., envir = mod_evaluation))


module_evaluation_UI <- function(id) {

  ns <- NS(id)

  list(
    CNGAE_operative_citas_2023_UI(ns('id_CNGAE_operative_citas_2023')),

    CNGAE_operative_recupera_2023_UI(ns('id_CNGAE_operative_recupera_2023')),

    CNGAE_operative_revision_2023_UI(ns('id_CNGAE_operative_revision_2023'))
  )

}

module_evaluation_Server <- function(id) {
  moduleServer(id, function(input, output, session) {


    data_and_update    <- mod_evaluation$data_and_update
    write_database_DOE <- mod_evaluation$write_database_DOE


    database_DOE <- reactive({

      data_operative <- (data_and_update('historial_seguimiento/xIktan_20231005104808909_reporteSegumiento.xlsx'))[[1]] %>%
        write_database_DOE()

      return(data_operative)
    })


    CNGAE_operative_citas_2023_Server('id_CNGAE_operative_citas_2023', database_DOE)

    CNGAE_operative_recupera_2023_Server('id_CNGAE_operative_recupera_2023', database_DOE)

    CNGAE_operative_revision_2023_Server('id_CNGAE_operative_revision_2023', database_DOE)

  })
}


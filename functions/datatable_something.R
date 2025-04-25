# Data table function on "q_aclaracion_oc_filter"

datatable_q_aclaracion_oc_filter <- function(database) {

  database %>%
    datatable(
      rownames   = FALSE,
      selection  = list(target = "cell"),
      extensions = c(
        "Buttons"
      ),
      options    = list(
        ordering    = FALSE,
        pageLength  = 5,
        dom         = "Blftip",
        buttons     = list(
          list(
            extend           = "colvis",
            text             = "Visibilidad de columnas",
            columns          = c(1:5, 7:9),
            collectionLayout = "fixed columns",
            popoverTitle     = "Control de visibilidad de columnas"
          )
        ),
        language    = list(
          url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
        ),
        columnDefs  = list(
          list(visible = FALSE, targets = c(1:3, 7:9))
        )
      )
    )
}


# Data table functión on "Actualización"

datatable_actualizacion <- function(database) {

  database %>%
    datatable(
      rownames   = FALSE,
      filter     = "top",
      extensions = c(
        "SearchBuilder", "DateTime", "Buttons"
      ),
      options    = list(
        ordering      = FALSE,
        pageLength    = 5,
        dom           = "QBlftip",
        buttons       = list(
          list(
            extend           = "colvis",
            text             = "Visibilidad de columnas",
            columns          = 0:9,
            collectionLayout = "fixed columns",
            popoverTitle     = "Control de visibilidad de columnas"
          )
        ),
        searchbuilder = TRUE,
        language      = list(
          url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
        ),
        columnDefs  = list(
          list(visible = FALSE, targets = c(7, 8, 9))
        )
      )
    )
}

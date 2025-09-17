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

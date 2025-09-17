#

db_q_aclaracion_oc_filter <- function(database_1, database_2, input_matrix) {

  if (length(input_matrix) == 0) {
    return(database_1)
  }

  aux_function <- function(database, x) {
    .matrix <- database %>%
      as.matrix()
    folio <- str_c(
      .matrix[x[1], 1], colnames(.matrix)[x[2] + 1]
    )
    return(folio)
  }

  folios <- apply(input_matrix, 1, aux_function, database = database_2)

  return(database_1 %>% filter(Folio %in% folios))

}

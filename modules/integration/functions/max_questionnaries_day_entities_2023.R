# indicator for plot_arrival_questionnaires_entities_2023

max_questionnaries_day_entities_2023 <- function(data, entitie) {

  .data <- data %>%
    count(Registro, Entidad, Censo) %>%
    filter(Entidad == entitie) %>%
    rename(`Cuestionarios enviados a revisión OC` = names(.)[[4]]) %>%
    group_by(Censo) %>%
    summarise(y_max = max(`Cuestionarios enviados a revisión OC`))

  return(.data)

}

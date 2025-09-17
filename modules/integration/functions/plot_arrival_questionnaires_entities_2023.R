# Plot on arrival of questionnaires for entities grid census

plot_arrival_questionnaires_entities_2023 <- function(data, entitie, year = 2025) {

  DIIE_dates <- str_c('DIIE_dates_', year) %>% get(envir = .GlobalEnv)

  .data <- data %>%
    filter(Entidad == entitie)

  if (nrow(.data) == 0) {
    return(NULL)
  }

  suppressWarnings({
    .plot <- .data %>%
      count(Registro, Censo) %>%
      rename(`Cuestionarios enviados a revisión OC` = names(.)[[3]]) %>%
      left_join(DIIE_dates, by = c("Censo" = "name")) %>%
      left_join(max_questionnaries_day_entities_2023(data, entitie), by = "Censo") %>% ## revisar!!
      ggplot(aes(Registro, `Cuestionarios enviados a revisión OC`)) +
      geom_rect(aes(xmin = `start CE`, xmax = `end CE`,
                    ymin = 0, ymax = y_max + 1, Responsable = "CE"),
                fill = "green", alpha = 0.1) +
      geom_rect(aes(xmin = prosecution, xmax = diffusion,
                    ymin = 0, ymax = y_max + 1, Responsable = "SPICNG y SAICNG"),
                fill = "red", alpha = 0.3) +
      geom_rect(aes(xmin = `start DIIE`, xmax = `end DIIE`,
                    ymin = 0, ymax = y_max + 1, Responsable = "SOCNG"),
                fill = "orange", alpha = 0.5) +
      geom_point(size = 1, color = "blue", alpha = 3/4) +
      facet_grid(Censo ~ ., scales = "free_y") +
      scale_x_date(date_breaks = "week", date_labels = "%d %b") +
      theme(
        axis.text.x  = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 7),
        axis.title.x = element_blank(),
        axis.text.y  = element_text(size = 7),
        axis.title.y = element_blank()
      )
  })

  return(ggplotly(.plot))

}

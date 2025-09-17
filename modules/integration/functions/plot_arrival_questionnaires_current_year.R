# Plot about arrival general of questionnaires in current year

plot_arrival_questionnaires_current_year <- function(data, .week, .title = "", year = 2025) { #ajustar similar a la función sobre firma y sello

  DIIE_dates <- str_c('DIIE_dates_', year) %>% get(envir = .GlobalEnv)

  suppressWarnings({
    .plot <- data %>%
      count(Registro) %>%
      rename(`Cuestionarios enviados a revisión OC` = names(.)[2]) %>%
      ggplot(aes(Registro, `Cuestionarios enviados a revisión OC`)) +
      geom_rect(aes(xmin = .week, xmax = .week + weeks(1),
                    ymin = 0, ymax = 50),
                fill = "grey", alpha = 3/4) +
      annotate("segment", x = DIIE_dates[[2, 5]], xend = DIIE_dates[[2, 5]], y = 0, yend = 50, colour = "red", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[3, 5]], xend = DIIE_dates[[3, 5]], y = 0, yend = 50, colour = "red", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[4, 5]], xend = DIIE_dates[[4, 5]], y = 0, yend = 50, colour = "red", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[5, 5]], xend = DIIE_dates[[5, 5]], y = 0, yend = 50, colour = "red", alpha = 1/2) +
      annotate("text", x = DIIE_dates[[2, 5]], y = 52, label = str_c("Cierre cap. \n", DIIE_dates[[2, 1]]), size = 2, color = "red") +
      annotate("text", x = DIIE_dates[[3, 5]], y = 52, label = str_c("Cierre cap. \n", DIIE_dates[[3, 1]]), size = 2, color = "red") +
      annotate("text", x = DIIE_dates[[4, 5]], y = 52, label = str_c("Cierre cap. \n", DIIE_dates[[4, 1]]), size = 2, color = "red") +
      annotate("text", x = DIIE_dates[[5, 5]], y = 52, label = str_c("Cierre cap. \n", DIIE_dates[[5, 1]]), size = 2, color = "red") +
      geom_point(aes(color = -`Cuestionarios enviados a revisión OC`,
                     label1 = Registro, label2 = `Cuestionarios enviados a revisión OC`),
                 show.legend = FALSE) +
      scale_x_date(date_breaks = "week", date_labels = "%d %b") +
      ggtitle(.title) +
      theme_classic() +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
        axis.title.x = element_blank(),
        axis.text.y  = element_text(size = 9),
        axis.title.y = element_blank(),
        plot.title   = element_text(size = 10)
      )
  })

  return(ggplotly(.plot, tooltip = c("label1", "label2")))

}

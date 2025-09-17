# Functions

team_data <- function(team, data) {
  return(team %>% left_join(data, by = "Usuario", multiple = "all"))
}

# Plot about arrival general of questionnaires in previous year
plot_arrival_questionnaires_previous_year <- function(data, .week, .title = "", year = 2025) { #ajustar similar a la función sobre firma y sello

  DIIE_dates_previous_year <- str_c('DIIE_dates_previous_year_', year) %>% get(envir = .GlobalEnv)

  suppressWarnings({
    .plot <- data %>%
      count(Registro) %>%
      rename(`Cuestionarios enviados a revisión OC` = names(.)[2]) %>%
      ggplot(aes(Registro, `Cuestionarios enviados a revisión OC`)) +
      geom_rect(aes(xmin = .week, xmax = .week + weeks(1),
                    ymin = 0, ymax = 50),
                fill = "grey", alpha = 3/4) +
      annotate("segment", x = DIIE_dates_previous_year[[2, 5]], xend = DIIE_dates_previous_year[[2, 5]], y = 0, yend = 50, colour = "red", alpha = 1/2) +
      annotate("segment", x = DIIE_dates_previous_year[[3, 5]], xend = DIIE_dates_previous_year[[3, 5]], y = 0, yend = 50, colour = "red", alpha = 1/2) +
      annotate("segment", x = DIIE_dates_previous_year[[4, 5]], xend = DIIE_dates_previous_year[[4, 5]], y = 0, yend = 50, colour = "red", alpha = 1/2) +
      annotate("segment", x = DIIE_dates_previous_year[[5, 5]], xend = DIIE_dates_previous_year[[5, 5]], y = 0, yend = 50, colour = "red", alpha = 1/2) +
      annotate("text", x = DIIE_dates_previous_year[[2, 5]], y = 52, label = str_c("Cierre cap. \n", DIIE_dates_previous_year[[2, 1]]), size = 2, color = "red") +
      annotate("text", x = DIIE_dates_previous_year[[3, 5]], y = 52, label = str_c("Cierre cap. \n", DIIE_dates_previous_year[[3, 1]]), size = 2, color = "red") +
      annotate("text", x = DIIE_dates_previous_year[[4, 5]], y = 52, label = str_c("Cierre cap. \n", DIIE_dates_previous_year[[4, 1]]), size = 2, color = "red") +
      annotate("text", x = DIIE_dates_previous_year[[5, 5]], y = 52, label = str_c("Cierre cap. \n", DIIE_dates_previous_year[[5, 1]]), size = 2, color = "red") +
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

count_arrival_questionnaires_week <- function(data, .week) {
  .n <- data %>%
    count(floor_date(Registro, "week", week_start = 1)) %>%
    rename(week = names(.)[1]) %>%
    filter(week == .week)

  if (nrow(.n) == 0) {
    return(0)
  }

  return(.n %>% .[[2]])
}

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

plot_arrival_questionnaires_grid_census_2023 <- function(data, max_day, .title = "", year = 2025) {

  DIIE_dates <- str_c('DIIE_dates_', year) %>% get(envir = .GlobalEnv)

  .data <- data %>%
    count(Registro, Censo) %>%
    rename(`Cuestionarios enviados a revisión OC` = names(.)[[3]]) %>%
    left_join(DIIE_dates, by = c("Censo" = "name")) %>%
    left_join(max_day, by = "Censo")

  suppressWarnings({
    .plot <- .data %>%
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
      geom_point(size = 0.2, color = "blue") +
      facet_grid(Censo ~ ., scales = "free_y") +
      scale_x_date(date_breaks = "week", date_labels = "%d %b") +
      ggtitle(.title) +
      theme(
        axis.text.x  = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 7),
        axis.title.x = element_blank(),
        axis.text.y  = element_text(size = 7),
        axis.title.y = element_blank(),
        plot.title   = element_text(size = 10)
      )
  })

  return(ggplotly(.plot))
}

# Plot on arrival of questionnaires for entities grid census
max_questionnaries_day_entities_2023 <- function(data, entitie) {

  .data <- data %>%
    count(Registro, Entidad, Censo) %>%
    filter(Entidad == entitie) %>%
    rename(`Cuestionarios enviados a revisión OC` = names(.)[[4]]) %>%
    group_by(Censo) %>%
    summarise(y_max = max(`Cuestionarios enviados a revisión OC`))

  return(.data)
}

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

count_questionnaires_firma_sello_week <- function(data, .week) {
  .n <- data %>%
    count(floor_date(Registro, "week", week_start = 1)) %>%
    rename(week = names(.)[1]) %>%
    filter(week == .week)

  if (nrow(.n) == 0) {
    return(0)
  }

  return(.n %>% .[[2]])
}

# DT on questionnaires set free by Registro
DT_questionnaires_firma_sello_registro <- function(.data) {

  .data %>%
    group_by(Registro) %>%
    dplyr::summarise(n = n(), `Folios<br>Cuestionarios en proceso de firma y sello (1)`= str_c(Folio, collapse = ", ")) %>%
    dplyr::arrange(desc(Registro))
}


# Plot on arrival general of questionnaires
plot_questionnaires_firma_sello <- function(data, .title = "") {

  ymax <- data %>%
    count(Registro) %>%
    arrange(desc(n)) %>%
    .[[1, 2]] + 5

  suppressWarnings({
    .plot <- data %>%
      group_by(Registro) %>%
      dplyr::summarise(n = n(), Folios = str_c(Folio, collapse = ", ")) %>%
      rename(`Cuestionarios en proceso de firma y sello (1)` = names(.)[2]) %>%
      ggplot(aes(Registro, `Cuestionarios en proceso de firma y sello (1)`)) +
      geom_point(aes(color = -`Cuestionarios en proceso de firma y sello (1)`,
                     label1 = Registro, label2 = `Cuestionarios en proceso de firma y sello (1)`),
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

# Plot on questionnaires set free for week
plot_questionnaires_firma_sello_week <- function(data, .title = "", year = 2025) {

  DIIE_dates <- str_c('DIIE_dates_', year) %>% get(envir = .GlobalEnv)

  .data <- data %>%
    count(ceiling_date(Registro, "week", week_start = 1)) %>%
    rename(Domingo = names(.)[1], `Cuestionarios acumulados en proceso de firma y sello (1)` = names(.)[2]) %>%
    transmute(Domingo = Domingo - 1, `Cuestionarios acumulados en proceso de firma y sello (1)`)

  ymax <- .data %>%
    arrange(desc(`Cuestionarios acumulados en proceso de firma y sello (1)`)) %>%
    .[[1, 2]] + 5

  suppressWarnings({
    .plot <- .data %>%
      ggplot(aes(Domingo, `Cuestionarios acumulados en proceso de firma y sello (1)`)) +
      annotate("segment", x = DIIE_dates[[2, 5]], xend = DIIE_dates[[2, 5]], y = 0, yend = ymax, colour = "orange", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[3, 5]], xend = DIIE_dates[[3, 5]], y = 0, yend = ymax, colour = "orange", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[4, 5]], xend = DIIE_dates[[4, 5]], y = 0, yend = ymax, colour = "orange", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[5, 5]], xend = DIIE_dates[[5, 5]], y = 0, yend = ymax, colour = "orange", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[2, 7]], xend = DIIE_dates[[2, 7]], y = 0, yend = ymax, colour = "red", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[3, 7]], xend = DIIE_dates[[3, 7]], y = 0, yend = ymax, colour = "red", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[4, 7]], xend = DIIE_dates[[4, 7]], y = 0, yend = ymax, colour = "red", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[5, 7]], xend = DIIE_dates[[5, 7]], y = 0, yend = ymax, colour = "red", alpha = 1/2) +
      annotate("text", x = DIIE_dates[[2, 5]], y = ymax + 2, label = str_c("Cierre cap. \n", DIIE_dates[[2, 1]]), size = 2, color = "orange") +
      annotate("text", x = DIIE_dates[[3, 5]], y = ymax + 2, label = str_c("Cierre cap. \n", DIIE_dates[[3, 1]]), size = 2, color = "orange") +
      annotate("text", x = DIIE_dates[[4, 5]], y = ymax + 2, label = str_c("Cierre cap. \n", DIIE_dates[[4, 1]]), size = 2, color = "orange") +
      annotate("text", x = DIIE_dates[[5, 5]], y = ymax + 2, label = str_c("Cierre cap. \n", DIIE_dates[[5, 1]]), size = 2, color = "orange") +
      annotate("text", x = DIIE_dates[[2, 7]], y = ymax + 2, label = str_c("Difusión \n", DIIE_dates[[2, 1]]), size = 2, color = "red") +
      annotate("text", x = DIIE_dates[[3, 7]], y = ymax + 2, label = str_c("Difusión \n", DIIE_dates[[3, 1]]), size = 2, color = "red") +
      annotate("text", x = DIIE_dates[[4, 7]], y = ymax + 2, label = str_c("Difusión \n", DIIE_dates[[4, 1]]), size = 2, color = "red") +
      annotate("text", x = DIIE_dates[[5, 7]], y = ymax + 2, label = str_c("Difusión \n", DIIE_dates[[5, 1]]), size = 2, color = "red") +
      geom_line(color = "blue" , alpha = 1/8) +
      geom_point(aes(color = -`Cuestionarios acumulados en proceso de firma y sello (1)`,
                     label1 = Domingo, label2 = `Cuestionarios acumulados en proceso de firma y sello (1)`),
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


# Plot on questionnaires set free per week separated by project.
plot_questionnaires_firma_sello_week_project <- function(data, project, .title = "", year = 2025) {

  DIIE_dates <- str_c('DIIE_dates_', year) %>% get(envir = .GlobalEnv)

  .data <- data %>%
    filter(Censo == project) %>%
    count(ceiling_date(Registro, "week", week_start = 1)) %>%
    rename(Domingo = names(.)[1], `Cuestionarios acumulados en proceso de firma y sello (1)` = names(.)[2]) %>%
    transmute(Domingo = Domingo - 1, `Cuestionarios acumulados en proceso de firma y sello (1)`)

  if (nrow(.data) == 0) {
    return(NULL)
  }

  ymax <- .data %>%
    arrange(desc(`Cuestionarios acumulados en proceso de firma y sello (1)`)) %>%
    .[[1, 2]] + 5

  .plot <- function(.census) {
    plot <- ggplot(.data, aes(Domingo, `Cuestionarios acumulados en proceso de firma y sello (1)`)) +
      annotate("segment", x = DIIE_dates[[which(DIIE_dates[1] == .census), 5]], xend = DIIE_dates[[which(DIIE_dates[1] == .census), 5]],
               y = 0, yend = ymax, colour = "orange", alpha = 1/2) +
      annotate("segment", x = DIIE_dates[[which(DIIE_dates[1] == .census), 7]], xend = DIIE_dates[[which(DIIE_dates[1] == .census), 7]],
               y = 0, yend = ymax, colour = "red", alpha = 1/2) +
      annotate("text",    x = DIIE_dates[[which(DIIE_dates[1] == .census), 5]], y = ymax + 2,
               label = str_c("Cierre cap. \n", .census), size = 2, color = "orange") +
      annotate("text",    x = DIIE_dates[[which(DIIE_dates[1] == .census), 7]], y = ymax + 2,
               label = str_c("Difusión \n", .census), size = 2, color = "red")

    return(plot)
  }

  suppressWarnings({
    .plot <- .plot(project) +
      geom_line(color = "blue" , alpha = 1/8) +
      geom_point(aes(color = -`Cuestionarios acumulados en proceso de firma y sello (1)`,
                     label1 = Domingo, label2 = `Cuestionarios acumulados en proceso de firma y sello (1)`),
                 show.legend = FALSE) +
      scale_x_date(date_breaks = "month", date_labels = "%d %b") +
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

# DT questionnaires set free by census.
DT_questionnaires_firma_sello_census <- function(.data, .census) {

  .data %>%
    filter(Censo == .census) %>%
    separate(Folio, into = c("entidad", "Cuestionario"), sep = 2) %>%
    select(1, 2) %>%
    group_by(Cuestionario) %>%
    summarise(Entidades = str_c(entidad, collapse = ", "))
}

# Plot on questionnaires set free separated by project.
plot_questionnaires_firma_sello_light <- function(data, .title = "", year = 2025) {

  DIIE_dates <- str_c('DIIE_dates_', year) %>% get(envir = .GlobalEnv)

  .data <- data %>%
    count(Registro, Censo) %>%
    rename(`Cuestionarios en proceso de firma y sello (1)` = names(.)[[3]])

  max_day <- .data %>%
    group_by(Censo) %>%
    summarise(y_max = max(`Cuestionarios en proceso de firma y sello (1)`))

  .data <- .data %>%
    left_join(DIIE_dates, by = c("Censo" = "name")) %>%
    left_join(max_day, by = "Censo")

  suppressWarnings({
    .plot <- .data %>%
      ggplot(aes(Registro, `Cuestionarios en proceso de firma y sello (1)`)) +
      geom_rect(aes(xmin = `start CE`, xmax = `end CE`,
                    ymin = 0, ymax = y_max + 1, Responsable = "CE"),
                fill = "green", alpha = 0.1) +
      geom_rect(aes(xmin = prosecution, xmax = diffusion,
                    ymin = 0, ymax = y_max + 1, Responsable = "SPICNG y SAICNG"),
                fill = "red", alpha = 0.3) +
      geom_rect(aes(xmin = `start DIIE`, xmax = `end DIIE`,
                    ymin = 0, ymax = y_max + 1, Responsable = "SOCNG"),
                fill = "orange", alpha = 0.5) +
      geom_point(size = 0.2, color = "blue") +
      facet_grid(Censo ~ ., scales = "free_y") +
      scale_x_date(date_breaks = "week", date_labels = "%d %b") +
      ggtitle(.title) +
      theme(
        axis.text.x  = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 7),
        axis.title.x = element_blank(),
        axis.text.y  = element_text(size = 7),
        axis.title.y = element_blank(),
        plot.title  = element_text(size = 10)
      )
  })

  return(ggplotly(.plot))
}

# Plot on set free questionnaires for entities separte by census
plot_questionnaires_firma_sello_light_entities <- function(data, entity, year = 2025) {

  DIIE_dates <- str_c('DIIE_dates_', year) %>% get(envir = .GlobalEnv)

  .data <- data %>%
    filter(Entidad == entity) %>%
    count(Registro, Censo) %>%
    rename(`Cuestionarios en proceso de firma y sello (1)` = names(.)[[3]])

  if (nrow(.data) == 0) {
    return(NULL)
  }

  max_day <- .data %>%
    group_by(Censo) %>%
    summarise(y_max = max(`Cuestionarios en proceso de firma y sello (1)`))

  .data <- .data %>%
    left_join(DIIE_dates, by = c("Censo" = "name")) %>%
    left_join(max_day, by = "Censo")

  suppressWarnings({
    .plot <- .data %>%
      ggplot(aes(Registro, `Cuestionarios en proceso de firma y sello (1)`)) +
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

# Count questionnnaires by time range.
count_questionnaires_firma_sello <- function(data, .census, .min = NULL, .max, year = 2025) {

  DIIE_dates <- str_c('DIIE_dates_', year) %>% get(envir = .GlobalEnv)
  .min <- pull(DIIE_dates, 2)[3]

  if (.census == "GLOBAL") {
    return(nrow(data %>% filter(.min <= Registro & Registro <= .max)))
  }

  return(data %>% filter(Censo == .census) %>% nrow())
}

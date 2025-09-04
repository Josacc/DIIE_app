# EVALUACIÓN DE LOS CNGAE -------------------------------------------


data_and_update_2023 <- function(.file) {

  pre_data <- read_xlsx(.file, skip = 8)

  .update <- pre_data %>%
    tail(2) %>%
    .[[1, 2]]

  .data <- pre_data %>%
    head(-2) %>%
    purrr::discard(is_logical) %>%
    rename(Folio = names(.)[1]) %>%
    mutate(Registro = as.Date(dmy_hms(Registro))) %>%
    mutate(`Contador de días` = parse_double(`Contador de días`)) %>%
    mutate(`Cantidad de obs` = str_count(Observación, "(P\\d+\\.\\d+)|(Complemento\\s\\d+)|(Anexo\\s\\d+)")) %>% # verificar los dígitos en los complementos de los cuestioanrios
    mutate(Entidad = factor(Entidad, levels = levels(pull(entities)))) %>%
    mutate(Censo = str_replace_all(str_sub(Folio, 3, 3),
                                   c("1" = "CNGE",
                                     "2" = "CNSPE",
                                     "3" = "CNSIPEE",
                                     "4" = "CNPJE",
                                     "5" = "CNIJE",
                                     "6" = "CNPLE",
                                     "7" = "CNDHE",
                                     "8" = "CNTAIPPDPE" ))) %>%
    mutate(Censo = factor(Censo, levels = levels(pull(census_2023, name))))

  return(list(.data, .update))
}


# Principal database for DOE analysis
write_database_DOE <- function(dataframe) {

  dataframe <- dataframe %>%
    select(-`Cantidad de obs`) %>%
    mutate(ID = seq_along(Folio)) %>%
    separate(Folio, into = c("id_estado", "Censo_n", "Módulo"), sep = c(2, 3), remove = FALSE) %>%
    mutate(id_estado = factor(id_estado, levels = levels(federal_entities[["id_estado"]]))) %>%
    mutate(Censo_n   = factor(Censo_n  , levels = 1:8)) %>%
    mutate(Estatus   = str_replace_all(Estatus, pattern = "\\(Revisión ROCE\\)", replacement = "Revisión ROCE")) %>%
    mutate(Estatus   = str_remove_all(Estatus, pattern = "\\(\\d+\\%\\)")) %>%
    relocate(ID) %>%
    relocate(Censo, .before = Entidad) %>%
    left_join(working_dates, by = "Registro") %>% # Se modificó "Registro" para considerar solo días habiles.
    select(-Registro) %>%
    rename(Registro = aux_var)

  return(dataframe)
}


# EDA about "Evolución del levantamiento" and "Recuper --------

# Function to get principal dataframe "Evolución y levantamiento".
DT_evolucion_levantamiento <- function(dataframe) {

  # Database of Folios with status "No aplica"
  dt_no_aplica <- DT_folio_no_aplica(dataframe)

  month_levels <- today() %>%
    month(label = TRUE, abbr = FALSE) %>%
    levels() %>%
    str_to_upper() %>%
    c("PENDIENTES")

  dataframe <- dataframe %>%
    mutate(
      Estatus = str_replace_all(
        Estatus,
        c("Recuperado con firma y sello por reconsulta \\(1\\)" = "Recuperado con firma y sello \\(1\\)")
      )
    ) %>%
    mutate(
      Mes = month(Registro, label = TRUE, abbr = FALSE) %>%
        str_to_upper()
    )

  # Mejora viable considerando el inicio del levantamiento; los argumentos de la función pueden ser vacíos.
  get_dataframe <- function(actividad, estatus) {

    df_aux <- dataframe %>%
      select(Folio, Registro, Estatus, Mes) %>%
      filter(Estatus == estatus | Estatus == "No aplica (1)") %>%
      group_by(Folio) %>%
      arrange(Registro, .by_group = TRUE) %>%
      mutate(n = seq_along(Folio)) %>%
      filter(n == 1) %>%
      select(-n)

    df_aux <- id_folio_extended %>%
      left_join(df_aux, by = "Folio") %>%
      replace_na(list(Mes = "PENDIENTES")) %>%
      filter(!(Folio %in% pull(dt_no_aplica))) %>% # Remove "Folios No aplica"
      mutate(Mes = fct(Mes, levels = month_levels)) %>%
      select(-Estatus) %>%
      mutate(`Actividades (fases)` = actividad)

    return(df_aux)
  }

  dframe <- map2(relacion_actividad_fase$`Actividades (fases)`, relacion_actividad_fase$`Estatus considerado`, get_dataframe) %>%
    bind_rows() %>%
    left_join(DOE_dates, by = c("Censo", "Actividades (fases)"))

  return(dframe)
}

# Graphic and data base "Evolucion del levantamiento"
plot_DT_evolucion_levantamiento <- function(dataframe, var_census, var_entities) {

  fases_censo <- DOE_dates %>%
    filter(Censo == var_census) %>%
    select(-Censo) %>%
    mutate(INICIO = format(INICIO, "%d %B") %>% str_to_upper()) %>%
    mutate(FIN    = format(FIN   , "%d %B") %>% str_to_upper())

  aux_df <- dataframe %>%
    filter(Censo == var_census) %>%
    filter(Entidad %in% var_entities) %>%
    group_by(`Actividades (fases)`, Mes) %>%
    summarise(n = n()) %>%
    mutate(`%` = round(n / sum(n), digits = 3) * 100) %>%
    select(-n)

  aux_plot <- aux_df %>%
    ggplot(aes(Mes, `%`, color = `Actividades (fases)`)) +
    geom_point() +
    geom_line(aes(group = `Actividades (fases)`)) +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_text(size = 7)
    ) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_color_manual(
      name = "Actividades (fases)",
      values = c("Integración de información preliminar (informantes)"       = "#BFDCEF",
                 "Revisión primaria y ajustes información preliminar (ROCE)" = "#1BA3E2",
                 "Revisión OC y liberación de información definitiva"        = "#0076C8",
                 "Recuperación de firmas y formalización de cuestionarios"   = "#003056")
    )

  aux_df <- aux_df %>%
    mutate(`%` = str_c(`%`, "%")) %>%
    pivot_wider(names_from = Mes, values_from = `%`, values_fill = "0%") %>%
    left_join(fases_censo, by = "Actividades (fases)") %>%
    relocate(`Actividades (fases)`, INICIO, FIN)

  return(
    list(
      dframe = aux_df,
      p = ggplotly(aux_plot)
    )
  )
}


# Graphic and data base "Recuperación oportuna de información"
plot_DT_recuperacion_oportuna_informacion <- function(dataframe, var_census, var_entities) {

  aux_DT <- dataframe %>%
    filter(Censo == var_census) %>%
    filter(Entidad %in% var_entities) %>%
    mutate(Cumplimiento = if_else(Registro <= FIN, true = "En tiempo", false = "Fuera de meta", missing = "PENDIENTES") %>%
             fct(levels = c("PENDIENTES", "Fuera de meta", "En tiempo")))

  aux_plot <- aux_DT %>%
    mutate(`Actividades (fases)` = fct_rev(`Actividades (fases)`)) %>%
    group_by(`Actividades (fases)`, Cumplimiento) %>%
    summarise(n = n()) %>%
    mutate(`%` = round(n / sum(n), digits = 3) * 100) %>%
    select(-n) %>%
    ungroup() %>%
    ggplot(aes(`Actividades (fases)`, `%`, fill = Cumplimiento)) +
    geom_col(position = "fill") +
    coord_flip(expand = FALSE) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    scale_y_continuous(labels = scales::label_percent(scale = 100)) +
    scale_fill_manual(
      values = c("En tiempo"     = "#0076C8",
                 "Fuera de meta" = "#1BA3E2",
                 "PENDIENTES"    = "#7F7F7F")
    )

  return(
    list(
      df   = aux_DT %>% select(Folio, Registro, `Actividades (fases)`, INICIO, FIN, Cumplimiento),
      plot = ggplotly(aux_plot) %>% plotly::layout(legend = list(traceorder = "reversed"))
    )
  )
}


# EDA about "Concertación de citas y entrega de cuesti --------

plot_citas_cuestionarios <- function(file_citas_agendadas, var_census, var_entities) {

  # Principal database
  aux_df <- read_xlsx(file_citas_agendadas, skip = 7) %>%
    suppressMessages() %>%
    suppressWarnings() %>%
    purrr::discard(is_logical) %>%
    filter(!is.na(Folio)) %>%
    select(Folio, `Fecha registro primer`, `Fecha Ultimo Movimien`) %>%
    mutate(`Fecha registro primer` = as_date(dmy(`Fecha registro primer`))) %>%
    mutate(`Fecha Ultimo Movimien` = as_date(dmy(`Fecha Ultimo Movimien`)))

  aux_df <- id_folio_extended %>%
    left_join(aux_df, by = "Folio") %>%
    left_join(dates_citas_cuestionarios, by = "Censo") %>%
    mutate(
      Concertación = if_else(
        `Fecha registro primer` <= Fin_citas, true = "En tiempo", false = "Fuera de meta", missing = "PENDIENTES"
      ) %>%
        fct(levels = c("En tiempo", "Fuera de meta", "PENDIENTES"))
    ) %>%
    mutate(
      Entrega = if_else(
        `Fecha Ultimo Movimien` <= Fin_cuestionarios, true = "En tiempo", false = "Fuera de meta", missing = "PENDIENTES"
      ) %>%
        fct(levels = c("En tiempo", "Fuera de meta", "PENDIENTES"))
    ) %>%
    filter(Censo == var_census) %>%
    filter(Entidad %in% var_entities)

  aux_plot <- function(var_x, var_title = NULL) {

    aux_df %>%
      count({{ var_x }}) %>%
      mutate(`%` = round(n / sum(n), digits = 3) * 100) %>%
      rename(Estatus = 1) %>%
      ggplot(aes(1.2, `%`, fill = Estatus)) +
      geom_col() +
      geom_text(aes(label = str_c(`%`, "%")),
                color    = "#FFFFFF",
                size     = 5,
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      ggtitle(var_title) +
      scale_fill_manual(
        name = NULL,
        values = c("En tiempo"     = "#003056",
                   "Fuera de meta" = "#0076C8",
                   "PENDIENTES"    = "#7F7F7F")
      ) +
      xlim(c(0.2, 1.7)) +
      theme(
        panel.background  = element_blank(),
        panel.grid        = element_blank(),
        axis.title        = element_blank(),
        axis.ticks        = element_blank(),
        axis.text         = element_blank(),
        legend.text       = element_text(size = 12)
      )
  }

  return(
    list(
      plot = aux_plot(`Concertación`, "Concertación") / aux_plot(Entrega , "Entrega"),
      df = aux_df %>% select(Folio, `Concertación`, Entrega)
    )
  )
}


# EDA about "Intervalos promedio de recuperación" ------------------------------------

plot_intervalo_recuperacion <- function(dataframe, var_census, var_entities) {

  wd <- working_dates %>%
    distinct(aux_var) %>%
    pull()

  inicio_integracion <- DOE_dates %>%
    filter(`Actividades (fases)` == "Integración de información preliminar (informantes)") %>%
    select(Censo, INICIO)

  censo_geom_text_zise <- c(
    "CNGE"       = 2.1,
    "CNSPE"      = 2.7,
    "CNSIPEE"    = 2.7,
    "CNPJE"      = 2.3,
    "CNIJE"      = 2.3,
    "CNPLE"      = 2.7,
    "CNDHE"      = 2.7,
    "CNTAIPPDPE" = 2.7
  )

  censo_geom_nudge_x <- c(
    "CNGE"       = 0.29,
    "CNSPE"      = 0.09,
    "CNSIPEE"    = 0.09,
    "CNPJE"      = 0.18,
    "CNIJE"      = 0.18,
    "CNPLE"      = 0.09,
    "CNDHE"      = 0.09,
    "CNTAIPPDPE" = 0.09
  )

  aux_df <- dataframe %>%
    select(-id_estado, -Censo_n, -Regional, -Mes, -INICIO, -FIN) %>%
    left_join(inicio_integracion, by = "Censo") %>%
    mutate(WD                = list(wd = wd)) %>%
    mutate(Registro_day      = match(Registro, WD$wd)) %>%
    mutate(INICIO_day        = match(INICIO, WD$wd)) %>%
    mutate(`Días laborables` = Registro_day - INICIO_day) %>%
    select(-WD, -Registro_day, -INICIO_day) %>%
    filter(Censo == var_census)

  max_min <- aux_df %>%
    group_by(Módulo, `Actividades (fases)`) %>%
    mutate(`Media nacional` = round(mean(`Días laborables`, na.rm = TRUE), 1)) %>%
    mutate(`Mínimo global`  = min(`Días laborables`, na.rm = TRUE)) %>%
    mutate(`Máximo global`  = max(`Días laborables`, na.rm = TRUE)) %>%
    ungroup() %>%
    count(Módulo, `Actividades (fases)`, `Media nacional`, `Mínimo global`, `Máximo global`) %>%
    select(-n)

  aux_plot <- aux_df %>%
    filter(Entidad %in% var_entities) %>%
    group_by(Módulo, `Actividades (fases)`) %>%
    mutate(`Promedio de días laborables` = round(mean(`Días laborables`, na.rm = TRUE), 1)) %>%
    ungroup() %>%
    left_join(max_min, by = c("Módulo", "Actividades (fases)")) %>%
    count(Censo, Módulo, `Actividades (fases)`, `Promedio de días laborables`, `Media nacional`, `Mínimo global`, `Máximo global`) %>%
    rename(Cuestionario = Módulo) %>%
    ggplot(aes(label1 = Censo)) +
    geom_linerange(aes(x = Cuestionario, ymin = `Mínimo global` , ymax = `Media nacional`), linewidth = 0.3, color = "#5ecbec") +
    geom_linerange(aes(x = Cuestionario, ymin = `Media nacional`, ymax = `Máximo global`),  linewidth = 0.3, color = "#ff7878") +
    geom_point(aes(x = Cuestionario, y = `Media nacional`), shape = 95, size = 1) +
    geom_point(aes(x = Cuestionario, y = `Promedio de días laborables`), size = 2, alpha = 0.8, color = "#0076C8") +
    geom_text(
      aes(x = Cuestionario, y = `Media nacional` - 2.5, label = str_c("µ: ", `Media nacional`)),
      size    = censo_geom_text_zise[var_census],
      nudge_x = censo_geom_nudge_x[var_census]
    ) +
    geom_text(
      aes(x = Cuestionario, y = `Mínimo global` - 2.5, label = str_c("Mín: ", `Mínimo global`)),
      size = censo_geom_text_zise[var_census],
      color = "#5ecbec"
    ) +
    geom_text(
      aes(x = Cuestionario, y = `Máximo global` + 3, label = str_c("Máx: ", `Máximo global`)),
      size = censo_geom_text_zise[var_census],
      color = "#ff7878"
    ) +
    facet_wrap(. ~ `Actividades (fases)`, scales = "free") +
    theme_bw() +
    labs(y = "Días")

  ggplotly(aux_plot)
  return(style(aux_plot, hoverinfo = "none", traces = c(1:12, 17:28)))

}


# EDA about "Revisiones realizadas a los cuestionarios" -------------------

plot_revisiones_cuestionarios <- function(dataframe, var_census, var_entities) {

  dt_no_aplica <- DT_folio_no_aplica(dataframe) %>%
    pull()

  censo_geom_text_zise <- c(
    "CNGE"       = 2.3,
    "CNSPE"      = 2.9,
    "CNSIPEE"    = 2.9,
    "CNPJE"      = 2.5,
    "CNIJE"      = 2.5,
    "CNPLE"      = 2.9,
    "CNDHE"      = 2.9,
    "CNTAIPPDPE" = 2.9
  )

  censo_geom_nudge_x <- c(
    "CNGE"       = 0.27,
    "CNSPE"      = 0.05,
    "CNSIPEE"    = 0.05,
    "CNPJE"      = 0.1,
    "CNIJE"      = 0.14,
    "CNPLE"      = 0.05,
    "CNDHE"      = 0.05,
    "CNTAIPPDPE" = 0.05
  )

  aux_df <- dataframe %>%
    select(-ID, -id_estado, -Censo_n, -`Contador de días`) %>%
    filter(!(Folio %in% dt_no_aplica)) %>%
    filter(str_detect(Estatus, "Revisión OC") & Perfil == "RESPONSABLE OPERATIVO") %>%
    filter(Censo == var_census)

  if (nrow(aux_df) == 0) {
    return(NULL)
  }

  aux_plot <- aux_df %>%
    count(Censo, Módulo, Entidad) %>%
    group_by(Módulo) %>%
    mutate(`Media nacional` = round(mean(n, na.rm = TRUE), 1)) %>%
    mutate(`Mínimo nacional`  = min(n, na.rm = TRUE)) %>%
    mutate(`Máximo nacional`  = max(n, na.rm = TRUE)) %>%
    filter(Entidad %in% var_entities) %>%
    mutate(`Promedio de revisiones` = round(mean(n, na.rm = TRUE), 1)) %>%
    ungroup() %>%
    count(Censo, Módulo, `Media nacional`, `Mínimo nacional`, `Máximo nacional`, `Promedio de revisiones`) %>%
    rename(Cuestionario = Módulo) %>%
    ggplot(aes(label1 = Censo)) +
    geom_linerange(aes(x = Cuestionario, ymin = `Mínimo nacional`, ymax = `Media nacional`), linewidth = 0.3, color = "#5ecbec") +
    geom_linerange(aes(x = Cuestionario, ymin = `Media nacional`, ymax = `Máximo nacional`), linewidth = 0.3, color = "#ff7878") +
    geom_point(aes(x = Cuestionario, y = `Media nacional`), shape = 95, size = 1) +
    geom_point(aes(x = Cuestionario, y = `Promedio de revisiones`), size = 2, alpha = 0.8, color = "#0076C8") +
    geom_text(
      aes(x = Cuestionario, y = `Media nacional`, label = str_c("µ: ", `Media nacional`)),
      size    = censo_geom_text_zise[var_census],
      nudge_x = censo_geom_nudge_x[var_census]
    ) +
    geom_text(
      aes(x = Cuestionario, y = `Mínimo nacional` - 0.2, label = str_c("Mín: ", `Mínimo nacional`)),
      size  = censo_geom_text_zise[var_census],
      color = "#5ecbec"
    ) +
    geom_text(
      aes(x = Cuestionario, y = `Máximo nacional` + 0.2, label = str_c("Máx: ", `Máximo nacional`)),
      size  = censo_geom_text_zise[var_census],
      color = "#ff7878"
    ) +
    theme_classic() +
    labs(y = "Revisiones")

  ggplotly(aux_plot)
  return(style(aux_plot, hoverinfo = "none", traces = c(1:3, 5:7)))
}


# EDA about "Duración del levantamiento por etapas" -----------------------

plot_duracion_levantamiento_etapas <- function(dataframe, var_census, var_entities) {

  dt_no_aplica <- DT_folio_no_aplica(dataframe) %>%
    pull()

  inicio_integracion <- DOE_dates %>%
    filter(`Actividades (fases)` == "Integración de información preliminar (informantes)") %>%
    select(Censo, INICIO)

  wd <- working_dates %>%
    distinct(aux_var) %>%
    pull()

  aux_df <- dataframe %>%
    filter(!(Folio %in% dt_no_aplica)) %>%
    select(Folio, Módulo, Censo, Entidad, Estatus, Registro) %>%
    left_join(inicio_integracion, by = "Censo") %>%
    mutate(WD           = list(wd = wd)) %>%
    mutate(INICIO_day   = match(INICIO,   WD$wd)) %>%
    mutate(Registro_day = match(Registro, WD$wd)) %>%
    mutate(diff_days    = Registro_day - INICIO_day) %>%
    group_by(Folio) %>%
    mutate(`Duración (días)` = diff_days - lag(diff_days)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(`Duración (días)` = replace_na(`Duración (días)`, diff_days)) %>%
    ungroup() %>%
    mutate(Estatus  = str_remove_all(Estatus, "\\s\\(\\d+\\)")) %>%
    mutate(Etapa    = str_replace_all(Estatus,
                                      c(
                                        "Aclaración ROCE derivado de OC"              = "Aclaración de información OC",
                                        "Aclaración de información OC"                = "Aclaración de información OC",
                                        "Aclaración de información Revisión ROCE"     = "Revisión ROCE",
                                        "Aclaración de información parcial"           = "Proceso de llenado",
                                        "En aclaración por reconsulta"                = "Recuperado con firma y sello",
                                        "En proceso de firma y sello"                 = "Recuperado con firma y sello",
                                        "En proceso de firma y sello por reconsulta"  = "Recuperado con firma y sello",
                                        "En proceso de llenado"                       = "Proceso de llenado",
                                        "Recuperacion parcial"                        = "Proceso de llenado",
                                        "Recuperado con firma y sello"                = "Recuperado con firma y sello",
                                        "Recuperado con firma y sello por reconsulta" = "Recuperado con firma y sello",
                                        "Revisión OC"                                 = "Validación OC",
                                        "Revisión ROCE"                               = "Revisión ROCE",
                                        "Revisión ROCE derivado de OC"                = "Revisión ROCE derivado de OC"
                                      )
    )) %>%
    mutate(Etapa = fct(Etapa, levels = c(
      "Recuperado con firma y sello",
      "Revisión ROCE derivado de OC",
      "Aclaración de información OC",
      "Validación OC",
      "Revisión ROCE",
      "Proceso de llenado"
    ))) %>%
    select(Módulo, Censo, Entidad, Etapa, `Duración (días)`) %>%
    filter(Censo == var_census) %>%
    count(Censo, Módulo, Entidad, Etapa, wt = `Duración (días)`, name = "Duración (días)") %>%
    filter(Entidad %in% var_entities) %>%
    count(
      Censo,
      Módulo,
      Etapa,
      wt = round(mean(`Duración (días)`, na.rm = TRUE), digits = 1),
      name = "Duración promedio (días)"
    )

  aux_plot <- aux_df %>%
    ggplot(aes(label1 = Censo)) +
    geom_col(
      aes(
        x = Módulo,
        y = `Duración promedio (días)`,
        fill = Etapa,
        text = paste("Módulo: ", Módulo, "</br>Etapa: ", Etapa, "</br>Duración promedio (días): ", `Duración promedio (días)`)
      )
    ) +
    scale_fill_manual(
      name = NULL,
      values = c(
        "Recuperado con firma y sello" = "#F2F2F2",
        "Revisión ROCE derivado de OC" = "#7F7F7F",
        "Aclaración de información OC" = "#404040",
        "Validación OC"                = "#1BA3E2",
        "Revisión ROCE"                = "#0076C8",
        "Proceso de llenado"           = "#003056"
      )
    ) +
    theme_classic()

  return(ggplotly(aux_plot, tooltip = c("label1", "text")))
}

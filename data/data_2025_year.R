# Default databases -------------------------------------------------------

source("data/reviewer_team.R")
source("data/federal_entities.R")

# Census
t_census <- tibble(
  Censo = c(
    "CNGE",
    "CNSPE",
    "CNSIPEE",
    "CNPJE",
    "CNIJE",
    "CNPLE",
    "CNDHE",
    "CNTAIPPDPE"
  ) %>% factor() %>% fct_inorder()
)

# Assignment number to census
num_to_census <- (`names<-`(levels(t_census[[1]]), as.character(nrow(t_census) %>% seq())))

# Database DIIE dates current year (update every year!).
DIIE_dates <- tibble(
  name = t_census$Censo,

  `start CE` = ymd(
    c(
      "2025-06-16",
      "2025-02-17",
      "2025-02-10",
      "2025-03-17",
      "2025-05-08",
      "2025-07-22",
      "2025-07-15",
      "2025-07-22"
    )
  ),

  `end CE` = ymd(
    c(
      "2025-09-05",
      "2025-05-09",
      "2025-04-25",
      "2025-06-27",
      "2025-08-15",
      "2025-09-27",
      "2025-09-20",
      "2025-09-27"
    )
  ),

  `start DIIE` = ymd(
    c(
      "2025-08-11",
      "2025-04-07",
      "2025-03-24",
      "2025-05-12",
      "2025-06-30",
      "2025-09-02",
      "2025-09-02",
      "2025-09-02"
    )
  ),

  `end DIIE` = ymd(
    c(
      "2025-09-26",
      "2025-06-06",
      "2025-05-23",
      "2025-07-25",
      "2025-09-05",
      "2025-10-11",
      "2025-10-04",
      "2025-10-11"
    )
  ),

  prosecution = ymd(
    c(
      "2025-09-29",
      "2025-06-09",
      "2025-05-12",
      "2025-07-21",
      "2025-09-08",
      "2025-10-14",
      "2025-10-07",
      "2025-10-14"
    )
  ),

  diffusion = ymd(
    c(
      "2025-12-04",
      "2025-08-14",
      "2025-07-17",
      "2025-10-02",
      "2025-11-06",
      "2025-12-13",
      "2025-12-13",
      "2025-12-13"
    )
  )
)

# Database on questionnaires (omit '1501' exclusive for CDMX, update every year!).
questionnaires <- tibble(
  Cuestionarios = c(
    "1101", "1102", "1103", "1104", "1105", "1106", "1107", "1108", "1109", "1110", "1111", "1112", "1201", "1301", "1401", "1501", "1601", "1701", "1702", "1703",
    "2101", "2201",
    "3101", "3201",
    "4101", "4201", "4301", "4401", "4501",
    "5101", "5201", "5301", "5401", "5501", "5601", "5701",
    "6101", "6201",
    "7101", "7201",
    "8101", "8201",  "8301"
  )
)

# Module count by census.
module_count <- questionnaires %>%
  transmute(Cuestionario = str_sub(Cuestionarios, 1, 2)) %>%
  transmute(Cuestionario = as.double(Cuestionario)) %>%
  count(Cuestionario) %>%
  transmute(Censo = Cuestionario %/% 10) %>%
  count(Censo, name = "n_modulos") %>%
  mutate(Censo = str_replace_all(Censo, num_to_census))

# Database on all folios except 091501 (update every year!).
pre_id_folio <- federal_entities %>%
  transmute(id_estado = as.character(id_estado)) %>%
  pull() %>%
  map(~str_c(., pull(questionnaires %>% filter(!Cuestionarios == '1501')))) %>%
  unlist() %>%
  tibble(Folio = .)

# Add folio '091501' exclusive for CDMX
id_091401 <- which(pre_id_folio$Folio == '091401')
id_folio <- add_row(pre_id_folio, Folio = '091501', .after = id_091401)

# Databases on everybody "Folios" extended version (update every year!).
id_folio_extended <- id_folio %>%
  separate(Folio, into = c("id_estado", "Censo_n", "Módulo"), sep = c(2, 3), remove = FALSE) %>%
  mutate(Censo = str_replace_all(Censo_n, num_to_census)) %>%
  mutate(id_estado = factor(id_estado, levels = levels(federal_entities[["id_estado"]]))) %>%
  mutate(Censo_n   = factor(Censo_n  , levels = nrow(t_census) %>% seq())) %>%
  mutate(Censo     = factor(Censo    , levels = levels(t_census$Censo))) %>%
  left_join(federal_entities, by = "id_estado") %>%
  select(-Abreviatura)

# Non-working days (update every year!).
holidays <- tibble(
  `Días Festivos` = ymd(c(
    "2025-01-01", "2025-02-03", "2025-03-17", "2025-04-17", "2025-04-18",
    "2025-05-01", "2025-05-05", "2025-07-08", "2025-09-16", "2025-11-02",
    "2025-11-17", "2025-12-25"
  ))
)

# Database dates current year. Attention in the year! (update every year!).
dates_current_year <- tibble(Registro = (ymd("2025-01-01") + c(0:364)))

# Database not-working days (update every year!).
nonworking_days <- dates_current_year %>%
  mutate(n = wday(Registro, week_start = 1)) %>%
  filter(n > 5 | Registro %in% pull(holidays)) %>%
  pull(Registro)

# Database function to get workday.
get_workday <- function(fecha, nw_days) {
  while (fecha %in% nw_days) {
    fecha <- fecha + 1
  }
  return(fecha)
}

# Database class Tibble con fechas del año y ajustadas a días efectivos (update every year!).
working_dates <- dates_current_year %>%
  pull() %>%
  map_vec(get_workday, nw_days = nonworking_days) %>%
  tibble(aux_var = .) %>%
  bind_cols(dates_current_year) %>%
  relocate(Registro)

# Database function to get folios with status "No aplica".
DT_folio_no_aplica <- function(principal_dataframe) {

  principal_dataframe %>%
    select(Folio, Estatus) %>%
    group_by(Folio) %>%
    mutate(n = seq_along(Folio)) %>%
    slice_max(n) %>%
    ungroup() %>%
    filter(str_detect(Estatus, "No aplica")) %>%
    select(Folio)

}


# Default data bases in previous year -------------------------------------

# Database DIIE dates previous year (update every year!).
DIIE_dates_previous_year <- tibble(
  name = t_census$Censo,

  `start CE` = ymd(
    c(
      "2024-05-13",
      "2024-03-04",
      "2024-02-19",
      "2024-03-11",
      "2024-05-06",
      "2024-07-22",
      "2024-07-15",
      "2024-07-22"
    )
  ),

  `end CE` = ymd(
    c(
      "2024-08-02",
      "2024-05-03",
      "2024-04-19",
      "2024-06-07",
      "2024-08-09",
      "2024-09-27",
      "2024-09-20",
      "2024-09-27"
    )
  ),

  `start DIIE` = ymd(
    c(
      "2024-07-01",
      "2024-04-15",
      "2024-04-01",
      "2024-05-06",
      "2024-06-28",
      "2024-09-02",
      "2024-09-02",
      "2024-09-02"
    )
  ),

  `end DIIE` = ymd(
    c(
      "2024-08-30",
      "2024-06-14",
      "2024-05-31",
      "2024-07-19",
      "2024-09-06",
      "2024-10-11",
      "2024-10-04",
      "2024-10-11"
    )
  ),

  prosecution = ymd(
    c(
      "2024-08-05",
      "2024-05-06",
      "2024-04-22",
      "2024-06-03",
      "2024-08-26",
      "2024-10-14",
      "2024-10-07",
      "2024-10-14"
    )
  ),

  diffusion = ymd(
    c(
      "2024-12-02",
      "2024-08-07",
      "2024-07-18",
      "2024-10-03",
      "2024-11-04",
      "2024-12-13",
      "2024-12-13",
      "2024-12-13"
    )
  )
)

# Non-working days in previous year (update every year!).
holidays_previous_year <- tibble(
  `Días Festivos` = ymd(c(
    "2024-01-01", "2024-02-05", "2024-03-18", "2024-03-28", "2024-03-29",
    "2024-05-01", "2024-05-05", "2024-07-08", "2024-09-16", "2024-10-01",
    "2024-11-02", "2024-11-18", "2024-12-25"
  ))
)

# Database days previous year. Attention in the year! (update every year!).
dates_previous_year <- tibble(Registro = (ymd("2024-01-01") + c(0:365)))

# Database not-working days previous year.
nonworking_days_previous_year <- dates_previous_year %>%
  mutate(n = wday(Registro, week_start = 1)) %>%
  filter(n > 5 | Registro %in% pull(holidays_previous_year)) %>%
  pull(Registro)

# Database class Tibble con fechas del año y ajustadas a días efectivos.
working_dates_previous_year <- dates_previous_year %>%
  pull() %>%
  map_vec(get_workday, nw_days = nonworking_days_previous_year) %>%
  tibble(aux_var = .) %>%
  bind_cols(dates_previous_year) %>%
  relocate(Registro)

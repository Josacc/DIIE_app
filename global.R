# DIIE app
# by Josacc

library(tidyverse)
library(readxl)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shiny)
library(shinyFeedback)
library(DT)
library(shinyfullscreen)
library(shinyauthr)
library(shinymanager)
library(shinythemes)
library(tools)
library(patchwork)
library(scales)

# Settings for shinyapss.io and Shiny Server
options(
  encoding               = "UTF-8",
  rsconnect.locale.cache = FALSE,
  rsconnect.locale       = "es_MX.utf8"
)
Sys.setlocale(locale = "es_MX.utf8")


source("user_base/application_user_base.R")
source("user_base/DIIE_user_base.R")
source("data/data_2025_year.R")
source("functions/data_and_update.R")
source("functions/top_ten_questions.R")
source("functions/entities_vs_observations.R")
source("functions/db_q_aclaracion_oc.R")
source("functions/others_functions.R")

set_labels(
  language              = "es",
  "Please authenticate" = "",
  "Username:"           = "Usuario:",
  "Password:"           = "Contraseña:",
  "Login"               = "Ingresar"
)


# Data base previous year
database_previous_year <- data_and_update("historial_seguimiento/xIktan_20231005104808909_reporteSegumiento.xlsx")[[1]]

# Folios "No aplica" previous year
vec_folios_no_aplica_previous_year <- DT_folio_no_aplica(database_previous_year) %>%
  pull()

# database_2023 <-
database_questionnaires_previous_year <- database_previous_year %>%
  filter(str_detect(Estatus, "Revisión OC"), Perfil == "RESPONSABLE OPERATIVO") %>%
  left_join(working_dates_previous_year, by = "Registro") %>% # Se modificó "Registro" para considerar solo días hábiles.
  select(-Registro) %>%
  rename(Registro = aux_var) %>%
  filter(!(Folio %in% vec_folios_no_aplica_previous_year))

# DIIE app
# by Josacc

library(tidyverse)
library(readxl)
library(rlang)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(DT)
library(shinyauthr)
library(shinymanager)
library(shinyjs)
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


set_labels(
  language              = "es",
  "Please authenticate" = "",
  "Username:"           = "Usuario:",
  "Password:"           = "Contraseña:",
  "Login"               = "Ingresar"
)

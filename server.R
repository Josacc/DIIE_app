library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(tools)
library(DT)
library(shinydashboard)
library(shinyauthr)
library(shinymanager)

source("functions/datatable_something.R")

function(input, output, session) {

  res_auth <- secure_server(
    check_credentials = check_credentials(application_user_base),
    timeout           = 20
  )

  auth <- callModule(
    module = auth_server,
    id     = "auth",
    check_credentials = check_credentials(application_user_base)
  )

  # CNGAE 2024
  CNGAE_current_year_Server('id_CNGAE_current_year')

}

library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(tools)
library(DT)
library(shinydashboard)
library(shinyauthr)
library(shinymanager)

source("modules/actualizacion.R")
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


# CNGAE 2023 --------------------------------------------------------------------

  CNGAE_current_year_Server('id_CNGAE_current_year')


# INTERNO --------------------------------------------------------------

  credentials <- loginServer(
    id       = "login",
    data     = DIIE_user_base,
    user_col = user,
    pwd_col  = password,
    log_out  = reactive(logout_init())
  )

  logout_init <- logoutServer(
    id     = "logout",
    active = reactive(credentials()$user_auth)
  )

  output$diie_interno <- renderUI({
    req(credentials()$user_auth)
    tabsetPanel(
      type = "pills",
      tabPanel(
        "Revisiones por OC",
        br(),
        br(),
        h4(
          p(strong("Tabulado de revisiones efectuadas por OC")),
          style = "color: #3c8dbc; margin: 0rem; margin-top: -1rem; margin-bottom: 3rem;"
        ),
        p(strong("NA: "), "cuestionario no aplica"),
        p(strong("NR: "), "cuestionario no revisado"),
        br(),
        DTOutput("table_q_aclaracion_oc"),
        br(),
        strong(textOutput("id_celdas_seleccionadas")),
        fluidRow(
          column(
            width = 12,
            actionButton(
              "id_bttn_clear_selection",
              label = "Limpiar selección",
              icon  = icon("broom"),
              class = "btn_custom_interno_1"
            ),
            actionButton(
              "id_bttn_filter_obs",
              label = "Filtrar",
              icon  = icon("filter"),
              class = "btn_custom_interno_2"
            )
          )
        ),
        br(),
        br(),
        br(),
        DTOutput("data_q_aclaracion_oc"),
        br(),
        br(),
        br(),
        br()
      ),
      tabPanel(
        "Ranking de entidades por preguntas observadas",
        br(),
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 2,
            radioButtons(
              "id_obs_vs_census_2023",
              "Nivel de análisis",
              choices = c("GLOBAL", levels(DIIE_dates[[1]]))
            )
          ),
          mainPanel(
            style = "height: 500px",
            width = 10,
            actionBttn(
              inputId = "info_button_obs_enviadas_OC",
              label   = "",
              icon    = icon("info-circle"),
              style   = "jelly"
            ),
            br(),
            br(),
            plotlyOutput(
              "plot_obs_vs_census_2023"
            )
          )
        )
      )
    )
  })

  # Ranking entidades
  reactive_obs_vs_census_2023 <- reactive({
    switch (input$id_obs_vs_census_2023,
            GLOBAL     = plot_entities_vs_obs(data()[[2]]),
            CNGE       = plot_entities_vs_obs_grid(data()[[2]], "CNGE"),
            CNSPE      = plot_entities_vs_obs_grid(data()[[2]], "CNSPE"),
            CNSIPEE    = plot_entities_vs_obs_grid(data()[[2]], "CNSIPEE"),
            CNPJE      = plot_entities_vs_obs_grid(data()[[2]], "CNPJE"),
            CNIJE      = plot_entities_vs_obs_grid(data()[[2]], "CNIJE"),
            CNPLE      = plot_entities_vs_obs_grid(data()[[2]], "CNPLE"),
            CNDHE      = plot_entities_vs_obs_grid(data()[[2]], "CNDHE"),
            CNTAIPPDPE = plot_entities_vs_obs_grid(data()[[2]], "CNTAIPPDPE")
    )
  })

  # Ranking entidades
  output$plot_obs_vs_census_2023 <- renderPlotly({
    req(credentials()$user_auth)
    validate(need(reactive_obs_vs_census_2023(), "Sin observaciones"))
    reactive_obs_vs_census_2023()
  })

  # Info Ranking entidades.
  observeEvent(input$info_button_obs_enviadas_OC, {
    req(credentials()$user_auth)
    show_alert(
      session = session,
      title   = "",
      text    = tags$div(
        tags$h3("Información",
                style = "color: #0076C8; font-weight: bold; text-align: center"),
        tags$br(),
        tags$br(),
        `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
        "La gráfica muestra por Censo el ranking de Entidades
        respecto a la cantidad de preguntas que fueron observadas exclusivamente
        por los responsables de revisión de OC."
      ),
      html  = TRUE,
      width = "35%"
    )
  })

  # Table questionnaires aclaracion oc
  output$table_q_aclaracion_oc <- renderDT({
    req(credentials()$user_auth)
    db_q_aclaracion_oc(data()[[1]], c("8101", "8201", "8301"))$datatable
  }, server = FALSE)

  bttn_filter_obs <- eventReactive(input$id_bttn_filter_obs, {
    input$table_q_aclaracion_oc_cells_selected
  })

  observeEvent(input$id_bttn_clear_selection, {
    proxy <- dataTableProxy("table_q_aclaracion_oc")
    selectCells(proxy, NULL)
  })

  output$id_celdas_seleccionadas <- renderText({
    paste0(
      "Celdas seleccionadas: ",
      nrow(input$table_q_aclaracion_oc_cells_selected)
    )

  })

  output$data_q_aclaracion_oc = renderDT({
    req(credentials()$user_auth)

    db_q_aclaracion_oc_filter(
      db_q_aclaracion_oc(data()[[1]], c("8101", "8201", "8301"))$data,
      db_q_aclaracion_oc(data()[[1]], c("8101", "8201", "8301"))$table,
      bttn_filter_obs()
    ) %>%
      datatable_q_aclaracion_oc_filter()
  })

}

# 'interno' module --------------------------------------------------------

interno_UI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Interno",
    icon = icon("square-poll-vertical"),
    div(
      class = "pull-right",
      logoutUI(
        id    = ns("logout"),
        label = "",
        icon  = icon("sign-out-alt")
      )
    ),
    loginUI(
      id            = ns("login"),
      title         = "",
      user_title    = "Usuario",
      pass_title    = "Contraseña",
      login_title   = "Iniciar sesión",
      error_message = "¡Usuario o contraseña no válidos!",
    ),
    uiOutput(ns("diie_interno"))
  )
}

interno_Server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    credentials <- loginServer(
      id       = 'login',
      data     = DIIE_user_base,
      user_col = user,
      pwd_col  = password,
      log_out  = reactive(logout_init())
    )

    logout_init <- logoutServer(
      id     = 'logout',
      active = reactive(credentials()$user_auth)
    )

    output$diie_interno <- renderUI({
      req(credentials()$user_auth)
      tabsetPanel(
        type = "pills",
        tabPanel(
          "Revisiones por OC",
          br(), br(),
          h4(
            p(strong("Tabulado de revisiones efectuadas por OC")),
            style = "color: #3c8dbc; margin: 0rem; margin-top: -1rem; margin-bottom: 3rem;"
          ),
          p(strong("NA: "), "cuestionario no aplica"),
          p(strong("NR: "), "cuestionario no revisado"),
          br(),
          DTOutput(ns("table_q_aclaracion_oc")),
          br(),
          strong(textOutput(ns("id_celdas_seleccionadas"))),
          br(),
          fluidRow(
            column(
              width = 12,
              actionButton(
                ns("id_bttn_filter_obs"),
                label = 'Filtrar celdas seleccionadas',
                icon  = icon("filter"),
                class = "btn_custom_interno_2"
              ),
              actionButton(
                ns("id_bttn_clear_selection"),
                label = "Limpiar selección",
                icon  = icon("broom"),
                class = "btn_custom_interno_1"
              )
            )
          ),
          br(), br(), br(),
          DTOutput(ns("data_q_aclaracion_oc")),
          br(), br(), br(), br()
        ),
        tabPanel(
          "Ranking de entidades por preguntas observadas",
          br(), br(),
          sidebarLayout(
            sidebarPanel(
              width = 2,
              radioButtons(
                ns("id_obs_vs_census_2023"),
                "Seleccionar el nivel de análisis",
                choices = c("GLOBAL", levels(pull(t_census)))
              )
            ),
            mainPanel(
              style = "height: 500px",
              width = 10,
              actionBttn(
                inputId = ns("info_button_obs_enviadas_OC"),
                label   = "",
                icon    = icon("info-circle"),
                style   = "jelly"
              ),
              br(), br(),
              plotlyOutput(
                ns("plot_obs_vs_census_2023")
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
      db_q_aclaracion_oc(data()[[1]])$datatable
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
        db_q_aclaracion_oc(data()[[1]])$data,
        db_q_aclaracion_oc(data()[[1]])$table,
        bttn_filter_obs()
      ) %>%
        datatable_q_aclaracion_oc_filter()
    })
  })
}

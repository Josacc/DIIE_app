# 'En proceso de firma y sello (1)' module --------------------------------

proceso_firma_sello1_UI <- function(id) {
  tabPanel(
    "En proceso de firma y sello (1)",
    tabsetPanel(
      type = "pills",
      tabPanel(
        "Cuestionarios en proceso de firma y sello (1)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 12,
            fluidRow(
              # Controlador por semana o día
              column(
                width = 2,
                awesomeRadio(
                  inputId = NS(id, "id_controller_plot_semana_day"),
                  label = "Segmentar",
                  choices = c("Por semana", "Por día")
                )
              ),
              # TabsetPanel por semana y por día.
              column(
                width = 10,
                tabsetPanel(
                  id = NS(id, "id_plot_questionnaries_firma_sello_select"),
                  type = "hidden",
                  # Panel por semana
                  tabPanel(
                    "Por semana",
                    selectInput(
                      NS(id, "id_questionnaires_firma_sello_census"),
                      "Censo",
                      choices = c("GLOBAL", levels(DIIE_dates[[1]])),
                      width = "150px"
                    ),
                    p(strong("Acumulado de cuestionarios en proceso de firma y sello (1): "),
                      strong(textOutput(NS(id, "text_count_firma_sello_census"), inline = TRUE)),
                      style = "color: #a71106")
                  ),
                  # Panel por día
                  tabPanel(
                    "Por día",
                    sliderInput(
                      NS(id, "id_slider_date_questionnaires_firma_sello"),
                      label = "Línea de tiempo",
                      min   = DIIE_dates[[3, 2]], # CNSIPEE start CE
                      max   = tail(pull(DIIE_dates), 1), # last diffusion
                      value = c(
                        DIIE_dates[[3, 2]],
                        tail(pull(DIIE_dates), 1)
                      ),
                      step  = days(1)
                    ),
                    tabsetPanel(
                      id = NS(id, "id_text_questionnaires_firma_sello_range"),
                      type = "hidden",
                      tabPanel(
                        "accumulated",
                        p(
                          strong("Acumulado de cuestionarios en proceso de firma y sello (1): "),
                          strong(textOutput(NS(id, "text_count_firma_sello_accumulated"), inline = TRUE)),
                          style = "color: #a71106"
                        )
                      ),
                      tabPanel(
                        "range",
                        p(
                          strong("Rango de cuestionarios en proceso de firma y sello (1): "),
                          strong(textOutput(NS(id, "text_count_firma_sello_range"), inline = TRUE)),
                          style = "color: #5fa4cc"
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          mainPanel(
            width = 12,
            tabsetPanel(
              id = NS(id, "id_plot_DT_questionnaries_firma_sello_select"),
              type = "hidden",
              tabPanel(
                "Por semana",
                tabsetPanel(
                  id = NS(id, "id_plot_DT"),
                  type = "hidden",
                  tabPanel(
                    "global",
                    plotlyOutput(
                      NS(id, "plot_questionnaires_firma_sello_week_global")
                    )
                  ),
                  tabPanel(
                    "census",
                    fluidRow(
                      column(
                        width = 5,
                        plotlyOutput(
                          NS(id, "plot_questionnaires_firma_sello_week_census")
                        )
                      ),
                      column(
                        width = 7,
                        dataTableOutput(NS(id, "table_questionnaires_set_free_census"))
                      )
                    )
                  )
                )
              ),
              tabPanel(
                "Por día",
                fluidRow(
                  column(
                    width = 6,
                    plotlyOutput(
                      NS(id, "plot_questionnaires_firma_sello_day")
                    )
                  ),
                  column(
                    width = 6,
                    dataTableOutput(NS(id, "table_questionnaires_set_free_registro"))
                  )
                )
              )
            )
          )
        ),
        br(),
      ),
      tabPanel(
        "Cuestionarios en proceso de firma y sello (1) por entidad",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput(
              NS(id, "id_questionnaires_firma_sello_entity"),
              "Entidad",
              choices = c("NACIONAL", levels(entities[[1]]))
            )
          ),
          mainPanel(
            width = 12,
            plotlyOutput(
              NS(id, "plot_questionnaires_firma_sello_entity"),
              height = "700px"
            )
          )
        )
      )
    )
  )
}

proceso_firma_sello1_Server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Database on questionnaires with status "firma y sello(1)"
    database_firma_sello <- reactive({

      vector_folios_no_aplica <- DT_folio_no_aplica(data()[[1]]) %>%
        pull()

      .data <- data()[[1]] %>%
        filter(str_detect(Estatus, "En proceso de firma y sello \\(1\\)")) %>%
        left_join(working_dates, by = "Registro") %>% # Se modificó "Registro" para considerar solo días hábiles.
        select(-Registro) %>%
        rename(Registro = aux_var) %>%
        filter(!(Folio %in% vector_folios_no_aplica))

      if (nrow(.data) == 0) {
        return(NULL)
      }

      return(.data)
    })


    # Cuestionarios en proceso de firma y sello (1)
    # Change control on sidebar panel: DT and plots by "semana" and "día".
    observeEvent(input$id_controller_plot_semana_day, {
      updateTabsetPanel(session, "id_plot_questionnaries_firma_sello_select", input$id_controller_plot_semana_day)
    })

    # Change control on main panel: DT and plots by "semana" and "día".
    observeEvent(input$id_controller_plot_semana_day, {
      updateTabsetPanel(session, "id_plot_DT_questionnaries_firma_sello_select", input$id_controller_plot_semana_day)
    })

    # Observe event plot global and plot by census and DT
    observeEvent(input$id_questionnaires_firma_sello_census, {
      if (input$id_questionnaires_firma_sello_census == "GLOBAL") {
        return(updateTabsetPanel(session, inputId = "id_plot_DT", selected = "global"))
      }
      return(updateTabsetPanel(session, inputId = "id_plot_DT", selected = "census"))
    })

    # Plot global
    reactive_questionnaires_firma_sello_week_global <- reactive({
      req(database_firma_sello())
      plot_questionnaires_firma_sello_week(database_firma_sello())
    })

    output$plot_questionnaires_firma_sello_week_global <- renderPlotly({
      validate(need(reactive_questionnaires_firma_sello_week_global(),
                    "Sin cuestionarios en proceso de firma y sello (1)"))
      reactive_questionnaires_firma_sello_week_global()
    })

    # Plot and DT by census
    reactive_questionnaires_firma_sello_week_census <- reactive({
      req(database_firma_sello())
      plot_questionnaires_firma_sello_week_project(database_firma_sello(), input$id_questionnaires_firma_sello_census)
    })

    output$plot_questionnaires_firma_sello_week_census <- renderPlotly({
      validate(need(reactive_questionnaires_firma_sello_week_census(),
                    "Sin cuestionarios en proceso de firma y sello (1)"))
      reactive_questionnaires_firma_sello_week_census()
    })

    output$table_questionnaires_set_free_census <- renderDataTable({
      validate(need(reactive_questionnaires_firma_sello_week_census(), ""))
      datatable(
        DT_questionnaires_firma_sello_census(database_firma_sello(), input$id_questionnaires_firma_sello_census),
        rownames = FALSE,
        options  = list(
          pageLength = 5,
          language   = list(
            url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
          )
        )
      )
    })

    # Count set free questionnaires by time range
    output$text_count_firma_sello_census <- renderText({
      count_questionnaires_firma_sello(
        database_firma_sello(),
        .census = input$id_questionnaires_firma_sello_census,
        .max    = dmy_hms(data()[[3]]) %>% as_date()
      )
    })

    # Update sliderInput; change value
    observeEvent(!is_null(data()), {
      updateSliderInput(
        inputId = "id_slider_date_questionnaires_firma_sello",
        max = dmy_hms(data()[[3]]) %>% as_date(),
        value = c(
          DIIE_dates[[3, 2]],
          dmy_hms(data()[[3]]) %>% as_date()
        )
      )
    })

    # Change value of count set free questionnaires by day
    observeEvent(input$id_slider_date_questionnaires_firma_sello, {
      if ((input$id_slider_date_questionnaires_firma_sello)[1] == DIIE_dates[[3, 2]] &&
          (input$id_slider_date_questionnaires_firma_sello)[2] == dmy(word(data()[[3]], 1))) {
        return(updateTabsetPanel(session, inputId = "id_text_questionnaires_firma_sello_range", selected = "accumulated"))
      }
      return(updateTabsetPanel(session, inputId = "id_text_questionnaires_firma_sello_range", selected = "range"))
    })

    output$text_count_firma_sello_accumulated <- renderText({
      count_questionnaires_firma_sello(
        database_firma_sello(),
        .census = "GLOBAL",
        .max    = (input$id_slider_date_questionnaires_firma_sello)[2]
      )
    })

    output$text_count_firma_sello_range <- renderText({
      count_questionnaires_firma_sello(
        database_firma_sello(),
        .census = "GLOBAL",
        .min    = (input$id_slider_date_questionnaires_firma_sello)[1],
        .max    = (input$id_slider_date_questionnaires_firma_sello)[2]
      )
    })

    # Plots on questionnnaries set free everyday.
    output$plot_questionnaires_firma_sello_day <- renderPlotly({
      req(database_firma_sello())
      plot_questionnaires_firma_sello(database_firma_sello())
    })

    output$table_questionnaires_set_free_registro <- renderDataTable({
      req(database_firma_sello())
      datatable(
        DT_questionnaires_firma_sello_registro(database_firma_sello()),
        rownames = FALSE,
        filter   = "top",
        options  = list(
          pageLength = 5,
          language   = list(
            url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
          )
        ),
        escape = FALSE
      )
    })

    # Cuestionarios en proceso de firma y sello (1) por entidad
    reactive_questionnaires_firma_sello_entity <- reactive({
      req(database_firma_sello())
      if (input$id_questionnaires_firma_sello_entity == "NACIONAL") {
        return(plot_questionnaires_firma_sello_light(database_firma_sello()))
      }

      plot_questionnaires_firma_sello_light_entities(database_firma_sello(), input$id_questionnaires_firma_sello_entity)
    })

    output$plot_questionnaires_firma_sello_entity <- renderPlotly({
      validate(need(reactive_questionnaires_firma_sello_entity(), "Sin cuestionarios en proceso de firma y sello (1)"))
      reactive_questionnaires_firma_sello_entity()
    })

  })
}

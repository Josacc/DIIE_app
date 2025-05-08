# 'revisión oc' module ----------------------------------------------------

revision_oc_UI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Revisión OC",
    tabsetPanel(
      type = "pills",
      tabPanel(
        "Cuestionarios enviados a revisión OC por entidad",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput(
              ns("id_questionnaires_vs_entities"),
              "Seleccionar la entidad",
              choices = c("NACIONAL", levels(entities[[1]]))
            )
          ),
          mainPanel(
            width = 12,
            plotlyOutput(
              ns("plot_arrival_questionnaires_entitie"),
              height = "700px"
            )
          )
        ),
        br()
      ),
      tabPanel(
        "Cuestionarios enviados a revisión OC",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 12,
            sliderInput(
              ns("id_slider_date_questionnaires"),
              label = "Línea de tiempo por semana",
              min   = floor_date(DIIE_dates %>% filter(name == "CNSIPEE") %>% select(`start CE`) %>% .[[1]], "week", week_start = 1) + weeks(3), # CNSIPEE start CE
              max   = ceiling_date(tail(DIIE_dates$diffusion, 1), "week", week_start = 1), # last diffusion
              value = floor_date(DIIE_dates %>% filter(name == "CNSIPEE") %>% select(`start CE`) %>% .[[1]], "week", week_start = 1) + weeks(3),
              step  = weeks(1),
              animate = TRUE
            ),
            p(strong("Cantidad de cuestionarios enviados a revisión OC por semana: "),
              strong(textOutput(ns("text_count_questionnaires"), inline = TRUE)),
              style = "color: #3c8dbc")
          ),
          mainPanel(
            style = "height: 400px",
            width = 12,
            plotlyOutput(
              ns("plot_arrival_questionnaires"),
              height = "400px"
            )
          )
        ),
        br()
      )#,
      # tabPanel(
      #   "Comparativo global 2023 vs 2024",
      #   br(),
      #   sidebarLayout(
      #     sidebarPanel(
      #       width = 12,
      #       sliderInput(
      #         ns("id_slider_date_questionnaires_weeks"),
      #         label = "Línea de tiempo por semana",
      #         min   = 1,
      #         max   = 30,
      #         value = 1,
      #         step  = 1,
      #         animate = TRUE
      #       ),
      #       p(strong("Año 2024, cantidad de cuestionarios enviados a revisión OC por semana: "),
      #         strong(textOutput(ns("text_count_questionnaires_weeks"), inline = TRUE)),
      #         style = "color: #3c8dbc"
      #       ),
      #       p(strong("Año 2023, cantidad de cuestionarios enviados a revisión OC por semana: "),
      #         strong(textOutput(ns("text_count_questionnaires_weeks_previous_year"), inline = TRUE)),
      #         style = "color: #5fa4cc"
      #       )
      #     ),
      #     mainPanel(
      #       style = "height: 400px",
      #       width = 12,
      #       fluidRow(
      #         column(
      #           width = 6,
      #           plotlyOutput(
      #             ns("plot_arrival_questionnaires_weeks"),
      #             height = "400px"
      #           )
      #         ),
      #         column(
      #           width = 6,
      #           plotlyOutput(
      #             ns("plot_arrival_questionnaires_weeks_previous_year"),
      #             height = "400px"
      #           )
      #         )
      #       )
      #     )
      #   ),
      #   br()
      # )
    )
  )
}

revision_oc_Server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    database_questionnaires <- reactive({
      vector_folios_no_aplica <- DT_folio_no_aplica(data()[[1]]) %>%
        pull()

      .data <- data()[[1]] %>%
        filter(str_detect(Estatus, "Revisión OC"), Perfil == "RESPONSABLE OPERATIVO") %>%
        left_join(working_dates, by = "Registro") %>% # Se modificó "Registro" para considerar solo días hábiles.
        select(-Registro) %>%
        rename(Registro = aux_var) %>%
        filter(!(Folio %in% vector_folios_no_aplica))

      if (nrow(.data) == 0) {
        return(NULL)
      }

      return(.data)
    })

    # Cuestionarios enviados a revisión OC
    output$text_count_questionnaires <- renderText({
      validate(need(database_questionnaires(), "0"))
      count_arrival_questionnaires_week(database_questionnaires(), input$id_slider_date_questionnaires)
    })

    output$plot_arrival_questionnaires <- renderPlotly({
      validate(need(database_questionnaires(), "Sin información"))
      x <- input$id_slider_date_questionnaires
      plot_arrival_questionnaires_current_year(database_questionnaires(), x)
    })

    # Comparativo global 2023 vs 2024
    output$text_count_questionnaires_weeks <- renderText({
      validate(need(database_questionnaires(), "0"))
      x <- input$id_slider_date_questionnaires_weeks
      count_arrival_questionnaires_week(database_questionnaires(), ymd("2024-02-19") + weeks(x)) # (update every year!).
    })

    output$text_count_questionnaires_weeks_previous_year <- renderText({
      x <- input$id_slider_date_questionnaires_weeks
      count_arrival_questionnaires_week(database_questionnaires_previous_year, ymd("2023-03-06") + weeks(x)) # (update every year!).
    })

    output$plot_arrival_questionnaires_weeks <- renderPlotly({
      validate(need(database_questionnaires(), "Sin información"))
      x <- input$id_slider_date_questionnaires_weeks
      plot_arrival_questionnaires_current_year(database_questionnaires(), ymd("2024-02-19") + weeks(x), "2024") # (update every year!).
    })

    output$plot_arrival_questionnaires_weeks_previous_year <- renderPlotly({
      x <- input$id_slider_date_questionnaires_weeks
      plot_arrival_questionnaires_previous_year(database_questionnaires_previous_year, ymd("2023-03-06") + weeks(x), "2023") # (update every year!).
    })

    # Cuestionarios enviados a OC por entidad
    max_questionnaries_day <- reactive({
      req(database_questionnaires())
      database_questionnaires() %>%
        count(Registro, Censo) %>%
        rename(`Cuestionarios enviados a revisión OC` = names(.)[[3]]) %>%
        group_by(Censo) %>%
        summarise(y_max = max(`Cuestionarios enviados a revisión OC`))
    })

    reactive_arrival_questionnaires_entitie <- reactive({
      req(max_questionnaries_day())
      switch (input$id_questionnaires_vs_entities,
              NACIONAL                          = plot_arrival_questionnaires_grid_census_2023(database_questionnaires(), max_questionnaries_day()),
              `AGUASCALIENTES`                  = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[1]),
              `BAJA CALIFORNIA`                 = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[2]),
              `BAJA CALIFORNIA SUR`             = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[3]),
              `CAMPECHE`                        = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[4]),
              `COAHUILA DE ZARAGOZA`            = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[5]),
              `COLIMA`                          = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[6]),
              `CHIAPAS`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[7]),
              `CHIHUAHUA`                       = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[8]),
              `CIUDAD DE MÉXICO`                = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[9]),
              `DURANGO`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[10]),
              `GUANAJUATO`                      = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[11]),
              `GUERRERO`                        = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[12]),
              `HIDALGO`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[13]),
              `JALISCO`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[14]),
              `MÉXICO`                          = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[15]),
              `MICHOACÁN DE OCAMPO`             = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[16]),
              `MORELOS`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[17]),
              `NAYARIT`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[18]),
              `NUEVO LEÓN`                      = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[19]),
              `OAXACA`                          = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[20]),
              `PUEBLA`                          = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[21]),
              `QUERÉTARO`                       = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[22]),
              `QUINTANA ROO`                    = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[23]),
              `SAN LUIS POTOSÍ`                 = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[24]),
              `SINALOA`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[25]),
              `SONORA`                          = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[26]),
              `TABASCO`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[27]),
              `TAMAULIPAS`                      = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[28]),
              `TLAXCALA`                        = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[29]),
              `VERACRUZ DE IGNACIO DE LA LLAVE` = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[30]),
              `YUCATÁN`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[31]),
              `ZACATECAS`                       = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[32])
      )
    })

    output$plot_arrival_questionnaires_entitie <- renderPlotly({
      validate(need(reactive_arrival_questionnaires_entitie(), "Sin cuestionarios enviados a revisión OC"))
      reactive_arrival_questionnaires_entitie()
    })

  })
}

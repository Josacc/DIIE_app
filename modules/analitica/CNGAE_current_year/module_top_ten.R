# 'top ten' module ----------------------------------------------------

tm_tabPanels <- function(id, censo_name) {

  tabPanel(
    title = censo_name,
    selectInput(
      inputId = NS(id, str_c("id_", censo_name)),
      label   = "Módulo",
      choices = str_c("Módulo ", seq(module_count[module_count$Censo == censo_name, ][[2]]))
    )
  )
}

top_ten_UI <- function(id) {
  tabPanel(
    "Top 10",
    h4(
      p(strong("Top 10 preguntas más observadas")),
      style = "color: #3c8dbc; margin: 0rem; margin-top: -1rem; margin-bottom: 3rem;"
    ),
    sidebarLayout(
      sidebarPanel(fluidRow(
        column(width = 6,
               selectInput(NS(id, "id_top_ten_question_2023"),
                           label = "Censo",
                           choices = levels(DIIE_dates[[1]])
               )
        ),
        column(width = 6,
               do.call(
                 tapsetPanel,
                 c(list(id = NS(id, "id_modulo_select_2023"), type = "hidden"), map(module_count[[1]], ~tm_tabPanels(id, .x)))
               )
               # tabsetPanel(
               #   id = "id_modulo_select_2023",
               #   type = "hidden",
               #   tabPanel(
               #     "CNGE",
               #     selectInput(
               #       "id_CNGE_2023",
               #       label = "Módulo",
               #       choices = str_c(
               #         "Módulo ",
               #         seq(
               #           module_count[module_count$Censo == "CNGE", ][[2]]
               #         )
               #       )
               #     )
               #   ),
               #   tabPanel(
               #     "CNSPE",
               #     selectInput(
               #       "id_CNSPE_2023",
               #       label = "Módulo",
               #       choices = str_c(
               #         "Módulo ",
               #         seq(
               #           module_count[module_count$Censo == "CNSPE", ][[2]]
               #         )
               #       )
               #     )
               #   ),
               #   tabPanel(
               #     "CNSIPEE",
               #     selectInput(
               #       "id_CNSIPEE_2023",
               #       label = "Módulo",
               #       choices = str_c(
               #         "Módulo ",
               #         seq(
               #           module_count[module_count$Censo == "CNSIPEE", ][[2]]
               #         )
               #       )
               #     )
               #   ),
               #   tabPanel(
               #     "CNPJE",
               #     selectInput(
               #       "id_CNPJE_2023",
               #       "Módulo",
               #       choices = str_c(
               #         "Módulo ",
               #         seq(
               #           module_count[module_count$Censo == "CNPJE", ][[2]]
               #         )
               #       )
               #     )
               #   ),
               #   tabPanel(
               #     "CNIJE",
               #     selectInput(
               #       "id_CNIJE_2023",
               #       label = "Módulo",
               #       choices = str_c(
               #         "Módulo ",
               #         seq(
               #           module_count[module_count$Censo == "CNIJE", ][[2]]
               #         )
               #       )
               #     )
               #   ),
               #   tabPanel(
               #     "CNPLE",
               #     selectInput(
               #       "id_CNPLE_2023",
               #       label = "Módulo",
               #       choices = str_c(
               #         "Módulo ",
               #         seq(
               #           module_count[module_count$Censo == "CNPLE", ][[2]]
               #         )
               #       )
               #     )
               #   ),
               #   tabPanel(
               #     "CNDHE",
               #     selectInput(
               #       "id_CNDHE_2023",
               #       label = "Módulo",
               #       choices = str_c(
               #         "Módulo ",
               #         seq(
               #           module_count[module_count$Censo == "CNDHE", ][[2]]
               #         )
               #       )
               #     )
               #   ),
               #   tabPanel(
               #     "CNTAIPPDPE",
               #     selectInput(
               #       "id_CNTAIPPDPE_2023",
               #       label = "Módulo",
               #       choices = str_c(
               #         "Módulo ",
               #         seq(
               #           module_count[module_count$Censo == "CNTAIPPDPE", ][[2]]
               #         )
               #       )
               #     )
               #   )
               # )
        )
      )
      ),
      mainPanel(
        width = 12,
        fluidRow(
          column(width = 5,
                 plotlyOutput(NS(id, "plot_top_ten_questions_2023"))
          ),
          column(width = 7,
                 dataTableOutput(NS(id, "table_top_ten_questions_2023"))
          )
        ),
        br(),
        br(),
        br()
      )
    )
  )
}

top_ten_Server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$id_top_ten_question_2023, {
      updateTabsetPanel(session, "id_modulo_select_2023", selected = input$id_top_ten_question_2023)
    })

    reactive_top_ten_questions_2023 <- reactive({
      switch(input$id_top_ten_question_2023,
             CNGE   = switch(input$id_CNGE_2023,
                             `Módulo 1` = top_ten_questions(data()[[2]], "CNGE", "M1"),
                             `Módulo 2` = top_ten_questions(data()[[2]], "CNGE", "M2"),
                             `Módulo 3` = top_ten_questions(data()[[2]], "CNGE", "M3"),
                             `Módulo 4` = top_ten_questions(data()[[2]], "CNGE", "M4"),
                             `Módulo 5` = top_ten_questions(data()[[2]], "CNGE", "M5")
             ),
             CNSPE   = switch(input$id_CNSPE_2023,
                              `Módulo 1` = top_ten_questions(data()[[2]], "CNSPE", "M1"),
                              `Módulo 2` = top_ten_questions(data()[[2]], "CNSPE", "M2")
             ),
             CNSIPEE = switch(input$id_CNSIPEE_2023,
                              `Módulo 1` = top_ten_questions(data()[[2]], "CNSIPEE", "M1"),
                              `Módulo 2` = top_ten_questions(data()[[2]], "CNSIPEE", "M2")
             ),
             CNPJE   = switch(input$id_CNPJE_2023,
                              `Módulo 1` = top_ten_questions(data()[[2]], "CNPJE", "M1"),
                              `Módulo 2` = top_ten_questions(data()[[2]], "CNPJE", "M2"),
                              `Módulo 3` = top_ten_questions(data()[[2]], "CNPJE", "M3"),
                              `Módulo 4` = top_ten_questions(data()[[2]], "CNPJE", "M4"),
                              `Módulo 5` = top_ten_questions(data()[[2]], "CNPJE", "M5")
             ),
             CNIJE   = switch(input$id_CNIJE_2023,
                              `Módulo 1` = top_ten_questions(data()[[2]], "CNIJE", "M1"),
                              `Módulo 2` = top_ten_questions(data()[[2]], "CNIJE", "M2"),
                              `Módulo 3` = top_ten_questions(data()[[2]], "CNIJE", "M3"),
                              `Módulo 4` = top_ten_questions(data()[[2]], "CNIJE", "M4"),
                              `Módulo 5` = top_ten_questions(data()[[2]], "CNIJE", "M5"),
                              `Módulo 6` = top_ten_questions(data()[[2]], "CNIJE", "M6"),
                              `Módulo 7` = top_ten_questions(data()[[2]], "CNIJE", "M7")
             ),
             CNPLE = switch(input$id_CNPLE_2023,
                            `Módulo 1` = top_ten_questions(data()[[2]], "CNPLE", "M1"),
                            `Módulo 2` = top_ten_questions(data()[[2]], "CNPLE", "M2")
             ),
             CNDHE = switch(input$id_CNDHE_2023,
                            `Módulo 1` = top_ten_questions(data()[[2]], "CNDHE", "M1"),
                            `Módulo 2` = top_ten_questions(data()[[2]], "CNDHE", "M2")
             ),
             CNTAIPPDPE = switch(input$id_CNTAIPPDPE_2023,
                                 `Módulo 1` = top_ten_questions(data()[[2]], "CNTAIPPDPE", "M1"),
                                 `Módulo 2` = top_ten_questions(data()[[2]], "CNTAIPPDPE", "M2"),
                                 `Módulo 3` = top_ten_questions(data()[[2]], "CNTAIPPDPE", "M3")
             )
      )
    })

    output$plot_top_ten_questions_2023 <- renderPlotly({
      validate(need(reactive_top_ten_questions_2023()[[1]], "Sin observaciones"))
      reactive_top_ten_questions_2023()[[1]]
    })

    output$table_top_ten_questions_2023 <- renderDataTable({
      validate(need(reactive_top_ten_questions_2023()[[1]], ""))
      datatable(
        reactive_top_ten_questions_2023()[[2]],
        rownames = FALSE,
        filter   = list(position = "top"),#"top",
        options  = list(
          dom      = "ltipr",
          language = list(
            url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
          )
        )
      )
    })
  })
}

# 'top ten' module ----------------------------------------------------

tm_tabPanels <- function(id, censo_name) {
  ns <- NS(id)
  tabPanel(
    title = censo_name,
    selectInput(
      inputId = ns(str_c("id_", censo_name)),
      label   = "Módulo",
      choices = str_c("Módulo ", seq(module_count[module_count$Censo == censo_name, ][[2]]))
    )
  )
}

top_ten_UI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Top 10",
    h4(
      p(strong("Top 10 preguntas más observadas")),
      style = "color: #3c8dbc; margin: 0rem; margin-top: -1rem; margin-bottom: 3rem;"
    ),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("id_top_ten_question"),
              label   = "Censo",
              choices = levels(DIIE_dates[[1]])
            )
          ),
          column(
            width = 6,
            do.call(
              tabsetPanel,
              c(list(id = ns("id_modulo_select"), type = "hidden"), map(module_count[[1]], ~tm_tabPanels(id, .x)))
            )
          )
        )
      ),
      mainPanel(
        width = 12,
        fluidRow(
          column(
            width = 5,
            plotlyOutput(ns("plot_top_ten_questions"))
          ),
          column(
            width = 7,
            dataTableOutput(ns("table_top_ten_questions"))
          )
        ),
        br(), br(), br()
      )
    )
  )
}

top_ten_Server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$id_top_ten_question, {
      updateTabsetPanel(session, "id_modulo_select", selected = input$id_top_ten_question)
    })

    reactive_top_ten_questions <- reactive({
      switch(input$id_top_ten_question,
             CNGE   = switch(input$id_CNGE,
                             `Módulo 1` = top_ten_questions(data()[[2]], "CNGE", "M1"),
                             `Módulo 2` = top_ten_questions(data()[[2]], "CNGE", "M2"),
                             `Módulo 3` = top_ten_questions(data()[[2]], "CNGE", "M3"),
                             `Módulo 4` = top_ten_questions(data()[[2]], "CNGE", "M4"),
                             `Módulo 5` = top_ten_questions(data()[[2]], "CNGE", "M5")
             ),
             CNSPE   = switch(input$id_CNSPE,
                              `Módulo 1` = top_ten_questions(data()[[2]], "CNSPE", "M1"),
                              `Módulo 2` = top_ten_questions(data()[[2]], "CNSPE", "M2")
             ),
             CNSIPEE = switch(input$id_CNSIPEE,
                              `Módulo 1` = top_ten_questions(data()[[2]], "CNSIPEE", "M1"),
                              `Módulo 2` = top_ten_questions(data()[[2]], "CNSIPEE", "M2")
             ),
             CNPJE   = switch(input$id_CNPJE,
                              `Módulo 1` = top_ten_questions(data()[[2]], "CNPJE", "M1"),
                              `Módulo 2` = top_ten_questions(data()[[2]], "CNPJE", "M2"),
                              `Módulo 3` = top_ten_questions(data()[[2]], "CNPJE", "M3"),
                              `Módulo 4` = top_ten_questions(data()[[2]], "CNPJE", "M4"),
                              `Módulo 5` = top_ten_questions(data()[[2]], "CNPJE", "M5")
             ),
             CNIJE   = switch(input$id_CNIJE,
                              `Módulo 1` = top_ten_questions(data()[[2]], "CNIJE", "M1"),
                              `Módulo 2` = top_ten_questions(data()[[2]], "CNIJE", "M2"),
                              `Módulo 3` = top_ten_questions(data()[[2]], "CNIJE", "M3"),
                              `Módulo 4` = top_ten_questions(data()[[2]], "CNIJE", "M4"),
                              `Módulo 5` = top_ten_questions(data()[[2]], "CNIJE", "M5"),
                              `Módulo 6` = top_ten_questions(data()[[2]], "CNIJE", "M6"),
                              `Módulo 7` = top_ten_questions(data()[[2]], "CNIJE", "M7")
             ),
             CNPLE = switch(input$id_CNPLE,
                            `Módulo 1` = top_ten_questions(data()[[2]], "CNPLE", "M1"),
                            `Módulo 2` = top_ten_questions(data()[[2]], "CNPLE", "M2")
             ),
             CNDHE = switch(input$id_CNDHE,
                            `Módulo 1` = top_ten_questions(data()[[2]], "CNDHE", "M1"),
                            `Módulo 2` = top_ten_questions(data()[[2]], "CNDHE", "M2")
             ),
             CNTAIPPDPE = switch(input$id_CNTAIPPDPE,
                                 `Módulo 1` = top_ten_questions(data()[[2]], "CNTAIPPDPE", "M1"),
                                 `Módulo 2` = top_ten_questions(data()[[2]], "CNTAIPPDPE", "M2"),
                                 `Módulo 3` = top_ten_questions(data()[[2]], "CNTAIPPDPE", "M3")
             )
      )
    })

    output$plot_top_ten_questions <- renderPlotly({
      validate(need(reactive_top_ten_questions()[[1]], "Sin observaciones"))
      reactive_top_ten_questions()[[1]]
    })

    output$table_top_ten_questions <- renderDataTable({
      validate(need(reactive_top_ten_questions()[[1]], ""))
      datatable(
        reactive_top_ten_questions()[[2]],
        rownames = FALSE,
        filter   = list(position = "top"),
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

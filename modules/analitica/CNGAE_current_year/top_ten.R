


navbarMenu(
  "Observaciones",
  icon = icon("square-poll-vertical"),
  tabPanel(
    "Top 10",
    h4(
      p(strong("Top 10 preguntas más observadas")),
      style = "color: #3c8dbc; margin: 0rem; margin-top: -1rem; margin-bottom: 3rem;"
    ),
    sidebarLayout(
      sidebarPanel(fluidRow(
        column(width = 6,
               selectInput("id_top_ten_question_2023",
                           label = "Censo",
                           choices = levels(DIIE_dates[[1]])
               )
        ),
        column(width = 6,
               tabsetPanel(
                 id = "id_modulo_select_2023",
                 type = "hidden",
                 tabPanel(
                   "CNGE",
                   selectInput(
                     "id_CNGE_2023",
                     label = "Módulo",
                     choices = str_c(
                       "Módulo ",
                       seq(
                         module_count[module_count$Censo == "CNGE", ][[2]]
                       )
                     )
                   )
                 ),
                 tabPanel(
                   "CNSPE",
                   selectInput(
                     "id_CNSPE_2023",
                     label = "Módulo",
                     choices = str_c(
                       "Módulo ",
                       seq(
                         module_count[module_count$Censo == "CNSPE", ][[2]]
                       )
                     )
                   )
                 ),
                 tabPanel(
                   "CNSIPEE",
                   selectInput(
                     "id_CNSIPEE_2023",
                     label = "Módulo",
                     choices = str_c(
                       "Módulo ",
                       seq(
                         module_count[module_count$Censo == "CNSIPEE", ][[2]]
                       )
                     )
                   )
                 ),
                 tabPanel(
                   "CNPJE",
                   selectInput(
                     "id_CNPJE_2023",
                     "Módulo",
                     choices = str_c(
                       "Módulo ",
                       seq(
                         module_count[module_count$Censo == "CNPJE", ][[2]]
                       )
                     )
                   )
                 ),
                 tabPanel(
                   "CNIJE",
                   selectInput(
                     "id_CNIJE_2023",
                     label = "Módulo",
                     choices = str_c(
                       "Módulo ",
                       seq(
                         module_count[module_count$Censo == "CNIJE", ][[2]]
                       )
                     )
                   )
                 ),
                 tabPanel(
                   "CNPLE",
                   selectInput(
                     "id_CNPLE_2023",
                     label = "Módulo",
                     choices = str_c(
                       "Módulo ",
                       seq(
                         module_count[module_count$Censo == "CNPLE", ][[2]]
                       )
                     )
                   )
                 ),
                 tabPanel(
                   "CNDHE",
                   selectInput(
                     "id_CNDHE_2023",
                     label = "Módulo",
                     choices = str_c(
                       "Módulo ",
                       seq(
                         module_count[module_count$Censo == "CNDHE", ][[2]]
                       )
                     )
                   )
                 ),
                 tabPanel(
                   "CNTAIPPDPE",
                   selectInput(
                     "id_CNTAIPPDPE_2023",
                     label = "Módulo",
                     choices = str_c(
                       "Módulo ",
                       seq(
                         module_count[module_count$Censo == "CNTAIPPDPE", ][[2]]
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
        fluidRow(
          column(width = 5,
                 plotlyOutput("plot_top_ten_questions_2023")
          ),
          column(width = 7,
                 dataTableOutput("table_top_ten_questions_2023")
          )
        ),
        br(),
        br(),
        br()
      )
    )
  )
),

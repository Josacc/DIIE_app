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

proceso_firma_sello1_Server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # hasta aquí


  })

}

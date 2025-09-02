# 'CNGAE_operative_2023' module ---------------------------------------------

CNGAE_operative_2023_UI <- function(id) {

  ns <- NS(id)

  tabItem(
    'CNGAE_operative_2023',

    fluidRow(

      box(
        solidHeader = TRUE,
        width       = 12,

        sidebarLayout(

          sidebarPanel(
            width = 2,

            radioButtons('id_select_panel_entity', label   = 'Nivel de análisis', choices = c('Nacional', 'Regional', 'Estatal')),

            fluidRow(

              column(
                width = 12,
                selectInput('id_census_evaluation_DOE', 'Censo', choices = levels(id_folio_extended[['Censo']])) # revisar!!!)
              ),

              column(
                width = 12,
                tabsetPanel(
                  id   = 'id_tab_evaluacion',
                  type = 'hidden',

                  tabPanel('Nacional'),

                  tabPanel(
                    'Regional',
                    selectInput('id_regional_evaluation_DOE', 'Regional', choices = levels(pull(federal_entities)))
                  ),

                  tabPanel(
                    'Estatal',
                    selectInput('id_entity_evaluation_DOE', 'Entidades', choices = levels(federal_entities[['Entidad']]))
                  )
                )
              )
            )
          ),

          mainPanel(
            width = 10,

            tabsetPanel(

              # Graphics "Concertación de citas y entrega de cuestionarios".
              tabPanel(
                "Concertación de citas y entrega de cuestionarios",
                br(),

                actionBttn("info_button_citas_cuestionarios", label = "", icon = icon("info-circle"), style = "jelly"),
                br(), br(),

                fluidRow(

                  column(width = 5, plotOutput("plot_concertacion_entrega", height = "600px")),

                  column(width = 7, dataTableOutput("table_concertacion_entrega"))
                )
              ),


              # Graphics "Evolución del levantamiento".
              tabPanel(
                "Evolución del levantamiento",
                br(),
                actionBttn(
                  inputId = "info_button_evolucion_levantamiento",
                  label   = "",
                  icon    = icon("info-circle"),
                  style   = "jelly"
                ),
                br(),
                br(),
                plotlyOutput(
                  "plot_evolucion_levantamiento",
                  height = "300px"
                ),
                dataTableOutput("table_evolucion_levantamiento")
              ),


              # Graphics "Recuperación oportuna de la información".
              tabPanel(
                "Recuperación oportuna de la información",
                br(),
                actionBttn(
                  inputId = "info_button_recuperacion_informacion",
                  label   = "",
                  icon    = icon("info-circle"),
                  style   = "jelly"
                ),
                br(),
                br(),
                plotlyOutput(
                  "plot_recuperacion_informacion_oportuna",
                  height = "300px"
                ),
                dataTableOutput("table_recuperacion_informacion")
              ),


              # Graphics "Intervalos promedio de recuperación".
              tabPanel(
                "Intervalos promedio de recuperación",
                br(),
                actionBttn(
                  inputId = "info_button_intervalos_recuperacion",
                  label   = "",
                  icon    = icon("info-circle"),
                  style   = "jelly"
                ),
                br(),
                br(),
                plotlyOutput(
                  "plot_promedio_recuperacion",
                  height = "700px"
                )
              ),


              # Graphics "Duración del levantamiento por etapas".
              tabPanel(
                "Duración del levantamiento por etapas",
                br(),
                actionBttn(
                  inputId = "info_button_levantamiento_estapas",
                  label   = "",
                  icon    = icon("info-circle"),
                  style   = "jelly"
                ),
                br(),
                br(),
                plotlyOutput(
                  "plot_levantamiento_estapas"
                )
              ),


              # Graphics "Revisiones realizadas a los cuestionarios".
              tabPanel(
                "Revisiones realizadas a los cuestionarios",
                br(),
                actionBttn(
                  inputId = "info_button_revisiones_cuestionarios",
                  label   = "",
                  icon    = icon("info-circle"),
                  style   = "jelly"
                ),
                br(),
                br(),
                plotlyOutput(
                  "plot_revisiones_realizadas"
                )
              )
            )
          )
        )
      )
    )
  )
}

CNGAE_operative_2023_Server <- function(id) {
  moduleServer(id, function(input, output, session) {


  })
}

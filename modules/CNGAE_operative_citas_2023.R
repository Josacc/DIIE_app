# 'CNGAE_operative_citas_2023' module ---------------------------------------------

source("data/data_2023_year.R")

CNGAE_operative_citas_2023_UI <- function(id) {

  ns <- NS(id)

  tabItem(
    'CNGAE_operative_citas_2023',

    fluidRow(

      box(
        solidHeader = TRUE,
        width       = 12,

        sidebarLayout(

          sidebarPanel(
            width = 2,

            radioButtons(ns('id_select_panel_entity'), label   = 'Nivel de análisis', choices = c('Nacional', 'Regional', 'Estatal')),

            fluidRow(

              column(
                width = 12,
                selectInput(ns('id_census_evaluation_DOE'), 'Censo', choices = levels(id_folio_extended[['Censo']]))
              ),

              column(
                width = 12,
                tabsetPanel(
                  id   = ns('id_tab_evaluacion'),
                  type = 'hidden',

                  tabPanel('Nacional'),

                  tabPanel(
                    'Regional',
                    selectInput(ns('id_regional_evaluation_DOE'), 'Regional', choices = levels(pull(federal_entities)))
                  ),

                  tabPanel(
                    'Estatal',
                    selectInput(ns('id_entity_evaluation_DOE'), 'Entidades', choices = levels(federal_entities[['Entidad']]))
                  )
                )
              )
            )
          ),

          mainPanel(
            width = 10,

            tabsetPanel(
              selected = 'Concertación de citas y entrega de cuestionarios',

              tabPanel(
                tags$b('Seguimiento y Evaluación'),
                br()
              ),


              # Graphics 'Concertación de citas y entrega de cuestionarios'.
              tabPanel(
                'Concertación de citas y entrega de cuestionarios',
                icon = icon("chart-pie"),
                br(),

                actionBttn(ns('info_button_citas_cuestionarios'), label = '', icon = icon('info-circle'), style = 'jelly'),
                br(), br(),

                fluidRow(

                  column(width = 5, plotOutput(ns('plot_concertacion_entrega'), height = '600px')),

                  column(width = 7, dataTableOutput(ns('table_concertacion_entrega')))
                )
              ),


              # Graphics 'Evolución del levantamiento'.
              tabPanel(
                'Evolución del levantamiento',
                icon = icon("chart-line"),
                br(),

                actionBttn(ns('info_button_evolucion_levantamiento'), label = '', icon = icon('info-circle'), style = 'jelly'),
                br(), br(),

                plotlyOutput(ns('plot_evolucion_levantamiento'), height = '300px'),
                dataTableOutput(ns('table_evolucion_levantamiento'))
              )
            )
          )
        )
      )
    )
  )
}

CNGAE_operative_citas_2023_Server <- function(id) {
  moduleServer(id, function(input, output, session) {


  })
}

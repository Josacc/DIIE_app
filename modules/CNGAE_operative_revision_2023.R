# 'CNGAE_operative_revision_2023' module ---------------------------------------------

source("data/data_2023_year.R")

CNGAE_operative_revision_2023_UI <- function(id) {

  ns <- NS(id)

  tabItem(
    'CNGAE_operative_revision_2023',

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
              selected = 'Duración del levantamiento por etapas',

              tabPanel(
                tags$b('Seguimiento y Evaluación'),
                br()
              ),

              # Graphics 'Duración del levantamiento por etapas'.
              tabPanel(
                'Duración del levantamiento por etapas',
                icon = icon('chart-column'),
                br(),

                actionBttn(ns('info_button_levantamiento_estapas'), label = '', icon = icon('info-circle'), style = 'jelly'),
                br(), br(),

                plotlyOutput(ns('plot_levantamiento_estapas'))
              ),


              # Graphics 'Revisiones realizadas a los cuestionarios'.
              tabPanel(
                'Revisiones realizadas a los cuestionarios',
                icon = icon('chart-column'),
                br(),

                actionBttn(ns('info_button_revisiones_cuestionarios'), label = '', icon = icon('info-circle'), style = 'jelly'),
                br(), br(),

                plotlyOutput(ns('plot_revisiones_realizadas'))
              )
            )
          )
        )
      )
    )
  )
}

CNGAE_operative_revision_2023_Server <- function(id) {
  moduleServer(id, function(input, output, session) {


  })
}

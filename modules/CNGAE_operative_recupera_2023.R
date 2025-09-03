# 'CNGAE_operative_recupera_2023' module ---------------------------------------------

source("data/data_2023_year.R")

CNGAE_operative_recupera_2023_UI <- function(id) {

  ns <- NS(id)

  tabItem(
    'CNGAE_operative_recupera_2023',

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
              selected = 'Recuperación oportuna de la información',

              tabPanel(
                tags$b('Seguimiento y Evaluación'),
                br()
              ),

              # Graphics 'Recuperación oportuna de la información'.
              tabPanel(
                'Recuperación oportuna de la información',
                icon = icon('chart-bar'),
                br(),

                actionBttn(ns('info_button_recuperacion_informacion'), label = '', icon = icon('info-circle'), style = 'jelly'),
                br(),br(),

                plotlyOutput(ns('plot_recuperacion_informacion_oportuna'), height = '300px'),
                dataTableOutput(ns('table_recuperacion_informacion'))
              ),


              # Graphics 'Intervalos promedio de recuperación'.
              tabPanel(
                'Intervalos promedio de recuperación',
                icon = icon('chart-column'),
                br(),

                actionBttn(ns('info_button_intervalos_recuperacion'), label = '', icon = icon('info-circle'), style = 'jelly'),
                br(), br(),

                plotlyOutput(ns('plot_promedio_recuperacion'), height = '700px')
              )
            )
          )
        )
      )
    )
  )
}

CNGAE_operative_recupera_2023_Server <- function(id) {
  moduleServer(id, function(input, output, session) {


  })
}

# 'CNGAE_operative_revision_2023' module ---------------------------------------------

source("data/data_2023_year.R")
source('modules/evaluation/operative_global.R')

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
                br(),

                tags$div(style = "text-align:   justify;
                              font-size:    20px;
                              color:        #1e4a75;
                              display:      block;
                              margin-left:  auto;
                              margin-right: auto;
                              width:        70%;
                              padding:      4rem;
                              border:       2px solid #ccc" ,
                         h2("INFORMACIÓN GENERAL", style = "text-align: center;"),
                         br(),
                         "Como parte de las actividades de análisis y seguimiento
                     del Departamento de Integración de Información Estadística (DIIE)
                     se presentan indicadores que ponderan varios
                     resultados obtenidos en el CNG de ambito estatal.",
                         br(),
                         strong("Bajo un enfoque informativo,"),
                         "se tiene el objetivo de detectar",
                         strong("áreas de oportunidad "), "de los equipos de
                     trabajo de Oficinas Centrales y los Departamentos Estatales,
                     así como obtener información que ayude a diseñar estrategias
                     focalizadas al próximo levantamiento.",
                         br(), br(),
                         "Los análisis que se presentan en cada uno de los apartados
                     fueron generados a partir dos consideraciones relevantes:",
                         br(),
                         HTML(str_c("<ol>",
                                    "<li> Se descartan los folios con estatus final ‘No aplica’, a excepción de los análisis de la pestaña ‘Observaciones’.  </li>",
                                    "<li> Las fechas se ajustan, de ser necesario, a días hábiles. Cualquier actividad efectuada en día inhábil se recorre al próximo día hábil. </li>",
                                    "</ol>"
                         ))
                )


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

CNGAE_operative_revision_2023_Server <- function(id, database_DOE) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$id_select_panel_entity, {
      updateTabsetPanel(session, inputId = "id_tab_evaluacion", selected = input$id_select_panel_entity)
    })

    # Data base "evolución del levantamiento".
    database_evolucion_levantamiento <- reactive(DT_evolucion_levantamiento(database_DOE()))


    # Plot evolución del levantamiento.
    output$plot_evolucion_levantamiento <- renderPlotly({

      switch (input$id_select_panel_entity,
              "Nacional" = plot_DT_evolucion_levantamiento(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE, levels(federal_entities[["Entidad"]])
              )[["p"]],

              "Regional" = plot_DT_evolucion_levantamiento(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
              )[["p"]],

              "Estatal" = plot_DT_evolucion_levantamiento(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                input$id_entity_evaluation_DOE
              )[["p"]]
      )

    })

    # Info evolución del levantamiento.
    observeEvent(input$info_button_evolucion_levantamiento, {
      show_alert(
        session = session,
        title   = "",
        text    = tags$div(
          tags$h3("Información",
                  style = "color: #0076C8; font-weight: bold; text-align: center"),
          tags$br(),
          tags$br(),
          `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
          "Se muestra el avance porcentual de cumplimiento de
        las actividades 3.2.3.1, 3.2.3.2, 3.2.3.3 y 3.2.4; los meses
        parametrizan el porcentaje de cuestionarios que han
        cumplido con cada Actividad (fase).",
          tags$br(),
          tags$br(),
          "La categoría “PENDIENTES” refiere al porcentaje de
        cuestionarios que no han cumplido con la respectiva Actividad (fase).",
          tags$br(),
          tags$br(),
          dataTableOutput("info_DT_relacion_actividad_estatus")
        ),
        html  = TRUE,
        width = "62%"
      )
    })

    output$info_DT_relacion_actividad_estatus <- renderDataTable(
      datatable(
        relacion_actividad_fase %>%
          mutate(
            `Actividades (fases)` = str_c(c("3.2.3.1 ", "3.2.3.2 ", "3.2.3.3 ", "3.2.4   "), `Actividades (fases)`)
          ),
        options = list(
          dom = 't'
        ),
        rownames = FALSE
      )
    )

    # Data table evolución del levantamiento.
    output$table_evolucion_levantamiento <- renderDataTable({

      switch (input$id_select_panel_entity,
              "Nacional" = plot_DT_evolucion_levantamiento(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                levels(federal_entities[["Entidad"]])
              )[["dframe"]] %>%
                datatable(
                  options = list(
                    dom = 't',
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                  ),
                  rownames = FALSE
                ),

              "Regional" = plot_DT_evolucion_levantamiento(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
              )[["dframe"]] %>%
                datatable(
                  options = list(
                    dom = 't',
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                  ),
                  rownames = FALSE
                ),

              "Estatal" = plot_DT_evolucion_levantamiento(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                input$id_entity_evaluation_DOE
              )[["dframe"]] %>%
                datatable(
                  options = list(
                    dom = 't',
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                  ),
                  rownames = FALSE
                )
      )

    })

    # Plot "Recuperación oportuna de la información".
    output$plot_recuperacion_informacion_oportuna <- renderPlotly({

      switch (input$id_select_panel_entity,
              "Nacional" = plot_DT_recuperacion_oportuna_informacion(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                levels(federal_entities[["Entidad"]])
              )$plot,

              "Regional" = plot_DT_recuperacion_oportuna_informacion(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
              )$plot,

              "Estatal" = plot_DT_recuperacion_oportuna_informacion(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                input$id_entity_evaluation_DOE
              )$plot
      )

    })

    # Info Recuperación oportuna de la información.
    observeEvent(input$info_button_recuperacion_informacion, {
      show_alert(
        session = session,
        title   = "",
        text    = tags$div(
          tags$h3("Información",
                  style = "color: #0076C8; font-weight: bold; text-align: center"),
          tags$br(),
          tags$br(),
          `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
          "La gráfica escinde el cumplimiento de cada actividad (fase) en tres categorías:
        “En tiempo”, “Fuera de meta” y “PENDIENTES”.  El parámetro de referencia
        para cada censo es la fecha límite de la actividad marcada en la planeación.",
          tags$br(),
          tags$br(),
          "La categoría “PENDIENTES” refiere al porcentaje de
        cuestionarios que no han cumplido con la respectiva Actividad (fase)."
        ),
        html  = TRUE,
        width = "50%"
      )
    })

    # DT "Recuperación oportuna de la información".
    output$table_recuperacion_informacion <- renderDataTable({

      switch (input$id_select_panel_entity,
              "Nacional" = plot_DT_recuperacion_oportuna_informacion(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                levels(federal_entities[["Entidad"]])
              )$df %>%
                datatable(
                  rownames = FALSE,
                  filter   = "top",
                  options  = list(
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                  )
                ),

              "Regional" = plot_DT_recuperacion_oportuna_informacion(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
              )$df %>%
                datatable(
                  rownames = FALSE,
                  filter   = "top",
                  options  = list(
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                  )
                ),

              "Estatal" = plot_DT_recuperacion_oportuna_informacion(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                input$id_entity_evaluation_DOE
              )$df %>%
                datatable(
                  rownames = FALSE,
                  filter   = "top",
                  options  = list(
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                  )
                )
      )

    })

    # Plot "Concertación de citas y entrega de cuestionarios".
    output$plot_concertacion_entrega <- renderPlot({

      file_citas_agendadas <- "citas_agendadas/xIktan_20231013114346215_citasAgendadas.xlsx"

      switch (input$id_select_panel_entity,
              "Nacional" = plot_citas_cuestionarios(
                file_citas_agendadas,
                input$id_census_evaluation_DOE,
                levels(federal_entities[["Entidad"]])
              )$plot,

              "Regional" = plot_citas_cuestionarios(
                file_citas_agendadas,
                input$id_census_evaluation_DOE,
                federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
              )$plot,

              "Estatal" = plot_citas_cuestionarios(
                file_citas_agendadas,
                input$id_census_evaluation_DOE,
                input$id_entity_evaluation_DOE
              )$plot
      )

    })

    # DT "Concertación de citas y entrega de cuestionarios".
    output$table_concertacion_entrega <- renderDataTable({

      file_citas_agendadas <- "citas_agendadas/xIktan_20231013114346215_citasAgendadas.xlsx"

      switch (input$id_select_panel_entity,
              "Nacional" = plot_citas_cuestionarios(
                file_citas_agendadas,
                input$id_census_evaluation_DOE,
                levels(federal_entities[["Entidad"]])
              )$df %>%
                datatable(
                  rownames = FALSE,
                  filter   = "top",
                  options  = list(
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                  )
                ),

              "Regional" = plot_citas_cuestionarios(
                file_citas_agendadas,
                input$id_census_evaluation_DOE,
                federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
              )$df %>%
                datatable(
                  rownames = FALSE,
                  filter   = "top",
                  options  = list(
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                  )
                ),

              "Estatal" = plot_citas_cuestionarios(
                file_citas_agendadas,
                input$id_census_evaluation_DOE,
                input$id_entity_evaluation_DOE
              )$df %>%
                datatable(
                  rownames = FALSE,
                  filter   = "top",
                  options  = list(
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                  )
                )
      )

    })

    # Info Concertación de citas y entrega de cuestionarios.
    observeEvent(input$info_button_citas_cuestionarios, {
      show_alert(
        session = session,
        title   = "",
        text    = tags$div(
          tags$h3("Información",
                  style = "color: #0076C8; font-weight: bold; text-align: center"),
          tags$br(),
          tags$br(),
          `style` = "text-align: center;
        margin-left:  auto;
        margin-right: auto;",
          'Análisis generado del reporte "Citas agendadas".',
          tags$br(),
          tags$br(),
          "Corte de información: 13/10/2023 11:43 hrs."
        ),
        html  = TRUE,
        width = "35%"
      )
    })

    # Plot "Intervalos promedio de recuperación".
    output$plot_promedio_recuperacion <- renderPlotly({

      switch (input$id_select_panel_entity,
              "Nacional" = plot_intervalo_recuperacion(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                levels(federal_entities[["Entidad"]])
              ),

              "Regional" = plot_intervalo_recuperacion(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
              ),

              "Estatal" = plot_intervalo_recuperacion(
                database_evolucion_levantamiento(),
                input$id_census_evaluation_DOE,
                input$id_entity_evaluation_DOE
              )
      )

    })

    # Info Intervalos promedio de recuperación.
    observeEvent(input$info_button_intervalos_recuperacion, {
      show_alert(
        session = session,
        title   = "",
        text    = tags$div(
          tags$h3("Información",
                  style = "color: #0076C8; font-weight: bold; text-align: center"),
          tags$br(),
          tags$br(),
          `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
          "Las gráficas muestran por cuestionario el número de días que tomó recuperar la
        información desde el inicio proyectado en la
        planeación de cada censo, hasta el cumplimiento de cada actividad (fase).",
          tags$br(),
          "A nivel regional se promedian los resultados obtenidos en cada módulo por las
        entidades involucradas.",
          tags$br(),
          tags$br(),
          "Abreviaturas:",
          tags$br(),
          "Mín: mínimo de días a nivel nacional que tomó recuperar la información.",
          tags$br(),
          "Máx: máximo de días a nivel nacional que tomó recuperar la información.",
          tags$br(),
          "µ: promedio de días a nivel nacional que tomó recuperar la información."
        ),
        html  = TRUE,
        width = "45%"
      )
    })

    # Plot "Revisiones realizadas a los cuestionarios".
    reactive_revisiones_realizadas <- reactive({
      req(database_DOE())
      switch (input$id_select_panel_entity,
              "Nacional" = plot_revisiones_cuestionarios(
                database_DOE(),
                input$id_census_evaluation_DOE,
                levels(federal_entities[["Entidad"]])
              ),

              "Regional" = plot_revisiones_cuestionarios(
                database_DOE(),
                input$id_census_evaluation_DOE,
                federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
              ),

              "Estatal" = plot_revisiones_cuestionarios(
                database_DOE(),
                input$id_census_evaluation_DOE,
                input$id_entity_evaluation_DOE
              )
      )
    })

    output$plot_revisiones_realizadas <- renderPlotly({
      validate(need(reactive_revisiones_realizadas(), "Sin información"))
      reactive_revisiones_realizadas()
    })

    # Info Revisiones realizadas a los cuestionarios.
    observeEvent(input$info_button_revisiones_cuestionarios, {
      show_alert(
        session = session,
        title   = "",
        text    = tags$div(
          tags$h3("Información",
                  style = "color: #0076C8; font-weight: bold; text-align: center"),
          tags$br(),
          tags$br(),
          `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
          "La gráfica muestra por cuestionario el número de revisiones que se han efectuado.",
          tags$br(),
          "A nivel regional se promedian los resultados obtenidos en cada módulo por las
        entidades involucradas.",
          tags$br(),
          tags$br(),
          "Abreviaturas:",
          tags$br(),
          "Mín: mínimo de revisiones a nivel nacional.",
          tags$br(),
          "Máx: máximo de revisiones a nivel nacional.",
          tags$br(),
          "µ: promedio de revisiones a nivel nacional."
        ),
        html  = TRUE,
        width = "31%"
      )
    })

    # Plot "Duración del levantamiento por etapas".
    output$plot_levantamiento_estapas <- renderPlotly({

      switch (input$id_select_panel_entity,
              "Nacional" = plot_duracion_levantamiento_etapas(
                database_DOE(),
                input$id_census_evaluation_DOE,
                levels(federal_entities[["Entidad"]])
              ),

              "Regional" = plot_duracion_levantamiento_etapas(
                database_DOE(),
                input$id_census_evaluation_DOE,
                federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
              ),

              "Estatal" = plot_duracion_levantamiento_etapas(
                database_DOE(),
                input$id_census_evaluation_DOE,
                input$id_entity_evaluation_DOE
              )
      )

    })

    # Info Duración del levantamiento por etapas.
    observeEvent(input$info_button_levantamiento_estapas, {
      show_alert(
        session = session,
        title   = "",
        text    = tags$div(
          tags$h3("Información",
                  style = "color: #0076C8; font-weight: bold; text-align: center"),
          tags$br(),
          tags$br(),
          `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
          "La gráfica muestra el promedio de días hábiles que duró cada etapa del levantamiento,
        las cuales están conformadas por los estatus del historial de seguimiento.",
          tags$br(),
          "Las barras con valores negativos denotan que el comienzo y cumplimiento
        de la actividad se realizó antes de la fecha de inicio considerada en la planeación.",
          tags$br(),
          tags$br(),
          dataTableOutput("info_DT_levantamiento_estapas")
        ),
        html  = TRUE,
        width = "70%"
      )
    })

    output$info_DT_levantamiento_estapas <- renderDataTable(
      datatable(
        tibble(
          Etapa = c(
            "Proceso de llenado",
            "Revisión ROCE",
            "Validación OC",
            "Aclaración de información OC",
            "Revisión ROCE derivado de OC",
            "Recuperado con firma y sello"
          ),
          Estatus = c(
            "En proceso de llenado, Aclaración de información parcial y Recuperacion parcial",
            "Revisión ROCE y Aclaración de información (Revisión ROCE)",
            "Revisión OC",
            "Aclaración de información OC y Aclaración ROCE derivado de OC",
            "Revisión ROCE derivado de OC",
            "En proceso de firma y sello, Recuperado con firma y sello,
          En proceso de firma y sello por reconsulta, En aclaración por reconsulta y
          Recuperado con firma y sello por reconsulta"
          )
        ),
        options = list(
          dom = 't',
          columnDefs = list(list(width = '250px', targets = c(0)))
        ),
        rownames = FALSE
      )
    )

  })
}

# 'interno' module --------------------------------------------------------

interno_UI_2024 <- function(id) {

  ns <- NS(id)

  tagList(

    tabsetPanel(
      type = "pills",

      tabPanel(
        "Revisiones por OC",
        br(), br(),

        h4(
          p(strong("Tabulado de revisiones efectuadas por OC")),
          style = "color: #3c8dbc; margin: 0rem; margin-top: -1rem; margin-bottom: 3rem;"
        ),

        p(strong("NA: "), "cuestionario no aplica"),
        p(strong("NR: "), "cuestionario no revisado"),
        br(),

        DTOutput(ns('table_q_aclaracion_oc')),
        br(),

        strong(textOutput(ns('id_celdas_seleccionadas'))),

        fluidRow(

          column(
            width = 12,
            actionButton(
              ns("id_bttn_clear_selection"),
              label = "Limpiar selección",
              icon  = icon("broom"),
              class = "btn_custom_interno_1"
            ),
            actionButton(
              ns("id_bttn_filter_obs"),
              label = "Filtrar",
              icon  = icon("filter"),
              class = "btn_custom_interno_2"
            )
          )
        ),
        br(), br(), br(),

        DTOutput(ns("data_q_aclaracion_oc")),
        br(), br(), br(), br()
      ),

      tabPanel(
        "Ranking de entidades por preguntas observadas",
        br(), br(),

        sidebarLayout(

          sidebarPanel(
            width = 2,
            radioButtons(
              ns("id_obs_vs_census_2023"),
              "Nivel de análisis",
              choices = c("GLOBAL", levels(DIIE_dates_2024[[1]]))
            )
          ),

          mainPanel(
            style = "height: 500px",
            width = 10,
            actionBttn(
              inputId = ns("info_button_obs_enviadas_OC"),
              label   = "",
              icon    = icon("info-circle"),
              style   = "jelly"
            ),
            br(), br(),
            plotlyOutput(ns("plot_obs_vs_census_2023"))
          )
        )
      )
    )
  )
}

interno_Server_2024 <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    db_q_aclaracion_oc_2024 <- function(database, delete_q = NULL) {

      .data <- database %>%
        filter(!(Folio %in% pull(DT_folio_no_aplica(database)))) %>%
        filter(Usuario %in% pull(reviewer_team, 1)) %>%
        filter(Perfil != "ADMINISTRADOR") %>%
        filter(
          str_detect(
            Estatus,
            "(Aclaración de información OC \\(\\d+\\))|(En proceso de firma y sello)"
          )
        ) %>%
        filter(
          !str_detect(str_extract(Observación, "[^ \\n]+"), '(C|c)(\\.{0,2}|\\s{0,2})$') | is.na(Observación)
        )

      database <- .data %>%
        count(Folio, name = "Revisiones") %>%
        mutate(Revisiones = as.character(Revisiones)) %>%
        bind_rows(DT_folio_no_aplica(database) %>% mutate(Revisiones = "NA")) %>%
        arrange(Folio)

      .table <- id_folio_extended_2024 %>%
        select(Folio, id_estado) %>%
        rename(Estado = "id_estado") %>%
        mutate(Cuestionario = str_sub(Folio, 3)) %>%
        left_join(database, by = "Folio") %>%
        mutate(Revisiones = replace_na(Revisiones, "NR")) %>%
        select(-Folio) %>%
        pivot_wider(names_from = Cuestionario, values_from = Revisiones) %>%
        select(-all_of(delete_q))


      .datatable <- .table %>%
        left_join(federal_entities, by = c("Estado" = "id_estado"), keep = FALSE) %>%
        relocate(Abreviatura) %>%
        select(-Estado, -Entidad, -Regional) %>%
        rename(Entidad = Abreviatura) %>%
        datatable(
          rownames   = FALSE,
          selection  = list(target = "cell"),
          extensions = c(
            "FixedColumns", "SearchBuilder", "Buttons", "FixedHeader"
          ),
          options    = list(
            ordering      = FALSE,
            pageLength    = 8,
            fixedHeader   = TRUE,
            dom           = "QBlftip",
            lengthMenu    = list(c(8, 16, 32), c("8", "16", "32")),
            search        = list(regex = TRUE),
            buttons       = list(
              list(
                extend           = "colvis",
                text             = "Visibilidad de columnas",
                columns          = 1:40,
                collectionLayout = "fixed columns",
                popoverTitle     = "Control de visibilidad de columnas"
              )
            ),
            searchbuilder = TRUE,
            scrollX       = TRUE,
            fixedColumns  = list(leftColumns = 1),
            language      = list(
              url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json",
              searchBuilder =list(
                add =  "Agregar condición"
              )
            ),
            columnDefs    = list(
              list(className = 'dt-center', targets = c(1:40))
            ),
            initComplete  = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'font-size': '90%'});",
              "}")
          )
        ) %>%
        formatStyle(
          columns         = c(1),
          color           = "navy",
          fontSize        = '90%'
        ) %>%
        formatStyle(
          columns         = c(2:41),
          fontWeight      = styleInterval(c(2), c("", "bold")),
          fontSize        = '90%'
        ) %>%
        formatStyle(
          columns         = c(2:41),
          backgroundColor = styleInterval(c(2, 3, 4), c("", "bisque", "yellow", "red"))
        ) %>%
        formatStyle(
          columns         = c(20, 22, 24, 29, 36, 38, 40, 43),
          `border-right`  = "3px solid #ddd"
        )

      return(list(datatable = .datatable, data = .data, table = .table))

    }


    # Ranking entidades
    reactive_obs_vs_census_2023 <- reactive({
      switch (input$id_obs_vs_census_2023,
              GLOBAL     = plot_entities_vs_obs(data()[[2]]),
              CNGE       = plot_entities_vs_obs_grid(data()[[2]], "CNGE"),
              CNSPE      = plot_entities_vs_obs_grid(data()[[2]], "CNSPE"),
              CNSIPEE    = plot_entities_vs_obs_grid(data()[[2]], "CNSIPEE"),
              CNPJE      = plot_entities_vs_obs_grid(data()[[2]], "CNPJE"),
              CNIJE      = plot_entities_vs_obs_grid(data()[[2]], "CNIJE"),
              CNPLE      = plot_entities_vs_obs_grid(data()[[2]], "CNPLE"),
              CNDHE      = plot_entities_vs_obs_grid(data()[[2]], "CNDHE"),
              CNTAIPPDPE = plot_entities_vs_obs_grid(data()[[2]], "CNTAIPPDPE")
      )
    })

    # Ranking entidades
    output$plot_obs_vs_census_2023 <- renderPlotly({

      validate(need(reactive_obs_vs_census_2023(), "Sin observaciones"))
      reactive_obs_vs_census_2023()

    })

    # Info Ranking entidades.
    observeEvent(input$info_button_obs_enviadas_OC, {

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
          "La gráfica muestra por Censo el ranking de Entidades
        respecto a la cantidad de preguntas que fueron observadas exclusivamente
        por los responsables de revisión de OC."
        ),
        html  = TRUE,
        width = "35%"
      )
    })


    # Table questionnaires aclaracion oc
    output$table_q_aclaracion_oc <- renderDT({

      db_q_aclaracion_oc_2024(data()[[1]], delete_q = c("8101", "8201", "8301"))$datatable

    }, server = FALSE)


    bttn_filter_obs <- eventReactive(input$id_bttn_filter_obs, {
      input$table_q_aclaracion_oc_cells_selected
    })


    observeEvent(input$id_bttn_clear_selection, {

      proxy <- dataTableProxy("table_q_aclaracion_oc")
      selectCells(proxy, NULL)

    })

    output$id_celdas_seleccionadas <- renderText({

      paste0(
        "Celdas seleccionadas: ",
        nrow(input$table_q_aclaracion_oc_cells_selected)
      )

    })

    output$data_q_aclaracion_oc = renderDT({
      # req(credentials()$user_auth)

      db_q_aclaracion_oc_filter(
        db_q_aclaracion_oc_2024(data()[[1]], c("8101", "8201", "8301"))$data,
        db_q_aclaracion_oc_2024(data()[[1]], c("8101", "8201", "8301"))$table,
        bttn_filter_obs()
      ) %>%
        datatable_q_aclaracion_oc_filter()
    })
  })
}

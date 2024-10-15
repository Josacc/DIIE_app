a <- data_and_update("historial_seguimiento/xIktan_20240911092237512_reporteSegumiento.xlsx")[[1]]

db_q_aclaracion_oc <- function(database, delete_q) {

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
      !str_detect(str_extract(Observación, "[^ \\n]+"), "C\\.") | is.na(Observación)
    )

  database <- .data %>%
    count(Folio, name = "Revisiones") %>%
    mutate(Revisiones = as.character(Revisiones)) %>%
    bind_rows(DT_folio_no_aplica(database) %>% mutate(Revisiones = "NA")) %>%
    arrange(Folio)

  .table <- id_folio_extended %>%
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
        ordering      = F,
        pageLength    = 8,
        fixedHeader   = TRUE,
        dom           = "QBlftip",
        lengthMenu    = list(c(8, 16, 32), c("8", "16", "32")),
        search        = list(regex = TRUE, search = "|"),
        buttons       = list(
          list(
            extend           = "colvis",
            text             = "Visibilidad de columnas",
            columns          = 1:34,
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
          list(className = 'dt-center', targets = c(1:34))
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
      columns         = c(2:38),
      fontWeight      = styleInterval(c(2), c("", "bold")),
      fontSize        = '90%'
    ) %>%
    formatStyle(
      columns         = c(2:38),
      backgroundColor = styleInterval(c(2, 3, 4), c("", "bisque", "yellow", "red"))
    ) %>%
    formatStyle(
      columns         = c(15, 17, 19, 24, 31, 33),
      `border-right`  = "3px solid #ddd"
    )

  return(list(datatable = .datatable, data = .data, table = .table))

}


# db_q_aclaracion_oc(a, c("8101", "8201", "8301"))$datatable

shinyApp(
  ui =  dashboardPage(
    dashboardHeader(
      titleWidth = 60
    ),
    dashboardSidebar(
      width = 60
    ),
    dashboardBody(
      tags$head(tags$style(HTML("
      .content-wrapper {
        background-color: #FFFFFF;
      }
    "
      ))),
      DTOutput('tbl_1'),
      br(),
      br(),
      verbatimTextOutput("text_out"),
      DTOutput('tbl_2')
    )
  ),
  server <-  function(input, output) {

    output$text_out <- renderPrint(input$tbl_1_cells_selected)

    output$tbl_1 = renderDT(
      db_q_aclaracion_oc(a, c("8101", "8201", "8301"))$datatable,
      server = FALSE
    )

    output$tbl_2 = renderDT({

      db_q_aclaracion_oc_filter(
        db_q_aclaracion_oc(a, c("8101", "8201", "8301"))$data,
        db_q_aclaracion_oc(a, c("8101", "8201", "8301"))$table,
        input$tbl_1_cells_selected
      ) %>%
        datatable(
          rownames   = FALSE,
          selection  = list(target = "cell"),
          extensions = c("Buttons", "FixedHeader"),
          options    = list(
            ordering    = FALSE,
            pageLength  = 10,
            fixedHeader = TRUE,
            dom         = "Blftip",
            buttons     = list(
              list(
                extend           = "colvis",
                columns          = c(1:5, 7:9),
                collectionLayout = "fixed columns",
                popoverTitle     = "Control de visibilidad de columnas"
              )
            ),
            language    = list(
              url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
            ),
            columnDefs  = list(
              list(visible = FALSE, targets = c(1:3, 7:9))
            )
          )
        )
    })
  }
)


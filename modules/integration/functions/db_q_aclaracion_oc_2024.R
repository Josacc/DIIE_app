#

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

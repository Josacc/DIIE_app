# Principal database for DOE analysis
write_database_DOE <- function(dataframe) {

  dataframe <- dataframe %>%
    select(-`Cantidad de obs`) %>%
    mutate(ID = seq_along(Folio)) %>%
    separate(Folio, into = c("id_estado", "Censo_n", "Módulo"), sep = c(2, 3), remove = FALSE) %>%
    mutate(id_estado = factor(id_estado, levels = levels(federal_entities[["id_estado"]]))) %>%
    mutate(Censo_n   = factor(Censo_n  , levels = 1:8)) %>%
    mutate(Estatus   = str_replace_all(Estatus, pattern = "\\(Revisión ROCE\\)", replacement = "Revisión ROCE")) %>%
    mutate(Estatus   = str_remove_all(Estatus, pattern = "\\(\\d+\\%\\)")) %>%
    relocate(ID) %>%
    relocate(Censo, .before = Entidad) %>%
    left_join(working_dates, by = "Registro") %>% # Se modificó "Registro" para considerar solo días habiles.
    select(-Registro) %>%
    rename(Registro = aux_var)

  return(dataframe)
}

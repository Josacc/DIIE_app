a <- read_xlsx("reconsultas/reconsultas_2024.xlsx")

a %>%
  select(-`Fecha de envío`, -`Fecha de atención`, -Año) %>%
  filter(Solución == "Omisión") %>%
  mutate(Responsable = str_replace(Responsable, "Toño", "Antonio")) %>%
  mutate(Censo = str_replace_all(
    Censo,
    c(
      "GOE" = "1",
      "SPE" = "2",
      "PEE" = "3",
      "PJE" = "4"
    )
  )) %>%
  mutate(Módulo = str_replace_all(
    Módulo,
    c(
      "M1" = "101",
      "M2" = "201",
      "M3" = "301",
      "M4" = "401",
      "M5" = "501"
    )
  )) %>%
  mutate(Entidad = str_c("0", Entidad) %>% str_sub(-2)) %>%
  mutate(Folio = str_c(Entidad, Censo, Módulo))

.plot <- a %>%
  count(Responsable, name = "Omisiones") %>%
  arrange(Omisiones) %>%
  mutate(Responsable = fct_inorder(Responsable)) %>%
  ggplot(aes(Responsable, Omisiones)) +
  geom_col(width = .7, fill = "#a71106") +
  labs(y = "Cantidad de omisiones") +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 30, hjust = 1, size = 8),
    axis.title.x = element_blank()
  )

ggplotly(.plot)









































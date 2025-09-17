# Team union for specific functions

team_data <- function(team, data) {
  return(team %>% left_join(data, by = "Usuario", multiple = "all"))
}

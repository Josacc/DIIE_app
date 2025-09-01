# shiny dashboardHeader
source('contact/contact.R')

header <- dashboardHeader(

  titleWidth = 320,

  title     = tags$div(
    style = "display: flex; align-items: center; padding-left: 17px;",
    tags$img(src = "logo_dashboardheader.png", height = "40px", style  = "margin-right: 10px;"),
    tags$span('Censo Nacional de Gobierno', style = "font-size: 16px; font-weight: bold;")
  ),

  tags$li(
    class = "dropdown",
    style = "display: flex; align-items: center; height: 50px;",
    actionButton("page_toggle", label = NULL, icon = icon("maximize"))
  ),

  dropdownMenu(
    type        = "messages",
    badgeStatus = NULL,
    headerText  = "Administrador",
    messageItem(
      from    = contact_name,
      message = contact_email,
      icon    = icon("user-gear"),
      href    = str_c("mailto:", contact_email, "?Subject=DIIE%20app")
    )
  )
)

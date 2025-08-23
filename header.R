# shiny dashboardHeader
source('contact/contact.R')

header <- dashboardHeader(

  titleWidth = 200,

  title     = tags$div(
    style = "display: flex; align-items: center; padding-left: 17px;",
    tags$img(src = "logo_dashboardheader.png", height = "40px", style  = "margin-right: 10px;"),
    tags$span("DIIE app", style = "font-size: 16px; font-weight: bold;")
  ),

  tags$li(
    class = "dropdown",
    style = "display: flex; align-items: center; height: 50px;",
    actionBttn("page_full", style = "stretch", icon("maximize"))
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

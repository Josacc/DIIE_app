# Servidor del módulo contenedor del navbarPage
modulo_navbar_contenedor_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    archivo_cargado <- reactiveVal(FALSE)
    datos_archivo <- reactiveVal(NULL)

    # Módulo de carga de archivo
    callModule(modulo_carga_archivo_server, "carga")

    observeEvent(modulo_carga_archivo_output$archivo_info(), {
      if (!is.null(modulo_carga_archivo_output$archivo_info())) {
        archivo_cargado(TRUE)
        datos_archivo(modulo_carga_archivo_output$archivo_info())
      } else {
        archivo_cargado(FALSE)
        datos_archivo(NULL)
      }
    })

    # Instanciar los otros módulos de tabPanel, pasando la reactividad
    callModule(modulo_tab_uno_server, "tabuno", archivo_cargado)
    callModule(modulo_tab_dos_server, "tabdos", archivo_cargado, datos_archivo)

    output$navbar_principal <- renderUI({
      navbarPage(
        "Mi Navbar",
        modulo_carga_archivo_ui(session$ns("carga")),
        conditionalPanel(
          condition = "output.archivo_cargado", # Usamos output para reactividad en UI
          modulo_tab_uno_ui(session$ns("tabuno")),
          modulo_tab_dos_ui(session$ns("tabdos"))
          # ... otros módulos de tabPanel
        )
      )
    })

    outputOptions(output, "archivo_cargado", suspendWhenHidden = FALSE) # Para que conditionalPanel funcione
  })
}

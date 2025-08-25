# 'info analitica' module -------------------------------------------------

info_analitica_UI_2024 <- function(id) {
  tabPanel(
    title = "Info",
    icon  = icon("info-circle"),
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
  )
}

info_analitica_Server_2024 <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

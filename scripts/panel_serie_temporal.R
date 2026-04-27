panel_serie_temporal <- nav_panel(
  title = h5(
    span(
      HTML('<span class="grommet-icons--time"></span>'),
      "Serie temporal"
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      title = h1("Opciones"),
      dateInput(
        "fecha_altura_min",
        label = "Fecha inicial",
        value = fecha_altura_min,
        format = "dd-mm-yyyy",
        min = fecha_altura_min,
        max = fecha_altura_max,
        language = "es"
      ),
      dateInput(
        "fecha_altura_max",
        label = "Fecha final",
        value = fecha_altura_max,
        format = "dd-mm-yyyy",
        min = fecha_altura_min,
        max = fecha_altura_max,
        language = "es"
      ),
      shinyWidgets::prettySwitch(
        "serie_temporal_ma",
        "Mostrar media móvil",
        fill = TRUE,
        inline = TRUE,
        bigger = TRUE,
        value = FALSE,
        status = "info"
      ),
      uiOutput("slider_ma"),
      hr(),
      h1("Datos de altura"),
      icon = HTML('<i class="bi bi-leaf-fill"></i>'),
      span(
        "Fuente de datos de la serie temporal de altura del Río Paraná: ",
        a(
          "Sistema Nacional de Información Hídrica",
          href = "https://snih.hidricosargentina.gob.ar/Filtros.aspx",
          target = "_blank",
          .noWS = "after"
        ),
        "."
      ),
      hr(),
      downloadButton(
        "descarga_serie_temporal",
        strong("Descargar datos de altura"),
        style = glue::glue("background-color: {verde};color: white;")
      )
    ),
    card(
      card_header(
        span(
          "Serie temporal de altura del río Paraná",
          class = "card-titulo"
        )
      ),
      girafeOutput("serie_temporal_altura")
    )
  )
)

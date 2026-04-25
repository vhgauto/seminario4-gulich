panel_serie_temporal <- nav_panel(
  title = h5(
    span(
      HTML('<span class="grommet-icons--time"></span>'),
      "Serie temporal"
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      title = h1("Rango temporal"),
      dateInput(
        "fecha_altura_min",
        label = h5("Fecha inicial"),
        value = fecha_altura_min,
        format = "dd-mm-yyyy",
        min = fecha_altura_min,
        max = fecha_altura_max,
        language = "es"
      ),
      dateInput(
        "fecha_altura_max",
        label = h5("Fecha final"),
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
        status = "info"
      ),
      uiOutput("slider_ma"),
      hr(),
      h1("Datos de altura"),
      icon = HTML('<i class="bi bi-leaf-fill"></i>'),
      span(
        "Los datos para la confección de la serie temporal de altura del Río Paraná proviene del ",
        a(
          "Sistema Nacional de Información Hídrica",
          href = "https://snih.hidricosargentina.gob.ar/Filtros.aspx",
          target = "_blank",
          .noWS = "before-end"
        ),
        "."
      ),
      hr(),
      downloadButton("descarga_serie_temporal", "Descargar datos de altura")
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

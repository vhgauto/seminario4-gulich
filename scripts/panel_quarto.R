panel_quarto <- nav_panel(
  title = h5("Reporte"),

  layout_sidebar(
    sidebar = sidebar(
      title = h1("PDF"),
      selectInput(
        "quarto_fecha_firma",
        label = "Opciones",
        choices = quarto_fecha_firma
      ),
      selectInput(
        "quarto_fecha_mapa",
        label = "Opciones",
        choices = quarto_fecha_mapa
      )
    ),
    card(
      downloadButton("render_quarto", "Generar reporte")
    )
  )
)

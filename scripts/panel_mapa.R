panel_mapa <- nav_panel(
  title = h5("Distribución geográfica"),
  layout_sidebar(
    sidebar = sidebar(
      title = h1("Opciones"),
      selectInput(
        "fecha",
        "Fecha",
        fechas
      ),
      selectInput(
        "tipo",
        "Tipo de mapa",
        c("RGB", "Turbidez", "Profundidad de disco")
      )
    ),
    card(
      card_header(h1("Mapa")),
      leafletOutput("mymap")
    )
  )
)

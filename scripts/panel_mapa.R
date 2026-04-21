panel_mapa <- nav_panel(
  title = h5("Distribución espacial"),
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
      ),
      hr(),
      h1("Algoritmos"),
      icon = HTML('<i class="bi bi-leaf-fill"></i>'),
      span(
        HTML(
          "El modelo para estimar turbidez se basa en la banda B05 (704 nm) de Sentinel-2 MSI "
        ),
        popover(
          HTML('<i class="bi bi-info-circle-fill"></i>.'),
          includeMarkdown(bib[1])
        )
      ),
      span(
        HTML(
          "La estimación de profundidad de disco de Secchi utiliza la banda X "
        ),
        popover(
          HTML('<i class="bi bi-info-circle-fill"></i>.'),
          includeMarkdown(bib[1])
        )
      )
    ),
    card(
      card_header(
        span(
          "Visualización en color real y distribución espacial de turbidez y profundidad de disco",
          class = "card-titulo"
        )
      ),
      leafletOutput("mapa_interactivo")
    )
  )
)

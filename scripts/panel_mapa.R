panel_mapa <- nav_panel(
  title = h5(
    span(
      HTML('<span class="material-symbols--map"></span>'),
      "Mapas"
    )
  ),
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
          "Los modelos para estimar <b>turbidez</b> y <b>profuncidad de disco de Secchi</b> son <em>random forest</em> que utilizan las bandas disponibles de Sentinel-2, sensor MSI, nivel de procesamiento L2A en reflectancia de superficie"
        ),
        popover(
          HTML('<i class="bi bi-info-circle-fill"></i>.'),
          includeMarkdown(bib[6])
        )
      ),
      hr(),
      downloadButton(
        "descarga_raster",
        strong("Descargar ráster"),
        style = glue::glue("background-color: {verde};color: white;")
      )
    ),
    card(
      card_header(
        span(
          "Visualización en color real y distribución espacial de turbidez y profundidad de disco",
          class = "card-titulo"
        ),
        nav_spacer(),
        uiOutput("gear_paleta")
      ),
      leafletOutput("mapa_interactivo")
    )
  )
)

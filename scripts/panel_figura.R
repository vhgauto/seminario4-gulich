panel_figura <- nav_panel(
  title = h5("Firma espectral"),

  layout_sidebar(
    sidebar = sidebar(
      title = h1("Opciones"),
      selectInput(
        "firma_espectral_sensor",
        "Procesador",
        c("ACOLITE", "SEN2COR")
      ),
      selectInput(
        "fecha_firma_espectral",
        "Fecha",
        fechas_firma_espectral
      ),
      hr(),
      h1("Corrección atmosférica"),
      icon = HTML('<i class="bi bi-leaf-fill"></i>'),
      span(
        HTML(
          "El método de corrección atmosférica de los productos <b>Seninel-2</b> es <i>Sen2Cor</i> "
        ),
        popover(
          HTML('<i class="bi bi-info-circle-fill"></i>.'),
          includeMarkdown(bib[2])
        )
      ),
      span(
        HTML(
          "Como alternativa al procesamiento de datos, <i>ACOLITE</i>"
        ),
        popover(
          HTML('<i class="bi bi-info-circle-fill"></i>.'),
          includeMarkdown(bib[3])
        )
      )
    ),
    card(
      card_header(
        span(
          "Distancia relativa de los sitios de muestreo y firma espectral",
          class = "card-titulo"
        )
      ),
      girafeOutput("firma_espectral_plot")
    )
  )
)

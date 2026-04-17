panel_figura <- nav_panel(
  title = h5("Firma espectral"),
  card(
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
          fechas
        ),
        hr(),
        h1("Corrección atmosférica"),
        icon = HTML('<i class="bi bi-leaf-fill"></i>'),
        span(
          HTML(
            "El método de corrección atmosférica de los productos <b>Seninel-2</b> es <i>Sen2Cor</i> "
          ),
          popover(
            HTML('<i class="bi bi-info-circle-fill"></i>'),
            includeMarkdown(bib[1])
          )
        ),
        span(
          HTML(
            "Como alternativa al procesamiento de datos, <i>ACOLITE</i>"
          ),
          popover(
            HTML('<i class="bi bi-info-circle-fill"></i>'),
            includeMarkdown(bib[2])
          )
        )
      ),
      h1("Cuestiones"),
      girafeOutput("firma_espectral_plot")
    )
  )
)

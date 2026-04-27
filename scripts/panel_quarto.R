panel_quarto <- nav_panel(
  title = h5(
    span(
      HTML('<span class="oui--app-reporting"></span>'),
      "Reporte"
    )
  ),
  layout_columns(
    col_widths = c(3, 6, 3),
    layout_column_wrap(
      width = 1,
      card(
        card_header(span("Firma espectral", class = "card-titulo")),
        selectInput(
          "quarto_fecha_firma",
          label = HTML("Seleccionar fecha para <b>firma espectral</b>"),
          choices = quarto_fecha_firma
        ),
        "Se creará una figura de la firma espectral para la fecha seleccionada, destacando las orillas de la Provincia de Chaco y Corrientes."
      ),
      card(
        card_header(span("Mapa", class = "card-titulo")),
        selectInput(
          "quarto_fecha_mapa",
          label = HTML(
            "Seleccionar fecha para mapa de <b>turbidez</b> y <b>profundidad de disco de Secchi</b>"
          ),
          choices = quarto_fecha_mapa
        ),
        "Se mostrarán dos mapas con la distribución espacial de los parámetros sobre la región de estudio en el Río Paraná."
      )
    ),
    card(
      card_header(span("Ejemplo de reporte", class = "card-titulo")),
      img(src = "p.png")
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      input_task_button(
        id = "render_quarto",
        label = span(
          bsicons::bs_icon("pen-fill"),
          "Generar reporte",
          style = "font-size: 2em"
        ),
        label_busy = span(
          "El reporte estará listo en pocos segundos",
          style = "font-size: 2em"
        ),
        icon_busy = HTML(
          '<span class="svg-spinners--6-dots-scale-middle"></span>'
        ),
        style = "height: 200px"
      ),
      uiOutput("boton_descarga_reporte")
    )
  )
)

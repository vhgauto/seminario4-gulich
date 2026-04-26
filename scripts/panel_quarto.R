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
          label = "Seleccionar fecha para firma espectral",
          choices = quarto_fecha_firma
        )
      ),
      card(
        card_header(span("Mapa", class = "card-titulo")),
        selectInput(
          "quarto_fecha_mapa",
          label = "Seleccionar fecha para mapa de turbidez y profundidad de disco de Secchi.",
          choices = quarto_fecha_mapa
        )
      )
    ),
    card(
      card_header(span("Ejemplo", class = "card-titulo")),
      "Single card taking up the entire second column."
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      input_task_button(
        id = "render_quarto",
        label = span("Generar reporte", style = "font-size: 2em"),
        label_busy = span(
          "El reporte estará listo en pocos segundos", style = "font-size: 2em"
        ),
        style = "height: 200px"
      ),
      uiOutput("boton_descarga_reporte")
    )
  )
)
# panel_quarto

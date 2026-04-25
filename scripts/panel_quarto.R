# panel_quarto <- nav_panel(

# layout_sidebar(
#   sidebar = sidebar(
#     title = h1("PDF"),
#     selectInput(
#       "quarto_fecha_firma",
#       label = "Opciones",
#       choices = quarto_fecha_firma
#     ),
#     selectInput(
#       "quarto_fecha_mapa",
#       label = "Opciones",
#       choices = quarto_fecha_mapa
#     )
#   ),
#   card(
#     input_task_button("render_quarto", "Generar reporte"),
#     uiOutput("boton_descarga_reporte")
#   )
# )
# )

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
        card_header("Card 1"),
        selectInput(
          "quarto_fecha_firma",
          label = "Opciones",
          choices = quarto_fecha_firma
        )
      ),
      card(
        card_header("Card 2"),
        selectInput(
          "quarto_fecha_mapa",
          label = "Opciones",
          choices = quarto_fecha_mapa
        )
      )
    ),
    card(
      card_header("Card 3"),
      "Single card taking up the entire second column."
    ),
    layout_column_wrap(
      width = 1,
      # heights_equal = "row",
      # height = 300,
      fill = FALSE,
      input_task_button(
        "render_quarto",
        "Generar reporte",
        # width = "100%",
        # height = "100%",
        style = "height: 200px"
      ),
      uiOutput("boton_descarga_reporte")
      # downloadButton(
      #   "gg",
      #   "HOLA",
      #   style = "height: 200px; text-align: center; color: red;
      #   vertical-align: middle;"
      # )
    )
  )
)
# panel_quarto

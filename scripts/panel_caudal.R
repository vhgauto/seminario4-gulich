panel_caudal <- nav_panel(
  title = h5(
    span(
      HTML('<span class="ph--flow-arrow-fill"></span>'),
      "Caudal"
    )
  ),
  layout_columns(
    col_widths = c(9, 3),
    card(
      card_header(
        span("Caudal del Río Paraná", class = "card-titulo")
      ),
      plotOutput("serie_caudal", click = clickOpts(id = "plot_click"))
    ),
    card(
      card_header(
        span(tooltip(
          span(
            "Contexto",
            bsicons::bs_icon("info-circle")
          ),
          HTML(
            "Altura y caudal obtenidos del </b>Sistema Nacional de Información Hídrica</b>"
          ),
          placement = "right"
        ), class = "card-titulo")),
      uiOutput("value_box_caudal")
    )
  )
)

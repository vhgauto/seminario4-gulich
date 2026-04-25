panel_tabla <- nav_panel(
  title = h5(
    span(
      HTML('<span class="carbon--qq-plot"></span>'),
      "Correlaciones"
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      title = h1("Opciones"),
      checkboxGroupInput(
        "param_tabla",
        h3("Parámetros"),
        param_nombre
      ),
      hr(),
      shinyWidgets::prettySwitch(
        "corr_mejor",
        "Mejor correlación",
        fill = TRUE,
        inline = TRUE,
        bigger = TRUE,
        status = "info"
      )
    ),
    card(
      card_header(
        span(
          "Correlaciones lineales entre parámetros fisicoquímicos y bandas espectrales",
          class = "card-titulo"
        )
      ),
      uiOutput("tabla_corr"),
      card_footer(HTML("<em>r</em>: Coeficiente de correlación de Pearson"))
    )
  )
)

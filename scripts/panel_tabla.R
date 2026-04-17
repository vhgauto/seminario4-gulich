panel_tabla <- nav_panel(
  title = h5("Correlaciones"),
  layout_sidebar(
    sidebar = sidebar(
      title = h1("Opciones"),
      checkboxGroupInput(
        "param_tabla",
        h3("Parámetros"),
        param_nombre
      ),
      h3(HTML("Seleccionar mejor <em>r</em>")),
      shinyWidgets::prettySwitch(
        "corr_mejor",
        "Mostrar mejor correlación",
        fill = TRUE,
        inline = TRUE,
        bigger = TRUE,
        status = "info"
      )
    ),
    card(
      card_header(h1("Tabla")),
      uiOutput("tabla_corr"),
      card_footer(HTML("<em>r</em>: Coeficiente de correlación de Pearson"))
    )
  )
)

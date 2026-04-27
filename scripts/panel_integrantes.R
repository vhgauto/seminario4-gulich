panel_integrantes <- nav_panel(
  title = h5(
    span(
      HTML('<span class="ic--sharp-person-pin"></span>'),
      "Acerca de"
    )
  ),
  layout_columns(
    col_widths = c(6, 6, 12),
    card(
      card_header(span("Seminario", class = "card-titulo")),
      span(
        HTML(
          "Esta aplicación forma parte del Seminario <b>Shiny para crear aplicaciones orientadas al sensado remoto</b> de <b style='color: #007e2e'>Mgtr. Víctor Gauto</b>, en el marco del <b>Doctorado en Geomática y Sistemas Espaciales</b> del Instituto Gulich."
        ),
        style = "font-size: 2em"
      )
    ),
    card(
      card_header(span("Integrantes", class = "card-titulo")),
      f_integrante(
        "Mgtr",
        "Víctor Gauto",
        "0000-0001-9960-8558",
        "victor.gauto@ca.frre.utn.edu.ar"
      ),
      f_integrante("Mgtr", "Enid Utgés", "0009-0003-5263-5198"),
      f_integrante("Dr", "Matías Bonansea", "0000-0003-1953-2595"),
      f_integrante("Dr", "Anabella Ferral", "0000-0002-9383-7728"),
      f_integrante("Dr", "Osvaldo Cardozo", "0000-0002-0345-4505")
    ),
    card(
      card_header(span("Instituciones", class = "card-titulo")),
      layout_columns(
        layout_column_wrap(
          width = 1 / 4,
          !!!lapply(logo_tbl$org, f_logo)
        )
      )
    )
  )
)

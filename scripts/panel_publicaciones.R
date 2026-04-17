panel_publicaciones <- nav_panel(
  title = h5("Publicaciones"),
  card(
    h4(
      icon_paper,
      "Remote Sensing Regression Models to Estimate Water Quality Indicators in Continental Waters in North-East Argentina"
    ),
    a(
      icon_doi,
      "10.1109/ARGENCON62399.2024.10735875",
      href = "https://doi.org/10.1109/ARGENCON62399.2024.10735875",
      target = "_blank"
    ),
    hr(),
    h4(
      icon_paper,
      "Turbidity Estimation by Machine Learning Modelling and Remote Sensing Techniques Applied to a Water Treatment Plant"
    ),
    a(
      icon_doi,
      "10.13044/j.sdewes.d13.0539",
      href = "http://dx.doi.org/10.13044/j.sdewes.d13.0539",
      target = "_blank"
    )
  )
)

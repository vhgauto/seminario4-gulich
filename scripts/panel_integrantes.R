panel_integrantes <- nav_panel(
  title = h5(
    span(
      HTML('<span class="ic--sharp-person-pin"></span>'),
      "Integrantes"
    )
  ),
  card(
    h1("Integrantes"),
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
    h1("Instituciones"),
    span(
      a(
        img(
          src = "logo_gistaq.png",
          height = "70"
        ),
        href = "https://www.instagram.com/gistaq.utn",
        target = "_blank",
        .noWS = "before-end"
      ),
      a(
        img(
          src = "logo_ig.png",
          height = "70"
        ),
        href = "https://www.instagram.com/gistaq.utn",
        target = "_blank",
        .noWS = "before-end"
      ),
      a(
        img(
          src = "logo_iidthh.png",
          height = "70"
        ),
        href = "https://www.instagram.com/gistaq.utn",
        target = "_blank",
        .noWS = "before-end"
      ),
      a(
        img(
          src = "logo_conae.png",
          height = "70"
        ),
        href = "https://www.instagram.com/gistaq.utn",
        target = "_blank",
        .noWS = "before-end"
      ),
      class = "eqi-container"
    ),
    span(
      a(
        img(
          src = "logo_utn.png",
          height = "70"
        ),
        href = "https://www.instagram.com/gistaq.utn",
        target = "_blank",
        .noWS = "before-end"
      ),
      a(
        img(
          src = "logo_unc.svg",
          height = "70"
        ),
        href = "https://www.instagram.com/gistaq.utn",
        target = "_blank",
        .noWS = "before-end"
      ),
      a(
        img(
          src = "logo_unne.png",
          height = "70"
        ),
        href = "https://www.instagram.com/gistaq.utn",
        target = "_blank",
        .noWS = "before-end"
      ),
      a(
        img(
          src = "logo_conicet.png",
          height = "70"
        ),
        href = "https://www.instagram.com/gistaq.utn",
        target = "_blank",
        .noWS = "before-end"
      ),
      class = "eqi-container"
    )
  )
)

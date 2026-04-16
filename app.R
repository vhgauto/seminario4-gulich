source("scripts/soporte.R")

panel_mapa <- nav_panel(
  title = h5("Distribución geográfica"),
  layout_sidebar(
    sidebar = sidebar(
      title = h1("Opciones"),
      selectInput(
        "fecha",
        "Fecha",
        fechas
      ),
      selectInput(
        "tipo",
        "Tipo de mapa",
        c("RGB", "Turbidez", "Profundidad de disco")
      )
    ),
    card(
      card_header(h1("Mapa")),
      leafletOutput("mymap")
    )
  )
)

panel_figura <- nav_panel(
  title = h5("Firma espectral"),
  card(
    layout_sidebar(
      sidebar = sidebar(
        title = h1("Opciones"),
        selectInput("firma_espectral_sensor", "Sensor", c("ACOLITE", "SEN2COR")),
        selectInput(
          "fecha_firma_espectral",
          "Fecha",
          fechas
        ),
        card(
          "Corrección atmosférica",
          h2("ACOLITE"),
          "cosas",
          h2("Sen2Cor"),
          "cosas"
        )
      ),
      h1("Cuestiones"),
      girafeOutput("firma_espectral_plot")
    )
  )
)

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

panel_integrantes <- nav_panel(
  title = h5("Integrantes"),
  card(
    h1("Integrantes"),
    span(
      "Víctor Gauto",
      a(
        HTML(
          '<span class="simple-icons--orcid"></span>'
        ),
        href = "https://orcid.org/0000-0001-9960-8558",
        target = "_blank"
      ),
      a(
        HTML('<span class="ic--round-email"></span>'),
        href = "mailto:victor.gauto@ca.frre.utn.edu.ar",
        target = "_blank"
      )
    ),
    span(
      "Enid Utgés",
      a(
        HTML('<span class="simple-icons--orcid"></span>'),
        href = "https://orcid.org/0009-0003-5263-5198",
        target = "_blank"
      )
    ),
    span(
      "Matías Bonansea",
      a(
        HTML('<span class="simple-icons--orcid"></span>'),
        href = "https://orcid.org/0000-0003-1953-2595",
        target = "_blank"
      )
    ),
    span(
      "Anabella Ferral",
      a(
        HTML('<span class="simple-icons--orcid"></span>'),
        href = "https://orcid.org/0000-0002-9383-7728",
        target = "_blank"
      )
    ),
    span(
      "Osvaldo Cardozo",
      a(
        HTML('<span class="simple-icons--orcid"></span>'),
        href = "https://orcid.org/0000-0002-0345-4505"
      )
    )
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
      class = "eqi-container"),
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

ui <- page_navbar(
  title = a(
    "Proyecto Paraná",
    href = "https://019d775d-3cc3-1ed8-fc69-3a457d8f7a43.share.connect.posit.cloud/",
    style = "text-decoration: none; color: #007e2e;"
  ),
  navbar_options = navbar_options(
    bg = "#e5e5e5",
    underline = TRUE
  ),
  panel_mapa,
  panel_figura,
  panel_publicaciones,
  panel_integrantes,
  nav_spacer(),
  nav_item(rrss_instagram),
  nav_item(rrss_github),
  footer = span(
    span(
      "Creado por",
      a(
        strong("Víctor Gauto"),
        href = "mailto:victor.gauto@outlook.com",
        target = "_blank",
        .noWS = "before-end"
      ),
      "|",
      a(
        icon("instagram"),
        href = "https://www.instagram.com/vhgauto",
        target = "_blank",
        .noWS = "before-end"
      ),
      a(
        icon("github"),
        href = "https://github.com/vhgauto",
        target = "_blank"
      )
    ),
    style = "padding: .4em; border-top: solid black 1px;
    background-color: #e5e5e5; text-align: right"
  ),
  theme = bs_theme(
    fg = "#341648", # MetBrewer: Tam
    bg = "#f2f2f2",
    base_font = font_google("Nunito"),
    heading_font = font_google("Bebas Neue"),
    code_font = font_google("Fira Code"),
    primary = "#9f2d54"
  ) |>
    bs_add_rules(sass::sass_file("extras/mis_estilos.scss"))
)


server <- function(input, output) {
  observeEvent(input$tipo, {
    tipo <- reactive(input$tipo)
    if (tipo() == "RGB") {
      output$mymap <- renderLeaflet({
        leaflet_rgb(input$fecha)
      })
    }
    if (tipo() == "Turbidez") {
      output$mymap <- renderLeaflet({
        leaflet_tipo(FECHA = input$fecha, TIPO = "turb")
      })
    }
    if (tipo() == "Profundidad de disco") {
      output$mymap <- renderLeaflet({
        leaflet_tipo(FECHA = input$fecha, TIPO = "secchi")
      })
    }
  })
  
  observeEvent(input$firma_espectral_sensor ,{
    sensor <- reactive(input$firma_espectral_sensor)
    
    if (sensor() == "ACOLITE") {
      output$firma_espectral_plot <- renderGirafe({
        f_firma_espectral(FECHA = input$fecha_firma_espectral, VAR = "reflect_acolite")
      })
    }
    
    if (sensor() == "SEN2COR") {
      output$firma_espectral_plot <- renderGirafe({
        f_firma_espectral(FECHA = input$fecha_firma_espectral, VAR = "reflect_sen2cor")
      })
    }
  })
}

shinyApp(ui, server)

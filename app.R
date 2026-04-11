source("scripts/soporte.R")

rrss_instagram <- tags$a(
  shiny::icon("instagram"),
  href = "https://www.instagram.com/gistaq.utn",
  target = "_blank",
  style = "font-size: 1.3em;"
)

rrss_github <- tags$a(
  shiny::icon("github"),
  href = "https://github.com/vhgauto/seminario4-gulich",
  target = "_blank",
  style = "font-size: 1.3em;"
)

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
        c("RGB", "Turbidez")
      )
    ),
    card(
      card_header(h1("Mapa")),
      leafletOutput("mymap")
    )
  )
)

panel_figura <- nav_panel(
  title = h5("Figuras"),
  layout_sidebar(
    sidebar = sidebar(
      title = h1("Control")
    ),
    h1("Cuestiones")
  )
)


ui <- page_navbar(
  title = a(
    # img(src = "gistaq.png", width = "15%", style = "padding-left: .3em;"),
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
  nav_spacer(),
  nav_item(rrss_instagram),
  nav_item(rrss_github),
  footer = span(
    span(
      "Creado por",
      a(
        strong("Víctor Gauto"),
        href = "mailto:victor.gauto@outlook.com",
        target = "_blank"
      ),
      "|",
      a(
        icon("instagram"),
        href = "https://www.instagram.com/vhgauto",
        target = "_blank"
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
        leaflet_turb(input$fecha)
      })
    }
  })
}

shinyApp(ui, server)

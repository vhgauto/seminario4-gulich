source("scripts/soporte.R")
source("scripts/panel_mapa.R")
source("scripts/panel_figura.R")
source("scripts/panel_tabla.R")
source("scripts/panel_publicaciones.R")
source("scripts/panel_integrantes.R")

ui <- page_navbar(
  title = a(
    HTML("Proyecto Paraná"),
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
  panel_tabla,
  nav_spacer(),
  nav_item(rrss_instagram),
  nav_item(rrss_github),
  footer = pie,
  theme = bs_theme(brand = "_brand.yml") |>
    bs_add_rules(sass::sass_file("extras/mis_estilos.scss"))
)

server <- function(input, output) {
  # MAPA
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

  # FIRMA ESPECTRAL
  observeEvent(input$firma_espectral_sensor, {
    sensor <- reactive(input$firma_espectral_sensor)

    if (sensor() == "ACOLITE") {
      output$firma_espectral_plot <- renderGirafe({
        f_firma_espectral(
          FECHA = input$fecha_firma_espectral,
          VAR = "reflect_acolite"
        )
      })
    }

    if (sensor() == "SEN2COR") {
      output$firma_espectral_plot <- renderGirafe({
        f_firma_espectral(
          FECHA = input$fecha_firma_espectral,
          VAR = "reflect_sen2cor"
        )
      })
    }
  })

  # TABLA CORRELACIONES
  observeEvent(input$param_tabla, {
    param_tabla <- reactive(input$param_tabla)
    corr_mejor <- reactive(input$corr_mejor)

    output$tabla_corr <- renderUI({
      l <- map2(input$param_tabla, input$corr_mejor, ~ f_tabla(.x, .y))

      layout_columns(!!!l)
    })
  })
}

shinyApp(ui, server)

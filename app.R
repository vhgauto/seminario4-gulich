source("scripts/soporte.R")
source("scripts/panel_mapa.R")
source("scripts/panel_figura.R")
source("scripts/panel_tabla.R")
source("scripts/panel_serie_temporal.R")
source("scripts/panel_publicaciones.R")
source("scripts/panel_integrantes.R")

# TODO: modelo secchi
# TODO: serie temporal caudal
# TODO: botones de descarga para tif

ui <- page_navbar(
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
  title = a(
    HTML("Proyecto Paraná"),
    href = "https://019d775d-3cc3-1ed8-fc69-3a457d8f7a43.share.connect.posit.cloud/",
    style = glue("text-decoration: none; color: {verde};")
  ),
  navbar_options = navbar_options(
    bg = "#e5e5e5",
    underline = TRUE
  ),
  panel_mapa,
  panel_figura,
  panel_tabla,
  panel_serie_temporal,
  panel_publicaciones,
  panel_integrantes,
  nav_spacer(),
  nav_item(rrss_instagram),
  nav_item(rrss_github),
  footer = pie,
  theme = bs_theme(brand = "_brand.yml") |>
    bs_add_rules(sass::sass_file("extras/mis_estilos.scss")),
  ff
)

server <- function(input, output) {
  # MAPA
  observeEvent(input$tipo, {
    tipo <- reactive(input$tipo)
    if (tipo() == "RGB") {
      output$mapa_interactivo <- renderLeaflet({
        leaflet_rgb(input$fecha)
      })
    }
    if (tipo() == "Turbidez") {
      output$mapa_interactivo <- renderLeaflet({
        leaflet_tipo(FECHA = input$fecha, TIPO = "turb")
      })
    }
    if (tipo() == "Profundidad de disco") {
      output$mapa_interactivo <- renderLeaflet({
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

  # SERIE TEMPORAL
  condición_fechas <- reactive({
    input$fecha_altura_min > input$fecha_altura_max
  })
  observeEvent(
    condición_fechas(),
    {
      if (condición_fechas()) {
        showModal(
          modalDialog(
            title = "Rango de fechas incorrecto.",
            easyClose = TRUE,
            "La fecha inicial debe ser menor a la fecha final.",
            footer = modalButton("Cerrar")
          )
        )
        output$serie_temporal_altura <- renderGirafe({
          f_serie_temporal_altura(
            FECHA_MIN = fecha_altura_min,
            FECHA_MAX = fecha_altura_max
          )
        })
      }
    }
  )

  observeEvent(input$serie_temporal_ma, {
    serie_temporal_ma <- reactive(input$serie_temporal_ma)
    if (serie_temporal_ma()) {
      output$slider_ma <- renderUI({
        sliderInput(
          "ma",
          "Elegir tamaño de ventana, en días.",
          min = 10,
          max = 50,
          value = 20,
          step = seq(10, 50, 10)
        )
      })
    } else {
      output$slider_ma <- renderUI({
        NULL
      })
    }
  })

  observeEvent(
    list(
      input$fecha_altura_min,
      input$fecha_altura_max,
      input$serie_temporal_ma,
      input$ma
    ),
    {
      output$serie_temporal_altura <- renderGirafe({
        if (input$serie_temporal_ma && !is.null(input$ma)) {
          f_serie_temporal_altura(
            FECHA_MIN = input$fecha_altura_min,
            FECHA_MAX = input$fecha_altura_max,
            ma = TRUE,
            n_ma = as.numeric(input$ma)
          )
        } else {
          f_serie_temporal_altura(
            FECHA_MIN = input$fecha_altura_min,
            FECHA_MAX = input$fecha_altura_max,
            ma = FALSE,
            n_ma = 20
          )
        }
      })
    }
  )

  observeEvent(
    list(
      input$fecha_altura_min,
      input$fecha_altura_max
    ),
    {
      output$descarga_serie_temporal <- downloadHandler(
        filename = function() {
          paste0(
            input$fecha_altura_min,
            "_",
            input$fecha_altura_max,
            "_altura.csv"
          )
        },
        content = function(file) {
          f_descarga_serie_temporal(
            FECHA_MIN = input$fecha_altura_min,
            FECHA_MAX = input$fecha_altura_max,
            FILE = file
          )
        }
      )
    }
  )
}

shinyApp(ui, server)

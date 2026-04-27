source("scripts/soporte.R")
source("scripts/panel_quarto.R")
source("scripts/panel_mapa.R")
source("scripts/panel_figura.R")
source("scripts/panel_tabla.R")
source("scripts/panel_serie_temporal.R")
source("scripts/panel_caudal.R")
source("scripts/panel_publicaciones.R")
source("scripts/panel_integrantes.R")

# TODO: verificar símbolo decimal en figuras/tablas
# TODO: verificar paquetes, no cargarlos a todos al inicio

ui <- page_navbar(
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
  title = a(
    HTML("Proyecto Paraná"),
    href = "https://vhgauto-seminario4-gulich.share.connect.posit.cloud/",
    style = glue::glue("text-decoration: none; color: {verde};")
  ),
  navbar_options = navbar_options(
    bg = "#e5e5e5",
    underline = TRUE
  ),
  panel_mapa,
  panel_figura,
  panel_caudal,
  panel_tabla,
  panel_serie_temporal,
  panel_quarto,
  panel_publicaciones,
  panel_integrantes,
  nav_spacer(),
  nav_item(rrss_instagram),
  nav_item(rrss_github),
  footer = pie,
  theme = bs_theme(brand = "_brand.yml") |>
    bs_add_rules(sass::sass_file("extras/mis_estilos.scss"))
)

server <- function(input, output, session) {
  # QUARTO ----
  observeEvent(
    list(
      input$quarto_fecha_firma,
      input$quarto_fecha_mapa,
      input$quarto_fecha_tabla,
      input$render_quarto
    ),
    {
      params_recepcion <- list(
        firma = input$quarto_fecha_firma,
        mapa = input$quarto_fecha_mapa
      )

      terminado <- reactiveVal(FALSE)

      render_quarto <- reactive(input$render_quarto)

      if (render_quarto()) {
        output$boton_descarga_reporte <- renderUI({
          NULL
        })
        f_quarto(FILE = file, PARAMS = params_recepcion)
        terminado(TRUE)
      }

      output$boton_descarga_reporte <- renderUI({
        req(terminado())
        downloadButton(
          "descarga_pdf",
          "Descargar PDF",
          style = glue::glue(
            "height: 200px; background-color:{verde}; color: white;",
            "font-size: 2em; padding-top: 70px;"
          )
        )
      })

      output$descarga_pdf <- downloadHandler(
        filename = function() {
          "p.pdf"
        },
        content = function(file) {
          file.copy("p.pdf", file)
        }
      )
    }
  )

  # MAPA ----
  # paleta de colores para TURB y SECCHI
  observeEvent(
    input$tipo,
    {
      tipo <- reactive(input$tipo)
      if (tipo() != "RGB") {
        output$gear_paleta <- renderUI({
          popover(
            bsicons::bs_icon("gear-fill", color = "white", size = "1.5em"),
            selectInput(
              "paletas_mapa",
              label = "Seleccionar paleta de colores",
              choices = paletas,
              selected = input$paletas_mapa
            )
          )
        })
      } else {
        output$gear_paleta <- renderUI({
          NULL
        })
      }
    }
  )

  # mapa interactivo
  observeEvent(list(input$tipo, input$paletas_mapa), {
    tipo <- reactive(input$tipo)
    paletas_mapa <- reactive(input$paletas_mapa)

    if (tipo() == "RGB") {
      output$mapa_interactivo <- renderLeaflet({
        leaflet_rgb(input$fecha)
      })
    }

    if (tipo() == "Turbidez" && !is.null(paletas_mapa())) {
      output$mapa_interactivo <- renderLeaflet({
        leaflet_tipo(
          FECHA = input$fecha,
          TIPO = "turb",
          PALETA = paletas_mapa()
        )
      })
    }

    if (tipo() == "Profundidad de disco" && !is.null(paletas_mapa())) {
      output$mapa_interactivo <- renderLeaflet({
        leaflet_tipo(
          FECHA = input$fecha,
          TIPO = "secchi",
          PALETA = paletas_mapa()
        )
      })
    }
  })

  # descarga
  observeEvent(list(input$fecha, input$tipo), {
    output$descarga_raster <- downloadHandler(
      filename = function() {
        paste0(
          input$fecha,
          "_",
          input$tipo,
          ".tif"
        )
      },
      content = function(file) {
        f_descarga_raster(FECHA = input$fecha, TIPO = input$tipo, FILE = file)
      }
    )
  })

  # FIRMA ESPECTRAL ----
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

  # TABLA CORRELACIONES ----
  output$tabla_corr <- renderUI({
    if (is.null(input$param_tabla) || length(input$param_tabla) == 0) {
      card(
        strong(
          "Seleccionar un parámetro para mostrar la tabla de correlaciones."
        ),
        style = "background-color: #a9a9a9; color: white; font-size: 2em;"
      )
    } else {
      l <- map2(input$param_tabla, input$corr_mejor, ~ f_tabla(.x, .y))
      layout_columns(!!!l)
    }
  })

  # SERIE TEMPORAL ----
  condición_fechas <- reactive({
    input$fecha_altura_min > input$fecha_altura_max
  })
  condición_período_fechas <- reactive({
    as.numeric(input$fecha_altura_max - input$fecha_altura_min) > 0 &
      as.numeric(input$fecha_altura_max - input$fecha_altura_min) < 60
  })
  observeEvent(
    list(input$fecha_altura_min, input$fecha_altura_max),
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

  observeEvent(
    list(
      input$serie_temporal_ma,
      input$fecha_altura_min,
      input$fecha_altura_max
    ),
    {
      serie_temporal_ma <- reactive(input$serie_temporal_ma)
      condición_período_fechas <- reactive({
        as.numeric(input$fecha_altura_max - input$fecha_altura_min) > 0 &
          as.numeric(input$fecha_altura_max - input$fecha_altura_min) < 60
      })
      if (serie_temporal_ma() & condición_período_fechas()) {
        showModal(
          modalDialog(
            title = "Rango de fechas debe ser superior a 60 días.",
            easyClose = TRUE,
            "La fecha.",
            footer = modalButton("Cerrar")
          )
        )

        output$slider_ma <- renderUI({
          NULL
        })
        output$serie_temporal_altura <- renderGirafe({
          f_serie_temporal_altura(
            FECHA_MIN = fecha_altura_min,
            FECHA_MAX = fecha_altura_max
          )
        })
        shinyWidgets::updatePrettySwitch(
          session = session,
          inputId = "serie_temporal_ma",
          value = FALSE
        )
      }

      if (serie_temporal_ma() & !condición_período_fechas()) {
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
    }
  )

  observeEvent(
    list(
      input$fecha_altura_min,
      input$fecha_altura_max,
      input$serie_temporal_ma,
      input$ma
    ),
    {
      período_válido <- as.numeric(
        input$fecha_altura_max - input$fecha_altura_min
      ) >=
        60
      output$serie_temporal_altura <- renderGirafe({
        if (input$serie_temporal_ma && !is.null(input$ma) && período_válido) {
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
  # CAUDAL ----
  output$serie_caudal <- renderPlot({
    f_caudal()
  })

  output$value_box_caudal <- renderUI({
    v <- nearPoints(
      terra::as.data.frame(d_caudal, xy = TRUE),
      input$plot_click,
      xvar = "fecha",
      yvar = "caudal",
      addDist = FALSE,
      allRows = FALSE,
      maxpoints = 1
    )

    l <- list(
      value_box(
        title = "Fecha",
        value = v$fecha,
        showcase = bsicons::bs_icon("calendar-event-fill"),
        theme = value_box_theme(bg = "#324d5a"),
        class = "p-0"
      ),
      value_box(
        title = markdown("Caudal (m<sup>3</sup> s<sup>-1</sup>)"),
        value = formato(round(v$caudal, 0)),
        showcase = bsicons::bs_icon("water"),
        theme = value_box_theme(bg = "#451b40"),
        span(
          "Fuente: ",
          a(
            HTML("<b>Sistema Nacional de Información Hídrica</b>"),
            href = "https://snih.hidricosargentina.gob.ar/",
            target = "_blank",
            style = "color:white"
          ),
          style = "color:white"
        )
      ),
      value_box(
        title = "Altura (m)",
        value = formato(round(v$altura, 1), nsmall = 1),
        showcase = bsicons::bs_icon("arrow-up-right-circle-fill"),
        theme = value_box_theme(bg = "#589445"),
        class = "p-0",
        span(
          "Fuente: ",
          a(
            HTML("<b>Sistema Nacional de Información Hídrica</b>"),
            href = "https://snih.hidricosargentina.gob.ar/",
            target = "_blank",
            style = "color:white"
          ),
          style = "color:white"
        )
      ),
      value_box(
        title = "Temperatura (°C)",
        value = formato(round(v$temperatura, 1), nsmall = 1),
        showcase = bsicons::bs_icon("thermometer-half"),
        theme = value_box_theme(bg = "#dcc88b"),
        class = "p-0"
      ),
      value_box(
        title = markdown("Viento (km h<sup>-1</sup>)"),
        value = formato(round(v$viento, 1), nsmall = 1),
        showcase = bsicons::bs_icon("wind"),
        theme = value_box_theme(bg = "#b5003c"),
        class = "p-0"
      )
    )

    layout_column_wrap(width = 1, !!!l)
  })
}

shinyApp(ui, server)

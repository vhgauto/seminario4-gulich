# paquetes ---------------------------------------------------------------

library(shiny)
library(bslib)
library(leaflet)
library(terra)
library(leafem)
library(leaflet.extras)

# datos ------------------------------------------------------------------

l <- list.files("recortes/", full.names = TRUE)
fechas <- basename(l) |>
  gsub(".tif", "", x = _) |>
  as.Date()
r <- lapply(l, rast)
r <- setNames(r, fechas)

# funciones --------------------------------------------------------------

# convierte el ráster a escala de 255 para visualizar en color real RGB
f_escalado <- function(FECHA) {
  w <- r[[as.character(FECHA)]]

  r_rango <- global(w$B04, c("min", "max"))
  g_rango <- global(w$B03, c("min", "max"))
  b_rango <- global(w$B02, c("min", "max"))

  w$B04 <- (w$B04 - r_rango$min) * 255 / (r_rango$max - r_rango$min)
  w$B03 <- (w$B03 - g_rango$min) * 255 / (g_rango$max - g_rango$min)
  w$B02 <- (w$B02 - b_rango$min) * 255 / (b_rango$max - b_rango$min)

  return(w)
}

# extrae los píxeles de agua y aplica modelo de turbidez
f_turb <- function(FECHA) {
  y <- r[[as.character(FECHA)]]
  mndwi <- (y$B03 - y$B11) / (y$B03 + y$B11)
  mascara <- thresh(mndwi, method = "otsu")
  mascara[isFALSE(mascara)] <- NA
  y_mascara <- y * mascara
  p <- 200 * y_mascara$B05 + 10
  p <- setNames(p, "turb")
  return(p)
}

# extrae los píxeles de agua y aplica modelo de profundidad de disco
f_secchi <- function(FECHA) {
  y <- r[[as.character(FECHA)]]
  mndwi <- (y$B03 - y$B11) / (y$B03 + y$B11)
  mascara <- thresh(mndwi, method = "otsu")
  mascara[isFALSE(mascara)] <- NA
  y_mascara <- y * mascara
  p <- 10 * y_mascara$B05 + 5
  p <- setNames(p, "secchi")
  return(p)
}

# mapa leaflet de ráster en color real RGB
leaflet_rgb <- function(FECHA) {
  leaflet() |>
    addTiles(group = "OSM (default)") |>
    addProviderTiles(
      providers$CartoDB.Positron,
      group = "Positron (minimal)",
      options = providerTileOptions(noWrap = TRUE)
    ) |>
    addProviderTiles(
      providers$Esri.WorldImagery,
      group = "World Imagery (satellite)",
      options = providerTileOptions(noWrap = TRUE)
    ) |>
    addRasterRGB(
      r[[as.character(FECHA)]],
      r = 4,
      g = 3,
      b = 2,
      quantiles = c(.2, .98),
      na.color = NA,
      group = "RGB"
    ) |>
    addLayersControl(
      baseGroups = c(
        "OSM (default)",
        "Positron (minimal)",
        "World Imagery (satellite)"
      ),
      overlayGroups = c("RGB"),
      options = layersControlOptions(collapsed = TRUE)
    ) |>
    addResetMapButton() |>
    addFullscreenControl(position = "bottomright")
}

# mapa leaflet de ráster de turbidez
leaflet_tipo <- function(FECHA, TIPO) {
  if (TIPO == "turb") {
    p <- f_turb(FECHA)
    pal_nombre <- "YlGnBu"
    grupo <- "Turbidez (NTU)"
    titulo <- "Turbidez<br>(NTU)"
  }

  if (TIPO == "secchi") {
    p <- f_secchi(FECHA)
    pal_nombre <- "YlOrBr"
    grupo <- "Profuncidad de disco (cm)"
    titulo <- "Profuncidad de<br>disco (cm)"
  }

  g <- global(p, c("min", "max"), na.rm = TRUE)
  d <- .1
  p[p < (1 + d) * g$min] <- NA
  p[p > (1 - d) * g$max] <- NA
  v <- na.omit(values(p))

  pal <- colorNumeric(
    palette = pal_nombre,
    domain = v,
    na.color = NA
  )

  leaflet() |>
    addTiles(group = "OSM (default)") |>
    addProviderTiles(
      providers$CartoDB.Positron,
      group = "Positron (minimal)",
      options = providerTileOptions(noWrap = TRUE)
    ) |>
    addProviderTiles(
      providers$Esri.WorldImagery,
      group = "World Imagery (satellite)",
      options = providerTileOptions(noWrap = TRUE)
    ) |>
    addRasterImage(
      p,
      colors = pal,
      group = grupo,
      layerId = grupo
    ) |>
    addImageQuery(
      p,
      layerId = grupo,
      group = grupo,
      digits = 1,
      prefix = ""
    ) |>
    addLegend(
      pal = pal,
      values = v,
      opacity = 1,
      position = "bottomright",
      title = titulo
    ) |>
    addLayersControl(
      baseGroups = c(
        "OSM (default)",
        "Positron (minimal)",
        "World Imagery (satellite)"
      ),
      overlayGroups = grupo,
      options = layersControlOptions(collapsed = TRUE)
    ) |>
    addResetMapButton() |>
    addFullscreenControl(position = "bottomright")
}

# links ------------------------------------------------------------------

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

icon_doi <- HTML('<span class="simple-icons--doi"></span>')
icon_paper <- HTML('<span class="quill--paper"></span>')

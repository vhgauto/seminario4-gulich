# library(tidyverse)
library(shiny)
library(bslib)
library(leaflet)
library(terra)
library(leafem)
library(leaflet.extras)

l <- list.files("recortes/", full.names = TRUE)
fechas <- basename(l) |>
  gsub(".tif", "", x = _) |>
  as.Date()
r <- lapply(l, rast)
r <- setNames(r, fechas)

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

f_turb <- function(FECHA) {
  y <- r[[as.character(FECHA)]]
  mndwi <- (y$B03 - y$B11) / (y$B03 + y$B11)
  mascara <- thresh(mndwi, method = "otsu")
  mascara[isFALSE(mascara)] <- NA
  y_mascara <- y * mascara
  turb <- 200 * y_mascara$B05 + 10
  turb <- setNames(turb, "turb")
  return(turb)
}

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

leaflet_turb <- function(FECHA) {
  r_turb <- f_turb(FECHA)
  g <- global(r_turb, c("min", "max"), na.rm = TRUE)
  d <- .1
  r_turb[r_turb < (1 + d) * g$min] <- NA
  r_turb[r_turb > (1 - d) * g$max] <- NA
  v <- na.omit(values(r_turb))

  pal <- colorNumeric(
    palette = "YlGnBu",
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
      r_turb,
      colors = pal,
      group = "Turbidez (NTU)",
      layerId = "Turbidez (NTU)"
    ) |>
    addImageQuery(
      r_turb,
      layerId = "Turbidez (NTU)",
      group = "Turbidez (NTU)",
      digits = 1,
      prefix = ""
    ) |>
    addLegend(
      pal = pal,
      values = v,
      opacity = 1,
      position = "bottomright",
      title = "Turbidez<br>(NTU)"
    ) |>
    addLayersControl(
      baseGroups = c(
        "OSM (default)",
        "Positron (minimal)",
        "World Imagery (satellite)"
      ),
      overlayGroups = "Turbidez (NTU)",
      options = layersControlOptions(collapsed = TRUE)
    ) |>
    addResetMapButton() |>
    addFullscreenControl(position = "bottomright")
}

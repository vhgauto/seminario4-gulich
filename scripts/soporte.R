# paquetes ---------------------------------------------------------------

library(shiny)
library(bslib)
library(terra)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(ggtext)
library(glue)
library(patchwork)
library(ggthemes)

violeta <- "#341648"
verde <- "#007e2e"
blanco <- "#f2f2f2"
negro <- "#000000"

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

# datos gis/lab -----------------------------------------------------------

banda_fct <- c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")

d <- read.csv("datos/lab_gis.csv") |> 
  as_tibble() |> 
  mutate(banda = factor(x = banda, levels = banda_fct))

f_firma_espectral <- function(FECHA, VAR) {
  e1 <- filter(d, fecha == FECHA) |> 
    distinct(punto, longitud)
  e2 <- filter(d, fecha == FECHA) |> 
    select(all_of(c("punto", "banda", VAR))) |> 
    rename("y" = 3) |> 
    distinct()
  
  g1 <- ggplot(e1, aes(longitud, 1, fill = as.factor(punto))) +
    geom_point_interactive(
      aes(data_id = punto, tooltip = glue("Punto: {punto}")),
      hover_nearest = TRUE , size = 3, shape = 21, stroke = 1, alpha = 1) +
    annotate(
      geom = "text",
      x = c(-Inf, Inf),
      y = 1,
      label = c("Orilla\nChaco", "Orilla\nCorrientes"),
      hjust = c(0, 1),
      vjust = -.5,
      size = 3,
      family = "Fira Code"
    ) +
    scale_x_continuous(expand = expansion(mult = .1, add = 0)) +
    scale_fill_manual(
      values = colorRampPalette(c("brown", "turquoise"))(length(unique(e1$punto))),
      guide = guide_none()
    ) +
    coord_cartesian(clip = "off", expand = FALSE) +
    theme_void(base_family = "Fira Code")
    # # theme_sub_axis(text = element_text(color = negro)) +
    # # theme_sub_axis_y(title = element_markdown(angle = 0, vjust = .5)) +
    # theme_sub_panel(grid.major = element_line(color = "grey80", linewidth = .2),
    #                 background = element_blank()) +
    # theme_sub_plot(background = element_blank())
  
  g2 <-  ggplot(e2, aes(banda, y, group = punto, color = as.factor(punto))) +
    geom_line_interactive(aes(data_id = punto), linewidth = 1, alpha = .5) +
    geom_point_interactive(
      aes(tooltip = round(y, 2), data_id = punto),
      hover_nearest = TRUE , size = 1.7, shape = 21, fill = "white", stroke = 1,
      alpha = .5
    ) +
    labs(x = NULL, y = "R<sub>rs</sub>") +
    scale_color_manual(
      values = colorRampPalette(c("brown", "turquoise"))(length(unique(e2$punto))),
      guide = guide_none()
    ) +
    theme_few(base_family = "Fira Code") +
    theme_sub_axis(text = element_text(color = negro)) +
    theme_sub_axis_y(title = element_markdown(angle = 0, vjust = .5)) +
    theme_sub_panel(grid.major = element_line(color = "grey80", linewidth = .2),
                    background = element_blank()) +
    theme_sub_plot(background = element_blank())
  
  g3 <- g1/g2 + plot_layout(heights = c(1, 5)) &
    theme_sub_panel(grid.major = element_line(color = "grey80", linewidth = .2),
                    background = element_blank()) +
    theme_sub_plot(background = element_rect(
      fill = "transparent",
      color = "transparent"
    ))
  
  girafe(
    ggobj = g3,
    options = list(
      opts_hover_inv(css = "opacity:.2"),
      opts_hover(css = girafe_css(
        css = "",
        point = "opacity:1;",
        line = "opacity:1;"
      )),
      opts_tooltip(
        opacity = 1,
        css = glue(
          "color:{negro};padding:5px;font-family: Fira Code;",
          "border-style:solid;border-color:{violeta};background:{blanco}"
        ),
        use_cursor_pos = TRUE,
        offx = 5,
        offy = 5
      )
    ),
    bg = "transparent"
  )
}
  
f_firma_espectral(fechas[3], "reflect_sen2cor")


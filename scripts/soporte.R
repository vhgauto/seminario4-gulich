# paquetes ---------------------------------------------------------------

library(shiny)
library(bslib)
library(terra)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(ggiraph)
# library(ggtext)
library(patchwork)
# library(ggthemes)
# library(gt)
# library(glue)
# library(shinyWidgets)
# library(brand.yml)
# library(corrr)
library(tidymodels)
library(tidyverse)
# library(ranger)

if (FALSE) {
  library(brand.yml)
  library(ranger)
}

# colores ----------------------------------------------------------------

violeta <- "#341648" # MetBrewer: Tam
verde <- "#007e2e"
blanco <- "#f2f2f2"
negro <- "#000000"
gris <- "grey80"

formato <- function(X, ...) {
  format(X, big.mark = ".", decimal.mark = ",", ...)
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
icon_autor <- HTML('<span class="material-symbols--person"></span>')
icon_año <- HTML('<span class="mdi--calendar"></span>')

# datos ------------------------------------------------------------------

# recortes de interés
l <- list.files("recortes/", full.names = TRUE)
l <- l[str_detect(l, "2026")]

# fechas de los recortes
fechas <- basename(l) |>
  gsub(".tif", "", x = _) |>
  ymd()

# ráster
r <- lapply(l, rast)
r <- setNames(r, fechas)

# orden correcto de bandas S2-MSI
banda_fct <- c(
  "B01",
  "B02",
  "B03",
  "B04",
  "B05",
  "B06",
  "B07",
  "B08",
  "B8A",
  "B11",
  "B12"
)

# datos de laboratorio y espectrales
d <- read.csv("datos/lab_gis.csv") |>
  as_tibble() |>
  mutate(banda = factor(x = banda, levels = banda_fct))

# fechas de las firmas espectrales
fechas_firma_espectral <- unique(d$fecha)

# nombre y unidades de los parámetros de laboratorio
param_etq <- c(
  "turb" = "Turbidez (NTU)",
  "secchi" = "Profundidad de disco de Secchi (cm)",
  "sol_sus" = "Sólidos suspendidos (ppm)",
  "cond" = "Conductividad (&mu;S/cm)"
)
param_nombre <- names(param_etq)
param_nombre <- setNames(
  param_nombre,
  c("Turbidez", "Profundidad de disco", "Sólidos suspendidos", "Conductividad")
)

# vector para recortar el Paraná
parana <- vect("vectores/sección_paraná.gpkg") |>
  project(r[[1]])

# altura del Río Paraná
d_altura <- vroom::vroom("datos/altura.csv", show_col_types = FALSE) |>
  filter(year(fecha) >= 2000)

# caudal del Río Paraná
d_caudal <- vroom::vroom("datos/caudal.csv", show_col_types = FALSE) |>
  filter(between(year(fecha), 2000, 2003)) |>
  mutate(semana = week(fecha), año = year(fecha)) |>
  mutate(
    uno = first(caudal),
    .by = c(semana, año)
  ) |>
  filter(caudal == uno) |>
  select(-uno) |>
  left_join(d_altura, by = join_by(fecha)) %>%
  mutate(temperatura = rnorm(nrow(.), 25, 3)) %>%
  mutate(viento = rnorm(nrow(.), 5, 2))

# altura promedio y rango de datos
m_altura <- mean(d_altura$altura)
fecha_altura_min <- min(d_altura$fecha)
fecha_altura_max <- ymd(20001231)
# fecha_altura_max <- max(d_altura$fecha)

# bibliografía -----------------------------------------------------------

bib <- bibtex::read.bib("extras/bibliografia.bib") |>
  format(style = "text")

# panel mapa -------------------------------------------------------------

# modelos de profundidad de disco y turbidez
rf_secchi <- get(load("modelos/rf_secchi.Rdata"))
rf_turb <- get(load("modelos/rf_turb.Rdata"))

# genera ráster de profundidad de disco
f_secchi2 <- function(FECHA) {
  y <- lista_agua[[as.character(FECHA)]] |>
    crop(parana, mask = TRUE)
  p <- terra::predict(y, workflowsets::extract_workflow(rf_secchi))
  p <- setNames(p, "secchi")
  p2 <- p * lista_mascara[[as.character(FECHA)]]
  q <- global(p2, quantile, probs = c(0.00, 0.99), na.rm = TRUE)
  p3 <- clamp(p2, q$X0., q$X99., values = FALSE)
  return(p3)
}

# genera ráster de turbidez
f_turb2 <- function(FECHA) {
  y <- lista_agua[[as.character(FECHA)]] |>
    crop(parana, mask = TRUE)
  p <- terra::predict(y, workflowsets::extract_workflow(rf_turb))
  p <- setNames(p, "turb")
  p2 <- p * lista_mascara[[as.character(FECHA)]]
  q <- global(p2, quantile, probs = c(0.02, 0.98), na.rm = TRUE)
  p3 <- clamp(p2, q$X2., q$X98., values = FALSE)
  return(p3)
}

# nombres de paletas de colores para los mapas
paletas <- RColorBrewer::brewer.pal.info |>
  filter(category == "seq") |>
  rownames()

# agua
lista_cropped_scaled_r <- map(r, ~ .x * 0.0001)

# MNDWI
lista_mndwi <- map(
  lista_cropped_scaled_r,
  \(X) {
    y <- (X$B03 - X$B11) / X$B03 + X$B11
    y[is.infinite(y)] <- NA
    y <- setNames(y, "mndwi")
  }
)

# máscara
lista_mascara <- map(lista_mndwi, \(X) {
  y <- thresh(X)
  y[y == 0] <- NA
  p <- patches(y)
  rz <- zonal(cellSize(p, unit = "m"), p, sum, as.raster = TRUE)
  m <- global(rz, "max", na.rm = TRUE)$max
  s <- ifel(rz < m, NA, y)
  return(s)
})

# ráster de agua
lista_agua <- map2(lista_cropped_scaled_r, lista_mascara, ~ .x * .y)

# mapa leaflet de ráster en color real RGB
leaflet_rgb <- function(FECHA) {
  leaflet() |>
    addTiles(group = "OSM (base)") |>
    addProviderTiles(
      providers$CartoDB.Positron,
      group = "Positron (mínimo)",
      options = providerTileOptions(noWrap = TRUE)
    ) |>
    addProviderTiles(
      providers$Esri.WorldImagery,
      group = "World Imagery (satelital)",
      options = providerTileOptions(noWrap = TRUE)
    ) |>
    addRasterRGB(
      r[[as.character(FECHA)]],
      r = 4,
      g = 3,
      b = 2,
      quantiles = c(.02, .98),
      na.color = NA,
      group = "RGB"
    ) |>
    addLayersControl(
      baseGroups = c(
        "OSM (base)",
        "Positron (mínimo)",
        "World Imagery (satelital)"
      ),
      overlayGroups = c("RGB"),
      options = layersControlOptions(collapsed = TRUE)
    ) |>
    addResetMapButton() |>
    addFullscreenControl(position = "bottomright")
}

# mapa leaflet de ráster de turbidez
leaflet_tipo <- function(FECHA, TIPO, PALETA) {
  if (TIPO == "turb") {
    p <- f_turb2(FECHA)
    pal_nombre <- PALETA
    grupo <- "Turbidez (NTU)"
    titulo <- "Turbidez<br>(NTU)"
    invertido <- FALSE
  }

  if (TIPO == "secchi") {
    p <- f_secchi2(FECHA)
    pal_nombre <- PALETA
    grupo <- "Profundidad de disco (cm)"
    titulo <- "Profundidad de<br>disco (cm)"
    invertido <- TRUE
  }

  v <- na.omit(values(p))

  pal <- colorNumeric(
    palette = pal_nombre,
    # domain = v,
    domain = c(min(v) - 1, max(v) + 1),
    na.color = NA,
    reverse = invertido
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

f_descarga_raster <- function(FECHA, TIPO, FILE) {
  y <- r[[as.character(FECHA)]]
  if (TIPO == "Turbidez") {
    writeRaster(f_turb(FECHA), FILE)
  } else if (TIPO == "Profundidad de disco") {
    writeRaster(f_secchi(FECHA), FILE)
  } else {
    writeRaster(y, FILE)
  }
}

# panel figura -----------------------------------------------------------

f_firma_espectral <- function(FECHA, VAR) {
  e1 <- filter(d, fecha == FECHA) |>
    distinct(punto, longitud)
  e2 <- filter(d, fecha == FECHA) |>
    select(all_of(c("punto", "banda", VAR))) |>
    rename("y" = 3) |>
    distinct() |>
    mutate(label = paste0(round(y, 3), "\nPunto: ", punto))

  ylim_min <- range(subset(d, fecha == FECHA)$reflect_acolite) |>
    min()
  ylim_max <- range(subset(d, fecha == FECHA)$reflect_sen2cor) |>
    max()

  g1 <- ggplot(e1, aes(longitud, 1, fill = as.factor(punto))) +
    geom_point_interactive(
      aes(data_id = punto, tooltip = glue::glue("Punto: {punto}")),
      hover_nearest = TRUE,
      size = 3,
      shape = 21,
      stroke = 1,
      alpha = 1
    ) +
    annotate(
      geom = "text",
      x = c(-Inf, Inf),
      y = 1,
      label = c("Orilla\nChaco", "Orilla\nCorrientes"),
      hjust = .5,
      vjust = -.5,
      size = 3,
      family = "Fira Code"
    ) +
    scale_x_continuous(expand = expansion(mult = .1, add = 0)) +
    scale_fill_manual(
      values = colorRampPalette(c("brown", "turquoise"))(length(unique(
        e1$punto
      ))),
      guide = guide_none()
    ) +
    coord_cartesian(clip = "off", expand = FALSE) +
    theme_void(base_family = "Fira Code")

  g2 <- ggplot(e2, aes(banda, y, group = punto, color = as.factor(punto))) +
    geom_line_interactive(aes(data_id = punto), linewidth = 1, alpha = .5) +
    geom_point_interactive(
      aes(tooltip = label, data_id = punto),
      hover_nearest = TRUE,
      size = 1.7,
      shape = 21,
      fill = "white",
      stroke = 1,
      alpha = .5
    ) +
    scale_color_manual(
      values = colorRampPalette(c("brown", "turquoise"))(length(unique(
        e2$punto
      ))),
      guide = guide_none()
    ) +
    coord_cartesian(
      ylim = c(ylim_min * .99, ylim_max * 1.05),
      xlim = c(.75, 11.25),
      expand = FALSE,
      clip = "off"
    ) +
    labs(x = NULL, y = "R<sub>rs</sub>") +
    ggthemes::theme_few(base_family = "Fira Code") +
    theme_sub_axis(text = element_text(color = negro)) +
    theme_sub_axis_x(text = element_text(face = "bold")) +
    theme_sub_axis_y(
      title = ggtext::element_markdown(angle = 0, vjust = .5)
    ) +
    theme_sub_panel(
      grid.major = element_line(color = "grey80", linewidth = .2),
      background = element_blank()
    ) +
    theme_sub_plot(background = element_blank()) +
    theme_sub_panel(
      grid.minor.y = element_line(color = "grey80", linewidth = .2)
    )

  g3 <- g1 /
    g2 +
    plot_layout(heights = c(1, 5)) &
    theme_sub_panel(
      grid.major = element_line(color = gris, linewidth = .2),
      background = element_blank()
    ) +
      theme_sub_plot(
        background = element_rect(
          fill = "transparent",
          color = "transparent"
        ),
        margin = margin(r = 15, l = 5)
      )

  girafe(
    ggobj = g3,
    options = list(
      opts_hover_inv(css = "opacity:.2"),
      opts_hover(
        css = girafe_css(
          css = "",
          point = "opacity:1;",
          line = "opacity:1;"
        )
      ),
      opts_tooltip(
        opacity = 1,
        css = glue::glue(
          "color:{negro};padding:5px;font-family: Fira Code;",
          "border-style:solid;border-color:{violeta};background:{blanco}"
        ),
        use_cursor_pos = TRUE,
        offx = 15,
        offy = 15
      ),
      opts_toolbar(saveaspng = FALSE, hidden = c("selection", "zoom", "misc"))
    ),
    bg = "transparent"
  )
}

# panel caudal ------------------------------------------------------------

f_caudal <- function() {
  d_caudal |>
    filter(year(fecha) < 2005) |>
    ggplot(aes(fecha, caudal)) +
    geom_line(color = violeta, linewidth = .3) +
    geom_point(
      size = 3,
      shape = 21,
      fill = blanco,
      color = violeta,
      stroke = 1
    ) +
    scale_x_date(date_minor_breaks = "3 month") +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    labs(y = "Caudal (m<sup>3</sup> s<sup>-1</sup>)", x = NULL) +
    ggthemes::theme_few(base_size = 22, base_family = "Fira Code") +
    theme_sub_panel(
      background = element_blank(),
      grid.major = element_line(color = gris, linewidth = .3),
      grid.minor = element_line(color = gris, linewidth = .1)
    ) +
    theme_sub_axis(text = element_text(color = negro)) +
    theme_sub_plot(background = element_rect(fill = blanco, color = NA)) +
    theme_sub_axis_left(title = ggtext::element_markdown())
}

# panel integrantes ------------------------------------------------------

f_integrante <- function(TITULO, INTEGRANTE, ORCID, EMAIL = NULL) {
  span(
    strong(TITULO),
    em(INTEGRANTE),
    a(
      HTML(
        '<span class="simple-icons--orcid"></span>'
      ),
      href = paste0("https://orcid.org/", ORCID),
      target = "_blank"
    ),
    if (!is.null(EMAIL)) {
      a(
        HTML('<span class="ic--round-email"></span>'),
        href = paste0("mailto:", EMAIL),
        target = "_blank"
      )
    } else {
      NULL
    },
    style = "font-size: 2em"
  )
}

logo_tbl <- tibble(
  org = c("gistaq", "ig", "iidthh", "conae", "utn", "unc", "unne", "conicet"),
  src = c(
    "logo_gistaq.png",
    "logo_ig.png",
    "logo_iidthh.png",
    "logo_conae.png",
    "logo_utn.png",
    "logo_unc.svg",
    "logo_unne.png",
    "logo_conicet.png"
  ),
  href = c(
    "https://www.instagram.com/gistaq.utn",
    "https://ig.conae.unc.edu.ar/",
    "https://iidthh.conicet.gov.ar/",
    "https://www.argentina.gob.ar/ciencia/conae",
    "https://www.frre.utn.edu.ar/",
    "https://www.unc.edu.ar/",
    "https://www.unne.edu.ar/",
    "https://www.conicet.gov.ar/"
  )
)

f_logo <- function(ORG) {
  span(
    a(
      img(
        src = subset(logo_tbl, org == ORG)$src,
        height = "70"
      ),
      href = subset(logo_tbl, org == ORG)$href,
      target = "_blank",
      .noWS = "before-end"
    ),
    class = "eqi-container"
  )
}

# panel tabla ------------------------------------------------------------

f_tabla <- function(PARAM, MAYOR) {
  tab <- filter(d, param == PARAM) |>
    tidyr::pivot_wider(
      names_from = banda,
      values_from = reflect_acolite,
      id_cols = c(punto, fecha, longitud, latitud, param, valor)
    ) |>
    select(valor, starts_with("B")) |>
    distinct() |>
    corrr::correlate(use = "pairwise.complete.obs", quiet = TRUE) |>
    select(1, 2) |>
    tidyr::drop_na() |>
    rename("Banda" = 1, "r" = 2) |>
    gt::gt() |>
    gt::fmt_number(r, decimals = 3, sep_mark = ".", dec_mark = ",") |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = "Banda")
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = list(gt::cells_body(), gt::cells_column_labels())
    ) |>
    gt::tab_header(title = gt::md(param_etq[PARAM])) |>
    gt::tab_options(
      table.width = 220,
      table.background.color = "transparent"
    )

  if (MAYOR) {
    tab |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = violeta),
          gt::cell_text(color = "white", weight = "bold")
        ),
        locations = gt::cells_body(rows = abs(r) == max(abs(r)))
      )
  } else {
    tab
  }
}

# pie --------------------------------------------------------------------

pie <- span(
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
)

# figura serie temporal --------------------------------------------------

estilo_serie_temporal <- function(g) {
  g +
    scale_x_date(
      labels = \(X) {
        scales::label_date_short(
          format = c("%Y", "%b", "%d", "%H:%M"),
          leading = ""
        )(X) |>
          toupper()
      },
      expand = expansion(add = 0, mult = .01),
      minor_breaks = scales::breaks_width("1 month")
    ) +
    scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = "."),
      expand = expansion(add = 0, mult = c(0, .01)),
      breaks = scales::breaks_width(1),
      minor_breaks = scales::breaks_width(.25)
    ) +
    coord_cartesian(ylim = c(0, NA)) +
    labs(y = "Altura (m)", x = NULL) +
    ggthemes::theme_few(base_family = "Fira Code") +
    theme(aspect.ratio = .7) +
    theme_sub_axis(
      text = element_text(color = negro, face = "bold"),
      title = element_text(face = "bold")
    ) +
    theme_sub_panel(
      grid.major = element_line(color = gris, linewidth = .3),
      grid.minor = element_line(color = gris, linewidth = .1),
      background = element_blank()
    ) +
    theme_sub_plot(background = element_blank(), margin = margin(r = 10, l = 5))
}

f_serie_temporal_altura <- function(
  FECHA_MIN,
  FECHA_MAX,
  ma = FALSE,
  n_ma = 20
) {
  d_serie <- filter(d_altura, between(fecha, FECHA_MIN, FECHA_MAX)) |>
    mutate(
      label = paste0(
        format(round(altura, 2), big.mark = ".", decimal.mark = ","),
        " m\n",
        fecha
      )
    )
  if (ma) {
    g_ma <- d_serie |>
      ggplot(aes(fecha, altura)) +
      geom_hline(
        yintercept = m_altura,
        color = verde,
        linetype = 2,
        linewidth = 1
      ) +
      tidyquant::geom_ma(n = n_ma, linetype = 1, linewidth = 1, color = violeta)
    g <- estilo_serie_temporal(g_ma)
  } else {
    g_serie <- d_serie |>
      ggplot(aes(fecha, altura)) +
      geom_hline(
        yintercept = m_altura,
        color = verde,
        linetype = 2,
        linewidth = 1
      ) +
      geom_line(color = violeta, linewidth = .5) +
      geom_point_interactive(
        aes(tooltip = label, data_id = fecha),
        shape = 21,
        size = 1,
        fill = verde,
        color = violeta,
        stroke = .5
      )
    g <- estilo_serie_temporal(g_serie)
  }

  girafe(
    ggobj = g,
    options = list(
      opts_hover_inv(css = "opacity:.7"),
      opts_hover(css = girafe_css(css = "fill:white;size:10pt")),
      opts_tooltip(
        opacity = 1,
        css = glue::glue(
          "color:{negro};padding:5px;font-family: Fira Code;",
          "border-style:solid;border-color:{violeta};background:{blanco}"
        ),
        use_cursor_pos = TRUE,
        offx = 5,
        offy = 5
      ),
      opts_toolbar(saveaspng = FALSE, hidden = c("selection", "zoom", "misc"))
    ),
    bg = "transparent"
  )
}

f_descarga_serie_temporal <- function(FECHA_MIN, FECHA_MAX, FILE) {
  filter(d_altura, between(fecha, FECHA_MIN, FECHA_MAX)) |>
    vroom::vroom_write(FILE)
}

# quarto -----------------------------------------------------------------

f_quarto <- function(FILE, PARAMS) {
  quarto::quarto_render(
    input = "p.qmd",
    execute_params = PARAMS
  )
  return(FILE)
}

quarto_fecha_firma <- unique(read.csv("datos/lab_gis.csv")$fecha)
quarto_fecha_mapa <- gsub(".tif", "", list.files("recortes/")) |>
  ymd()
quarto_fecha_mapa <- quarto_fecha_mapa[str_which(quarto_fecha_mapa, "2026")]

# List of branded MSK colors
msk_colors <- c(
  `MSK blue` = "#007CBA",
  `MSK orange` = "#DF4602",
  `Dark blue` = "#006098",
  `Medium blue` = "#40B4E5",
  `Light blue` = "#8FC7E8",
  `Dark orange` = "#C24D00",
  `Medium orange` = "#F29934",
  `Light orange` = "#F6C65B",
  `Dark violet` = "#83276B",
  `Light violet` = "#B687B8",
  `Dark magenta` = "#E92076",
  `Light magenta` = "#EFB3CB",
  `Dark green` = "#4C8B2B",
  `Light green` = "#A6CE39",
  `Dark turquoise` = "#006C68",
  `Light turquoise` = "#009490",
  `Dark yellow` = "#FFC20E",
  `Light yellow` = "#FFE100",
  `Dark grey` = "#9E9E98",
  `Light grey` = "#D5D4C7")


#' Function to extract MSK colors as hex codes
#'
#' @param ... Character names of msk_colors
#'
msk_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (msk_colors)

  msk_colors[cols]
}


# List of color palettes I think work okay for visualizations based on these
msk_palettes <- list(
  `main`  = msk_cols("MSK blue", "MSK orange", "Medium blue",
                     "Medium orange", "Light blue", "Light orange",
                     "Dark blue", "Dark orange"),
  `blues`  = msk_cols("MSK blue", "Medium blue", "Light blue", "Dark blue"),
  `oranges`   = msk_cols("MSK orange", "Medium orange", "Light orange",
                         "Dark orange"),

  `contrast` = msk_cols("MSK blue", "MSK orange", "Dark violet", "Dark magenta",
                        "Dark green", "Dark turquoise", "Dark yellow",
                        "Light violet", "Light magenta", "Light green",
                        "Light turquoise", "Light yellow")
)


#' Return function to interpolate a MSK color palette
#'
#' @param palette Character name of palette in msk_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
msk_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- msk_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}


#' Color scale constructor for MSK colors
#'
#' @param palette Character name of palette in msk_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_msk <- function(palette = "main", discrete = TRUE,
                            reverse = FALSE, ...) {
  pal <- msk_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("msk_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Fill scale constructor for MSK colors
#'
#' @param palette Character name of palette in msk_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_msk <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- msk_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("msk_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}




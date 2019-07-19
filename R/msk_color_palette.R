#' Named list of MSK brand colors
#'
#' This is named vector of the MSK brand color hex codes. Run `msk_colors` to
#' see the names and hex codes for the MSK brand colors.
#'
#' @export

msk_colors <- c(
  `msk_blue` = "#007CBA",
  `msk_orange` = "#DF4602",
  `medium_blue` = "#40B4E5",
  `medium_orange` = "#F29934",
  `light_blue` = "#8FC7E8",
  `light_orange` = "#F6C65B",
  `dark_blue` = "#006098",
  `dark_orange` = "#C24D00",
  `dark_violet` = "#83276B",
  `dark_green` = "#4C8B2B",
  `dark_magenta` = "#E92076",
  `dark_turquoise` = "#006C68",
  `dark_gray` = "#9E9E98",
  `dark_yellow` = "#FFC20E",
  `light_violet` = "#B687B8",
  `light_green` = "#A6CE39",
  `light_magenta` = "#EFB3CB",
  `light_turquoise` = "#009490",
  `light_gray` = "#D5D4C7",
  `light_yellow` = "#FFE100"
)


#' Function to extract colors from \code{msk_colors} as hex codes
#'
#' @param ... Character names of msk_colors
#' @export

msk_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(msk_colors)
  }

  unname(msk_colors[cols])
}


#' Complete list of MSK branded color palettes
#'
#' @description Creates color palettes based on MSK brand colors, including
#' "main" for the main blue and orange colors, "blues" for all the blue colors,
#' "oranges" for all the orange colors, and "contrast" for an ordered palette of
#' contrasting colors.
#'
#' @export
#'
#' @examples
#' msk_palettes[["main"]]
#' msk_palettes[["contrast"]]
msk_palettes <- list(
  `main` = msk_cols(
    "msk_blue", "msk_orange", "medium_blue", "medium_orange", "light_blue",
    "light_orange", "dark_blue", "dark_orange"
  ),
  `blues` = msk_cols("dark_blue", "msk_blue", "medium_blue", "light_blue"),
  `oranges` = msk_cols(
    "dark_orange", "msk_orange", "medium_orange",
    "light_orange"
  ),
  `contrast` = msk_cols(
    "msk_blue", "msk_orange", "dark_violet", "dark_green", "dark_magenta",
    "dark_turquoise", "dark_gray", "dark_yellow", "medium_blue", "medium_orange",
    "light_violet", "light_green", "light_magenta", "light_turquoise",
    "light_gray", "light_yellow"
  )
)


#' Access the colors in a MSK branded color palette
#'
#' @description MSK brand colors can be accessed and used in plotting
#'
#' @param name Name of desired palette, supplied in quotes. Choices are: "all",
#' "main" (default), "blues", "oranges", "contrast"
#' @param n Number of colors desired. If omitted, uses all colors,
#' or the needed number of colors if less than the total.
#' @param type Either "continuous" or "discrete". Use continuous if you want
#' to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#'   @importFrom grDevices colorRampPalette
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#'
#' library(ggplot2)
#'
#' # Print a plot showing the colors in a palette, in order
#' msk_palette("main")
#' msk_palette("blues")
#' msk_palette("contrast")
#'
#' # use a single brand color from a palette
#' # here using the fourth color from the "main" palette
#' ggplot(mtcars, aes(hp, mpg)) +
#' geom_point(size = 4, color = msk_palette("main")[4])
#'
#' # use a discrete color scale - uses fixed colors from the requested palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point(size = 4) +
#' scale_color_manual(values = msk_palette("main"))
#'
#' # use a continuous color scale - interpolates between colors
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#' geom_point(size = 4, alpha = .6) +
#' scale_color_gradientn(colors = msk_palette("blues", type = "continuous"))
#'
#' # use a fill color
#' ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
#' geom_histogram(bins = 20, position = "dodge") +
#' scale_fill_manual(values = msk_palette("main"))
#'

msk_palette <- function(name = "main", n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  # since the palettes in msk_palettes are named vectors, ggplot will try to match the names to levels of the variables in the data, which is not what we want. Rather we just want to use them in order, so we need to use unname() here

  pal <- unname(msk_palettes[[name]])
  if (is.null(pal)) {
    stop("Palette not found.")
  }

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  out <- switch(type,
    continuous = grDevices::colorRampPalette(pal)(n),
    discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}


#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
#' @export
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n),
    col = x,
    ylab = "", xaxt = "n", yaxt = "n", bty = "n"
  )

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}


#' Return function to interpolate an MSK color palette
#'
#' @param palette Character name of palette in msk_palettes.
#' Options include "all", "main", "blues", "oranges", and "contrast".
#' @param reverse Boolean indicating whether the palette should be reversed.
#' Default to FALSE.
#' @param ... Additional arguments to pass to colorRampPalette()

msk_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- msk_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


#' Color scale creator to add MSK brand colors in ggplot
#'
#' @description This color scale generator will interpolate between colors,
#' even when discrete scales are provided.
#' To use exact discrete colors, see examples in \code{msk_palette}
#'
#' @param palette Character name of palette in msk_palettes, supplied in quotes.
#' Options include "all", "main", "blues", "oranges", and "contrast".
#' Default is "main".
#' @param discrete Boolean indicating whether color aesthetic is discrete.
#' Default is TRUE.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' Default is FALSE.
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # use a discrete color scale
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point(size = 4) +
#' scale_color_msk("main")
#'
#' # use a continuous color scale
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#' geom_point(size = 4, alpha = .6) +
#' scale_color_msk(palette = "blues", discrete = FALSE)

scale_color_msk <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- msk_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("msk_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Fill scale creator to add MSK brand colors in ggplot
#'
#' @description This fill scale generator will interpolate between the colors
#' in the palette provided
#'
#' @param palette Character name of palette in msk_palettes, supplied in quotes.
#' Options include "all", "main", "blues", "oranges", and "contrast".
#' Default is "main".
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' Default is TRUE.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' Default is FALSE.
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # use a fill color
#' # alternative use that involves interpolation
#' ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
#' geom_histogram(bins = 20, position = "dodge") +
#' scale_fill_msk()

scale_fill_msk <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- msk_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("msk_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

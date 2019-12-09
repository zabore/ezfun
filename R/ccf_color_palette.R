#' Named list of CCF brand colors
#'
#' This is named vector of the CCF brand color hex codes. Run `ccf_colors` to
#' see the names and hex codes for the CCF brand colors.
#'
#' @export

ccf_colors <- c(
  `ccf_blue` = "#0078bf",
  `ccf_green` = "#00843d",
  `ccf_black` = "#4b4b45",
  `tango` = "#f08122",
  `honey_flower` = "#5c2161",
  `maroon_flush` = "#a61f56",
  `lightning_yellow` = "#f7c612",
  `blue_haze` = "#bdbbd4",
  `candy_corn` = "#fdf061",
  `wattle` = "#d5e048",
  `seagull` = "#79d0e6",
  `mauvelous` = "#ee93ae",
  `flamingo` = "#ef413d",
  `beryl_green` = "#dedab7",
  `cinderella` = "#fcdcda",
  `lola` = "#e2d4d9",
  `surf_crest` = "#cce4cb",
  `light_apricot` = "#fdd1b0",
  `botticelli` = "#d3dee8",
  `silver_sand` = "#c6c8c8",
  `jumbo` = "#7b7d83",
  `pickled_bluewood` = "#384d5f",
  `millbrook` = "#51362d",
  `mondo` = "#473f29"
)


#' Function to extract colors from \code{ccf_colors} as hex codes
#'
#' @param ... Character names of ccf_colors
#'
#' @export

ccf_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(ccf_colors)
  }

  unname(ccf_colors[cols])
}


#' CCF branded color palettes
#'
#' @description Creates color palettes based on CCF brand colors, including
#' "main", which contains all primary and secondary CCF colors,
#' "bright", which contains all supporting bright CCF colors,
#' "neutral", which contains all supporting neutral CCF colors,
#' "all", which contains all colors from main, bright, and neutral for when
#' more colors are needed,
#' "blues", which contains a high and low blue color to create continuous blue
#' scales,
#' "greens", which contains a high and low green color to create continuous
#' green scales
#'
#' @export
#'
#' @examples
#' ccf_palettes[["main"]]
#' ccf_palettes[["contrast"]]
ccf_palettes <- list(
  `main` = ccf_cols(
    "ccf_blue", "ccf_green", "ccf_black", "tango", "honey_flower",
    "maroon_flush", "lightning_yellow"
  ),
  `bright` = ccf_cols(
    "blue_haze", "candy_corn", "wattle", "seagull",  "mauvelous", "flamingo"
    ),
  `neutral` = ccf_cols(
    "beryl_green", "cinderella", "lola", "surf_crest", "light_apricot",
    "botticelli", "silver_sand", "jumbo", "pickled_bluewood", "millbrook",
    "mondo"
  ),
  `all` = ccf_cols(
    "ccf_blue", "ccf_green", "ccf_black", "tango", "honey_flower",
    "maroon_flush", "lightning_yellow",
    "blue_haze", "candy_corn", "wattle", "seagull",  "mauvelous", "flamingo",
    "beryl_green", "cinderella", "lola", "surf_crest", "light_apricot",
    "botticelli", "silver_sand", "jumbo", "pickled_bluewood", "millbrook",
    "mondo"
  ),
  `blues` = ccf_cols("ccf_blue", "botticelli"),
  `greens` = ccf_cols("ccf_green", "surf_crest"),
  `contrast` = ccf_cols("ccf_blue", "lightning_yellow", "maroon_flush",
                        "ccf_green", "tango", "honey_flower",
                        "wattle", "flamingo", "seagull", "jumbo")
)


#' Access the colors in a CCF branded color palette
#'
#' @description CCF brand colors can be accessed and used in plotting
#'
#' @param name Name of desired palette, supplied in quotes. Choices are:
#' "main" (default), "bright", "neutral", "all", "blues", and "greens"
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
#' # Print a plot to visualize the colors in a palette, in order
#' ccf_palette("main")
#' ccf_palette("bright")
#' ccf_palette("neutral")
#'
#' # use a single brand color from a palette
#' # here using the fourth color from the "main" palette
#' ggplot(mtcars, aes(hp, mpg)) +
#' geom_point(size = 4, color = ccf_palette("main")[4])
#'
#' # as an alternative to the above, you can select a single color by name
#' ggplot(mtcars, aes(hp, mpg)) +
#' geom_point(size = 4, color = ccf_cols("tango"))
#'
#' # use a discrete color scale - uses fixed colors from the requested palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point(size = 4) +
#' scale_color_manual(values = ccf_palette("main"))
#'
#' # use a continuous color scale - interpolates between colors
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#' geom_point(size = 4, alpha = .6) +
#' scale_color_gradientn(colors = ccf_palette("blues", type = "continuous"))
#'
#' # use a fill color
#' ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
#' geom_histogram(bins = 20, position = "dodge") +
#' scale_fill_manual(values = ccf_palette("main"))

ccf_palette <- function(name = "main", n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  # since the palettes in ccf_palettes are named vectors, ggplot will try to match the names to levels of the variables in the data, which is not what we want. Rather we just want to use them in order, so we need to use unname() here

  pal <- unname(ccf_palettes[[name]])
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


#' Return function to interpolate a CCF color palette
#'
#' @param palette Character name of palette in ccf_palettes.
#' Options include "main" (default), "bright", "neutral", "all", "blues" and
#' "greens".
#' @param reverse Boolean indicating whether the palette should be reversed.
#' Default to FALSE.
#' @param ... Additional arguments to pass to colorRampPalette()

ccf_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ccf_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

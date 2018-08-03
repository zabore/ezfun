#' Complete list of MSK branded color palettes
#'
#' @export

msk_palettes <- list(
  all = c("#007CBA", "#DF4602", "#40B4E5", "#F29934", "#8FC7E8", "#F6C65B",
          "#006098", "#C24D00", "#83276B", "#4C8B2B", "#E92076",
          "#006C68", "#9E9E98", "#FFC20E", "#B687B8", "#A6CE39", "#EFB3CB",
          "#009490", "#D5D4C7", "#FFE100"),
  main  = c("#007CBA", "#DF4602", "#40B4E5", "#F29934", "#8FC7E8", "#F6C65B",
            "#006098", "#C24D00"),
  blues  = c("#006098", "#007CBA", "#40B4E5", "#40B4E5"),
  oranges   = c("#C24D00", "#DF4602", "#F29934", "#F6C65B"),
  contrast = c("#007CBA", "#DF4602", "#83276B", "#4C8B2B", "#E92076",
               "#006C68", "#9E9E98", "#FFC20E", "#B687B8", "#A6CE39", "#EFB3CB",
               "#009490", "#D5D4C7", "#FFE100")
)


#' MSK branded colors palette generator
#'
#' @param n Number of colors desired. If omitted, uses all colours.
#' @param name Name of desired palette. Choices are: \code{all},
#' \code{main} (default),  \code{blues}, \code{oranges}, \code{contrast}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#' to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' msk_palette("main")
#' msk_palette("blues")
#' msk_palette("contrast")
#'
#' library(ggplot2)
#'
#' # use a single brand color from a palette
#' ggplot(mtcars, aes(hp, mpg)) +
#' geom_point(size = 4, color = msk_palette("main")[1])
#'
#' # use a discrete color scale
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point(size = 4) +
#' scale_color_manual(values = msk_palette("contrast"))
#'
#' # use a continuous color scale
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#' geom_point(size = 4, alpha = .6) +
#' scale_color_gradientn(colors = msk_palette("blues", type = "continuous"))

msk_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- msk_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")

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


#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}



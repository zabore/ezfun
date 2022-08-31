#' Set CCF color palette
#'
#' This function sets the CCF color palette as the default colors.  It does
#' this by adding custom functions,`scale_colour_discrete()` and
#' `scale_fill_discrete()`, to the global environment. A typical workflow would
#' include this function at the top of a script, and subsequent calls to
#' `ggplot()` will utilize the CCF color palette. This function was originally
#' written by Michael Curry at MSK, and only modified here to utilize CCF
#' colors. I also made a few minor changes
#'
#' @param palette specify the color palette of interest. Default is "all".
#' Other options include "main", "bright", "neutral", "blues", and "greens".
#' @param reverse logical to reverse the color palette. Defaults to FALSE.
#' @param colors an optional argument of hex colors in an unnamed vector.
#' When provided the `palette = ` argument is overridden and the colors
#' supplied will be used. Default is `NULL`.
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' set_ccf_palette()
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point()
#'
#' # using the color argument to select a few from the CCF color palette
#' set_ccf_palette(colors = ccf_palettes[["main"]][c(2,5,6)])
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point()

set_ccf_palette <- function(palette = "all", reverse = FALSE, colors = NULL) {

  # choosing the CCF palette
  if(!is.null(colors)) pal <- colors else
    pal <- ccf_palettes[[palette]]

  # reversing palette if requested
  if (reverse == TRUE) pal <- rev(pal)

  # defining color palette functions in named list
  palette_fns <- list(
    scale_colour_discrete =
      function(..., values = pal)
        ggplot2::scale_colour_manual(..., values = values),
    scale_fill_discrete =
      function(..., values = pal)
        ggplot2::scale_fill_manual(..., values = values)
  )

  # exporting functions to the global environment
  # list2env will unlist and add list elements to the specifed environment
  list2env(palette_fns, envir = .GlobalEnv)
}

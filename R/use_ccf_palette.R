#' Set CCF color palette
#'
#' This function sets the CCF color palette as the default colors.  It does
#' this by adding custom functions,`scale_colour_discrete()` and
#' `scale_fill_discrete()`, to the global environment. A typical workflow would
#' include this function at the top of a script, and subsequent calls to
#' `ggplot()` will utilize the CCF color palette. This function was originally
#' written by Michael Curry at MSK, and only modified here to utilize CCF
#' colors.
#'
#' @param colors an optional argument of hex colors in an unnamed vector.
#' When provided the `palette = ` argument is overridden and the colors
#' supplied will be used. Default is `NULL`.
#' @export
#' @examples
#' \donttest{
#' ezfun::set_ccf_palette()
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point()
#'
#' # using the color argument to select a few from the CCF color palette
#' ezfun::set_ccf_palette(colors = ezfun::ccf_palettes[["main"]][c(1,2,5,6)])
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point()
#' }

set_ccf_palette <- function(palette = c("main", "bright", "neutral", "all",
                                        "blues", "greens"),
                            reverse = FALSE, colors = NULL) {
  # choosing the CCF palette
  palette <-
    ifelse(
      is.null(colors),
      ccf_palettes[[match.arg(palette)]],
      colors
      )

  # reversing palette if requested
  if (reverse == TRUE) palette <- rev(palette)

  # defining color palette functions in named list
  palette_fns <- list(
    scale_colour_discrete =
      function(..., values = palette)
        ggplot2::scale_colour_manual(..., values = values),
    scale_fill_discrete =
      function(..., values = palette)
        ggplot2::scale_fill_manual(..., values = values)
  )

  # exporting functions to the global environment
  # list2env will unlist and add list elements to the specifed environment
  list2env(palette_fns, envir = .GlobalEnv)
}

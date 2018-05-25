ezpalette <- c("#61e294", "#3f7cff", "#7F0799", "#FF6F59", "#69747C", "#650d1b", "#A37B73", "#2e5339", "#40a189", "#FCD757")

#' A custom color scale made by Emily Zabor, with help from Coolors app
#'
#' Basically copying code from hrbrmstr/hrbrthemes colors.r
#'
#' @export
#' @examples
#' library(scales)
#' scales::show_col(ez_pal()(9))
ez_pal <- function() { manual_pal(ezpalette) }

#' Discrete color & fill scales based on the ez palette
#'
#' See [ez_pal]().
#'
#' @md
#' @inheritParams ggplot2::scale_colour_hue
#' @rdname scale_ez
#' @export
scale_colour_ez <- function(...) { discrete_scale("colour", "ez", ez_pal(), ...) }

#' @export
#' @rdname scale_ez
scale_color_ez <- scale_colour_ez

#' @export
#' @rdname scale_ez
scale_fill_ez <- function(...) { discrete_scale("fill", "ez", ez_pal(), ...) }

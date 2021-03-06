ezpalette <- c("#61e294", "#3f7cff", "#7F0799", "#FF6F59", "#69747C", "#650d1b", "#A37B73", "#2e5339", "#40a189", "#FCD757")

#' A custom color scale made by Emily Zabor, with help from Coolors app
#'
#' Basically copying code from hrbrmstr/hrbrthemes colors.r
#'
#' @export
#' @examples
#' scales::show_col(ez_pal()(9))
ez_pal <- function() {
  scales::manual_pal(ezpalette)
}

#' Discrete color & fill scales based on the ez palette
#'
#' See \link{ez_pal}
#'
#' @md
#' @inheritParams ggplot2::scale_colour_hue
#' @rdname scale_ez
#' @export
scale_colour_ez <- function(...) {
  ggplot2::discrete_scale("colour", "ez", ez_pal(), ...)
}

#' @export
#' @rdname scale_ez
scale_color_ez <- scale_colour_ez

#' @export
#' @rdname scale_ez
scale_fill_ez <- function(...) {
  ggplot2::discrete_scale("fill", "ez", ez_pal(), ...)
}

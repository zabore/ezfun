#' Basic theme preferences for ggplot
#'
#' Basic theme preferences for ggplot.
#' Functionally a crib from (https://github.com/hrbrmstr/hrbrthemes/)
#'
#' @import ggplot2
#'
#' @param base_size base font size
#' @param plot_title_face,plot_title_size,plot_title_margin plot tilte face,
#' size and margin
#' @param subtitle_face,subtitle_size plot subtitle face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_face,strip_text_size facet label face and size
#' @param caption_face,caption_size,caption_margin plot caption face, size
#' and margin
#' @param axis_title_face,axis_title_size axis title face and size
#' @param x_axis_hjust x-axis title justification (0=left, 0.5=center, 1=right)
#' @param y_axis_hjust y-axis title justification (0=left, 0.5=center, 1=right)
#' @param axis_text_size font size of axis text
#' @param plot_margin plot margin (specify with [ggplot2::margin])
#' @param grid_col grid color
#' @param axis_col axis color
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#'
#' @export

theme_ezbasic <- function(
  # base_family = "Arial Narrow",
  base_size = 11.5,
  # plot_title_family = base_family,
  plot_title_size = 16,
  plot_title_face = "plain",
  plot_title_margin = 10,
  # subtitle_family = base_family,
  subtitle_size = 12,
  subtitle_face = "plain",
  subtitle_margin = 15,
  # strip_text_family = base_family,
  strip_text_size = 12,
  strip_text_face = "plain",
  # caption_family = base_family,
  caption_size = 10,
  caption_face = "plain",
  caption_margin = 10,
  caption = NULL,
  axis_text_size = base_size,
  # axis_title_family = base_family,
  axis_title_size = 9,
  axis_title_face = "plain",
  # axis_title_just = "rt",
  x_axis_hjust = 0,
  y_axis_hjust = 1,
  plot_margin = margin(30, 30, 30, 30),
  grid_col = "#cccccc",
  # grid = TRUE,
  axis_col = "#cccccc",
  axis = FALSE,
  ticks = FALSE) {

  ret <- ggplot2::theme_minimal()

  ret <- ret + theme(legend.background = element_blank(),
                     legend.key = element_blank())

  ret <- ret + theme(legend.position = "bottom",
                     legend.title = element_blank(),
                     panel.grid.major=element_line(color = grid_col,
                                                   size = 0.2),
                     panel.grid.minor=element_line(color = grid_col,
                                                   size = 0.15),
                     axis.title.x=element_text(hjust = x_axis_hjust,
                                               size = axis_title_size),
                     axis.title.y = element_text(hjust = y_axis_hjust,
                                                 size = axis_title_size))

  # list(ret,
  #      scale_color_ez(),
  #      scale_fill_ez())

}

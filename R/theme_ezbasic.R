#' Basic theme preferences for ggplot
#'
#' Basic theme preferences for ggplot. Functionally a crib of
#' https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r with some
#' default changes
#'

#' @param base_size base font size
#' @param plot_title_face,plot_title_size,plot_title_margin plot title
#' face, size and margin
#' @param subtitle_face,subtitle_size plot subtitle face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_face,strip_text_size facet label font face and size
#' @param caption_face,caption_size,caption_margin plot caption face, size and
#' margin
#' @param axis_title_face,axis_title_size axis title font face and size
#' @param axis_title_just axis title font justification, one of `[blmcrt]`
#' @param plot_margin plot margin (specify with [ggplot2::margin()])
#' @param grid_col,axis_col grid & axis colors; both default to `#cccccc`
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`,
#' `y`)
#' @param axis_text_size font size of axis text
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @param legend_title includes legend title if `TRUE`, defaults to `FALSE`
#' @param legend_bottom places legend at bottom if `TRUE`, places legend to
#' default position if `FALSE`
#' @param legend_just legend justification, one of `right`, `left`, `center`.
#' Defaults to `left`.
#' @export
#'

theme_ezbasic <- function(base_size = 11.5,
                          plot_title_size = 16,
                          plot_title_face="plain",
                          plot_title_margin = 10,
                          subtitle_size = 13,
                          subtitle_face = "plain",
                          subtitle_margin = 15,
                          strip_text_size = 12,
                          strip_text_face = "plain",
                          caption_size = 9,
                          caption_face = "plain",
                          caption_margin = 10,
                          axis_text_size = base_size,
                          axis_title_size = 9,
                          axis_title_face = "plain",
                          axis_title_just = "lt",
                          plot_margin = margin(10, 10, 10, 10),
                          grid_col = "#cccccc",
                          grid = TRUE,
                          axis_col = "#cccccc",
                          axis = FALSE,
                          ticks = FALSE,
                          legend_title = FALSE,
                          legend_bottom = TRUE,
                          legend_just = "left") {

  ret <- ggplot2::theme_minimal(base_size = base_size)

  ret <- ret + theme(legend.background = element_blank())
  ret <- ret + theme(legend.key = element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.major = element_line(color = grid_col,
                                                       size = 0.2))
    ret <- ret + theme(panel.grid.minor = element_line(color = grid_col,
                                                       size = 0.15))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x =
                                                          element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y =
                                                          element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x =
                                                          element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y =
                                                          element_blank())
    }

  } else {
    ret <- ret + theme(panel.grid = element_blank())
  }


  if (legend_title == TRUE) {

    ret <- ret + theme(legend.title = element_text(size = caption_size))

  } else {

    ret <- ret + theme(legend.title = element_blank())

  }


  if (legend_bottom == FALSE) {

    ret <- ret

  } else {

    ret <- ret + theme(legend.position = "bottom")

  }


  if (legend_just == "right") {
    ret <- ret + theme(legend.justification = "right")
  } else if (legend_just == "center") {
    ret <- ret + theme(legend.justification = "center")
  } else {
    ret <- ret + theme(legend.justification = "left")
  }


  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + theme(axis.text.x=element_text(size=axis_text_size, margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(size=axis_text_size, margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               face=axis_title_face))
  ret <- ret + theme(axis.title.y.right=element_text(hjust=yj, size=axis_title_size, angle=90,
                                                     face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face))
  ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)

  ret

}

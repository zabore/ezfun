#' Basic theme preferences for ggplot
#'
#' @import ggplot2
#'
#' @export

theme_ezbasic <- function() {

  ret <- ggplot2::theme_minimal()

  ret <- ret + theme(legend.position = "bottom",
                     legend.title = element_blank(),
                     panel.grid.major=element_line(color="#cccccc", size=0.2),
                     panel.grid.minor=element_line(color="#cccccc", size=0.15),
                     axis.title.x=element_text(hjust=1, size = 9),
                     axis.title.y = element_text(hjust = 1, size = 9))

  list(ret,
       scale_color_brewer(palette = "Set2"),
       scale_fill_brewer(palette = "Set2"))

}

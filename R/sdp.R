#' Get p-value from survdiff()
#'
#' \code{sdp} returns the p-value from the \code{\link[survival]{survdiff}} function
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param sd is a \code{\link[survival]{survdiff}} object
#'
#' @return Returns a p-value rounded to 3 digits or "<.001" if the p-value is
#' <.001
#'
#' @export
#'

sdp <- function(sd) {
  pval <- 1 - stats::pchisq(sd$chisq, length(sd$n) - 1)
  if (pval > 0.001) {
    return(round(pval, 3))
  } else
  if (pval < 0.001) return("<.001")
}

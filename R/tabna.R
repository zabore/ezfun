#' Cross-tabulation with \code{useNA = "ifany"}
#'
#' \code{tabna} is an implementation of \code{table} with argument \code{useNA = "ifany"}
#'
#' See the help file for \code{table} for detailed information about possible arguments to the function
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param ... the function takes any standard arguments to \code{table}
#'
#' @return \code{tabna} returns a contingency table with NAs included, if any
#'
#' @export
#'

tabna <- function(...) {
  table(..., useNA = "ifany")
}

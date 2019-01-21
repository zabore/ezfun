#' Format resuls from multivariable competing risks regression model
#'
#' \code{mvcrrres} takes a multivariable competing risks regression object and puts the
#' resulting HR (95\% CI) and p-values into a table
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param mod is a multivariable Cox regression object
#'
#' @return Returns a dataframe
#'
#' @export
#'

mvcrrres <- function(mod) {
  hr.ci <- paste0(
    round(summary(mod)$conf.int[, 1], 2), " (",
    round(summary(mod)$conf.int[, 3], 2), ", ",
    round(summary(mod)$conf.int[, 4], 2), ")"
  )
  p <- round(summary(mod)$coef[, 5], 3)
  res <- data.frame(hr.ci, p, stringsAsFactors = FALSE)
  res$p[res$p < 0.001] <- "<.001"
  colnames(res) <- c("HR (95% CI)", "p-value")
  return(res)
}

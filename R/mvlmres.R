#' Format resuls from multivariable linear regression model
#'
#' \code{mvlmres} takes a multivariable linear regression object and formats the
#' resulting Est (SE) and p-values into a table
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param mod is a multivariable linear regression object from \code{lm}
#'
#' @return Returns a dataframe
#'
#' @export
#'

mvlmres <- function(mod) {
  est_se <- paste0(
    round(summary(mod)$coefficients[-1, 1], 2), " (",
    round(summary(mod)$coefficients[-1, 2], 2), ")"
  )
  p <- round(summary(mod)$coefficients[-1, 4], 3)
  res <- data.frame(est_se, p, stringsAsFactors = F)
  res$p[res$p < 0.001] <- "<.001"
  colnames(res) <- c("**Est (SE)**", "**p-value**")
  return(res)
}

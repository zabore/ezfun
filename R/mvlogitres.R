#' Format resuls from multivariable Cox regression model
#'
#' \code{mvlogitres} takes a multivariable logistic regression object and formats the
#' resulting OR (95\% CI) and p-values into a table
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param mod is a multivariable logistic regression object from \code{glm}
#'
#' @return Returns a dataframe
#'
#' @export
#'

mvlogitres <- function(mod) {
  or.ci <- paste0(round(exp(coef(mod)[-1]), 2), " (", 
                  round(exp(confint.default(mod)[-1, 1]), 2), " - ",
                  round(exp(confint.default(mod)[-1, 2]), 2), ")")
  p <- round(summary(mod)$coefficients[-1, 4], 3)
  res <- data.frame(or.ci, p, stringsAsFactors = F)
  res$p[res$p < 0.001] <- "<.001"
  colnames(res) <- c('OR (95% CI)', 'p-value')
  return(res)
}

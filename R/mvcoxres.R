#' Format resuls from multivariable Cox regression model
#'
#' \code{mvcoxres} takes a multivariable Cox regression object and formats the
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

mvcoxres <- function(mod) {
  hr.ci <- paste0(round(summary(mod)$conf.int[, 1], 2), " (",
                  round(summary(mod)$conf.int[, 3], 2), ", ",
                  round(summary(mod)$conf.int[, 4], 2), ")")
  p <- round(summary(mod)$coefficients[, 5], 3)
  res <- data.frame(hr.ci, p, stringsAsFactors = F)
  res$p[res$p < 0.001] <- "<.001"
  colnames(res) <- c('**HR (95% CI)**', '**p-value**')
  return(res)
}

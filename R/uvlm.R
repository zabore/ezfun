#' Table of univariable linear regression results
#'
#' \code{uvlm} takes lists of continuous and/or categorical variables, calls
#' \code{\link[stats]{lm}} to run a linear regression model for each, and returns a table with
#' Est (SE) and p-value for each variable that is suitable for printing in a
#' Word \code{R Markdown} file.
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param contvars is a list of the continuous variables you want in the rows
#' e.g. list('Age')
#' @param catvars is a list of the categorical variables you want in the rows
#' e.g. list('Gender','Race')
#' @param out is the continuous outcome variable (needs to be in quotes)
#' @param dat is the dataset for analysis
#'
#' @return Returns a dataframe. If there are warnings or errors from \code{\link[stats]{lm}}
#' then blank rows are returned.
#'
#' @export
#'

uvlm <- function(contvars, catvars, out, dat) {

  dat <- as.data.frame(dat)

  mats <- vector('list', length(contvars) + length(catvars))
  nc <- length(contvars)

  if(!is.null(contvars)) {

    for(k in 1:nc) {

      mats[[k]] <- matrix(NA, nrow = 1, ncol = 3)
      tryCatch({

        coef <- summary(stats::lm(dat[, out] ~ dat[, contvars[[k]]]))$coefficients
        mats[[k]][, 2] <- paste0(
          round(coef[2, 1], 2), " (", round(coef[2, 2], 2), ")")
        mats[[k]][, 3] <- round(coef[2, 4], 3)
      }, warning = function(w) {
        mats[[k]][, 2] <- NA
        mats[[k]][, 3] <- NA
      }, error = function(e) {
        mats[[k]][, 2] <- NA
        mats[[k]][, 3] <- NA
      })

      mats[[k]] <- as.data.frame(mats[[k]], stringsAsFactors = FALSE)
      mats[[k]][, 1] <- as.character(mats[[k]][, 1])
      mats[[k]][1, 1] <- paste0("**", contvars[k], "**")
    }

  }

  if(!is.null(catvars)) {

    for(k in 1:length(catvars)) {

      mats[[k + nc]] <- matrix(
        '', nrow = length(
          unique(dat[, catvars[[k]]][!is.na(dat[, catvars[[k]]]) &
                                       !is.na(dat[, out])])) + 1, ncol = 3)

      tryCatch({

        m1 <- stats::lm(dat[!is.na(dat[, catvars[[k]]]), out] ~ 1)
        m2 <- stats::lm(dat[!is.na(dat[, catvars[[k]]]), out] ~ factor(dat[!is.na(dat[, catvars[[k]]]), catvars[[k]]]))
        coef <- summary(m2)$coefficients
        mats[[k + nc]][3:nrow(mats[[k + nc]]), 2] <- paste0(
          round(coef[-1, 1], 2), " (", round(coef[-1, 2], 2), ")")
        mats[[k + nc]][1, 3] <- round(stats::anova(m1, m2)["Pr(>F)"][2, ], 3)
      }, warning = function(w) {
        mats[[k + nc]][3:nrow(mats[[k + nc]]), 2] <- NA
        mats[[k + nc]][1, 3] <- NA
      }, error = function(e) {
        mats[[k + nc]][3:nrow(mats[[k + nc]]), 2] <- NA
        mats[[k + nc]][1, 3] <- NA
      })

      mats[[k + nc]] <- as.data.frame(mats[[k + nc]], stringsAsFactors = FALSE)
      mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
      mats[[k + nc]][1, 1] <- paste0("**", catvars[k], "**")
      for(l in 1:length(
        unique(dat[, catvars[[k]]][!is.na(dat[, catvars[[k]]]) &
                                   !is.na(dat[, out])]))) {

        mats[[k + nc]][l + 1, 1] <- paste(
          levels(as.factor(dat[!is.na(dat[, catvars[[k]]]) &
                                 !is.na(dat[, out]), catvars[[k]]]))[l])
      }

    }

  }

  mats <- do.call(rbind, mats)
  colnames(mats) <- c('**Variable**', '**Est (SE)**', '**p-value**')
  mats$"**p-value**" [mats$"**p-value**"  == '0'] <- "<.001"
  return(mats)
}

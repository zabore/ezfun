#' Table of univariable logistic regression results
#'
#' \code{uvlogit} takes lists of continuous and/or categorical variables, calls
#' \code{glm} to run a logistic regression model for each, and returns a table with
#' OR (95% CI) and p-value for each variable that is suitable for printing in a
#' Word \code{R Markdown} file.
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param contvars is a list of the continuous variables you want in the rows
#' e.g. list('Age')
#' @param catvars is a list of the categorical variables you want in the rows
#' e.g. list('Gender','Race')
#' @param out is the binary outcome variable. Must be coded 0/1.
#' (needs to be in quotes)
#' @param dat is the dataset for analysis
#'
#' @return Returns a dataframe. If there are warnings or errors from \code{glm}
#' then blank rows are returned.
#'
#' @export
#'

uvlogit <- function(contvars, catvars, out, dat) {

  library(aod)

  mats <- vector('list', length(contvars) + length(catvars))
  nc <- length(contvars)

  if(!is.null(contvars)) {

    for(k in 1:nc) {

      mats[[k]] <- matrix(NA, nrow = 1, ncol = 3)
      tryCatch({

        mod <- glm(dat[, out] ~ dat[, contvars[[k]]], family = "binomial")
        res <- exp(cbind(coef(mod), confint(mod)))
        mats[[k]][, 2] <- paste0(round(res[2, 1], 2), " (",
                                 round(res[2, 2], 2), "-",
                                 round(res[2, 3], 2), ")")
        mats[[k]][, 3] <- round(summary(mod)$coefficients[2, 4], 3)
      }, warning = function(w) {
        mats[[k]][, 2] <- NA
        mats[[k]][, 3] <- NA
      }, error = function(e) {
        mats[[k]][, 2] <- NA
        mats[[k]][, 3] <- NA
      })

      mats[[k]] <- as.data.frame(mats[[k]], stringsAsFactors = FALSE)
      mats[[k]][, 1] <- as.character(mats[[k]][, 1])
      mats[[k]][1, 1] <- paste(contvars[k])
    }

  }

  if(!is.null(catvars)) {

    for(k in 1:length(catvars)) {

      mats[[k + nc]] <- matrix('',
                               nrow = length(unique(dat[, catvars[[k]]][!is.na(dat[, catvars[[k]]]) &
                                                                          !is.na(dat[, out])])) + 1,
                               ncol = 3)

      tryCatch({

        mod2 <- glm(dat[!is.na(dat[, catvars[[k]]]), out] ~
                      factor(dat[!is.na(dat[, catvars[[k]]]), catvars[[k]]]),
                    family = "binomial")
        res2 <- exp(cbind(coef(mod2), confint(mod2)))
        mats[[k + nc]][3:nrow(mats[[k + nc]]), 2] <- paste0(round(res2[-1, 1], 2), " (",
                                                            round(res2[-1, 2], 2), "-",
                                                            round(res2[-1, 3], 2), ")")
        mats[[k + nc]][1, 3] <- round(wald.test(b = coef(mod2), Sigma = vcov(mod2),
                                                Terms = 2:length(coef(mod2)))$result$chi2["P"], 3)
      }, warning = function(w) {
        mats[[k + nc]][3:nrow(mats[[k + nc]]), 2] <- NA
        mats[[k + nc]][1, 3] <- NA
      }, error = function(e) {
        mats[[k + nc]][3:nrow(mats[[k + nc]]), 2] <- NA
        mats[[k + nc]][1, 3] <- NA
      })

      mats[[k + nc]] <- as.data.frame(mats[[k + nc]], stringsAsFactors = FALSE)
      mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
      mats[[k + nc]][1, 1] <- paste(catvars[k])
      for(l in 1:length(unique(dat[, catvars[[k]]][!is.na(dat[, catvars[[k]]]) &
                                                   !is.na(dat[, out])]))) {

        mats[[k + nc]][l + 1, 1] <- paste(levels(as.factor(dat[!is.na(dat[, catvars[[k]]])
                                                               & !is.na(dat[, out]), catvars[[k]]]))[l])
      }

    }

  }

  mats <- do.call(rbind, mats)
  colnames(mats) <- c('', 'OR (95% CI)', 'p-value')
  mats$`p-value`[mats$`p-value` == '0'] <- "<.001"
  return(mats)
}

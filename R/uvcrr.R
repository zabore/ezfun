#' Table of univariable competing risks regression results
#'
#' \code{uvcrr} takes lists of continuous and/or categorical variables, runs a
#' univariable \code{crr} model for each, and puts the resulting HR (95\% CI)
#' and p-value into a table suitable for printing in a Word \code{R Markdown}
#' file.
#'
#' \code{uvcrr} uses all function defaults to \code{crr}. For example, the
#' failure code is set to 1. See the help file for \code{crr} for additional
#' details.
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param contvars is a list of the continuous variables you want in the rows
#' e.g. list('Age')
#' @param catvars is a list of the categorical variables you want in the rows
#' e.g. list('Gender','Race')
#' @param event is the event indicator (needs to be in quotes)
#' @param time is the survival time variables (needs to be in quotes)
#' @param dat is the dataset for analysis
#'
#' @return Returns a dataframe. If there are warnings or errors from \code{crr}
#' then blank rows are returned.
#'
#' @export
#'

uvcrr <- function(contvars, catvars, event, time, dat) {

  library(cmprsk)
  library(aod)

  dat <- dat[!is.na(dat[, time]) & !is.na(dat[, event]), ]

  dat <- as.data.frame(dat)

  nc <- length(contvars)

  mats <- vector('list', length(catvars) + length(contvars))
  if(!is.null(contvars)) {

    for(k in 1:nc) {

      mats[[k]] <- matrix(NA, nrow = 1, ncol = 3)
      tryCatch({

        m1 <- crr(dat[!is.na(dat[, contvars[[k]]]), time],
                  dat[!is.na(dat[, contvars[[k]]]), event],
                  dat[!is.na(dat[, contvars[[k]]]), contvars[[k]]])
        mats[[k]][1, 2] <- paste0(round(summary(m1)$conf.int[, 1], 2), " (",
                                  round(summary(m1)$conf.int[, 3], 2), "-",
                                  round(summary(m1)$conf.int[, 4], 2), ")")
        mats[[k]][1, 3] <- round(summary(m1)$coef[, 5], 3)
      }, warning = function(w) {
        print(w$message)
        mats[[k]][1, 2] <- NA
        mats[[k]][1, 3] <- NA
      }, error = function(e) {
        print(e$message)
        mats[[k]][1, 2] <- NA
        mats[[k]][1, 3] <- NA
      })

      mats[[k]] <- as.data.frame(mats[[k]], stringsAsFactors = FALSE)
      mats[[k]][, 1] <- as.character(mats[[k]][, 1])
      mats[[k]][, 1] <- paste0("**", contvars[k], "**")
    }

  }

  if(!is.null(catvars)) {

    for(k in 1:length(catvars)) {

      mats[[k + nc]] <- matrix(
        '', nrow = length(levels(factor(dat[, catvars[[k]]]))) + 1, ncol = 3)
      covs1 <- model.matrix(~ factor(dat[, catvars[[k]]]))[, -1]
      tryCatch({

        m2 <- crr(dat[!is.na(dat[, catvars[[k]]]), time],
                  dat[!is.na(dat[, catvars[[k]]]), event], covs1)
        p1 <- wald.test(
          m2$var, m2$coef,
          Terms = 1:(length(levels(factor(dat[, catvars[[k]]]))) - 1))
        for(i in 1:length(levels(factor(dat[, catvars[[k]]])))) {

          if(i == 1) {

            mats[[k + nc]][i + 1, 2] <- '1.00'
          }

          else if(i > 1) {

            mats[[k + nc]][i + 1, 2] <- paste0(
              round(summary(m2)$conf.int[i - 1, 1], 2), " (",
              round(summary(m2)$conf.int[i - 1, 3], 2), "-",
              round(summary(m2)$conf.int[i - 1, 4], 2), ")")
          }

        }

        mats[[k + nc]][1, 3] <- round(p1$result$chi2[3], 3)
      }, warning = function(w) {
        print(w$message)
        mats[[k + nc]][2:length(levels(factor(dat[, catvars[[k]]]))), 2] <- NA
        mats[[k + nc]][1, 3] <- NA
      }, error = function(e) {
        print(e$message)
        mats[[k + nc]][2:length(levels(factor(dat[, catvars[[k]]]))), 2] <- NA
        mats[[k + nc]][1, 3] <- NA
      })

      for(i in 1:length(levels(factor(dat[, catvars[[k]]])))) {

        mats[[k + nc]][i + 1, 1] <- paste(levels(as.factor(dat[, catvars[[k]]]))[i])
      }

      mats[[k + nc]] <- as.data.frame(mats[[k + nc]], stringsAsFactors = FALSE)
      mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
      mats[[k + nc]][1, 1] <- paste0("**", catvars[k], "**")
    }

  }

  mats<-do.call(rbind, mats)
  colnames(mats)[1:3] <- c('**Variable**', '**HR (95% CI)**', '**p-value**')
  mats$"**p-value**"[mats$"**p-value**" == '0'] <- "<.001"
  return(mats)
}

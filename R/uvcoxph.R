#' Table of univariable Cox regression results
#'
#' \code{uvcoxph} takes lists of continuous and/or categorical variables, runs a univariable \code{\link[survival]{coxph}} model for
#' each, and puts the resulting HR (95\% CI) and p-value into a table suitable for printing in a Word R Markdown
#' file.
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param contvars is a list of the continuous variables you want in the rows e.g. list('Age')
#' @param catvars is a list of the categorical variables you want in the rows e.g. list('Gender','Race')
#' @param event is the event indicator (needs to be in quotes)
#' @param time is the survival time variables (needs to be in quotes)
#' @param dat is the dataset for analysis
#' @param strata is a possible strata term for use in calculating the log-rank
#' p-values. Defaults to NULL. Entries should be in quotes, e.g. "Surgeon"
#'
#' @return Returns a dataframe. If there are warnings or errors from \code{\link[survival]{coxph}} then blank rows are returned.
#'
#' @export
#'

uvcoxph <- function(contvars, catvars, event, time, dat, strata = NULL) {

  dat <- dat[!is.na(dat[, time]) & !is.na(dat[, event]), ]

  dat <- as.data.frame(dat)

  mats <- vector('list', length(contvars) + length(catvars))

  if(is.null(strata)) {

    if(!is.null(contvars)) {

      for(k in 1:length(contvars)) {

        mats[[k]] <- matrix(NA, nrow = 1, ncol = 3)
        tryCatch({

          m1 <- survival::coxph(survival::Surv(dat[, time], dat[, event]) ~ dat[, contvars[[k]]])
          mats[[k]][1, 2] <- paste0(
            round(summary(m1)$conf.int[, "exp(coef)"], 2), " (",
            round(summary(m1)$conf.int[, "lower .95"], 2), "-",
            round(summary(m1)$conf.int[, "upper .95"], 2), ")")
          mats[[k]][1, 3] <- round(summary(m1)$sctest["pvalue"], 3)
        }, warning = function(w) {
          print(utils::str(w$message))
          mats[[k]][1, 2] <- NA
          mats[[k]][1, 3] <- NA
        }, error = function(e) {
          print(utils::str(e$message))
          mats[[k]][1, 2] <- NA
          mats[[k]][1, 3] <- NA
        })

        mats[[k]] <- as.data.frame(mats[[k]], stringsAsFactors = FALSE)
        mats[[k]][, 1] <- as.character(mats[[k]][, 1])
        mats[[k]][, 1]<- paste0("**", contvars[k], "**")
      }

    }

    if(!is.null(catvars)) {

      for(k in 1:length(catvars)) {

        mats[[k + length(contvars)]] <- matrix(
          ' ', nrow = length(levels(factor(dat[, catvars[[k]]]))) + 1, ncol = 3)

        tryCatch({
          m2 <- survival::coxph(survival::Surv(dat[, time], dat[, event]) ~
                        factor(dat[, catvars[[k]]]))

          for(i in 1:length(levels(factor(dat[, catvars[[k]]])))) {

            if(i == 1) {

              mats[[k + length(contvars)]][i + 1, 2] <- '1.00'
            }

            else if(i > 1) {

              mats[[k + length(contvars)]][i + 1, 2] <- paste0(
                round(summary(m2)$conf.int[i - 1, "exp(coef)"], 2), " (",
                round(summary(m2)$conf.int[i - 1, "lower .95"], 2), "-",
                round(summary(m2)$conf.int[i - 1, "upper .95"], 2), ")")
            }

          }

          mats[[k + length(contvars)]][1, 3] <- round(
            summary(m2)$sctest["pvalue"], 3)
        }, warning = function(w) {
          print(w$message)
          mats[[k + length(contvars)]][2:length(
            levels(factor(dat[, catvars[[k]]]))), 2] <- NA
          mats[[k + length(contvars)]][1, 3] <- NA
        }, error = function(e) {
          print(e$message)
          mats[[k + length(contvars)]][2:length(
            levels(factor(dat[, catvars[[k]]]))), 2] <- NA
          mats[[k + length(contvars)]][1, 3] <- NA
        })

        for(i in 1:length(levels(factor(dat[, catvars[[k]]])))) {

          mats[[k + length(contvars)]][i + 1, 1] <- paste(
            levels(as.factor(dat[, catvars[[k]]]))[i])
        }

        mats[[k + length(contvars)]] <- as.data.frame(
          mats[[k + length(contvars)]], stringsAsFactors = FALSE)
        mats[[k + length(contvars)]][, 1] <- as.character(
          mats[[k + length(contvars)]][, 1])
        mats[[k + length(contvars)]][1, 1]<- paste0("**", catvars[k], "**")
      }

    }
  } else {

    if(!is.null(contvars)) {

      for(k in 1:length(contvars)) {

        mats[[k]] <- matrix(NA, nrow = 1, ncol = 3)
        tryCatch({

          m1 <- survival::coxph(survival::Surv(dat[, time], dat[, event]) ~ dat[, contvars[[k]]] +
                        strata(dat[, strata]))
          mats[[k]][1, 2] <- paste0(
            round(summary(m1)$conf.int[, "exp(coef)"], 2), " (",
            round(summary(m1)$conf.int[, "lower .95"], 2), "-",
            round(summary(m1)$conf.int[, "upper .95"], 2), ")")
          mats[[k]][1, 3] <- round(summary(m1)$sctest["pvalue"], 3)
        }, warning = function(w) {
          print(utils::str(w$message))
          mats[[k]][1, 2] <- NA
          mats[[k]][1, 3] <- NA
        }, error = function(e) {
          print(utils::str(e$message))
          mats[[k]][1, 2] <- NA
          mats[[k]][1, 3] <- NA
        })

        mats[[k]] <- as.data.frame(mats[[k]], stringsAsFactors = FALSE)
        mats[[k]][, 1] <- as.character(mats[[k]][, 1])
        mats[[k]][, 1]<- paste0("**", contvars[k], "**")
      }

    }

    if(!is.null(catvars)) {

      for(k in 1:length(catvars)) {

        mats[[k + length(contvars)]] <- matrix(
          ' ', nrow = length(levels(factor(dat[, catvars[[k]]]))) + 1, ncol = 3)

        tryCatch({
          m2 <- survival::coxph(survival::Surv(dat[, time], dat[, event]) ~
                        factor(dat[, catvars[[k]]]) + strata(dat[, strata]))

          for(i in 1:length(levels(factor(dat[, catvars[[k]]])))) {

            if(i == 1) {

              mats[[k + length(contvars)]][i + 1, 2] <- '1.00'
            }

            else if(i > 1) {

              mats[[k + length(contvars)]][i + 1, 2] <- paste0(
                round(summary(m2)$conf.int[i - 1, "exp(coef)"], 2), " (",
                round(summary(m2)$conf.int[i - 1, "lower .95"], 2), "-",
                round(summary(m2)$conf.int[i - 1, "upper .95"], 2), ")")
            }

          }

          mats[[k + length(contvars)]][1, 3] <- round(
            summary(m2)$sctest["pvalue"], 3)
        }, warning = function(w) {
          print(w$message)
          mats[[k + length(contvars)]][2:length(
            levels(factor(dat[, catvars[[k]]]))), 2] <- NA
          mats[[k + length(contvars)]][1, 3] <- NA
        }, error = function(e) {
          print(e$message)
          mats[[k + length(contvars)]][2:length(
            levels(factor(dat[, catvars[[k]]]))), 2] <- NA
          mats[[k + length(contvars)]][1, 3] <- NA
        })

        for(i in 1:length(levels(factor(dat[, catvars[[k]]])))) {

          mats[[k + length(contvars)]][i + 1, 1] <- paste(
            levels(as.factor(dat[, catvars[[k]]]))[i])
        }

        mats[[k + length(contvars)]] <- as.data.frame(
          mats[[k + length(contvars)]], stringsAsFactors = FALSE)
        mats[[k + length(contvars)]][, 1] <- as.character(
          mats[[k + length(contvars)]][, 1])
        mats[[k + length(contvars)]][1, 1]<- paste0("**", catvars[k], "**")
      }

    }
  }

  mats <- do.call(rbind, mats)
  colnames(mats) <- c('**Variable**', '**HR (95% CI)**', '**p-value**')
  mats$"**p-value**"[mats$"**p-value**" == '0'] <- "<.001"
  return(mats)
}

#' Table of univariable survival analysis results
#'
#' \code{uvsurv} takes lists of continuous and/or categorical variables.
#' For continuous variables, \code{\link[survival]{coxph}} returns HR (95\% CI) and
#' log-rank p-values. For categorical variables, \code{\link[survival]{coxph}} returns
#' HR (95\% CI) and log-rank p-values and \code{\link[survival]{survfit}} produces median
#' survival (95\% CI) and a survival estimate at a specified time. Results
#' are put into  a table suitable for printing in a WordR Markdown
#' file.
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param contvars is a list of the continuous variables you want in the rows
#' e.g. list('Age')
#' @param catvars is a list of the categorical variables you want in the rows
#' e.g. list('Gender','Race')
#' @param event is the survival event indicator (needs to be in quotes)
#' @param time is the survival time variable (needs to be in quotes)
#' @param test is the timepoint you would like to estimate, in whatever
#' units the survival time is in
#' @param dat is the dataset to use for analysis
#' @param strata is a possible strata term for use in calculating the log-rank
#' p-values. Defaults to NULL. Entries should be in quotes, e.g. "Surgeon"
#' @param cuminc if TRUE allows you to get the estimates at specified timepoint
#' on cumulative incidence scale (i.e. 1 - KM) rather than survival scale.
#' Defaults to FALSE.
#'
#' @return Returns a dataframe
#'
#' @export
#'

uvsurv <- function(contvars, catvars, event, time, test, dat, strata = NULL,
                   cuminc = FALSE) {

  dat <- as.data.frame(dat)

  mats <- vector('list', length(catvars) + length(contvars))
  nc <- length(contvars)

  if(is.null(strata)) {
    if(!is.null(contvars)) {

      for(k in 1:nc) {

        mats[[k]] <- matrix(NA, nrow = 1, ncol = 5)
        c1 <- survival::coxph(survival::Surv(dat[, time], dat[, event]) ~ dat[, contvars[[k]]])
        mats[[k]][1, 2] <- 'NA'
        mats[[k]][1, 3] <- 'NA'
        mats[[k]][1, 4] <- paste0(
          round(summary(c1)$conf.int[1, "exp(coef)"], 2), " (",
          round(summary(c1)$conf.int[1, "lower .95"], 2), "-",
          round(summary(c1)$conf.int[1, "upper .95"], 2), ")")
        mats[[k]][1, 5] <- round(summary(c1)$sctest["pvalue"], 3)
        mats[[k]][1, 5][mats[[k]][1, 5] == '0'] <- "<.001"
        mats[[k]] <- as.data.frame(mats[[k]], stringsAsFactors = F)
        mats[[k]][, 1] <- as.character(mats[[k]][, 1])
        mats[[k]][, 1] <- paste0("**", contvars[k], "**")
      }

    }

    if(!is.null(catvars) & cuminc == FALSE) {

      for(k in 1:length(catvars)) {
        mats[[k + nc]] <- matrix('', nrow = length(
          levels(factor(dat[, catvars[[k]]]))) + 1, ncol = 5)
        s1 <- survival::Surv(dat[, time], dat[, event]) ~ factor(dat[, catvars[[k]]])
        f1 <- survival::survfit(s1)
        c1 <- survival::coxph(s1)

        for(i in 1:length(levels(factor(dat[, catvars[[k]]])))) {
          mats[[k + nc]][i + 1, 1] <- paste(
            levels(as.factor(dat[, catvars[[k]]]))[i])
          mats[[k + nc]][i + 1, 2] <- paste0(
            round(summary(f1)$table[i, 'median'], 2), " (",
            round(summary(f1)$table[i, "0.95LCL"], 2), "-",
            round(summary(f1)$table[i, "0.95UCL"], 2), ")")
          mats[[k + nc]][i + 1,3] <- paste0(
            round(summary(f1, times = test)$surv, 2), " (",
            round(summary(f1, times = test)$lower, 2), "-",
            round(summary(f1,times=test)$upper,2), ")")[i]
          if(i == 1) {
            mats[[k + nc]][i + 1, 4] <- '1.00'
          } else if(i > 1) {
            mats[[k + nc]][i + 1, 4] <- paste0(
              round(summary(c1)$conf.int[i - 1, "exp(coef)"], 2), " (",
              round(summary(c1)$conf.int[i - 1, "lower .95"], 2), "-",
              round(summary(c1)$conf.int[i - 1, "upper .95"],2), ")")
          }
        }
        mats[[k + nc]][1, 5] <- round(summary(c1)$sctest["pvalue"], 3)
        mats[[k + nc]][1, 5][mats[[k + nc]][1, 5] == '0'] <- "<.001"
        mats[[k + nc]] <- as.data.frame(mats[[k + nc]], stringsAsFactors = F)
        mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
        mats[[k + nc]][1, 1] <- paste0("**", catvars[k], "**")
      }

    } else if(!is.null(catvars) & cuminc == TRUE) {

        for(k in 1:length(catvars)) {
          mats[[k + nc]] <- matrix('', nrow = length(
            levels(factor(dat[, catvars[[k]]]))) + 1, ncol = 5)
          s1 <- survival::Surv(dat[, time], dat[, event]) ~ factor(dat[, catvars[[k]]])
          f1 <- survival::survfit(s1)
          c1 <- survival::coxph(s1)

          for(i in 1:length(levels(factor(dat[, catvars[[k]]])))) {
            mats[[k + nc]][i + 1, 1] <- paste(
              levels(as.factor(dat[, catvars[[k]]]))[i])
            mats[[k + nc]][i + 1, 2] <- paste0(
              round(summary(f1)$table[i, 'median'], 2), " (",
              round(summary(f1)$table[i, "0.95LCL"], 2), "-",
              round(summary(f1)$table[i, "0.95UCL"], 2), ")")
            mats[[k + nc]][i + 1,3] <- paste0(
              round(1 - summary(f1, times = test)$surv, 2), " (",
              round(1 - summary(f1, times = test)$upper, 2), "-",
              round(1 - summary(f1,times=test)$lower,2), ")")[i]
            if(i == 1) {
              mats[[k + nc]][i + 1, 4] <- '1.00'
            } else if(i > 1) {
              mats[[k + nc]][i + 1, 4] <- paste0(
                round(summary(c1)$conf.int[i - 1, "exp(coef)"], 2), " (",
                round(summary(c1)$conf.int[i - 1, "lower .95"], 2), "-",
                round(summary(c1)$conf.int[i - 1, "upper .95"],2), ")")
            }
          }
          mats[[k + nc]][1, 5] <- round(summary(c1)$sctest["pvalue"], 3)
          mats[[k + nc]][1, 5][mats[[k + nc]][1, 5] == '0'] <- "<.001"
          mats[[k + nc]] <- as.data.frame(mats[[k + nc]], stringsAsFactors = F)
          mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
          mats[[k + nc]][1, 1] <- paste0("**", catvars[k], "**")
        }

      }
  } else {

    if(!is.null(contvars)) {

      for(k in 1:nc) {

        mats[[k]] <- matrix(NA, nrow = 1, ncol = 5)
        c1 <- survival::coxph(survival::Surv(dat[, time], dat[, event]) ~ dat[, contvars[[k]]] +
                      strata(dat[, strata]))
        mats[[k]][1, 2] <- 'NA'
        mats[[k]][1, 3] <- 'NA'
        mats[[k]][1, 4] <- paste0(
          round(summary(c1)$conf.int[1, "exp(coef)"], 2), " (",
          round(summary(c1)$conf.int[1, "lower .95"], 2), "-",
          round(summary(c1)$conf.int[1, "upper .95"], 2), ")")
        mats[[k]][1, 5] <- round(summary(c1)$sctest["pvalue"], 3)
        mats[[k]][1, 5][mats[[k]][1, 5] == '0'] <- "<.001"
        mats[[k]] <- as.data.frame(mats[[k]], stringsAsFactors = F)
        mats[[k]][, 1] <- as.character(mats[[k]][, 1])
        mats[[k]][, 1] <- paste0("**", contvars[k], "**")
      }

    }

    if(!is.null(catvars) & cuminc == FALSE) {

      for(k in 1:length(catvars)) {
        mats[[k + nc]] <- matrix('', nrow = length(
          levels(factor(dat[, catvars[[k]]]))) + 1, ncol = 5)
        s1 <- survival::Surv(dat[, time], dat[, event]) ~ factor(dat[, catvars[[k]]])
        f1 <- survival::survfit(s1)
        c1 <- survival::coxph(survival::Surv(dat[, time], dat[, event]) ~
                      factor(dat[, catvars[[k]]]) + strata(dat[, strata]))

        for(i in 1:length(levels(factor(dat[, catvars[[k]]])))) {
          mats[[k + nc]][i + 1, 1] <- paste(
            levels(as.factor(dat[, catvars[[k]]]))[i])
          mats[[k + nc]][i + 1, 2] <- paste0(
            round(summary(f1)$table[i, 'median'], 2), " (",
            round(summary(f1)$table[i, "0.95LCL"], 2), "-",
            round(summary(f1)$table[i, "0.95UCL"], 2), ")")
          mats[[k + nc]][i + 1,3] <- paste0(
            round(summary(f1, times = test)$surv, 2), " (",
            round(summary(f1, times = test)$lower, 2), "-",
            round(summary(f1,times=test)$upper,2), ")")[i]
          if(i == 1) {
            mats[[k + nc]][i + 1, 4] <- '1.00'
          } else if(i > 1) {
            mats[[k + nc]][i + 1, 4] <- paste0(
              round(summary(c1)$conf.int[i - 1, "exp(coef)"], 2), " (",
              round(summary(c1)$conf.int[i - 1, "lower .95"], 2), "-",
              round(summary(c1)$conf.int[i - 1, "upper .95"],2), ")")
          }
        }
        mats[[k + nc]][1, 5] <- round(summary(c1)$sctest["pvalue"], 3)
        mats[[k + nc]][1, 5][mats[[k + nc]][1, 5] == '0'] <- "<.001"
        mats[[k + nc]] <- as.data.frame(mats[[k + nc]], stringsAsFactors = F)
        mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
        mats[[k + nc]][1, 1] <- paste0("**", catvars[k], "**")
      }

    } else if(!is.null(catvars) & cuminc == TRUE) {

      for(k in 1:length(catvars)) {
        mats[[k + nc]] <- matrix('', nrow = length(
          levels(factor(dat[, catvars[[k]]]))) + 1, ncol = 5)
        s1 <- survival::Surv(dat[, time], dat[, event]) ~ factor(dat[, catvars[[k]]])
        f1 <- survival::survfit(s1)
        c1 <- survival::coxph(survival::Surv(dat[, time], dat[, event]) ~
                      factor(dat[, catvars[[k]]]) + strata(dat[, strata]))

        for(i in 1:length(levels(factor(dat[, catvars[[k]]])))) {
          mats[[k + nc]][i + 1, 1] <- paste(
            levels(as.factor(dat[, catvars[[k]]]))[i])
          mats[[k + nc]][i + 1, 2] <- paste0(
            round(summary(f1)$table[i, 'median'], 2), " (",
            round(summary(f1)$table[i, "0.95LCL"], 2), "-",
            round(summary(f1)$table[i, "0.95UCL"], 2), ")")
          mats[[k + nc]][i + 1,3] <- paste0(
            round(1 - summary(f1, times = test)$surv, 2), " (",
            round(1 - summary(f1, times = test)$upper, 2), "-",
            round(1 - summary(f1,times=test)$lower,2), ")")[i]
          if(i == 1) {
            mats[[k + nc]][i + 1, 4] <- '1.00'
          } else if(i > 1) {
            mats[[k + nc]][i + 1, 4] <- paste0(
              round(summary(c1)$conf.int[i - 1, "exp(coef)"], 2), " (",
              round(summary(c1)$conf.int[i - 1, "lower .95"], 2), "-",
              round(summary(c1)$conf.int[i - 1, "upper .95"],2), ")")
          }
        }
        mats[[k + nc]][1, 5] <- round(summary(c1)$sctest["pvalue"], 3)
        mats[[k + nc]][1, 5][mats[[k + nc]][1, 5] == '0'] <- "<.001"
        mats[[k + nc]] <- as.data.frame(mats[[k + nc]], stringsAsFactors = F)
        mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
        mats[[k + nc]][1, 1] <- paste0("**", catvars[k], "**")
      }

    }

  }

  mats <- do.call(rbind,mats)
  colnames(mats)[1:5] <- c('**Variable**',
                           '**Median survival (95% CI)**',
                           '**Estimate (95% CI)**',
                           '**HR (95% CI)**',
                           '**p-value**')
  return(mats)
}


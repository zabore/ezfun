#' Table 1 with random effects model p-values
#'
#' \code{tab1_re} takes lists of continuous and/or categorical variables and
#' returns Median (spread) for continuous variables and N (\%) for categorical
#' variables. Produces a table with both an overall column and columns by
#' another variable. For a binary by variable only, it produces p-values from a
#' random effects model.
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param contvars is a list of the continuous variables you want in the rows
#' e.g. list('Age'). Can be NULL.
#' @param catvars is a list of the categorical variables you want in the rows
#' e.g. list('Gender','Race'). Can be NULL.
#' @param byvar is the categorical variable you want to tabulate by across the
#' columns (needs to be in quotes). MUST BE 0/1 since it will be used as the
#' outcome variable in \code{glmer}.
#' @param dat is the dataset to use for analysis
#' @param col takes the value TRUE or FALSE indicating whether you want column
#' percent (TRUE, default) or row percent (FALSE)
#' @param spread takes the value "range" or "iqr" indicating whether you want
#' (min, max) or (Q1, Q3) in summaries of continuous variables. Defaults to
#' "range".
#'
#' @return Returns a dataframe. If there are warnings or errors from
#' \code{glmer} then NA is returned in place of the p-value.
#'
#' @export
#'

tab1_re <- function(contvars, catvars, byvar, re, dat, col = TRUE,
                    spread = "range") {

  library(lme4)

  if(spread == "range") vals <- list("Min.", "Max.") else
    if(spread == "iqr") vals <- list("1st Qu.", "3rd Qu.")

    if(is.null(byvar)) {print("Must supply a binary argument to byvar")}

    else if(!is.null(byvar)) {

      mats <- vector('list', length(contvars) + length(catvars))
      nc <- length(contvars)

      if(!is.null(contvars)) {

        for(k in 1:nc) {

          if(anyNA(dat[, contvars[[k]]]) == FALSE) {

            mats[[k]] <- matrix(NA, nrow = 1,
                                ncol = length(unique(dat[, byvar])) + 3)
            y <- round(summary(dat[, contvars[[k]]]), 2)
            x <- tapply(dat[, contvars[[k]]], dat[, byvar], summary)
            for(j in 1:length(unique(dat[, byvar]))) {

              mats[[k]][1, 2] <- paste0(y["Median"], " (",
                                        y[vals[[1]]], ", ",
                                        y[vals[[2]]], ")")
              mats[[k]][1, j + 2] <- paste0(round(sapply(x, "[",
                                                         "Median")[j], 2), " (",
                                            round(sapply(x, "[",
                                                         vals[[1]])[j], 2), ", ",
                                            round(sapply(x, "[",
                                                         vals[[2]])[j], 2), ")")
            }

            form <- as.formula(paste0(byvar, " ~ scale(", contvars[[k]],
                                      ") + (1 | ", re, ")"))

            mats[[k]][1, ncol(mats[[k]])] <- tryCatch(
              round(summary(glmer(form, data = dat,
                                  family = binomial))$coefficients[2, 4], 3),
              warning = function(w) {return(NA)},
              error = function(e) {return(NA)})

            mats[[k]] <- as.data.frame(mats[[k]], stringsAsFactors = FALSE)
            mats[[k]][, 1] <- as.character(mats[[k]][, 1])
            mats[[k]][1, 1] <- paste(contvars[k])
          }

          else if(anyNA(dat[,contvars[[k]]]) == TRUE) {

            mats[[k]] <- matrix(NA, nrow = 2, ncol = length(unique(dat[, byvar])) + 3)
            y <- round(summary(dat[, contvars[[k]]]), 2)
            x <- tapply(dat[, contvars[[k]]], dat[, byvar], summary)
            for(j in 1:length(unique(dat[, byvar]))) {

              mats[[k]][1, 2] <- paste0(y["Median"], " (",
                                        y[vals[[1]]], ", ",
                                        y[vals[[2]]], ")")
              mats[[k]][2, 2] <- y["NA's"]
              mats[[k]][1, j + 2] <- paste0(round(sapply(x, "[" ,
                                                         "Median")[j], 2), " (",
                                            round(sapply(x, "[",
                                                         vals[[1]])[j], 2), ", ",
                                            round(sapply(x, "[",
                                                         vals[[2]])[j], 2), ")")
              mats[[k]][2, j + 2] <- sapply(x, "[", "NA's")[j]
            }

            mats[[k]][1, ncol(mats[[k]])] <- tryCatch(
              round(summary(glmer(dat[, byvar] ~ scale(dat[, contvars[[k]]]) +
                                    (1 | dat[, re]),
                                  family = binomial))$coefficients[2, 4], 3),
              warning = function(w) {return(NA)},
              error = function(e) {return(NA)})

            mats[[k]] <- as.data.frame(mats[[k]], stringsAsFactors = FALSE)
            mats[[k]][, 1] <- as.character(mats[[k]][, 1])
            mats[[k]][1, 1] <- paste(contvars[k])
            mats[[k]][2, 1] <- "N missing"
            mats[[k]][, ncol(mats[[k]])] <- as.character(mats[[k]][, ncol(mats[[k]])])
            mats[[k]][2, ncol(mats[[k]])] <- ''
          }

        }

      }

      if(!is.null(catvars)) {

        for(k in 1:length(catvars)) {

          mats[[k + nc]] <- matrix('',
                                   nrow = length(unique(dat[, catvars[[k]]])) + 1,
                                   ncol = length(unique(dat[, byvar])) + 3)
          y1 <- table(dat[, catvars[[k]]], useNA = 'ifany')
          x1 <- table(dat[, catvars[[k]]], dat[, byvar], useNA = 'ifany')
          if(col == TRUE) {

            y2 <- prop.table(y1)
            x2 <- prop.table(x1, 2)
            for(j in 1:length(unique(dat[, byvar]))) {

              for(i in 1:length(unique(dat[, catvars[[k]]]))) {

                mats[[k + nc]][i + 1, 2] <- paste0(y1[i], " (",
                                                   round(y2[i] * 100, 1), ")")
                mats[[k + nc]][i + 1, j + 2] <- paste(x1[i, j], " (",
                                                      round(x2[i, j] * 100, 1), ")",
                                                      sep = '')
              }

            }

          }

          else if(col == FALSE) {

            x2 <- prop.table(x1, 1)
            for(j in 1:length(unique(dat[, byvar]))) {

              for(i in 1:length(unique(dat[, catvars[[k]]]))) {

                mats[[k + nc]][i + 1, 2] <- y1[i]
                mats[[k + nc]][i + 1, j + 2] <- paste(x1[i, j], " (",
                                                      round(x2[i, j] * 100, 1), ")",
                                                      sep = '')
              }

            }

          }

          form0 <- as.formula(paste0(byvar, " ~ 1 + (1 | ", re, ")"))
          form2 <- as.formula(paste0(byvar, " ~ factor(", catvars[[k]],
                                    ") + (1 | ", re, ")"))

          mats[[k + nc]][1, ncol(mats[[k + nc]])] <- tryCatch(
            round(anova(glmer(form0, data = dat[!is.na(dat[, catvars[[k]]]), ],
                              family = binomial),
                        glmer(form2, data = dat[!is.na(dat[, catvars[[k]]]), ],
                              family = binomial))$"Pr(>Chisq)"[2], 3),
            warning = function(w) {return(NA)},
            error = function(e) {return(NA)})

          mats[[k + nc]] <- as.data.frame(mats[[k + nc]], stringsAsFactors = FALSE)
          mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
          mats[[k + nc]][1, 1] <- paste(catvars[k])
          if(any(is.na(dat[, catvars[[k]]])) == FALSE) {

            for(l in 1:length(unique(dat[, catvars[[k]]]))) {

              mats[[k + nc]][l + 1, 1] <- paste(levels(as.factor(dat[, catvars[[k]]]))[l])
            }

          }

          else if(any(is.na(dat[, catvars[[k]]])) == TRUE) {

            for(l in 1:(length(unique(dat[, catvars[[k]]])) - 1)) {

              mats[[k + nc]][l + 1, 1] <- paste(levels(as.factor(dat[, catvars[[k]]]))[l])
            }

            mats[[k + nc]][length(unique(dat[, catvars[[k]]])) + 1, 1] <- 'NA'
          }

        }

      }

      mats <- do.call(rbind, mats)
      colnames(mats)[1:2] <- c('', 'Overall')
      colnames(mats)[ncol(mats)] <- 'p-value'
      mats$`p-value`[mats$`p-value` == '0'] <- "<.001"
      for(m in 1:length(unique(dat[, byvar]))) {

        colnames(mats)[m + 2] <- paste(levels(as.factor(dat[, byvar]))[m])
      }

      return(mats)
    }
}

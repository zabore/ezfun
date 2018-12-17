###############################################################################
#' Table 1
#'
#' \code{tab1} takes lists of continuous and/or categorical variables and
#' returns Median (spread) for continuous variables and N (\%) for categorical
#' variables. Produces a table with both an overall column and columns by
#' another variable.
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param contvars is a list of the continuous variables you want in the rows
#' e.g. list('Age'). Can be NULL.
#' @param catvars is a list of the categorical variables you want in the rows
#' e.g. list('Gender','Race'). Can be NULL.
#' @param byvar is the categorical variable you want to tabulate by across the
#' columns (needs to be in quotes). Can be NULL.
#' @param dat is the dataset to use for analysis
#' @param col takes the value TRUE or FALSE indicating whether you want column
#' percent (TRUE, default) or row percent (FALSE)
#' @param spread takes the value "range" or "iqr" indicating whether you want
#' (min, max) or (Q1, Q3) in summaries of continuous variables. Defaults to
#' "range".
#' @param pval takes the value TRUE or FALSE indicating whether p-values should
#' be included. Defaults to TRUE. If TRUE, \code{stats::kruskal.test} p-values are
#' produced for continuous variables and either \code{stats::fisher.test} or
#' \code{stats::chisq.test} p-values are produced for categorical variables.
#' See \code{param} for testing details for categorical variables.
#' @param fisher takes the value TRUE or FALSE. If TRUE, \code{stats::fisher.test}
#' p-values are produced. If FALSE, \code{stats::chisq.test} p-values are produced.
#'
#' @return Returns a dataframe. If there are warnings or errors from
#' \code{stats::kruskal.test}, \code{stats::fisher.test}, or \code{stats::chisq.test} then NA is
#' returned in place of the p-value.
#'
#' @export
#'
##############################################################################

tab1 <- function(contvars, catvars, byvar, dat, col = TRUE, spread = "range",
                 pval = TRUE, fisher = TRUE) {

  dat <- as.data.frame(dat)

  if(spread == "range") vals <- list("Min.", "Max.") else
    if(spread == "iqr") vals <- list("1st Qu.", "3rd Qu.")

    if(is.null(byvar)) {

      mats <- vector('list', length(contvars) + length(catvars))
      nc <- length(contvars)

      if(!is.null(contvars)) {

        for(k in 1:nc) {

          if(anyNA(dat[, contvars[[k]]]) == FALSE) {

            mats[[k]] <- matrix(NA, nrow = 1, ncol = 2)
            y <- round(summary(dat[, contvars[[k]]]), 1)
            mats[[k]][, 2] <- paste0(y["Median"], " (",
                                     y[vals[[1]]], ", ",
                                     y[vals[[2]]], ")")
            mats[[k]] <- as.data.frame(mats[[k]],
                                       stringsAsFactors = FALSE)
            mats[[k]][, 1] <- as.character(mats[[k]][, 1])
            mats[[k]][1, 1] <- paste0("**", contvars[k], "**")
          }

          else if(anyNA(dat[, contvars[[k]]]) == TRUE) {

            mats[[k]] <- matrix(NA, nrow = 2, ncol = 2)
            y <- round(summary(dat[, contvars[[k]]]), 1)
            mats[[k]][1, 2] <- paste0(y["Median"], " (",
                                      y[vals[[1]]], ", ",
                                      y[vals[[2]]], ")")
            mats[[k]][2, 2] <- y["NA's"]
            mats[[k]] <- as.data.frame(mats[[k]],
                                       stringsAsFactors = FALSE)
            mats[[k]][, 1] <- as.character(mats[[k]][, 1])
            mats[[k]][1, 1] <- paste0("**", contvars[k], "**")
            mats[[k]][2, 1] <- "N missing"
          }

        }

      }

      if(!is.null(catvars)) {

        for(k in 1:length(catvars)) {

          if(is.factor(dat[, catvars[[k]]]) == TRUE) {
            dat[, catvars[[k]]] <- factor(dat[, catvars[[k]]])}

          mats[[k + nc]] <- matrix('',
                                   nrow = length(unique(
                                     dat[, catvars[[k]]])) + 1,
                                   ncol = 2)
          y1 <- table(dat[, catvars[[k]]], useNA = 'ifany')
          y2 <- prop.table(y1)
          for(i in 1:length(unique(dat[, catvars[[k]]]))) {

            mats[[k + nc]][i + 1, 2] <- paste0(
              y1[i], " (", round(y2[i] * 100, 1), ")")
          }
          mats[[k + nc]] <- as.data.frame(mats[[k + nc]],
                                          stringsAsFactors = FALSE)
          mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
          mats[[k + nc]][1, 1] <- paste0("**", catvars[k], "**")
          if(any(is.na(dat[, catvars[[k]]])) == FALSE) {

            for(l in 1:length(unique(dat[, catvars[[k]]]))) {

              mats[[k + nc]][l + 1, 1] <- paste(
                levels(as.factor(dat[, catvars[[k]]]))[l])
            }

          }

          else if(any(is.na(dat[, catvars[[k]]])) == TRUE) {

            for(l in 1:(length(unique(dat[, catvars[[k]]])) - 1)) {
              mats[[k + nc]][l + 1, 1] <- paste(levels(as.factor(
                dat[, catvars[[k]]]))[l])
            }

            mats[[k + nc]][length(unique(dat[, catvars[[k]]])) + 1,
                           1] <- 'NA'
          }

        }

      }

      mats <- do.call(rbind, mats)
      colnames(mats)[1:2] <- c('**Variable**', paste0('**Overall (n = ', nrow(dat), ")**"))
      return(mats)
    }

    else if(!is.null(byvar)) {

      if(pval == FALSE) {

        mats <- vector('list', length(contvars) + length(catvars))
        nc <- length(contvars)

        if(!is.null(contvars)) {

          for(k in 1:nc) {

            if(anyNA(dat[, contvars[[k]]]) == FALSE) {

              mats[[k]] <- matrix(NA, nrow = 1,
                                  ncol = length(
                                    unique(dat[, byvar])) + 2)
              y <- round(summary(dat[, contvars[[k]]]), 1)
              x <- tapply(dat[, contvars[[k]]], dat[, byvar],
                          summary)
              for(j in 1:length(unique(dat[, byvar]))) {

                mats[[k]][, 2] <- paste0(y["Median"], " (",
                                         y[vals[[1]]], ", ",
                                         y[vals[[2]]], ")")
                mats[[k]][, j + 2] <- paste0(
                  round(sapply(x, "[" , "Median")[j], 1), " (",
                  round(sapply(x, "[", vals[[1]])[j], 1), ", ",
                  round(sapply(x, "[", vals[[2]])[j], 1), ")")
              }

              mats[[k]] <- as.data.frame(mats[[k]],
                                         stringsAsFactors = FALSE)
              mats[[k]][, 1] <- as.character(mats[[k]][, 1])
              mats[[k]][1, 1] <- paste0("**", contvars[k], "**")
            }

            else if(anyNA(dat[, contvars[[k]]]) == TRUE) {

              mats[[k]] <- matrix(NA, nrow = 2,
                                  ncol = length(
                                    unique(dat[, byvar])) + 2)
              y <- round(summary(dat[, contvars[[k]]]), 1)
              x <- tapply(dat[, contvars[[k]]], dat[, byvar],
                          summary)
              for(j in 1:length(unique(dat[, byvar]))) {

                mats[[k]][1, 2] <- paste0(y["Median"], " (",
                                          y[vals[[1]]], ", ",
                                          y[vals[[2]]], ")")
                mats[[k]][2, 2] <- y["NA's"]
                mats[[k]][1, j + 2] <- paste0(
                  round(sapply(x, "[" , "Median")[j], 1), " (",
                  round(sapply(x, "[", vals[[1]])[j], 1), ", ",
                  round(sapply(x, "[", vals[[2]])[j], 1), ")")
                mats[[k]][2, j + 2] <- sapply(x, "[", "NA's")[j]
              }

              mats[[k]] <- as.data.frame(mats[[k]],
                                         stringsAsFactors = FALSE)
              mats[[k]][, 1] <- as.character(mats[[k]][, 1])
              mats[[k]][1, 1] <- paste0("**", contvars[k], "**")
              mats[[k]][2, 1] <- "N missing"
            }

          }

        }

        if(!is.null(catvars)) {

          for(k in 1:length(catvars)) {

            if(is.factor(dat[, catvars[[k]]]) == TRUE) {
              dat[, catvars[[k]]] <- factor(dat[, catvars[[k]]])}

            mats[[k + nc]] <- matrix(
              '', nrow = length(unique(dat[, catvars[[k]]])) + 1,
              ncol = length(unique(dat[, byvar])) + 2)
            y1 <- table(dat[, catvars[[k]]], useNA = 'ifany')
            x1 <- table(dat[, catvars[[k]]], dat[, byvar],
                        useNA = 'ifany')

            if(col == TRUE) {

              y2 <- prop.table(y1)
              x2 <- prop.table(x1, 2)
              for(j in 1:length(unique(dat[, byvar]))) {

                for(i in 1:length(unique(dat[, catvars[[k]]]))) {

                  mats[[k + nc]][i + 1, 2] <- paste0(
                    y1[i], " (", round(y2[i] * 100, 1), ")")
                  mats[[k + nc]][i + 1, j + 2] <- paste0(
                    x1[i, j], " (", round(x2[i, j] * 100,1),
                    ")")
                }

              }

            }

            else if(col == FALSE) {

              x2 <- prop.table(x1, 1)
              for(j in 1:length(unique(dat[, byvar]))) {

                for(i in 1:length(unique(dat[, catvars[[k]]]))) {

                  mats[[k + nc]][i + 1, 2] <- y1[i]
                  mats[[k + nc]][i + 1, j + 2] <- paste0(
                    x1[i, j], " (", round(x2[i, j] * 100,1),
                    ")")
                }

              }

            }

            mats[[k + nc]] <- as.data.frame(mats[[k + nc]],
                                            stringsAsFactors = FALSE)
            mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
            mats[[k + nc]][1, 1] <- paste0("**", catvars[k], "**")
            if(any(is.na(dat[, catvars[[k]]])) == FALSE) {

              for(l in 1:length(unique(dat[, catvars[[k]]]))) {

                mats[[k + nc]][l + 1, 1] <- paste(
                  levels(as.factor(dat[, catvars[[k]]]))[l])
              }

            }

            else if(any(is.na(dat[, catvars[[k]]])) == TRUE) {

              for(l in 1:(length(unique(dat[, catvars[[k]]])) - 1)) {

                mats[[k + nc]][l + 1, 1] <- paste(
                  levels(as.factor(dat[, catvars[[k]]]))[l])
              }

              mats[[k + nc]][length(
                unique(dat[, catvars[[k]]])) + 1, 1] <- 'NA'
            }

          }

        }

        mats <- do.call(rbind, mats)
        colnames(mats)[1:2] <- c('**Variable**', paste0('**Overall (n = ', nrow(dat), ")**"))
        for(m in 1:length(unique(dat[, byvar]))) {
          colnames(mats)[m + 2] <- paste0(
            "**", levels(as.factor(dat[, byvar]))[m], " (n = ",
            table(dat[, byvar], useNA = 'ifany')[m], ")**")
        }

        return(mats)
      }

      else if(pval == TRUE) {

        mats <- vector('list', length(contvars) + length(catvars))
        nc <- length(contvars)

        if(!is.null(contvars)) {

          for(k in 1:nc) {

            if(anyNA(dat[, contvars[[k]]]) == FALSE) {

              mats[[k]] <- matrix(NA, nrow = 1,
                                  ncol = length(
                                    unique(dat[, byvar])) + 3)
              y <- round(summary(dat[, contvars[[k]]]), 1)
              x <- tapply(dat[, contvars[[k]]], dat[, byvar],
                          summary)
              for(j in 1:length(unique(dat[, byvar]))) {

                mats[[k]][1, 2] <- paste0(y["Median"], " (",
                                          y[vals[[1]]], ", ",
                                          y[vals[[2]]], ")")
                mats[[k]][1, j + 2] <- paste0(
                  round(sapply(x, "[", "Median")[j], 1), " (",
                  round(sapply(x, "[", vals[[1]])[j], 1), ", ",
                  round(sapply(x, "[", vals[[2]])[j], 1), ")")
              }

              mats[[k]][1, ncol(mats[[k]])] <- round(
                stats::kruskal.test(dat[, contvars[[k]]] ~ as.factor(
                  dat[, byvar]))$p.value, 3)
              mats[[k]] <- as.data.frame(mats[[k]],
                                         stringsAsFactors = FALSE)
              mats[[k]][, 1] <- as.character(mats[[k]][, 1])
              mats[[k]][1, 1] <- paste0("**", contvars[k], "**")
            }

            else if(anyNA(dat[,contvars[[k]]]) == TRUE) {

              mats[[k]] <- matrix(NA, nrow = 2, ncol = length(
                unique(dat[, byvar])) + 3)
              y <- round(summary(dat[, contvars[[k]]]), 1)
              x <- tapply(dat[, contvars[[k]]], dat[, byvar],
                          summary)
              for(j in 1:length(unique(dat[, byvar]))) {

                mats[[k]][1, 2] <- paste0(y["Median"], " (",
                                          y[vals[[1]]], ", ",
                                          y[vals[[2]]], ")")
                mats[[k]][2, 2] <- y["NA's"]
                mats[[k]][1, j + 2] <- paste0(
                  round(sapply(x, "[" , "Median")[j], 1), " (",
                  round(sapply(x, "[", vals[[1]])[j], 1), ", ",
                  round(sapply(x, "[", vals[[2]])[j], 1), ")")
                mats[[k]][2, j + 2] <- sapply(x, "[", "NA's")[j]
              }

              mats[[k]][1, ncol(mats[[k]])] <- tryCatch({
                round(stats::kruskal.test(
                  dat[, contvars[[k]]] ~
                    as.factor(dat[, byvar]))$p.value, 3)
              }, error = function(e) {
                NA
              })

              mats[[k]] <- as.data.frame(mats[[k]],
                                         stringsAsFactors = FALSE)
              mats[[k]][, 1] <- as.character(mats[[k]][, 1])
              mats[[k]][1, 1] <- paste0("**", contvars[k], "**")
              mats[[k]][2, 1] <- "N missing"
              mats[[k]][, ncol(mats[[k]])] <- as.character(
                mats[[k]][, ncol(mats[[k]])])
              mats[[k]][2, ncol(mats[[k]])] <- ''
            }

          }

        }

        if(!is.null(catvars)) {

          for(k in 1:length(catvars)) {

            if(is.factor(dat[, catvars[[k]]]) == TRUE) {
              dat[, catvars[[k]]] <- factor(dat[, catvars[[k]]])}

            mats[[k + nc]] <- matrix(
              '', nrow = length(unique(dat[, catvars[[k]]])) + 1,
              ncol = length(unique(dat[, byvar])) + 3)
            y1 <- table(dat[, catvars[[k]]], useNA = 'ifany')
            x1 <- table(dat[, catvars[[k]]], dat[, byvar],
                        useNA = 'ifany')

            if(col == TRUE) {

              y2 <- prop.table(y1)
              x2 <- prop.table(x1, 2)
              for(j in 1:length(unique(dat[, byvar]))) {

                for(i in 1:length(unique(dat[, catvars[[k]]]))) {

                  mats[[k + nc]][i + 1, 2] <- paste0(
                    y1[i], " (", round(y2[i] * 100, 1), ")")
                  mats[[k + nc]][i + 1, j + 2] <- paste(
                    x1[i, j], " (", round(x2[i, j] * 100, 1),
                    ")", sep = '')
                }

              }

            }

            else if(col == FALSE) {

              x2 <- prop.table(x1, 1)
              for(j in 1:length(unique(dat[, byvar]))) {

                for(i in 1:length(unique(dat[, catvars[[k]]]))) {

                  mats[[k + nc]][i + 1, 2] <- y1[i]
                  mats[[k + nc]][i + 1, j + 2] <- paste(
                    x1[i, j], " (", round(x2[i, j] * 100, 1),
                    ")", sep = '')
                }

              }

            }

            if(fisher == TRUE) {

              mats[[k + nc]][1, ncol(mats[[k + nc]])] <- tryCatch(
                round(stats::fisher.test(dat[, catvars[[k]]],
                                  dat[, byvar])$p.value, 3),
                warning = function(w) {return(NA)},
                error = function(e) {return(NA)})
            }

            else if(fisher == FALSE) {

              mats[[k + nc]][1, ncol(mats[[k + nc]])] <- tryCatch(
                round(stats::chisq.test(dat[, catvars[[k]]],
                                 dat[, byvar])$p.value, 3),
                warning = function(w) {return(NA)},
                error = function(e) {return(NA)})
            }

            mats[[k + nc]] <- as.data.frame(mats[[k + nc]],
                                            stringsAsFactors = FALSE)
            mats[[k + nc]][, 1] <- as.character(mats[[k + nc]][, 1])
            mats[[k + nc]][1, 1] <- paste0("**", catvars[k], "**")
            if(any(is.na(dat[, catvars[[k]]])) == FALSE) {

              for(l in 1:length(unique(dat[, catvars[[k]]]))) {

                mats[[k + nc]][l + 1, 1] <- paste(
                  levels(as.factor(dat[, catvars[[k]]]))[l])
              }

            }

            else if(any(is.na(dat[, catvars[[k]]])) == TRUE) {

              for(l in 1:(length(unique(dat[, catvars[[k]]])) - 1)) {

                mats[[k + nc]][l + 1, 1] <- paste(
                  levels(as.factor(dat[, catvars[[k]]]))[l])
              }

              mats[[k + nc]][length(
                unique(dat[, catvars[[k]]])) + 1, 1] <- 'NA'
            }

          }

        }

        mats <- do.call(rbind, mats)
        colnames(mats)[1] <- '**Variable**'
        colnames(mats)[2] <- paste0('**Overall (n = ', nrow(dat), ")**")
        colnames(mats)[ncol(mats)] <- '**p-value**'
        mats$"**p-value**"[mats$"**p-value**" == '0'] <- "<.001"
        for(m in 1:length(unique(dat[, byvar]))) {
          colnames(mats)[m + 2] <- paste0(
            "**", levels(as.factor(dat[, byvar]))[m], " (n = ",
            table(dat[, byvar], useNA = 'ifany')[m], ")**")
        }

        return(mats)
      }

    }

}

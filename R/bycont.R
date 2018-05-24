#' Table of one or more categorical variables by a single continuous variable
#'
#' \code{bycont} takes a list of categorical variabls and returns
#' median(min, max) of the single continuous variable within each level
#' of each categorical variable. Computes Kruskall-Wallis p-values.
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param catvars is a list of the categorical variables for the rows of the
#' table e.g. list('Gene1', 'Gene2')
#' @param contvar is the continuous variable you want summarized by each
#' categorical variable. Must be in quotes.
#' @param dat is the dataset to use for analysis
#' @param pval takes the value TRUE or FALSE indicating whether p-values
#' should be computed. Defaults to TRUE. When TRUE, Kruskal-Wallis p-values
#' are produced.
#'
#' @return Returns a dataframe
#'
#' @export
#'

bycont <- function(catvars, contvar, dat, pval = TRUE){

  dat <- as.data.frame(dat)

  if(pval == FALSE) {

    mats <- vector('list', length(catvars))

    for(k in 1:length(catvars)) {

      mats[[k]] <- matrix('', nrow = length(
        unique(dat[, catvars[[k]]])) + 1, ncol = 3)
      summ <- tapply(dat[, contvar], dat[, catvars[[k]]], summary)
      for(j in 1:length(unique(dat[, catvars[[k]]]))) {

        mats[[k]][j + 1, 2] <- paste0(
          sapply(summ, "[", 3)[j], " (", sapply(summ, "[", 1)[j], ", ",
          sapply(summ, "[", 6)[j], ")")
        mats[[k]][j + 1, 3] <- sapply(summ, "[", 7)[j]
      }

      mats[[k]] <- as.data.frame(mats[[k]])
      mats[[k]][, 1] <- as.character(mats[[k]][, 1])
      mats[[k]][1, 1] <- paste0("**", catvars[k], "**")

      if(any(is.na(dat[, catvars[[k]]])) == FALSE) {
        for(l in 1:length(unique(dat[, catvars[[k]]]))) {
          mats[[k]][l+1, 1] <- paste(
            levels(as.factor(dat[, catvars[[k]]]))[l])
        }

      } else if(any(is.na(dat[, catvars[[k]]])) == TRUE) {

        for(l in 1:(length(unique(dat[,catvars[[k]]])) - 1)) {

          mats[[k]][l+1, 1] <- paste(levels(as.factor(dat[, catvars[[k]]]))[l])
        }

        mats[[k]][length(unique(dat[,catvars[[k]]])) + 1, 1] <- 'NA'
      }

    }

    mats <- do.call(rbind, mats)
    colnames(mats) <- c('**Variable**', '**Median (Min, Max)**', '**N NA**')
    return(mats)

  } else if(pval == TRUE) {

    mats <- vector('list', length(catvars))
    for(k in 1:length(catvars)) {

      mats[[k]] <- matrix('', nrow = length(unique(dat[, catvars[[k]]])) + 1,
                          ncol = 4)
      summ <- tapply(dat[, contvar], dat[, catvars[[k]]], summary)

      for(j in 1:length(unique(dat[, catvars[[k]]]))) {
        mats[[k]][j + 1, 2] <- paste0(
          sapply(summ, "[", 3)[j], " (", sapply(summ, "[", 1)[j], ", ",
          sapply(summ, "[", 6)[j], ")")
        mats[[k]][j + 1, 3] <- sapply(summ, "[", 7)[j]

      }
      mats[[k]][1, 4] <- round(
        kruskal.test(dat[, contvar] ~
                       as.factor(dat[, catvars[[k]]]))$p.value, 3)
      mats[[k]] <- as.data.frame(mats[[k]])
      mats[[k]][, 1] <- as.character(mats[[k]][, 1])
      mats[[k]][1, 1] <- paste0("**", catvars[k], "**")

      if(any(is.na(dat[, catvars[[k]]])) == FALSE) {

        for(l in 1:length(unique(dat[, catvars[[k]]]))) {
          mats[[k]][l+1, 1] <- paste(
            levels(as.factor(dat[, catvars[[k]]]))[l])
        }

      } else if(any(is.na(dat[, catvars[[k]]])) == TRUE) {

        for(l in 1:(length(unique(dat[,catvars[[k]]])) - 1)) {

          mats[[k]][l+1, 1] <- paste(levels(as.factor(dat[, catvars[[k]]]))[l])
        }

        mats[[k]][length(unique(dat[,catvars[[k]]])) + 1, 1] <- 'NA'
      }

    }

    mats <- do.call(rbind, mats)
    colnames(mats) <- c('**Variable**', '**Median (Min, Max)**',
                        '**N NA**', '**p-value**')
    return(mats)
  }
}

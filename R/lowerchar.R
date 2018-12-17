#' Convert to lowercase
#'
#' \code{lowerchar} converts the levels of character variables from upper or mixed
#' case to lower case
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param dfname is the name of the dataframe on which to perform the action
#'
#' @return Nothing is returned from \code{lowerchar}, the action is simply perfomed on the columns of
#' dataframe \code{dfname}
#'
#' @export lowerchar
#'

lowerchar <- function(dfname) {
  data.frame(lapply(dfname, function(v) {
    if (is.character(v)) {
      return(trimws(tolower(v)))
    } else {
      return(v)
    }
  }), stringsAsFactors = F)
}

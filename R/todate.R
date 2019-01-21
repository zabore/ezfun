#' Convert to date format
#'
#' \code{todate} converts any POSIXct format variables in the dataframe to date format
#'
#' Note that this function will mainly apply to dataframes imported using the \code{read_excel} function
#' from the \code{readxl} package. Dataframes imported using, for example, \code{read.csv} instead
#' will have dates in character format and therefore \code{todate} will not apply.
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param dfname is the name of the dataframe on which to perform the action
#'
#' @return Nothing is returned from \code{todate}, the action is simply perfomed on the columns of
#' dataframe \code{dfname}
#'
#' @export
#'

todate <- function(dfname) {
  data.frame(lapply(dfname, function(v) {
    if (inherits(v, "POSIXct")) {
      return(as.Date(v))
    } else {
      return(v)
    }
  }), stringsAsFactors = F)
}

#' Calculate age in years based on two dates and their respective formats
#'
#' \code{compute_age} takes two dates, and optionally two date
#' formats as arguments, and returns a whole number age
#'
#' @author Emily C Zabor \email{zabore@@mskcc.org}
#'
#' @param start_date is the name of the starting date variable,
#' supplied in quotes, e.g. "birth_date"
#' @param end_date is the name of the ending date variable,
#' supplied in quotes, e.g. "surgery_date"
#' @param data is the dataframe in which to find the start and
#' end dates
#' @param start_format is the format of the character variable
#' stored in \code{start_date}. The variable will be temporarily
#' formatted as a date to perform the calculation using
#' \code{as.Date()}. Defaults to "%m/%d/%Y".
#' Must be supplied in quotes.
#' #' @param end_format is the format of the character variable
#' stored in \code{end_date}. The variable will be temporarily
#' formatted as a date to perform the calculation using
#' \code{as.Date()}. Defaults to "%m/%d/%Y".
#' Must be supplied in quotes
#'
#' @return Returns a continuous integer valued age in years
#'
#' @export

compute_age <- function(start_date,
                        end_date,
                        data,
                        start_format = "%m/%d/%Y",
                        end_format = "%m/%d/%Y") {
  dt1 <- as.Date(data[, start_date], format = start_format)
  dt2 <- as.Date(data[, end_date], format = end_format)

  floor(as.numeric(difftime(dt2, dt1, units = "days")) / 365.25)
}

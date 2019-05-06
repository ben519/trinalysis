#' @title
#' End of Month
#'
#' @description
#' Return the first day of the month for each date in a vector of dates
#'
#' @details
#' Returns the first day of the month for each date in a vector of dates
#'
#' @param dates A vector of dates
#'
#' @export
#'
#' @examples
#' fom(as.Date("2016-2-14"))
#' fom(seq.Date(as.Date("2015-1-1"), as.Date("2015-12-31"), by="month"))

fom <- function(dates, p = as.POSIXlt(dates)){
  # Returns the first day of the month for each Date in dates

  p$mday <- 1
  return(as.Date(p))
}

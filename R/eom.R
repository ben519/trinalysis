#' @title
#' End of Month
#'
#' @description
#' Return the last day of the month for each date in a vector of dates
#'
#' @details
#' Returns the last day of the month for each date in a vector of dates
#'
#' @param dates A vector of dates
#'
#' @export
#'
#' @examples
#'
#' eom(as.Date("2016-2-14"))
#' eom(seq.Date(as.Date("2015-1-1"), as.Date("2015-12-31"), by="month"))

eom <- function(dates){
  # Returns the last day of the month for each Date in dates

  pDates <- as.POSIXlt(dates)
  return(as.Date(modifyList(pDates, list(mon=pDates$mon + 1, mday=0))))
}

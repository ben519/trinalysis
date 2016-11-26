#' @title
#' Triangle Skeleton
#'
#' @description
#' Generate the dates needed to build a triangle analysis
#'
#' @details
#' Returns a data.table with columns {LeftOrigin, RightOrigin, ValuationDate, Age}
#'
#' @param minLeftOrigin When should the first origin period (potentially) start? (Depends on \code{fromMinLeftOrigin})
#' @param fromMinLeftOrigin Should the first origin period start on exactly \code{minLeftOrigin}? If \code{FALSE} the last
#' valuation date will be exactly \code{lastValuationDate}
#' @param originLength The length of each origin period in months
#' @param rowDev Number of months between rows
#' @param colDev Number of months between columns
#' @param initialAge At what age should the first valuation period start?
#' @param lastValuationDate When whould the last valuation date (potentially) be? (Depends on \code{fromMinLeftOrigin})
#'
#' @importFrom lubridate rollback
#' @importFrom lubridate %m+%
#' @importFrom lubridate %m-%
#' @import data.table
#' @export
#'
#' @examples
#' triangle_skeleton(as.Date("2014-1-1"))
#' eom(seq.Date(as.Date("2015-1-1"), as.Date("2015-12-31"), by="month"))

triangle_skeleton <- function(minLeftOrigin, fromMinLeftOrigin=FALSE, originLength=12, rowDev=12, colDev=12, initialAge=originLength, lastValuationDate=Sys.Date()){
  # Returns a table of skeleton parameters that define a set of triangles

  # Input corrections
  minLeftOrigin <- rollback(minLeftOrigin) + 1  # Force minLeftOrigin to be a first-of-month
  lastValuationDate <- rollback((rollback(lastValuationDate) + 1) %m+% months(1))  # Force the lastValuationDate to be an end-of-month

  # Generate all leftOrigins
  if(fromMinLeftOrigin){
    leftOrigins <- seq(minLeftOrigin, (lastValuationDate + 1) %m-% months(initialAge), by=paste(rowDev, "months"))
  } else{
    leftOrigins <- rev(seq((lastValuationDate + 1) %m-% months(initialAge), minLeftOrigin, by=paste(-rowDev, "months")))
  }

  # Generate all rightOrigins
  rightOrigins <- leftOrigins %m+% months(originLength) - 1

  # Helper method to get all valuation dates given (rightOrigin, originLength, initialAge, lastValuationDate, colDev)
  getValDts <- function(rightOrigin, originLength, initialAge, lastValuationDate, colDev){
    seq((as.Date(rightOrigin) + 1) %m-% months(originLength-initialAge), lastValuationDate + 1, by=paste(colDev,"months")) - 1
  }

  skeleton <- data.table(LeftOrigin=leftOrigins, RightOrigin=rightOrigins, lastValuationDate=lastValuationDate, colDev=colDev, originLength=originLength, initialAge=initialAge)
  skeleton <- skeleton[, list(ValuationDate=getValDts(rightOrigin=RightOrigin, originLength=originLength, initialAge=initialAge, lastValuationDate=lastValuationDate, colDev=colDev)), keyby=list(LeftOrigin, RightOrigin)]
  skeleton[, Age := year(ValuationDate)*12 + month(ValuationDate)-(year(LeftOrigin)*12 + month(LeftOrigin)) + 1]
  setkey(skeleton, "LeftOrigin", "RightOrigin", "ValuationDate")

  return(skeleton[])
}

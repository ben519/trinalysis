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
#' @param method One of {"static", "dynamic"}. If static, Age x is relative to the beginning of the cohort. If dynamic,
#' Age x is relative to the acquisition date of each customer
#'
#' @importFrom lubridate rollback
#' @importFrom lubridate %m+%
#' @importFrom lubridate %m-%
#' @import data.table
#' @export
#'
#' @examples
#' triangle_skeleton(as.Date("2014-1-1"))

triangle_skeleton <- function(minLeftOrigin, fromMinLeftOrigin = FALSE, originLength = 12, rowDev = 12, colDev = 12,
                              initialAge = originLength, lastValuationDate = Sys.Date(), method = "static"){
  # Returns a table of skeleton parameters that define a set of triangles

  # Input corrections
  minLeftOrigin <- fom(minLeftOrigin)  # Force minLeftOrigin to be a first-of-month
  lastValuationDate <- eom(lastValuationDate)  # Force the lastValuationDate to be an end-of-month

  # Generate all leftOrigins
  if(fromMinLeftOrigin){
    leftOrigins <- seq(minLeftOrigin, (lastValuationDate + 1) %m-% months(initialAge), by=paste(rowDev, "months"))
  } else{
    leftOrigins <- rev(seq((lastValuationDate + 1) %m-% months(initialAge), minLeftOrigin, by=paste(-rowDev, "months")))
  }
  if(length(leftOrigins) == 0) stop("Not enough data for the given parameters")

  # Generate all rightOrigins
  rightOrigins <- leftOrigins %m+% months(originLength) - 1

  if(method == "dynamic"){
    # Remove immature cohorts
    removeCohorts <- which((rightOrigins + 1) %m+% months(initialAge) - 1 > lastValuationDate)
    leftOrigins <- leftOrigins[-removeCohorts]
    rightOrigins <- rightOrigins[-removeCohorts]
  }

  skeleton <- data.table(
    LeftOrigin=leftOrigins,
    RightOrigin=rightOrigins,
    lastValuationDate=lastValuationDate,
    colDev=colDev,
    originLength=originLength,
    initialAge=initialAge
  )

  val_dates <- function(originDate, initialAge, colDev, lastValuationDate){
    length.out <- ceiling((lastValuationDate - originDate)/28)
    valDts <- originDate %m+% months(seq(from = initialAge, by = colDev, length.out = length.out)) - 1
    valDts <- valDts[valDts <= lastValuationDate]
    return(valDts)
  }

  if(method == "static"){
    skeleton <- skeleton[, list( ValuationDate = val_dates(
      originDate = LeftOrigin, initialAge = initialAge, colDev = colDev, lastValuationDate = lastValuationDate
    )), by = list(LeftOrigin, RightOrigin, lastValuationDate, colDev, originLength, initialAge)]
    skeleton[, Age := seq(from = initialAge[1L], by = colDev, length.out = .N), by = list(LeftOrigin, RightOrigin)]
    skeleton[, c("colDev", "originLength", "initialAge", "lastValuationDate") := NULL]
    setcolorder(skeleton, c("LeftOrigin", "RightOrigin", "ValuationDate", "Age"))
  } else if(method == "dynamic"){
    skeleton <- skeleton[, list(RightValuationDate = val_dates(
      originDate = RightOrigin, initialAge = initialAge, colDev = colDev, lastValuationDate = lastValuationDate
    )), by = list(LeftOrigin, RightOrigin, lastValuationDate, colDev, originLength, initialAge)]
    skeleton[, Age := seq(from = initialAge[1L], by = colDev, length.out = .N), by = list(LeftOrigin, RightOrigin)]
    skeleton[, LeftValuationDate := LeftOrigin %m+% months(Age) - 1]
    skeleton[, c("colDev", "originLength", "initialAge", "lastValuationDate") := NULL]
    setcolorder(skeleton, c("LeftOrigin", "RightOrigin", "LeftValuationDate", "RightValuationDate", "Age"))
  }

  return(skeleton[])
}

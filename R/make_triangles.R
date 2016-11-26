#' @title
#' Make Triangles
#'
#' @description
#' Convert triangles in tall format to a list of triangles in triangular format
#'
#' @details
#' Returns a list of data.table objects
#'
#' @param transactions.cmltv A data.table of cumulative transaction valuations (result of calling cumulate_transactions())
#' @param format How should the triangles be returned? Either "tall" (a data.table) or "triangular" (a list of data.tables)
#' @param minLeftOrigin See ?triangle_skeleton
#' @param originLength See ?triangle_skeleton
#' @param rowDev See ?triangle_skeleton
#' @param colDev See ?triangle_skeleton
#' @param lastValuationDate See ?triangle_skeleton
#' @param fromMinLeftOrigin See ?triangle_skeleton
#' @param initialAge See ?triangle_skeleton
#' @param financialCols What financial columns in \code{transactions.cmltv} should generate triangles? Default="auto" guesses
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' set.seed(2357)
#' transactions <- sample_transactions(3, minDate=as.Date("2010-1-1"), maxDate=as.Date("2015-12-31"))
#' transactions.cmltv <- cumulate_transactions(transactions, financialCols="Amount")
#' make_triangles(transactions.cmltv)  # guess the financial columns
#' make_triangles(transactions.cmltv, financialCols=c("Transactions.cmltv"))  # specify the financial columns

make_triangles <- function(transactions.cmltv, format="triangular",
                           minLeftOrigin=as.Date(paste0(min(year(transactions.cmltv$FirstValuationDate)), "-1-1")),
                           originLength=12, rowDev=12, colDev=12, lastValuationDate=max(transactions.cmltv$ValuationDate),
                           fromMinLeftOrigin=TRUE, initialAge=originLength, financialCols="auto", verbose=FALSE){
  # Method to build triangles from a cumulative transactions dataset (result of calling cumulate_transactions())
  # format can be one of {"tall", "triangular"}
  # If "tall", a single data.table is returned
  # If "triangular", a list of triangle objects is returned
  # financialCols should be a character vector corresponding to cumulative-valued columns of transactions.cmltv for which to
  # generate triangles (in addition to the guaranteed triangles {ActiveCustomers, NewCustomers, NewCustomers.cmltv}). If "auto",
  # financialCols will look for numeric columns whose name ends in ".cmltv"

  # Get financialCols
  if(financialCols == "auto"){
    numeric_cols <- colnames(transactions.cmltv)[sapply(transactions.cmltv, is.numeric)]
    financialCols <- numeric_cols[grepl("\\.cmltv$", numeric_cols)]
  }

  # Get the triangle skeletons
  if(verbose) print("Getting triangle skeletons")
  params <- triangle_skeleton(minLeftOrigin=minLeftOrigin, originLength=originLength, rowDev=rowDev, colDev=colDev,
                              lastValuationDate=lastValuationDate, fromMinLeftOrigin=fromMinLeftOrigin, initialAge=initialAge)

  # Helper method to subset customers into a row and then partition transactions in that row and aggregate them
  rowPartitionSums <- function(valDts, leftO, rightO){
    # For testing:
    # leftO <- params$LeftOrigin[1]; rightO <- params$RightOrigin[1]
    # valDts <- params[LeftOrigin==leftO & RightOrigin==rightO]$ValuationDate

    # Get the group of customers in the row of the triangle defined by leftO and rightO
    transactions.cmltv.subset <- transactions.cmltv[FirstValuationDate >= leftO & FirstValuationDate <= rightO]

    # If there are no customers in this row, fill in the data as necessary
    if(nrow(transactions.cmltv.subset) == 0){

      # Build a table with the primary columns
      primary <- data.table(ValuationDate=valDts, ActiveCustomers=0L, NewCustomers=0L, NewCustomers.cmltv=0L)

      if(length(financialCols) > 0){
        extra.cmltv <- transactions.cmltv.subset[, financialCols, with=FALSE]
        extra.cmltv <- extra.cmltv[, lapply(.SD, function(x) rep(ifelse(class(x) == "integer", 0L, 0), length(valDts)))]

        # Build a table with the extra non cumulative columns
        extra.nonCmltv <- copy(extra.cmltv)
        setnames(extra.nonCmltv, gsub("\\..*","",colnames(extra.cmltv))) # remove .cmltv from the column names

        # Build a table with all the extra columns
        extra <- cbind(extra.cmltv, extra.nonCmltv)

        # Combine the primary and extra tables into one
        result <- cbind(primary, extra)

        # Set the column order of result
        setcolorder(result, c(colnames(primary), sort(colnames(extra))))

      } else{
        result <- primary

        # Set the column order of result
        setcolorder(result, colnames(primary))
      }

      return(result)
    }

    # Build a table to partition the data by CustomerID and ValuationDate
    partitioner <- CJ(CustomerID=unique(transactions.cmltv.subset$CustomerID), ValuationDate=valDts)

    # Add the Partition Numbers for each customer (used in calculating "active" customers)
    partitioner[, PNum:=seq_along(ValuationDate), by=CustomerID]

    # For each row in transactions.cmltv.subset get the nearest partition number via a backward rolling join from partitioner
    # to transactions.cmltv.subset
    setkey(partitioner, "CustomerID", "ValuationDate")
    setkey(transactions.cmltv.subset, "CustomerID", "ValuationDate")
    backwardjoin <- partitioner[transactions.cmltv.subset, roll=-Inf]

    # Partition the data via a forward rolling join from transactions.cmltv.subset to partitioner
    forwardjoin <- backwardjoin[partitioner, roll=TRUE]

    # Aggregate results
    expr <- "ActiveCustomers=sum(PNum == i.PNum, na.rm=TRUE), NewCustomers.cmltv=sum(!is.na(PNum))"
    if(length(financialCols) > 0)
      expr <- paste(expr, ",", paste0(financialCols, "=sum(", financialCols, ", na.rm=TRUE)", collapse=", "))
    expr <- paste("list(", expr, ")")
    result <- forwardjoin[, eval(parse(text=expr)), by=ValuationDate]

    # Build the non-cumulative columns
    nonCmltv <- result[, !c("ValuationDate", "ActiveCustomers"), with=FALSE]
    nonCmltv <- nonCmltv[, lapply(.SD, function(x) c(x[1], tail(x,-1) - head(x,-1)))]
    setnames(nonCmltv, gsub("\\..*","",colnames(nonCmltv))) # remove .cmltv from the column names

    # Join result and nonCmltv tables
    result <- cbind(result, nonCmltv)

    # Set the column order of result
    guaranteedCols <- c("ValuationDate", "ActiveCustomers", "NewCustomers", "NewCustomers.cmltv")
    setcolorder(result, c(guaranteedCols, sort(setdiff(colnames(result), guaranteedCols))))

    return(result)
  }

  # For each (LeftOrigin, RightOrigin) pair, partition and aggregate the transactions by the ValuationDate column
  if(verbose) print("Building triangle data.table")
  triangleDT <- params[, c(rowPartitionSums(ValuationDate, LeftOrigin[1], RightOrigin[1]), Age=list(Age)),
                       by=list(LeftOrigin, RightOrigin)]

  # Change the column order so that Age comes after ValuationDate
  setcolorder(triangleDT, unique(c("LeftOrigin", "RightOrigin", "ValuationDate", "Age", colnames(triangleDT))))

  # If format == "triangular", return a list of triangle objects. Otherwise return triangleDT
  if(verbose) print("Building triangle list")
  if(format=="triangular") return(tall_to_triangular(triangleDT)) else return(triangleDT)
}

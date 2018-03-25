#' @title
#' Make Triangles
#'
#' @description
#' Convert triangles in tall format to a list of triangles in triangular format
#'
#' @details
#' Returns a list of data.table objects
#'
#' @param transactions A data.table of with at least columns {TransactionID, TransactionDate, CustomerID}
#' @param format How should the triangles be returned? Either "tall" (a data.table) or "triangular" (a list of data.tables)
#' @param minLeftOrigin See ?triangle_skeleton
#' @param originLength See ?triangle_skeleton
#' @param rowDev See ?triangle_skeleton
#' @param colDev See ?triangle_skeleton
#' @param lastValuationDate See ?triangle_skeleton
#' @param fromMinLeftOrigin See ?triangle_skeleton
#' @param initialAge See ?triangle_skeleton
#' @param colsFinancial What financial columns in \code{transactions} should generate triangles? Default="auto" guesses
#' @param verbose Should progress details be displayed?
#' @param colCustomerID Name of column representing customer id. Default="CustomerID"
#' @param colTransactionDate Name of column representing transaction date Default="TransactionDate"
#' @param method One of {"static", "dynamic"}. If static, Age x is relative to the beginning of the cohort. If dynamic,
#' Age x is relative to the acquisition date of each customer
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' set.seed(2357)
#' transactions <- sample_transactions(3, minDate=as.Date("2010-1-1"), maxDate=as.Date("2015-12-31"))
#' make_triangles(transactions)
#' make_triangles(transactions, colsFinancial=c("Amount"))
#' make_triangles(transactions, colsFinancial=c("Amount"), method = "dynamic")

make_triangles <- function(transactions, format = "triangular", minLeftOrigin = NULL, originLength = 12, rowDev = 12,
                           colDev = 12, lastValuationDate = NULL, fromMinLeftOrigin = TRUE, initialAge = originLength,
                           colsFinancial = "auto", verbose=FALSE, colCustomerID = "CustomerID",
                           colTransactionDate = "TransactionDate", method = "static"){
  # Method to build triangles from transactions

  # Assign values for minLeftOrigin, lastValuationDate
  if(is.null(minLeftOrigin)) minLeftOrigin <- min(transactions$TransactionDate)
  if(is.null(lastValuationDate)) lastValuationDate <- max(transactions$TransactionDate)

  # Assign values for colsFinancial
  if(colsFinancial[1L] == "auto"){
    numeric_cols <- colnames(transactions)[sapply(transactions, is.numeric)]
    colsFinancial <- setdiff(numeric_cols, c(colCustomerID, colTransactionDate))

    # Exclude columns that end in "id", "num", or "number"
    colsFinancial <- colsFinancial[!stringr::str_detect(stringr::str_to_lower(colsFinancial), "id|number|num$")]
  }

  # Get the triangle skeletons
  if(verbose) print("Getting triangle skeletons")
  skel <- triangle_skeleton(
    minLeftOrigin = minLeftOrigin,
    fromMinLeftOrigin = fromMinLeftOrigin,
    originLength = originLength,
    rowDev = rowDev,
    colDev = colDev,
    initialAge = initialAge,
    lastValuationDate = lastValuationDate,
    method = method
  )

  # Aggregate transactions by (CustomerID, TransactionDate)
  transdaily <- transactions[, list(
    Transactions = .N,
    Amount = sum(Amount, na.rm = T)
  ), keyby = list(CustomerID, TransactionDate)]
  for(col in colsFinancial) set(transdaily, j = col, value = as.numeric(transdaily[[col]]))

  # Determine unique customers, origin periods
  origins <- unique(skel[, list(LeftOrigin, RightOrigin)])
  custs <- transdaily[, list(FirstTransactionDate = min(TransactionDate)), keyby = CustomerID]

  # Loop through the different origin periods
  resultList <- list()
  for(i in seq_len(nrow(origins))){
    origin_i <- origins[i]
    custs_i <- custs[between(FirstTransactionDate, origin_i$LeftOrigin, origin_i$RightOrigin)]
    trans_i <- transdaily[custs_i, on = "CustomerID"]
    skel_i <- skel[origin_i, on = c("LeftOrigin", "RightOrigin")]

    if(nrow(custs_i) == 0){
      result <- skel_i[, list(Age, Transactions = 0, ActiveCustomers = 0)]
      for(col in colsFinancial) set(result, j = col, value = 0)
      result[, eval(parse(text = paste(colsFinancial, " = NULL")))]
      result[, `:=`(LeftOrigin = origin_i$LeftOrigin, RightOrigin = origin_i$RightOrigin)]
      resultList <- c(resultList, list(result[]))
      next
    }

    # Build a table of all (customer, age) pairs
    skel_i[, Join := 1L]
    custs_i[, Join := 1L]
    joinTbl <- skel_i[custs_i, on = "Join", nomatch = 0, allow.cartesian = T]

    # For each (customer, age) set ValuatinoDate
    if(method == "dynamic"){
      joinTbl[, ValuationDate := FirstTransactionDate %m+% months(Age) - 1]
    }

    # Set LB, RB
    joinTbl[, `:=`(
      LB = c(LeftOrigin[1L], head(ValuationDate, -1) + 1L),
      RB = ValuationDate
    ), by = CustomerID]
    joinTbl[, c("LeftOrigin", "RightOrigin", "FirstTransactionDate", "Join") := NULL]

    # Non-Equi Join
    # For every (customer, valuationdate) get all the transactions falling into the period
    # (customer, valuationdate) pairs with no transactions will be retained
    trans_i[, `:=`(CustID = CustomerID)]
    joinedTbl <- trans_i[joinTbl, on = c("CustomerID", "TransactionDate>=LB", "TransactionDate<=RB")]

    # Aggregate the results per (Age)
    strEvalCols <- paste0(colsFinancial, " = sum(", colsFinancial, ", na.rm = T)")
    strEvalCols <- c("Transactions = sum(Transactions, na.rm = T)", "CustsWithoutTransaction = sum(is.na(CustID))", strEvalCols)
    strEvalCols <- paste0(strEvalCols, collapse = ", ")
    strEval <- paste0("list(", strEvalCols, ")")
    result <- joinedTbl[, eval(parse(text = strEval)), keyby = Age]
    result[, `:=`(ActiveCustomers = nrow(custs_i) - CustsWithoutTransaction, CustsWithoutTransaction = NULL)]
    result[, `:=`(LeftOrigin = origin_i$LeftOrigin, RightOrigin = origin_i$RightOrigin)]

    # Append results
    resultList <- c(resultList, list(result[]))
  }

  # Combine result sets
  triangleDT <- rbindlist(resultList, use.names = T)

  # If method = Static, include ValuationDate
  if(method == "static") triangleDT[skel, ValuationDate := i.ValuationDate, on = c("LeftOrigin", "RightOrigin", "Age")]

  # Make cumulative columns
  cmltvCols <- c("Transactions", colsFinancial)
  strEvalCols <- paste0(cmltvCols, ".cmltv = cumsum(", cmltvCols, ")", collapse = ", ")
  strEval <- paste0("`:=`(", strEvalCols, ")")
  triangleDT[, eval(parse(text = strEval)), by = list(LeftOrigin, RightOrigin)]

  # Clean up
  colz <- c("LeftOrigin", "RightOrigin", "ValuationDate", "Age", "ActiveCustomers", "Transactions", "Transactions.cmltv")
  colz <- intersect(unique(c(colz, sort(colnames(triangleDT)))), colnames(triangleDT))
  setcolorder(triangleDT, colz)

  # If format == "triangular", return a list of triangle objects. Otherwise return triangleDT
  if(format=="triangular"){
    return(tall_to_triangular(triangleDT))
  } else{
    return(triangleDT[])
  }
}

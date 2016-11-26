#' @title
#' Transaction Cohorts
#'
#' @description
#' Generate triangles from transactions relative to a specified cohort column
#'
#' @details
#' Returns a list of triangles (possibly a list of lists, if \code{format="triangular"}), where the top level list elements
#' correspond to the unique values of the \code{colCohort} in the transactions dataset
#'
#' @param transactions A data.table of transactions with a column for CustomerID, TransactionDate, and Cohort
#' @param colCohort Name of the column for which to group cohorts
#' @param firstTransactionWRTCohort If TRUE, a single customer can be mapped to many origins - each based on his first transaction
#' within each cohort. If FALSE, each customer has a fixed origin based on his first overall transaction
#' @param allCohort Triangles be returned representing the combined sum of all cohorts?
#' @param format "tall" or "triangular"
#' @param minLeftOrigin See ?triangle_skeleton
#' @param originLength See ?triangle_skeleton
#' @param rowDev See ?triangle_skeleton
#' @param colDev See ?triangle_skeleton
#' @param lastValuationDate See ?triangle_skeleton
#' @param fromMinLeftOrigin See ?triangle_skeleton
#' @param initialAge See ?triangle_skeleton
#' @param verbose Should progress details be displayed?
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' set.seed(2357)
#' transactions <- sample_transactions(10)
#' transaction_cohorts(transactions, colCohort="Product")

transaction_cohorts <- function(transactions, colCohort, colCustomerID="CustomerID", colTransactionDate="TransactionDate",
                                colsFinancial=NULL, firstTransactionWRTCohort=FALSE, allCohort=TRUE, format="triangular",
                                minLeftOrigin=as.Date(paste0(min(year(transactions[[colTransactionDate]])), "-1-1")),
                                originLength=12, rowDev=12, colDev=12, lastValuationDate=max(transactions[[colTransactionDate]]),
                                fromMinLeftOrigin=TRUE, initialAge=originLength, verbose=FALSE){
  # Returns a list of triangles (possibly a list of lists, if format="triangular"), where the top level list elements correspond
  # to the unique values of the colCohort in the transactions.copy dataset
  # If firstTransactionWRTCohort is TRUE, FirstValuationDate for each customer is relative to only his purchases within cohort X,
  # otherwise FirstValuationDate is relative to all his purchases
  # If allCohort is TRUE, an additional list element titled "All" is returned that corresponds to a non-subsetted triangle
  # remaining parameters get passed to the getTriangles() method (Note, each triangle will have the same dimensions)

  # Copy the transactions dataset so we don't muck it up
  transactions.copy <- copy(transactions[, c(colCustomerID, colTransactionDate, colCohort, colsFinancial), with=FALSE])
  setnames(transactions.copy, c(colCustomerID, colTransactionDate, colCohort), c("CustomerID", "TransactionDate", "Cohort"))

  # If firstTransactionWRTCohort is FALSE, build a table of each customer's first transaction date realtive to all his purchases
  if(!firstTransactionWRTCohort) firstTransactionDates <- transactions.copy[, list(FirstTransactionDate=min(TransactionDate)),
                                                                            keyby=CustomerID]

  # Get the set of unique cohorts
  if(is.null(colCohort)) stop("colCohort is null in transaction_cohorts()")
  setkey(transactions.copy, "Cohort") # Key transactions by Cohort
  cohorts <- unique(transactions.copy$Cohort) # Get the unique cohorts

  # Build the list of triangles by cohort
  triList <- lapply(cohorts, function(cohort){

    if(verbose) print(paste0("Building triangles for ", cohort))

    # Subset the transactions by the given cohort
    cohortTrans <- transactions.copy[cohort, nomatch=0]

    # Build the cumulative transaction dataset
    cohortTrans.cmltv <- cumulate_transactions(transactions.copy, colsFinancial=colsFinancial)

    # If firstTransactionWRTCohort is FALSE, overwrite FirstTransactionDate for each customer
    if(!firstTransactionWRTCohort){
      cohortTrans.cmltv[, FirstValuationDate := NULL]
      cohortTrans.cmltv <- firstTransactionDates[cohortTrans.cmltv]
      setnames(cohortTrans.cmltv, "FirstTransactionDate", "FirstValuationDate")
    }

    # Get the triangles for this cohort
    tri <- make_triangles(
      cohortTrans.cmltv, format=format, minLeftOrigin=minLeftOrigin, originLength=originLength, rowDev=rowDev, colDev=colDev,
      lastValuationDate=lastValuationDate, fromMinLeftOrigin=fromMinLeftOrigin, initialAge=initialAge
    )

    return(tri)
  })

  # Set the list element names
  names(triList) <- cohorts

  # If allCohort is TRUE, build the triangle for all chorts
  if(allCohort){
    if(verbose) print("Building triangles for _All")
    transactions.cmltv <- cumulate_transactions(transactions.copy, colsFinancial=colsFinancial)
    allTri <- make_triangles(
      transactions.cmltv, format=format, minLeftOrigin=minLeftOrigin, originLength=originLength, rowDev=rowDev, colDev=colDev,
      lastValuationDate=lastValuationDate, fromMinLeftOrigin=fromMinLeftOrigin, initialAge=initialAge
    )
    triList_all <- list(allTri)
    names(triList_all) <- "_All"
    triList <- c(triList_all, triList)
  }

  return(triList)
}

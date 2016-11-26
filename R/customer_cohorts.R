#' @title
#' Customer Cohorts
#'
#' @description
#' Generate customer level triangles, i.e. triangles per customer-level feature (e.g. first product purchased)
#'
#' @details
#' Returns a list of triangles (possibly a list of lists, if format="triangular"), where the top level list elements correspond
#' to the unique values of the colCohort in the customers dataset
#'
#' @param transactions.cmltv A data.table of cumulative transaction valuations (result of calling cumulate_transactions())
#' @param customers A data.table of customers (probably the result of calling get_customers())
#' @param colCohort Name of the column for which to group cohorts
#' @param colCustomerID Name of the CustomerID column
#' @param colTransactionDate Name of the TransactionDate column
#' @param colsFinancial Character vector corresponding to the name of financial columns in \code{transactions.cmltv}
#' @param allCohort Triangles be returned representing the combined sum of all cohorts?
#' @param format How should the triangles be returned? Either "tall" (a data.table) or "triangular" (a list of data.tables)
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
#' transactions <- sample_transactions(3, minDate=as.Date("2010-1-1"), maxDate=as.Date("2015-12-31"))
#' customers <- get_customers(transactions, colsFinancial="Amount", catCols="Product")
#' transactions.cmltv <- cumulate_transactions(transactions, colsFinancial="Amount")
#' customer_cohorts(transactions.cmltv, customers, colCohort="Product.First")

customer_cohorts <- function(transactions.cmltv, customers, colCohort, colCustomerID="CustomerID",
                             colTransactionDate="TransactionDate", colsFinancial="auto", allCohort=TRUE, format="triangular",
                             minLeftOrigin=min(transactions.cmltv$FirstValuationDate), originLength=12, rowDev=12, colDev=12,
                             lastValuationDate=max(transactions.cmltv$ValuationDate), fromMinLeftOrigin=TRUE,
                             initialAge=originLength, verbose=FALSE){
  # Returns a list of triangles (possibly a list of lists, if format="triangular"), where the top level list elements correspond
  # to the unique values of the colCohort in the customers dataset
  # Only the inner-join between custs and trans.cmltv will be used for building triangles
  # If allCohort is TRUE, an additional list element titled "All" is returned that corresponds to a non-subsetted triangle
  # remaining parameters get passed to the getTriangles() method (Note, each triangle will have the same dimensions)

  if(verbose)
    print(paste0("Triangle parameters{
                 format:", format, ",
                 minLeftOrigin:", minLeftOrigin, ",
                 originLength:", originLength, ",
                 rowDev:", rowDev, ",
                 colDev:", colDev, ",
                 lastValuationDate:", lastValuationDate, ",
                 fromMinLeftOrigin:", fromMinLeftOrigin, ",
                 initialAge:", initialAge,
                 "}"))

  # Copy transactions.cmltv so we don't muck it up
  transactions.cmltv.copy <- copy(transactions.cmltv)

  # Map cohort values from customers to transactions.cmltv.copy
  transactions.cmltv.copy[customers, eval(parse(text=paste0("Cohort := ", colCohort))), on=colCustomerID]

  # Adjust names
  setnames(transactions.cmltv.copy, c(colCustomerID), c("CustomerID"))

  # Key trans.cmltv by colCohort
  setkey(transactions.cmltv.copy, "Cohort")

  # Get the set of unique cohorts
  cohorts <- sort(unique(customers[[colCohort]]))

  # Build the list of triangles by cohort
  triList <- lapply(cohorts, function(cohort){

    if(verbose) print(paste0("Building triangles for ", cohort))

    # Subset the cumulative transactions by the given cohort
    cohortTrans.cmltv <- transactions.cmltv.copy[.(cohort), nomatch=0]

    # Get the triangles for this cohort
    tri <- make_triangles(
      cohortTrans.cmltv[, !"Cohort"], format=format, minLeftOrigin=minLeftOrigin, originLength=originLength, rowDev=rowDev,
      colDev=colDev, lastValuationDate=lastValuationDate, fromMinLeftOrigin=fromMinLeftOrigin, initialAge=initialAge,
      colsFinancial=colsFinancial
    )

    return(tri)
  })

  # Set the list element names
  names(triList) <- cohorts

  # If allCohort is TRUE, build the triangle for all chorts
  if(allCohort){
    if(verbose) print("Building triangles for _All")
    allTri <- make_triangles(
      transactions.cmltv.copy[, !"Cohort"], format=format, minLeftOrigin=minLeftOrigin, originLength=originLength, rowDev=rowDev,
      colDev=colDev, lastValuationDate=lastValuationDate, fromMinLeftOrigin=fromMinLeftOrigin, initialAge=initialAge,
      colsFinancial=colsFinancial
    )
    triList_all <- list(allTri)
    names(triList_all) <- "_All"
    triList <- c(triList_all, triList)
  }

  return(triList)
}

#' @title
#' Cumulate Transactions
#'
#' @description
#' Aggregate transactions monthly and measure cumulative financial values as well as cumulative transaction count per customer
#'
#' @details
#' Returns a data.table object with columns {[CustomerID], FirstValuationDate, ValuationDate, Transactions.cmltv} and
#' {Financial1.cmltv, Financial2.cmltv, ...} for all specified financial columns in the transactions dataset
#'
#' @param transactions A data.table of transactions with a column for CustomerID and TransactionDate
#' @param colCustomerID Name of the CustomerID column
#' @param colTransactionDate Name of the TransactionDate column
#' @param colsFinancial Character vector corresponding to the name of financial columns in \code{transactions}
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' set.seed(2016)
#' transactions <- sample_transactions(10)
#' cumulate_transactions(transactions)
#' cumulate_transactions(transactions, colsFinancial=c("Amount"))

cumulate_transactions <- function(transactions, colCustomerID="CustomerID", colTransactionDate="TransactionDate", colsFinancial=NULL){
  # Groups transactions by month and then takes cumulative sums on all specified financial columns

  # Copy the transactions dataset so we don't muck it up
  transactions.copy <- copy(transactions[, c(colCustomerID, colTransactionDate, colsFinancial), with=FALSE])
  setnames(transactions.copy, c(colCustomerID, colTransactionDate), c("CustomerID", "TransactionDate"))

  # Build expression to aggregate data
  if(is.null(colsFinancial)){
    expr <- "list(Transactions=.N)"
  } else{
    expr <- paste0("list(Transactions=.N, ", paste0(colsFinancial, "=sum(", colsFinancial, ")", collapse=", "), ")")
  }

  # Group the transactions occuring in the same month, per customer
  transactions.grouped <- transactions.copy[, eval(parse(text=expr)), keyby=list(CustomerID, TransactionDate=eom(TransactionDate))]

  # Get cumulative sums
  numericCols <- c("Transactions", colsFinancial)
  expr <- paste0("list(ValuationDate=TransactionDate, ", paste0(numericCols, ".cmltv=cumsum(", numericCols, ")", collapse=", "), ")")
  transactions.cmltv <- transactions.grouped[, eval(parse(text=expr)), by=CustomerID]
  transactions.cmltv <- transactions.grouped[, list(CustomerID, FirstValuationDate=TransactionDate)][transactions.cmltv, on="CustomerID", mult="first"]

  setkey(transactions.cmltv, "CustomerID", "ValuationDate")
  return(transactions.cmltv)
}

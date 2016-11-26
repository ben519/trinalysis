#' @title
#' Get Customers
#'
#' @description
#' Build a customers dataset from a transactions dataset with required columns {CustomerID, TransactionDate} (or alternate names)
#'
#' @details
#' Returns a data.table object with one row per unique CustomerID
#'
#' @param transactions A data.table of transactions with a column for CustomerID and TransactionDate
#' @param colCustomerID Name of the CustomerID column
#' @param colTransactionDate Name of the TransactionDate column
#' @param colsFinancial Character vector corresponding to the name of financial columns in \code{transactions}
#' @param colsCategory Character vector corresponding to the name of categorical columns in \code{transactions}
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' set.seed(2016)
#' transactions <- sample_transactions(10)
#' get_customers(transactions)
#' get_customers(transactions, colsFinancial=c("Amount"), colsCategory=c("Product"))

get_customers <- function(transactions, colCustomerID="CustomerID", colTransactionDate="TransactionDate", colsFinancial=NULL,
                          colsCategory=NULL){
  # Build a customers dataset from a transactions dataset with required columns {CustomerID, TransactionDate} (or alternate names)
  # If colsFinancial is given, each associated financial column in transactions will be summed and averaged per customer
  # If colsCategory is given, the first value of each categorical column will be given per customer (based on his earliest
  # transaction with ties broken randomly)

  # Copy the transactions dataset so we don't muck it up
  transactions.copy <- copy(transactions[, c(colCustomerID, colTransactionDate, colsFinancial, colsCategory), with=FALSE])
  setnames(transactions.copy, c(colCustomerID, colTransactionDate), c("CustomerID", "TransactionDate"))

  # Build expression to get the sum and mean of colsFinancial
  if(is.null(colsFinancial)){
    expr <- paste0("list(Transactions=.N, TransactionDate.First=min(TransactionDate), TransactionDate.Last=max(TransactionDate))")
  } else{
    expr <- paste0(
      "list(Transactions=.N, TransactionDate.First=min(TransactionDate), TransactionDate.Last=max(TransactionDate), ",
      paste0(colsFinancial, ".Sum=sum(", colsFinancial, "), ", colsFinancial, ".Avg=mean(", colsFinancial, ")", collapse=", "),
      ")"
    )
  }

  # Aggregate by Customer
  customers <- transactions.copy[, eval(parse(text=expr)), keyby=CustomerID]

  if(!is.null(colsCategory)){

    # Build the expression to get the first of each category
    expr <- paste0("`:=`(", paste0(colsCategory, ".First=", colsCategory, collapse=", "), ")")

    # For each customer get the category(s) of his first transaction
    setnames(customers, "TransactionDate.First", "TransactionDate")
    setkey(customers, "CustomerID", "TransactionDate")

    transactions.copy <- transactions.copy[sample(.N, .N)]  # random shuffle
    setkey(transactions.copy, "CustomerID", "TransactionDate")

    # Join and evaluate (NOTE - if multiple tranasctions on same first day, only the first is kept)
    customers[transactions.copy, mult="first", eval(parse(text=expr))]
    setnames(customers, "TransactionDate", "TransactionDate.First")
  }

  # Reset the column names
  setnames(transactions.copy, c("CustomerID", "TransactionDate"), c(colCustomerID, colTransactionDate))

  # Reset the keys
  setkey(customers, "CustomerID")

  return(customers[])
}

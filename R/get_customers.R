#' @title
#' Get Customers
#'
#' @description
#' Build a customers dataset from a transactions dataset with required columns {CustomerID, TransactionDate} (or alternate names)
#'
#' @details
#' Returns a data.table object with one row per unique CustomerID
#'
#' @param nCusts How many customers?
#' @param sdTransactions The number of transactions per customer = \code{abs(ceiling(rnorm(n=1, sd=sdTransactions)))}
#' @param sdAmount The amount of each transaction = \code{abs(round(rnorm(n=1, sd=sdAmount),2))}
#' @param minDate The minimum possible date a transaction could occur
#' @param maxDate The maximum possible date a transaction could occur
#' @param products A character vector of potential product names
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
#' get_customers(transactions, financialCols=c("Amount"), catCols=c("Product"))

get_customers <- function(transactions, colCustomerID="CustomerID", colTransactionDate="TransactionDate", financialCols=NULL, catCols=NULL){
  # Build a customers dataset from a transactions dataset with required columns {CustomerID, TransactionDate} (or alternate names)
  # If financialCols is given, each associated financial column in transactions will be summed and averaged per customer
  # If catCols is given, the first value of each categorical column will be given per customer (based on his earliest
  # transaction with ties broken randomly)

  # Copy the transactions dataset so we don't muck it up
  transactions.copy <- copy(transactions[, c(colCustomerID, colTransactionDate, financialCols, catCols), with=FALSE])
  setnames(transactions.copy, c(colCustomerID, colTransactionDate), c("CustomerID", "TransactionDate"))

  # Build expression to get the sum and mean of financialCols
  if(is.null(financialCols)){
    expr <- paste0("list(Transactions=.N, TransactionDate.First=min(TransactionDate), TransactionDate.Last=max(TransactionDate))")
  } else{
    expr <- paste0(
      "list(Transactions=.N, TransactionDate.First=min(TransactionDate), TransactionDate.Last=max(TransactionDate), ",
      paste0(financialCols, ".Sum=sum(", financialCols, "), ", financialCols, ".Avg=mean(", financialCols, ")", collapse=", "),
      ")"
    )
  }

  # Aggregate by Customer
  customers <- transactions.copy[, eval(parse(text=expr)), keyby=CustomerID]

  if(!is.null(catCols)){

    # Build the expression to get the first of each category
    expr <- paste0("`:=`(", paste0(catCols, ".First=", catCols, collapse=", "), ")")

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

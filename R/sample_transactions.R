#' @title
#' Sample Transactions
#'
#' @description
#' Generate a sample transactions dataset
#'
#' @details
#' Returns a data.table object with columns {TransactionID, TransactionDate, CustomerID, Amount, Product}
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
#' sample_transactions(nCusts=10)
#' sample_transactions(nCusts=10, sdTransactions=5, sdAmount=1000)
#' sample_transactions(nCusts=10, minDate=as.Date("2000-1-1"), maxDate=as.Date("2016-12-31"))
#' sample_transactions(nCusts=10, products=c("baseball", "football", "soccerball"))

sample_transactions <- function(nCusts, sdTransactions=10, sdAmount=10, minDate=as.Date("2014-1-1"), maxDate=as.Date("2015-12-31"), products=LETTERS){
  # Method to build sample transactions dataset

  # Randomly sample nCusts dates between min and max.  These will be the FirstTransaction dates for each customer
  transactions <- data.table(CustomerID=seq(nCusts), FirstTransactionDate=minDate + sample(as.integer(maxDate-minDate), size=nCusts, replace=TRUE))

  # For each customer randomly determine the number of transactions
  transactions[, Transactions:=abs(ceiling(rnorm(n=nCusts, sd=sdTransactions)))]

  # For each customer, randomly determine transaction dates
  transactions <- transactions[, list(TransactionDate=c(FirstTransactionDate, FirstTransactionDate + abs(ceiling(rnorm(n=Transactions, sd=maxDate-minDate))))), by=list(CustomerID)]

  # Remove the dates past maxDate
  transactions <- transactions[TransactionDate <= maxDate]

  # Randomly determine Amount and Product
  transactions[, `:=`(Amount=abs(round(rnorm(n=nrow(transactions), sd=sdAmount), 2)), Product=products[sample(length(products), size=nrow(transactions), replace=TRUE)])]

  # Add TransactionID
  transactions[, TransactionID:=.I]

  # Fix $0 transactions
  transactions[Amount==0, Amount:=.01]

  # Set the column order
  setcolorder(transactions, c("TransactionID", "TransactionDate", "CustomerID", "Amount", "Product"))

  setkey(transactions, "TransactionID")
  return(transactions[])
}

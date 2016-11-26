#' @title
#' Tall to Triangular
#'
#' @description
#' Convert triangles in tall format to a list of triangles in triangular format
#'
#' @details
#' Returns a list of data.table objects
#'
#' @param triangleDT Triangles in tall, data.table format
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' set.seed(2357)
#' transactions <- sample_transactions(10)
#' transactions.cmltv <- cumulate_transactions(transactions)
#' triangles <- make_triangles(transactions.cmltv, format="tall")
#' tall_to_triangular(triangles)

tall_to_triangular <- function(triangleDT){
  # Converts a set of triangles in the tall, data.table format to a list of triangular formats

  triCols <- colnames(triangleDT[, !c("LeftOrigin", "RightOrigin", "ValuationDate", "Age"), with=FALSE])

  mylist <- list()
  for(colname in triCols){
    mylist[[length(mylist)+1]] <- as.triangle(copy(triangleDT), valueCol=colname)
  }
  names(mylist) <- triCols
  return(mylist)
}

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
#' @param triangleDT Should headers be descriptive?
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' set.seed(2357)
#' transactions <- sample_transactions(10)
#' triangles <- make_triangles(transactions, format="tall")
#' tall_to_triangular(triangles)

tall_to_triangular <- function(triangleDT, descriptiveHeaders = TRUE){
  # Converts a set of triangles in the tall, data.table format to a list of triangular formats

  dropcols <- intersect(colnames(triangleDT), c("LeftOrigin", "RightOrigin", "ValuationDate", "Age"))
  triCols <- colnames(triangleDT[, !dropcols, with=FALSE])

  mylist <- list()
  for(colname in triCols){
    mylist[[length(mylist)+1]] <- as.triangle(copy(triangleDT), valueCol=colname, descriptiveHeaders = descriptiveHeaders)
  }
  names(mylist) <- triCols
  return(mylist)
}

#' @title
#' as.triangle
#'
#' @description
#' Convert a single triangle from tall format to triangular format
#'
#' @details
#' Returns a data.table object with rows representing origin periods and columns representing valuation ages
#'
#' @param triangleDT A triangle in tall, data.table format
#' @param valueCol Name of the column to convert to triangular format
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
#' as.triangle(triangles, valueCol="ActiveCustomers")

as.triangle <- function(triangleDT, valueCol="Transactions"){
  # Convert a triangle from tall format to triangular format

  tri <- triangleDT[, c("LeftOrigin", "RightOrigin", "Age", valueCol), with=FALSE]
  tri[, Origin := paste0(LeftOrigin, " - ", RightOrigin)]
  tri <- dcast.data.table(tri, Origin ~ Age, value.var=valueCol, drop=FALSE)
  result <- as.matrix(tri[, 2:ncol(tri), with=FALSE])
  dimnames(result) <- list(Origin=tri$Origin, Age=colnames(tri)[2:ncol(tri)])
  return(result)
}

#' @title
#' as.triangle
#'
#' @description
#' Convert a single triangle from tall format to triangular format
#'
#' @details
#' Returns a matrix object with rows representing origin periods and columns representing valuation ages
#'
#' @param triangleDT A triangle in tall, data.table format
#' @param valueCol Name of the column to convert to triangular format
#' @param descriptiveHeaders Should headers be descriptive?
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
#' as.triangle(triangles, valueCol="ActiveCustomers")
#' as.triangle(triangles, valueCol="Transactions")

as.triangle <- function(triangleDT, valueCol="Transactions", descriptiveHeaders = TRUE){
  # Convert a triangle from tall format to triangular format

  tri <- triangleDT[, c("LeftOrigin", "RightOrigin", "Age", valueCol), with=FALSE]
  tri[, Origin := paste0(LeftOrigin, " - ", RightOrigin)]

  if(descriptiveHeaders == T & !stringr::str_detect(valueCol, "\\.cmltv$")){
    tri[, Header := paste0(shift(Age, type = "lag", fill = 0), " - ", Age), by = Origin]
    tri[, Header := factor(Header, levels = unique(Header))]
  } else{
    tri[, Header := Age]
  }

  tri <- dcast.data.table(tri, Origin ~ Header, value.var=valueCol, drop=FALSE)
  result <- as.matrix(tri[, 2:ncol(tri), with=FALSE])
  dimnames(result) <- list(Origin = tri$Origin, `Age (months)` = colnames(tri)[2:ncol(tri)])
  return(result)
}

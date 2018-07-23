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

tall_to_triangular <- function(triangleDT, descriptiveHeaders = TRUE, idCols = "Cohort"){
  # Converts a set of triangles in the tall, data.table format to a list of triangular formats

  triCols <- setdiff(colnames(triangleDT), c("LeftOrigin", "RightOrigin", "ValuationDate", "Age", "Cohort", "CohortCustomers"))
  triList <- list()
  for(colname in triCols){
    triList[[length(triList) + 1L]] <- as.triangle(
      triangleDT = copy(triangleDT),
      valueCol = colname,
      descriptiveHeaders = descriptiveHeaders,
      idCols = setdiff(idCols, colname)
    )
  }
  names(triList) <- triCols
  return(triList)
}

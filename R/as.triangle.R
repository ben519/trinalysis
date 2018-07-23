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

as.triangle <- function(triangleDT, valueCol = "Transactions", descriptiveHeaders = TRUE, idCols = "Cohort"){
  # Convert a triangle from tall format to triangular format
  # idCols must either contain {LeftOrigin, RightOrigin} or {Cohort} but can contain additional columns (commonly CohortCustomers)

  if(length(intersect(c("LeftOrigin", "RightOrigin"), idCols)) != 2 & !"Cohort" %in% idCols)
    stop("idCols must either contain {LeftOrigin, RightOrigin} or {Cohort}")

  if(valueCol %in% idCols) stop("valueCol can't be one of idCols")

  keepCols <- unique(c("Cohort", idCols, "Age", valueCol))
  tri <- triangleDT[, keepCols, with=FALSE]

  if(descriptiveHeaders == T & !stringr::str_detect(valueCol, "\\.cmltv$")){
    tri[, Header := paste0(shift(Age, type = "lag", fill = 0), " - ", Age), by = Cohort]
    tri[, Header := factor(Header, levels = unique(Header))]
  } else{
    tri[, Header := Age]
  }

  dcastFormula <- paste(paste(idCols, collapse = " + "), " ~ Header")
  result <- dcast.data.table(tri, eval(parse(text = dcastFormula)), value.var=valueCol, drop = c(TRUE, FALSE))
  return(result[])
}

#' @title
#' Write Triangles
#'
#' @description
#' Method to write many formats of triangles to a CSV file
#'
#' @details
#' Writes triangles to a CSV file
#'
#' @param triangles Triangles in tall or a list of them in triangular format, or a list of either of those
#' @param filename filename (optionally with path) determining where to save triangles. DEFAULT is triangles.csv inside the
#' current working directory
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' set.seed(2016)
#' transactions <- sample_transactions(10)
#' transactions.cmltv <- cumulate_transactions(transactions, colsFinancial="Amount")
#' tris <- make_triangles(transactions.cmltv)
#' write_triangles(tris)

write_triangles <- function(triangles, filename="triangles.csv"){
  # Method to write triangles to a csv file
  # Determines if triangles is a data.table or a list of triangles
  # Calls the appropriate method (writeTriangleDT or writeTriangleList)

  triListOut.1 <- function(triList){
    # Takes a list of triangular objects (height 1) compiles the data into a single matrix for output

    output <- mapply(function(tri, triName){

      # Combine the data and the column names
      newTri <- rbind(colnames(tri), tri)

      # Combine the rownames
      newTri <- cbind(c("Origin", rownames(tri)), newTri)

      # Add a line for this triangle's name
      newTri <- rbind(rep(triName, ncol(newTri)), newTri)

      # Add a line of empty space beneath
      newTri <- rbind(newTri, rep("", ncol(newTri)))

    }, triList, names(triList), SIMPLIFY=FALSE)

    # Combine all elements in the modified list
    output <- do.call(rbind, output)

    # Remove the bottom line of whitespce
    output <- head(output, -1)

    return(output)
  }

  triListOut.2 <- function(triCohortList){
    # Takes a list of lists of triangular objects and compiles the data into a single matrix for output

    output <- mapply(function(triList, triListName){

      # Modify the sublists using triListOut.1
      newTriList <- triListOut.1(triList)

      # Add a line for these triangles' name
      newTriList <- rbind(rep(triListName, ncol(newTriList)), newTriList)

      # Add a line of empty space to the right
      newTri <- cbind(newTriList, rep("", nrow(newTriList)))

    }, triCohortList, names(triCohortList), SIMPLIFY=FALSE)

    # Combine all elements in the modified list
    output <- do.call(cbind, output)

    # Remove the far right line of whitespace
    output <- output[, 1:(ncol(output)-1)]

    return(output)
  }

  if(is(triangles, "list")){

    if("list" %in% class(triangles[[1]])){
      # triangles is a list of list of triangles

      output <- triListOut.2(triCohortList=triangles)
      write.table(output, sep=",", file=filename, row.names=FALSE, col.names=FALSE, na="")

    } else if(is(triangles[[1]], "matrix")){
      # triangles is a list of trianglular objects

      output <- triListOut.1(triList=triangles)
      write.table(output, sep=",", file=filename, row.names=FALSE, col.names=FALSE, na="")

    } else{
      # triangles is a list of data.table objects
      # collapse the list of data.tables into a single data.table with an extra "Cohort" column

      output <- lapply(names(triangles), FUN=function(cohort){
        tri <- copy(triangles[[cohort]])
        if("Cohort" %in% colnames(tri)) setnames(tri, "Cohort", "Cohort_")
        tri[, Cohort:=cohort]
        return(tri)
      })
      output <- do.call(rbind, output)
      setcolorder(output, c("Cohort", setdiff(colnames(triangles[[1]]), "Cohort")))
      write.table(output, sep=",", file=filename, row.names=FALSE, na="")
    }
  } else{
    # triangles is a single data.table

    write.table(triangles, sep=",", file=filename, row.names=FALSE, na="")
  }
}

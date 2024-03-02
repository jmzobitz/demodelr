#' Matrix eigenvalues and eigenvectors
#'
#' \code{eigenvalues} visualizes the vector field for a one or two dimensional differential equation.
#'
#' @param matrix_entries entries of your matrix in row wise format.
#' So the matrix
#' #   4  3
#' #   2  1
#' # would be entered in c(4,3,2,1)
#'
#' @param matrix_rows the number of rows and columns in your SQUARE matrix.

#' @return The result is a list with two elements (denoted by the “$”), values and vectors.
#' result$values are the eigenvalues, stored as a vector. The leading eigenvalue is the first entry in the vector.
#'
# result$vectors gives the corresponding right eigenvectors, sequentially stored as columns of a matrix (so eigenvector #1 corresponds to eigenvalue #1.  The vectors are all normalized to have a length of 1.

#' @examples
#' eigenvalues(c(1,2,3,4))
#'
#' # Note: for the 3 x 3 case, we need to define the number of matrix rows:
#' eigenvalues(c(1,2,3,4,5,6,7,8,9),matrix_rows=3)


#' @export



eigenvalues <- function(matrix_entries,matrix_rows=2) {

  if (sqrt(length(matrix_entries)) != matrix_rows) {
    stop("Your matrix is not a square matrix")
  }
  ### The following code shapes the entries into a matrix for R
  aMatrix = matrix(matrix_entries, nrow=matrix_rows, byrow=TRUE)

  ### Now calculate the eigenvectors and eigenvalues
  result = eigen(aMatrix)
  result$vectors <- data.frame(result$vectors)

  return(result)

}


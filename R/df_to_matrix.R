#' Convert a dataframe to a matrix
#'
#' This function converts a dataframe to a matrix, with the first column of the dataframe becoming the matrix rownames.
#' @param x A dataframe.
#' @examples
#' df_to_matrix(my.dataframe)
#' @export


# Convert a dataframe to a matrix, with the first column becoming rownames

df_to_matrix <- function(x) {

  m <- as.matrix(x[ , -1])
  rownames(m) <- x[ , 1]
  m
}

# Convert a dataframe to a matrix, with the first column becoming rownames

df_to_matrix <- function(x) {

  m <- as.matrix(x[ , -1])
  rownames(m) <- x[ , 1]
  m
}

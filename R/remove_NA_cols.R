#' Remove columns from an array if they contain all NA values
#'
#' A convenience function to clean up your data by removing any columns that will not be useful since they consist entirely of NA values.
#' @param x A dataframe or other array.
#' @examples
#' remove_NA_cols(my.dataframe)
#' @export


# Remove columns from an array if they contain all NA values

remove_NA_cols <- function(x) {

  x[ , sapply(1:ncol(x), function(i) sum(is.na(x[ , i])) != nrow(x))]
}

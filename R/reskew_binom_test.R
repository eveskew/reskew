#' Perform the exact binomial test with custom output format
#'
#' This function is a wrapper for \code{binom.test()} with a custom output format. Specifically, the function returns a dataframe with columns for the mean of the input vector as well as min and max values calculated from \code{binom.test}. This function is particularly useful in conjunction with the \code{stat_summary()} function in \pkg{ggplot2} for plotting summary statistics of binary data.
#' @param numeric.vector A binary numeric vector.
#' @examples
#' test.vector <- rep(c(1, 0), 25)
#'
#' reskew_binom_test(test.vector)
#' @export


reskew_binom_test <- function(numeric.vector) {

  # Perform the exact binomial test
  binom.out <- binom.test(sum(numeric.vector), length(numeric.vector))

  # Package mean, confidence interval min and max into a dataframe
  df <- data.frame(sum(numeric.vector)/length(numeric.vector),
                   binom.out$conf.int[1], binom.out$conf.int[2])
  colnames(df) <- c("y", "ymin", "ymax")

  # Return the dataframe
  return(df)
}

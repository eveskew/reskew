#' Convert a factor variable into integers
#'
#' This function converts a factor variable into an integer vector, which is useful for coding factors in the Stan programming language. Note that \code{addNA()} is the default, so if \code{NA}s exist in the factor, they will be included in the resulting vector as their own integer value.
#' @param factor A factor.
#' @examples
#' to_stan_factor(my.factor)
#' @export


# Convert factor variable to integers for use as Stan data

to_stan_factor <- function(factor) {

  addNA(factor) %>%
    as.integer()
}

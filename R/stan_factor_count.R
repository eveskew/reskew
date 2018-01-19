#' Count the number of levels in a factor
#'
#' This function counts the number of levels in a factor variable and is meant to be used in conjunction with \code{to_stan_factor()}. Note that both of these functions use \code{addNA()}, such that any exisiting \code{NA} values in the factor are grouped together and counted as a distinct factor level.
#' @param factor A factor.
#' @examples
#' stan_factor_count(my.factor)
#' @export


# Count levels of a factor variable for use as Stan data

stan_factor_count <- function(factor) {

  addNA(factor) %>%
    as.integer() %>%
    max()
}

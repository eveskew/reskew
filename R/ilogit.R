#' Inverse logit function
#'
#' This function performs the inverse logit transformation.
#' @param x A number.
#' @examples
#' ilogit(my.number)
#' @export


# Inverse logit transformation

ilogit <- function(x) { return(1 / (1 + exp(-x))) }

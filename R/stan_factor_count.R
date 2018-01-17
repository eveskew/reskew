# Count levels of a factor variable for use as Stan data

stan_factor_count <- function(factor) {

  addNA(factor) %>%
    as.integer() %>%
    max()
}

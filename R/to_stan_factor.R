# Convert factor variable to integers for use as Stan data

to_stan_factor <- function(factor) {

  addNA(factor) %>%
    as.integer()
}

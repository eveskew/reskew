# Inverse logit transformation

ilogit <- function(x) { return(1 / (1 + exp(-x))) }

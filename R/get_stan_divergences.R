# Define a function that allows you to check for divergent iterations across
# all Markov chains from a fit Stan model object

get_stan_divergences <- function(fit.stan.model) {

  # Get diagnostic parameters from a fit Stan model
  sampler.params <- rstan::get_sampler_params(fit.stan.model, inc_warmup = F)

  # Use "n_divergent__" as the default string to match
  match.string <- "n_divergent__"

  # If the model instead has a "divergent__" column, convert the match
  # string to this string
  if (sum(colnames(sampler.params[[1]]) %in% "divergent__") > 0)
    match.string <- "divergent__"

  # Report the column sums of divergent iteration columns for every chain
  # used in the fit model
  colSums(sapply(sampler.params, function(x) x[ , match.string]))
}

#' Process a fit Stan model
#'
#' This function processes a fit Stan model object, recording divergent iterations, Rhat values, and, if they have been stored, log-likelihood values along with WAIC.
#' @param fit.stan.model A Stan model object.
#' @param n.pars.to.trim The number of parameters in the Stan model to \strong{exclude} from calculations. Useful for excluding \code{lp__} and any parameters that are saved at every model iteration, thus becoming unwieldy.
#' @examples
#' process_stanfit(my.stan.model, 1)
#' @export


# Define a function that processes a fit Stan model object

process_stanfit <- function(fit.stan.model, n.pars.to.trim = 0, pars.to.trim = "") {

  # Initialize list
  list <- list()

  # Get divergences
  list$divergences <- get_stan_divergences(fit.stan.model)

  # Get Rhat summary
  list$Rhat.summary <-
    summary(rstan::summary(fit.stan.model)$summary[ , "Rhat"])

  # Get trimmed parameter names
  list$pars.trim <-
    fit.stan.model@model_pars[1:(length(fit.stan.model@model_pars) - n.pars.to.trim)]

  list$pars.trim <- list$pars.trim[!(list$pars.trim %in% pars.to.trim)]

  # Package info into a dataframe
  list$df <- as.data.frame(fit.stan.model, pars = list$pars.trim)

  if("log_lik" %in% fit.stan.model@model_pars) {

    # Extract log likelihood values
    # Note that the extract() function with default behavior
    # (i.e., permuted = TRUE) will return a DIFFERENT log likelihood matrix
    # than what I'm doing here. By default, extract() will permute rows of the
    # matrix, which correspond to model iterations. extract_log_lik() does
    # not have this behavior. Ultimately, the difference here shouldn't matter
    # since WAIC operations operate on columns first.
    list$log_lik <- loo::extract_log_lik(fit.stan.model)

    # Calculate WAIC
    list$waic <- loo::waic(list$log_lik)
  }

  # Return list
  return(list)
}

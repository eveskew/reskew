#' A custom precis plot function
#'
#' This function is a modified version of \code{rethinking}'s plot method for \code{precis} objects. It allows for custom x-axis limits and specification of row labels.
#' @examples
#' my.precis.object <- rethinking::precis(my.dataframe, prob = 0.95)
#'
#' reskew_precis_plot(my.precis.object, xlab = "Parameter Estimate",
#'                    labels = my.custom.labels, xlim = c(-2, 8))
#' @export


# Define a custom precis plot function that allows you to alter x-axis limits
# and specify row labels

reskew_precis_plot <-
  function (x, y, pars, labels, cex, col.ci = "black", xlab = "Value", xlim...) {

    x <- x@output
    if (!missing(pars)) { x <- x[pars, ] }
    n <- nrow(x)
    mu <- x[n:1, 1]
    left <- x[[3]][n:1]
    right <- x[[4]][n:1]
    rethinking::set_nice_margins()
    dotchart(mu, labels = labels, cex = cex, xlab = xlab, xlim = xlim...)
    for (i in 1:length(mu)) lines(c(left[i], right[i]), c(i, i),
                                  lwd = 2, col = col.ci)
    abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
  }

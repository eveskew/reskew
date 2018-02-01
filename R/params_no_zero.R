#' @export


# Define a function to check for parameters that do not overlap zero in the specified
# interval. Parameters can also be excluded.

params_no_zero <- function (processed.stanfit, interval = 0.99,
                            exclude = c("tilde", "displacement")) {

  params.to.exclude <- toString(exclude) %>% gsub(", ", "|", .)

  rethinking::precis(processed.stanfit$df,
                     prob = interval, depth = 2,
                     pars = grep(params.to.exclude, processed.stanfit$pars.trim,
                                 invert = T, value = T))@output %>%
    round(2) %>%
    tibble::rownames_to_column() %>%
    filter( (`|0.99` >= 0 & `0.99|` >= 0) | (`|0.99` <= 0 & `0.99|` <= 0) )
}

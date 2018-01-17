# Generate a detection matrix for viral_species given a particular level of
# host aggregation

viral_detection_matrix <- function(dataframe, host.aggregation, S) {

  # Generate the detection matrix
  x <- dataframe %>%
    dplyr::group_by_("viral_species", host.aggregation) %>%
    dplyr::summarize(count = n()) %>%
    reshape2::dcast(paste0("viral_species ~ ", host.aggregation)) %>%
    # Remove rows where the viral species has not been identified
    dplyr::filter(!is.na(viral_species)) %>%
    # Remove column representing viral detections in unidentified taxa
    dplyr::select(which(colnames(.) != "NA")) %>%
    # Convert to a matrix
    df_to_matrix()

  # Convert NAs to zeroes
  x[is.na(x)] <- 0

  # Get rid of viral species that have zero row sums
  # (these represent viruses that were only observed in unknown taxa)
  x <- x[rowSums(x) > 0, ]

  # n = number of distinct viral species observed
  n <- dim(x)[1]

  # J = number of distinct host aggregations sampled
  J <- dim(x)[2]

  # K = number of samples (test_ids) for each host aggregation
  K <- dataframe %>%
    dplyr::group_by_(host.aggregation) %>%
    dplyr::summarize(count = n_distinct(test_id)) %>%
    dplyr::filter_(!is.na(host.aggregation)) %>%
    dplyr::filter(.[[1]] %in% colnames(x)) %>%
    dplyr::pull(count)

  # S = size of the supercommunity from which observed viral species are drawn
  if (missing(S)) { S <- n*5 }
  else { S <- S }

  # All viruses in the detection matrix should have been detected at least once
  assertthat::assert_that(sum(rowSums(x) > 0) == n)

  # For each host taxa, there can never be more observations of a given virus
  # than there were observations
  assertthat::assert_that(sum(sapply(1:J, function(i) max(x[ , i]) <= K[i])) == J)

  # Return list
  return(list("x" = x, "n" = n, "J" = J, "K" = K, "S" = S))
}

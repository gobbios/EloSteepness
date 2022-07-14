#' numeric summary of steepness
#'
#' @param x result from \code{\link{elo_steepness_from_matrix}},
#'        \code{\link{elo_steepness_from_sequence}} or
#'        \code{\link{davids_steepness}}
#' @param quantiles numeric, the quantiles to be returned
#'
#' @return a data.frame with one row providing a summary of the
#'         steepness posterior
#' @export
#'
#' @examples
#' data(dommats, package = "EloRating")
#'
#' res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 1, iter = 1000,
#'                                  silent = TRUE, refresh = 0)
#' steepness_precis(res)


steepness_precis <- function(x, quantiles = c(0.055, 0.25, 0.75, 0.945)) {
  if (!"steepness" %in% names(x)) {
    stop("wrong input object")
  }
  is_elo <- "cumwinprobs" %in% names(x)
  x <- x$steepness

  out <- data.frame(mean = mean(x),
                    sd = sd(x),
                    median = median(x),
                    mad = mad(x))
  nm <- paste0("q", substr(sprintf("%.3f", quantiles), 3, 5))
  for (i in seq_len(length(quantiles))) {
    out <- cbind(out, quantile(x, prob = quantiles[i]))
    colnames(out)[ncol(out)] <- nm[i]
  }

  if (is_elo) {
    out$mean_cv <- NA
    out$median_cv <- NA
    if (ncol(x) > 1) {
      means <- apply(x, 2, mean)
      out$mean_cv[1] <- sd(means) / mean(means)
      medians <- apply(x, 2, median)
      out$median_cv[1] <- sd(medians) / mean(medians)
    }
  }

  rownames(out) <- NULL
  out
}

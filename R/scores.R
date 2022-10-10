#' numeric summaries of individual scores
#' 
#' either based on summed winning probabilities or David's scores
#'
#' @param x result from \code{\link{elo_steepness_from_matrix}},
#'        \code{\link{elo_steepness_from_sequence}} or
#'        \code{\link{davids_steepness}}
#' @param quantiles numeric, the quantiles to be returned
#' @param elo_scores logical, with default \code{FALSE}. 
#'        If \code{TRUE} Elo-ratings are returned,
#'        rather than the default summed winning probabilities. This
#'        argument has no consequences if \code{x} is the result of
#'        \code{\link{davids_steepness}}.
#'
#' @importFrom stats quantile sd
#'
#' @return a data.frame with one line per individual, providing
#'         summaries of posteriors for individual scores
#' @export
#'
#' @examples
#' \donttest{
#' data("bonobos", package = "EloRating")
#' res <- davids_steepness(bonobos, refresh = 0, cores = 2)
#' scores(res)
#'
#' data("dommats", package = "EloRating")
#' m <- dommats$elephants
#' res <- elo_steepness_from_matrix(m, n_rand = 1, refresh = 0,
#'                                  iter = 1000, warmup = 500)
#' scores(res)
#' }

scores <- function(x, quantiles = c(0.045, 0.955), elo_scores = FALSE) {
  correct_object <- FALSE

  if ("cumwinprobs" %in% names(x)) {
    if (elo_scores) {
      y <- extract(x$stanfit, "EloStart")$EloStart
    } else {
      y <- x$cumwinprobs
    }
    res <- matrix(ncol = dim(y)[3], nrow = length(y[, , 1]))
    for (i in seq_len(ncol(res))) {
      res[, i] <- y[, , i]
    }
    correct_object <- TRUE
  }

  if ("norm_ds" %in% names(x)) {
    res <- x$norm_ds
    correct_object <- TRUE
  }

  if (!correct_object) {
    stop("object 'x' not of correct format")
  }

  n_ids <- ncol(res)

  out <- data.frame(id = x$ids,
                    mean = colMeans(res),
                    sd = apply(res, 2, sd),
                    median = apply(res, 2, median),
                    mad = apply(res, 2, mad))
  nm <- paste0("q", substr(sprintf("%.3f", quantiles), 3, 5))
  for (i in seq_len(length(quantiles))) {
    out <- cbind(out, apply(res, 2, quantile, prob = quantiles[i]))
    colnames(out)[ncol(out)] <- nm[i]
  }

  if ("cumwinprobs" %in% names(x)) {
    out$mean_cv <- NA
    if (ncol(x$steepness) > 1) {
      for (i in seq_len(length(x$ids))) {
        z <- colMeans(y[, , i])
        out$mean_cv[i] <- sd(z) / mean(z)
      }
    }
  }

  out
}

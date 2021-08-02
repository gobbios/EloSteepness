#' numeric summaries of individual ratings/scores (Elo-ratings or David's scores)
#'
#' @param x result from \code{\link{elo_steepness_from_matrix}} or
#'        \code{\link{davids_steepness}}
#' @param quantiles numeric, the quantiles to be returned
#'
#' @importFrom stats quantile sd
#'
#' @return a data.frame with one line per individual
#' @export
#'
#' @examples
#' data("bonobos", package = "EloRating")
#' res <- davids_steepness(bonobos, refresh = 0)
#' scores(res)
#' \dontrun{
#' data("dommats", package = "EloRating")
#' m <- dommats$badgers
#' res <- elo_steepness_from_matrix(m, n_rand = 2, refresh = 0)
#' scores(res)
#' }

scores <- function(x, quantiles = c(0.045, 0.955)) {
  correct_object <- FALSE
  if ("cumwinprobs" %in% names(x)) {
    y <- x$cumwinprobs
    res <- matrix(ncol = dim(y)[3], nrow = length(y[, , 1]))
    for (i in seq_len(ncol(res))) {
      res[, i] <- y[, , i]
    }
    xlab <- "cumulative Elo winning probability"
    correct_object <- TRUE
  }
  if ("norm_ds" %in% names(x)) {
    res <- x$norm_ds
    xlab <- "David's score (normalized)"
    correct_object <- TRUE
  }

  if (!correct_object) {
    stop("object 'x' not of correct format")
  }

  n_ids <- ncol(res)

  out <- data.frame(id = x$ids,
                    mean = colMeans(res),
                    sd = apply(res, 2, sd),
                    median = apply(res, 2, median))
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

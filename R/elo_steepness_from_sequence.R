#' steepness based on Bayesian Elo-rating
#'
#' for interaction data with known sequence of observations
#'
#' @param winner character (or factor) of winning individuals
#' @param loser character (or factor) of losing individuals
#' @param algo character, either \code{"fixed_sd"}, \code{"original"}, or
#'             \code{"fixed_k"}.This determines which algorithm
#'             to estimate Elo-ratings is used. Default is
#'             \code{"fixed_sd"}, which is a slight modification from
#'             Goffe et al's original code. \code{"fixed_k"} fixes the k 
#'             parameter ('shift coefficient' in Goffe et al) to
#'             the set value rather than estimating it from the data.
#' @param silent logical, suppress warnings (default is \code{FALSE})
#' @param k numeric, provides a fixed k parameter. This only has effects if
#'          \code{algo = "fixed_k"}. At its default \code{NULL} a value of
#'          0.4 is used.
#' @param ... additional arguments for \code{sampling()}
#'
#'
#' @return a list with results of the model fitting
#'         (see \code{\link{elo_steepness_from_matrix}}) for details
#'       
#' @export
#'
#' @examples
#' data(adv, package = "EloRating")
#' res <- elo_steepness_from_sequence(winner = adv$winner, loser = adv$loser,
#'                                    cores = 1, chains = 2, iter = 1000, 
#'                                    warmup = 500, seed = 1, refresh = 0)
#' plot_steepness(res)
#'

elo_steepness_from_sequence <- function(winner,
                                        loser,
                                        algo = c("fixed_sd", "original", "fixed_k"),
                                        silent = FALSE,
                                        k = NULL,
                                        ...) {
  algo <- match.arg(algo)

  # prepare data for Stan
  # not outsourced to dedicated function (yet) (as is the case for the 
  #   matrix versions via prep_data_for_rstan)
  # all individuals
  ids <- unique(c(as.character(winner), as.character(loser)))
  # indices of winners and losers
  w <- sapply(winner, function(x) which(ids == x))
  l <- sapply(loser, function(x) which(ids == x))

  standat <- list(winner = matrix(w),
                  loser = matrix(l),
                  diff_f = 1,
                  N = length(w),
                  K = length(ids),
                  n_rand = 1)
  standat$presence <- matrix(ncol = standat$K, nrow = standat$N, 1)
  standat$y <- rep(1, standat$N)
  standat$ids <- ids

  if (algo == "original") {
    if (silent) {
      res <- suppressWarnings(sampling(stanmodels$multi_steep_original,
                                       data = standat, ...))
    } else {
      res <- sampling(stanmodels$multi_steep_original, data = standat, ...)
    }
  }
  if (algo == "fixed_sd") {
    if (silent) {
      res <- suppressWarnings(sampling(stanmodels$multi_steep_fixed_sd,
                                       data = standat, ...))
    } else {
      res <- sampling(stanmodels$multi_steep_fixed_sd, data = standat, ...)
    }
  }
  if (algo == "fixed_k") {
    if (is.null(k)) k <- 0.4
    standat$k <- k
    dim(standat$k) <- 1
    if (silent) {
      res <- suppressWarnings(sampling(stanmodels$multi_steep_fixed_sd_fixed_k,
                                       data = standat, ...))
    } else {
      res <- sampling(stanmodels$multi_steep_fixed_sd_fixed_k, data = standat, ...)
    }
  }

  # extract any sampling issues
  issues <- sampler_diagnostics(res)

  # steepness values
  xres <- extract(res, "steepness")$steepness
  # summed winning probabilities (formerly: cumulative win probs)
  cumwinprobs <- extract(res, "cumwinprobs")$cumwinprobs
  # k values
  if (algo == "fixed_k") {
    kvals <- k
  } else {
    kvals <- extract(res, "k")$k
  }
  
  res <- list(steepness = xres,
              cumwinprobs = cumwinprobs,
              k = kvals,
              ids = standat$ids,
              diagnostics = issues,
              stanfit = res,
              mat = data.frame(winner = winner, loser = loser),
              algo = algo,
              sequence_supplied = TRUE)
  class(res) <- "elo_steepness"
  res
}

#' steepness based on Bayesian Elo-rating
#' 
#' for interaction data with known sequence of observations
#'
#' @param winner character (or factor) of winning individuals
#' @param loser character (or factor) of losing individuals
#' @param algo character, either \code{"fixed_sd"} or \code{"original"}.
#'             This determines which algorithm is used. Default is
#'             \code{"fixed_sd"}, which is a slight modification from
#'             Goffe et al's original code.
#' @param silent logical, suppress warnings
#' @param ... additional arguments for \code{sampling()}
#'
#'
#' @return a list
#' @export
#'
#' @examples
#' data(adv, package = "EloRating")
#' \dontrun{
#' res <- elo_steepness_from_sequence(winner = adv$winner, loser = adv$loser, cores = 4,
#'                                    iter = 3000, warmup = 2000, refresh = 0)
#' plot_steepness(res)
#'
#' }

elo_steepness_from_sequence <- function(winner,
                                        loser,
                                        algo = c("fixed_sd", "original"),
                                        silent = FALSE,
                                        ...) {
  algo <- match.arg(algo)
  
  # prepare data for Stan
  # not outsourced to dedicated function (yet) (as is the case for the matrix versions via prep_data_for_rstan)
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

  if (silent) {
    op_ori <- options()$warn
    options(warn = -1)
  }
  if (algo == "original") {
    res <- sampling(stanmodels$multi_steep_original, data = standat, ...)
  }
  if (algo == "fixed_sd") {
    res <- sampling(stanmodels$multi_steep_fixed_sd, data = standat, ...)
  }

  if (silent) {
    options(warn = op_ori)
  }

  issues <- c(divergent = NA, energy = NA, depth = NA)
  # check_energy
  issues["energy"] <- sum(get_bfmi(res) < 0.2)
  # divergent iterations
  issues["divergent"] <- sum(get_divergent_iterations(res))
  # tree depth
  issues["depth"] <- sum(sum(get_max_treedepth_iterations(res)))

  issues <- list(has_issues = any(issues > 0), issues)

  # steepness values
  xres <- extract(res, "steepness")$steepness
  # cum win probs
  cumwinprobs <- extract(res, "cumwinprobs")$cumwinprobs

  res <- list(steepness = xres,
              cumwinprobs = cumwinprobs,
              ids = standat$ids,
              diagnostics = issues,
              stanfit = res,
              mat = data.frame(winner = winner, loser = loser),
              algo = algo,
              sequence_supplied = TRUE)
  class(res) <- "elo_steepness"
  res
}

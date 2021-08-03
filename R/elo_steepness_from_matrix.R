#' steepness based on Bayesian Elo-rating
#'
#' @param mat square interaction matrix
#' @param algo character, either \code{"fixed_sd"} or \code{"original"}. This
#'             determines which algorithim is used. Default is \code{"fixed_sd"},
#'             which is a slight modification from Goffe et al's original code.
#' @param n_rand numeric, number of randomized sequences
#' @param silent logical, suppress warnings
#' @param static logical, treat the data as static (i.e. think of data in
#'               matrix form). Default is \code{TRUE}. \code{FALSE} is not
#'               yet supported.
#' @param ... additional arguments for \code{sampling()}
#'
#' @importFrom rstan sampling extract get_bfmi
#' @importFrom rstan get_divergent_iterations get_max_treedepth_iterations
#' @return a list
#' @export
#'
#' @examples
#' data(dommats, package = "EloRating")
#' \dontrun{
#' res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 10, cores = 4,
#'                                  iter = 3000, warmup = 2000, refresh = 0)
#' plot_steepness(res)
#'
#' res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 10, cores = 4,
#'                                  algo = "original", 
#'                                  iter = 3000, warmup = 2000, refresh = 0)
#' plot_steepness(res)
#' }

elo_steepness_from_matrix <- function(mat,
                                      algo = c("fixed_sd", "original"),
                                      n_rand = 10,
                                      silent = FALSE,
                                      static = TRUE,
                                      ...) {
  algo <- match.arg(algo)

  standat <- prep_data_for_rstan(mat = mat,
                                 n_rand = n_rand,
                                 silent = silent,
                                 for_elo_model = TRUE)

  if (silent) {
    options(warn = -1)
  }
  if (algo == "original") {
    res <- sampling(stanmodels$multi_steep_original, data = standat, ...)
  }
  if (algo == "fixed_sd") {
    res <- sampling(stanmodels$multi_steep_fixed_sd, data = standat, ...)
  }

  if (silent) {
    options(warn = 0)
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

  list(steepness = xres,
       cumwinprobs = cumwinprobs,
       ids = standat$ids,
       diagnostics = issues,
       stanfit = res,
       algo = algo)
}

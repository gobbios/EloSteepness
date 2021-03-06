#' steepness based on Bayesian Elo-rating
#' 
#' for interaction data with unknown sequence of observations
#'
#' @param mat square interaction matrix
#' @param algo character, either \code{"fixed_sd"} or \code{"original"}.
#'             This determines which algorithm is used. Default is
#'             \code{"fixed_sd"}, which is a slight modification from
#'             Goffe et al's original code.
#' @param n_rand numeric, number of randomized sequences. Default is
#'               \code{NULL}, which uses a rule of thumb to determine the
#'               number (see below for more details).
#' @param silent logical, suppress warnings (default is \code{FALSE})
#' @param ... additional arguments for \code{\link[rstan]{sampling}()}
#'
#' @details The number of randomizations is set in the following way, unless
#'          a specific number is provided. If there are more than 500
#'          observed interactions, \code{n_rand = 5}. If there are less than
#'          100 interactions, \code{n_rand = 50}. In the remaining cases,
#'          \code{n_rand = 20}.
#'          
#'          If the function call produces warnings about divergent transitions,
#'          large Rhat values or low effective sample sizes, increase the
#'          number of iterations (via \code{iter=}) and/or adjust the 
#'          sampling controls (e.g. 
#'          via \code{control = list(adapt_delta = 0.9)}).
#'          
#'          
#'           
#'          
#'
#' @importFrom rstan sampling extract
#' 
#' @return a list with results of the modelling fitting, containing the
#'         following list items:
#' \describe{
#'   \item{\code{steepness}}{a matrix with the posterior samples for steepness.
#'                    Each column corresponds to one randomization (as
#'                    set via \code{n_rand}). Each row is one iteration.}
#'   \item{\code{cumwinprobs}}{an array with posterior cumulative winning
#'         probabilities for each individual.}
#'   \item{\code{ids}}{a character vector with individual ID codes as supplied 
#'         in \code{mat}}
#'   \item{\code{diagnostics}}{a list with information regarding sampling 
#'         problems}
#'   \item{\code{stanfit}}{the actual \code{\link[rstan]{stanfit}} object}
#'   \item{\code{mat}}{the input matrix}
#'   \item{\code{algo}}{character, describing whether the original fitting 
#'         algorithm was used (\code{"original"}) or the one with fixed SD
#'         of start ratings (\code{"fixed_sd"})}
#'   \item{\code{sequence_supplied}}{logical, were data supplied as matrix
#'         (\code{FALSE}) or as sequence via winner/loser vector (\code{TRUE})}
#' }
#' 
#' @export
#'
#' @examples
#' data(dommats, package = "EloRating")
#' res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 1, cores = 2,
#'                                  iter = 1000, warmup = 500, refresh = 0)
#' plot_steepness(res)
#'
#' \donttest{
#' # use the original underlying algorithm by Goffe et al 2018
#' # will warn about divergent iterations and low effective sample sizes
#' # but warnings can be caught/suppressed by setting silent = TRUE
#' 
#' res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 1,
#'                                  algo = "original", silent = TRUE,
#'                                  iter = 1000, warmup = 500, refresh = 0)
#' res$diagnostics
#' 
#' # or the sampling can be tweaked to achieve better convergence:
#' # (this still might produce some divergent transitions on occasion)
#' res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 1, cores = 2,
#'                                  algo = "original", silent = TRUE,
#'                                  iter = 3000, warmup = 500, refresh = 0,
#'                                  control = list(adapt_delta = 0.99))
#' res$diagnostics
#' }

elo_steepness_from_matrix <- function(mat,
                                      algo = c("fixed_sd", "original"),
                                      n_rand = NULL,
                                      silent = FALSE,
                                      ...) {
  algo <- match.arg(algo)

  # determine number of randomizations
  if (is.null(n_rand)) {
    if (sum(mat) > 500) n_rand <- 5
    if (sum(mat) <= 500 & sum(mat) > 100) n_rand <- 20
    if (sum(mat) <= 100) n_rand <- 50
  }

  standat <- prep_data_for_rstan(mat = mat,
                                 n_rand = n_rand,
                                 silent = silent,
                                 for_elo_model = TRUE)

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

  # extract any sampling issues
  issues <- sampler_diagnostics(res)

  # steepness values
  xres <- extract(res, "steepness")$steepness
  # cum win probs
  cumwinprobs <- extract(res, "cumwinprobs")$cumwinprobs

  res <- list(steepness = xres,
              cumwinprobs = cumwinprobs,
              ids = standat$ids,
              diagnostics = issues,
              stanfit = res,
              mat = mat,
              algo = algo,
              sequence_supplied = FALSE
              )
  class(res) <- "elo_steepness"
  res
}

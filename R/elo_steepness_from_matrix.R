#' steepness based on Bayesian Elo-rating
#' 
#' for interaction data with unknown sequence of observations
#'
#' @param mat square interaction matrix
#' @param algo character, either \code{"fixed_sd"}, \code{"original"}, or
#'             \code{"fixed_k"}.This determines which algorithm
#'             to estimate Elo-ratings is used. Default is
#'             \code{"fixed_sd"}, which is a slight modification from
#'             Goffe et al's original code. \code{"fixed_k"} fixes the k 
#'             parameter ('shift coefficient' in Goffe et al) to
#'             the set value rather than estimating it from the data.
#' @param n_rand numeric, number of randomized sequences. Default is
#'               \code{NULL}, which uses a rule of thumb to determine the
#'               number (see below for more details).
#' @param silent logical, suppress warnings (default is \code{FALSE})
#' @param k numeric, provides a fixed k parameter. This only has effects if
#'          \code{algo = "fixed_k"}. At its default \code{NULL} a value of
#'          0.4 is used.
#' @param ... additional arguments for \code{\link[rstan]{sampling}()}
#'
#' @details The number of randomizations is set in the following way, unless
#'          a specific number is provided. If there are more than 500
#'          observed interactions, \code{n_rand = 5}. If there are less than
#'          100 interactions, \code{n_rand = 50}. In the remaining cases,
#'          \code{n_rand = 20}.
#'          
#' If the function call produces warnings about divergent transitions,
#'          large Rhat values or low effective sample sizes, increase the
#'          number of iterations (via \code{iter=}) and/or adjust the 
#'          sampling controls (e.g. 
#'          via \code{control = list(adapt_delta = 0.9)}).
#'          
#' If the argument \code{seed = } is supplied, its value will be passed to
#'          \code{\link[rstan]{sampling}()} to ensure reproducibility of the 
#'          MCMC sampling, but the same seed will then also apply 
#'          to the randomization of the interaction sequence order(s).
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
#'   \item{\code{k}}{an array with posterior k values.}
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
#' # using small numbers for iterations etc to speed up running time
#' res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 1, cores = 2,
#'                                  iter = 800, warmup = 300, 
#'                                  refresh = 0, chains = 2, seed = 1)
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
#' # (and the number of iterations should be set higher)
#' res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 1, chains = 2,
#'                                  algo = "original", silent = TRUE, seed = 1,
#'                                  iter = 1000, warmup = 500, refresh = 0,
#'                                  control = list(adapt_delta = 0.99))
#' res$diagnostics
#' }

elo_steepness_from_matrix <- function(mat,
                                      algo = c("fixed_sd", "original", "fixed_k"),
                                      n_rand = NULL,
                                      silent = FALSE,
                                      k = NULL,
                                      ...) {
  algo <- match.arg(algo)
  # catch additional arguments to check whether seed is supplied
  xargs <- list(...)
  if ("seed" %in% names(xargs)) {
    set.seed(xargs$seed)
  }

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

  # reset seed
  if ("seed" %in% names(xargs)) {
    set.seed(NULL)
  }

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
    standat$k <- rep(k, n_rand)
    if (n_rand == 1) dim(standat$k) <- 1
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
  # cum win probs
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
              mat = mat,
              algo = algo,
              sequence_supplied = FALSE
              )
  class(res) <- "elo_steepness"
  res
}

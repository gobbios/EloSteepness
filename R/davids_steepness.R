#' David's scores and steepness with Bayesian flavor
#'
#' @param mat square interaction matrix
#' @param silent logical, suppress warnings (default is \code{FALSE})
#' @param ... additional arguments for \code{\link[rstan]{sampling}()}
#'
#' @return a list with results of the modelling fitting, containing the 
#'         following list items:
#' \describe{
#'   \item{\code{steepness}}{a one-column matrix with the posterior samples 
#'         for steepness. Each row is one iteration.}
#'   \item{\code{norm_ds}}{an matrix with posterior normalized
#'         David's scores for each individual. Each column is one individual.
#'         Each row is one iteration.}
#'   \item{\code{ids}}{a character vector with individual ID codes as supplied
#'         in \code{mat}}
#'   \item{\code{diagnostics}}{a list with information regarding sampling 
#'         problems}
#'   \item{\code{stanfit}}{the actual \code{\link[rstan]{stanfit}} object}
#'   \item{\code{mat}}{the input matrix}
#' }
#' 
#' @export
#' @examples
#' data(dommats, package = "EloRating")
#' res <- davids_steepness(dommats$elephants, refresh = 0)
#' plot_steepness(res)

davids_steepness <- function(mat, 
                             silent = FALSE,
                             ...) {
  
  standat <- prep_data_for_rstan(mat = mat,
                                 for_elo_model = FALSE)
  
  if (silent) {
    res <- suppressWarnings(sampling(stanmodels$ds_steep, data = standat, ...))
  } else {
    res <- sampling(stanmodels$ds_steep, data = standat, ...)
  }
  

  # extract any sampling issues
  issues <- sampler_diagnostics(res)
  
  # steepness values
  xres <- extract(res, "xsteep")$xsteep[, 2, drop = FALSE]
  
  # normalized DS (equivalent to summed winning probabilities in Elo model)
  norm_ds <- extract(res, "normds")$normds

  res <- list(steepness = xres,
              norm_ds = norm_ds,
              ids = standat$ids,
              diagnostics = issues,
              stanfit = res,
              mat = mat)
  class(res) <- "david_steepness"
  res
}

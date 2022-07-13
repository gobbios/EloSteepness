#' David's scores and steepness with Bayesian flavor
#'
#' @param mat square interaction matrix
#' @param silent logical, suppress warnings (default is \code{FALSE})
#' @param ... additional arguments for \code{\link[rstan]{sampling}()}
#'
#' @return a list or stan fit object
#' @export
#' @examples
#' \dontrun{
#' data(dommats, package = "EloRating")
#' res <- davids_steepness(dommats$elephants, refresh = 0)
#' plot_steepness(res)
#' }

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
  
  # cumulative winning probabilities
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

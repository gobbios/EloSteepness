#' David's scores and steepness with Bayesian flavour
#'
#' @param mat square interaction matrix
#' @param ... arguments for \code{rstan::sampling}
#'
#' @return a list or stan fit object
#' @export
#' @examples 
#' \dontrun{
#' data(dommats, package = "EloRating")
#' res <- davids_steepness(dommats$elephants, refresh = 0)
#' plot_steepness(res)
#' }

davids_steepness <- function(mat, ...) {
  standat <- prep_data_for_rstan(mat = mat,
                                 for_elo_model = FALSE)

  res <- sampling(stanmodels$ds_steep, data = standat, ...)

  issues <- c(divergent = NA, energy = NA, depth = NA)
  # check_energy
  issues["energy"] <- sum(get_bfmi(res) < 0.2)
  # divergent iterations
  issues["divergent"] <- sum(get_divergent_iterations(res))
  # tree depth
  issues["depth"] <- sum(sum(get_max_treedepth_iterations(res)))

  issues <- list(has_issues = any(issues > 0), issues)

  # steepness values
  xres <- extract(res, "xsteep")$xsteep[, 2, drop = FALSE]
  # cum win probs
  norm_ds <- extract(res, "normds")$normds

  list(steepness = xres,
       norm_ds = norm_ds,
       ids = standat$ids,
       diagnostics = issues,
       stanfit = res)
}

#' catch Stan sampling issues without throwing a warning
#'
#' @param object \code{\link[rstan]{stanfit}} object
#'
#' @return a list regarding any sampling issues encountered during fitting
#' @importFrom rstan Rhat ess_bulk ess_tail get_bfmi
#' @importFrom rstan get_divergent_iterations get_max_treedepth_iterations

sampler_diagnostics <- function(object) {

  issues <- c(divergent = NA, energy = NA, depth = NA)
  issues2 <- c(bulk_ess = NA, tail_ess = NA, rhat = NA)

  # check_energy
  issues["energy"] <- sum(get_bfmi(object) < 0.2)
  # divergent iterations
  issues["divergent"] <- sum(get_divergent_iterations(object))
  # tree depth
  issues["depth"] <- sum(sum(get_max_treedepth_iterations(object)))

  # essentially identical to rstan::throw_sampler_warnings
  sims <- as.array(object)
  issues2["rhat"] <- sum(apply(sims, MARGIN = 3, FUN = Rhat) > 1.05)
  temp <- apply(sims, MARGIN = 3, FUN = ess_bulk)
  issues2["bulk_ess"] <- sum(temp < 100 * ncol(sims))
  temp <- apply(sims, MARGIN = 3, FUN = ess_tail)
  issues2["tail_ess"] <- sum(temp < 100 * ncol(sims))

  issues <- list(has_issues = any(issues > 0) | any(issues2 > 0),
                 per_sample = issues, 
                 per_parameter = issues2)
}

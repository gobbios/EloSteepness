#' summary
#'
#' @param object result from \code{\link{elo_steepness_from_matrix}},
#'        \code{\link{elo_steepness_from_sequence}} or
#'        \code{\link{davids_steepness}}
#' @param ... further arguments passed to or from other methods (ignored)
#' @importFrom EloRating prunk creatematrix
#' @return Nothing returned. Called for side effects of textual output 
#'         to console.
#'
#' @export
summary.elo_steepness <- function(object, ...) {

  a <- object$algo
  if (a == "fixed_sd") {
    a <- "fixed"
  } else {
    a <- "estimated"
  }
  if (object$algo == "fixed_k") a <- "fixed"
  
  nrand <- ncol(object$steepness)
  cat("steepness based on Bayesian Elo-ratings\n")
  cat("(using", a, "standard deviation of start ratings)\n")
  cat("---------------------------------------\n")
  if (object$sequence_supplied) {
    cat("data supplied in sequence format:\n")
    cat("no sequence randomization performed\n")
  } else {
    cat("data supplied in matrix format:\n")
    cat("number of randomized sequences:", nrand, "\n")
  }
  cat("---------------------------------------\n")
  cat("total number of posterior samples generated:",
      length(object$steepness),
      "\n")
  cat("number of samples with issues:",
      sum(object$diagnostics$per_sample),
      "\n")
  cat("number of parameters with issues:",
      sum(object$diagnostics$per_parameter),
      "\n")
  cat("---------------------------------------\n")

  if (object$sequence_supplied) {
    m <- creatematrix(winners = object$mat[, "winner"],
                      losers = object$mat[, "loser"])
  } else {
    m <- object$mat
  }

  n_id <- length(object$ids)

  cat("matrix with",
      sum(m), "interactions between",
      n_id, "individuals", "\n")

  cat(sprintf("%.1f", round(sum(m) / n_id, 2)),
      "interactions per individual\n")
  cat(sprintf("%.1f", round(sum(m) / ((n_id - 1) * n_id * 0.5), 2)),
      "interactions per dyad\n")
  x <- as.numeric(prunk(m)[1])

  cat("proportion of unknown relationships:", sprintf("%.3f", x), "\n")
  if (object$algo == "fixed_k") {
    cat("k value was fixed at", object$k, "\n")
  } else {
    kvals <- as.numeric(object$k)
    kout <- sprintf("%.2f", quantile(kvals, probs = c(0.055, 0.5, 0.945)))
    cat("k value was estimated with median =", kout[2], 
        paste0("(89% CI: ", kout[1], "-", kout[3], ")"), "\n")
  }
  
  cat("------------------------------------------\n")

  xres <- steepness_precis(object, quantiles = c(0.055, 0.25, 0.5, 0.75, 0.945))

  cat("mean steepness is", sprintf("%.2f", xres$mean[1]),
      paste0("(SD=", sprintf("%.2f", xres$sd[1]), ")"), "\n")
  cat("median steepness is", sprintf("%.2f", xres$mean[1]),
      paste0("(MAD=", sprintf("%.2f", xres$mad[1]), ")"), "\n")
  cat("89% credible interval is between",
      sprintf("%.2f", xres$q055[1]), "and",
      sprintf("%.2f", xres$q945[1]), "\n")
}

#' @rdname summary.elo_steepness
#' @export
summary.david_steepness <- function(object, ...) {
  cat("steepness based on Bayesian David's scores\n")
  cat("------------------------------------------\n")

  cat("total number of posterior samples generated:",
      length(object$steepness),
      "\n")
  cat("number of samples with issues:",
      sum(object$diagnostics$per_sample),
      "\n")
  cat("number of parameters with issues:",
      sum(object$diagnostics$per_parameter),
      "\n")
  cat("---------------------------------------\n")

  m <- object$mat

  cat("matrix with",
      sum(m), "interactions between",
      ncol(m), "individuals", "\n")

  cat(sprintf("%.1f", round(sum(m) / ncol(m), 2)),
      "interactions per individual\n")
  cat(sprintf("%.1f", round(sum(m) / ((ncol(m) - 1) * ncol(m) * 0.5), 2)),
      "interactions per dyad\n")
  x <- as.numeric(prunk(m)[1])
  cat("proportion of unknown relationships:", sprintf("%.3f", x), "\n")

  cat("------------------------------------------\n")

  xres <- steepness_precis(object, quantiles = c(0.055, 0.25, 0.5, 0.75, 0.945))

  cat("mean steepness is", sprintf("%.2f", xres$mean[1]),
      paste0("(SD=", sprintf("%.2f", xres$sd[1]), ")"), "\n")
  cat("median steepness is", sprintf("%.2f", xres$mean[1]),
      paste0("(MAD=", sprintf("%.2f", xres$mad[1]), ")"), "\n")
  cat("89% credible interval is between",
      sprintf("%.2f", xres$q055[1]), "and",
      sprintf("%.2f", xres$q945[1]), "\n")
}

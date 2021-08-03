#' summary
#'
#' @param object result from \code{\link{elo_steepness_from_matrix}} or
#'        \code{\link{davids_steepness}}
#' @param ... further arguments passed to or from other methods (ignored)
#' @importFrom EloRating prunk
#'
#' @export
summary.elo_steepness <- function(object, ...) {
  
  a <- object$algo
  if (a == "fixed_sd") {
    a <- "fixed"
  } else {
    a <- "estimated"
  }
  nrand <- ncol(object$steepness)
  cat("steepness based on Bayesian Elo-ratings\n")
  cat("---------------------------------------\n")
  cat("(using", a, "standard deviation of starting ratings)\n")
  cat("number of randomized sequences:", nrand, "\n")
  
  cat("total number of posterior samples generated:", length(object$steepness), "\n")
  cat("number of samples with issues:", sum(object$diagnostics[[2]]), "\n")
  
  m <- object$mat
  
  cat("matrix with", 
      sum(m), "interactions between", 
      ncol(m), "individuals", "\n")

  cat(sprintf("%.1f", round(sum(m) / ncol(m), 2)), 
      "interactons per individual\n")
  x <- as.numeric(prunk(m)[1])
  cat("proportion of unknown relationships:", sprintf("%.3f", x), "\n")
  
  cat("------------------------------------------\n")
  
  xres <- steepness_precis(object)
  
  cat("mean steepness is", sprintf("%.2f", xres$mean[1]), 
      paste0("(SD=", sprintf("%.2f", xres$sd[1]), ")"), "\n")
  cat("median steepness is", sprintf("%.2f", xres$mean[1]), 
      paste0("(MAD=", sprintf("%.2f", xres$mad[1]), ")"), "\n")
  cat("89% credible interval is between", 
      sprintf("%.2f", xres$q045[1]), "and", 
      sprintf("%.2f", xres$q955[1]), "\n")
}

#' @export
summary.david_steepness <- function(object, ...) {
  cat("steepness based on Bayesian David's scores\n")
  cat("------------------------------------------\n")
  
  cat("total number of posterior samples generated:", length(object$steepness), "\n")
  cat("number of samples with issues:", sum(object$diagnostics[[2]]), "\n")
  
  
  m <- object$mat
  
  cat("matrix with", 
      sum(m), "interactions between", 
      ncol(m), "individuals", "\n")
  
  cat(sprintf("%.1f", round(sum(m) / ncol(m), 2)), 
      "interactons per individual\n")
  x <- as.numeric(prunk(m)[1])
  cat("proportion of unknown relationships:", sprintf("%.3f", x), "\n")
  
  cat("------------------------------------------\n")
  
  xres <- steepness_precis(object)
  
  cat("mean steepness is", sprintf("%.2f", xres$mean[1]), 
      paste0("(SD=", sprintf("%.2f", xres$sd[1]), ")"), "\n")
  cat("median steepness is", sprintf("%.2f", xres$mean[1]), 
      paste0("(MAD=", sprintf("%.2f", xres$mad[1]), ")"), "\n")
  cat("89% credible interval is between", 
      sprintf("%.2f", xres$q045[1]), "and", 
      sprintf("%.2f", xres$q955[1]), "\n")
}

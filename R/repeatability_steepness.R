#' steepness via repeatability (cf aniDom package)
#'
#' @param mat square interaction matrix
#' @param n_rand numeric, number of randomized sequences
#'               (default is \code{1000})
#' @references Sanchez-Tojar et al 2018
#' @return a steepness value
#' @importFrom aniDom estimate_uncertainty_by_repeatability
#' @importFrom EloRating mat2seq
#' @export
#'
#' @examples
#' data(bonobos, package = "EloRating")
#' repeatability_steepness(bonobos, n_rand = 20)

repeatability_steepness <- function(mat, n_rand = 1000) {
  colnames(mat) <- NULL
  rownames(mat) <- NULL
  s <- mat2seq(mat)
  s <- s[sample(seq_len(nrow(s))), ]
  res <- catch_warnings(estimate_uncertainty_by_repeatability(winners = s[, 1],
                                                              losers = s[, 2],
                                                              n.rands = n_rand))

  list(steepness = res$value,
       has_issues = !is.null(res$warning))
}

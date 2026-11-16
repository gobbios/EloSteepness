#' steepness via repeatability (cf aniDom package)
#' 
#' function written for easy comparison with other methods for validation study
#'
#' @param mat square interaction matrix
#' @param n_rand numeric, number of randomized sequences
#'               (default is \code{1000})
#' @references Sanchez-Tojar et al 2018
#' @return a steepness value
#' @importFrom lme4 lmer VarCorr
#' @importFrom EloRating mat2seq
#' @export
#' 
#' @details
#' The function was initially written as a wrapper around \code{aniDom::estimate_uncertainty_by_repeatability}.
#' This version removed the dependency on \code{aniDom}.
#' The corresponding code snippet:
#' \preformatted{
#' estimate_uncertainty_by_repeatability(
#'  winners = s[, 1],
#'  losers = s[, 2],
#'  n.rands = n_rand)
#' }
#'
#' @examples
#' data(bonobos, package = "EloRating")
#' repeatability_steepness(bonobos, n_rand = 20)

repeatability_steepness <- function(mat, n_rand = 1000) {
  colnames(mat) <- NULL
  rownames(mat) <- NULL
  s <- mat2seq(mat)
  
  # helper function (replaces elo_scores from aniDom)
  elo_scores_anidom <- function(winner, loser) {
    scores <- numeric(max(winner, loser))
    for (i in seq_along(winner)) {
      w <- winner[i]
      l <- loser[i]
      delta <- 200 * (1 - 1 / (1 + exp(-(scores[w] - scores[l]) / 100)))
      scores[w] <- scores[w] + delta
      scores[l] <- scores[l] - delta
    }
    cbind(id = seq_along(scores), scores)
  }
  
  aux <- list()
  for (i in seq_len(n_rand)) {
    s <- s[sample(nrow(s)), ]
    # temp <- elo_scores_anidom(winner = s[, 1], loser = s[, 2])
    aux[[length(aux) + 1]] <- elo_scores_anidom(winner = s[, 1], loser = s[, 2])
  }
  aux <- data.frame(do.call("rbind", aux))
  # fit lmm for repeatability
  lmmres <- catch_warnings(lmer(scores ~ 1 + (1|id), data = aux))
  aux <- data.frame(VarCorr(lmmres$value))
  res <- aux$vcov[aux$grp == "id"] / sum(aux$vcov)
  
  list(steepness = res,
       has_issues = !is.null(lmmres$warning))

}

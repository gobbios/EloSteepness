#' generate dominance interactions with specified steepness
#'
#' @param n_ind integer, the number of individuals
#' @param n_int integer, the number of interactions
#' @param steep numeric (between 0 and 1), the desired steepness value
#' @param id_bias numeric, between 0 and 1. If 0 all individual are equally
#'        likely to interact. If 1, some individuals have higher propensities
#'        to interact
#' @param rank_bias numeric, between 0 and 1. If 0 there is no relationship
#'        between rank distance and interaction propensity. If 1 there is a
#'        strong relationship: dyads closer in rank interact more often.
#' @importFrom stats runif pnorm
#' @importFrom utils combn
#' @return a list with the first item being the interactions in sequence form,
#'         the second item as matrix and the last a list with input settings
#' @export
#'
#' @examples
#' res <- simple_steep_gen(n_ind = 5, n_int = 30, steep = 0.99)
#' res$sequence
#' res$matrix
#'
#'
#' library(EloRating)
#' steeps <- runif(20, 0, 1)
#' nids <- sample(6:10, length(steeps), TRUE)
#' mats <- sapply(1:length(steeps), function(x) {
#'   simple_steep_gen(nids[x], nids[x] ^ 2.5, steeps[x], 0)[[2]]
#'  })
#' obs_steeps <- unlist(lapply(mats, function(x)steepness(x)[1]))
#' plot(steeps, obs_steeps, xlim = c(0, 1), ylim = c(0, 1))
#' abline(0, 1)


simple_steep_gen <- function(n_ind,
                             n_int,
                             steep,
                             id_bias = 0,
                             rank_bias = 0) {
  # weights for dyads (and ids)
  dyad_weights <- generate_interaction_probs(n_ind = n_ind,
                                             id_bias = id_bias,
                                             rank_bias = rank_bias)

  # the number of dyads
  d <- seq_len(nrow(dyad_weights))
  # output matrix
  m <- matrix(ncol = n_ind, nrow = n_ind, 0)
  # output sequence
  s <- matrix(ncol = 2, nrow = n_int)

  for (k in seq_len(n_int)) {
    x <- dyad_weights[sample(d, size = 1, prob = dyad_weights[, "final"]), 1:2]
    if (runif(1, 0, 1) > (steep + 1) / 2) {
      x <- rev(x)
    }
    m[x[1], x[2]] <- m[x[1], x[2]] + 1
    s[k, ] <- x
  }

  colnames(m) <- paste0("i_", seq_len(n_ind))
  rownames(m) <- colnames(m)

  s[, 1] <- colnames(m)[s[, 1]]
  s[, 2] <- colnames(m)[as.numeric(s[, 2])]
  list(sequence = s,
       matrix = m,
       settings = list(dyads = dyad_weights,
                       id_bias = id_bias,
                       rank_bias = rank_bias,
                       steep = steep))
}

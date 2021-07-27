#' generate dyadic interaction probabilities for a group with fixed individual
#' and dyadic biases
#'
#' @param n_ind numeric, number of individuals
#' @param id_bias numeric, between 0 and 1. If 0 all individual are equally
#'        likely to interact. If 1, some individuals have higher propensities
#'        to interact
#' @param rank_bias numeric, between 0 and 1. If 0 there is no relationship
#'        between rank distance and interaction propensity. If 1 there is a
#'        strong relationship: dyads closer in rank interact more often.
#' @importFrom stats pnorm runif
#' @importFrom utils combn
#' @return a matrix
#' @export
#'
#' @examples
#'
#'
#' x <- generate_interaction_probs(n_ind = 10, id_bias = 0.2, rank_bias = 1)
#' rankdiff <- x[, 2] - x[, 1]
#' interactprob <- x[, "final"]
#' # closer in rank (smaller rank diff) = interaction more likely
#' plot(rankdiff, interactprob)
#'
#' x <- generate_interaction_probs(n_ind = 10, id_bias = 0.2, rank_bias = 0)
#' rankdiff <- x[, 2] - x[, 1]
#' interactprob <- x[, "final"]
#' # approx. equal probs for all dyads regardless of rank diff
#' plot(rankdiff, interactprob)
#'
#'
#' x <- generate_interaction_probs(n_ind = 10, id_bias = 0, rank_bias = 0)
#' interactprob <- x[, "final"]
#' y <- sample(1:nrow(x), 1000, replace = TRUE, prob = interactprob)
#' y <- as.numeric(x[y, 1:2])
#' # approx. equal numbers of interactions per ID
#' sort(table(y))
#'
#' # skewed interaction numbers
#' x <- generate_interaction_probs(n_ind = 10, id_bias = 1, rank_bias = 0)
#' interactprob <- x[, "final"]
#' y <- sample(1:nrow(x), 1000, replace = TRUE, prob = interactprob)
#' y <- as.numeric(x[y, 1:2])
#' sort(table(y))

generate_interaction_probs <- function(n_ind, id_bias = 0, rank_bias = 0) {
  if (id_bias == 1) {
    id_bias <- 0.9999
  }
  if (id_bias == 0) {
    id_bias <- 0.0001
  }
  res <- runif(n_ind, min = 1 - id_bias, max = 1)
  res <- res / sum(res)

  ids <- t(combn(seq_len(n_ind), 2))
  ids <- cbind(ids, id1 = res[ids[, 1]], id2 = res[ids[, 2]])


  ids <- cbind(ids, dyad = ids[, "id1"] + ids[, "id2"])
  ids[, "id1"] <- ids[, "id1"] / sum(ids[, "id1"])
  ids[, "id2"] <- ids[, "id2"] / sum(ids[, "id2"])
  ids[, "dyad"] <- ids[, "dyad"] / sum(ids[, "dyad"])

  rankdiff <- ids[, 2] - ids[, 1]
  rankdiff <- pnorm(as.numeric(rank_bias * scale(rankdiff) * (-1)))
  rankdiff <- rankdiff / sum(rankdiff)
  final <- (ids[, "id1"] + ids[, "id2"]) * rankdiff # / sum(rankdiff)
  ids <- cbind(ids, final = final / sum(final))

  ids
}

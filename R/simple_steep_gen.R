#' generate dominance interactions with specified steepness
#'
#' @param n_ind integer, the number of individuals
#' @param n_int integer, the number of interactions
#' @param steep numeric (between 0 and 1), the desired steepness value
#' @param id_bias numeric, between 0 and 1. If 0 all individual are equally
#'        likely to interact. If 1, some individuals have higher propensities
#'        to interact.
#' @param rank_bias numeric, between 0 and 1. If 0 there is no relationship
#'        between rank distance and interaction propensity. If 1 there is a
#'        strong relationship: dyads closer in rank interact more often.
#' @param sequential logical, default is \code{TRUE}. See details.
#' @importFrom stats runif pnorm rbinom
#' @importFrom utils combn
#' @return a list with the first item being the interactions in sequence form
#'         (\code{$sequence}). The second item (\code{$matrix}) is the 
#'         square interaction matrix and the third item (\code{$settings}) 
#'         is a list with input settings (including probabilities to interact
#'         for each dyad).
#' @export
#' 
#' @details Initially (and this is still the default), the function generated 
#'          interactions and their outcomes sequentially: first a dyad 
#'          was chosen that interacted and then its winner was determined. 
#'          This was repeated for as many interactions as set by \code{n_int=}. 
#' 
#' @details The same results can be achieved much
#'          more efficiently by first setting the number of interactions per 
#'          dyad and then looping through all dyads and then generate the 
#'          interactions and their outcomes per dyad. This can be achieved by
#'          setting \code{sequential = FALSE}. In this latter case the
#'          'sequence' of interactions reported in the results is just a
#'          randomized version of all interactions, whereas in the former case
#'          there is a 'natural sequence' (although it is meaningless because
#'          the sequence is irrelevant with respect to outcomes of individual
#'          interactions (the system is stable)).
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
                             rank_bias = 0,
                             sequential = TRUE) {
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
  
  if (sequential) {
    for (k in seq_len(n_int)) {
      x <- dyad_weights[sample(x = d, size = 1, 
                               prob = dyad_weights[, "final"]), 1:2]
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
  }
  
  if (!sequential) {
    # determine interactions per dyad
    d2 <- factor(d, levels = d)
    n_per_dyad <- table(sample(x = d2, size = n_int, replace = TRUE, 
                               prob = dyad_weights[, "final"]))
    for (i in seq_along(d)) {
      m[dyad_weights[i, 1], dyad_weights[i, 2]] <- 
        sum(rbinom(n_per_dyad[i], 1, (steep + 1)/2))
      m[dyad_weights[i, 2], dyad_weights[i, 1]] <-
        n_per_dyad[i] - m[dyad_weights[i, 1], dyad_weights[i, 2]]
    }

    colnames(m) <- paste0("i_", seq_len(n_ind))
    rownames(m) <- colnames(m)
    s <- mat2seq(m)[sample(seq_len(n_int)), ]
    rownames(s) <- NULL
    colnames(s) <- NULL
    s <- apply(s, 2, as.character)
  }

  list(sequence = s,
       matrix = m,
       settings = list(dyads = dyad_weights,
                       id_bias = id_bias,
                       rank_bias = rank_bias,
                       steep = steep))
}

#' prepare data for stan call
#'
#' @param mat square interaction matrix
#' @param n_rand numeric, number of randomizations
#' @param silent logical, omit printing messages regarding non-fatal data
#'               issues. Default is \code{FALSE}, i.e. do print messages.
#' @param for_elo_model logical, output ready for Elo steepness (default,
#'        \code{TRUE}). If \code{FALSE}, prep for David's score steepness.
#' @importFrom utils getFromNamespace
#'
#' @return a list that is formatted so that it can be handed over to the
#'         respective Stan models
#'
prep_data_for_rstan <- function(mat,
                                n_rand = 1,
                                silent = FALSE,
                                for_elo_model = TRUE) {

  # clean matrix (for both Elo and DS)
  # get individual names/codes if present
  if (!is.null(colnames(mat))) {
    ids <- colnames(mat)
  } else {
    ids <- NULL
  }

  # check and remove for individuals without interactions from matrix
  xvec <- rowSums(mat, na.rm = TRUE) + colSums(mat, na.rm = TRUE) == 0
  if (any(xvec)) {
    if (!silent) {
      message(sum(xvec),
              " individual(s) without interactions removed from data")
    }
    mat <- mat[!xvec, !xvec]
    if (!is.null(colnames(mat))) {
      ids <- ids[!xvec]
    }
  }

  # check diagonal
  diag(mat)[is.na(diag(mat))] <- 0
  xvec <- diag(mat) != 0
  if (any(xvec)) {
    diag(mat) <- 0
    if (!silent) {
      message(sum(xvec),
              " diagonal values were not 0 (nor NA) and were replaced by 0")
    }
  }

  if (for_elo_model) {
    # fast function to transform matrix into sequence
    m_foo <- getFromNamespace("mat2seqint", "EloRating")

    # generate sequences
    n <- sum(mat)
    x <- m_foo(mat)

    locmat <- matrix(rep(seq_len(n), n_rand), ncol = n_rand)
    locmat <- apply(locmat, 2, sample)

    # winner and loser index matrices
    winnermat <- matrix(x[[1]][locmat], ncol = n_rand)
    losermat <- matrix(x[[2]][locmat], ncol = n_rand)

    outlist <- list(winner = winnermat,
                    loser = losermat,
                    diff_f = 1,
                    N = n,
                    K = ncol(mat),
                    n_rand = n_rand)
    outlist$presence <- matrix(ncol = outlist$K, nrow = outlist$N, 1)
    outlist$y <- rep(1, outlist$N)
    outlist$ids <- ids
  }

  if (!for_elo_model) {
    locs <- which(upper.tri(mat), arr.ind = TRUE)

    ints <- c()
    dyad <- c()
    i1 <- c()
    i2 <- c()

    for (i in seq_len(nrow(locs))) {
      x <- c(rep(1, mat[locs[i, 1], locs[i, 2]]),
             rep(0, mat[locs[i, 2], locs[i, 1]]))
      nx <- length(x)
      if (nx > 0) {
        ints <- c(ints, x)
        dyad <- c(dyad, rep(i, nx))
        i1 <- c(i1, rep(locs[i, 1], nx))
        i2 <- c(i2, rep(locs[i, 2], nx))
      }
    }

    outlist <- list(N = sum(mat),
                    K = nrow(locs),
                    I = nrow(mat),
                    interactions = ints,
                    dyad = dyad,
                    id1 = as.numeric(i1),
                    id2 = as.numeric(i2),
                    locs = locs,
                    m = mat)
    outlist$ids <- ids
  }

  outlist
}

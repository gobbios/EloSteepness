#' remove interactions from matrix to increase sparseness
#' @importFrom EloRating prunk
#' @param m input matrix
#' @param removal_mode character, should interactions be removed interaction by
#'        interaction (\code{"by_interaction"}), or by removing one dyad
#'        entirely at a time (\code{"by_dyad"}). Default is \code{"mix"}, i.e.
#'        a random mix between the two strategies.
#' @param stop_at numeric, fraction of unknown relationships to be reached
#' @param max_out numeric, the number of matrices to be returned maximally.
#'        This is useful if the input matrix is fairly large. If set, this
#'        will return the input matrix plus \code{max_out} randomly selected
#'        matrices from the remaining produced matrices. So in fact, the output
#'        comprises \code{max_out + 1} matrices (subject to the \code{stop_at}
#'        specification).
#'
#' @return a list with two items. \code{$summary} is a data frame with an
#'         overview. \code{matrices} contains the actual interaction
#'         matrices with increasing proportion of unknown relationships.
#'
#' @export
#' @examples
#' data(bonobos)
#' res <- remove_dyads(bonobos)
#' res$summary
#' length(res$matrices)
#' lapply(res$matrices, prunk)
#'
#' res <- remove_dyads(bonobos, max_out = 2)
#' # first plus two randomly selected = 3 matrices
#' length(res$matrices)
#' res$summary

remove_dyads <- function(m,
                         removal_mode = c("mix", "by_interaction", "by_dyad"),
                         stop_at = 0.5,
                         max_out = NULL) {

  removal_mode <- match.arg(removal_mode)

  # some prep
  diag(m)[is.na(diag(m))] <- 0
  if (any(diag(m) != 0)) {
    message("non-zero entries in diagonal occurred and were replaced by zeros")
  }
  diag(m)[diag(m) != 0] <- 0

  n_ind <- ncol(m)
  dyads <- t(combn(seq_len(n_ind), 2))
  # which dyads have interactions
  has_i <- apply(dyads, 1, function(x) m[x[1], x[2]] + m[x[2], x[1]]) > 0
  # calculate cut-offs
  max_remove <- floor(nrow(dyads) * stop_at)
  min_remove <- sum(!has_i) + 1
  if (max_remove < min_remove) {
    stop("input is already more sparse than desired output", call. = FALSE)
  }
  # prepare output
  outdata <- data.frame(step = (min_remove - 1):max_remove, prunk = NA)
  if (removal_mode == "mix") {
    outdata$by_interaction <- sample(c(TRUE, FALSE),
                                     size = nrow(outdata),
                                     replace = TRUE)
  }
  if (removal_mode == "by_interaction") {
    outdata$by_interaction <- TRUE
  }
  if (removal_mode == "by_dyad") {
    outdata$by_interaction <- FALSE
  }
  outdata$by_interaction[1] <- NA
  outmats <- list(m)

  for (i in 2:nrow(outdata)) {
    if (outdata$by_interaction[i]) {
      current_prunk <- prunk(m)[1]
      done <- FALSE
      while (!done) {
        # select cells and remove interactions until one more dyad
        #    has zero interactions
        cell <- sample(which(m > 0), 1)
        m[cell] <- m[cell] - 1
        new_prunk <- prunk(m)[1]
        if (new_prunk > current_prunk) {
          outmats[[length(outmats) + 1]] <- m
          done <- TRUE
        }
      }
      has_i <- apply(dyads, 1, function(x) m[x[1], x[2]] + m[x[2], x[1]]) > 0
    }

    if (!outdata$by_interaction[i]) {
      d <- sample(seq_len(nrow(dyads)), 1, prob = has_i)
      m[dyads[d, 1], dyads[d, 2]] <- 0
      m[dyads[d, 2], dyads[d, 1]] <- 0
      outmats[[length(outmats) + 1]] <- m
      has_i[d] <- FALSE
    }
  }

  outdata$prunk <- unlist(lapply(outmats, function(x) prunk(x)[1]))
  outdata$n_int <- unlist(lapply(outmats, sum))
  foo <- function(x) sum(colSums(x) + rowSums(x) == 0)
  outdata$n_zero <- unlist(lapply(outmats, foo))

  if (!is.null(max_out)) {
    if (nrow(outdata) > (max_out + 1)) {
      xwhich <- c(1, sort(sample(2:nrow(outdata), max_out)))
      outdata <- outdata[xwhich, ]
      outmats <- outmats[xwhich]
    }
  }

  list(summary = outdata, matrices = outmats)
}

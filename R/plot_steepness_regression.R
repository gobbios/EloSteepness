#' plot steepness regression
#' 
#' visually combine individual scores with group-level steepness
#'
#' @param x result from \code{\link{elo_steepness_from_matrix}},
#'        \code{\link{elo_steepness_from_sequence}} or
#'        \code{\link{davids_steepness}}
#' @param adjust numeric, parameter for smoothing posterior of individual scores
#' @param color logical, default is \code{TRUE} where individuals get color-
#'        coded. If \code{FALSE}: a gray scale is used. It is also possible
#'        to hand over a vector with colors, which then must be correspond
#'        in length to the number of individuals.
#' @param width_fac numeric, relative width of posterior distributions. This is
#'        actually affects the 'height' but since the posteriors are rotated it
#'        visually represents width.
#' @param axis_extend numeric, an extension factor to extend the horizontal
#'        axis to leave space for the posteriors. When set to 0 the axis stops
#'        at \emph{n} (the number of individuals, which represents the
#'        lowest rank).
#'
#' @return a plot
#' @export
#' @importFrom stats density coefficients lm
#' @importFrom grDevices adjustcolor grey
#' @importFrom graphics box
#'
#' @examples
#' data("bonobos", package = "EloRating")
#' res <- davids_steepness(bonobos, refresh = 0, iter = 1000)
#' plot_steepness_regression(res, width_fac = 0.5)


plot_steepness_regression <- function(x,
                                      adjust = 3,
                                      color = TRUE,
                                      width_fac = 0.1,
                                      axis_extend = 0.1) {

  correct_object <- FALSE

  if ("cumwinprobs" %in% names(x)) {
    y <- x$cumwinprobs
    scores <- matrix(ncol = dim(y)[3], nrow = length(y[, , 1]))
    for (i in seq_len(ncol(scores))) {
      scores[, i] <- y[, , i]
    }
    ylab <- "summed Elo winning probability"
    correct_object <- TRUE
  }
  if ("norm_ds" %in% names(x)) {
    scores <- x$norm_ds
    ylab <- "David's score (normalized)"
    correct_object <- TRUE
  }

  if (!correct_object) {
    stop("object 'x' not of correct format")
  }

  rranks <- t(apply(-scores, 1, rank))
  ranks <- colMeans(rranks)
  pdata <- apply(scores, 2, density, adjust = adjust)
  n <- length(pdata)

  # deal with colors
  if (!isFALSE(color) & !isTRUE(color) & !is.null(color)) {
    cols <- NULL
    if (length(color) == n) {
      cols <- color
    }
    if (length(color) == 1) {
      cols <- rep(color, n)
    }
    if (is.null(cols)) {
      stop("colour vector does not match number of ids")
    }
  }

  if (isTRUE(color)) {
    cols <- sample(hcl.colors(n = n, "zissou", alpha = 0.7))
  }
  if (isFALSE(color)) {
    cols <- sample(gray.colors(n = n, start = 0.3, end = 0.9, alpha = 0.7))
  }

  plot(0, 0, type = "n", xlim = c(1, n * (1 + axis_extend)), ylim = c(0, n - 1),
       las = 1, xlab = "", ylab = "",
       axes = FALSE, yaxs = "i")
  axis(1, at = 1:n, mgp = c(2, 0.7, 0), tcl = -0.3)
  axis(2, at = 0:(n - 1), las = 1, mgp = c(2, 0.7, 0), tcl = -0.3)
  box(bty = "l")
  title(xlab = "mean ordinal rank", line = 1.8)
  title(ylab = ylab, line = 1.8)

  xlines <- t(sapply(sample(nrow(scores), 1000), function(x) {
    coefficients(lm(scores[x, ] ~ rranks[x, ]))
  }))
  xrange <- range(ranks)

  invisible(sapply(seq_len(nrow(xlines)), function(x) {
    points(xrange,
           c(xlines[x, 1] + xlines[x, 2] * xrange[1],
             xlines[x, 1] + xlines[x, 2] * xrange[2]),
           lwd = 0.5, type = "l", col = grey(0.2, 0.1))
  }))

  for (i in seq_len(length(ranks))) {
    at <- ranks[i]
    x <- pdata[[i]]
    polygon(x = c(x$y * width_fac + at, rep(at, length(x$x))),
            y = c(x$x, rev(x$x)),
            border = NA, col = cols[i])
    rm(x, at)
  }
  points(ranks, apply(scores, 2, median), pch = 21, bg = "grey90",
         col = adjustcolor(cols, alpha.f = 1), lwd = 3, cex = 1.5, xpd = TRUE)
}

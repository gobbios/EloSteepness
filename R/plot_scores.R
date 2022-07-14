#' plot individual scores (either Elo or David)
#'
#' @param x result from \code{\link{elo_steepness_from_matrix}},
#'        \code{\link{elo_steepness_from_sequence}} or
#'        \code{\link{davids_steepness}}
#' @param adjustpar numeric, parameter for smoothing posterior of individual
#'        scores
#' @param color logical, default is \code{TRUE} where individuals get 
#'        color-coded. If \code{FALSE}: a gray scale is used. It is 
#'        also possible to hand over a vector with colors, which then 
#'        must correspond in length to the number of individuals.
#' @param subset_ids character, plot only those individual codes. Default is
#'        \code{NULL}, i.e. all individuals are included in the plot.
#' @param include_others logical, should other IDs (those \emph{not} in
#'        \code{subset_ids}) be included as contours. Default is \code{TRUE}.
#'        This only has an effect if \code{subset_ids} is different from
#'        \code{NULL},
#'
#' @return a plot
#' @export
#' @importFrom stats density
#' @importFrom graphics polygon axis points
#' @importFrom grDevices hcl.colors gray.colors
#'
#' @examples
#' data(dommats, package = "EloRating")
#' 
#' res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 1,
#'                                  silent = TRUE, refresh = 0,
#'                                  iter = 1000, warmup = 500)
#' plot_scores(res)
#' 
#' res <- davids_steepness(dommats$elephants, refresh = 0)
#' plot_scores(res)
#' plot_scores(res, color = FALSE)
#' plot_scores(res, adjustpar = 0.3)

plot_scores <- function(x,
                        adjustpar = 4,
                        color = TRUE,
                        subset_ids = NULL,
                        include_others = TRUE) {
  correct_object <- FALSE
  if ("cumwinprobs" %in% names(x)) {
    y <- x$cumwinprobs
    res <- matrix(ncol = dim(y)[3], nrow = length(y[, , 1]))
    for (i in seq_len(ncol(res))) {
      res[, i] <- y[, , i]
    }
    xlab <- "cumulative Elo winning probability"
    correct_object <- TRUE
  }
  if ("norm_ds" %in% names(x)) {
    res <- x$norm_ds
    xlab <- "David's score (normalized)"
    correct_object <- TRUE
  }

  if (!correct_object) {
    stop("object 'x' not of correct format")
  }

  n_ids <- ncol(res)

  if (!is.null(subset_ids)) {
    colnames(res) <- x$ids
    cn_locs <- which(!x$ids %in% subset_ids)
  }

  # prep data and set axis limits
  pdata <- apply(res, 2, density, adjust = adjustpar)
  pmax <- max(unlist(lapply(pdata, function(x) max(x$y))))
  xl <- c(0, n_ids - 1)
  yl <- c(0, pmax * 1.05)

  # deal with colors
  if (!isFALSE(color) & !isTRUE(color) & !is.null(color)) {
    cols <- NULL
    if (length(color) == n_ids) {
      cols <- color
    }
    if (length(color) == 1) {
      cols <- rep(color, n_ids)
    }
    if (is.null(cols)) {
      stop("colour vector does not match number of ids")
    }
  }

  if (isTRUE(color)) {
    cols <- sample(hcl.colors(n = n_ids, "zissou", alpha = 0.7))
  }
  if (isFALSE(color)) {
    cols <- sample(gray.colors(n = n_ids, start = 0.3, end = 0.9, alpha = 0.7))
  }

  border_cols <- rep("black", n_ids)
  if (!is.null(subset_ids)) {
    cols[cn_locs] <- NA
    if (!include_others) {
      border_cols[cn_locs] <- NA
    }
  }

  # setup
  plot(0, 0, type = "n", xlim = xl, ylim = yl, yaxs = "i",
       xaxs = "i", axes = FALSE, xlab = "", ylab = "")
  title(ylab = "density", line = 1)
  title(xlab = xlab, line = 1.8)
  axis(1, gap.axis = 0.2, mgp = c(2, 0.7, 0), tcl = -0.3)

  # draw the filled posteriors
  for (i in seq_len(ncol(res))) {
    p <- pdata[[i]]
    p$x[p$x > (n_ids - 1)] <- n_ids - 1
    p$x[p$x < 0] <- 0
    polygon(c(p$x, rev(p$x)), c(rep(0, length(p$x)), rev(p$y)),
            border = NA, col = cols[i])
  }

  # draw the contours
  for (i in seq_len(ncol(res))) {
    p <- pdata[[i]]
    p$x[p$x > (n_ids - 1)] <- n_ids - 1
    p$x[p$x < 0] <- 0
    polygon(c(p$x, rev(p$x)), c(rep(0, length(p$x)), rev(p$y)),
            border = border_cols[i], col = NA, lwd = 0.4, xpd = TRUE)
  }
}

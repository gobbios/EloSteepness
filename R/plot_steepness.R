#' plot steepness density
#'
#' @param x result from \code{\link{elo_steepness_from_matrix}},
#'        \code{\link{elo_steepness_from_sequence}} or
#'        \code{\link{davids_steepness}}
#' @param adjustpar numeric, parameter for smoothing posterior of individual
#'        scores
#' @param print_numbers logical, if \code{TRUE} (default) print numeric
#'        summaries into into the plot and omit them if \code{FALSE}
#'
#' @return a plot
#' @export
#' @importFrom stats density mad median
#' @importFrom graphics polygon text title axis points
#' @importFrom grDevices hcl.colors
#'
#' @examples
#' \donttest{
#' data("dommats", package = "EloRating")
#' m <- dommats$elephants
#' res <- elo_steepness_from_matrix(m, n_rand = 3, refresh = 0, cores = 2, 
#'                                  iter = 1000, warmup = 500)
#' plot_steepness(res)
#' }

plot_steepness <- function(x, adjustpar = 1.5, print_numbers = TRUE) {
  if (!"steepness" %in% names(x)) {
    stop("wrong input object")
  }
  x <- x$steepness

  pd <- density(as.numeric(x), adjust = adjustpar)
  pdata <- apply(x, 2, density, adjust = adjustpar)
  ymax <- max(pd$y, unlist(lapply(pdata, function(x) max(x$y))))
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, ymax * 1.05),
       xaxs = "i", yaxs = "i", ylab = "", xlab = "",
       las = 1, axes = FALSE, ann = FALSE)
  polygon(pd, col = hcl.colors(3, "zissou1")[2], lwd = 3)
  invisible(lapply(pdata, function(x) {
    points(x$x, x$y, type = "l", col = "grey", lwd = 0.5)
    }))

  if (print_numbers) {
    text(0.03, ymax * 0.35,
         paste("mean = ", sprintf("%.2f",
                                  mean(as.numeric(x)))), adj = 0)
    text(0.03, ymax * 0.3,
         paste("median = ", sprintf("%.2f",
                                    median(as.numeric(x)))), adj = 0)
    text(x = 0.03, y = ymax * 0.25,
         paste("sd = ", sprintf("%.2f", sd(as.numeric(x)))), adj = 0)
    text(x = 0.03, y = ymax * 0.2,
         paste("mad = ", sprintf("%.2f", mad(as.numeric(x)))), adj = 0)
  }

  title(ylab = "density", line = 1.5)
  title(xlab = "steepness", line = 1.8)
  axis(1, mgp = c(2, 0.7, 0), tcl = -0.3)
}

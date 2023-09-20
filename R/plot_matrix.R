#' plot (rather than print) a matrix
#'
#' a helper function
#'
#' @param mat square matrix
#' @param greyout numeric, the values to be grayed out
#' @param prunkcol color value, which if set to some color will highlight
#'                 unknown relationships with rectangles of that color.
#' @param label_col color values for column and row labels
#' @importFrom graphics rect
#'
#' @return a plot and an invisible list with coordinates and 
#'         content of the matrix to be plotted

plot_matrix <- function(mat, greyout = NULL, prunkcol = NULL, label_col = "black") {
  n <- ncol(mat)
  colmat <- mat
  colmat[, ] <- "black"

  if (!is.null(greyout)) {
    colmat[mat %in% greyout] <- grey(0.7)
  }

  diag(colmat) <- NA

  diag(mat) <- ""
  plot(0, 0, "n", xlim = c(0.5, n + 0.5), ylim = c(n + 0.5, 0.5),
       ann = FALSE, axes = FALSE)
  xmat <- sapply(seq_len(n), function(x) rep(x, n))
  ymat <- t(xmat)

  text(xmat, ymat, mat, col = colmat, adj = c(0.5, 0))

  cn <- colnames(mat)
  text(rep(0, n), seq_len(n), cn, xpd = TRUE, font = 2, adj = c(0.5, 0), col = label_col)
  text(seq_len(n), rep(0, n), cn, xpd = TRUE, font = 2, adj = c(0.5, 0), col = label_col)

  if (!is.null(prunkcol)) {
    pmat <- apply(mat, 2, as.numeric)
    pmat <- pmat + t(pmat) == 0
    pmat <- which(pmat, arr.ind = TRUE)
    pmat <- pmat[pmat[, 1] < pmat[, 2], , drop = FALSE]
    if (nrow(pmat) > 0) {
      for (i in seq_len(nrow(pmat))) {
        rect(xleft = pmat[i, 2] - 0.4,
             ybottom = pmat[i, 1] - 0.5,
             xright = pmat[i, 2] + 0.4,
             ytop = pmat[i, 1] + 0.3,
             border = prunkcol, lwd = 1.5)
      }
    }
  }
  
  invisible(list(xmat = xmat, ymat = ymat, content = mat))
}

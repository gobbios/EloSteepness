#' proportion of interactions against the rank order
#'
#' @param mat square interaction matrix
#'
#' @return numeric value of upward steepness
#' @importFrom EloRating DS
#' @export
#' @examples
#' data(bonobos, package ="EloRating")
#' upward_steepness(bonobos)

upward_steepness <- function(mat) {
  # determine order based on DS
  xorder <- DS(mat)$ID
  # reorder matrix
  mat <- mat[xorder, xorder]
  # prop of interactions below diag (after reorderding)
  1 - sum(mat[lower.tri(mat)]) / sum(mat)
}

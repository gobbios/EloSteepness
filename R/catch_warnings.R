#' catch warnings alongside results without returning warning
#'
#' helper function
#'
#' @param expr an \R expression to evaluate
#'
#' @return a list where the first entry is the result of \code{expr} and the
#'         second provides information about warnings
#'
#' @export
#'
#' @source \code{demo(error.catching)}
#'
#' @examples
#' log(3)
#' catch_warnings(log(3))
#'
#' # produces warning
#' # log(-3)
#' # catch it
#' catch_warnings(log(-3))
#'
#' # produces error
#' # log("x")
#' # catch it
#' catch_warnings(log("x"))

catch_warnings <- function(expr) {
  w2 <- NULL
  w_handler <- function(w) {
    w2 <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w_handler),
       warning = w2)
}

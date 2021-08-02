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
#' \dontrun{
#' log(-3)
#' catch_warnings(log(-3))
#' 
#' log("x")
#' catch_warnings(log("x"))
#' }

catch_warnings <- function(expr) {
  W <- NULL
  w.handler <- function(w) {
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), 
                                   warning = w.handler), 
       warning = W)
}

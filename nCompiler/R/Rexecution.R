## This file contains R versions of functions supported in nFunction compilation.

#' @export
parallel_for <- function(index, range, body, ...) {
  index <- substitute(index)
  range <- substitute(range)
  body <- substitute(body)
  for_loop <- quote(for(i in range) body)
  for_loop[[2]] <- index
  for_loop[[3]] <- range
  for_loop[[4]] <- body
  eval(for_loop, envir = parent.frame())
}
#' @export
square <- function(x) x*x
#' @export
cube <- function(x) x*x*x
#' @export
logit <- function(x) log(x/(1-x))
#' @export
rsqrt <- function(x) 1/sqrt(x)

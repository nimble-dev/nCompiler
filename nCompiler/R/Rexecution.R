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
parallel_reduce <- function(f, x, init, ...) {
  Reduce(f, x, init)
}

#' @export
square <- function(x) x*x
#' @export
cube <- function(x) x*x*x
#' @export
rsqrt <- function(x) 1/sqrt(x)
#' @export
logit <- function(x) log(x/(1-x))
#' @export
ilogit <- function(x) 1/(1+exp(-x))
#' @export
expit <- ilogit
#' @export
cloglog <- function(x) log(-log(1-x))
#' @export
icloglog <- function(x) 1-exp(-exp(x))
#' @export
probit <- function(x) qnorm(x)
#' @export
iprobit <- function(x) pnorm(x)
#' @export
phi <- iprobit
#' @export
loggam <- lgamma
#' @export
logfact <- lfactorial

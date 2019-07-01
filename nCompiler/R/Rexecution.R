## This file contains R versions of functions supported in nFunction compilation.

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

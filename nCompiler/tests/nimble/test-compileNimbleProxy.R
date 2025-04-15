

nf <- nimbleFunction(
  run = function(x = double(1)) {
    ans <- silly(x, billy$new(), c$d) + 1
    h <- willy$new()
    return(ans)
    returnType(double(1))
  }
)

debug(RCfun_find_needed_recurse)
RCfun_find_needed(nf)

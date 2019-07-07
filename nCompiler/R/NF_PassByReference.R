## Create pass-by-reference semantics

#' @export
passByReference <- function(fun,
                            refArgs) {
  ## For now take refArgs by name.  Later it could take various formats
  passedAsFunction <- is.function(fun)
  code <- if(passedAsFunction)
            body(fun)
  else
    fun
  ## Should check for valid refArgs names
  arg2refLines <- lapply(refArgs,
                         function(x)
                           substitute(ARGNAME <- arg2nimRef(ARGNAME),
                                      list(ARGNAME = as.name(x))
                                      )
                         )
  for(i in seq_along(refArgs)) {
    thisArg <- refArgs[i]
    sublist <- structure(
      list(substitute( nimDeref(ARGNAME),
                      list(ARGNAME = as.name(thisArg)))),
      names = thisArg)
    code <-
      eval(
        substitute(
          substitute(code,
                     sublist),
          list(code = code)
        )
      )
  }
  if(code[[1]] != '{')
    code <- substitute({CODE}, list(CODE=code))
  
  code <-as.call(c(code[[1]], arg2refLines, as.list(code[-1])))

  if(passedAsFunction) {
    body(fun) <- code
    fun
  } else {
    code
  }
}

#' @export
nimRef <- function(x, env = parent.frame(), name) {
  if(missing(name)) name <- as.character(substitute(x))
  structure(list(env = env, name = name),
            class = 'nimRef')
}

#' @export
arg2nimRef <- function(arg) {
  ## check that it is a name, not a constant
  argExpr <- substitute(arg)
  parentSubCall <- substitute(substitute(AE),
                              list(AE = argExpr))
  callingArgExpr <- eval(parentSubCall,
                         parent.frame())
  if(!is.name(callingArgExpr)) {
    if(identical(callingArgExpr[[1]], as.name("nimDeref"))) {
      callingArgExpr <- callingArgExpr[[2]]
      if(!is.name(callingArgExpr))
        stop("Something is wrong passing a nimRef via nimDeref.")
    }
    else
      stop("Something is wrong passing a nimRef.")
  }
  ans <- nimRef(env = parent.frame(2),
                name = as.character(callingArgExpr))
  origClass <- class(ans$env[[ans$name]])
  if(origClass == 'nimRef')
    ans <- ans$env[[ans$name]]
  ans
}

#' @export
nimDeref <- function(x) {
  get(x$name, envir = x$env)
}

#' @export
`nimDeref<-` <- function(x, value) {
  assign(x$name, value, envir = x$env)
  x
}

## Create pass-by-reference semantics

#' @export
passByReference <- function(fun,
                            refArgs = character(),
                            blockRefArgs = character()) {
  passedAsFunction <- is.function(fun)
  code <- if(passedAsFunction)
            body(fun)
          else
            fun

  args_2_subList <- function(args, suffix)
    args |>
      lapply(function(x) as.name(paste0(x, suffix))) |>
      structure(names = args)

  subList_2_lines <- function(subList,
                              fun_name) {
    lines <- list()
    for(i in seq_along(subList)) {
      lines[[i]] <-
        substitute(
          nCompiler::FN(NEWARGNAME, substitute(ARGNAME)),
          list(FN = as.name(fun_name),
               NEWARGNAME = as.character(subList[[i]]),
               ARGNAME = as.name(names(subList)[i])))
    }
    lines
  }

  subList <- args_2_subList(refArgs, "_Ref__")
  refArg_activeBinding_lines <- subList_2_lines(subList, "createRef")

  blockSubList <- args_2_subList(blockRefArgs, "_BlockRef__")
  blockRefArg_activeBinding_lines <- subList_2_lines(blockSubList, "createBlockRef")

  code <-
    eval(
      substitute(
        substitute(code,
                   c(subList, blockSubList)),
        list(code = code)
      )
    )

  if(code[[1]] != '{')
    code <- substitute({CODE}, list(CODE=code))

  code <-as.call(c(code[[1]],
                   refArg_activeBinding_lines,
                   blockRefArg_activeBinding_lines,
                   as.list(code[-1])))

  if(passedAsFunction) {
    body(fun) <- code
    fun
  } else {
    code
  }
}

#' @export
createRef <- function(innerName,
                      outerCode,
                      env,
                      innerEnv ) {
  if(missing(env)) env <- parent.frame(n = 2)
  if(missing(innerEnv)) innerEnv <- parent.frame()
  if(!is.name(outerCode)) {
    stop(
      paste0("An nCompiler reference argument can be called only with a variable, ",
             "with no indexing or other operation. '", deparse(outerCode), "' is not allowed."),
             call. = FALSE)
  }
  outerName <- as.character(outerCode)
  binding <-
    function(v)
      if(missing(v))
        eval(outerCode, env)
      else {
        assign(outerName, v, env)
        v
      }

  makeActiveBinding(innerName, binding, innerEnv)
  NULL
}

#' @export
createBlockRef <- function(innerName,
                           outerCode,
                           env,
                           innerEnv,
                           dummyName = 'DUMMY_FOR_CREATE_BLOCK_REF_') {
  # There is potential for more elaborate error-trapping.
  # E.g. we could determine the sizes (or net length) of outerCode (assignment target)
  # and check that v matches it.
  if(missing(env)) env <- parent.frame(n = 2)
  if(missing(innerEnv)) innerEnv <- parent.frame()
  if(is.character(outerCode))
    outerCode <- parse(text = outerCode, keep.source = FALSE)[[1]]
  outer_dummy_assign_code <- substitute(L <- R,
                                        list(L = outerCode,
                                             R = as.name(dummyName)))
  binding <-
    function(v)
      if(missing(v))
        eval(outerCode, env)
      else {
        assign(dummyName, v, env)
        on.exit(rm(list = dummyName, envir = env))
        eval(outer_dummy_assign_code, env)
        v
      }
  makeActiveBinding(innerName, binding, innerEnv)
  NULL
}

passByReferenceIntoC <- function(fun,
                                 refArgs = character(),
                                 blockRefArgs = character()) {
  passedAsFunction <- is.function(fun)
  code <- if(passedAsFunction)
            body(fun)
          else
            fun

  args_2_subList <- function(args, suffix)
    args |>
      lapply(function(x) as.name(paste0(x, suffix))) |>
      structure(names = args)

  subList_2_lines <- function(subList,
                              fun_name) {
    lines <- list()
    for(i in seq_along(subList)) {
      lines[[i]] <-
        substitute(
          NEWARGNAME <- nCompiler::FN(substitute(ARGNAME)),
          list(FN = as.name(fun_name),
               NEWARGNAME = subList[[i]],
               ARGNAME = as.name(names(subList)[i])))
    }
    lines
  }

  subList <- args_2_subList(refArgs, "_RefIntoC_")
  refArg_activeBinding_lines <- subList_2_lines(subList, "createRefInfoIntoC")

  blockSubList <- list()
  blockRefArg_activeBinding_lines  <- list()
  ## blockSubList <- args_2_subList(blockRefArgs, "_BlockRef__")
  ## blockRefArg_activeBinding_lines <- subList_2_lines(blockSubList, "createBlockRef")

  code <-
    eval(
      substitute(
        substitute(code,
                   c(subList, blockSubList)),
        list(code = code)
      )
    )

  if(code[[1]] != '{')
    code <- substitute({CODE}, list(CODE=code))

  code <-as.call(c(code[[1]],
                   refArg_activeBinding_lines,
                   blockRefArg_activeBinding_lines,
                   as.list(code[-1])))

  if(passedAsFunction) {
    body(fun) <- code
    fun
  } else {
    code
  }
}

#' @export
createRefInfoIntoC <- function(outerCode,
                               env,
                               innerEnv ) {
  ## for foo(x)
  ## outerCode is the x used in the calling function, e.g. my_x in a call foo(my_x)
  ## env is the calling environment i.e. of foo(my_x)
  ## innerEnv is the local function environment, i.e. of x and x_RefIntoC
  if(missing(env)) env <- parent.frame(n = 2)
  if(missing(innerEnv)) innerEnv <- parent.frame()
  outerName <- as.character(outerCode)
  list(outerName, env) # If other things are not needed, we can remove them.
}

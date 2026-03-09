## Various functionality providing backward compatibility with nimble.

## Create an nClass generator with nimble::nimbleList inputs.

#' @export
nimbleList <- function(..., name = as.character(NA), predefined = FALSE, where = parent.frame()) {
    ## In `nimbleList`, we have `where = getNimbleFunctionEnvironment()`,
    ## which tries to get top-level env (i.e., pkg namespace or global env).
    ## Here we default to the default environment/frame passed to `nClass`, since
    ## we presumably want consistent behavior across different pathways that result
    ## in calling `nClass()`.
    Call <- match.call(expand.dots = TRUE)
    nms <- names(Call)
    if (any(nms == "name")) {
        if (!is.character(Call[[which(nms == "name")]])) 
            stop("Elements of a nimbleList cannot be named `name`.")
        Call <- Call[-which(names(Call) == "name")]
    }
    nms <- names(Call)
    if (any(nms == "predefined")) {
        if (!is.logical(Call[[which(nms == "predefined")]])) 
            stop("Elements of a nimbleList cannot be named `predefined`.")
        Call <- Call[-which(nms == "predefined")]
    }
    nms <- names(Call)
    if (any(nms == "where")) {
        if (!is.environment(Call[[which(nms == "where")]])) 
            stop("Elements of a nimbleList cannot be named `where`.")
        Call <- Call[-which(nms == "where")]
    }
    if (length(Call) < 2) 
        stop("No arguments specified for nimbleList")
    argList <- list()
    if ((is.call(Call[[2]]) && deparse(Call[[2]][[1]]) == "list") || 
        (!is.call(Call[[2]]) && is.list(eval(Call[[2]], envir = parent.frame())))) {
        callList <- eval(Call[[2]], envir = parent.frame())
        for (iArg in seq_along(callList)) {
            argList[[iArg]] <- list(name = callList[[iArg]]$name, 
                type = callList[[iArg]]$type, dim = callList[[iArg]]$dim)
        }
    }
    else {
        for (iArg in 2:length(Call)) {
            argList[[iArg - 1]] <- list(name = names(Call)[iArg], 
                type = deparse(Call[[iArg]][[1]]))
            argList[[iArg - 1]]$dim <- if (length(Call[[iArg]]) > 
                1) 
                deparse(Call[[iArg]][[2]])
            else 0
        }
    }
    types <- list(vars = sapply(argList, function(x) {
        return(x$name)
    }), types = sapply(argList, function(x) {
        return(x$type)
    }), dims = sapply(argList, function(x) {
        return(x$dim)
    }))
    if (any(c("name", "predefined", "where") %in% types$vars)) 
        stop("Elements of a nimbleList cannot be named `name`, `predefined` or `where`.")

    Cpublic = createTypeList(types)
    if(!missing(name)) {
        nc <- nClass(name, Cpublic = Cpublic, predefined = predefined, env = where)
    } else nc <- nClass(Cpublic = Cpublic, predefined = predefined, env = where)
    return(nc)
}

## Utility to convert a nimble "types" list to a "Cpublic" list.
createTypeList <- function(types) {
    typelist <- as.list(paste0(types$types, "(", types$dims, ")"))
    names(typelist) <- types$vars
    return(typelist)
}

## Copied from nimble. We probably don't want to re-export `nimbleType` as that
## would introduce an nCompiler dependence on `nimble`.

#' @export
nimbleType <- setRefClass(
  Class = 'nimbleType',
  fields = c('name', 'type', 'dim'),
  methods = list(
    initialize = function(name, type, dim = NA){
      name <<- name
      type <<- type
      dim <<- dim
    },
    show = function(){
      cat("nimbleType object with name ", name, ", type ", type, ", dim ",
          dim,"\n", sep = "")
    }
  )
)

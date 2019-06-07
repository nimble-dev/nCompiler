#' @export
nCompile <- function(...,
                     dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                     cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                     env = parent.frame(),
                     control = list(),
                     returnList = FALSE) { ## return a list even if there is only one unit being compiled.
  dotsDeparses <- unlist(lapply( substitute(list(...))[-1], deparse ))
  origList <- list(...)
  if(is.null(names(origList))) 
    names(origList) <- rep('', length(origList))
  boolNoName <- names(origList)==''
  origIsList <- unlist(lapply(origList, is.list))
  for(i in which(origIsList)) {
    if(is.null(names(origList[[i]])) || any(names(origList[[i]])==""))
      stop("If you provide a list of compilation units, it must be named.")
  }
  dotsDeparses[origIsList] <- ''
  names(origList)[boolNoName] <- dotsDeparses[boolNoName]
  units <- do.call('c', origList)
  unitTypes <- get_nCompile_types(units)
  if(is.null(names(units))) names(units) <- rep('', length(units))
  if(length(units) == 0) stop('No objects for compilation provided')
  unitResults <- list()
  ## names(units) should be fully populated and unique. TO-DO: check.
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nF") {
      unitResults[[i]] <- nCompile_nFunction(units[[i]],
                                             stopAfterRcppPacket = TRUE,
                                             env = env,
                                             control = control)
                                            #, name = names(units)[i])
    }
  }
  cpp_names <- lapply(units, 
                      function(x)
                        NFinternals(x)$cpp_code_name)
  ## Write the results jointly, with one .cpp file and multiple .h files.
  ## This fits Rcpp::sourceCpp's requirements.
  RcppPacket_list <- lapply(unitResults, 
                            function(uR) NFinternals(uR)$RcppPacket) 
  cppfile <- "nCompiler_multiple_units.cpp"
  writeCpp_nCompiler_combine(RcppPacket_list,
                             cppfile = cppfile)
  resultEnv <- new.env()
  ans <- compileCpp_nCompiler(cppfile,
                              dir = dir,
                              cacheDir = cacheDir,
                              env = resultEnv,
                              returnList = returnList)
  ## Next we re-order results using input names,
  ## in case the ordering in the C++ code or in Rcpp's handling
  ## does not match order of units.
  ## cpp_names should be 1-to-1 with names(ans)
  ## We want to return with names(ans) changed to
  ## names(units) corresponding to cpp_names.
  if(is.list(ans)) {
    newNames <- names(ans)
    for(i in seq_along(units)) {
      iRes <- which(cpp_names[i] == names(ans))
      if(length(iRes) != 1) {
        warning("Name matching of results had a problem.  Returning list of compiled results with internal C++ names.")
        return(ans)
      }
      newNames[iRes] <- names(units)[i]
    }
    names(ans) <- newNames
  }
  ans
}

get_nCompile_types <- function(units) {
  ans <- character(length(units))
  for(i in seq_along(units)) {
      if(isNF(units[[i]])) ans[i] <- 'nF'
    else if(isNCgenerator(units[[i]])) ans[i] <- 'nCgen'
    else if(isNC(units[[i]])) 
      stop(paste0("The #", i, " object to be compiled is an nClass object.\n",
                  "Only nClass generators (the class definition, not an object of the class) should be compiled."),
           call.=FALSE)
    else stop(paste0("The #", i, " object to be compiled is neither an nFunction nor an nClass generator (class definition).\n"))
  }
  ans
}

#' Self-contained scheme for looping through compiler calls.

# Returns the Rcpp packet list.
compileLoop <- function(units,
                        env,
                        control) {
  if (length(units) == 0)
    stop('No objects for compilation provided')

  if (is.null(names(units)))
      names(units) <- rep('', length(units))
  unitTypes <- get_nCompile_types(units)

  ## names(units) should be fully populated and unique. TO-DO: check.
  RcppPacket_list <- vector(length = length(units), mode = "list")
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nF") {
      unitResult <- nCompile_nFunction(units[[i]],
                                       stopAfterRcppPacket = TRUE,
                                       env = env,
                                       control = control)
      RcppPacket_list[[i]] <- NFinternals(unitResult)$RcppPacket
    }
    else if(unitTypes[i] == "nCgen") {
      unitResult <- nCompile_nClass(units[[i]],
                                     stopAfterRcppPacket = TRUE,
                                     env = env,
                                     control = control)
      RcppPacket_list[[i]] <- NCinternals(unitResult)$RcppPacket
    }
  }
  RcppPacket_list
}


# Prescreens the unit list to ensure that all types are handleable.
# Returns a string descriptor indicating whether the type is a function or a generator.
get_nCompile_types <- function(units) {
  ans <- character(length(units))
  for(i in seq_along(units)) {
    if(isNF(units[[i]]))
      ans[i] <- 'nF'
    else if(isNCgenerator(units[[i]]))
      ans[i] <- 'nCgen'
    else if(isNC(units[[i]])) 
      stop(paste0("The #", i, " object to be compiled is an nClass object.\n",
                  "Only nClass generators (the class definition, not an object of the class) should be compiled."), call.=FALSE)
    else
      stop(paste0("The #", i, " object to be compiled is neither an nFunction nor an nClass generator (class definition).\n"))
  }
  ans
}

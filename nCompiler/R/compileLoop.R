
#' Self-contained scheme for looping through compiler calls.

# Returns the Rcpp packet list.
compileLoop <- function(units,
                        unitName,
                        env = parent.frame(),
                        control = list(),
                        roxygen = list(),
                        combined = FALSE) {
  if (length(units) == 0)
    stop('No objects for compilation provided')

  roxygenFlag <- getRoxygenFlag(units, roxygen)
  unitTypes <- get_nCompile_types(units)

  ## names(units) should be fully populated and unique. TO-DO: check.
  RcppPacket_list <- vector(length = length(units), mode = "list")
  for(i in seq_along(units)) {
    if (unitTypes[i] == "nF") {
      unitResult <-
        if (combined)
          nCompile_nFunction(units[[i]],
                             stopAfterRcppPacket = TRUE,
                             env = env,
                             control = control
                             )
      else
          nCompile_nFunction(units[[i]],
                             stopAfterRcppPacket = FALSE,
                             control = list(endStage = "writeCpp",
                                            useUniqueNameInCode = TRUE)
                             )
      RcppPacket_list[[i]] <- NFinternals(unitResult)$RcppPacket
      RcppPacket_list[[i]]$cppContent <- roxify(unitName[[i]], RcppPacket_list[[i]], roxygenFlag, roxygen)
    }
    else if(unitTypes[i] == "nCgen") {
      unitResult <-
        if (combined)
          nCompile_nClass(units[[i]],
                          stopAfterRcppPacket = TRUE,
                          env = env,
                          control = control)
      else
        nCompile_nClass(units[[i]],
                        stopAfterRcppPacket = FALSE,
                        control = list(endStage = "writeCpp")
                        )
      RcppPacket_list[[i]] <- NCinternals(if (combined) unitResult
                                          else units[[i]])$RcppPacket
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

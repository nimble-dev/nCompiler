NFcompilerStages <- list(
  start = 0,
  setInputOutputTypes = 1,
  substituteMangledArgumentNames = 2,
  initializeCode = 3,
  initializeAuxiliaryEnvironment = 4,
  normalizeCalls = 5,
  simpleTransformations = 6,
  simpleIntermediates = 7,
  labelAbstractTypes = 8,            # set 'type' field of every exprClass in a syntax tree
  processAD = 8.5,
  ##    setToEigen = 8
  # Yes means eigen is required (e.g. %*%).  No means eigen can't be used (e.g. print).  Maybe means eigen can be used or not (e.g. +).  
  addInsertions = 9,                 # Convert insertions entries of exprClass objects into lines of code in the syntax tree.  (Currently there are none.)
  setImplementation = 10,            # Mark symbols in the symbolTable with their implementation.  Currently only "Eigen"
  #    labelForEigen = 11
  doImplementation = 12,             # Strip eigenize() and convert the line of code it contains to eigen code.
  addDebugging = 13,
  makeCppDef = 14,                    # Create a cppDef object to manage C++ code.
  makeRcppPacket = 15,               # The RcppPacket contains character vectors for a .h file and a .cpp file.
  writeCpp = 16,                     # Write contents of the RcppPacket to files
  compileCpp = 17,                   # Call C++ compiler, ultimately via Rcpp:sourceCpp
  end = 18 ## compiler uses value of end-1 to check if done
)

NFgetStageID <- function(stage) {
  if(is.character(stage))
    NFcompilerStages[[stage]]
  else
    stage
}

NFcompilerMaybeStop <- function(stage, control) {
  ## stage is a stage about to be started.
  ## If control$endStage is the same as stage, then
  ## this returns FALSE because the stage hasn't been 
  ## done yet.
  stageID <- NFgetStageID(stage)
  if(stageID <= NFgetStageID(control$endStage))
    FALSE
  else
    TRUE
}

NFcompilerMaybeStopAfter <- function(stage, control) {
  ## stage is a stage completed.
  ## If control$endStage is the same as stage, then
  ## this returns TRUE because the stage has been done.
  stageID <- NFgetStageID(stage)
  if(stageID < NFgetStageID(control$endStage))
    FALSE
  else
    TRUE
}

NFcompilerMaybeSkip <- function(stage, control) {
  stageID <- NFgetStageID(stage)
  if(stageID < NFgetStageID(control$startStage))
    TRUE
  else
    FALSE
}

NFcompilerMaybeDebug <- function(stage, control) {
  stageID <- NFgetStageID(stage)
  if(stageID >= NFgetStageID(control$startDebugStage) &&
       stageID <= NFgetStageID(control$endDebugStage) )
    substitute({
      debug <- TRUE
      cat(MSG)
      browser(text = MSG)
    },
    ## get the message evaluated here, not in calling frame
    list(MSG = paste0("Debugging at stage ",
                      stageID,
                      " (",
                      stage,
                      ")\n"))
    )
  else
    quote(debug <- FALSE)
}

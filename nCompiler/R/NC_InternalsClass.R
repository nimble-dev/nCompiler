
NC_InternalsClass <- R6::R6Class(
  classname = "NC_InternalsClass",
  portable = FALSE,
  public = list(
    symbolTable = NULL,
    methodNames = character(),
    fieldNames = character(),
    classname = character(),
    cpp_classname = character(),
    RcppPacket = NULL,
    isOnlyC = FALSE, ## somewhat redundant but perhaps convenient - TBD.
    enableDerivs = NULL,
    enableSaving = NULL,
    predefined = FALSE,
    initialize = function(classname,
                          Cpublic,
                          isOnlyC = FALSE,
                          enableDerivs = NULL,
                          enableSaving = get_nOption("enableSaving"),
                          predefined = FALSE) {
      self$classname <- classname
      self$cpp_classname <- Rname2CppName(classname)
      self$isOnlyC = isOnlyC
      numEntries <- length(Cpublic)
      if(numEntries) {
        isMethod <- rep(FALSE, numEntries)
        for(i in seq_along(Cpublic)) {
          if(isNF(Cpublic[[i]])) {
            isMethod[i] <- TRUE
            NFinternals(Cpublic[[i]])$isMethod <- TRUE
            next;
          }
          if(is.function(Cpublic[[i]])) {
            stop(paste0('Cpublic methods should be provided as nFunctions, ',
                        'not functions. ', names(Cpublic)[i], ' is a function.'),
                 call. = FALSE)
          }
        }
        symbolTable <<- argTypeList2symbolTable(Cpublic[!isMethod])
        methodNames <<- names(Cpublic)[isMethod]
        fieldNames <<- names(Cpublic)[!isMethod]
      }
      if(!is.null(enableDerivs)) {
        if(!is.list(enableDerivs))
          enableDerivs <- as.list(enableDerivs)
        for(i in enableDerivs) {
          if(!(i %in% self$methodNames))
            stop(paste0('enableDerivs entry ', i, ' is not a method in Cpublic.'))
        }
        self$enableDerivs <- enableDerivs
        self$predefined <- predefined
      }
      self$enableSaving <- enableSaving
    }
  )
)

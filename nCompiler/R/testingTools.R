## generateTypesText_oneCase <- function(caseDef,
##                                       endStage = 'labelAbstractTypes',
##                                       showArgs = list(showType = TRUE)) {
##   if(is.null(caseDef$fun)) {
##     warning(paste0("There is an empty case."))
##     return(NULL)
##   }
##   nf <- nFunction(fun = caseDef$fun)
##   NFC <- nCompile_nFunction(
##     nf,
##     control = list(
##       endStage = endStage
##     )
##   )
##   if(NFgetStageID(endStage) < NFgetStageID('makeRcppPacket'))
##     return(
##       capture.output(do.call(NFC$code$print, showArgs))
##       )
##   if(endStage == 'makeRcppPacket') {
##     return(capture.output(
##       writeCpp_nCompiler(NFinternals(NFC)$RcppPacket, con = stdout())
##     ))
##   }
## }

## writeTypesText_manyCases <- function(cases,
##                                      dir,
##                                      ...) {
##   if(!missing(dir))
##     dir.create(dir, showWarnings = FALSE)
##   else
##     fileName = stdout()
##   for(i in seq_along(cases)) {
##     thisName <- names(cases)[i]
##     if(!missing(dir))
##       fileName <- file.path(dir, Rname2CppName(thisName))
##     thisOutput <- try(generateTypesText_oneCase(cases[[i]],
##                                                 ...))
##     if(inherits(thisOutput, 'try-error'))
##       writeLines(paste0("Error in case ", thisName, "."))
##     writeLines(c(paste0("Case: ", thisName),
##                  '-----',
##                  thisOutput,
##                  '\n'),
##                con = fileName)
##   }
##   invisible(NULL)
## }

compareFilesUsingDiff <- function(trialFile, 
                                  correctFile,
                                  main = "") {
  if(main == "") main <- paste0(trialFile, 
                                ' and ',
                                correctFile,
                                ' do not match\n')
  diffOutput <- system2('diff', 
                        c(trialFile, correctFile),
                        stdout = TRUE)
  test_that(paste0(main,
                   paste0(diffOutput, collapse = '\n')
                   ),
            expect_true(length(diffOutput) == 0)
  )
  invisible(NULL)
}

compareAllFilesInDir <- function(trialDir, 
                                 correctDir) {
  trialFiles <- list.files(trialDir)
  correctFiles <- list.files(correctDir)
  intersectFiles <- intersect(trialFiles, correctFiles)
  compareFiles <- function(trialFile, correctFile) {
    compareFilesUsingDiff(file.path(trialDir, trialFile),
                          file.path(correctDir, correctFile))
  }
  mapply(compareFiles,
         intersectFiles,
         intersectFiles)
  extraTrialFiles <- setdiff(trialFiles, correctFiles)
  extraCorrectFiles <- setdiff(correctFiles, trialFiles)
  expect_true(
    isTRUE(length(extraTrialFiles)==0 & length(extraCorrectFiles)==0),
    info = paste0("Trial files without a correct file:", 
                  paste(extraTrialFiles),
                  "Correct files without a trial file:", 
                  paste(extraCorrectFiles)),
    label = "Mismatch in trial vs. correct files."
  )
}

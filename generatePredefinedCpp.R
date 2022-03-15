## Newer predefined system (as of 3/15/22).
# How to to create a new predefined class:
# 1. Enter the class definition in a file in the R directory.
#    Some are in zzz_NC_predefined.R but they can be anywhere after nClass has been defined.
#
# 2. Include a classname in the nClass call, so it is not randomly generated.
#    Include a argument like predefined="my_predefined_local" in the nClass call,
#                            with a character value that is different from classname.
#    Be sure the nClass is an exported R object in the paackage (usually).
#    The class name will be used for the C++ code that will be generated each time you use the nClass.
#    The predefined name will be used for permanent files of generated C++ that you will copy to package source code and modify.
#    The example here will be my_predefined <- nClass(classname = "my_predefined", predefined = "my_predefined_pkg", ...)
#
# 3. build nCompiler again with the new nClass included.
#
# 4. Load nCompiler and do nCompile(my_predefined, control = list(generate_predefined = TRUE)).
#    Be sure you know where code is being generated (default is tempdir(), can be changed by dir argument to nCompile).
#
# 5. Copy my_predefined.h to my_predefined_pkg.h in package source code (i.e. inst/include/nCompiler).
#
# 6. Copy the cpp file, which will have an automatically generated name like "nCompiler_units_1.cpp", to
#    my_predfined_pkg.cpp in the package source code. (Note these names match the predefined argument in the nClass class.)
#
# 7. (DEPRECATED - DO NOT DO THIS. LEAVING IT HERE FOR NOTES)
#    In my_predefined.cpp, there will be a line like
#    #include "my_predefined.h"
#    Change that to
#    #include "my_predefined_pkg.h"
#    i.e. replace the classname with the predefined name.
#
# 8. In my_predefined.h and my_predefined.cpp, there will be #ifndef protections against
#    multiple #includes.  They will look like
#    #ifndef __test_predefined_H
#    #define __test_predefined_H
#    and
#    #ifndef __test_predefined_CPP
#    #define __test_predefined_CPP
#
#    Change each of these to have a unique name in some way, e.g.
#    #ifndef __test_predefined_H_PREDEF
#    #define __test_predefined_H_PREDEF
#    and similarly for the CPP one
#
# 8. If any other files (which must be included from nCompiler_Eigen_fxns.h)
#    in inst/include/nCompiler that need to use my_predefined,
#    put the code using my_predefined inside a code block like this:
#    #ifdef PREDEFINED_my_predefined
#    #include PREDEFINED_HEADER(PREDEFINED_test_predefined)
#    <rest of your code>
#    #endif
#
#    (The C++ pre-processor variable PREDEFINED_my_predefined will be set by generated code
#     automatically when you use the predefined class later.  In this case it will be 'my_predefined_pkg')
#
#    Note that it will not work to put such code just anywhere.  It needs to be in one of the files that
#    will be arranged to be included after the predefined header, so that
#    PREDEFINED_my_predefined has been defined for the C++ preprocessor
#    Currently the only file included at that time is nCompiler_Eigen_fxns.h, which is simply a set
#    of other include statements.  So it should be possible to add to those as needed (and possibly rename).
#
# 9. build nCompiler again so that the package includes the new .h and .cpp files.
#
# 10. You should now be able to use my_predefined as a type in nClasses and nFunctions.
#
# 11. When calling nCompile with code that uses my_predefined, include my_predefined as a compilation unit
#     (i.e. an input) to nCompile.  Otherwise it will not be used and there will be an error.
#
# Note: It is still possible to set control = list(generate_predefined = TRUE) and use
# locally generated versions of the predefined classes, not the versions that you have
# copied into package sources and installed.  However, other functions that uses the classes
# will not work.
#
# Note: During experimental work, in steps 5 and 6 you can copy the files directly to the installed directory
#       and then not need to rebuild and resintall the package in order to try them.

################################
### OLDER SYSTEM (as of 3/15/22)
## This script:
## 1. Reads files in nCompiler/R that have predefined nClass definitions.
## 2. Generates the .cpp and .h content in inst
library(nCompiler)

#set_nOption("use_nCompLocal", TRUE)

## if(!("generatePredefinedCpp.R" %in% list.files()))
##   stop("You need source generatePredefinedCpp.R with its directory as your working directory.")

## predefined_filebase <- "predefined"
## clear_predefined_files <- function(compileAttributes = TRUE) {
##   dir1 = file.path("nCompiler","src")
##   dir2 = file.path("nCompiler","inst","include","nCompiler")
##   for(dir in c(dir1, dir2)) {
##     for(file in paste0(predefined_filebase, c(".h", ".cpp"))) {
##       dirfile <- file.path(dir, file)
##       if(file.exists(dirfile))
##          file.remove(dirfile)
##     }
##   }
##   if(compileAttributes)
##     compileAttributes("nCompiler")
## }

## predefFiles <- c("zzz_NC_Predefined.R")
## sourceDir <- file.path("nCompiler", "R")

## predefinedNames <- character()
## predefinedRcppPackets <- list()

## getRcppPacket <- function(parsed, className) {
##   env <- new.env()
##   eval(parsed, envir = env)
##   RcppPacket <- nCompile_nClass(env[[className]], 
##                                 control = list(endStage = 'writeCpp',
##                                                filename = predefined_filebase))
##   RcppPacket
## }

## for(pF in predefFiles) {
## ##  pF <- predefFiles[1]
##   pathedFile <- file.path(sourceDir, pF)
##   parsed <- parse(file = pathedFile)
##   for(iP in seq_along(parsed)) {
##     ##iP <- 1;
##     thisParsed <- parsed[[iP]]
    
##     if(is.call(thisParsed)) {
##       if(deparse(thisParsed[[1]]) == "<-") {
##         if(is.call(thisParsed[[3]])) {
##           if(deparse(thisParsed[[3]][[1]]) == "nClass") {
##             className <- deparse(thisParsed[[2]])
##             predefinedNames <- append(predefinedNames, className)
##             predefinedRcppPackets <- c(predefinedRcppPackets, 
##                                        list(getRcppPacket(thisParsed, className)))
##           }
##         }
##       }
##     }
##   }
## }

## sepContents <- function(x, element, label) 
##   c("//--------------------------------------",
##     "//--------------------------------------",
##     paste0("// ", label),
##     "//--------------------------------------",
##     x[[element]]
##   )

## combinedPacket <- nCompiler:::Rcpp_nCompilerPacket(
##   cppContent = c("//GENERATED BY generatedPredefinedCpp.R. DO NOT EDIT BY HAND",
##     unlist(mapply(sepContents, 
##            x = predefinedRcppPackets, 
##            label = predefinedNames,
##            MoreArgs = list(element = "cppContent"), 
##            SIMPLIFY=FALSE, 
##            USE.NAMES = FALSE))),
##   hContent = c("//GENERATED BY generatedPredefinedCpp.R. DO NOT EDIT BY HAND",
##     unlist(mapply(sepContents, 
##            x = predefinedRcppPackets, 
##            label = predefinedNames,
##            MoreArgs = list(element = "hContent"), 
##            SIMPLIFY=FALSE, 
##            USE.NAMES = FALSE))),
##   filebase = predefined_filebase
## )

## # Remove "// [[Rcpp::depends(nCompiler)]]"
## # This should occur once per cppDef in cppContent
## removeBool <- combinedPacket$cppContent == "// [[Rcpp::depends(nCompiler)]]"
## combinedPacket$cppContent[removeBool] <- ""
## # This should not occur in hContent, but we can defensively check anyway
## removeBool <- combinedPacket$hContent == "// [[Rcpp::depends(nCompiler)]]"
## combinedPacket$hContent[removeBool] <- ""

## ## remove any old files
## clear_predefined_files(compileAttributes = FALSE)
## nCompiler:::writeCpp_nCompiler(combinedPacket, dir = file.path("nCompiler","inst","include","nCompiler"))
## nCompiler:::writeCpp_nCompiler(combinedPacket, dir = file.path("nCompiler","src"))
## require(Rcpp)
## compileAttributes("nCompiler")

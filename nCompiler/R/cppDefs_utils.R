
# This seemingly redundant or idempotent function makes it easier to code. E.g.
# nParse(cppLiteral(paste("some code with a ", var)))
# This gets the expressions evaluated.
# Well... I'm trying to not need this.
## cppLiteral <- function(text, types = NULL) {
##     template <- quote(cppLiteral(text, types))
##     template[[2]] <- text
##     template[[3]] <- types
##     template
## }

writeCode <- function(x, ...) writeLines(unlist(x), ...)

generateAll <- function(x, ...) lapply(x, function(y) y$generate(...))

codeSubstitute <- function(code, subList) {
    eval(substitute(substitute(code, subList), list(code = code)))
}
## This allows definition of a template and substitution into it with lighter syntax
## examples
## template <- quote({RNAME <- foo(CNAME)})
## codeSubstitute(template, list(RNAME = 'Ra', CNAME = 'ca'))
## l1 <- codeSubstitute(template, list(RNAME = 'Ra', CNAME = as.name('ca')))

putCodeLinesInBrackets <- function(codeLines) {
    as.call(c(as.name('{'), codeLines))
}

# This is the location of the RcppUtils.cpp, etc. files.
IncludeCodeDir <- character()
NimbleCodeDir <- system.file("CppCode", package = "nCompiler")

nCompilerIncludeFile <- function(file, path = IncludeCodeDir)
{
  if(length(path)) 
     sprintf('"%s"', normalizePath(sprintf("%s/%s", sub("/$", "", path), file)))
  else
     sprintf("<nCompiler/%s>", file)
}

makeDefaultDirName <- function()  file.path(tempdir(), 'nCompiler_generatedCode')

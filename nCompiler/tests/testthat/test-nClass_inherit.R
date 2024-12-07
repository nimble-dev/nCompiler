
library(nCompiler)

message("uncompiled nClass Cpublic variables are not initialized well")

ncA <- nClass(
  Rpublic = list(
    fooRA = function() v.A
  ),
  Cpublic = list(
    v.A = 'numericVector',
    wA = 'numericScalar',
    add.wA = nFunction(
      function(x.1 = 'numericVector') {
#        cat("add_wA for ncA\n")
#        ans <- add_wA2(x)
        return(wA + x.1); returnType('numericVector')
      },
      control = list(changeKeywords=TRUE),
      compileInfo = list(virtual = FALSE)
    )
  ),
  compileInfo = list(interface = "none")
)

objA <- ncA$new()
objA$wA <- 10
objA$add.wA(1:3)

#debug(nCompiler:::nCompile_nClass)
debug(nCompiler:::addGenericInterface_impl)

CncA <- nCompile(ncA)
CobjA <- CncA$new()
CobjA$wA <- 10
CobjA$wA
CobjA$v.A <- 1:3
CobjA$v.A

CobjA$add.wA(1:3)
CobjA$add.wA(1:3)

CobjA$v.A <- 1:3
CobjA$fooRA()

ncB <- nClass(
  inherit = ncA,
  Cpublic = list(
    ## add_wA= nFunction(
    ##   function(x = 'numericVector') {
    ##   #  cat("add_wA for ncB\n")
    ##     return(10 + wA + x); returnType('numericVector')
    ##   },
    ##   control=list(changeKeywords=FALSE)
    ## ),
    wA = 'numericScalar',
    add_wA2= nFunction(
      function(log = 'numericVector') {
      #  cat("add_wA2 for ncB\n")
        return(2*wA + log); returnType('numericVector')
      },
      control=list(changeKeywords=FALSE)
    )
  )
)

ncB
objB <- ncB$new()
objB$wA <- 10
objB$add_wA
objB$add_wA(1:3)
objB$super$add_wA
identical(objB$add_wA, objB$super$add_wA) #FALSE, but would be TRUE if a derived method was not given

debug(nCompiler:::build_compiled_nClass)
debug(nCompiler:::nCompile_finish_nonpackage)
debug(nCompiler:::addGenericInterface_impl)

CncB <- nCompile(ncA, ncB) # Order still matters
CobjB <- CncB$ncB$new()
CobjB$wA <- 10
CobjB$add_wA2(1:3)
CobjB$v.A <- 11:13
CobjB$v.A
CobjB$add.wA(15:18)#oops the base class wA is not accessible but that's by design to mimic R6.

# So R6 semantics are natively like "virtual"
# and base class methods are called via super$method
# and there is no multiple inheritance.
#
# Finding inheritance is tricky because
#  the generator retains an unevaluated expression for inherits.
#  We should probably resolve that once an put it in NCinternals.

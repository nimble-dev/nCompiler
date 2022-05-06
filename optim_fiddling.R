library(nCompiler)

# TODO: consider supporting nOptim for use with non-member nFunctions.  but, 
# recognize this will still generally require that the nFunction's signature 
# is specified at compile time, adding complexity for nCompiler users and 
# possibly also for developers.

ncOptimProblem <- nClass(
  classname = 'OptimProblem',
  Cpublic = list(
    f = nFunction(
      fun = function(par) {
        ans <- sum(par^2)
        return(ans) 
      }, 
      argTypes = list(par = 'numericVector'),
      refArgs = 'par', # this is an optional decision
      returnType = 'double'
    ),
    optim = nFunction(
      fun = function(par_init) {
        # do optimization, then return function at max
        control <- OptimControlList$new()
        control$initToDefaults()
        control$trace <- 1
        lower <- -1e2
        upper <- 1e2
        result <- nOptim(par_init, f, "", "Nelder-Mead", lower, upper, control, FALSE)
        return(result)
      }, 
      # ideally, these would be auto-detected based on f
      argTypes = list(par_init = 'numericVector'), 
      returnType = 'OptimResultList'
    )
  )
)

cOptimProblem <- nCompile(ncOptimProblem, OptimControlList, OptimResultList)

x <- cOptimProblem$ncOptimProblem$new()
r <- x$optim(-20) # failure to optimize due to Nelder-Mead instabilities on 1d problems
r <- x$optim(20) # we see that the issue really is with the optimization alg.

r$par
r$value
r$convergence
r$counts

# when stats::optim issues a warning about Nelder-Mead it also silently changes
# the optimization method it calls (to L-BFGS), without providing the user any 
# output to notice this behavior; so, stats::optim appears to work, where 
# direct calls to nOptim appear to fail for the same problem.
o = optim(par = -20, fn = function(x) sum(x^2), method = 'Nelder-Mead', 
         control = list(trace=10))

o$par
o$value
o$convergence
o$counts

# view and attempt to compile generated c++
system(paste('open',tempdir()))
Rcpp::sourceCpp(file.path(tempdir(),'nCompiler_generatedCode','nCompiler_units_1.cpp'))
context("Testing parallelization via TBB.")

nc <- nClass(
  Cpublic = list(
    go = nFunction(
      fun = function(x = 'numericVector') {
        y <- x
        parallel_for(i, 1:10,
                     {
                       y[i] <- 2 * x[i]
                     },
                     "x",
                     "y")
        return(y)
      },
      returnType = 'numericVector'
    )
  )
)

Cnc <- nCompile_nClass(nc, control = list(endStage = "makeCppDef"))
class(Cnc)
writeCode(Cnc$generate())
writeCode(Cnc$generate(TRUE))

set_nOption("showCompilerOutput", TRUE)
Cnc <- nCompile_nClass(nc)
nc1 <- nc$new()
## nc1$go(101:110) ## does not run uncompiled
Cnc1 <- Cnc$new()
Cnc1$go(101:110) ## runs in parallel

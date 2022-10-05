# Check for successful catching and handling of alias risk in
# generated Eigen expressions

# Alias error when it occurs happens on 4th element
# due to packet use.
# So increase this to longer just to ensure
# coverage under any different packet lenghts, since I have
# no idea if those are the same on all systems.
foo <- nFunction(
  function(x = numericVector()) {
    x <- x + 1.1 # no need for .eval()
    return(x)
    returnType(numericVector())
  }
)
cfoo <- nCompile_nFunction(foo)
expect_equal(foo(1:30), cfoo(1:30))

foo <- nFunction(
  function(x = numericVector()) {
    x[2:20] <- x[1:19] + 1.1 # eval is needed
    return(x)
    returnType(numericVector())
  }
)
cfoo <- nCompile_nFunction(foo)
expect_equal(foo(1:30), cfoo(1:30))

foo <- nFunction(
  function(x = numericVector()) {
    y <- x
    x[2:20] <- y[1:19] + 1.1 + x[1:19] # eval is needed
    return(x)
    returnType(numericVector())
  }
)
cfoo <- nCompile_nFunction(foo)
expect_equal(foo(1:30), cfoo(1:30))

# This case initially failed.
# It revealed the need for nEval_.
# Specifically, if the LHS is a whole
# object and the RHS is a subset of the object
# the regular .eval() does not work.
foo <- nFunction(
  function(x = numericVector()) {
    x <- x[1:2] + 1.1
    return(x)
    returnType(numericVector())
  }
)
nOptions(pause_after_writing_files = TRUE)
nOptions(showCompilerOutput = TRUE)
cfoo <- nCompile_nFunction(foo)
expect_equal(foo(1:30), cfoo(1:30))
expect_equal(foo(1:2), cfoo(1:2))

foo <- nFunction(
  function(x = numericVector()) {
    y <- x[1:2]
    x <- y
    return(x)
    returnType(numericVector())
  }
)
cfoo <- nCompile_nFunction(foo)
expect_equal(foo(1:30), cfoo(1:30))

foo <- nFunction(
  function(x = numericVector()) {
    y <- x
    x <- y[1:2]
    return(x)
    returnType(numericVector())
  }
)
cfoo <- nCompile_nFunction(foo)
expect_equal(foo(1:30), cfoo(1:30))


## Tests to add in future:
# Cases like x[...][...]
# Cases like a$b$x[...]
# Cases like y[ x[...] ]
# Risky operators like t(x)
# Safe operators like nFunction, chainedCall, nDiag, nEigen, others.
#    The list of safe operators was not very carefully checked when this system was written.

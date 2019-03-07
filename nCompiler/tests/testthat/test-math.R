context("Testing of math functions in nCompiler code")

## As in current nimble's BUGS_mathCompatability.R. Where to put this?
square <- function(x) x*x
cube <- function(x) x*x*x

make_input <- function(argType, size = 3) {
  switch(argType,
         "integerScalar()" = rgeom(1, 0.5),
         "numericScalar()" = rnorm(1),
         "numericVector()" = rnorm(size),
         ## Make different sized dimensions to avoid bugs that might be hidden by symmetry.
         "numericMatrix()" = matrix(rnorm(size*(size+1)), nrow = size, ncol = size+1),
         "numericArray(nDim = 3)" = array(rnorm(size*(size+1)*(size+2)), dim = size + 0:2),
         "integerVector()" = sample(1:10, size = size, replace = TRUE),
         "logicalVector()" = sample(c(TRUE, FALSE), size = size, replace = TRUE))
}

test_math <- function(test_info, fun, argTypes, returnType,
                      dir = file.path(tempdir(), "nimble_generatedCode"),
                      control = list(),
                      verbose = FALSE) {
  nf <- nFunction(fun = fun,
                       argTypes = argTypes,
                       returnType = returnType)
  cpp <- nCompile_nFunction(nf, dir = dir, control = control)
  all_args <- lapply(argTypes, make_input)
  cpp_args <- all_args
  names(cpp_args) <- nCompiler:::mangleArgumentNames(names(cpp_args))
  ## For vectors, current R returns a vector while C++ returns a 1D array.
  ## We can modify the wrap<> specialization to change this.
  rAns <- do.call(fun, all_args)
  cAns <- do.call(cpp, cpp_args)
  if(is.array(cAns)) {
    expect_equivalent(as.array(rAns),
                      cAns,
                      info = paste0("Test: ", test_info))
  } else {
    expect_equivalent(rAns,
                      cAns,
                      info = paste0("Test: ", test_info))
  }
}

test_that("basics",
{
  ## +,-
  test_math("add to scalar",
            fun = function(x) return(x + 1.1),
            argTypes = list(x = "numericScalar()"),
            returnType = "numericScalar()")
  test_math("add to scalar, no dot",
            fun = function(x) return(x + 1),
            argTypes = list(x = "numericScalar()"),
            returnType = "numericScalar()")
  test_math("add to vector, no dot",
            fun = function(x) return(x + 1),
            argTypes = list(x = "numericVector()"),
            returnType = "numericVector()")
  test_math("add double and integer vectors, ",
            fun = function(x, y) { ans <- x + y; return(ans) },
            argTypes = list(x = "numericVector()",
                            y = "integerVector()"),
            returnType = "numericVector()")
  test_math("subtract logical and integer vectors, ",
            fun = function(x, y) { ans <- x - y; return(ans) },
            argTypes = list(x = "logicalVector()",
                            y = "integerVector()"),
            returnType = "integerVector()")

  ## *,/
  test_math("multiply boolean vector by scalar literal",
            fun = function(x) { ans <- 4*x; return(ans) },
            argTypes = list(x = "logicalVector()"),
            returnType = "numericVector()")
  test_math("divide boolean vector by scalar literal",
            fun = function(x) { ans <- x/4; return(ans) },
            argTypes = list(x = "logicalVector()"),
            returnType = "numericVector()")
  test_math("divide scalar literal by boolean vector",
            fun = function(x) { ans <- 4/x; return(ans) },
            argTypes = list(x = "logicalVector()"),
            returnType = "numericVector()")
  test_math("multiply integer vector by scalar literal",
            fun = function(x) { ans <- 4*x; return(ans) },
            argTypes = list(x = "integerVector()"),
            returnType = "numericVector()")
  test_math("divide integer vector by scalar literal",
            fun = function(x) { ans <- x/4; return(ans) },
            argTypes = list(x = "integerVector()"),
            returnType = "numericVector()")
  test_math("divide scalar literal by integer vector",
            fun = function(x) { ans <- 4/x; return(ans) },
            argTypes = list(x = "integerVector()"),
            returnType = "numericVector()")
  test_math("multiply double vector by scalar literal",
            fun = function(x) { ans <- 4*x; return(ans) },
            argTypes = list(x = "numericVector()"),
            returnType = "numericVector()")
  test_math("divide double vector by scalar literal",
            fun = function(x) { ans <- x/4; return(ans) },
            argTypes = list(x = "numericVector()"),
            returnType = "numericVector()")
  test_math("divide scalar literal by double vector",
            fun = function(x) { ans <- 4/x; return(ans) },
            argTypes = list(x = "numericVector()"),
            returnType = "numericVector()")
  test_math("multiply integer vector by double scalar variable",
            fun = function(x, y) { ans <- x*y; return(ans) },
            argTypes = list(x = "integerVector()",
                            y = "numericScalar()"),
            returnType = "numericVector()")
  test_math("divide integer vector by double scalar variable",
            fun = function(x, y) { ans <- x/y; return(ans) },
            argTypes = list(x = "integerVector()",
                            y = "numericScalar()"),
            returnType = "numericVector()")
  ## test_math("multiply logical vector by integer scalar variable", ## compile error
  ##           fun = function(x, y) { ans <- x*y; return(ans) },
  ##           argTypes = list(x = "logicalVector()",
  ##                           y = "integerScalar()"),
  ##           returnType = "numericVector()")

  ## mean
  test_math("mean of numeric vector",
            fun = function(x) {ans <- mean(x); return(ans)},
            argTypes = list(x = "numericVector()"),
            returnType = "numericScalar()")

  ## any, all
  ## expect_equivalent fails:
  ## Error: as.array(do.call(fun, all_args)) not equivalent to do.call(cpp, cpp_args).
  ## target is array, current is logical
  test_math("any of boolean vector",
            fun = function(x) {ans <- any(x); return(ans)},
            argTypes = list(x = "logicalVector()"),
            returnType = "logicalScalar()")
  test_math("all of boolean vector",
            fun = function(x) {ans <- all(x); return(ans)},
            argTypes = list(x = "logicalVector()"),
            returnType = "logicalScalar()")

  ## sqrt
  ## Error in eval(call(handler, code, symTab), envir = genCppEnv) : 
  ## attempt to use zero-length variable name
  ## test_math("sqrt of scalar", ## error
  ##           fun = function(x) {ans <- sqrt(x); return(ans)},
  ##           argTypes = list(x = "numericScalar()"),
  ##           returnType = "numericScalar()")
  test_math("sqrt of vector",
            fun = function(x) {ans <- sqrt(x); return(ans)},
            argTypes = list(x = "numericVector()"),
            returnType = "numericVector()")
  test_math("sqrt of matrix",
            fun = function(x) {ans <- sqrt(x); return(ans)},
            argTypes = list(x = "numericMatrix()"),
            returnType = "numericMatrix()")
  test_math("sqrt of array",
            fun = function(x) {ans <- sqrt(x); return(ans)},
            argTypes = list(x = "numericArray(nDim = 3)"),
            returnType = "numericArray(nDim = 3)")

  ## abs
  ## Error in eval(call(handler, code, symTab), envir = genCppEnv) : 
  ## attempt to use zero-length variable name
  ## test_math("abs of scalar", ## error
  ##           fun = function(x) {ans <- abs(x); return(ans)},
  ##           argTypes = list(x = "numericScalar()"),
  ##           returnType = "numericScalar()")
  test_math("abs of vector",
            fun = function(x) {ans <- abs(x); return(ans)},
            argTypes = list(x = "numericVector()"),
            returnType = "numericVector()")
  test_math("abs of matrix",
            fun = function(x) {ans <- abs(x); return(ans)},
            argTypes = list(x = "numericMatrix()"),
            returnType = "numericMatrix()")
  test_math("abs of array",
            fun = function(x) {ans <- abs(x); return(ans)},
            argTypes = list(x = "numericArray(nDim = 3)"),
            returnType = "numericArray(nDim = 3)")

  ## cube
  ## Error in eval(call(handler, code, symTab), envir = genCppEnv) : 
  ## attempt to use zero-length variable name
  ## test_math("cube of scalar", ## R error
  ##           fun = function(x) {ans <- cube(x); return(ans)},
  ##           argTypes = list(x = "numericScalar()"),
  ##           returnType = "numericScalar()")
  test_math("cube of vector",
            fun = function(x) {ans <- cube(x); return(ans)},
            argTypes = list(x = "numericVector()"),
            returnType = "numericVector()")
  test_math("cube of matrix",
            fun = function(x) {ans <- cube(x); return(ans)},
            argTypes = list(x = "numericMatrix()"),
            returnType = "numericMatrix()")
  test_math("cube of array",
            fun = function(x) {ans <- cube(x); return(ans)},
            argTypes = list(x = "numericArray(nDim = 3)"),
            returnType = "numericArray(nDim = 3)")

  ## square
  test_math("square of vector",
            fun = function(x) {ans <- square(x); return(ans)},
            argTypes = list(x = "numericVector()"),
            returnType = "numericVector()")

  ## exp
  ## Error in eval(call(handler, code, symTab), envir = genCppEnv) : 
  ## attempt to use zero-length variable name
  ## test_math("exp of scalar", ## R error
  ##           fun = function(x) {ans <- exp(x); return(ans)},
  ##           argTypes = list(x = "numericScalar()"),
  ##           returnType = "numericScalar()")
  test_math("exp of vector",
            fun = function(x) {ans <- exp(x); return(ans)},
            argTypes = list(x = "numericVector()"),
            returnType = "numericVector()")
  test_math("exp of matrix",
            fun = function(x) {ans <- exp(x); return(ans)},
            argTypes = list(x = "numericMatrix()"),
            returnType = "numericMatrix()")
  test_math("exp of array",
            fun = function(x) {ans <- exp(x); return(ans)},
            argTypes = list(x = "numericArray(nDim = 3)"),
            returnType = "numericArray(nDim = 3)")

  ## pow
  test_math("pow of vector with constant",
            fun = function(x) {ans <- x^4; return(ans)},
            argTypes = list(x = "numericVector()"),
            returnType = "numericVector()")
  test_math("pow of vector with variable",
            fun = function(x, y) {ans <- x^y; return(ans)},
            argTypes = list(x = "numericVector()",
                            y = "numericScalar()"),
            returnType = "numericVector()")
  test_math("pow of matrix with variable",
            fun = function(x, y) {ans <- x^y; return(ans)},
            argTypes = list(x = "numericMatrix()",
                            y = "numericScalar()"),
            returnType = "numericMatrix()")
  test_math("pow of array with variable",
            fun = function(x, y) {ans <- x^y; return(ans)},
            argTypes = list(x = "numericArray(nDim = 3)",
                            y = "numericScalar()"),
            returnType = "numericArray(nDim = 3)")

  ## ==,!=,<=,>=,<,>
  test_math("equality of two numeric vectors",
            fun = function(x, y) {ans <- x == y; return(ans)},
            argTypes = list(x = "numericVector()",
                            y = "numericVector()"),
            returnType = "logicalVector()")
  test_math("non-equality of two numeric vectors",
            fun = function(x, y) {ans <- x != y; return(ans)},
            argTypes = list(x = "numericVector()",
                            y = "numericVector()"),
            returnType = "logicalVector()")
  test_math("less than or equal comparison of two numeric vectors",
            fun = function(x, y) {ans <- x <= y; return(ans)},
            argTypes = list(x = "numericVector()",
                            y = "numericVector()"),
            returnType = "logicalVector()")
  test_math("greater than or equal comparison of two numeric vectors",
            fun = function(x, y) {ans <- x >= y; return(ans)},
            argTypes = list(x = "numericVector()",
                            y = "numericVector()"),
            returnType = "logicalVector()")
  test_math("less than comparison of two numeric vectors",
            fun = function(x, y) {ans <- x < y; return(ans)},
            argTypes = list(x = "numericVector()",
                            y = "numericVector()"),
            returnType = "logicalVector()")
  test_math("greater than comparison of two numeric vectors",
            fun = function(x, y) {ans <- x > y; return(ans)},
            argTypes = list(x = "numericVector()",
                            y = "numericVector()"),
            returnType = "logicalVector()")

  ## &,|
  test_math("boolean and of two boolean vectors",
            fun = function(x, y) {ans <- x & y; return(ans)},
            argTypes = list(x = "logicalVector()",
                            y = "logicalVector()"),
            returnType = "logicalVector()")
  test_math("boolean or of two boolean vectors",
            fun = function(x, y) {ans <- x | y; return(ans)},
            argTypes = list(x = "logicalVector()",
                            y = "logicalVector()"),
            returnType = "logicalVector()")

  ## %%
  ##test_math("modulo of double vector by double literal", ## compile error
  ##          fun = function(x) {ans <- x %% 3; return(ans)},
  ##          argTypes = list(x = "numericVector()"),
  ##          returnType = "numericVector()",
  ##          dir = "Cpp")

  ## pmax,pmin
  test_math("pmax of double vector",
            fun = function(x, y) {ans <- pmax(x, y); return(ans)},
            argTypes = list(x = "numericVector()",
                            y = "numericVector()"),
            returnType = "numericVector()")
  test_math("pmin of double vector",
            fun = function(x, y) {ans <- pmax(x, y); return(ans)},
            argTypes = list(x = "numericVector()",
                            y = "numericVector()"),
            returnType = "numericVector()")

})

library(nCompiler)
library(testthat)

test_that("ETaccessor type works", {
  nc <- nClass(
    Cpublic = list(
      s = 'numericScalar',
      v = 'numericVector',
      m = 'numericMatrix',
      get_s = nFunction(
        function() {
          ans <- ETaccess(s)
          return(ans)
          returnType('ETaccessor')
        }
      ),
      get_inner = nFunction(
        function(vn = 'string') {
          ans <- self[[vn]]
          return(ans)
          returnType('ETaccessor')
        }
      ),
      use = nFunction(
        function(acc = 'ETaccessor') {
          return(as(acc, "numericMatrix"))
          returnType("numericMatrix")
        }
      ),
      get = nFunction(
        function(i = 'integerScalar', vn = 'string') {
          nSwitch(i, 1:4,
                  eta <- get_s(),
                  eta <- get_inner(vn),
                  eta <- self[[vn]],
                  {
                    eta <- self[[vn]]
                    res <- use(eta)
                  }
                  )
          if(i < 4)
            res <- as(eta, "numericMatrix")
          return(res)
          returnType("numericMatrix")
        }
      )
    ),
    compileInfo=list(interfaceInclude = c("s","v","m", "get"))
  )

  cnc <- nCompile(nc)
  obj <- cnc$new()
  obj$s <- 1.2
  obj$v <- c(2.3, 3.4)
  obj$m <- matrix(5:10, nrow = 3)
  expect_equal(obj$get(1, "not_used"), matrix(1.2))
  expect_equal(obj$get(2, "v"), matrix(obj$v))
  expect_equal(obj$get(3, "m"), obj$m)
  expect_equal(obj$get(4, "v"), matrix(obj$v))
  rm(obj); gc()
})

## This test is disabled because we have for now
## changed the design decision about how this should work
## It should work like obj[[varName]] even if varName "x"
## so that `[[` always returns the same type.
## This is a real point of debate and may be reconsidered
## again.
## There was some problem encountered that led to this change.
##
## test_that("obj[['x']] works like obj$x", {
##   nc <- nClass(
##     Cpublic = list(
##       x = 'numericVector'
##     )
##   )
##   nc2 <- nClass(
##     Cpublic = list(
##       nf = nFunction(
##         function(obj = 'nc') {
##           v <- obj[["x"]]
##           return(v)
##           returnType('numericVector')
##         }
##       )
##     )
##   )
##   for(mode in c("R","non-pkg", "pkg")) {
##     if(mode == "R") {
##       obj <- nc$new()
##       obj2 <- nc2$new()
##     } else {
##       package <- mode=="pkg"
##       comp <- nCompile(nc, nc2, package = package)
##       obj <- comp$nc$new()
##       obj2 <- comp$nc2$new()
##     }
##     obj$x <- c(1.2, 2.3)
##     expect_equal(obj2$nf(obj), obj$x)
##     rm(obj); gc()
##   }
## })


test_that("obj[[var_name]] works", {
  nc <- nClass(
    Cpublic = list(
      x = 'numericVector'
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      nf = nFunction(
        function(obj = 'nc', var_name = 'string') {
          ETacc <- obj[[var_name]]
          v <- as(ETacc, 'numericVector')
          return(v)
          returnType('numericVector')
        }
      )
    )
  )
  for(mode in c("R","non-pkg", "pkg")) {
    if(mode == "R") {
      obj <- nc$new()
      obj2 <- nc2$new()
    } else {
      package <- mode=="pkg"
      comp <- nCompile(nc, nc2, package = package)
      obj <- comp$nc$new()
      obj2 <- comp$nc2$new()
    }
    obj$x <- c(1.2, 2.3)
    expect_equal(obj2$nf(obj, "x"), obj$x)
    rm(obj); gc()
  }
})

test_that("as(obj[[var_name]], type) works", {
  nc <- nClass(
    Cpublic = list(
      x = 'numericVector'
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      nf = nFunction(
        function(obj = 'nc', var_name = 'string') {
          v <- as(obj[[var_name]], 'numericVector')
          return(v)
          returnType('numericVector')
        }
      )
    )
  )
  for(mode in c("R","non-pkg", "pkg")) {
    if(mode == "R") {
      obj <- nc$new()
      obj2 <- nc2$new()
    } else {
      package <- mode=="pkg"
      comp <- nCompile(nc, nc2, package = package)
      obj <- comp$nc$new()
      obj2 <- comp$nc2$new()
    }
    obj$x <- c(1.2, 2.3)
    expect_equal(obj2$nf(obj, "x"), obj$x)
    rm(obj); gc()
  }
})

test_that("ETaccess(expr, copy = TRUE) works for tensor expressions", {
  # An expression such as a slice or a sum is evaluated into a tensor that the accessor owns.
  nc <- nClass(
    Cpublic = list(
      v = 'numericVector',
      m = 'numericMatrix',
      get_slice = nFunction(
        function() {
          ans <- ETaccess(v[2:3], copy = TRUE)
          return(ans)
          returnType('ETaccessor')
        }
      ),
      get_sum = nFunction(
        function() {
          ans <- ETaccess(m + 1, copy = TRUE)
          return(ans)
          returnType('ETaccessor')
        }
      ),
      use_slice = nFunction(
        function() {
          acc <- get_slice()
          return(as(acc, "numericVector"))
          returnType("numericVector")
        }
      )
    ),
    compileInfo = list(interfaceInclude = c("v", "m", "get_slice", "get_sum", "use_slice"))
  )
  cnc <- nCompile(nc)
  obj <- cnc$new()
  obj$v <- c(1.5, 2.5, 3.5, 4.5)
  obj$m <- matrix(1:6, nrow = 2)
  expect_equal(obj$get_slice(), c(2.5, 3.5))
  expect_equal(obj$get_sum(), matrix(1:6, nrow = 2) + 1)
  expect_equal(obj$use_slice(), c(2.5, 3.5))
  rm(obj); gc()
})

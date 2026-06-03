library(nCompiler)
library(testthat)

test_that("obj[['x']] works like obj$x", {
  nc <- nClass(
    Cpublic = list(
      x = 'numericVector'
    )
  )
  nf <- nFunction(
    function(obj = 'nc') {
      v <- obj[["x"]]
      return(v)
      returnType('numericVector')
    }
  )
  for(mode in c("R","non-pkg", "pkg")) {
    if(mode == "R") {
      obj <- nc$new()
      foo <- nf
    } else {
      package <- mode=="pkg"
      comp <- nCompile(nc, nf, package = package)
      obj <- comp$nc$new()
      foo <- comp$nf
    }
    obj$x <- c(1.2, 2.3)
    expect_equal(foo(obj), obj$x)
    rm(obj); gc()
  }
})


test_that("obj[[var_name]] works", {
  nc <- nClass(
    Cpublic = list(
      x = 'numericVector'
    )
  )
  nf <- nFunction(
    function(obj = 'nc', var_name = 'string') {
      ETacc <- obj[[var_name]]
      v <- as(ETacc, 'numericVector')
      return(v)
      returnType('numericVector')
    }
  )
  for(mode in c("R","non-pkg", "pkg")) {
    if(mode == "R") {
      obj <- nc$new()
      foo <- nf
    } else {
      package <- mode=="pkg"
      comp <- nCompile(nc, nf, package = package)
      obj <- comp$nc$new()
      foo <- comp$nf
    }
    obj$x <- c(1.2, 2.3)
    foo(obj, "x")
    expect_equal(foo(obj, "x"), obj$x)
    rm(obj); gc()
  }
})

test_that("as(obj[[var_name]], type) works", {
  nc <- nClass(
    Cpublic = list(
      x = 'numericVector'
    )
  )
  nf <- nFunction(
    function(obj = 'nc', var_name = 'string') {
      v <- as(obj[[var_name]], 'numericVector')
        return(v)
      returnType('numericVector')
    }
  )
  for(mode in c("R","non-pkg", "pkg")) {
    if(mode == "R") {
      obj <- nc$new()
      foo <- nf
    } else {
      package <- mode=="pkg"
      comp <- nCompile(nc, nf, package = package)
      obj <- comp$nc$new()
      foo <- comp$nf
    }
    obj$x <- c(1.2, 2.3)
    foo(obj, "x")
    expect_equal(foo(obj, "x"), obj$x)
    rm(obj); gc()
  }
})

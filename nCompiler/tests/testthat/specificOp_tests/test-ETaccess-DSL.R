library(nCompiler)
library(testthat)

test_that("obj[['x']] works like obj$x", {
  nc <- nClass(
    Cpublic = list(
      x = 'numericVector'
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      nf = nFunction(
        function(obj = 'nc') {
          v <- obj[["x"]]
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
    expect_equal(obj2$nf(obj), obj$x)
    rm(obj); gc()
  }
})


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

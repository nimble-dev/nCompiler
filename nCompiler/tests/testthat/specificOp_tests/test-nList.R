message("nList uncompiled is rudimentary.")
message("nList needs tests of nLists of nClass types or even other nLists")
#debug(nCompiler:::type2symbol)

# working here on nList2, a working step towards a new more fully featured nList


## # To-do:
## #  polish the many set and get cases
## #  decide on support for recycling rule
## #  provide a get and set ALL
## #  error trap:
## #  checks on index ranges throughout
## #  decide on automatic extension behavior
## #  implement bracket and double bracket etc.
## #  finish compilation tracking of nClassBuilder.
## #  opDef for compilation.

## # To-do:
## # think about the type TBD step
## #   We will need a class_shared and project_shared env to
## #   stick into each auxEnv. I don't see another way.
## #   Then resolveSymbolTBD will need to invoke the nClassBuilder etc.
## #
## # figure out to manage output more

test_that("nList('numericScalar') works in nFunction", {
  foo <- nFunction(
    function(x = "nList('numericScalar')") {
      ans <- nList('numericScalar', 4)
      ans[[1]] <- 10.5
      z <- ans[[1]]
      ans[[2]] <- z+1
      ans[[3]] <- x[[2]]
      return(ans)
    },
    returnType = "nList(numericScalar())"
  )
  cfoo <- nCompile(foo)
  x <- list(1, 2, 3)
  expect_identical(cfoo(x), list(10.5, 11.5, 2, 0))
  expect_identical(foo(x), list(10.5, 11.5, 2, NULL))
  cfoo <- nCompile(foo, package = TRUE)
  x <- list(1, 2, 3)
  expect_identical(cfoo(x), list(10.5, 11.5, 2, 0))
  expect_identical(foo(x), list(10.5, 11.5, 2, NULL))
})

test_that("nList('numericVector') works in nFunction", {
  foo <- nFunction(
    function(x = "nList('numericVector()')") {
      ans <- nList('numericVector', 4)
      ans[[1]] <- x[[1]]
      ans[[2]] <- ans[[1]] + 1
      ans[[3]] <- x[[2]]
      for(i in 1:3)
        ans[[3]][i] <- ans[[3]][i] + 1000
      return(ans)
    },
    returnType = "nList('numericVector')" # How do alternate modes of saying this go through?
  )
  x <- list(1:3, 11:13)
  expect_identical(foo(x), list(x[[1]], x[[1]]+1, x[[2]]+1000, NULL))
  cfoo <- nCompile(foo)
  expect_equal(cfoo(x), list(x[[1]], x[[1]]+1, x[[2]]+1000, numeric(0)))
})

test_that("nList(my_type) finds my_type by correct scoping from nFunction", {
  make_foo <- function() {
    my_type <- nType(numericVector())
    foo <- nFunction(
      function(x = "nList(T(my_type))") {
        ans <- nList('T(my_type)', 4)
        ans[[1]] <- x[[1]]
        ans[[2]] <- ans[[1]] + 1
        ans[[3]] <- x[[2]]
        for(i in 1:3)
          ans[[3]][i] <- ans[[3]][i] + 1000
        return(ans)
      },
      returnType = "nList({{my_type}})" # How do alternate modes of saying this go through?
    )
    foo
  }
  foo <- make_foo()
  x <- list(1:3, 11:13)
  expect_identical(foo(x), list(x[[1]], x[[1]]+1, x[[2]]+1000, NULL))
  cfoo <- nCompile(foo)
  expect_equal(cfoo(x), list(x[[1]], x[[1]]+1, x[[2]]+1000, numeric(0)))
})

test_that("nList('numericVector') works in nClass", {
  nc <- nClass(
    Cpublic = list(
      list_vec = "nList('numericVector')",
      foo = nFunction(
        function(x = "nList(numericScalar())") {
          x[[1]] <- list_vec[[2]][2]
          return(x)
          returnType("nList('numericScalar')")
        })
    )
  )
  nc1 <- nc$new()
  nc1$list_vec <- list(1:3, 11:13)
  x <- list(101, 102, 103)
  expect_equal(nc1$foo(x), list(12, 102, 103))
  Cnc <- nCompile(nc)
  obj <- Cnc$new()
  obj$list_vec <- nc1$list_vec
  expect_equal(obj$list_vec, nc1$list_vec)
  expect_equal(obj$foo(x), list(12, 102, 103))
  rm(obj); gc();

  Cnc <- nCompile(nc, package = TRUE)
  obj <- Cnc$new()
  obj$list_vec <- nc1$list_vec
  expect_equal(obj$list_vec, nc1$list_vec)
  expect_equal(obj$foo(x), list(12, 102, 103))
  rm(obj); gc()
})

test_that("nList(my_type) finds my_type by correct scoping from nClass checking multiple pathways", {
  make_nc <- function() {
    my_Vtype <- nType(numericVector())
    my_Stype <- nType(numericScalar())
    nc <- nClass(
      Cpublic = list(
        list_vec = "nList(T(my_Vtype))",
        foo = nFunction(
          function(x = nList(T(my_Stype))) {
            x[[1]] <- list_vec[[2]][2]
            return(x)
            returnType("nList('numericScalar')")
          })
      )
    )
    nc
  }
  nc <- make_nc()

  nc1 <- nc$new()
  nc1$list_vec <- list(1:3, 11:13)
  x <- list(101, 102, 103)
  expect_equal(nc1$foo(x), list(12, 102, 103))
  Cnc <- nCompile(nc)
  obj <- Cnc$new()
  obj$list_vec <- nc1$list_vec
  expect_equal(obj$list_vec, nc1$list_vec)
  expect_equal(obj$foo(x), list(12, 102, 103))
  rm(obj); gc();

  Cnc <- nCompile(nc, package = TRUE)
  obj <- Cnc$new()
  obj$list_vec <- nc1$list_vec
  expect_equal(obj$list_vec, nc1$list_vec)
  expect_equal(obj$foo(x), list(12, 102, 103))
  rm(obj); gc()
})

test_that("nList of an nClass, requiring delayed type resolution, combined with scoping", {
  ## mync <- nClass(Cpublic=list(x = 'numericVector'))
  ## elemType <- nType(mync())
  ## nListType <- nType(nList(T(elemType)))
  ## nListSym <- nCompiler:::type2symbol({{nListType}})
  ## nListSym$elementSym$uniqueID()
  ## nListSym$uniqueID()
  ## nListSym$cpp_typename()

  make_ncAB <- function() {
    my_Vtype <- nType(numericVector())
    my_Stype <- nType(numericScalar())
    nc <- nClass(
      Cpublic = list(
        list_vec = "nList(T(my_Vtype))",
        foo = nFunction(
          function(x = "nList(T(my_Stype))") {
            x[[1]] <- list_vec[[2]][2]
            return(x)
            returnType("nList('numericScalar')")
          })
      )
    )
    my_nctype <- nType(nc())
    ncB <- nClass(
      Cpublic = list(
        list_nc = "nList(nc())",
        one = nFunction(function() return(1), returnType = 'integerScalar'),
        fooB = nFunction(
          function(x = "nList(T(my_nctype))") {
            list_nc2 <- nList(nc(), 2)
            list_nc2[[1]] <- x[[2]]
            list_nc2[[2]] <- nc$new()
            list_nc2[[2]]$list_vec <- nList(numericVector(), 2)
            list_nc2[[2]]$list_vec[[1]] <- 8:10
            return(list_nc2)
            returnType(nList(nc()))
          }
         )
      )
    )
    list(nc = nc, ncB = ncB)
  }

  ncAB <- make_ncAB()
  nc <- ncAB$nc
  ncB <- ncAB$ncB

  # uncompiled nc
  nc1 <- nc$new()
  nc1$list_vec <- list(1:3, 11:13)
  x <- list(101, 102, 103)
  expect_equal(nc1$foo(x), list(12, 102, 103))

  for(package in c(FALSE, TRUE)) {
  CncAB <- nCompile(nc, ncB, package = package)
  Cnc <- CncAB$nc
  CncB <- CncAB$ncB

  # compiled nc
  obj <- Cnc$new()
  obj$list_vec <- nc1$list_vec
  expect_equal(obj$list_vec, nc1$list_vec)
  expect_equal(obj$foo(x), list(12, 102, 103))

  # compiled ncB
  objB <- CncB$new()
  objB$list_nc <- list(obj)
  expect_identical(objB$list_nc[[1]]$list_vec, obj$list_vec)
  objB$list_nc[[1]]$list_vec[[1]] <- 4:6
  expect_equal(obj$list_vec[[1]], 4:6)
  obj2 <- Cnc$new()
  obj2$list_vec <- list(5:8, 15:18)
  res <- objB$fooB(list(obj, obj2))
  expect_equal(res[[1]]$list_vec, obj2$list_vec)
  expect_equal(res[[2]]$list_vec, list(8:10, numeric()))

  rm(obj, objB); gc();
  }
})

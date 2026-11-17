## flex_(logical_scalar) = ... does not seem to work.
test_that("flex_ system works for mean()", {
  # set_nOption("use_flexible_assignment", TRUE)
  meanX <- nFunction(
    fun = function(x) {
      ans <- mean(x)
      return(ans)
    },
    argTypes = list(x = "numericVector()"),
    returnType = "numericScalar()"
  )

  x <- rnorm(10)
  expect_equal(meanX(x), mean(x), info = "uncompiled meanX works")
  CmeanX <- nCompile(meanX)
  expect_equal(CmeanX(x), meanX(x), info = "uncompiled meanX works")
})

# Assignments generated with flex_() (see checkFlexAssignment and tensorFlex.h).
# See also cpp_tests/test-tensorFlex-cpp.R for direct C++ tests.
flexAssignFuns <- list(
  vec_from_matprod = nFunction(
    function(X = "numericMatrix", b = "numericVector") {
      y <- nNumeric(1)
      y <- X %*% b
      return(y)
      returnType("numericVector")
    }
  ),
  vec_from_mat = nFunction(
    function(X = "numericMatrix") {
      y <- nNumeric(1)
      y <- X
      return(y)
      returnType("numericVector")
    }
  ),
  mat_from_vec = nFunction(
    function(v = "numericVector") {
      M <- nMatrix(0, 1, 1)
      M <- v
      return(M)
      returnType("numericMatrix")
    }
  ),
  vec_from_scalar = nFunction(
    function(a = "numericScalar") {
      y <- nNumeric(3)
      y <- a
      return(y)
      returnType("numericVector")
    }
  ),
  vecidx_from_scalar = nFunction(
    function(a = "numericScalar") {
      y <- nNumeric(5)
      y[2:4] <- a
      return(y)
      returnType("numericVector")
    }
  ),
  vecidx_from_mat = nFunction(
    function(X = "numericMatrix") {
      y <- nNumeric(5)
      y[2:4] <- X
      return(y)
      returnType("numericVector")
    }
  ),
  vecidx_from_vec = nFunction(
    function(x = "numericVector") {
      y <- nNumeric(5)
      y[2:4] <- x
      return(y)
      returnType("numericVector")
    }
  ),
  matcol_from_vec = nFunction(
    function(v = "numericVector") {
      M <- nMatrix(0, 3, 3)
      M[, 2] <- v
      return(M)
      returnType("numericMatrix")
    }
  ),
  scalar_from_mat = nFunction(
    function(X = "numericMatrix") {
      a <- 0
      a <- X
      return(a)
      returnType("numericScalar")
    }
  )
)

cFlexAssignFuns <- do.call(nCompile, flexAssignFuns)

test_that("flex_ assignment works for whole-object LHS with different nDim or scalar RHS", {
  X <- matrix(1:6, 3)
  b <- c(1, 2)
  expect_equal(cFlexAssignFuns$vec_from_matprod(X, b), as.numeric(X %*% b))
  expect_error(cFlexAssignFuns$vec_from_matprod(X, cbind(b, b)))
  expect_equal(cFlexAssignFuns$vec_from_mat(matrix(1:3, 3, 1)), c(1, 2, 3))
  expect_equal(cFlexAssignFuns$vec_from_mat(matrix(1:3, 1, 3)), c(1, 2, 3))
  expect_error(cFlexAssignFuns$vec_from_mat(X), "Extents must match")
  expect_equal(cFlexAssignFuns$mat_from_vec(1:3), matrix(1:3, 3, 1))
  expect_equal(cFlexAssignFuns$vec_from_scalar(2.5), 2.5)
})

test_that("flex_ assignment works for indexed LHS", {
  expect_equal(cFlexAssignFuns$vecidx_from_scalar(7), c(0, 7, 7, 7, 0))
  expect_equal(cFlexAssignFuns$vecidx_from_mat(matrix(1:3, 1, 3)), c(0, 1, 2, 3, 0))
  expect_equal(cFlexAssignFuns$vecidx_from_mat(matrix(1:3, 3, 1)), c(0, 1, 2, 3, 0))
  expect_error(cFlexAssignFuns$vecidx_from_mat(matrix(1:6, 2, 3)), "Extents must match")
  expect_equal(cFlexAssignFuns$vecidx_from_vec(4:6), c(0, 4, 5, 6, 0))
  # sizes are checked when check_sizes_on_assignment is TRUE (the default)
  expect_error(cFlexAssignFuns$vecidx_from_vec(1:2), "Extents must match")
  expect_error(cFlexAssignFuns$matcol_from_vec(1:4), "Extents must match")
  M <- matrix(0, 3, 3)
  M[, 2] <- 1:3
  expect_equal(cFlexAssignFuns$matcol_from_vec(1:3), M)
})

test_that("flex_ assignment to a scalar requires exactly one element", {
  expect_equal(cFlexAssignFuns$scalar_from_mat(matrix(4, 1, 1)), 4)
  expect_error(cFlexAssignFuns$scalar_from_mat(matrix(1:4, 2)), "to a scalar")
})

test_that("check_sizes_on_assignment = FALSE uses plain assignment for indexed LHS with the same nDim", {
  old <- get_nOption("check_sizes_on_assignment")
  on.exit(set_nOption("check_sizes_on_assignment", old))
  set_nOption("check_sizes_on_assignment", FALSE)
  nf <- nFunction(
    function(x = "numericVector") {
      y <- nNumeric(5)
      y[2:4] <- x
      return(y)
      returnType("numericVector")
    }
  )
  cnf <- nCompile(nf)
  expect_equal(cnf(4:6), c(0, 4, 5, 6, 0))
  # Mismatched sizes are not checked here and would be undefined behavior, so they are not tested.
})

# Tests for the as() keyword.
#
# as(object, type) reinterprets the shape or scalar type of an object at
# compile time using Eigen TensorMap / StridedTensorMap views to minimise
# copies.  Cross-scalar LHS uses a CastingProxy that writes element values
# back to the source on destruction.
#
# Shape semantics (same as mapTyped): when the target nDim is less than the
# source nDim, singleton dimensions are dropped; when more, 1s are appended.
# A runtime error is thrown if too many non-singleton dimensions would be lost.
#
# Items not yet implemented (marked with skip()):
#   - Uncompiled (R-side) execution of as().
#   - Cross-scalar LHS: requires CastingProxy::operator=.
#   - Immediate indexing of an as() expression: as(x, type)[i] (complex Eigen
#     interaction between STM and the indexing subsystem).
#   - Runtime-source path: ETaccessorBase input from genericInterfaceC.

# ---------------------------------------------------------------------------
# Same scalar, dimension change — RHS
# ---------------------------------------------------------------------------

test_that("as(): same scalar 2D→1D singleton-drop, RHS", {
  nc <- nClass(
    Cpublic = list(
      col_to_vec = nFunction(
        function(x = numericMatrix) {
          ans <- as(x, "numericVector")
          return(ans)
          returnType(numericVector)
        }
      ),
      row_to_vec = nFunction(
        function(x = numericMatrix) {
          ans <- as(x, "numericVector")
          return(ans)
          returnType(numericVector)
        }
      )
    )
  )
  for(package in c(FALSE, TRUE)) {
    ncc <- nCompile(nc, package = package)
    nco <- ncc$new()

    # Single-column (n×1) matrix: singleton column dim is dropped.
    x_col <- matrix(as.numeric(1:6), nrow = 6, ncol = 1)
    expect_equal(nco$col_to_vec(x_col), as.numeric(1:6))

    # Single-row (1×n) matrix: singleton row dim is dropped.
    x_row <- matrix(as.numeric(1:6), nrow = 1, ncol = 6)
    expect_equal(nco$row_to_vec(x_row), as.numeric(1:6))
  }
  rm(nco); gc()
})

test_that("as(): same scalar 1D→2D dimension padding, RHS", {
  nc <- nClass(
    Cpublic = list(
      vec_to_mat = nFunction(
        function(x = numericVector) {
          ans <- as(x, "numericMatrix")
          return(ans)
          returnType(numericMatrix)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()

  # A length-n vector is viewed as an (n×1) matrix (second dim padded with 1).
  x <- as.numeric(1:5)
  expect_equal(nco$vec_to_mat(x), matrix(x, ncol = 1))
  rm(nco); gc()
})

test_that("as(): same scalar, result used in arithmetic", {
  nc <- nClass(
    Cpublic = list(
      sum_col = nFunction(
        function(x = numericMatrix) {
          ans <- sum(as(x, "numericVector"))
          ans <- ans + sum(as(x, "numericVector") + (1:length(x)))
          # ans <- sum(v)
          return(ans)
          returnType(numericScalar)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()

  x <- matrix(as.numeric(1:4), nrow = 4, ncol = 1)
  expect_equal(nco$sum_col(x), sum(x) + sum(x + 1:length(x)))
  rm(nco); gc()
})

# ---------------------------------------------------------------------------
# Cross scalar — RHS (lazy Eigen cast, no copy until evaluation)
# ---------------------------------------------------------------------------

test_that("as(): cross scalar double→integer, RHS", {
  nc <- nClass(
    Cpublic = list(
      to_int = nFunction(
        function(x = numericVector) {
          ans <- as(x, "integerVector")
          return(ans)
          returnType(integerVector)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()

  x <- c(1.9, 2.1, 3.7, -0.5)
  expect_equal(nco$to_int(x), as.integer(x))
})

test_that("as(): cross scalar integer→double, RHS", {
  nc <- nClass(
    Cpublic = list(
      to_double = nFunction(
        function(x = integerVector) {
          ans <- as(x, "numericVector")
          return(ans)
          returnType(numericVector)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()

  x <- 1:5L
  expect_equal(nco$to_double(x), as.numeric(x))
  rm(nco); gc()
})

test_that("as(): cross scalar logical→double, RHS", {
  nc <- nClass(
    Cpublic = list(
      to_double = nFunction(
        function(x = logicalVector) {
          ans <- as(x, "numericVector")
          return(ans)
          returnType(numericVector)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()

  x <- c(TRUE, FALSE, TRUE, FALSE)
  expect_equal(nco$to_double(x), as.numeric(x))
  rm(nco); gc()
})

test_that("as(): cross scalar integer→double 2D, RHS", {
  nc <- nClass(
    Cpublic = list(
      mat_to_double = nFunction(
        function(x = integerMatrix) {
          ans <- as(x, "numericMatrix")
          return(ans)
          returnType(numericMatrix)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()

  x <- matrix(1:6L, nrow = 2, ncol = 3)
  expect_equal(nco$mat_to_double(x), matrix(as.numeric(1:6), nrow = 2, ncol = 3))
  rm(nco); gc()
})

# ---------------------------------------------------------------------------
# Same scalar — LHS (assign through StridedTensorMap view)
# ---------------------------------------------------------------------------

test_that("as(): same scalar 1D viewed as 2D on LHS writes through STM", {
  nc <- nClass(
    Cpublic = list(
      assign_via_view = nFunction(
        function(x = numericVector, y = numericMatrix) {
          as(x, "numericMatrix") <- y
          return(x)
          returnType(numericVector)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()

  # x has 5 elements; y is (5×1), matching the (n,1) view of x.
  # After assignment, x should contain y's values.
  x <- as.numeric(1:5)
  y <- matrix(as.numeric(6:10), nrow = 5, ncol = 1)
  expect_equal(nco$assign_via_view(x, y), as.numeric(6:10))
  rm(nco); gc()
})

test_that("as(): same scalar 2D viewed as 1D on LHS writes through STM", {
  nc <- nClass(
    Cpublic = list(
      assign_via_view = nFunction(
        function(x = numericMatrix, y = numericVector) {
          as(x, "numericVector") <- y
          return(x)
          returnType(numericMatrix)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()

  # x is a (5×1) matrix; y is a length-5 vector.
  # After assignment, x should contain y's values in its single column.
  x <- matrix(as.numeric(1:5), nrow = 5, ncol = 1)
  y <- as.numeric(6:10)
  expect_equal(nco$assign_via_view(x, y), matrix(y, nrow = 5, ncol = 1))
  rm(nco); gc()
})

# ---------------------------------------------------------------------------
# Runtime error for incompatible shapes
# ---------------------------------------------------------------------------

test_that("as(): runtime error when non-singleton dims cannot be dropped", {
  nc <- nClass(
    Cpublic = list(
      bad_reshape = nFunction(
        function(x = numericMatrix) {
          ans <- as(x, "numericVector")
          return(ans)
          returnType(numericVector)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()

  # A (3×4) matrix has two non-singleton dims — cannot map to 1D.
  x <- matrix(as.numeric(1:12), nrow = 3, ncol = 4)
  expect_error(nco$bad_reshape(x))
  rm(nco); gc()
})

# ---------------------------------------------------------------------------
# Pending / not yet implemented
# ---------------------------------------------------------------------------

test_that("as(): cross-scalar LHS (CastingProxy write-back)", {
  skip("Cross-scalar LHS requires CastingProxy::operator= — not yet implemented.")
})

test_that("as(): immediate indexing of as() result — as(x, type)[i]", {
  skip("Indexing directly on an as() expression (STM path) — not yet implemented.")
})

test_that("as(): runtime-source path via ETaccessorBase", {
  skip("Runtime-source path (ETaccessorBase from genericInterfaceC) — not yet integrated.")
})

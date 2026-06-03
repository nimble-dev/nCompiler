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
# Each test runs in three modes: uncompiled R ("R"), compiled non-package
# ("non_pkg"), and compiled package ("pkg").
#
# All planned cases are implemented.

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
  package <- FALSE
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    # Single-column (n×1) matrix: singleton column dim is dropped.
    x_col <- matrix(as.numeric(1:6), nrow = 6, ncol = 1)
    expect_equal(nco$col_to_vec(x_col), as.numeric(1:6))

    # Single-row (1×n) matrix: singleton row dim is dropped.
    x_row <- matrix(as.numeric(1:6), nrow = 1, ncol = 6)
    expect_equal(nco$row_to_vec(x_row), as.numeric(1:6))
    rm(nco); gc()
  }
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
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    # A length-n vector is viewed as an (n×1) matrix (second dim padded with 1).
    x <- as.numeric(1:5)
    expect_equal(nco$vec_to_mat(x), matrix(x, ncol = 1))
    rm(nco); gc()
  }
})

test_that("as(): same scalar, result used in arithmetic", {
  nc <- nClass(
    Cpublic = list(
      sum_col = nFunction(
        function(x = numericMatrix) {
          ans <- sum(as(x, "numericVector"))
          ans <- ans + sum(as(x, "numericVector") + (1:length(x)))
          return(ans)
          returnType(numericScalar)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- matrix(as.numeric(1:4), nrow = 4, ncol = 1)
    expect_equal(nco$sum_col(x), sum(x) + sum(x + 1:length(x)))
    rm(nco); gc()
  }
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
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- c(1.9, 2.1, 3.7, -0.5)
    expect_equal(nco$to_int(x), as.integer(x))
    rm(nco); gc()
  }
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
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- 1:5L
    expect_equal(nco$to_double(x), as.numeric(x))
    rm(nco); gc()
  }
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
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- c(TRUE, FALSE, TRUE, FALSE)
    expect_equal(nco$to_double(x), as.numeric(x))
    rm(nco); gc()
  }
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
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- matrix(1:6L, nrow = 2, ncol = 3)
    expect_equal(nco$mat_to_double(x), matrix(as.numeric(1:6), nrow = 2, ncol = 3))
    rm(nco); gc()
  }
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
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    # x has 5 elements; y is (5×1), matching the (n,1) view of x.
    # After assignment, x should contain y's values.
    x <- as.numeric(1:5)
    y <- matrix(as.numeric(6:10), nrow = 5, ncol = 1)
    expect_equal(nco$assign_via_view(x, y), as.numeric(6:10))
    rm(nco); gc()
  }
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
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    # x is a (5×1) matrix; y is a length-5 vector.
    # After assignment, x should contain y's values in its single column.
    x <- matrix(as.numeric(1:5), nrow = 5, ncol = 1)
    y <- as.numeric(6:10)
    expect_equal(nco$assign_via_view(x, y), matrix(y, nrow = 5, ncol = 1))
    rm(nco); gc()
  }
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
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    # A (3×4) matrix has two non-singleton dims — cannot map to 1D.
    x <- matrix(as.numeric(1:12), nrow = 3, ncol = 4)
    expect_error(nco$bad_reshape(x))
    rm(nco); gc()
  }
})

# ---------------------------------------------------------------------------
# Cross-scalar LHS (CastingProxy write-back)
# ---------------------------------------------------------------------------

test_that("as(): cross-scalar LHS — double source viewed as integer, integer assigned, writes back double", {
  nc <- nClass(
    Cpublic = list(
      assign_int_to_double = nFunction(
        function(x = numericVector, y = integerVector) {
          as(x, "integerVector") <- y
          return(x)
          returnType(numericVector)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- c(1.5, 2.5, 3.5)
    y <- c(10L, 20L, 30L)
    expect_equal(nco$assign_int_to_double(x, y), as.numeric(y))
    rm(nco); gc()
  }
})

test_that("as(): cross-scalar LHS — integer source viewed as double, double assigned, writes back integer", {
  nc <- nClass(
    Cpublic = list(
      assign_double_to_int = nFunction(
        function(x = integerVector, y = numericVector) {
          as(x, "numericVector") <- y
          return(x)
          returnType(integerVector)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- 1:3L
    y <- c(10.9, 20.1, 30.7)
    # double values are truncated to integer on write-back
    expect_equal(nco$assign_double_to_int(x, y), as.integer(y))
    rm(nco); gc()
  }
})

# ---------------------------------------------------------------------------
# Indexing of as() results
# ---------------------------------------------------------------------------

# RHS scalar result — same scalar -----------------------------------------------

test_that("as(): RHS scalar indexing same-scalar (all-singleton)", {
  nc <- nClass(
    Cpublic = list(
      get_elem = nFunction(
        function(x = numericVector) {
          ans <- as(x, "numericMatrix")[2, 1]
          return(ans)
          returnType(numericScalar)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- c(10.0, 20.0, 30.0)   # viewed as 3×1 matrix; [2,1] == x[2]
    expect_equal(nco$get_elem(x), 20.0)
    rm(nco); gc()
  }
})

# RHS scalar result — cross scalar -----------------------------------------------

test_that("as(): RHS scalar indexing cross-scalar (double→integer)", {
  nc <- nClass(
    Cpublic = list(
      get_int = nFunction(
        function(x = numericVector) {
          ans <- as(x, "integerVector")[2]
          return(ans)
          returnType(integerScalar)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- c(1.9, 2.7, 3.1)
    expect_equal(nco$get_int(x), 2L)   # truncation toward zero
    rm(nco); gc()
  }
})

# RHS range result — same scalar -------------------------------------------------

test_that("as(): RHS range indexing same-scalar (pure range)", {
  nc <- nClass(
    Cpublic = list(
      sub_mat = nFunction(
        function(x = numericVector) {
          ans <- as(x, "numericMatrix")[1:3, 1:1, drop=FALSE]
          return(ans)
          returnType(numericMatrix)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- as.numeric(1:5)   # viewed as 5×1; rows 1:3, col 1:1 → 3×1
    expect_equal(nco$sub_mat(x), matrix(1:3, ncol = 1))
    rm(nco); gc()
  }
})

# RHS range result — cross scalar ------------------------------------------------

test_that("as(): RHS range indexing cross-scalar (integer→double)", {
  nc <- nClass(
    Cpublic = list(
      sub_dbl = nFunction(
        function(x = integerVector) {
          ans <- as(x, "numericMatrix")[1:3, 1:1, drop=FALSE]
          return(ans)
          returnType(numericMatrix)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- 1:5L   # viewed as 5×1; rows 1:3 → 3×1 double matrix
    expect_equal(nco$sub_dbl(x), matrix(as.numeric(1:3), ncol = 1))
    rm(nco); gc()
  }
})

# LHS scalar write-back — same scalar -------------------------------------------

test_that("as(): LHS scalar assignment same-scalar (all-singleton)", {
  nc <- nClass(
    Cpublic = list(
      set_elem = nFunction(
        function(x = numericVector, val = numericScalar) {
          as(x, "numericMatrix")[2, 1] <- val
          return(x)
          returnType(numericVector)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- c(10.0, 20.0, 30.0)
    result <- nco$set_elem(x, 99.0)
    expect_equal(result, c(10.0, 99.0, 30.0))
    rm(nco); gc()
  }
})

# LHS range write-back — same scalar --------------------------------------------

test_that("as(): LHS range assignment same-scalar (pure range)", {
  nc <- nClass(
    Cpublic = list(
      set_range = nFunction(
        function(x = numericVector, y = numericMatrix) {
          as(x, "numericMatrix")[1:3, 1:1] <- y
          return(x)
          returnType(numericVector)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- as.numeric(1:5)
    y <- matrix(c(10.0, 20.0, 30.0), nrow = 3, ncol = 1)
    result <- nco$set_range(x, y)
    expect_equal(result, c(10.0, 20.0, 30.0, 4.0, 5.0))
    rm(nco); gc()
  }
})

# LHS scalar write-back — cross scalar ------------------------------------------

test_that("as(): LHS scalar assignment cross-scalar (double source, integer view)", {
  nc <- nClass(
    Cpublic = list(
      set_int_elem = nFunction(
        function(x = numericVector, val = integerScalar) {
          as(x, "integerMatrix")[2, 1] <- val
          return(x)
          returnType(numericVector)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- c(10.0, 20.0, 30.0)   # double; view as int matrix, assign int to [2,1]
    result <- nco$set_int_elem(x, 99L)
    expect_equal(result, c(10.0, 99.0, 30.0))
    rm(nco); gc()
  }
})

# LHS range write-back — cross scalar -------------------------------------------

test_that("as(): LHS range assignment cross-scalar (double source, integer view)", {
  nc <- nClass(
    Cpublic = list(
      set_int_range = nFunction(
        function(x = numericVector, y = integerMatrix) {
          as(x, "integerMatrix")[1:3, 1:1] <- y
          return(x)
          returnType(numericVector)
        }
      )
    )
  )
  for(mode in c("R", "non_pkg", "pkg")) {
    if(mode == "R") {
      nco <- nc$new()
    } else {
      package <- mode == "pkg"
      ncc <- nCompile(nc, package = package)
      nco <- ncc$new()
    }

    x <- as.numeric(1:5)
    y <- matrix(c(10L, 20L, 30L), nrow = 3, ncol = 1)
    result <- nco$set_int_range(x, y)
    expect_equal(result, c(10.0, 20.0, 30.0, 4.0, 5.0))
    rm(nco); gc()
  }
})

# Inputs are true scalars, target type is non-scalar
# same scalar element type
test_that("as(): true scalar input (same element type)", {
  foo <- nFunction(
    function(x = 'numericScalar', scalar_res = numericVector()) {
      v <- as(x, 'numericMatrix')
      y <- x
      w <- v
      as(y, "numericMatrix") <- 3*w
      scalar_res[1] <- y
      return(v)
      returnType(double(2))
    },
    refArgs = "scalar_res"
  )
  cfoo <- nCompile(foo)
  scalar_res <- 0
  ans <- foo(2, scalar_res)
  cscalar_res <- -1
  cans <- cfoo(2, cscalar_res)
  expect_identical(ans, cans)
  expect_identical(dim(cans), c(1L,1L))
  expect_equal(cans[1,1], 2)

  expect_identical(scalar_res, cscalar_res)
  expect_identical(cscalar_res, 6)
  rm(ans, cans); gc()
})

# cross scalar element type
test_that("as(): true scalar input (different element type)", {
  foo <- nFunction(
    function(x = 'numericScalar', scalar_res = numericVector()) {
      v <- as(x, 'integerMatrix')
      y <- x
      w <- v
      as(y, "integerMatrix") <- 3*w
      scalar_res[1] <- y
      return(v)
      returnType(integer(2))
    },
    refArgs = "scalar_res"
  )
  cfoo <- nCompile(foo)
  scalar_res <- 0
  ans <- foo(2, scalar_res)
  cscalar_res <- -1
  cans <- cfoo(2, cscalar_res)
  expect_identical(ans, cans)
  expect_identical(dim(cans), c(1L,1L))
  expect_identical(cans[1,1], 2L)

  expect_identical(scalar_res, cscalar_res)
  expect_identical(cscalar_res, 6)
  rm(ans, cans); gc()
})


# Inputs are NOT true scalars, target type is true scalar
# same type
test_that("as(): true scalar target type (same element type)", {
  foo <- nFunction(
    function(x = 'numericMatrix', scalar_res = numericVector()) {
      v <- as(x, 'numericScalar')
      y <- x
      w <- v
      as(y, "numericScalar") <- 3*w
      scalar_res[1] <- y[1,1]
      return(v)
      returnType(double())
    },
    refArgs = "scalar_res"
  )
  cfoo <- nCompile(foo)
  x <- matrix(2, nrow = 1, ncol = 1)
  scalar_res <- 0
  ans <- foo(x, scalar_res)
  cscalar_res <- -1
  cans <- cfoo(x, cscalar_res)
  expect_identical(ans, cans)
  expect_true(is.null(dim(cans)))
  expect_equal(cans, 2)
  expect_identical(scalar_res, cscalar_res)
  expect_identical(cscalar_res, 6)
  rm(ans, cans); gc()
})

# cross type
test_that("as(): true scalar target type (same element type)", {
  foo <- nFunction(
    function(x = 'integerMatrix', scalar_res = numericVector()) {
      v <- as(x, 'numericScalar')
      y <- x
      w <- v
      as(y, "numericScalar") <- 3*w
      scalar_res[1] <- y[1,1]
      return(v)
      returnType(double())
    },
    refArgs = "scalar_res"
  )
  cfoo <- nCompile(foo)
  x <- matrix(2, nrow = 1, ncol = 1)
  scalar_res <- 0
  ans <- foo(x, scalar_res)
  cscalar_res <- -1
  cans <- cfoo(x, cscalar_res)
  expect_identical(ans, cans)
  expect_true(is.null(dim(cans)))
  expect_equal(cans, 2)
  expect_identical(scalar_res, cscalar_res)
  expect_identical(cscalar_res, 6)
  rm(ans, cans); gc()
})

# Inputs are true scalars and output is also a true scalar
# same type
test_that("as(): true scalar input and  target type (same element type)", {
  foo <- nFunction(
    function(x = 'numericScalar', scalar_res = numericVector()) {
      v <- as(x, 'numericScalar')
      y <- x
      w <- v
      as(y, "numericScalar") <- 3*w
      scalar_res[1] <- y
      return(v)
      returnType(double())
    },
    refArgs = "scalar_res"
  )
  cfoo <- nCompile(foo)
  x <- 2.3
  scalar_res <- 1
  ans <- foo(x, scalar_res)
  cscalar_res <- 0
  cans <- cfoo(x, cscalar_res)
  expect_identical(ans, cans)
  expect_true(is.null(dim(cans)))
  expect_equal(cans, 2.3)
  expect_identical(scalar_res, cscalar_res)
  expect_identical(cscalar_res, 3*2.3)
  rm(ans, cans); gc()
})

# cross type
test_that("as(): true scalar input and  target type (different element type)", {
  foo <- nFunction(
    function(x = 'numericScalar', scalar_res = numericVector()) {
      v <- as(x, 'integerScalar')
      y <- x
      w <- v
      as(y, "integerScalar") <- 3*w
      scalar_res[1] <- y
      return(v)
      returnType(integer())
    },
    refArgs = "scalar_res"
  )
  cfoo <- nCompile(foo)
  x <- 2.3
  scalar_res <- 1
  ans <- foo(x, scalar_res)
  cscalar_res <- 0
  cans <- cfoo(x, cscalar_res)
  expect_identical(ans, cans)
  expect_true(is.null(dim(cans)))
  expect_equal(cans, 2L)
  expect_identical(scalar_res, cscalar_res)
  expect_identical(cscalar_res, 3*2L)
  rm(ans, cans); gc()
})

# ---------------------------------------------------------------------------
# Runtime-source path via ETaccessorBase (RuntimeCastingProxy)
#
# obj->access("varname") returns unique_ptr<ETaccessorBase>.
# as_nC<T,N>(*acc) constructs a RuntimeCastingProxy that views source data
# directly when scalars match, or makes a cast copy otherwise (writing back
# on destruction when LHS mode).
#
# Tests use cppLiteral with a two-nClass pattern: ncAcc holds the data member
# x, and ncOps creates a local ncAcc object inside each method so that
# data->access("x") is valid C++ (avoids the self->access() path that requires
# the self-keyword PR not yet merged).  Compiled modes only.
# ---------------------------------------------------------------------------

test_that("as(): ETaccessorBase RHS paths (same-scalar, cross-scalar sum and element)", {
  ncAcc <- nClass(
    Cpublic = list(x = 'numericVector')
  )
  ncOps <- nClass(
    Cpublic = list(
      # same scalar: no copy, direct pointer into x
      rhs_same_sum = nFunction(
        function(v = numericVector) {
          data <- ncAcc$new()
          data$x <- v
          ans <- 0.0
          cppLiteral('{ auto _acc = data->access("x"); flex_(ans) = as_nC<double,1>(*_acc)().sum(); }')
          return(ans)
          returnType(numericScalar)
        }
      ),
      # cross scalar sum: cast-copy double→int, sum truncated values
      rhs_xscalar_sum = nFunction(
        function(v = numericVector) {
          data <- ncAcc$new()
          data$x <- v
          ans <- 0L
          cppLiteral('{ auto _acc = data->access("x"); flex_(ans) = as_nC<int,1>(*_acc)().sum(); }')
          return(ans)
          returnType(integerScalar)
        }
      ),
      # cross scalar element: operator()() then (i) on the proxy
      rhs_xscalar_elem = nFunction(
        function(v = numericVector, i = integerScalar) {
          data <- ncAcc$new()
          data$x <- v
          ans <- 0L
          cppLiteral('{ auto _acc = data->access("x"); flex_(ans) = as_nC<int,1>(*_acc)()(i - 1); }')
          return(ans)
          returnType(integerScalar)
        }
      )
    )
  )
  for(mode in c("non_pkg", "pkg")) {
    package <- mode == "pkg"
    comp <- nCompile(ncAcc, ncOps, package = package)
    nco <- comp$ncOps$new()

    # same scalar: 1+2+3+4 = 10
    expect_equal(nco$rhs_same_sum(c(1.0, 2.0, 3.0, 4.0)), 10.0)

    # cross scalar sum: 1.9→1, 2.1→2, 3.7→3 → sum = 6
    expect_equal(nco$rhs_xscalar_sum(c(1.9, 2.1, 3.7)), 6L)

    # cross scalar element: x[2] = 20.1 → truncates to 20
    expect_equal(nco$rhs_xscalar_elem(c(10.9, 20.1, 30.7), 2L), 20L)

    rm(nco); gc()
  }
})

test_that("as(): ETaccessorBase LHS paths (same-scalar write-through, cross-scalar write-back)", {
  ncAcc <- nClass(
    Cpublic = list(x = 'numericVector')
  )
  ncOps <- nClass(
    Cpublic = list(
      # same scalar LHS: direct pointer write, no destructor copy, verify via second accessor
      lhs_same = nFunction(
        function(v = numericVector) {
          data <- ncAcc$new()
          data$x <- numeric(length = length(v), value = 0)
          cppLiteral('{ auto _acc = data->access("x"); as_nC<double,1,AsMode::LHS>(*_acc)() = v; }')
          ans <- 0.0
          cppLiteral('{ auto _acc2 = data->access("x"); flex_(ans) = as_nC<double,1>(*_acc2)().sum(); }')
          return(ans)
          returnType(numericScalar)
        }
      ),
      # cross scalar LHS: int proxy writes int values into double storage on destruction
      lhs_xscalar = nFunction(
        function(v = integerVector) {
          data <- ncAcc$new()
          data$x <- numeric(length = length(v), value = 0)
          cppLiteral('{ auto _acc = data->access("x"); as_nC<int,1,AsMode::LHS>(*_acc)() = v; }')
          ans <- 0.0
          cppLiteral('{ auto _acc2 = data->access("x"); flex_(ans) = as_nC<double,1>(*_acc2)().sum(); }')
          return(ans)
          returnType(numericScalar)
        }
      )
    )
  )
  for(mode in c("non_pkg", "pkg")) {
    package <- mode == "pkg"
    comp <- nCompile(ncAcc, ncOps, package = package)
    nco <- comp$ncOps$new()

    # same scalar: write [10,20,30], read sum = 60
    expect_equal(nco$lhs_same(c(10.0, 20.0, 30.0)), 60.0)

    # cross scalar: write int [10,20,30] into double x, read sum = 60
    expect_equal(nco$lhs_xscalar(c(10L, 20L, 30L)), 60.0)

    rm(nco); gc()
  }
})

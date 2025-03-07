# These tests work.
# Note that test-RcppTypes also uses cppLiteral,
# so this is largely redundant with that.
test_that("cppLiteral with list of scalars works", {
  nf <- nFunction(
    fun = function(x = double(0),
                   y = logical(0)) {
      a <- x + 1
      b <- y == TRUE
      cppLiteral(
        'ans = Rcpp::List::create(
  Rcpp::Named("item1") = a,
  Rcpp::Named("item2") = b
);',
types = list(ans = "RcppList")
)
      return(ans)
      returnType("RcppList")
    }
)
  nfC <- nCompile(nf)
  ans <- nfC(2, FALSE)
  expect_true(is.list(ans))
  expect_equal(length(ans), 2)
  expect_equal(ans$item1, 3)
  expect_equal(ans$item2, FALSE)
})

test_that("cppLiteral with list including vector works", {
  nf <- nFunction(
    fun = function(x = double(1),
                   y = double(0)) {
      z <- x + y
      cppLiteral(
        'ans = Rcpp::List::create(
  Rcpp::Named("x") = Rcpp::wrap(x),
  Rcpp::Named("y") = y,
  Rcpp::Named("z") = Rcpp::wrap(z)
);',
types = list(ans = "RcppList")
)
      return(ans)
      returnType("RcppList")
    }
)
  nfC <- nCompile(nf)
  ans <- nfC(c(1,2,3), 4)
  expect_true(is.list(ans))
  expect_equal(length(ans), 3)
  expect_equal(ans$x, c(1,2,3))
  expect_equal(ans$y, 4)
  expect_equal(ans$z, c(5,6,7))
})

##########
## Beginning work on a generalized concept
## of nCpp to replace or extent cppLiteral

test_that("nCpp works with vector of text", {
  nf <- nFunction(
    fun = function() {
      nCpp(c("int x = 0;",
             "Rprintf(\"hw %i\\n \", x)"))
    }
  )
  nfC <- nCompile(nf)
  expect_identical(capture.output(nfC())[1], "hw 0")
})

test_that("nCpp works with evaluation in correct environment", {
  make_nf <- function() {
    x_value <- 10
    nf <- nFunction(
      fun = function() {
        nCpp(c("int x = ", x_value, ";",
               "Rprintf(\"hw %i\\n \", x)"))
      }
    )
  }
  nf <- make_nf()
  nfC <- nCompile(nf)
  expect_identical(capture.output(nfC())[1], "hw 0")
})

test_that("nCpp works within a line", {
  make_nf <- function() {
    nf <- nFunction(
      fun = function(ivec = nCpp('Eigen::Tensor<int, 1>')) {
        nCpp("return(ivec);")
        returnType('integerVector')
      }
    )
  }
  nf <- make_nf()
  nfC <- nCompile(nf)
  expect_identical(nfC(1:3), 1:3)
})

test_that("types as objects works", {
  make_nf <- function() {
    my_type <- nMakeType(integerVector())
    nf <- nFunction(
      fun = function(ivec = T(my_type)) {
        nCpp("x = ivec+1;", types = list(x = quote(T(my_type))))
        return(x+1L);
        returnType(T(my_type))
      }
    )
  }
  nf <- make_nf()
  nfC <- nCompile(nf)
  expect_identical(nfC(1:3), 3:5)
})

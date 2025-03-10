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
  expect_identical(capture.output(nfC())[1], "hw 10")
})

## Closure of an R6 methods is replaced with the nClass's parent_env
## This is how R6 works and so also nClass
test_that("nCpp works with evaluation in correct environment in nClass", {
  make_nf <- function() {
    x_value <- 10 # will be ignored because there will be a new parent env
    nf <- nFunction(
      fun = function() {
        nCpp(c("int x = ", x_value, ";",
               "Rprintf(\"hw %i\\n \", x)"))
      }
    )
  }
  nf <- make_nf()
  myenv <- new.env()
  myenv$x_value <- 101
  nc <- nClass(
    Cpublic = list(nf = nf),
    env = myenv
  )
  ncC <- nCompile(nc)
  obj <- ncC$new()
  obj$nf()
  expect_identical(capture.output(obj$nf())[1], "hw 101")
})

test_that("nCpp works with evaluation in correct environment", {
  make_nc <- function() {
    x_value <- 10
    nc <- nClass(
      Cpublic = list(
        nf = nFunction(
          fun = function() {
            nCpp(c("int x = ", x_value, ";",
                   "Rprintf(\"hw %i\\n \", x)"))
          }
        )
      ))
    nc
  }
  nc <- make_nc()
  ncC <- nCompile(nc)
  obj <- ncC$new()
  expect_identical(capture.output(obj$nf())[1], "hw 10")
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

test_that("types as objects work with an nClass", {
  make_nf <- function() {
    ## It looks like arg and return types use the closure because
    # they are determined at definition
    # but nCpp uses the nClass parent_env because it is processed later.
    my_type <- nMakeType(integerVector()) # to be ignored because method closure is replaced with parent_env
    nf <- nFunction(
      fun = function(ivec = T(my_type)) {
        nCpp("x = ivec.cast<double>()+1.2;", types = list(x = quote(T(my_type))))
        return(ivec+1L);
        returnType(T(my_type))
      }
    )
  }
  nf <- make_nf()
  myenv <- new.env()
  myenv$my_type <- nMakeType(numericVector())
  nc <- nClass(
    Cpublic = list(
      cust = quote(nCpp('double')),
      v = quote(T(my_type)),
      nf = nf),
    env = myenv
  )
  ncC <- nCompile(nc)
  obj <- ncC$new()
  obj$nf(1:3)
  expect_identical(obj$nf(1:3), 2:4)
})

test_that("manual C++ pieces in nFunction work", {
  foo <- nFunction(
    name = "foo",
    function(x = numericScalar()) { # manually to become vector
      nCpp("return x[0];")
      returnType('numericVector') # manually to become scalar
    },
    compileInfo = list(
      prototype = "double foo(Eigen::Tensor<double, 1> y)",
      deftype = "double foo(Eigen::Tensor<double, 1> x)"
    )
  )
  cppDefs <- nCompile(foo, control = list(return_cppDefs = TRUE))
  decl <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=TRUE)))
  def <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=FALSE)))
  expect_true(grepl("^double foo\\(Eigen::Tensor<double, 1> y\\);$", decl))
  expect_true(grepl("^double foo\\(Eigen::Tensor<double, 1> x\\)", def[2]))
  #  cfoo <- nCompile(foo) # these should work but we're avoiding full compilation for speed
  #  expect_identical(cfoo(1:3), 1)

  foo <- nFunction(
    name = "foo",
    function(x = integerVector()) { # replace with numericVector
      nCpp("return x[0];")
      returnType('integerScalar') # replace with numericScalar
    },
    compileInfo = list(
      name = "myfoo",
      cpp_code_name = "myfoo2", # not used because name over-rides it
      scopes = c("s1", "s2"),
      qualifiers = c("const -> double"),
      args = "(Eigen::Tensor<double, 1> x, double y)",
      returnType = "double"
    )
  )
  cppDefs <- nCompile(foo, control = list(return_cppDefs = TRUE))
  decl <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=TRUE)))
  def <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=FALSE)))
  expect_true(grepl("double s1::s2::myfoo \\(Eigen::Tensor<double, 1> x, double y\\) const -> double", decl))
  expect_true(grepl("double s1::s2::myfoo \\(Eigen::Tensor<double, 1> x, double y\\) const -> double", def[2]))
  ##
  foo <- nFunction(
    name = "foo",
    function() { # replace with numericVector
      nCpp("return x[0];")
    },
    compileInfo = list(
      cpp_code_name = "myfoo2",
      args = "(Eigen::Tensor<TYPE, 1> x)",
      returnType = "double",
      template = "template<typename TYPE>",
      callFromR = FALSE
    )
  )
  cppDefs <- nCompile(foo, control = list(return_cppDefs = TRUE))
  decl <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=TRUE)))
  def <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=FALSE)))
  expect_true(grepl("template<typename TYPE>", decl[1]))
  expect_true(grepl("double myfoo2 \\(Eigen::Tensor<TYPE, 1> x\\)", decl[2]))
  expect_true(grepl("template<typename TYPE>", def[1]))
  expect_true(grepl("double myfoo2 \\(Eigen::Tensor<TYPE, 1> x\\)", def[2]))
  ##
  foo <- nFunction(
    name = "foo",
    function() { },
    compileInfo = list(
      cpp_code_name = "myfoo2",
      args = "(Eigen::Tensor<TYPE, 1> x)",
      returnType = "double",
      template = "template<typename TYPE>",
      callFromR = FALSE
    )
  )

})

# Tests of exprClass_put_args_in_order and
# exprClass_match_call.
#
# These are used from compile_normalizeCalls
# using the matchDef element of the opDef.

library(nCompiler)
library(testthat)

# PAIO = put arguments in order

PAIOtest <- function(input, def, expected, compileArgs = NULL, auxExpected) {
  inputcode <- nParse(input)
  expecterror = expected=="error"
  result <- if(expecterror) {
    nCompiler:::exprClass_put_args_in_order(def = def, expr = inputcode, compileArgs = compileArgs) |> expect_error()
    return(invisible(NULL))
  } else
    nCompiler:::exprClass_put_args_in_order(def = def, expr = inputcode, compileArgs = compileArgs)
  outputcode <- nDeparse(result)
  expect_identical(outputcode, expected)
  if(!missing(auxExpected)) {
    if(!identical(inputcode$aux, auxExpected))
      print(list(inputcode_aux = inputcode$aux, auxExpected = auxExpected))
    expect_identical(inputcode$aux, auxExpected)
  }
  invisible(NULL)
}

test_that("PAIO test tool works", {
  PAIOtest("foo(A = a)", \(A=A){}, "foo(A = a)")
})

test_that("nonary defs", {
  cat("Two caught errors expected:\n")
  PAIOtest("foo()", \(){}, "foo()")
  PAIOtest("foo(1)", \(){}, "error")
  PAIOtest("foo(A = 1)", \(){}, "error")
})

test_that("unary defs", {
  cat("Two caught errors expected:\n")
  # with default
  auxExpectedNoA <- list(provided_as_missing = "A", missing = character(0), compileArgs=list())
  auxExpected <- list(provided_as_missing = character(0), missing = character(0), compileArgs =list())
  PAIOtest("foo()", \(A = a){}, "foo(A = a)", auxExpected = auxExpectedNoA)
  PAIOtest("foo(1)", \(A = a){}, "foo(A = 1)", auxExpected = auxExpected)
  PAIOtest("foo(A = a2)", \(A = a){}, "foo(A = a2)", auxExpected = auxExpected)
  PAIOtest("foo(B = b)", \(A = a){}, "error")
  # without default
  auxExpectedNoA <- list(provided_as_missing = "A", missing = "A",compileArgs=list())
  PAIOtest("foo()", \(A){}, "foo()", auxExpected = auxExpectedNoA)
  PAIOtest("foo(a)", \(A){}, "foo(A = a)", auxExpected = auxExpected)
  PAIOtest("foo(A = a)", \(A){}, "foo(A = a)", auxExpected = auxExpected)
  PAIOtest("foo(B = b)", \(A){}, "error")
})

test_that("unary compileArg", {
  cat("Two caught errors expected:\n")
  # with default
  auxExpectedNoA = list(provided_as_missing = "A", missing = character(0),
                      compileArgs = list(A = quote(a)))
  auxExpected = list(provided_as_missing = character(0), missing = character(0),
                        compileArgs = list(A = quote(a)))
  PAIOtest("foo()", \(A = a){}, "foo()", compileArgs="A", auxExpected = auxExpectedNoA)
  PAIOtest("foo(a)", \(A = a2){}, "foo()", compileArgs="A", auxExpected = auxExpected)
  PAIOtest("foo(A=a)", \(A = a2){}, "foo()", compileArgs="A", auxExpected = auxExpected)
  PAIOtest("foo(B=b)", \(A = a2){}, "error")
  # without default
  auxExpectedNoA = list(provided_as_missing = "A", missing = "A",
                        compileArgs = list())
  PAIOtest("foo()", \(A){}, "foo()", compileArgs="A", auxExpected = auxExpectedNoA)
  PAIOtest("foo(a)", \(A){}, "foo()", compileArgs="A", auxExpected = auxExpected)
  PAIOtest("foo(A=a)", \(A){}, "foo()", compileArgs="A", auxExpected = auxExpected)
  PAIOtest("foo(B=b)", \(A){}, "error")
})

test_that("binary def", {
  # with both defaults
  auxExpectedNoAB <- list(provided_as_missing = c("A", "B"), missing = character(0), compileArgs=list())
  auxExpectedNoB <- list(provided_as_missing = c("B"), missing = character(0), compileArgs=list())
  auxExpectedNoA <- list(provided_as_missing = c("A"), missing = character(0), compileArgs=list())
  auxExpected <- list(provided_as_missing = character(0), missing = character(0), compileArgs =list())
  PAIOtest("foo()", \(A = a, B = b){}, "foo(A = a, B = b)", auxExpected = auxExpectedNoAB)
  PAIOtest("foo(a2)", \(A = a, B = b){}, "foo(A = a2, B = b)", auxExpected = auxExpectedNoB)
  PAIOtest("foo(A = a2)", \(A = a, B = b){}, "foo(A = a2, B = b)", auxExpected = auxExpectedNoB)
  PAIOtest("foo(B = b2)", \(A = a, B = b){}, "foo(A = a, B = b2)", auxExpected = auxExpectedNoA)
  PAIOtest("foo(A = a2, B = b2)", \(A = a, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(A = a2, b2)", \(A = a, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(a2, B = b2)", \(A = a, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(a2, b2)", \(A = a, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(B = b2, A = a2)", \(A = a, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(B = b2, a2)", \(A = a, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  # without A default
  auxExpectedNoAB <- list(provided_as_missing = c("A", "B"), missing = "A", compileArgs=list())
  auxExpectedNoB <- list(provided_as_missing = c("B"), missing = character(0), compileArgs=list())
  auxExpectedNoA <- list(provided_as_missing = c("A"), missing = "A", compileArgs=list())
  auxExpected <- list(provided_as_missing = character(0), missing = character(0), compileArgs =list())
  PAIOtest("foo()", \(A, B = b){}, "foo(B = b)", auxExpected = auxExpectedNoAB)
  PAIOtest("foo(a2)", \(A, B = b){}, "foo(A = a2, B = b)", auxExpected = auxExpectedNoB)
  PAIOtest("foo(A = a2)", \(A, B = b){}, "foo(A = a2, B = b)", auxExpected = auxExpectedNoB)
  PAIOtest("foo(B = b2)", \(A, B = b){}, "foo(B = b2)", auxExpected = auxExpectedNoA)
  PAIOtest("foo(A = a2, B = b2)", \(A, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(A = a2, b2)", \(A, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(a2, B = b2)", \(A, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(a2, b2)", \(A, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(B = b2, A = a2)", \(A, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(B = b2, a2)", \(A, B = b){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  # without B default
  auxExpectedNoAB <- list(provided_as_missing = c("A", "B"), missing = "B", compileArgs=list())
  auxExpectedNoB <- list(provided_as_missing = c("B"), missing = "B", compileArgs=list())
  auxExpectedNoA <- list(provided_as_missing = c("A"), missing = character(), compileArgs=list())
  auxExpected <- list(provided_as_missing = character(0), missing = character(0), compileArgs =list())
  PAIOtest("foo()", \(A = a, B){}, "foo(A = a)", auxExpected = auxExpectedNoAB)
  PAIOtest("foo(a2)", \(A = a, B){}, "foo(A = a2)", auxExpected = auxExpectedNoB)
  PAIOtest("foo(A = a2)", \(A = a, B){}, "foo(A = a2)", auxExpected = auxExpectedNoB)
  PAIOtest("foo(B = b2)", \(A = a, B){}, "foo(A = a, B = b2)", auxExpected = auxExpectedNoA)
  PAIOtest("foo(A = a2, B = b2)", \(A = a, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(A = a2, b2)", \(A = a, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(a2, B = b2)", \(A = a, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(a2, b2)", \(A = a, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(B = b2, A = a2)", \(A = a, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(B = b2, a2)", \(A = a, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  # with no defaults
  auxExpectedNoAB <- list(provided_as_missing = c("A", "B"), missing = c("A", "B"), compileArgs=list())
  auxExpectedNoB <- list(provided_as_missing = c("B"), missing = "B", compileArgs=list())
  auxExpectedNoA <- list(provided_as_missing = c("A"), missing = "A", compileArgs=list())
  auxExpected <- list(provided_as_missing = character(0), missing = character(0), compileArgs =list())
  PAIOtest("foo()", \(A, B){}, "foo()", auxExpected = auxExpectedNoAB)
  PAIOtest("foo(a2)", \(A, B){}, "foo(A = a2)", auxExpected = auxExpectedNoB)
  PAIOtest("foo(A = a2)", \(A, B){}, "foo(A = a2)", auxExpected = auxExpectedNoB)
  PAIOtest("foo(B = b2)", \(A, B){}, "foo(B = b2)", auxExpected = auxExpectedNoA)
  PAIOtest("foo(A = a2, B = b2)", \(A, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(A = a2, b2)", \(A, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(a2, B = b2)", \(A, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(a2, b2)", \(A, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(B = b2, A = a2)", \(A, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
  PAIOtest("foo(B = b2, a2)", \(A, B){}, "foo(A = a2, B = b2)", auxExpected = auxExpected)
})

test_that("binary def with one compileArg", {
  # with both defaults
  auxExpectedNoAB <- list(provided_as_missing = c("A", "B"), missing = character(0), compileArgs=list(A = quote(a)))
  auxExpectedNoB <- list(provided_as_missing = c("B"), missing = character(0), compileArgs=list(A = quote(a)))
  auxExpectedNoA <- list(provided_as_missing = c("A"), missing = character(0), compileArgs=list(A = quote(a)))
  auxExpected <- list(provided_as_missing = character(0), missing = character(0), compileArgs =list(A = quote(a)))
  PAIOtest("foo()", \(A = a, B = b){}, "foo(B = b)", compileArgs="A", auxExpected = auxExpectedNoAB)
  PAIOtest("foo(a)", \(A = a2, B = b){}, "foo(B = b)", compileArgs="A", auxExpected = auxExpectedNoB)
  PAIOtest("foo(A = a)", \(A = a2, B = b){}, "foo(B = b)", compileArgs="A", auxExpected = auxExpectedNoB)
  PAIOtest("foo(B = b2)", \(A = a, B = b){}, "foo(B = b2)", compileArgs="A", auxExpected = auxExpectedNoA)
  PAIOtest("foo(A = a, B = b2)", \(A = a2, B = b){}, "foo(B = b2)", compileArgs="A", auxExpected = auxExpected)
  PAIOtest("foo(A = a, b2)", \(A = a2, B = b){}, "foo(B = b2)", compileArgs="A", auxExpected = auxExpected)
  PAIOtest("foo(a, B = b2)", \(A = a2, B = b){}, "foo(B = b2)", compileArgs="A", auxExpected = auxExpected)
  PAIOtest("foo(a, b2)", \(A = a2, B = b){}, "foo(B = b2)", compileArgs="A", auxExpected = auxExpected)
  PAIOtest("foo(B = b2, A = a)", \(A = a2, B = b){}, "foo(B = b2)", compileArgs="A", auxExpected = auxExpected)
  PAIOtest("foo(B = b2, a)", \(A = a2, B = b){}, "foo(B = b2)", compileArgs="A", auxExpected = auxExpected)
  # skip full permutations of defaults
})

test_that("nonary def except for dots", {
  PAIOtest("foo()", \(...){}, "foo()")
  PAIOtest("foo(1)", \(...){}, "foo(1)")
  PAIOtest("foo(1, 2)", \(...){}, "foo(1, 2)")
  PAIOtest("foo(A = 1)", \(...){}, "foo(A = 1)")
  PAIOtest("foo(A = 1, 2)", \(...){}, "foo(A = 1, 2)")
  PAIOtest("foo(1, B = 2)", \(...){}, "foo(1, B = 2)")
  PAIOtest("foo(A = 1, B = 2)", \(...){}, "foo(A = 1, B = 2)")
})

test_that("nonary def except for dots with compileArg", {
  # We get a "..." in the missing entries if nothing was provided.
  # Is that useful? For now it is considered a feature, not a bug.
  # Empty arguments:
  auxExpectedNoA=list(provided_as_missing="...", missing=character(), compileArgs=list())
  PAIOtest("foo()", \(...){}, "foo()", compileArgs="A", auxExpected=auxExpectedNoA)
  # Not empty arguments:
  auxExpectedNoA=list(provided_as_missing=character(), missing=character(), compileArgs=list())
  auxExpected=list(provided_as_missing=character(), missing=character(), compileArgs=list(A = quote(a)))
  PAIOtest("foo(1)", \(...){}, "foo(1)", compileArgs="A", auxExpected=auxExpectedNoA)
  PAIOtest("foo(1, 2)", \(...){}, "foo(1, 2)", compileArgs="A", auxExpected=auxExpectedNoA)
  PAIOtest("foo(A = a)", \(...){}, "foo()", compileArgs="A", auxExpected=auxExpected)
  PAIOtest("foo(A = a, 2)", \(...){}, "foo(2)", compileArgs="A", auxExpected=auxExpected)
  PAIOtest("foo(a, B = 2)", \(...){}, "foo(a, B = 2)", compileArgs="A", auxExpected=auxExpectedNoA)
  PAIOtest("foo(A = a, B = 2)", \(...){}, "foo(B = 2)", compileArgs="A", auxExpected=auxExpected)
  PAIOtest("foo(B = 2, A = a)", \(...){}, "foo(B = 2)", compileArgs="A", auxExpected=auxExpected)
})

test_that("binary def with dots at the end", {
  # We get a "..." in the missing entries if nothing was provided.
  # Is that useful? For now it is considered a feature, not a bug.
  # Empty arguments:
  auxExpectedNoAB=list(provided_as_missing=c("A","B","..."), missing=character(), compileArgs=list())
  PAIOtest("foo()", \(A = a, B = b, ...){}, "foo(A = a, B = b)", auxExpected=auxExpectedNoAB)
  # Not empty arguments:
  # A and B both have defaults
  auxExpectedNoAB=list(provided_as_missing=c("A", "B"), missing=character(), compileArgs=list())
  auxExpectedNoA=list(provided_as_missing="A", missing=character(), compileArgs=list())
  auxExpectedNoB=list(provided_as_missing="B", missing=character(), compileArgs=list())
  auxExpected=list(provided_as_missing=character(), missing=character(), compileArgs=list())
  PAIOtest("foo(C = c, D = d)", \(A = a, B = b, ...){}, "foo(A = a, B = b, C = c, D = d)", auxExpected=auxExpectedNoAB)
  PAIOtest("foo(1, C = c, D = d)", \(A = a, B = b, ...){}, "foo(A = 1, B = b, C = c, D = d)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(A = 1, C = c, D = d)", \(A = a, B = b, ...){}, "foo(A = 1, B = b, C = c, D = d)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, D = d, A = 1)", \(A = a, B = b, ...){}, "foo(A = 1, B = b, C = c, D = d)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, A = 1, D = d)", \(A = a, B = b, ...){}, "foo(A = 1, B = b, C = c, D = d)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, 1, D = d)", \(A = a, B = b, ...){}, "foo(A = 1, B = b, C = c, D = d)", auxExpected=auxExpectedNoB)

  PAIOtest("foo(1, 2, C = c)", \(A = a, B = b, ...){}, "foo(A = 1, B = 2, C = c)", auxExpected=auxExpected)
  PAIOtest("foo(1, 2, C = c, 3)", \(A = a, B = b, ...){}, "foo(A = 1, B = 2, C = c, 3)", auxExpected=auxExpected)
  PAIOtest("foo(C = c, 1, D = d, 2)", \(A = a, B = b, ...){}, "foo(A = 1, B = 2, C = c, D = d)", auxExpected=auxExpected)
  PAIOtest("foo(C = c, B = 2, D = d, A = 1)", \(A = a, B = b, ...){}, "foo(A = 1, B = 2, C = c, D = d)", auxExpected=auxExpected)
  PAIOtest("foo(C = c, B = 2, D = d, 1)", \(A = a, B = b, ...){}, "foo(A = 1, B = 2, C = c, D = d)", auxExpected=auxExpected)

  auxExpectedNoB=list(provided_as_missing=c("B","..."), missing=character(), compileArgs=list())
  auxExpected=list(provided_as_missing="...", missing=character(), compileArgs=list())
  PAIOtest("foo(A = a)", \(A = a, B = b, ...){}, "foo(A = a, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(A = a, 2)", \(A = a, B = b, ...){}, "foo(A = a, B = 2)", auxExpected=auxExpected)
  PAIOtest("foo(a, B = 2)", \(A = a, B = b, ...){}, "foo(A = a, B = 2)", auxExpected=auxExpected)
  PAIOtest("foo(A = a, B = 2)", \(A = a, B = b, ...){}, "foo(A = a, B = 2)", auxExpected=auxExpected)
  PAIOtest("foo(B = 2, A = a)", \(A = a, B = b, ...){}, "foo(A = a, B = 2)", auxExpected=auxExpected)

  # No default for A
  #auxExpectedNoAB=list(provided_as_missing=c("A", "B"), missing=character(), compileArgs=list())
  auxExpectedNoA=list(provided_as_missing=c("A", "B"), missing="A", compileArgs=list())
  auxExpectedNoB=list(provided_as_missing="B", missing=character(), compileArgs=list())
  auxExpected=list(provided_as_missing=character(), missing=character(), compileArgs=list())
  PAIOtest("foo(C = c, D = d)", \(A, B = b, ...){}, "foo(B = b, C = c, D = d)", auxExpected=auxExpectedNoA)
  PAIOtest("foo(1, C = c, D = d)", \(A, B = b, ...){}, "foo(A = 1, B = b, C = c, D = d)", auxExpected=auxExpectedNoB)

  PAIOtest("foo(A = 1, C = c, D = d)", \(A, B = b, ...){}, "foo(A = 1, B = b, C = c, D = d)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, D = d, A = 1)", \(A, B = b, ...){}, "foo(A = 1, B = b, C = c, D = d)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, A = 1, D = d)", \(A, B = b, ...){}, "foo(A = 1, B = b, C = c, D = d)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, 1, D = d)", \(A, B = b, ...){}, "foo(A = 1, B = b, C = c, D = d)", auxExpected=auxExpectedNoB)

  PAIOtest("foo(1, 2, C = c)", \(A, B = b, ...){}, "foo(A = 1, B = 2, C = c)", auxExpected=auxExpected)
  PAIOtest("foo(1, 2, C = c, 3)", \(A, B = b, ...){}, "foo(A = 1, B = 2, C = c, 3)", auxExpected=auxExpected)
  PAIOtest("foo(C = c, 1, D = d, 2)", \(A, B = b, ...){}, "foo(A = 1, B = 2, C = c, D = d)", auxExpected=auxExpected)
  PAIOtest("foo(C = c, B = 2, D = d, A = 1)", \(A, B = b, ...){}, "foo(A = 1, B = 2, C = c, D = d)", auxExpected=auxExpected)
  PAIOtest("foo(C = c, B = 2, D = d, 1)", \(A, B = b, ...){}, "foo(A = 1, B = 2, C = c, D = d)", auxExpected=auxExpected)
})

test_that("binary def with dots in the middle", {
  # This case comes up for `[`(x, i, ..., drop) and `[<-`(x, i, ..., drop, value)
  #
  # We get a "..." in the missing entries if nothing was provided.
  # Is that useful? For now it is considered a feature, not a bug.
  # Empty arguments:
  auxExpectedNoAB=list(provided_as_missing=c("A","...", "B"), missing=character(), compileArgs=list())
  PAIOtest("foo()", \(A = a, ..., B = b){}, "foo(A = a, B = b)", auxExpected=auxExpectedNoAB)
  # Not empty arguments:
  # A and B both have defaults
  auxExpectedNoAB=list(provided_as_missing=c("A", "B"), missing=character(), compileArgs=list())
  auxExpectedNoA=list(provided_as_missing="A", missing=character(), compileArgs=list())
  auxExpectedNoB=list(provided_as_missing="B", missing=character(), compileArgs=list())
  auxExpected=list(provided_as_missing=character(), missing=character(), compileArgs=list())
  PAIOtest("foo(C = c, D = d)", \(A = a, ..., B = b){}, "foo(A = a, C = c, D = d, B = b)", auxExpected=auxExpectedNoAB)
  PAIOtest("foo(1, C = c, D = d)", \(A = a, ..., B = b){}, "foo(A = 1, C = c, D = d, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(A = 1, C = c, D = d)", \(A = a, ..., B = b){}, "foo(A = 1, C = c, D = d, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, D = d, A = 1)", \(A = a, ..., B = b){}, "foo(A = 1, C = c, D = d, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, A = 1, D = d)", \(A = a, ..., B = b){}, "foo(A = 1, C = c, D = d, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, 1, D = d)", \(A = a, ..., B = b){}, "foo(A = 1, C = c, D = d, B = b)", auxExpected=auxExpectedNoB)

  PAIOtest("foo(1, 2, C = c)", \(A = a, ..., B = b){}, "foo(A = 1, 2, C = c, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(1, 2, C = c, 3)", \(A = a, ..., B = b){}, "foo(A = 1, 2, C = c, 3, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, 1, D = d, 2)", \(A = a, ..., B = b){}, "foo(A = 1, C = c, D = d, 2, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, B = 2, D = d, A = 1)", \(A = a, ..., B = b){}, "foo(A = 1, C = c, D = d, B = 2)", auxExpected=auxExpected)
  PAIOtest("foo(C = c, B = 2, D = d, 1)", \(A = a, ..., B = b){}, "foo(A = 1, C = c, D = d, B = 2)", auxExpected=auxExpected)

  auxExpectedNoB=list(provided_as_missing=c("...", "B"), missing=character(), compileArgs=list())
  auxExpected=list(provided_as_missing="...", missing=character(), compileArgs=list())
  PAIOtest("foo(A = a)", \(A = a, ..., B = b){}, "foo(A = a, B = b)", auxExpected=auxExpectedNoB)
  auxExpectedNoB=list(provided_as_missing=c("B"), missing=character(), compileArgs=list())
  auxExpected=list(provided_as_missing="...", missing=character(), compileArgs=list())
  PAIOtest("foo(A = a, 2)", \(A = a, ..., B = b){}, "foo(A = a, 2, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(a, B = 2)", \(A = a, ..., B = b){}, "foo(A = a, B = 2)", auxExpected=auxExpected)
  PAIOtest("foo(A = a, B = 2)", \(A = a, ..., B = b){}, "foo(A = a, B = 2)", auxExpected=auxExpected)
  PAIOtest("foo(B = 2, A = a)", \(A = a, ..., B = b){}, "foo(A = a, B = 2)", auxExpected=auxExpected)

  # No default for A
  auxExpectedNoAB=list(provided_as_missing=c("A", "B"), missing="A", compileArgs=list())
  auxExpectedNoA=list(provided_as_missing=c("A"), missing="A", compileArgs=list())
  auxExpectedNoB=list(provided_as_missing="B", missing=character(), compileArgs=list())
  auxExpected=list(provided_as_missing=character(), missing=character(), compileArgs=list())

  PAIOtest("foo(C = c, D = d)", \(A, ..., B = b){}, "foo(C = c, D = d, B = b)", auxExpected=auxExpectedNoAB)

  PAIOtest("foo(1, C = c, D = d)", \(A, ..., B = b){}, "foo(A = 1, C = c, D = d, B = b)", auxExpected=auxExpectedNoB)

  PAIOtest("foo(A = 1, C = c, D = d)", \(A, ..., B = b){}, "foo(A = 1, C = c, D = d, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, D = d, A = 1)", \(A, ..., B = b){}, "foo(A = 1, C = c, D = d, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, A = 1, D = d)", \(A, ..., B = b){}, "foo(A = 1, C = c, D = d, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, 1, D = d)", \(A, ..., B = b){}, "foo(A = 1, C = c, D = d, B = b)", auxExpected=auxExpectedNoB)

  PAIOtest("foo(1, 2, C = c)", \(A, ..., B = b){}, "foo(A = 1, 2, C = c, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(1, 2, C = c, 3)", \(A, ..., B = b){}, "foo(A = 1, 2, C = c, 3, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, 1, D = d, 2)", \(A, ..., B = b){}, "foo(A = 1, C = c, D = d, 2, B = b)", auxExpected=auxExpectedNoB)
  PAIOtest("foo(C = c, B = 2, D = d, A = 1)", \(A, ..., B = b){}, "foo(A = 1, C = c, D = d, B = 2)", auxExpected=auxExpected)
  PAIOtest("foo(C = c, B = 2, D = d, 1)", \(A, ..., B = b){}, "foo(A = 1, C = c, D = d, B = 2)", auxExpected=auxExpected)
})

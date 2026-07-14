library(testthat)

test_that("insertArg name updating works", {
  # Case 0: inserting an unnamed arg when other args are unnamed
  expr <- nParse("foo(a)")
  expect_equal(names(expr$args), NULL)
  expect_no_error(out <- capture_output(expr$print()))
  newArg <- nParse("x")
  nCompiler:::insertArg(expr, 2, newArg)
  expect_equal(names(expr$args), NULL)
  expect_no_error(out <- capture_output(expr$print()))
  # Case 1: inserting an unnamed arg when there are already named args
  expr <- nParse("foo(a = 1)")
  expect_equal(names(expr$args), "a")
  expect_no_error(out <- capture_output(expr$print()))
  newArg <- nParse("x")
  nCompiler:::insertArg(expr, 2, newArg)
  expect_equal(names(expr$args), c("a", ""))
  expect_no_error(out <- capture_output(expr$print()))
  # Case 2: inserting a named argument when others are unnamed
  # This is a regression test for a bug where inserting an argument with a name
  # when there were not other named arguments created an error
  # in exprClass_print
  expr <- nParse("foo(a)")
  expect_equal(names(expr$args), NULL)
  expect_no_error(out <- capture_output(expr$print()))
  newArg <- nParse("x")
  nCompiler:::insertArg(expr, 2, newArg, "b")
  expect_equal(names(expr$args), c("", "b"))
  expect_no_error(out <- capture_output(expr$print()))
})

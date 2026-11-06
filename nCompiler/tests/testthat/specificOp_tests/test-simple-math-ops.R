test_that("step, equals, pow and inprod work uncompiled and compiled", {
  nf_step <- nFunction(function(x = double()) { return(step(x)) }, returnType = 'integer')
  nf_equals <- nFunction(function(x = double(), y = double()) { return(equals(x, y)) }, returnType = 'integer')
  nf_pow <- nFunction(function(x = double(), y = double()) { return(pow(x, y)) }, returnType = 'double')
  nf_inprod <- nFunction(function(v = double(1), w = double(1)) { return(inprod(v, w)) }, returnType = 'double')
  nfs <- list(nf_step = nf_step, nf_equals = nf_equals, nf_pow = nf_pow, nf_inprod = nf_inprod)
  comp <- nCompile(nf_step, nf_equals, nf_pow, nf_inprod)

  for(nm in names(nfs)) {
    args <- switch(nm,
                   nf_step = list(list(-0.5), list(0), list(2)),
                   nf_equals = list(list(2, 2), list(2, 3)),
                   nf_pow = list(list(2.5, 3), list(4, 0.5)),
                   nf_inprod = list(list(c(1, 2, 3), c(4, 5, 6))))
    for(a in args) {
      expect_equal(do.call(nfs[[nm]], a), do.call(comp[[nm]], a))
    }
  }
  expect_identical(nf_step(-0.5), 0L)
  expect_identical(nf_step(0), 1L)
  expect_equal(nf_equals(2, 2), 1)
  expect_equal(nf_equals(2, 3), 0)
  expect_equal(nf_pow(2.5, 3), 15.625)
  expect_equal(nf_inprod(c(1, 2, 3), c(4, 5, 6)), 32)

  # As in nimble, equals supports only scalar arguments.
  nf_equals_vec <- nFunction(function(x = double(1)) { return(equals(x, x)) }, returnType = 'integer')
  expect_error(nCompile(nf_equals_vec), "scalar")
})

test_that("inverse works uncompiled and compiled", {
  set.seed(1)
  M <- crossprod(matrix(rnorm(16), 4)) + diag(4)
  Mi <- matrix(c(2L, 1L, 1L, 3L), 2)

  nf_inv <- nFunction(function(A = double(2)) { return(inverse(A)) }, returnType = 'double(2)')
  nf_inv_expr <- nFunction(function(A = double(2)) { return(inverse(A + t(A))) }, returnType = 'double(2)')
  nf_inv_int <- nFunction(function(A = integer(2)) { return(inverse(A)) }, returnType = 'double(2)')
  nf_inv_in_expr <- nFunction(function(A = double(2), v = double(1)) { return(inverse(A) %*% v) }, returnType = 'double(2)')
  comp <- nCompile(nf_inv, nf_inv_expr, nf_inv_int, nf_inv_in_expr)

  # Uncompiled uses solve() (LAPACK); compiled uses Eigen's inverse() as in nimble,
  # so results agree to numerical tolerance.
  expect_equal(nf_inv(M), solve(M))
  expect_equal(comp$nf_inv(M), solve(M))
  expect_equal(comp$nf_inv(M) %*% M, diag(4))
  expect_equal(comp$nf_inv_expr(M), solve(M + t(M)))
  expect_equal(nf_inv_expr(M), solve(M + t(M)))
  expect_equal(comp$nf_inv_int(Mi), solve(Mi))
  expect_equal(nf_inv_int(Mi), solve(Mi))
  expect_equal(comp$nf_inv_in_expr(M, 1:4), solve(M) %*% (1:4))
  expect_equal(nf_inv_in_expr(M, 1:4), solve(M) %*% (1:4))

  expect_error(comp$nf_inv(matrix(1, 2, 3)), "non-square")

  nf_inv_vec <- nFunction(function(x = double(1)) { return(inverse(x)) }, returnType = 'double(2)')
  expect_error(nCompile(nf_inv_vec), "matrix")
})

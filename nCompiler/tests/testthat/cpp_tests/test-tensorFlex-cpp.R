# Tests of the C++ flex_() assignment system (tensorFlex.h), written directly with cppLiteral.
# flex_(y) = x allows assignment between objects with different numbers of dimensions
# when their extents match except for extents of 1.

flexFuns <- list(
  ## Scalar Lhs
  scalar_from_int_literal = nFunction(
    function() {
      cppLiteral('flex_(ans) = 3;', types = list(ans = 'numericScalar'))
      cppLiteral('return ans;')
      returnType('numericScalar')
    }),
  scalar_from_reduction = nFunction(
    function(x = 'numericVector') {
      cppLiteral('flex_(ans) = x.sum();', types = list(ans = 'numericScalar'))
      cppLiteral('return ans;')
      returnType('numericScalar')
    }),
  scalar_from_vector = nFunction(
    function(x = 'numericVector') {
      cppLiteral('flex_(ans) = x;', types = list(ans = 'numericScalar'))
      cppLiteral('return ans;')
      returnType('numericScalar')
    }),
  scalar_from_matrix_op = nFunction(
    function(X = 'numericMatrix') {
      cppLiteral('flex_(ans) = X * 2.0;', types = list(ans = 'numericScalar'))
      cppLiteral('return ans;')
      returnType('numericScalar')
    }),
  int_from_vector = nFunction(
    function(x = 'numericVector') {
      cppLiteral('flex_(ans) = x;', types = list(ans = 'integerScalar'))
      cppLiteral('return ans;')
      returnType('integerScalar')
    }),
  bool_from_reduction = nFunction(
    function(x = 'numericVector') {
      cppLiteral('flex_(ans) = (x > 2.0).any();', types = list(ans = 'logicalScalar'))
      cppLiteral('return ans;')
      returnType('logicalScalar')
    }),
  long_from_vector = nFunction(
    function(x = 'numericVector') {
      cppLiteral('long L; flex_(L) = x; flex_(ans) = L;', types = list(ans = 'numericScalar'))
      cppLiteral('return ans;')
      returnType('numericScalar')
    }),
  ## Whole-object Lhs
  vector_from_matrix = nFunction(
    function(X = 'numericMatrix') {
      cppLiteral('flex_(y) = X;', types = list(y = 'numericVector'))
      cppLiteral('return y;')
      returnType('numericVector')
    }),
  vector_from_matrix_op = nFunction(
    function(X = 'numericMatrix') {
      cppLiteral('flex_(y) = X * 2.0;', types = list(y = 'numericVector'))
      cppLiteral('return y;')
      returnType('numericVector')
    }),
  vector_from_vector_cast = nFunction(
    function(x = 'integerVector') {
      cppLiteral('y = Eigen::Tensor<double, 1>(5); flex_(y) = x;', types = list(y = 'numericVector'))
      cppLiteral('return y;')
      returnType('numericVector')
    }),
  vector_from_scalar = nFunction(
    function(a = 'numericScalar') {
      cppLiteral('y = Eigen::Tensor<double, 1>(5); flex_(y) = a;', types = list(y = 'numericVector'))
      cppLiteral('return y;')
      returnType('numericVector')
    }),
  vector_from_reduction = nFunction(
    function(x = 'numericVector') {
      cppLiteral('y = Eigen::Tensor<double, 1>(5); flex_(y) = x.sum();', types = list(y = 'numericVector'))
      cppLiteral('return y;')
      returnType('numericVector')
    }),
  matrix_from_vector = nFunction(
    function(x = 'numericVector') {
      cppLiteral('flex_(M) = x;', types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    }),
  matrix_from_array3 = nFunction(
    function() {
      cppLiteral('Eigen::Tensor<double, 3> A(2, 1, 3); for(int i = 0; i < 6; ++i) A.data()[i] = i + 1; flex_(M) = A;',
                 types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    }),
  matrix_from_scalar = nFunction(
    function(a = 'numericScalar') {
      cppLiteral('flex_(M) = a;', types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    }),
  array3_from_matrix = nFunction(
    function(X = 'numericMatrix') {
      cppLiteral('flex_(A) = X;', types = list(A = 'numericArray(nDim = 3)'))
      cppLiteral('return A;')
      returnType('numericArray(nDim = 3)')
    }),
  array3_from_array4 = nFunction(
    function(B = 'numericArray(nDim = 4)') {
      cppLiteral('flex_(A) = B;', types = list(A = 'numericArray(nDim = 3)'))
      cppLiteral('return A;')
      returnType('numericArray(nDim = 3)')
    }),
  ## Fixed-size Lhs (Eigen ops that can be assigned to but not resized)
  column_from_vector = nFunction(
    function(x = 'numericVector') {
      cppLiteral('M = Eigen::Tensor<double, 2>(3, 3); M.setZero(); flex_(M.chip(1, 1)) = x;',
                 types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    }),
  column_from_matrix = nFunction(
    function(X = 'numericMatrix') {
      cppLiteral('M = Eigen::Tensor<double, 2>(3, 3); M.setZero(); flex_(M.chip(1, 1)) = X;',
                 types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    }),
  slice_from_vector = nFunction(
    function(x = 'numericVector') {
      cppLiteral('M = Eigen::Tensor<double, 2>(3, 3); M.setZero();
                  Eigen::array<Eigen::Index, 2> offsets{{0, 2}}, extents{{3, 1}};
                  flex_(M.slice(offsets, extents)) = x;', types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    }),
  slice_from_matrix = nFunction(
    function(X = 'numericMatrix') {
      cppLiteral('M = Eigen::Tensor<double, 2>(3, 3); M.setZero();
                  Eigen::array<Eigen::Index, 2> offsets{{0, 2}}, extents{{3, 1}};
                  flex_(M.slice(offsets, extents)) = X;', types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    }),
  column_from_scalar = nFunction(
    function(a = 'numericScalar') {
      cppLiteral('M = Eigen::Tensor<double, 2>(3, 3); M.setZero(); flex_(M.chip(1, 1)) = a;',
                 types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    }),
  column_from_reduction = nFunction(
    function(x = 'numericVector') {
      cppLiteral('M = Eigen::Tensor<double, 2>(3, 3); M.setZero(); flex_(M.chip(1, 1)) = x.sum();',
                 types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    }),
  column_from_int_vector = nFunction(
    function(x = 'integerVector') {
      cppLiteral('M = Eigen::Tensor<double, 2>(3, 3); M.setZero(); flex_(M.chip(1, 1)) = x;',
                 types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    }),
  named_column_from_vector = nFunction(
    function(x = 'numericVector') {
      cppLiteral('M = Eigen::Tensor<double, 2>(3, 3); M.setZero(); auto col = M.chip(1, 1); flex_(col) = x;',
                 types = list(M = 'numericMatrix'))
      cppLiteral('return M;')
      returnType('numericMatrix')
    })
)

cFlexFuns <- do.call(nCompile, flexFuns)

test_that("flex_ assigns to a scalar from objects with one element", {
  expect_equal(cFlexFuns$scalar_from_int_literal(), 3)
  expect_equal(cFlexFuns$scalar_from_reduction(1:3), 6)
  expect_equal(cFlexFuns$scalar_from_vector(5), 5)
  expect_equal(cFlexFuns$scalar_from_matrix_op(matrix(4, 1, 1)), 8)
  expect_identical(cFlexFuns$int_from_vector(7.9), 7L)
  expect_true(cFlexFuns$bool_from_reduction(1:3))
  expect_false(cFlexFuns$bool_from_reduction(1:2))
  expect_equal(cFlexFuns$long_from_vector(12), 12)
  expect_error(cFlexFuns$scalar_from_vector(1:3), "extents \\(.*\\) to a scalar")
  expect_error(cFlexFuns$scalar_from_matrix_op(matrix(1:4, 2)), "extents \\(.*\\) to a scalar")
})

test_that("flex_ assigns to whole objects, resizing them", {
  expect_equal(cFlexFuns$vector_from_matrix(matrix(1:4, 4, 1)), 1:4)
  expect_equal(cFlexFuns$vector_from_matrix(matrix(1:4, 1, 4)), 1:4)
  expect_equal(cFlexFuns$vector_from_matrix(matrix(3, 1, 1)), 3)
  expect_equal(cFlexFuns$vector_from_matrix_op(matrix(1:3, 1, 3)), 2 * (1:3))
  expect_error(cFlexFuns$vector_from_matrix(matrix(1:6, 2, 3)), "to an object with 1 dimension")
  expect_equal(cFlexFuns$vector_from_vector_cast(1:3), c(1, 2, 3))
  expect_equal(cFlexFuns$vector_from_scalar(2.5), 2.5)
  expect_equal(cFlexFuns$vector_from_reduction(1:4), 10)
  expect_equal(cFlexFuns$matrix_from_vector(1:3), matrix(1:3, 3, 1))
  expect_equal(cFlexFuns$matrix_from_array3(), matrix(1:6, 2, 3))
  expect_equal(cFlexFuns$matrix_from_scalar(4), matrix(4, 1, 1))
  # Spare LHS dimensions keep the leftmost leading or interior 1s of the RHS.
  expect_equal(cFlexFuns$array3_from_matrix(matrix(1:5, 1, 5)), array(1:5, c(1, 5, 1)))
  expect_equal(cFlexFuns$array3_from_matrix(matrix(1:5, 5, 1)), array(1:5, c(5, 1, 1)))
  expect_equal(cFlexFuns$array3_from_array4(array(1:30, c(5, 1, 6, 1))), array(1:30, c(5, 1, 6)))
  expect_equal(cFlexFuns$array3_from_array4(array(1:30, c(1, 5, 1, 6))), array(1:30, c(1, 5, 6)))
  expect_equal(cFlexFuns$array3_from_array4(array(1:30, c(5, 6, 1, 1))), array(1:30, c(5, 6, 1)))
  expect_equal(cFlexFuns$array3_from_array4(array(7, c(1, 1, 1, 1))), array(7, c(1, 1, 1)))
  expect_error(cFlexFuns$array3_from_array4(array(1:24, c(2, 3, 2, 2))), "to an object with 3 dimension")
})

test_that("flex_ assigns to fixed-size targets when extents match except for 1s", {
  colResult <- function(v) { M <- matrix(0, 3, 3); M[, 2] <- v; M }
  lastColResult <- function(v) { M <- matrix(0, 3, 3); M[, 3] <- v; M }
  expect_equal(cFlexFuns$column_from_vector(1:3), colResult(1:3))
  expect_equal(cFlexFuns$column_from_matrix(matrix(1:3, 1, 3)), colResult(1:3))
  expect_equal(cFlexFuns$column_from_matrix(matrix(1:3, 3, 1)), colResult(1:3))
  expect_equal(cFlexFuns$slice_from_vector(1:3), lastColResult(1:3))
  expect_equal(cFlexFuns$slice_from_matrix(matrix(1:3, 1, 3)), lastColResult(1:3))
  expect_equal(cFlexFuns$slice_from_matrix(matrix(1:3, 3, 1)), lastColResult(1:3))
  expect_equal(cFlexFuns$column_from_int_vector(1:3), colResult(1:3))
  expect_equal(cFlexFuns$named_column_from_vector(4:6), colResult(4:6))
  expect_error(cFlexFuns$column_from_vector(1:2), "Extents must match")
  expect_error(cFlexFuns$column_from_matrix(matrix(1:6, 2, 3)), "Extents must match")
  expect_error(cFlexFuns$slice_from_vector(1:4), "Extents must match")
})

test_that("flex_ fills fixed-size targets from scalars", {
  colResult <- function(v) { M <- matrix(0, 3, 3); M[, 2] <- v; M }
  expect_equal(cFlexFuns$column_from_scalar(7), colResult(7))
  expect_equal(cFlexFuns$column_from_reduction(1:3), colResult(6))
})

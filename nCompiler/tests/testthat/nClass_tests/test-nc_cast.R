# Tests of nc_cast (see generic_class_interface.h), which casts between nClass types
# without relying on RTTI (dynamic_cast) matching across DLLs, and of the corresponding
# typed_cast/tensor_cast for ETaccessor objects (see ETaccessor_post_Rcpp.h).
# Each call to nCompile creates a separate DLL, so objects created by one nCompile
# call and used by code from another exercise the cross-DLL case.

#library(nCompiler); library(testthat)

# Generators are named the same as their classnames, so that, e.g., the type 'ncA_nc_cast'
# of a member is found by scoping. Each test puts them in its own environment with list2env.
make_nc_cast_defs <- function() {
  ncA_nc_cast <- nClass(
    classname = "ncA_nc_cast",
    Cpublic = list(
      x = 'numericScalar',
      v = 'numericVector',
      foo = nFunction(
        function() {return(x)},
        returnType = 'numericScalar',
        compileInfo = list(virtual = TRUE)
      )
    )
  )
  ncB_nc_cast <- nClass(
    classname = "ncB_nc_cast",
    inherit = ncA_nc_cast,
    Cpublic = list(
      foo = nFunction(
        function() {return(x + 100)},
        returnType = 'numericScalar'
      )
    )
  )
  # ncC holds an ncA (by base class) and uses it.
  ncC_nc_cast <- nClass(
    classname = "ncC_nc_cast",
    Cpublic = list(
      a = 'ncA_nc_cast',
      call_foo = nFunction(function() {return(a$foo())}, returnType = 'numericScalar'),
      # Use access() of the held object, which returns an ETaccessor created by that object's DLL.
      access_x = nFunction(function() {
        cppLiteral('return a->access("x")->scalar<double>();')
        returnType('numericScalar')
      }),
      access_v_sum = nFunction(function() {
        cppLiteral('Eigen::Tensor<double, 1> &vref = a->access("v")->ref<1, double>(); double ans = 0; for(int i = 0; i < vref.size(); ++i) ans += vref(i); return ans;')
        returnType('numericScalar')
      })
    )
  )
  ncD_nc_cast <- nClass(
    classname = "ncD_nc_cast",
    Cpublic = list(y = 'numericScalar')
  )
  list(ncA_nc_cast = ncA_nc_cast, ncB_nc_cast = ncB_nc_cast,
       ncC_nc_cast = ncC_nc_cast, ncD_nc_cast = ncD_nc_cast)
}

test_that("nc_class_key is generated, unique per generator, and the class name for predefined nClasses", {
  list2env(make_nc_cast_defs(), environment())
  defs2 <- make_nc_cast_defs()
  keyA <- NCinternals(ncA_nc_cast)$nc_class_key
  expect_true(startsWith(keyA, "ncA_nc_cast_"))
  # Two generators with the same class name get different keys.
  expect_false(identical(keyA, NCinternals(defs2$ncA_nc_cast)$nc_class_key))
  # A predefined nClass's key is its class name.
  expect_identical(NCinternals(nListBase_nClass)$nc_class_key,
                   NCinternals(nListBase_nClass)$cpp_classname)
  # The key is emitted in the class declaration.
  cppDefs <- nCompile(ncA_nc_cast, control = list(return_cppDefs = TRUE))
  out <- capture_output(cppDefs[[1]]$generate(declaration = TRUE) |> writeCode())
  expect_true(grepl(paste0('nc_class_key() {return "', keyA, '";}'), out, fixed = TRUE))
})

test_that("nc_cast works for assignment to a base class member within one DLL", {
  list2env(make_nc_cast_defs(), environment())
  comp <- nCompile(ncA_nc_cast, ncB_nc_cast, ncC_nc_cast, ncD_nc_cast)
  b <- comp$ncB_nc_cast$new()
  b$x <- 2
  obj <- comp$ncC_nc_cast$new()
  obj$a <- b
  expect_equal(obj$call_foo(), 102)
  d <- comp$ncD_nc_cast$new()
  expect_error(obj$a <- d, "Invalid nClass assignment")
  rm(b, obj, d); gc()
})

test_that("nc_cast works for an object from one DLL assigned to a base class member of a class from another DLL", {
  list2env(make_nc_cast_defs(), environment())
  # Separate nCompile calls create separate DLLs. ncA is compiled into both as a needed class.
  compB <- nCompile(ncB_nc_cast)
  compC <- nCompile(ncC_nc_cast)
  compD <- nCompile(ncD_nc_cast)
  b <- compB$new()
  b$x <- 3
  b$v <- c(1, 2, 3)
  obj <- compC$new()
  obj$a <- b
  # The call goes through b's own vtable (from the first DLL) to the derived method.
  expect_equal(obj$call_foo(), 103)
  # ETaccessor objects created by b's DLL are used by code in obj's DLL.
  expect_equal(obj$access_x(), 3)
  expect_equal(obj$access_v_sum(), 6)
  # An object of an unrelated class from another DLL is rejected.
  d <- compD$new()
  expect_error(obj$a <- d, "Invalid nClass assignment")
  rm(b, obj, d); gc()
})

test_that("a compiled nList of scalars can be copied from another DLL", {
  # Separate nCompile calls create separate DLLs.
  rNL_a <- nList("numericScalar")
  rNL_b <- nList("numericScalar")
  cNL_a <- nCompile(rNL_a)
  cNL_b <- nCompile(rNL_b)
  src <- cNL_a$new()
  src$set_all_values(list(10, 20, 30))
  obj <- cNL_b$new()
  obj$set_all_values(src)
  expect_equal(obj$getLength(), 3L)
  for(i in 1:3) expect_equal(obj[[i]], i * 10)
  rm(src, obj); gc()
})

test_that("a compiled nList of nClass objects can be copied from another DLL, sharing the elements", {
  ncF_nc_cast <- nClass(classname = "ncF_nc_cast", Cpublic = list(x = 'numericScalar'))
  NLF_a <- nList(ncF_nc_cast())
  NLF_b <- nList(ncF_nc_cast())
  comp_a <- nCompile(ncF_nc_cast, NLF_a)
  cNLF_b <- nCompile(NLF_b) # ncF_nc_cast is compiled again into this DLL as a needed class
  e <- comp_a$ncF_nc_cast$new()
  e$x <- 5
  # nCompile names a compiled nList by its class name, so we index by position.
  src <- comp_a[[2]]$new()
  src$set_all_values(list(e))
  obj <- cNLF_b$new()
  obj$set_all_values(src)
  expect_equal(obj$getLength(), 1L)
  expect_equal(obj[[1]]$x, 5)
  e$x <- 6 # the element is shared, not copied
  expect_equal(obj[[1]]$x, 6)
  rm(e, src, obj); gc()
})

test_that("copying a compiled nList rejects elements of a different class with the same C++ name", {
  # Two different generators with the same classname, so the same C++ class name but
  # different layouts, each compiled with an nList of it in its own DLL. Copying an element
  # between them must be rejected rather than succeed by matching type information by name.
  make_and_compile <- function(fields) {
    ncE_nc_cast <- nClass(classname = "ncE_nc_cast", Cpublic = fields)
    NLE_nc_cast <- nList(ncE_nc_cast())
    nCompile(ncE_nc_cast, NLE_nc_cast)
  }
  comp_1 <- make_and_compile(list(x = 'numericScalar'))
  comp_2 <- make_and_compile(list(v = 'numericVector', y = 'integerScalar'))
  e <- comp_1$ncE_nc_cast$new()
  src <- comp_1[[2]]$new() # the nList; see note in previous test
  src$set_all_values(list(e))
  obj <- comp_2[[2]]$new()
  expect_error(obj$set_all_values(src), "not of this nList's element type")
  rm(e, src, obj); gc()
})

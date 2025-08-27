library(nCompiler)

message("uncompiled nClass Cpublic variables are not initialized well")
message("Using inheritance for nClasses with serialization needs fixing.")
message("See comments in test-nClass_inherit.R for more notes.")

## See also test-nClass_nested

# With inheritcance, we DO NOT support interfacing to both base class and derived class.
# Only the most derived class should have interface = "generic" or "base".
# Any class to be used as a base class should have interface = "none".
# If one wants a pure object of that class, make an inherited class solely
# for the purpose of having an interface.
# This limitation would appear to be quite tricky to work around in C++,
#  so there are no immediate plans to do so.

# Making R6 and C++ inheritance behavior match comes reasonably close but is
# not perfect.

# We support a compileInfo element for nFunction methods of nClasses that is
# `virtual` set to TRUE or FALSE. This is what is sounds like: whether to make
# the C++ method virtual.

# R6 semantics are natively like "virtual": There is no notion of having a
# pointer to either base or derived. You just have an object, so if there
# is a method of the same name as in a base class, the derived method will be called.
# One can access base class methods as super$method(). We currently do not
# support that syntax but could potentially consider it.

# In our C++, only the most derived class should have an interface, so
# in effect we have the same system: the most derived version will be called.

# It appears that in R6, if a base and derived class have a member variable (not method)
# of the same name, there is only ever one copy of it, not one for each level
# of the class hierarchy.

# Finding inheritance in R6 was tricky because
#  the generator retains an unevaluated expression for inherits.
#  We resolve that once an put it in NCinternals.

# Here is a summary of cases where compiled and uncompiled behavior will differ:
#
# 1. Base class and derived class both have member variable ("x"):
#    - In uncompiled, there is only ever one "x".
#    - In compiled, only the derived "x" is accessed by the interface.
#    - If one provided get/set methods for changing "x" in the base class
#      and if there are base class methods that use "x", then uncompiled and
#      compiled could use different values of "x".
#
# 2. No "super" in compiled code. Currently there is no compilation support for using
#    "self$super$method" to access base class methods.
#
# 3. cppLiteral coding of base class methods or use of inheritance:
#    Well, anything in cppLiteral is not supported for uncompiled execution.
#    Here in particular it stands out that harnessing virtual method dispatch is
#    not something that can be mimicked in uncompiled R6.
#
# Hence the following recommendations if one wants uncompiled and compiled to have
#  the same behavior:
# - Do not use the same variable name in base and derived classes.
# - Do not use "super".

test_that("nClass hierarchies work as expected (including uncompiled vs compiled discrepancies)", {
  ncA <- nClass(
    Rpublic = list(
      fooRA = function() v.A
    ),
    Cpublic = list(
      v.A = 'numericVector',
      wA = 'numericScalar',
      xA = 'numericScalar',
      # Method to overload in derived class, using duplicated member
      add.wA = nFunction(
        function(x.1 = 'numericVector') {
          return(wA + x.1); returnType('numericVector')
        },
        compileInfo = list(virtual = FALSE)
      ),
      # Method to inherit and specialize in derived class, using duplicated member
      add.2wA.virt = nFunction(
        function(x.1 = 'numericVector') {
          return(2*wA + x.1); returnType('numericVector')
        },
        compileInfo = list(virtual = TRUE)
      ),
      # Non-virtual method to call as base-class method from derived class, using duplicated member
      sub.wA = nFunction(
        function(x.1 = 'numericVector') {
          return(-wA + x.1); returnType('numericVector')
        },
        compileInfo = list(virtual = FALSE)
      ),
      # Virtual non-specialized method to call as base-class method from derived class, using duplicated member
      sub.2wA.virt = nFunction(
        function(x.1 = 'numericVector') {
          return(-2*wA + x.1); returnType('numericVector')
        },
        compileInfo = list(virtual = TRUE)
      ),
      # Non-virtual method to call as base-class method from derived class, using non-duplicated member
      add.xA = nFunction(
        function(x.1 = 'numericVector') {
          return(xA + x.1); returnType('numericVector')
        },
        compileInfo = list(virtual = FALSE)
      ),
      # Virtual non-specialized method to call as base-class method from derived class, using non-duplicated member
      add.2xA.virt = nFunction(
        function(x.1 = 'numericVector') {
          return(2*xA + x.1); returnType('numericVector')
        },
        compileInfo = list(virtual = TRUE)
      ),
      # Way to get and set base-class duplicated wA
      set_wA_A = nFunction(
        function(v = 'numericScalar') {
          wA <<- v
        }),
      get_wA_A = nFunction(
        function() {
          return(wA); returnType('numericScalar')
        })
    ),
    compileInfo = list(interface = "none", createFromR = FALSE)
  )

  ncB <- nClass(
    inherit = ncA,
    Cpublic = list(
      wA = 'numericScalar',
      zB = 'numericScalar',
      mult.wA = nFunction(
        function(v = 'numericVector') {
          return(wA * v); returnType('numericVector')
        }),
      add.wA = nFunction(
        function(v = 'numericVector') {
          return(wA + v + 1000); returnType('numericVector')
        }),
      add.2wA.virt = nFunction(
        function(v = 'numericVector') {
          return(2*wA + v + 1000); returnType('numericVector')
        })
    )
  )

  # nOptions(pause_after_writing_files = TRUE)
  for(i in 1:2) { # non-packaged then pacakged
    if(i == 1) {
      comp <- nCompile(ncB, ncA, package = FALSE) # Order no longer matters (good) but could if they have different #define's or  #include's???
    } else if (i == 2) {
      comp <- nCompile(ncA, ncB, package = TRUE) # Order no longer matters (good)
    }
    objB <- ncB$new()
    CobjB <- comp$ncB$new()

    objB$wA <- 10
    CobjB$wA <- 10
    expect_equal(objB$wA, 10)
    expect_equal(CobjB$wA, 10)

    ## Call derived-only method
    expect_equal(objB$mult.wA(1:3), 10*(1:3))
    expect_equal(CobjB$mult.wA(1:3), 10*(1:3))

    ## Call over-loaded (non-virtual) derived method
    expect_equal(objB$add.wA(1:3), 10 + (1:3) + 1000)
    expect_equal(CobjB$add.wA(1:3), 10 + (1:3) + 1000)

    ## Call virtual derived method
    expect_equal(objB$add.2wA.virt(1:3), 2*10 + (1:3) + 1000)
    expect_equal(CobjB$add.2wA.virt(1:3), 2*10 + (1:3) + 1000)

    objB$xA <- 15
    CobjB$xA <- 15
    expect_equal(objB$xA, 15)
    expect_equal(CobjB$xA, 15)

    ## Call base method (non-virtual, not overloaded)
    expect_equal(objB$add.xA(1:3), 15 + (1:3))
    expect_equal(CobjB$add.xA(1:3), 15 + (1:3))

    ## Call base method (virtual, not specialized)
    expect_equal(objB$add.2xA.virt(1:3), 2*15 + (1:3))
    expect_equal(CobjB$add.2xA.virt(1:3), 2*15 + (1:3))


    #### In the following two cases, uncompiled accesses the one and only wA
    #### but compiled accesses the base class wA, which has not been set.
    ## Call over-loaded (non-virtual) derived method
    expect_equal(objB$sub.wA(1:3), -10 + (1:3))
    message("Known discrepancy in compiled vs uncompiled occurs with inheritance if there are member variables with the same name.")
    expect_false(all(CobjB$sub.wA(1:3) == -10 + (1:3)))

    ## Call virtual derived method
    expect_equal(objB$sub.2wA.virt(1:3), -2*10 + (1:3))
    message("Known discrepancy in compiled vs uncompiled occurs with inheritance if there are member variables with the same name.")
    expect_false(all(CobjB$sub.2wA.virt(1:3) == -2*10 + (1:3)))

    #### We can use get/set methods to set the base class wA
    #### but then the uncompiled object will have its one and only wA changed
    #### and thus the derived-class results will differ
    objB$set_wA_A(37)
    CobjB$set_wA_A(37)
    expect_equal(objB$wA, 37)
    expect_false(CobjB$wA == 37)
    expect_equal(CobjB$wA, 10)
    expect_equal(CobjB$get_wA_A(), 37)

    # so now everything uses -37
    expect_equal(objB$sub.wA(1:3), -37 + (1:3))
    expect_true(all(CobjB$sub.wA(1:3) == -37 + (1:3)))

    expect_equal(objB$sub.2wA.virt(1:3), -2*37 + (1:3))
    expect_true(all(CobjB$sub.2wA.virt(1:3) == -2*37 + (1:3)))

    # and the discrepancy arises here, where compiled still has a derived class value of 10
    message('Known discrepancy, different manifestation.')
    expect_equal(objB$add.wA(1:3), 37 + (1:3) + 1000)
    expect_equal(CobjB$add.wA(1:3), 10 + (1:3) + 1000)

    ## Call virtual derived method
    message('Known discrepancy, different manifestation.')
    expect_equal(objB$add.2wA.virt(1:3), 2*37 + (1:3) + 1000)
    expect_equal(CobjB$add.2wA.virt(1:3), 2*10 + (1:3) + 1000)

    rm(objB, CobjB)
    gc()
  }

})

##############

test_that("inheriting-only classes in 3-level hierarchy works", {
  ncBase <- nClass(
    classname = "ncBase",
    Cpublic = list(
      x = 'numericScalar',
      add_x = nFunction(function(v = 'numericScalar') {
        return(v + x); returnType('numericScalar');
      },
      name = "add_x"),
      add_2x_virt = nFunction(function(v = 'numericScalar') {
        return(v + 2*x); returnType('numericScalar');
      })
    ),
    compileInfo = list(interface = "none",createFromR=FALSE)
  )

  ncMid <- nClass(
    inherit = ncBase,
    classname = "ncMid",
    compileInfo = list(interface = "none",createFromR=FALSE),
    Cpublic = list(x2 = 'numericScalar')
  )

  ncDer <- nClass(
    inherit = ncMid,
    Cpublic = list(x3 = 'numericScalar')
  )

  comp <- nCompile(ncBase, ncMid, ncDer) # check if order still matters

  Cobj <- comp$ncDer$new()
  Cobj$x <- 10
  expect_equal(Cobj$add_x(15), 25)
  expect_equal(method(Cobj$private$CppObj, "add_x")(15), 25)
  expect_equal(Cobj$add_2x_virt(15), 35)
})

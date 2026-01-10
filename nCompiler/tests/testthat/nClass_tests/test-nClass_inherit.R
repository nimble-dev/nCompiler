#library(nCompiler)

message("uncompiled nClass Cpublic variables are not initialized well")
message("Using inheritance for nClasses with serialization needs fixing.")
message("See comments in test-nClass_inherit.R for more notes.")

## See also test-nClass_nested

# The `inherit` argument to nClass can take a single argument, similar to R6
# It is captured as an expression that returns a single nClass generator.
# (This must always be the same object, so the expression can't generate a new one each time it is evaluated.)

# We use the inheritance semantics of R6 classes to set the default rules for
# nClasses.
#
# For fields: If two R6 classes have fields of the same name, they seem to
#. become one field. Therefore we disallow this in nClasses in order to
#  avoid generating C++ classes that actually have two distinct members
#. of the same name and then getting different compiled vs. uncompiled behavior.
#  This is checked in NC_check_inheritance.
#. nOptions(allow_inherited_field_duplicates=TRUE) disables this rule and
#. checking, and allows nCompile to happily generate C++ classes with
#  distinct members of the same name. This is fine if a user doesn't care
#.  about uncompiled behavior or discrepancies.
#
# For methods: In two R6 classes have methods of the same name, that works
#  fine and a base class method can be accessed by super$foo().
#. However, R6 has no notion of virtual vs. non-virtual inheritance, no
#. notion of signatures (argument and return types) being required to match
#. for virtual inheritance, and no notion of base class pointers. In effect,
#  R6 objects are just passed as objects and a method call will always use
#  the most derived version. To match that, we require nClass inherited methods
#. of the same name to have exactly matching argument names, types, and return type.
#  And we require that the first base class with a method must mark it with
#. compileInfo=list(virtual=TRUE) (in the nFunction call). The last requirement
#  is a bit like the use of "override" in C++ in that it shouldn't be strictly
#  necessary but can allow us during compilation to catch potentially nasty bugs
#  by giving the programmer a way to declare their intention. We require that
#. (whereas C++ override is optional).  Error-trapping happens in NC_check_inheritance.
#. nOptions(allow_method_overloading=TRUE) removes these rules and allows
#. the compiler to generate C++ classes with overloaded versions of the same
#  name and to have even the same name and signature be not virtual. This
# only makes sense if the user doesn't care about uncompiled behavior matching.

# As just noted, we support a compileInfo element for nFunction methods of nClasses that is
# `virtual` set to TRUE or FALSE. This is what is sounds like: whether to make
# the C++ method virtual.

# Finding inheritance in R6 was tricky because
#  the generator retains an unevaluated expression for inherits.
#  We now keep it that way as `inheritQ` (for "quoted")
# This allows an nClass call to inherit from a method that isn't defined yet.

# We do not currently support "super$" in compilation, so there is no
# way to call a base class method (yet).

test_that("nClass hierarchy traps lack of virtual declaration", {
  ncA <- nClass(
    Cpublic = list(
      foo = nFunction(
        function(x=double(1)) {returnType(integer(1))},
        compileInfo=list(virtual=FALSE)
      )
    )
  )
  ncB <- nClass(
    inherit = ncA,
    Cpublic = list(
      foo = nFunction(
        function(x=double(1)) {returnType(integer(1))},
        compileInfo=list(virtual=FALSE)
      )
    )
  )
  expect_error(
    comp <- nCompile(ncA, ncB)
  )
})


test_that("nClass hierarchy traps mismatched argument names", {
  ncA <- nClass(
    Cpublic = list(
      foo = nFunction(
        function(z=double(1)) {returnType(integer(1))},
        compileInfo=list(virtual=TRUE)
      )
    )
  )
  ncB <- nClass(
    inherit = ncA,
    Cpublic = list(
      foo = nFunction(
        function(x=double(1)) {returnType(integer(1))},
        compileInfo=list(virtual=FALSE)
      )
    )
  )
  expect_error(
    comp <- nCompile(ncA, ncB)
  )
})

test_that("nClass hierarchy traps mismatched argument types", {
  ncA <- nClass(
    Cpublic = list(
      foo = nFunction(
        function(x=double(0)) {returnType(integer(1))},
        compileInfo=list(virtual=TRUE)
      )
    )
  )
  ncB <- nClass(
    inherit = ncA,
    Cpublic = list(
      foo = nFunction(
        function(x=double(1)) {returnType(integer(1))},
        compileInfo=list(virtual=FALSE)
      )
    )
  )
  expect_error(
    comp <- nCompile(ncA, ncB)
  )
})

test_that("nClass hierarchy traps mismatched return types", {
  ncA <- nClass(
    Cpublic = list(
      foo = nFunction(
        function(x=double(1)) {returnType(integer(1))},
        compileInfo=list(virtual=TRUE)
      )
    )
  )
  ncB <- nClass(
    inherit = ncA,
    Cpublic = list(
      foo = nFunction(
        function(x=double(1)) {returnType(integer(0))},
        compileInfo=list(virtual=FALSE)
      )
    )
  )
  expect_error(
    comp <- nCompile(ncA, ncB)
  )
})

test_that("nClass hierarchy traps inherited field duplicate names", {
  ncA <- nClass(
    Cpublic = list(
      x = 'numericVector',
      y = 'numericVector',
      foo = nFunction(
        function(x=double(1)) {returnType(integer(1))},
        compileInfo=list(virtual=TRUE)
      )
    )
  )
  ncB <- nClass(
    inherit = ncA,
    Cpublic = list(
      x = 'numericVector',
      z = 'numericVector',
      foo = nFunction(
        function(x=double(1)) {returnType(integer(1))},
        compileInfo=list(virtual=FALSE)
      )
    )
  )
  expect_error(
    comp <- nCompile(ncA, ncB)
  )
})

message("add a test that inheriting from interface = 'none' results in not being able to use base class fields and methods")

test_that("nClass hierarchies work as expected (including uncompiled vs compiled discrepancies)",
{
  # This was written before all the error-trapping above.
  # I am going to disable the error-trapping. I think this is good
  # because now we also test the more general compilation, but
  # I may not be thinking about cases we're missing.
  oldOpt1 <- nOptions("allow_method_overloading")
  oldOpt2 <- nOptions("allow_inherited_field_duplicates")
  nOptions(allow_method_overloading = TRUE)
  nOptions(allow_inherited_field_duplicates = TRUE)
  # The tests below worked in an early version.
  # Then some changes were made around inheritance handling.
  # Now we need these options to keep these tests functioning.
  # To-Do: Update these tests to be more deliberate about these options.
  on.exit({
    nOptions(allow_method_overloading = oldOpt1)
    nOptions(allow_inherited_field_duplicates = oldOpt2)
  })
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
    compileInfo = list(interface = "generic", createFromR = FALSE)
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

# cat("With inheritance, we may now be able to interface at multiple levels, but it is untested.\n")

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
    compileInfo = list(interface = "generic",createFromR=FALSE)
  )

  ncMid <- nClass(
    inherit = ncBase,
    classname = "ncMid",
    compileInfo = list(interface = "generic",createFromR=FALSE),
    Cpublic = list(x2 = 'numericScalar')
  )

  ncDer <- nClass(
    inherit = ncMid,
    Cpublic = list(x3 = 'numericScalar')
  )

  ncUseBase <- nClass(
    classname = "ncUseBase",
    Cpublic = list(
      myBase = 'ncBase',
      call_add_x = nFunction(
        fun = function(v = 'numericScalar') {
          return(myBase$add_x(v)); returnType('numericScalar')
        }
      )
    )
  )

  comp <- nCompile(ncUseBase, ncBase, ncMid, ncDer) # check if order still matters
  Cobj <- comp$ncDer$new()
  Cobj$x <- 10
  expect_equal(Cobj$add_x(15), 25)
  expect_equal(method(to_generic_interface(Cobj), "add_x")(15), 25)
  expect_equal(Cobj$add_2x_virt(15), 35)

  Cobj2 <- comp$ncUseBase$new()
  expect_true(is.null(Cobj2$myBase))
  Cobj2$myBase <- Cobj
  expect_equal(Cobj2$call_add_x(15), 25)

  rm(Cobj, Cobj2); gc()
})

cat("Add inline checking of validity of shared_ptr's in generated code.\n")

test_that("inheritance with interfaces at multiple levels", {
  # This was written before all the error-trapping above.
  # I am going to disable the error-trapping. I think this is good
  # because now we also test the more general compilation, but
  # I may not be thinking about cases we're missing.
  oldOpt1 <- nOptions("allow_method_overloading")
  oldOpt2 <- nOptions("allow_inherited_field_duplicates")
  nOptions(allow_method_overloading = TRUE)
  nOptions(allow_inherited_field_duplicates = TRUE)
  on.exit({
    nOptions(allow_method_overloading = oldOpt1)
    nOptions(allow_inherited_field_duplicates = oldOpt2)
  })
  ncBase <- nClass(
    classname = "ncBase",
    Cpublic = list(
      base_x = 'numericScalar',
      # get_base_x will be non-virtual and uniquely named
      get_base_x = nFunction(
        function() {
          return(base_x); returnType('numericScalar')
        },
        name = "get_base_x"),
      # get_x will be non-virtual and non uniquely named
      get_x = nFunction(
        function() {
          return(base_x); returnType('numericScalar')
        },
        name = "get_x"),
    # get_x_virt will be virtual
      get_x_virt = nFunction(
        function() {
          return(base_x); returnType('numericScalar')
        },
        name = "get_x_virt",
        compileInfo=list(virtual=TRUE))
    ),
    compileInfo = list(interface = "full",createFromR=TRUE)
  )

  ncMid <- nClass(
    inherit = ncBase,
    classname = "ncMid",
    Cpublic = list(
      mid_x = 'numericScalar',
      # get_base_x will be non-virtual and uniquely named
      get_mid_x = nFunction(
        function() {
          return(mid_x); returnType('numericScalar')
        },
        name = "get_mid_x"),
      # get_x will be non-virtual and non uniquely named
      get_x = nFunction(
        function() {
          return(mid_x); returnType('numericScalar')
        },
        name = "get_x"),
      # get_base_x_from_mid will be non-virtual and access base class member
      get_base_x_from_mid = nFunction(
        function() {
          return(base_x); returnType('numericScalar')
        },
        name = "get_base_x_from_mid"),

      # get_x_virt will be virtual
      get_x_virt = nFunction(
        function() {
          return(mid_x); returnType('numericScalar')
        },
        name = "get_x_virt",
        compileInfo=list(virtual=TRUE))
    ),
    compileInfo = list(interface = "full",createFromR=FALSE)
  )

  ncDer <- nClass(
    inherit = ncMid,
    Cpublic = list(
      make_mid = nFunction(
        function() {return(ncMid$new()); returnType('ncMid')}
      ),
      der_x = 'numericScalar',
      # get_base_x will be non-virtual and uniquely named
      get_der_x = nFunction(
        function() {
          return(der_x); returnType('numericScalar')
        },
        name = "get_der_x"),
      # get_x will be non-virtual and non uniquely named
      get_x = nFunction(
        function() {
          return(der_x); returnType('numericScalar')
        },
        name = "get_x"),
      # get_base_x_from_mid will be non-virtual and access base class member
      get_base_x_from_der = nFunction(
        function() {
          return(base_x); returnType('numericScalar')
        },
        name = "get_base_x_from_der"),
      get_mid_x_from_der = nFunction(
        function() {
          return(mid_x); returnType('numericScalar')
        },
        name = "get_mid_x_from_der"),

      # get_x_virt will be virtual
      get_x_virt = nFunction(
        function() {
          return(der_x); returnType('numericScalar')
        },
        name = "get_x_virt",
        compileInfo=list(virtual=TRUE))
    ),
    compileInfo = list(interface = "full",createFromR=TRUE)
  )

  useClasses <- nClass(
    classname = "useClasses",
    Cpublic = list(
      myBase = 'ncBase',
      myMid = 'ncMid',
      myDer = 'ncDer',
      useBase = nFunction(
        function(i = integer()) {
          returnType(double())
          if(i == 1) return(myBase$get_x_virt())
          if(i == 2) return(myBase$get_base_x())
          if(i == 3) return(myBase$get_x())
          if(i == 4) return(myBase$base_x)
          return(-1)
        },
        name = "useBase"),
      useMid = nFunction(
        function(i = integer()) {
          returnType(double())
          if(i == 1) return(myMid$get_x_virt())
          if(i == 2) return(myMid$get_base_x())
          if(i == 3) return(myMid$get_x())
          if(i == 4) return(myMid$base_x)

          if(i == 5) return(myMid$get_base_x_from_mid())
          if(i == 6) return(myMid$get_mid_x())
          if(i == 7) return(myMid$mid_x)
          return(-1)
        }
      ),
      useDer = nFunction(
        function(i = integer()) {
          returnType(double())
          if(i == 1) return(myDer$get_x_virt())
          if(i == 2) return(myDer$get_base_x())
          if(i == 3) return(myDer$get_x())
          if(i == 4) return(myDer$base_x)

          if(i == 5) return(myDer$get_base_x_from_mid())
          if(i == 6) return(myDer$get_mid_x())
          if(i == 7) return(myDer$mid_x)

          if(i == 8) return(myDer$get_base_x_from_der())
          if(i == 9) return(myDer$get_mid_x_from_der())
          if(i == 10) return(myDer$get_der_x())
          if(i == 11) return(myDer$der_x)
          return(-1)
        }
      )
    )
  )

  comp <- nCompile(ncBase, ncMid, ncDer, useClasses)

  # der obj works on its own
  Cder <- comp$ncDer$new()
  Cder$base_x <- 1
  expect_equal(Cder$base_x, 1)
  expect_equal(Cder$get_base_x(), 1)
  expect_equal( Cder$get_base_x_from_mid(),1)
  expect_equal( Cder$get_base_x_from_der(),1)

  Cder$mid_x <- 2
  expect_equal( Cder$mid_x, 2)
  expect_equal( Cder$get_mid_x(), 2)
  expect_equal( Cder$get_mid_x_from_der(), 2)

  Cder$der_x <- 3
  expect_equal (Cder$der_x, 3)
  expect_equal (Cder$get_der_x(), 3)

  expect_equal (Cder$get_x(), 3)
  expect_equal (Cder$get_x_virt(), 3)

  expect_error(Cmid <- comp$ncMid$new())

  # mid object works on its own (even though can't be created from R)
  Cmid <- Cder$make_mid()
  Cmid$base_x <- 111
  Cmid$mid_x <- 222
  expect_equal(c(
    Cmid$base_x
  , Cmid$get_base_x()
  , Cmid$get_base_x_from_mid()), rep(111, 3))

  expect_equal(c(
    Cmid$mid_x
  , Cmid$get_mid_x()
  , Cmid$get_x()), rep(222, 3))

  # base object works on its own
  Cbase <- comp$ncBase$new()
  Cbase$base_x <- 11
  expect_equal(Cbase$get_x_virt(), 11)
  expect_error(Cbase$get_der_x())

  obj <- comp$useClasses$new()
  obj$myBase <- Cbase
  obj$myDer <- Cder

  # base accessing an actual base
  expect_equal(c(
    obj$useBase(1)
    ,obj$useBase(2)
    ,obj$useBase(3)
    ,obj$useBase(4)), rep(11, 4))

  # der accessing an actual der
  expect_equal(c(
    obj$useDer(1)
    ,obj$useDer(2)
    ,obj$useDer(3)
    ,obj$useDer(4)), c(3, 1, 3, 1))

  expect_equal(c(
    obj$useDer(5)
    ,obj$useDer(6)
    ,obj$useDer(7)), c(1, 2, 2))

  expect_equal(c(
    obj$useDer(8)
   ,obj$useDer(9)
   ,obj$useDer(10)
   ,obj$useDer(11)), c(1, 2, 3, 3))

  # base pointing to a der
  obj$myBase <- Cder
  expect_equal(c(
    obj$useBase(1)
    ,obj$useBase(2)
    ,obj$useBase(3)
    ,obj$useBase(4)), c(3,1,1,1))


  # base pointing to a mid
  obj$myBase <- Cmid
  expect_equal(c(
    obj$useBase(1)
   ,obj$useBase(2)
   ,obj$useBase(3)
   ,obj$useBase(4)), c(222,111,111,111))

  # mid pointing to a der
  obj$myMid <- Cder
  expect_equal(c(
    obj$useMid(1)
   ,obj$useMid(2)
   ,obj$useMid(3)
   ,obj$useMid(4)), c(3,1,2,1))

  expect_equal(c(
    obj$useMid(5)
   ,obj$useMid(6)
   ,obj$useMid(7)), c(1, 2, 2))

  rm(Cder, Cmid, Cbase); gc()
})

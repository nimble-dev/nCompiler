context("Testing nClass compilation")

test_that("nCompile_nClass works",
{
    nc1 <- nClass(
        Rpublic = list(
            Rv = NULL,
            Rfoo = function(x) x+1
        ),
        Cpublic = list(
            Cv = 'numericScalar',
            Cfoo = nFunction(
                fun = function(x) {
                    return(x+1)
                },
                argTypes = list(x = 'numericScalar'),
                returnType = 'numericScalar')
        )
    )
#    set_nOption('showCompilerOutput', TRUE)
    ans <- try(nCompile_nClass(nc1, interface = "generic"))
    expect_true(is.function(ans)) ## compilation succeeded
    obj <- ans()
    expect_true(class(obj) == "loadedObjectEnv")
    expect_equal(method(obj, "Cfoo")(1.2), 2.2)
    value(obj, "Cv") <- 1.23
    expect_equal(value(obj, "Cv"), 1.23)
})

test_that("nCompile_nClass works when there are no methods",
          {
            nc1 <- nClass(
              Rpublic = list(
                Rv = NULL,
                Rfoo = function(x) x+1
              ),
              Cpublic = list(
                Cv = 'numericScalar'
              )
            )            
            ans <- try(nCompile_nClass(nc1, interface = "generic"))
            expect_true(is.function(ans)) ## compilation succeeded
            obj <- ans()
            expect_true(class(obj) == "loadedObjectEnv")
            value(obj, "Cv") <- 1.23
            expect_equal(value(obj, "Cv"), 1.23)
          })

test_that("nCompile_nClass works when there are no member data",
          {
            nc1 <- nClass(
              Rpublic = list(
                Rv = NULL,
                Rfoo = function(x) x+1
              ),
              Cpublic = list(
                Cfoo = nFunction(
                  fun = function(x) {
                    return(x+1)
                  },
                  argTypes = list(x = 'numericScalar'),
                  returnType = 'numericScalar')
              )
            )
            ans <- try(nCompile_nClass(nc1, interface = "generic"))
            expect_true(is.function(ans)) ## compilation succeeded
            obj <- ans()
            expect_true(class(obj) == "loadedObjectEnv")
            expect_equal(method(obj, "Cfoo")(1.2), 2.2)
          })

test_that("nCompile_nClass works with one method calling another",
          {
            nc1 <- nClass(
              Rpublic = list(
                Rv = NULL,
                Rfoo = function(x) x+1
              ),
              Cpublic = list(
                Cfoo = nFunction(
                  fun = function(x) {
                    return(x+1)
                  },
                  argTypes = list(x = 'numericScalar'),
                  returnType = 'numericScalar'),
                Cbar = nFunction(
                  fun = function(w) {
                    ans <- Cfoo(w)
                    return(ans)
                  },
                  argTypes = list(w = 'numericScalar'),
                  returnType = 'numericScalar')
              )
            )
            ans <- try(nCompile_nClass(nc1, interface = "generic"))
            expect_true(is.function(ans)) ## compilation succeeded
            obj <- ans()
            expect_true(class(obj) == "loadedObjectEnv")
            expect_equal(method(obj, "Cfoo")(1.2), 2.2)
            expect_equal(method(obj, "Cbar")(1.2), 2.2)
          })

test_that("nCompile_nClass works 2",
{
    nc1 <- nClass(
        Rpublic = list(
            Rv = NULL,
            Rfoo = function(x) x+1
        ),
        Cpublic = list(
            Cs = 'numericScalar',
            Cv = 'numericVector',
            Cx = 'numericMatrix',
            Cfoo = nFunction(
                fun = function(x) {
                    return(x+1)
                },
                argTypes = list(x = 'numericScalar'),
                returnType = 'numericScalar'),
            Cbar = nFunction(
                fun = function(x, y) {
                    return(x + y)
                },
                argTypes = list(x = 'numericMatrix',
                                y = 'numericMatrix'),
                returnType = 'numericMatrix')
        )
    )
    ans <- try(nCompile_nClass(nc1, interface = "generic"))
    expect_true(is.function(ans)) ## compilation succeeded
    obj <- ans()
    expect_true(class(obj)=="loadedObjectEnv")
    expect_equal(method(obj, "Cfoo")(1.2), 2.2)
    x <- matrix(as.numeric(1:6), nrow = 2)
    y <- matrix(as.numeric(101:106), nrow = 2)
    expect_equal(method(obj, "Cbar")(
      x, y
    ),
    x+y)
    
    value(obj, "Cs")
    value(obj, "Cv")
    value(obj, "Cx")
    value(obj, "Cs") <- 10
    expect_equal(value(obj, "Cs"), 10)
    value(obj, "Cv") <- 1:4
    expect_equal(value(obj, "Cv"), array(1:4, dim = 4))
    value(obj, "Cx") <- matrix(1:6, nrow = 3)
    expect_equal(value(obj, "Cx"), matrix(1:6, nrow = 3))
})

test_that("nCompile_nClass works with integerMatrix and logicalMatrix",
          {
            nc1 <- nClass(
              Cpublic = list(
                CIM = 'integerMatrix',
                CLM = 'logicalMatrix',
                CfooIM = nFunction(
                  fun = function(x) {
                    return(x)
                  },
                  argTypes = list(x = 'integerMatrix'),
                  returnType = 'integerMatrix'),
                CfooLM = nFunction(
                  fun = function(x) {
                    return(x)
                  },
                  argTypes = list(x = 'logicalMatrix'),
                  returnType = 'logicalMatrix')
              )
            )
            ans <- try(nCompile_nClass(nc1, interface = "generic"))
            expect_true(is.function(ans)) ## compilation succeeded
            obj <- ans()
            expect_true(class(obj)=="loadedObjectEnv")
            x <- matrix(1L:4L, nrow = 2)
            expect_equal(method(obj, "CfooIM")(x), x)
            expect_equal(value(obj, "CIM"), matrix(0, nrow = 0, ncol = 0))
            value(obj, "CIM") <- matrix(1L:4L, nrow = 2)
            expect_identical(value(obj, "CIM"), matrix(1L:4L, nrow = 2))
            y <- matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2)
            expect_identical(method(obj, "CfooLM")(y), y)
            expect_identical(value(obj, "CLM"), matrix(FALSE, nrow = 0, ncol = 0))
            value(obj, "CLM") <- matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2)
            expect_identical(value(obj, "CLM"), matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2))
          })

test_that("compiling nClass works with nClass members",{
  nc1 <- nClass(
    Cpublic = list(
      v = 'numericVector',
      foo = nFunction(
        fun = function(c = 'numericScalar') {
          return(c * v)
        },
      returnType = 'numericVector')
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      my_nc1 = 'nc1',
      foo = nFunction(
        fun = function(c = 'numericScalar') {
          return(c)
        },
        returnType = 'numericScalar')
    )
  )
  message("nCompile doesn't keep names straight with multiple nClasses.\n")
  test <- nCompile(nc1, nc2)
  obj1 <- test[[1]]()
  obj2 <- test[[2]]()
  Rv <- 1:4
  value(obj2, 'my_nc1') <- obj1
  value(obj1,'v') <- Rv
  expect_equal(value(obj1, 'v'),
               array(Rv))
  expect_equal(value( value(obj2, 'my_nc1'), 'v' ),
               array(Rv))
})

test_that("compiling nClass works with nClass members and methods",{
  nc1 <- nClass(
    Cpublic = list(
      v = 'numericVector',
      foo = nFunction(
        fun = function(c = 'numericScalar') {
          return(c * v)
        },
        returnType = 'numericVector')
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      my_nc1 = 'nc1',
      foo = nFunction(
        fun = function(c = 'numericScalar') {
          ans <- 2 * my_nc1$foo(c)
          return(ans)
        },
        returnType = 'numericVector')
    )
  )
  message("nCompile doesn't keep names straight with multiple nClasses.\n")
  test <- nCompile(nc1, nc2)
  obj1 <- test[[1]]()
  obj2 <- test[[2]]()
  Rv <- 1:4
  value(obj2, 'my_nc1') <- obj1
  value(obj1,'v') <- Rv
  expect_equal(value(obj1, 'v'),
               array(Rv))
  expect_equal(value( value(obj2, 'my_nc1'), 'v' ),
               array(Rv))
  expect_equal(method(obj1, 'foo')(2),
               array(2*Rv))
  expect_equal(method(obj2, 'foo')(2),
               array(2*2*Rv))
})

test_that("compiling nClass works with nClass members and methods",{
  nc1 <- nClass(
    Cpublic = list(
      v = 'numericVector',
      foo = nFunction(
        fun = function(c = 'numericScalar') {
          return(c * v)
        },
        returnType = 'numericVector')
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      my_nc1 = 'nc1',
      take_nc1 = nFunction(
        fun = function(arg_nc1 = 'nc1',
                       c = 'numericScalar') {
          ans <- 2 * arg_nc1$foo(c)
          return(ans)
        },
        returnType = 'numericVector'),
      pass_nc1 = nFunction(
        fun = function(c = 'numericScalar') {
          ans <- 3 * take_nc1(my_nc1, c)
          return(ans)
        },
        returnType = 'numericVector'
      )
    )
  )
  message("nCompile doesn't keep names straight with multiple nClasses.\n")
  test <- nCompile(nc1, nc2)
  obj1 <- test[[1]]()
  obj2 <- test[[2]]()
  value(obj2, 'my_nc1') <- obj1
  Rv <- 1:4
  value(obj1,'v') <- Rv
  expect_equal(value(obj1, 'v'),
               array(Rv))
  expect_equal(value( value(obj2, 'my_nc1'), 'v' ),
               array(Rv))
  expect_equal(method(obj1, 'foo')(2), 
               array(2*Rv))
  expect_equal(method(obj2, 'pass_nc1')(2), 
               array(3*2*2*Rv))
  expect_equal(method(obj2, 'take_nc1')(obj1, 2),
               array(2*2*Rv))
})
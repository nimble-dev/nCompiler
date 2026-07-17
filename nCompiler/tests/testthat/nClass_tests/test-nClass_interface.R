# (These work when run directly, but not when run through test_package().)

test_that(
  "Basic and full interfaces work",
  {
    nc1 <- nClass(
      Rpublic = list(
        Rv = NULL,
        Rfoo = function(x) x+1
      ),
      Cpublic = list(
        Cv = 'numericScalar',
        Ca = 'numericVector',
        Cfoo = nFunction(
          fun = function(x) {
            return(x+1)
          },
          argTypes = list(x = 'numericScalar'),
          returnType = 'numericScalar')
      )
    )
#    ans <- nCompile_nClass(nc1, interface = "generic")
    ans <- nCompile(nc1, interfaces = "generic")
    obj <- ans()
    value(obj, 'Cv') <- 2.3
    check <- value(obj, 'Cv')
    expect_equal(check, 2.3, info = "scalar value() and `value<-()`")

    value(obj, 'Ca') <- c(2.3, 3.4, 4.5)
    check <- value(obj, 'Ca')
    expect_equal(check, c(2.3, 3.4, 4.5), info = "vector value() and `value()<-`")

    check <- method(obj, 'Cfoo')(3.4)
    expect_equal(check, 4.4, info = "method()")

    nc1c <- build_compiled_nClass(nc1)
    test <- nc1c$new(obj)
    test$Cv <- 5.6
    check <- test$Cv
    expect_equal(check, 5.6, info = "full interface active binding")
    test$Rv <- 6.7
    check <- test$Rv
    expect_equal(check, 6.7, info = "full interface R field")
    check <- test$Rfoo(7.8)
    expect_equal(check, 8.8, info = "full interface R method")
    check <- test$Cfoo(8.9)
    expect_equal(check, 9.9, info = "full interface R method")
  })

test_that(
  "interface = \"full\" works",
  {
    nc1 <- nClass(
      Rpublic = list(
        Rv = NULL,
        Rfoo = function(x) x+1
      ),
      Cpublic = list(
        Cv = 'numericScalar',
        Ca = 'numericVector',
        Cfoo = nFunction(
          fun = function(x) {
            return(x+1)
          },
          argTypes = list(x = 'numericScalar'),
          returnType = 'numericScalar')
      )
    )
#    ans <- nCompile_nClass(nc1, interface = "full")
    ans <- nCompile(nc1, interfaces = "full")
    expect_true(isCompiledNCgenerator(ans))
    obj <- ans$new()
    expect_true(inherits(obj, "nClass"))
    obj$Cv <- 2.3
    check <- obj$Cv
    expect_equal(check, 2.3, info = "scalar set and get from a full interface")

    obj$Ca <- c(2.3, 3.4, 4.5)
    check <- obj$Ca
    expect_equal(check, c(2.3, 3.4, 4.5), info = "vector set and get from a full interface")

    check <- obj$Cfoo(3.4)
    expect_equal(check, 4.4, info = "method from a full interface")
  })

test_that("getting a generic interface pointer within C++ works", {
  nc1 <- nList("integerVector()")
  nc2 <- nClass(
    classname = "nc2",
    Cpublic = list(
      nc1obj = "nc1()",
      x = "integerVector",
      nc2 = nFunction(
        function() {
          # The following is not (yet) supported
          # nc1obj <- nList("integerVector()")$new()
          nc1obj <- nc1$new()
        },
        compileInfo = list(constructor = TRUE)
      )
    )
  )
  nc3 <- nClass(
    classname = "nc3",
    Cpublic = list(
      nc2obj = "nc2()",
      nc1obj = "nc1()",
      check1 = nFunction(
        function() {
          cppLiteral("auto basePtr = this->get_interface_ptr(\"nc2obj\")")
          #derivedPtr <- nc2$new()
          cppLiteral("derivedPtr = std::dynamic_pointer_cast<nc2>(basePtr);",
                     types = list(derivedPtr = "nc2()"))
          ans <- derivedPtr$x
          return(ans)
          returnType("integerVector")
        }
      ),
      check2 = nFunction(
        function() {
          cppLiteral("auto basePtr = this->get_interface_ptr(\"nc1obj\")")
          cppLiteral("nListBasePtr = std::dynamic_pointer_cast<nListBase_nClass>(basePtr);",
                     types = list(nListBasePtr = "nListBase_nClass()"))
          cppLiteral("bool ok; if(nListBasePtr) {ok=true;} else {ok=false;} ")
          cppLiteral("std::cout<< ok << std::endl;")
          length(nListBasePtr) <- 3
        }
      ),
      check3 = nFunction(
        function() {
          cppLiteral("auto basePtr = nc2obj->get_interface_ptr(\"nc1obj\")")
          cppLiteral("nListBasePtr = std::dynamic_pointer_cast<nListBase_nClass>(basePtr);",
                     types = list(nListBasePtr = "nListBase_nClass()"))
          cppLiteral("{bool ok; if(nListBasePtr) {ok=true;} else {ok=false;} ")
          cppLiteral("std::cout<< ok << std::endl;}")

          length(nListBasePtr) <- 2
          # N.B. The name "nList_I1" can be found from the NCinternals of nc1.
          # It is the classID.
          cppLiteral("nListDerPtr = std::dynamic_pointer_cast<nList_I1>(nListBasePtr);",
                      types = list(nListDerPtr = "nc1()"))
          cppLiteral("bool ok; if(nListDerPtr) {ok=true;} else {ok=false;} ")
          cppLiteral("std::cout<<ok<<std::endl")
          nListDerPtr[[1]] <- 1L:3L
        }
      ),
      nc3 = nFunction(
        function() {
          nc2obj <- nc2$new()
          nc1obj <- nc1$new()
        },
        compileInfo = list(constructor = TRUE)
      )
    )
  )

  comp <- nCompile(nc3, nc1, nc2)
  obj <- comp$nc3$new()
  # obj$nc2obj
  obj$nc2obj$x <- 1:3
  expect_equal(obj$check1(), 1:3)
  obj$check2()
  expect_equal(obj$nc1obj |> as.list() |> length(), 3)
  obj$check3()
  expect_equal(obj$nc2obj$nc1obj |> as.list() |> length(), 2)
  expect_equal(obj$nc2obj$nc1obj[[1]], 1L:3L)
})

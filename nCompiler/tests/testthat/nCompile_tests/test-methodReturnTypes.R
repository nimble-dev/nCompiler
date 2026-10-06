#
test_that("TBD types are handled for an nClass returned by another class' method", {

  innerNC <- nClass(
    classname = "innerNC",
    Cpublic = list(x = double(1))
  )
  # set up so f1 and f2 have the same return type by different names
  make_f1 <- function(innerNC) {
    innerNC2 <- innerNC
    f1 <- nFunction(
      fun = function() {
        ans <- innerNC2$new()
        ans$x <- 1:3
        return(ans)
        returnType(innerNC2())
      }
    )
    f1
  }
  f1 <- make_f1(innerNC)

  # set up so nc$foo and f2 also have the same return type by different names
  make_nc <- function(innerNC) {
    innerNC3 <- innerNC
    nc <- nClass(
      classname = "nc",
      Cpublic = list(
        foo = nFunction(
          function() {
            ans <- innerNC3$new()
            ans$x <- 4:6
            return(ans)
            returnType(innerNC3())
          }
        )
      )
    )
    nc
  }
  nc <- make_nc(innerNC)

  f2 <- nFunction(
    fun = function() {
      ans <- f1()
      return(ans)
      returnType(innerNC())
    }
  )

  comp <- nCompile(f2, nc)
  expect_equal(comp$f2()$x, 1:3)
  expect_equal(comp$nc$new()$foo()$x, 4:6)
})

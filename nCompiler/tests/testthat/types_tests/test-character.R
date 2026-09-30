# A separate file for character types since they are distinct and implemented as a separate unit of work.

test_that("character tensors work", {
  nc <- nClass(
    Cpublic = list(
      cs = "character(0)",
      cv = "character(1)",
      cm = "nMatrix(type = 'character')",
      ca = "nArray(type = 'character', nDim = 3)",
      ## ca = "array(type = 'character', nDim = 3)",
      test_cs = nFunction(
        function(x = "character()") {
          y <- "hw"
          z <- x
          y <- z
          self$cs <- y
          return(y)
          returnType(character())
        }
      ),
      test_cv = nFunction(
        function(x = "character(1)") {
          y <- character()
          z <- x
          w <- c(x[2], "raw", x[1])
          z <- c(x[2], "raw", x[1])
          a <- z[2:3]
          setSize(y, 3) #setSize but not `length<-` is consistent uncompiled vs compiled, for new values
          y[1:2] <- z[2:3]
          y[2] <- "yay"
          self$cv <- y
          return(y)
          returnType(character(1))
        }
      ),
      test_cm = nFunction(
        function(x = "characterMatrix()") {
          y <- matrix(type = "character", nrow = 2, ncol = 3)
          z <- x
          a <- z[1:2, 2:3]
          y[1, 1:3] <- z[2, 1:3]
          y[2, 2] <- "yay"
          self$cm <- y
          return(y)
          returnType(nMatrix(type = "character"))
        }
      ),
      test_ca = nFunction(
        function(x = "characterArray(nDim = 3)") {
          D <- c(2, 3, 4)
          y <- array(type = "character", dim = D, nDim = 3)
          z <- x
          a <- z[1:2, 2:3, 3:4]
          y[1, 2:3, 3:4] <- z[2, 1:2, 2:3]
          y[2, 2, 2] <- "yay"
          self$ca <- y
          return(y)
          returnType(nArray(type = "character", nDim = 3))
        }
      )
    )
  )

  comp <- nCompile(nc)

  obj <- nc$new()
  Cobj <- comp$new()
  expect_equal(obj$test_cs("bar"), "bar")
  expect_equal(obj$cs, "bar")
  obj$cs <- "foo"
  expect_equal(obj$cs, "foo")
  expect_equal(Cobj$test_cs("bar"), "bar")
  expect_equal(obj$test_cs("bar"), "bar")
  expect_equal(Cobj$cs, "bar")
  expect_equal(obj$cs, "bar")
  Cobj$cs <- "foo"
  expect_equal(Cobj$cs, "foo")

  cv <- c("a", "b")
  cv2 <- c("raw", "yay", "")
  expect_equal(obj$test_cv(cv), cv2)
  expect_equal(obj$cv, cv2)
  obj$cv <- letters[3:4]
  expect_equal(obj$cv, letters[3:4])
  expect_equal(Cobj$test_cv(cv), cv2)
  expect_equal(obj$test_cv(cv), cv2)
  expect_equal(Cobj$cv, cv2)
  expect_equal(obj$cv, cv2)
  Cobj$cv <- letters[3:4]
  expect_equal(Cobj$cv, letters[3:4])

  cm <- matrix(LETTERS[1:6], nrow = 2)
  cm2 <- nMatrix(type = "character", nrow = 2, ncol = 3)
  cm2[1, 1:3] <- cm[2, 1:3]
  cm2[2, 2] <- "yay"
  expect_equal(obj$test_cm(cm), cm2)
  expect_equal(Cobj$test_cm(cm), cm2)
  expect_equal(Cobj$cm, cm2)
  expect_equal(obj$cm, cm2)

  ca <- array(LETTERS[1:(2*3*4)], dim = c(2, 3, 4))
  ca2 <- nArray(type = "character", dim = c(2, 3, 4))
  ca2[1, 2:3, 3:4] <- ca[2, 1:2, 2:3]
  ca2[2,2,2] <- "yay"
  expect_equal(obj$test_ca(ca), ca2)
  expect_equal(Cobj$test_ca(ca), ca2)
  expect_equal(obj$ca, ca2)
  expect_equal(Cobj$ca, ca2)

  rm(obj); gc()
})

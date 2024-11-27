# NOT WORKING

context("Testing seq and colon operators")

test_that("Basic colon usage works", {
  nc <- nClass(
    Cpublic = list(
      colon_dd = nFunction(
        function(x = numericScalar, y = numericScalar) {
          ans <- x:y
          return(ans)
          returnType(numericVector)
        }
      ),
      colon_ii = nFunction(
        function(x = integerScalar, y = integerScalar) {
          ans <- x:y
          return(ans)
          returnType(integerVector)
        }
      ),
      colon_di = nFunction(
        function(x = numericScalar, y = integerScalar) {
          ans <- x:y
          return(ans)
          returnType(numericVector)
        }
      ),
      colon_id = nFunction(
        function(x = integerScalar, y = numericScalar) {
          ans <- x:y
          return(ans)
          returnType(integerVector)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()
  expect_identical(nco$colon_dd(2, 7), as.numeric(2:7))
  expect_identical(nco$colon_dd(2.1, 7), 2.1:7)
  expect_identical(nco$colon_ii(2, 7), 2:7)
  expect_identical(nco$colon_di(2.1, 7), 2.1:7)
  expect_identical(nco$colon_id(2, 7.1), 2:7.1)
})

test_that("Basic seq usage works", {
  nc <- nClass(
    Cpublic = list(
      ## Empty args
      seq_empty = nFunction(
        function() {
          ans <- 1 + exp(seq())
          return(ans)
          returnType(numericVector)
        }
      ),
      ## seq(to)
      seq_td = nFunction(
        function(x = numericScalar) {
          ans <- 1 + exp(seq(to = 2*x))
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_ti = nFunction(
        function(x = integerScalar) {
          ans <- seq(to = x)
          return(ans)
          returnType(integerVector)
        }
      ),
      ## seq(from) scalar
      seq_fd = nFunction(
        function(x = numericScalar) {
          ans <- 1 + exp(seq(from = 2*x))
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fi = nFunction(
        function(x = integerScalar) {
          ans <- seq(from = x)
          return(ans)
          returnType(integerVector)
        }
      ),
      ## seq(from) vector
      seq_fdv = nFunction(
        function(x = numericVector) {
          ans <- 1 + exp(seq(from = 2*x[1:3])) # should use length of vector
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fiv = nFunction(
        function(x = integerVector) {
          ans <-  seq(from = x) # should use length of vector
          return(ans)
          returnType(integerVector)
        }
      ),
      ## seq(from, to)
      seq_fd_td = nFunction(
        function(x = numericScalar, y = numericScalar) {
          ans <- seq(x, y)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fi_td = nFunction(
        function(x = integerScalar, y = numericScalar) {
          ans <- seq(x, y)
          return(ans)
          returnType(integerVector)
        }
      ),
      seq_fi_ti = nFunction(
        function(x = integerScalar, y = integerScalar) {
          ans <- seq(x, y)
          return(ans)
          returnType(integerVector)
        }
      ),
      seq_fd_ti = nFunction(
        function(x = numericScalar, y = integerScalar) {
          ans <- seq(x, y)
          return(ans)
          returnType(numericVector)
        }
      ),
      # seq(length.out)
      seq_ld = nFunction(
        function(x = numericScalar) {
          ans <- seq(length.out = x)
          return(ans)
          returnType(integerVector)
        }
      ),
      ## seq(from, to, by) from default ordering
      seq_fd_td_bd = nFunction(
        function(x = numericScalar, y = numericScalar, z = numericScalar) {
          ans <- seq(x, y, z)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fi_ti_bi = nFunction(
        function(x = integerScalar, y = integerScalar, z = integerScalar) {
          ans <- seq(x, y, z)
          return(ans)
          returnType(integerVector)
        }
      ),
      seq_fi_td_bd = nFunction(
        function(x = integerScalar, y = numericScalar, z = numericScalar) {
          ans <- seq(x, y, z)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fd_ti_bd = nFunction(
        function(x = numericScalar, y = integerScalar, z = numericScalar) {
          ans <- seq(x, y, z)
          return(ans)
          returnType(numericVector)
        }
      ),
      ## seq(by)
      seq_bd = nFunction(
        function(x = numericScalar) {
          ans <- seq(by = x)
          return(ans)
          returnType(integerVector)
        }
      ),
      ## seq(from, by)
      seq_fd_bd = nFunction(
        function(x = numericScalar, by = numericScalar) {
          ans <- seq(from = x, by = by)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fi_bi = nFunction(
        function(x = integerScalar, by = integerScalar) {
          ans <- seq(from = x, by = by)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fi_bd = nFunction(
        function(x = integerScalar, by = numericScalar) {
          ans <- seq(from = x, by = by)
          return(ans)
          returnType(numericVector)
        }
      ),
      ## seq(from, length.out)
      seq_fd_ld = nFunction(
        function(x = numericScalar, len = numericScalar) {
          ans <- seq(from = x, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fi_li = nFunction(
        function(x = integerScalar, len = integerScalar) {
          ans <- seq(from = x, length.out = len)
          return(ans)
          returnType(integerVector)
        }
      ),
      seq_fi_ld = nFunction(
        function(x = integerScalar, len = numericScalar) {
          ans <- seq(from = x, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      ## seq(to = , length.out)
      seq_td_ld = nFunction(
        function(x = numericScalar, len = numericScalar) {
          ans <- seq(to = x, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_ti_ld = nFunction(
        function(x = integerScalar, len = numericScalar) {
          ans <- seq(to = x, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_td_li = nFunction(
        function(x = numericScalar, len = integerScalar) {
          ans <- seq(to = x, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_ti_li = nFunction(
        function(x = integerScalar, len = integerScalar) {
          ans <- seq(to = x, length.out = len)
          return(ans)
          returnType(integerVector)
        }
      ),
      ## seq(to = , by)
      seq_td_bd = nFunction(
        function(x = numericScalar, by = numericScalar) {
          ans <- seq(to = x, by = by)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_ti_bd = nFunction(
        function(x = integerScalar, by = numericScalar) {
          ans <- seq(to = x, by = by)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_td_bi = nFunction(
        function(x = numericScalar, by = integerScalar) {
          ans <- seq(to = x, by = by)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_ti_bi = nFunction(
        function(x = integerScalar, by = integerScalar) {
          ans <- seq(to = x, by = by)
          return(ans)
          returnType(numericVector)
        }
      ),
      ## seq(by, length)
      seq_bd_ld = nFunction(
        function(x = numericScalar, len = numericScalar) {
          ans <- seq(by = x, length = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_bi_ld = nFunction(
        function(x = integerScalar, len = numericScalar) {
          ans <- seq(by = x, length = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_bd_li = nFunction(
        function(x = numericScalar, len = integerScalar) {
          ans <- seq(by = x, length = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_bi_li = nFunction(
        function(x = integerScalar, len = integerScalar) {
          ans <- seq(by = x, length = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      ## seq(from, to, length)
      seq_fd_td_ld = nFunction(
        function(x = numericScalar, to = numericScalar, len = numericScalar) {
          ans <- seq(from = x, to = to, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fi_td_ld = nFunction(
        function(x = integerScalar, to = numericScalar, len = numericScalar) {
          ans <- seq(from = x, to = to, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fd_ti_ld = nFunction(
        function(x = numericScalar, to = integerScalar, len = numericScalar) {
          ans <- seq(from = x, to = to, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_fi_ti_ld = nFunction(
        function(x = integerScalar, to = integerScalar, len = numericScalar) {
          ans <- seq(from = x, to = to, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      ## seq(from, by, length)
      seq_fd_bd_ld = nFunction(
        function(from = numericScalar, by = numericScalar, len = numericScalar) {
          ans <- seq(from = from, by = by, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      ## seq(from, by, length)
      seq_fi_bd_ld = nFunction(
        function(from = integerScalar, by = numericScalar, len = numericScalar) {
          ans <- seq(from = from, by = by, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      ## seq(from, by, length)
      seq_fd_bi_ld = nFunction(
        function(from = numericScalar, by = integerScalar, len = numericScalar) {
          ans <- seq(from = from, by = by, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      ## seq(from, by, length)
      seq_fi_bi_ld = nFunction(
        function(from = integerScalar, by = integerScalar, len = numericScalar) {
          ans <- seq(from = from, by = by, length.out = len)
          return(ans)
          returnType(integerVector)
        }
      ),
      ## seq(to, by, length)
      seq_td_bd_ld = nFunction(
        function(to = numericScalar, by = numericScalar, len = numericScalar) {
          ans <- seq(to = to, by = by, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_ti_bd_ld = nFunction(
        function(to = integerScalar, by = numericScalar, len = numericScalar) {
          ans <- seq(to = to, by = by, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_td_bi_ld = nFunction(
        function(to = numericScalar, by = integerScalar, len = numericScalar) {
          ans <- seq(to = to, by = by, length.out = len)
          return(ans)
          returnType(numericVector)
        }
      ),
      seq_ti_bi_ld = nFunction(
        function(to = integerScalar, by = integerScalar, len = numericScalar) {
          ans <- seq(to = to, by = by, length.out = len)
          return(ans)
          returnType(integerVector)
        }
      )
    )
  )

  ## nc <- nClass(
  ##   Cpublic = list(
  ##     ## individual case goes here for building up tests
  ##   )
  ## )

  ## The two most useful debug spots:
  ## debug(nCompiler:::labelAbstractTypesEnv$Seq)
  ## debug(nCompiler:::eigenizeEnv$Seq)
  ncc <- nCompile(nc)

  nc_obj <- ncc$new()
  expect_equivalent(nc_obj$seq_empty(), 1 + exp(1))
  expect_equivalent(nc_obj$seq_td(7), 1+exp(seq(to = 2*7)))
  expect_identical(nc_obj$seq_ti(7), seq(to = 7L))
  expect_equivalent(nc_obj$seq_fd(5), 1 + exp(1:10))
  expect_identical(nc_obj$seq_fi(7), seq(from = 7L))
  expect_equivalent(nc_obj$seq_fd(-5), 1 + exp(seq(from = 2*(-5))))

  expect_equivalent(nc_obj$seq_fdv(5:10), 1 + exp(1:3))
  expect_identical(nc_obj$seq_fiv(5:10), seq(5L:10L))

  expect_equivalent(nc_obj$seq_fd_td(7, 20), seq(7, 20))
  expect_equivalent(nc_obj$seq_fd_td(7.1, 20.2), seq(7.1, 20.2))
  expect_equivalent(nc_obj$seq_fd_td(7.1, 20.2), seq(7.1, 20.2))
  expect_equivalent(nc_obj$seq_fd_td(-1, -5), seq(-1, -5))
  expect_equivalent(nc_obj$seq_fd_td(-1, -1), seq(-1, -1))

  expect_identical(nc_obj$seq_fi_td(1, 5.2), seq(from=1L, 5.2))
  expect_identical(nc_obj$seq_fi_ti(1, 5), seq(from=1L, 5L))
  expect_identical(nc_obj$seq_fd_ti(1.1, 5), seq(from=1.1, 5L))

  expect_equivalent(nc_obj$seq_ld(4), 1:4)
  expect_equivalent(nc_obj$seq_ld(4.1), 1:5)
  expect_equivalent(nc_obj$seq_ld(0), integer())
  expect_equivalent(nc_obj$seq_ld(.1), 1)

  expect_equivalent(nc_obj$seq_fd_td_bd(0, 1, .1), seq(0, 1, .1))
  expect_equivalent(nc_obj$seq_fd_td_bd(1, -1, -.1), seq(1, -1, -.1))
  expect_equivalent(nc_obj$seq_fd_td_bd(1.1, -1.3, -.1), seq(1.1, -1.3, -.1))
  expect_equivalent(nc_obj$seq_fd_td_bd(1, 10, 1), seq(1, 10, 1))
#  expect_equivalent(nc_obj$seq_fd_td_bd(1, 10, -1), seq(1, 10, -1)) # Correctly generates error
  expect_equivalent(nc_obj$seq_fd_td_bd(1, 1, 1), seq(1, 1, 1))
  expect_equivalent(nc_obj$seq_fd_td_bd(1, 1, .1), seq(1, 1, .1))
  expect_equivalent(nc_obj$seq_fd_td_bd(1, 1, 0), seq(1, 1, 0))

  expect_identical(nc_obj$seq_fi_ti_bi(1, 10, 2), seq(1L, 10L, 2L))
  expect_identical(nc_obj$seq_fi_td_bd(1, 10, 2), seq(1L, 10, 2))
  expect_identical(nc_obj$seq_fd_ti_bd(1, 10, 2), seq(1, 10L, 2))

  expect_equivalent(nc_obj$seq_bd(1), seq(by = 1))
  expect_equivalent(nc_obj$seq_bd(0), seq(by = 0))
  expect_equivalent(nc_obj$seq_bd(0.5), seq(by = 0.5))
  expect_equivalent(nc_obj$seq_bd(-.5), seq(by = -0.5))

  expect_equivalent(nc_obj$seq_fd_bd(1, 1), seq(from = 1, by = 1))
  expect_equivalent(nc_obj$seq_fd_bd(0, .1), seq(from = 0, by = .1))
  expect_equivalent(nc_obj$seq_fd_bd(2, -.1), seq(from = 2, by = -.1))
#  expect_equivalent(nc_obj$seq_fd_bd(2, 0), seq(from = 2, by = 0)) # Correctly gives error msg
  expect_equivalent(nc_obj$seq_fd_bd(1.5, -.1), seq(from = 1.5, by = -.1))
  expect_equivalent(nc_obj$seq_fd_bd(1,0), seq(from = 1, by = 0))
  expect_equivalent(nc_obj$seq_fd_bd(2, -.4), seq(from = 2, by = -.4))
  expect_equivalent(nc_obj$seq_fd_bd(2, -4), seq(from = 2, by = -4))

  expect_identical(nc_obj$seq_fi_bi(2, -4), seq(from = 2L, by = -4L))
  expect_identical(nc_obj$seq_fi_bd(2, -4), seq(from = 2L, by = -4))

  expect_equivalent(nc_obj$seq_fd_ld(1.5, 10), seq(from = 1.5, length.out = 10))
  expect_equivalent(nc_obj$seq_fd_ld(1.5, 0), seq(from = 1.5, length.out = 0))
  expect_equivalent(nc_obj$seq_fd_ld(1.5, 1.1), seq(from = 1.5, length.out = 1.1))
  expect_equivalent(nc_obj$seq_fd_ld(2, 10), seq(from = 2, length.out = 10))
  expect_equivalent(nc_obj$seq_fd_ld(-1, 0), seq(from = -1, length.out = 0))
  expect_equivalent(nc_obj$seq_fd_ld(-2, 1.1), seq(from = -2, length.out = 1.1))

  expect_identical(nc_obj$seq_fi_ld(2, 10), seq(from = 2L, length.out = 10))
  expect_equivalent(nc_obj$seq_fi_ld(-1, 0), seq(from = -1L, length.out = 0)) # R returns integer based on length.out value; we can't do that.
  expect_identical(nc_obj$seq_fi_ld(-2, 1.1), seq(from = -2L, length.out = 1.1))

  expect_equivalent(nc_obj$seq_td_ld(1.5, 10), seq(to = 1.5, length.out = 10))
  expect_equivalent(nc_obj$seq_td_ld(1, 10), seq(to = 1, length.out = 10))
  expect_equivalent(nc_obj$seq_td_ld(5, 1), seq(to = 5, length.out = 1))
  expect_equivalent(nc_obj$seq_td_ld(5, 0), seq(to = 5, length.out = 0))
  #  expect_equivalent(nc_obj$seq_td_ld(5, -1), seq(to = 5, length.out = -1)) # Correctly gives error msg
  expect_identical(nc_obj$seq_ti_ld(5, 3), seq(to = 5L, length.out = 3))
  expect_identical(nc_obj$seq_td_li(5, 3), seq(to = 5, length.out = 3L))
  expect_identical(nc_obj$seq_ti_li(5, 3), seq(to = 5L, length.out = 3L))

  expect_equivalent(nc_obj$seq_td_bd(3, 1), seq(to = 3, by = 1))
  expect_equivalent(nc_obj$seq_td_bd(3, .1), seq(to = 3, by = .1))
  #expect_equivalent(nc_obj$seq_td_bd(3, -.1), seq(to = 3, by = -.1)) # Correctly gives error msg
#  expect_equivalent(nc_obj$seq_td_bd(-1.5, .1), seq(to = -1.5, by = .1)) # Correctly gives error msg
  expect_equivalent(nc_obj$seq_td_bd(-1.5, -.1), seq(to = -1.5, by = -.1))
#  expect_equivalent(nc_obj$seq_td_bd(-1.5, 0), seq(to = -1.5, by = 0)) # Correctly gives error msg
  expect_identical(nc_obj$seq_ti_bd(3L, 1), seq(to = 3L, by = 1))
  expect_identical(nc_obj$seq_td_bi(3, 1L), seq(to = 3, by = 1L))
  expect_identical(nc_obj$seq_ti_bi(3L, 1L), seq(to = 3L, by = 1L))

  expect_equivalent(nc_obj$seq_bd_ld(3, 5), seq(by = 3, length.out = 5))
  expect_equivalent(nc_obj$seq_bd_ld(3, 1), seq(by = 3, length.out = 1))
  expect_equivalent(nc_obj$seq_bd_ld(3, 0), seq(by = 3, length.out = 0))
  expect_equivalent(nc_obj$seq_bd_ld(-3, 5), seq(by = -3, length.out = 5))
  expect_equivalent(nc_obj$seq_bd_ld(-3, 1), seq(by = -3, length.out = 1))
  expect_equivalent(nc_obj$seq_bd_ld(-3, 0), seq(by = -3, length.out = 0))
#  expect_equivalent(nc_obj$seq_bd_ld(-3, -1), seq(by = -3, length.out = -1)) # Correctly gives error msg
  expect_identical(nc_obj$seq_bi_ld(3L, 5), seq(by = 3L, length.out = 5))
  expect_identical(nc_obj$seq_bd_li(3, 5L), seq(by = 3, length.out = 5L))
  expect_identical(nc_obj$seq_bi_li(3L, 5L), seq(by = 3L, length.out = 5L))

  expect_equivalent(nc_obj$seq_fd_td_ld(10, -1, 12), seq(10, -1, length.out = 12 ))
  expect_equivalent(nc_obj$seq_fd_td_ld(-4, 8, 5.6), seq(-4, 8, length.out = 5.6 ))

  expect_identical(nc_obj$seq_fi_td_ld(10L, -1, 12), seq(10L, -1, length.out = 12 ))
  expect_identical(nc_obj$seq_fd_ti_ld(10, -1L, 12), seq(10, -1L, length.out = 12 ))
  expect_identical(nc_obj$seq_fi_ti_ld(10L, -1L, 12), seq(10L, -1L, length.out = 12 ))

  expect_equivalent(nc_obj$seq_fd_bd_ld(10, -.4, 12), seq(10, by=-.4, length.out = 12 ))
  expect_equivalent(nc_obj$seq_fd_bd_ld(10, -.4, 1), seq(10, by=-.4, length.out = 1 ))
  expect_equivalent(nc_obj$seq_fd_bd_ld(10, -.4, 0), seq(10, by=-.4, length.out = 0 ))

  expect_equivalent(nc_obj$seq_fi_bd_ld(10L, -.4, 12), seq(10L, by=-.4, length.out = 12 ))
  expect_equivalent(nc_obj$seq_fd_bi_ld(10, -6L, 12), seq(10, by= -6L, length.out = 12 ))
  expect_equivalent(nc_obj$seq_fi_bi_ld(10L, -6L, 12), seq(10L, by=-6L, length.out = 12 ))

  expect_equivalent(nc_obj$seq_td_bd_ld(10, -.4, 5), seq(to = 10, by=-.4, length.out = 5 ))
  expect_equivalent(nc_obj$seq_td_bd_ld(10, -.4, 1), seq(to = 10, by=-.4, length.out = 1 ))
  expect_equivalent(nc_obj$seq_td_bd_ld(10, -.4, 1.1), seq(to = 10, by=-.4, length.out = 1.1 ))
  expect_equivalent(nc_obj$seq_td_bd_ld(10, -.4, 0), seq(to = 10, by=-.4, length.out = 0 ))

  expect_equivalent(nc_obj$seq_ti_bd_ld(10L, -.4, 5), seq(to = 10L, by=-.4, length.out = 5 ))
  expect_equivalent(nc_obj$seq_td_bi_ld(10, -2L, 5), seq(to = 10, by=-2L, length.out = 5 ))
  expect_equivalent(nc_obj$seq_ti_bi_ld(10L, -2L, 5), seq(to = 10L, by=-2L, length.out = 5 ))
})

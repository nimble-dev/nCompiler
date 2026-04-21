# Tests for nList2 — the new higher-level nList2 implementation.
#
# Tests are in 3 sections
# Handling of uncompiled objects from R
# Handling of compiled objects from R
# Handling of compiled objects within compiled nFunctions

# Build an nList2 and compile it
rNL <- nList2("numericScalar")
cNL <- nCompile(rNL)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_uncompiled <- function() {
  obj <- rNL$new()
  length(obj) <- 4
  for(i in 1:4) obj[[i]] <- i * 10.0
  obj
}

make_compiled <- function() {
  obj <- cNL$new()
  obj$setLength(4L)
  for(i in 1:4) obj[[i]] <- i * 10.0
  obj
}

# ---------------------------------------------------------------------------
# Uncompiled: length
# ---------------------------------------------------------------------------

test_that("nList2 uncompiled: length, setLength, length<-", {
  obj <- rNL$new()
  expect_equal(length(obj), 0)
  length(obj) <- 4
  expect_equal(length(obj), 4)
  length(obj) <- 2
  expect_equal(length(obj), 2)
  obj$setLength(6L)
  expect_equal(obj$getLength(), 6L)
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Uncompiled: double-bracket get / set
# ---------------------------------------------------------------------------

test_that("nList2 uncompiled: [[ get and set", {
  obj <- rNL$new()
  length(obj) <- 3
  obj[[1]] <- 1.1
  obj[[2]] <- 2.2
  obj[[3]] <- 3.3
  expect_equal(obj[[1]], 1.1)
  expect_equal(obj[[2]], 2.2)
  expect_equal(obj[[3]], 3.3)
  obj[[2]] <- 99.9
  expect_equal(obj[[2]], 99.9)
  rm(obj); gc()
})

test_that("nList2 uncompiled: [[ get gives and error and set expands", {
  obj <- rNL$new()
  expect_error(obj[[1]])
  obj[[2]] <- 2.2
  expect_equal(obj[[1]], NULL)
  expect_equal(obj[[2]], 2.2)
  rm(obj); gc()
})

test_that("nList2 uncompiled: [[ set chains correctly", {
  obj <- rNL$new()
  obj[[2]] <- 2.2
  A <- list(obj = obj)
  A$obj[[1]] <- 1.1
  expect_equal(A$obj[[1]], 1.1)
  expect_equal(A$obj[[2]], 2.2)
  rm(obj); gc()
})



# ---------------------------------------------------------------------------
# Uncompiled: single-bracket get
# ---------------------------------------------------------------------------

test_that("nList2 uncompiled: [ get with integer indices", {
  obj <- make_uncompiled()
  expect_equal(as.list(obj[c(1L, 3L)]),     list(10.0, 30.0))
  expect_equal(as.list(obj[c(4L, 2L, 1L)]), list(40.0, 20.0, 10.0))
  rm(obj); gc()
})

test_that("nList2 uncompiled: [ get with numeric indices", {
  obj <- make_uncompiled()
  expect_equal(as.list(obj[c(2.0, 4.0)]), list(20.0, 40.0))
  rm(obj); gc()
})

test_that("nList2 uncompiled: [ get with logical indices", {
  obj <- make_uncompiled()
  expect_equal(as.list(obj[c(TRUE, FALSE, TRUE, FALSE)]), list(10.0, 30.0))
  rm(obj); gc()
})

test_that("nList2 uncompiled: [ get errors for bad indices", {
  obj <- make_uncompiled()
  expect_error(obj[5])
  expect_error(obj[0])
  expect_error(obj[rep(TRUE, 5)])
  rm(obj); gc()
})


# ---------------------------------------------------------------------------
# Uncompiled: single-bracket set with list values
# ---------------------------------------------------------------------------

test_that("nList2 uncompiled: [<- with list values", {
  obj <- rNL$new()
  length(obj) <- 4
  obj[c(1L, 3L)] <- list(100.0, 300.0)
  expect_equal(obj[[1]], 100.0)
  expect_equal(obj[[3]], 300.0)
  rm(obj); gc()
})

test_that("nList2 uncompiled: [<- with list, recycling rule", {
  obj <- rNL$new()
  length(obj) <- 4
  obj[1:4] <- list(1.0, 2.0)   # 2 values recycled over 4 positions
  expect_equal(obj[[1]], 1.0)
  expect_equal(obj[[2]], 2.0)
  expect_equal(obj[[3]], 1.0)
  expect_equal(obj[[4]], 2.0)
  rm(obj); gc()
})

test_that("nList2 uncompiled: [<- extends contents when necessary", {
  obj <- rNL$new()
  obj[5] <- 5.5
  expect_equal(length(obj), 5)
  expect_equal(obj[[5]], 5.5)
  expect_equal(obj[1:4] |> as.list(), vector(mode="list", 4))
  obj[c(4, 6)] <- c(4.4, 6.6)
  expect_equal(length(obj), 6)
  rm(obj); gc()
})

test_that("nList2 uncompiled: [<- chains correctly", {
  obj <- rNL$new()
  obj[1] <- 5.5
  A <- list(obj = obj)
  A$obj[2] <- 6.6
  expect_equal(length(A$obj), 2)
  expect_equal(A$obj[[2]], 6.6)
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Uncompiled: single-bracket set with a single scalar value (recycling)
# ---------------------------------------------------------------------------

test_that("nList2 uncompiled: [<- with single scalar value", {
  obj <- rNL$new()
  length(obj) <- 4
  obj[c(1L, 2L, 3L)] <- 7.7
  expect_equal(obj[[1]], 7.7)
  expect_equal(obj[[2]], 7.7)
  expect_equal(obj[[3]], 7.7)
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Uncompiled: single-bracket set with another nList2
# ---------------------------------------------------------------------------

test_that("nList2 uncompiled: [<- with nList2 value", {
  src <- rNL$new()
  length(src) <- 2
  src[[1]] <- 200.0
  src[[2]] <- 400.0

  dst <- rNL$new()
  length(dst) <- 4
  for(i in 1:4) dst[[i]] <- 0.0
  dst[c(1L, 3L)] <- src   # src IS an nList2 -> singleBracket_set_nList path
  expect_equal(dst[[1]], 200.0)
  expect_equal(dst[[3]], 400.0)
  expect_equal(dst[[2]], 0.0)
  expect_equal(dst[[4]], 0.0)
  rm(src, dst); gc()
})

# ---------------------------------------------------------------------------
# Uncompiled: as.list
# ---------------------------------------------------------------------------

test_that("nList2 uncompiled: as.list", {
  obj <- make_uncompiled()
  expect_equal(as.list(obj), list(10.0, 20.0, 30.0, 40.0))
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Compiled object used from R: length
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: length, setLength, length<-", {
  obj <- cNL$new()
  expect_equal(length(obj), 0L)
  length(obj) <- 4
  expect_equal(length(obj), 4L)
  length(obj) <- 2
  expect_equal(length(obj), 2L)
  obj$setLength(6L)
  expect_equal(obj$getLength(), 6L)
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Compiled object used from R: double-bracket get / set
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: [[ get and set", {
  obj <- cNL$new()
  obj$setLength(3L)
  obj[[1]] <- 1.1
  obj[[2]] <- 2.2
  obj[[3]] <- 3.3
  expect_equal(obj[[1]], 1.1)
  expect_equal(obj[[2]], 2.2)
  expect_equal(obj[[3]], 3.3)
  obj[[2]] <- 99.9
  expect_equal(obj[[2]], 99.9)
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [[ with numeric and logical index", {
  obj <- make_compiled()
  expect_equal(obj[[2.0]],  20.0)  # numeric index
  expect_equal(obj[[TRUE]], 10.0)  # logical TRUE -> first element
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [[ get gives and error and set expands", {
  obj <- cNL$new()
  expect_error(obj[[1]])
  obj[[2]] <- 2.2
  expect_equal(obj[[1]], 0)
  expect_equal(obj[[2]], 2.2)
  rm(obj); gc()
})

test_that("nList2 uncompiled: [[ set chains correctly", {
  obj <- rNL$new()
  obj[[2]] <- 2.2
  A <- list(obj = obj)
  A$obj[[1]] <- 1.1
  expect_equal(A$obj[[1]], 1.1)
  expect_equal(A$obj[[2]], 2.2)
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Compiled object used from R: single-bracket get
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: [ get with integer indices", {
  obj <- make_compiled()
  expect_equal(as.list(obj[c(1L, 3L)]),     list(10.0, 30.0))
  expect_equal(as.list(obj[c(4L, 2L, 1L)]), list(40.0, 20.0, 10.0))
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [ get with numeric indices", {
  obj <- make_compiled()
  expect_equal(as.list(obj[c(2.0, 4.0)]), list(20.0, 40.0))
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [ get with logical indices", {
  obj <- make_compiled()
  expect_equal(as.list(obj[c(TRUE, FALSE, TRUE, FALSE)]), list(10.0, 30.0))
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [ get, logical index recycled over contents", {
  obj <- make_compiled()                      # length 4: 10,20,30,40
  expect_equal(as.list(obj[c(TRUE, FALSE)]),  list(10.0, 30.0))
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Compiled object used from R: single-bracket set with list values
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: [<- with list values, integer indices", {
  obj <- make_compiled()
  obj[c(1L, 3L)] <- list(100.0, 300.0)
  expect_equal(obj[[1]], 100.0)
  expect_equal(obj[[2]], 20.0)
  expect_equal(obj[[3]], 300.0)
  expect_equal(obj[[4]], 40.0)
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [<- with list values, numeric indices", {
  obj <- make_compiled()
  obj[c(2.0, 4.0)] <- list(200.0, 400.0)
  expect_equal(obj[[2]], 200.0)
  expect_equal(obj[[4]], 400.0)
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [<- with list values, logical indices", {
  obj <- make_compiled()
  obj[c(TRUE, FALSE, TRUE, FALSE)] <- list(11.0, 33.0)
  expect_equal(obj[[1]], 11.0)
  expect_equal(obj[[2]], 20.0)
  expect_equal(obj[[3]], 33.0)
  expect_equal(obj[[4]], 40.0)
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Compiled object used from R: single-bracket set — recycling rule
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: [<- list recycled over integer indices", {
  obj <- make_compiled()
  obj[1:4] <- list(1.0, 2.0)
  expect_equal(as.list(obj), list(1.0, 2.0, 1.0, 2.0))
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [<- list recycled over logical indices", {
  obj <- make_compiled()
  obj[c(TRUE, FALSE, TRUE, FALSE)] <- list(99.0)  # single value -> recycled
  expect_equal(obj[[1]], 99.0)
  expect_equal(obj[[3]], 99.0)
  expect_equal(obj[[2]], 20.0)   # untouched
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [<- logical index recycled over contents", {
  obj <- make_compiled()                    # length 4: 10,20,30,40
  obj[c(TRUE, FALSE)] <- list(11.0, 33.0)  # bools recycled to T,F,T,F
  expect_equal(obj[[1]], 11.0)
  expect_equal(obj[[2]], 20.0)
  expect_equal(obj[[3]], 33.0)
  expect_equal(obj[[4]], 40.0)
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Compiled object used from R: single-bracket set with single scalar value
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: [<- single scalar recycled over positions", {
  obj <- make_compiled()
  obj[c(1L, 2L, 3L)] <- 7.7
  expect_equal(obj[[1]], 7.7)
  expect_equal(obj[[2]], 7.7)
  expect_equal(obj[[3]], 7.7)
  expect_equal(obj[[4]], 40.0)   # untouched
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Compiled object used from R: single-bracket set with another nList2
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: [<- with same-type nList2", {
  src <- cNL$new()
  src$setLength(2L)
  src[[1]] <- 200.0
  src[[2]] <- 400.0

  dst <- make_compiled()
  dst[c(1L, 3L)] <- src
  expect_equal(dst[[1]], 200.0)
  expect_equal(dst[[2]], 20.0)
  expect_equal(dst[[3]], 400.0)
  expect_equal(dst[[4]], 40.0)
  rm(src, dst); gc()
})

# ---------------------------------------------------------------------------
# Compiled object used from R: automatic expansion of contents on out-of-bounds [<-
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: integer [<- auto-expands contents", {
  obj <- cNL$new()
  obj$setLength(2L)
  obj[[1]] <- 1.0; obj[[2]] <- 2.0
  obj[c(1L, 5L)] <- list(10.0, 50.0)   # index 5 is out of bounds
  expect_equal(length(obj), 5L)
  expect_equal(obj[[1]], 10.0)
  expect_equal(obj[[5]], 50.0)
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: numeric [<- auto-expands contents", {
  obj <- cNL$new()
  obj$setLength(2L)
  obj[[1]] <- 1.0; obj[[2]] <- 2.0
  obj[c(1.0, 6.0)] <- list(10.0, 60.0)  # index 6 is out of bounds
  expect_equal(length(obj), 6L)
  expect_equal(obj[[1]], 10.0)
  expect_equal(obj[[6]], 60.0)
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: logical [<- auto-expands when bools longer than contents", {
  obj <- cNL$new()
  obj$setLength(2L)
  obj[[1]] <- 1.0; obj[[2]] <- 2.0
  obj[c(TRUE, FALSE, TRUE, FALSE, TRUE)] <- list(10.0, 30.0, 50.0)  # bools longer -> resize to 5
  expect_equal(length(obj), 5L)
  expect_equal(obj[[1]], 10.0)
  expect_equal(obj[[3]], 30.0)
  expect_equal(obj[[5]], 50.0)
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Compiled object used from R: as.list
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: as.list", {
  obj <- make_compiled()
  expect_equal(as.list(obj), list(10.0, 20.0, 30.0, 40.0))
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Error cases: invalid indices for [[
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: [[ NA index errors", {
  obj <- make_compiled()
  expect_error(obj[[NA_integer_]])
  expect_error(obj[[NA_real_]])
  expect_error(obj[[NA]])            # logical NA
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [[ out-of-bounds errors", {
  obj <- make_compiled()
  expect_error(obj[[0L]])
  expect_error(obj[[5L]])            # length is 4
  expect_error(obj[[-1L]])
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [[ FALSE logical errors", {
  obj <- make_compiled()
  expect_error(obj[[FALSE]])
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [[ multi-element index errors", {
  obj <- make_compiled()
  expect_error(obj[[c(1L, 2L)]])
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [[ non-finite numeric errors", {
  obj <- make_compiled()
  expect_error(obj[[Inf]])
  expect_error(obj[[NaN]])
  rm(obj); gc()
})

# ---------------------------------------------------------------------------
# Error cases: invalid indices / values for [<-
# ---------------------------------------------------------------------------

test_that("nList2 compiled obj used from R: [<- NA index errors", {
  obj <- make_compiled()
  expect_error(obj[NA_integer_]  <- list(1.0))
  expect_error(obj[NA_real_]     <- list(1.0))
  expect_error(obj[NA]           <- list(1.0))   # logical NA
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [<- zero or negative integer index errors", {
  obj <- make_compiled()
  expect_error(obj[0L]  <- list(1.0))
  expect_error(obj[-1L] <- list(1.0))
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [<- non-finite numeric index errors", {
  obj <- make_compiled()
  expect_error(obj[Inf] <- list(1.0))
  expect_error(obj[NaN] <- list(1.0))
  rm(obj); gc()
})

test_that("nList2 compiled obj used from R: [<- zero-length replacement errors", {
  obj <- make_compiled()
  expect_error(obj[1:2] <- list())
  rm(obj); gc()
})

test_that("nList2: [<- wrong nList2 element type is rejected", {
  rNL_int <- nList2("integerScalar")
  cNL_int <- nCompile(rNL_int)
  src <- cNL_int$new()
  src$setLength(2L)
  dst <- make_compiled()   # numericScalar nList2
  expect_error(dst[1:2] <- src)
  rm(src, dst); gc()
})

# ---------------------------------------------------------------------------
# Same-type identity: multiple generators of the same type
# ---------------------------------------------------------------------------
# Following tests do not work yet.

## foo <- nFunction(
##   fun = function() {
##     ans <- nList2("integerScalar")$new()
##     return(ans)
##     returnType("nList2('integerScalar')")
##   }
## )
## cfoo <- nCompile(foo)
## NL2 <- nList2("integerScalar")
## cfoo <- nCompile(NL2)
## cfoo <- nCompile(NL2, foo)

test_that("nList2: two generators of same type have equal classID", {
  rNL1 <- nList2("numericScalar")
  rNL2 <- nList2("numericScalar")
  expect_equal(NCinternals(rNL1)$classID, NCinternals(rNL2)$classID)
  rm(rNL1, rNL2); gc()
})

test_that("nList2: duplicate units in nCompile are error-trapped", {
  rNL1 <- nList2("numericScalar")
  rNL2 <- nList2("numericScalar")
  # Both generators have the same classID; nCompile should deduplicate and
  # produce exactly one compiled nList2 class without error.
  expect_error(comp <- nCompile(rNL1, rNL2))
  rm(rNL1, rNL2); gc()
})

test_that("nList2: multiple ways to indicate the same nList2 are de-duplicated", {
  rNL1 <- nList2("numericScalar")
  rNL2 <- nList2("integerScalar")
  foo <- nFunction(
    name = "foo",
    function(nl = 'rNL1') {
      return(nl)
    },
    returnType = "nList2('numericScalar')"
  )
  comp <- nCompile(rNL1, rNL2, foo)
  cNL1a <- comp$rNL1$new()
  cNL2a <- comp$rNL2$new()
  expect_error(comp$foo(cNL2a))
  expect_no_error(comp$foo(cNL1a))
  # By manually verifying that a different type would error out
  # we can see that the rNL1 is the same type.
})

message("FIX ME: Type de-duplication for nList2 is not complete.")
## test_that("nList2: multiple ways to indicate the same nList2 are WRONGLY NOT de-duplicated", {
##   rNL1 <- nList2("double()") # equivalent to numericScalar but not seen as the same - FIX ME
##   foo <- nFunction(
##     name = "foo",
##     function(nl = 'rNL1') {
##       return(nl)
##     },
##     returnType = "nList2('numericScalar')"
##   )
##   comp <- expect_error(nCompile(rNL1, foo))
## })


# ---------------------------------------------------------------------------
# Compiled: various [ an [[ get and set operations compile and work
# ---------------------------------------------------------------------------

test_that("nList2 various bracket get and set operations compile and work",
{
  rNL <- nList2("numericScalar")
  nc <- nClass(
    classname = "nc_holds_nList2",
    Cpublic = list(
      lst = 'rNL',
      get_single_bracket_int = nFunction(
        function(inds = integer(1)) {
          res <- lst[inds]
          return(res)
        },
        returnType = 'rNL'
      ),
      get_single_bracket_double = nFunction(
        function(inds = double(1)) {
          res <- lst[inds]
          return(res)
        },
        returnType = 'rNL'
      ),
      get_single_bracket_logical = nFunction(
        function(inds = logical(1)) {
          res <- lst[inds]
          return(res)
        },
        returnType = 'rNL'
      ),
      set_single_bracket_int = nFunction(
        function(inds = integer(1), v = 'rNL') {
          lst[inds] <- v
          return(lst)
        },
        returnType = 'rNL'
      ),
      set_single_bracket_double = nFunction(
        function(inds = double(1), v = 'rNL') {
          lst[inds] <- v
          return(lst)
        },
        returnType = 'rNL'
      ),
      set_single_bracket_logical = nFunction(
        function(inds = logical(1), v = 'rNL') {
          lst[inds] <- v
          return(lst)
        },
        returnType = 'rNL'
      ),
      get_double_bracket = nFunction(
        function(ind = integer()) {
          return(lst[[ind]])
        },
        returnType = 'numericScalar'
      ),
      set_double_bracket = nFunction(
        function(ind = integer(), v = 'numericScalar') {
          lst[[ind]] <- v
          return(v)
        },
        returnType = 'numericScalar'
      )
    )
  )
  comp <- nCompile(nc, rNL)

  obj <- comp$nc$new()
  obj$lst <- nc$new()

  length(obj$lst) <- 3
  curlst <- as.list(obj$lst)
  expect_equal(length(curlst), 3)

  lst2 <- comp$rNL$new()
  lst2[1:3] <- list(1, 2, 3)
  expect_identical(as.list(lst2), list(1, 2, 3))

  obj$set_single_bracket_int(c(3, 1, 2), lst2[c(3, 1, 2)])
  expect_identical(obj$lst |> as.list(), list(1, 2, 3))
  lst_out <- obj$get_single_bracket_int(c(3, 1, 2))
  expect_identical(lst_out |> as.list(), list(3, 1, 2))

  obj$set_single_bracket_double(c(3, 1, 2), lst2[c(3, 1, 2)])
  expect_identical(obj$lst |> as.list(), list(1, 2, 3))
  lst_out <- obj$get_single_bracket_double(c(3, 1, 2))
  expect_identical(lst_out |> as.list(), list(3, 1, 2))

  obj$set_single_bracket_logical(c(TRUE, FALSE, TRUE), lst2[c(3, 2)])
  expect_identical(obj$lst |> as.list(), list(3, 2, 2))
  lst_out <- obj$get_single_bracket_logical(c(TRUE, TRUE, FALSE))
  expect_identical(lst_out |> as.list(), list(3, 2))

  obj$set_double_bracket(4, 4.4)
  expect_identical(obj$lst |> as.list(), list(3, 2, 2, 4.4))
  lst_out <- obj$get_single_bracket_logical(c(TRUE, TRUE, FALSE))
  expect_identical(lst_out |> as.list(), list(3, 2, 4.4))

  expect_error(obj$get_double_bracket(8))
  expect_error(obj$get_double_bracket(0))
  expect_error(obj$get_single_bracket(c(1, 2, 8)))
  expect_error(obj$get_single_bracket(c(1, 2, 0)))
})

pause_<-function(){browser(); NULL}
test_that("nList2 of nClass elements works", {
  element_nc <- nClass(
    classname = "element_nc_",
    Cpublic = list(
      x = 'numericVector',
      foo = nFunction(
        function(a = double()) {
        return(a+1)
        returnType(double())
        }
      )
    )
  )
  rNL <- nList2('element_nc')
  use_NL <- nFunction(
    function(nl = "nList2('element_nc')", i = integer()) {
      nl[[i]]$x <- 1:3
    }
  )
  comp <- nCompile(use_NL, rNL, element_nc)
  nl1 <- comp$rNL$new()
  cat("a\n")
  cat(tempdir())
  pause_()
  nl1[[1]] <- comp$element_nc$new()
  cat("a\n")
  cat(tempdir())
  pause_()
  nl1[[1]]$x <- 11:14
  expect_error(nl2 <- use_NL(nl1, 2))
  comp$use_NL(nl1, 1)
  expect_equal(nl1[[1]]$x, 1:3)
  length(nl1) <- 3
  nl1[[3]] <- comp$element_nc$new()
  comp$use_NL(nl1, 3)
  expect_equal(nl1[[3]]$x, 1:3)
})

## The following tests might be made to work fine.
## At the time of working on this I ran out of time to pursue further tests,
## so these were left incompletely worked out.
##
## test_that("nList2: nClass member of nList2 type compiles and works", {
##   rNL <- nList2("numericScalar")
##   nc <- nClass(
##     classname = "nc_holds_nList2",
##     Cpublic = list(
##       lst = 'rNL',
##       init = nFunction(function() {
##         lst <<- rNL$new()
##        # lst$setLength(3L)
##         A <- lst[[1]]
##         length(lst) <- 4
##         len <- length(lst)
##         #lst[[1]] <<- 10.0; lst[[2]] <<- 20.0; lst[[3]] <<- 30.0
##       }),
##       getLen = nFunction(
##         function() { return(lst$getLength()) },
##         returnType = 'integerScalar'
##       )
##     )
##   )
## #  debug(nCompiler:::simpleTransformationsEnv$CheckOpAssignment)
##   comp <- nCompile(rNL, nc)
##   obj <- comp$nc$new()
##   obj$init()
##   expect_equal(obj$getLen(), 3L)
##   rm(rNL, nc, comp, obj); gc()
## })

## test_that("nList2: nFunction return type of nList2 compiles and works", {
##   rNL <- nList2("numericScalar")
##   nc <- nClass(
##     classname = "nc_returns_nList2",
##     Cpublic = list(
##       lst = 'rNL',
##       init = nFunction(function() {
##         lst <<- rNL$new()
##         lst$setLength(2L)
##         lst[[1]] <<- 100.0; lst[[2]] <<- 200.0
##       }),
##       getLst = nFunction(
##         function() { return(lst) },
##         returnType = 'rNL'
##       )
##     )
##   )
##   comp <- nCompile(rNL, nc)
##   obj <- comp$nc_returns_nList2$new()
##   obj$init()
##   got <- obj$getLst()
##   expect_equal(got[[1]], 100.0)
##   expect_equal(got[[2]], 200.0)
##   rm(rNL, nc, comp, obj, got); gc()
## })

## test_that("nList2: nFunction argument of nList2 type compiles and works", {
##   rNL <- nList2("numericScalar")
##   nc <- nClass(
##     classname = "nc_nList2_arg",
##     Cpublic = list(
##       lenOf = nFunction(
##         function(x = 'rNL') { return(x$getLength()) },
##         returnType = 'integerScalar'
##       )
##     )
##   )
##   comp <- nCompile(rNL, nc)
##   obj  <- comp$nc_nList2_arg$new()
##   lst  <- comp$nList2$new()
##   lst$setLength(5L)
##   expect_equal(obj$lenOf(lst), 5L)
##   rm(rNL, nc, comp, obj, lst); gc()
## })

## test_that("nList2: nList2 created internally in nFunction code compiles and works", {
##   rNL <- nList2("numericScalar")
##   nc <- nClass(
##     classname = "nc_creates_nList2",
##     Cpublic = list(
##       makeAndSum = nFunction(
##         function() {
##           tmp <- rNL$new()
##           tmp$setLength(3L)
##           tmp[[1]] <- 1.0; tmp[[2]] <- 2.0; tmp[[3]] <- 3.0
##           s <- tmp[[1]] + tmp[[2]] + tmp[[3]]
##           return(s)
##         },
##         returnType = 'numericScalar'
##       )
##     )
##   )
##   comp <- nCompile(rNL, nc)
##   obj  <- comp$nc_creates_nList2$new()
##   expect_equal(obj$makeAndSum(), 6.0)
##   rm(rNL, nc, comp, obj); gc()
## })

## test_that("nList2: objects from two same-type generators are interchangeable", {
##   # Two independent nCompile calls, each producing a nList2<double>.
##   # An object from one DLL should be passable to a method compiled in the
##   # other DLL, because they share the same C++ classname (and thus RTTI).
##   rNL1 <- nList2("numericScalar")
##   rNL2 <- nList2("numericScalar")

##   cNL1 <- nCompile(rNL1)
##   rNL3 <- nList2("numericScalar")   # fresh generator, same type
##   nc <- nClass(
##     classname = "nc_accept_nList2",
##     Cpublic = list(
##       lenOf = nFunction(
##         function(x = 'rNL3') { return(x$getLength()) },
##         returnType = 'integerScalar'
##       )
##     )
##   )
##   comp2 <- nCompile(rNL3, nc)

##   src <- cNL1$nList2$new()
##   src$setLength(4L)
##   obj <- comp2$nc_accept_nList2$new()
##   expect_equal(obj$lenOf(src), 4L)
##   rm(rNL1, rNL2, rNL3, cNL1, nc, comp2, src, obj); gc()
## })

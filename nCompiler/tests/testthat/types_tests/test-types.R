# typeSpec tests

# try with rlang
#
# nCapT <- function(arg) {
#   arg <- substitute(arg)
#   substitute(rlang::enquo(ARG), list(ARG = arg)) |> eval(parent.frame())
# }
#
# foo <- function(a1) {
#   a1t <- rlang::enquo(a1)
#   a1t
# }
#
# foo2 <- function(a1) {
#   a1t <- nCapT(a1)
#   a1t
# }
#
# foo3 <- function(a3) {
#   a3t <- rlang::enquo(a3)
#   foo({{a3t}})
# }
#
# foo4 <- function(a4) {
#   a4t <- nCapT(a4)
#   foo2({{a4t}})
# }
#
# bar <- function(a5) {
#   # force(a5) # This mars the promise so don't force before passing through.
#   # Alternatively catch it with enquo or our synonym nCapT
#   foo({{a5}})
# }
#
# nT <- function(type) {
#   nCapT(type)
# }
#
# nTList <- function(..., .list = NULL) {
#   if(!is.null(.list)) {
#     res <- vector(mode = "list", length = length(.list))
#     pf <- parent.frame()
#     for(i in seq_along(.list)) {
#       new_quo <- substitute(rlang::quo(EXPR), list(EXPR = .list[[i]])) |> eval()
#       new_quo <- rlang::quo_set_env(new_quo, pf)
#       res[[i]] <- new_quo
#     }
#     return(res)
#   }
#   res <- rlang::enquos(..., .homonyms = "error")
#   res
# }
#
# foo(numericVector())
# bar(numericVector())
# bar(double())
# foo2(numericVector())
# foo3(numericVector())
# foo4(numericVector())
# nv <- 'numericVector'
# foo({{nv}})
# nT(numericVector())
# nv2 <- foo(numericVector())
# nv2
# foo({{nv2}})
# mynt <- nT(numericVector())
# foo(numericVector())
# foo({{mynt}})
# nv3 <- rlang::quo('numericVector')
# foo({{nv3}})
# tl <- list(a = 'numericVector', b = quote(numericVector()))
# debugonce(nTList)
# nTList(.list = tl)
# identical(foo(numericVector()), nTList(.list=tl)[[2]])
# nTList(a = 'numericVector', b = numericVector(), c = {{ nv2 }}, {{nv}})
# list(a = nT('numericVector'), b = nT(numericVector()))
# f <- function(a = numericVector(), b = 'numericVector') {}
# ff <- formals(f)
# nTList(.list = ff)
#
# pairlist2TList <- function(ff) {
#   rlang::call2(quote(nTList), !!!ff) |> eval(parent.frame())
# }
#
# # something like passing a type into nList_nClass
#
# f1 <- function(type) {
#   ttype <- nCapT(type)
#   f2 <- function(a = numericVector(), b = {{ttype}}) {}
#   ff <- formals(f2)
#   # eval instead of eval_tidy here gets the environment right
#   # whereas eval_tidy inserts a masking environment, I guess.
#   pairlist2TList(ff)
# #  rlang::call2(quote(nTList), !!!ff) |> eval()
#   #nTList(.list = ff)
# }
# # debugonce(f1)
# f1('numericScalar')
# f1(integerVector())
# f1(nList(nList('numericScalar')))
#
# # test a situation where there are hidden call layers
# myR6 <- R6::R6Class(
#   public = list(
#     xt = NULL,
#     initialize = function(x) {
#       self$xt <- nCapT(x)
#     }
#   )
# )
# myobj <- myR6$new(numericVector())
# myobj$xt
# doodle <- function(x) {
#   xt <- nCapT(x)
#   obj <- myR6$new({{xt}})
#   obj$xt
# }
# doodle(numericVector())
#
#
# # Test taking a list into a function and then calling another function on each item
#
# f2 <- function(obj) {
#   tobj <- nCapT(obj)
#   print(rlang::quo_get_expr(tobj) |> rlang::expr_text())
#   NULL
# }
#
# f1 <- function(mylist) {
#   # could be a list of quosures, a list of type inputs, or a mix
#   tmyList <- nTList(.list = mylist)
#   for(obj in tmyList) {
#     f2({{obj}})
#   }
#   for(i in seq_along(tmyList)) {
#     f2({{tmyList[[i]]}})
#   }
# }
# # debugonce(f1)
# f1(list(a = 'numericVector', b = quote(numericVector())))
# lll <- nTList(a = 'numericVector', b = numericVector())
# f1(lll)
# lll2 <- list(a = nT('numericVector'), b = nT(numericVector()))
# f1(lll2)
# NULL
#





###############
#
# foo <- function(a1) {
#   a1t <- nCaptureType(a1)
#   a1t
# }
#
# foo2 <- function(a2) {
#   a2t <- nCaptureType(a2)
#   a2t
# }
#
# foo3 <- function(a3) {
#   a3t <- nCaptureType(a3)
#   res <- foo(T(a3t))
#   res
# }
#
# foo(NULL)
# nCaptureType(numericVector())
# nv <- 'numericVector'
#
# nCaptureType(T(nv))
# foo3(numericVector())
# foo(numericVector())
# #debugonce(nCaptureType)
# foo2(numericVector())
# foo('numericVector')
# nv <- 'numericVector'
# foo(nv) # type appears as nv
# foo(T(nv)) # type is numericVector
# nv2 <- foo(numericVector())
# foo(T(nv2))
# foo2(T(nv2))
# foo3(T(nv2))
# nv3 <- nType(numericVector(a))
# nv3
# foo2(T(nv3))
# nType(T(nv))
# tl <- list(a = 'numericVector', b = quote(numericVector()))
# nTypeList(.list = tl)
# nTypeList(a = 'numericVector', b = numericVector(), c = T(nv3))#, d = T(quote(numericVector())))
# # should be idempotent
# tlres <- nTypeList(a = 'numericVector', b = numericVector(), c = T(nv3), d = T(quote(numericVector())))
# nTypeList(.list=tlres)
#
# ## junk <- function(x = numericVector(), y = 'numericVector', z = nList( nList( 'numericVector'))) {}
# ## fj <- formals(junk)
# ## nTypeList(fj)
# ## nTypeList(list())
# ## junk2 <- list(x = nType(surprise()))
# ## nTypeList(junk2)


################################
# newer tests: since moving to rlang quosures
# for passing around types

test_that("type handling with quosures: basics", {
  foo <- function(a1) {
    a1t <- nCaptureType(a1) # essentially a synonym for rlang::enquo, named for type concepts
    a1t
  }
  foo2 <- function(a2) {
    a2t <- nCaptureType(a2)
    foo({{a2t}})
  }
  t1 <- foo("numericVector")
  t2 <- foo2("numericVector")
  expect_identical(t1, t2)
  expect_identical(nCompiler:::nTypeSpec(t1), nCompiler:::nTypeSpec(t2))

  tt <- nType("numericVector")
  t1 <- foo({{tt}})
  t2 <- foo2({{tt}})
  expect_identical(t1, t2)
  expect_identical(nCompiler:::nTypeSpec(t1), nCompiler:::nTypeSpec(t2))

  t1 <- foo("numericVector()")
  t2 <- foo2("numericVector()")
  expect_identical(t1, t2)
  expect_identical(nCompiler:::nTypeSpec(t1), nCompiler:::nTypeSpec(t2))

  t1 <- foo(numericVector())
  t2 <- foo2(numericVector())
  expect_identical(t1, t2)
  expect_identical(nCompiler:::nTypeSpec(t1), nCompiler:::nTypeSpec(t2))

  t1 <- foo(numericVector)
  t2 <- foo2(numericVector)
  expect_identical(t1, t2)
  expect_identical(nCompiler:::nTypeSpec(t1), nCompiler:::nTypeSpec(t2))

  t1 <- foo("numericVector")
  t2 <- foo("numericVector()")
  t3 <- foo(numericVector())
  t4 <- foo(numericVector)
  ts1 <- nCompiler:::nTypeSpec(t1)[c('funName', 'args')]
  ts2 <- nCompiler:::nTypeSpec(t2)[c('funName', 'args')]
  ts3 <- nCompiler:::nTypeSpec(t3)[c('funName', 'args')]
  ts4 <- nCompiler:::nTypeSpec(t4)[c('funName', 'args')]
  expect_identical(ts1, ts2)
  expect_identical(ts1, ts3)
  expect_identical(ts1, ts4)
})

#################################
# older tests, updated to look at uniqueID() and cpp_typename().
test_that("numericVector(5)",
{
  ## as if `a = numericVector(5)` declared in function argument
  a <- quote(numericVector(5))
  aSym <- `:::`("nCompiler", "type2symbol")({{a}},
                                     name = "a",
                                     origName = "orig_a",
                                     isArg = TRUE)
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_identical(aSym$nDim, 1)
  expect_identical(aSym$isRef, FALSE)
  expect_identical(aSym$isArg, TRUE)
  expect_identical(aSym$uniqueID(), "D1")
  expect_identical(aSym$cpp_typename(), "Eigen::Tensor<double, 1>")
})

test_that("ref(numericVector(5))",
{
  ## as if `a = ref(numericVector(5))` declared in function-argument
  a <- quote(ref(numericVector(5)))
  aSym <- `:::`("nCompiler", "type2symbol")({{a}},
                                     name = "a",
                                     origName = "orig_a",
                                     isArg = TRUE)
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_identical(aSym$nDim, 1)
  expect_identical(aSym$isRef, TRUE)
  expect_identical(aSym$isArg, TRUE)
  expect_identical(aSym$uniqueID(), "D1")
  expect_identical(aSym$cpp_typename(), "Eigen::Tensor<double, 1> &")
})

test_that("numericVector(5) isRef=TRUE",
{
  ## as if `a = numericVector(5)` declared in function-argument
  ## and isRef=TRUE used to indicate reference
  a <- quote(numericVector(5))
  aSym <- `:::`("nCompiler", "type2symbol")({{a}},
                                     name = "a",
                                     origName = "orig_a",
                                     isArg = TRUE,
                                     isRef = TRUE)
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_identical(aSym$nDim, 1)
  expect_identical(aSym$isRef, TRUE)
  expect_identical(aSym$isArg, TRUE)
})

test_that("ref(numericVector(5) via explicitType)",
{
  ## as if `a` declared in function-argument,
  ## with argType = `ref(numericVector(5))`
  a <- NULL
  aExplicit <- quote(ref(numericVector(5)))

  aSym <- `:::`("nCompiler", "type2symbol")({{a}},
                                     name = "a",
                                     origName = "orig_a",
                                     isArg = TRUE,
                                     explicitType = {{aExplicit}})
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_identical(aSym$nDim, 1)
  expect_identical(aSym$isRef, TRUE)
  expect_identical(aSym$isArg, TRUE)
})

test_that("numericVector(5) via explicitType",
{
  ## as if `a` declared in function-argument,
  ## with argType = `numericVector(5)`
  a <- NULL
  aExplicit <- quote(numericVector(5))
  aSym <- `:::`("nCompiler", "type2symbol")({{a}},
                                     name = "a",
                                     origName = "orig_a",
                                     isArg = TRUE,
                                     explicitType = {{aExplicit}})
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_identical(aSym$nDim, 1)
  expect_identical(aSym$isRef, FALSE)
  expect_identical(aSym$isArg, TRUE)
})

test_that("numericVector(5) isRef = TRUE via explicitType",
{
  ## as if `a` declared in function-argument,
  ## with argType = `numericVector(5)`
  a <- NULL
  aExplicit <- quote(numericVector(5))
  aSym <- `:::`("nCompiler", "type2symbol")({{a}},
                                     name = "a",
                                     origName = "orig_a",
                                     isArg = TRUE,
                                     isRef = TRUE,
                                     explicitType = {{aExplicit}})
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_identical(aSym$nDim, 1)
  expect_identical(aSym$isRef, TRUE)
  expect_identical(aSym$isArg, TRUE)
})

test_that("ref(numericVector(5)) via explicitType with default value to ignore",
{
  ## as if `a` declared in function-argument with default and
  ## with argType = `ref(numericVector(5))`
  a <- quote(rnorm(5)) ## ignored
  aExplicit <- quote(ref(numericVector(5)))
  aSym <- `:::`("nCompiler", "type2symbol")({{a}},
                                     name = "a",
                                     origName = "orig_a",
                                     isArg = TRUE,
                                     explicitType = {{aExplicit}})
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_identical(aSym$nDim, 1)
  expect_identical(aSym$isRef, TRUE)
  expect_identical(aSym$isArg, TRUE)
})

test_that("infer type from evaluating default",
{
  ## infer type of `a` from default

  # This commented code is curious.
  # It actually does produce a quosure env that is emptyenv
  # and so it does go through type from evaluation of rnorm(5)
  # However that is more of an accident because {{a}} when
  # a is an R expression may not be standard or expected.
  # a <- quote(rnorm(5))
  # aSym <- `:::`("nCompiler", "type2symbol")({{a}},
  #                                    name = "a",
  #                                    origName = "orig_a",
  #                                    isArg = TRUE
  #                                    )

  a <- quote(rnorm(5))
  aSym <- `:::`("nCompiler", "type2symbol")(O(!!a),
                                            name = "a",
                                            origName = "orig_a",
                                            isArg = TRUE
  )
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_equal(aSym$nDim, 1)
  expect_identical(aSym$isRef, FALSE)
  expect_identical(aSym$isArg, TRUE)

  aSym <- `:::`("nCompiler", "type2symbol")(O(rnorm(5)),
                                            name = "a",
                                            origName = "orig_a",
                                            isArg = TRUE
  )
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_equal(aSym$nDim, 1)
  expect_identical(aSym$isRef, FALSE)
  expect_identical(aSym$isArg, TRUE)

  at <- nType(O(rnorm(5)))
  aSym <- `:::`("nCompiler", "type2symbol")(T(at),
                                            name = "a",
                                            origName = "orig_a",
                                            isArg = TRUE
  )
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_equal(aSym$nDim, 1)
  expect_identical(aSym$isRef, FALSE)
  expect_identical(aSym$isArg, TRUE)
})

test_that("infer type from evaluating default, with scoping needed",
{
  ## infer type of `a` from default with scoping needed to
  ## evaluate the default expression.
  fun1 <- function() {
    fun2 <- function(n) rnorm(n)
    a <- quote(fun2(5))
    aSym <- `:::`("nCompiler", "type2symbol")(O(!!a),
                                       name = "a",
                                       origName = "orig_a",
                                       isArg = TRUE
                                       )
    aSym
  }
  aSym <- fun1()
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_equal(aSym$nDim, 1)
  expect_identical(aSym$isRef, FALSE)
  expect_identical(aSym$isArg, TRUE)
})

test_that("Trap error from duplicate setting of isRef. (This should show a warning.)",
{
  ## Error-trapping:
  ## Duplicate setting of ref
  a <- quote(ref(numericVector(5)))
  cat("expecting an error:\n")
  expect_error(
    `:::`("nCompiler", "type2symbol")(O(!!a),
                               name = "a",
                               origName = "orig_a",
                               isArg = TRUE,
                               isRef = TRUE)
  )

  ## Error-trapping
  ## Type incompatible with default
  a <- quote(matrix(1:4, nrow = 2, ncol = 2))
  aExplicit <- nType(numericVector())
  cat("expecting an error:\n")
  expect_error(suppressWarnings( # this gives a warning and an error, so for testing we suppress the warning
    `:::`("nCompiler", "type2symbol")(O(!!a),
                               name = "a",
                               origName = "orig_a",
                               isArg = TRUE,
                               explicitType = {{aExplicit}})
  ))
})

test_that("nMatrix(type = \"integer\")",
{
  ## Type constructed from object
  cat('\nTo do: test of using nType object to define type\n')

  ## some other basic types
  b <- quote(nMatrix(type = "integer",
                     nrow = 3,
                     ncol = 5))
  bSym <- `:::`("nCompiler", "type2symbol")(O(!!b),
                                     name = "b",
                                     origName = "orig_b",
                                     isArg = TRUE)
  expect_identical(bSym$name, "b")
  expect_identical(bSym$type, "integer")
  expect_equal(bSym$nDim, 2)
  expect_identical(bSym$isRef, FALSE)
  expect_identical(bSym$isArg, TRUE)
})

test_that("list type works",
{
  l <- "RcppList"
  lSym <- `:::`("nCompiler", "type2symbol")(!!l, name = "l", isArg = TRUE)
  expect_identical(lSym$name, "l")
  expect_identical(lSym$type, "Rcpp::List")
  expect_identical(lSym$isRef, FALSE)
  expect_identical(lSym$isArg, TRUE)
})

test_that("list arguments handled correctly",
{
  ## working from a list
  a <- quote(numericMatrix())
  b <- NULL
  bExplicit = quote(nMatrix(type = "integer"))
  aRef <- TRUE
  bRef <- FALSE
  symTab <- `:::`("nCompiler", "typeList2symbolTable")(
    typeList = list(a = a,
                       b = b),
    origNames = c("orig_a", "orig_b"),
    isRef = list(a = aRef, b = bRef),
    explicitTypeList = list(b = {{bExplicit}})
  )
  expect_identical(symTab$getSymbolNames(),
                   c("a","b"))
  aSym <- symTab$getSymbol("a")
  expect_identical(aSym$name, "a")
  expect_identical(aSym$type, "double")
  expect_equal(aSym$nDim, 2)
  expect_identical(aSym$isRef, TRUE)
  expect_identical(aSym$isArg, FALSE)

  bSym <- symTab$getSymbol("b")
  expect_identical(bSym$name, "b")
  expect_identical(bSym$type, "integer")
  expect_equal(bSym$nDim, 2)
  expect_identical(bSym$isRef, FALSE)
  expect_identical(bSym$isArg, FALSE)

  ## void() (return type default)
  vSym <- `:::`("nCompiler", "type2symbol")(quote(void()))
  expect_identical(vSym$type, "void")
#  expect_identical(vSym$nDim, 0)
})

test_that("symbolTBD works",
{
  nCompiler:::resetLabelFunctionCreators()
  nc1 <- nClass(
    Cpublic = list(a = 'numericScalar')
  )
  sym_nc1 <- nCompiler:::type2symbol('nc1', 'nc1obj')
  symTab <- nCompiler:::symbolTableClass$new()
  symTab$addSymbol(sym_nc1)
  nCompiler:::resolveTBDsymbols(symTab)
  expect_equal(symTab$getSymbol("nc1obj")$genCppVar()$generate(),
               "std::shared_ptr<nClass_1> nc1obj")

  nCompiler:::resetLabelFunctionCreators()
  nc1 <- nClass(
      Cpublic = list(a = 'numericScalar')
  )
  sym_nc1 <- nCompiler:::type2symbol('nc1', 'nc1obj')
  symTab <- nCompiler:::symbolTableClass$new()
  symTab$addSymbol(sym_nc1)
  project_env <- new.env()
  project_env$known_nClasses <- new.env()
  nCompiler:::resolveTBDsymbols(symTab, project_env = project_env)
  expect_equal(symTab$getSymbol("nc1obj")$genCppVar()$generate(),
               "std::shared_ptr<nClass_1> nc1obj")
})

test_that("symbolTBD works with a function from a call", {
  nCompiler:::resetLabelFunctionCreators()
  myenv <- new.env()
  myenv$nc1 <- nClass(
    Cpublic = list(a = 'numericScalar')
  )
  sym_nc1 <- nCompiler:::type2symbol("myenv$nc1()", "nc1obj")
  res <- sym_nc1$resolveSym()
  expect_true(inherits(res, "symbolNC"))
  expect_equal(res$type, myenv$nc1$classname)

  nCompiler:::resetLabelFunctionCreators()
  myenv <- new.env()
  myenv$nc1 <- nClass(
      Cpublic = list(a = 'numericScalar')
  )
  sym_nc1 <- nCompiler:::type2symbol("myenv$nc1()", "nc1obj")
  project_env <- new.env()
  project_env$known_nClasses <- new.env()
  res <- sym_nc1$resolveSym(project_env)
  expect_true(inherits(res, "symbolNC"))
  expect_equal(res$type, myenv$nc1$classname)
})

cat("\nSee test-types.R for notes on remaining issues to test.\n")
## Need to make refs work with slices and blocks
## Need to enforce that with isRef=TRUE, no form of default value is valid,
## even in a nimble type declaration.
## Need to add expect_error tests.
## Need to test case of providing a nType object directly.

test_that("types as objects works using {{ }}", {
  make_nf <- function() {
    my_type <- nType(integerVector())
    nf <- nFunction(
      fun = function(ivec = {{my_type}}) {
        # x <- 1L
        nCpp("x = ivec+1;", types = list(x = quote(T(my_type))))
        return(x+1L);
        returnType({{my_type}})
      }
    )
  }
  nf <- make_nf()
  nfC <- nCompile(nf)
  expect_identical(nfC(1:3), 3:5)
})

test_that("types as objects works using T()", {
  make_nf <- function() {
    my_type <- nType(integerVector())
    nf <- nFunction(
      fun = function(ivec = T(my_type)) {
        # x <- 1L
        nCpp("x = ivec+1;", types = list(x = quote(T(my_type))))
        return(x+1L);
        returnType(T(my_type))
      }
    )
  }
  nf <- make_nf()
  nfC <- nCompile(nf)
  expect_identical(nfC(1:3), 3:5)
})

test_that("types as objects work with an nClass", {
  make_nf <- function() {
    ## It looks like arg and return types use the closure because
    # they are determined at definition
    # but nCpp uses the nClass parent_env because it is processed later.
    my_type <- nType(integerVector()) # to be ignored because method closure is replaced with parent_env
    nf <- nFunction(
      fun = function(ivec = T(my_type)) {
        nCpp("x = ivec.cast<double>()+1.2;", types = list(x = quote(T(my_type))))
        return(ivec+1L);
        returnType(T(my_type))
      }
    )
  }
  nf <- make_nf()
  myenv <- new.env()
  myenv$my_type <- nType(numericVector())
  nc <- nClass(
    Cpublic = list(
      cust = quote(nCpp('double')),
      v = quote(T(my_type)),
      nf = nf),
    env = myenv
  )
  ncC <- nCompile(nc)
  obj <- ncC$new()
  obj$nf(1:3)
  expect_identical(obj$nf(1:3), 2:4)
})

test_that("string type works in nFunctions", {
    foo <- nFunction(
        fun=function(mystr = 'string'){
            return(mystr)
        }, returnType = 'string'
    )
    cfoo <- nCompile(foo)
    expect_identical(cfoo("hw"),"hw")
})

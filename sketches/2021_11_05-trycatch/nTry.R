# demonstrating error messages

library(nCompiler)

# enable stack tracing
compilerOptions = nOptions()$compilerOptions
compilerOptions$cppStacktrace = TRUE
set_nOption(x = 'compilerOptions', compilerOptions)


#
# demonstrate basic and nested errors
#

mult <- function(x,y) {
  z <- x * y
  return(z)
}

nmult <- nFunction(
  fun = mult,
  argTypes = list(x = 'numericVector()', y = 'numericVector()'),
  returnType = 'numericVector()'
)

testfun <- function(x,y,z) {
  a <- nmult(y, z)
  res <- x + a
  return(res)
}

nFun <- nFunction(
  fun = testfun,
  argTypes = list(x = 'numericVector()',
                  y = 'numericVector()',
                  z = 'numericVector()'), 
  returnType = 'numericVector()'
)

cFun <- nCompile(nmult, nFun)

# demonstrate nested error
x <- 1:5
y <- 1:5
z <- 1:10
cFun$nFun_2_NFID_2(x, y, z)

# demonstrate basic error
x <- 1:10
y <- 1:5
z <- 1:5
cFun$nFun_2_NFID_2(x, y, z)


#
# demonstrate that error labels are generated once per line
#

testfun_condensed <- function(x,y,z) {
  res <- x + nmult(y, z)
  return(res)
}

nFun_condensed <- nFunction(
  fun = testfun_condensed,
  argTypes = list(x = 'numericVector()',
                  y = 'numericVector()',
                  z = 'numericVector()'), 
  returnType = 'numericVector()'
)

cFun_condensed <- nCompile(nmult, nFun_condensed)

# demonstrate nested error
x <- 1:5
y <- 1:5
z <- 1:10
cFun_condensed$nFun_3_NFID_3(x, y, z)

# demonstrate basic error
x <- 1:10
y <- 1:5
z <- 1:5
cFun_condensed$nFun_3_NFID_3(x, y, z)


#
# demonstrate errors within loops
#

testfun2 <- function(x,y) {
  res <- x
  for(i in 1:10) 
    res <- res + y
  
  return(res)
}

testfun3 <- function(x,y) {
  res <- x
  i <- 1
  while(i < 10) {
    res <- res + y
    i <- i + 1
  }
  return(res)
}

nFun2 <- nFunction(
  fun = testfun2,
  argTypes = list(x = 'numericVector()',
                  y = 'numericVector()'), 
  returnType = 'numericVector()'
)

nFun3 <- nFunction(
  fun = testfun3,
  argTypes = list(x = 'numericVector()',
                  y = 'numericVector()'), 
  returnType = 'numericVector()'
)

cFun2 <- nCompile(nFun2, nFun3)

# demonstrate error in for loop
x <- 1:10
y <- 1:5
cFun2$nFun_4_NFID_4(x, y)

# demonstrate error in while loop
x <- 1:10
y <- 1:5
cFun2$nFun_5_NFID_5(x, y)


#
# recompile without stack trace
#

# disable stack tracing
compilerOptions = nOptions()$compilerOptions
compilerOptions$cppStacktrace = FALSE
set_nOption(x = 'compilerOptions', compilerOptions)

cFunPlain <- nCompile(nmult, nFun, nFun2, nFun3)

# demonstrate nested error
x <- 1:5
y <- 1:5
z <- 1:10
cFunPlain$nFun_2_NFID_2(x, y, z)

# demonstrate basic error
x <- 1:10
y <- 1:5
z <- 1:5
cFunPlain$nFun_2_NFID_2(x, y, z)

# demonstrate error in for loop
x <- 1:10
y <- 1:5
cFunPlain$nFun_3_NFID_3(x, y)

# demonstrate error in while loop
x <- 1:10
y <- 1:5
cFunPlain$nFun_4_NFID_4(x, y)



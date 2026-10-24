library(nCompiler)

library(Matrix)

set.seed(1)
x <- c(1, 3, 3, 4)
     num <- c(1, 2, 2, 1)
     adj <- c(2, 1,3, 2,4, 3)

spmat <- matrix(0, 4, 4)
spmat[1,2] <- spmat[2,1] <- spmat[2,3] <- spmat[3,2] <- spmat[3,4] <- spmat[4,3] <- -1
diag(spmat) <- c(1,2,2,1)
spmatS <- Matrix(spmat, sparse = TRUE)  # dsCMatrix (symmetric)

i <- c(1,1,2,2,2,3,3,3,4,4)
j <- c(1,2,1,2,3,2,3,4,3,4)
values <- c(1,-1,-1,2,-1,-1,2,-1,-1,1)
spmatG <- sparseMatrix(i=i,j=j,x=values)  # dgCMatrix (general)

values <- c(1,.6,.6,1,.3,.3,1,-.3,-.3,1)
spmatGpd <- sparseMatrix(i=i,j=j,x=values)  # dgCMatrix (general)


mat1 <- matrix(rnorm(16), 4,4)
mat2 <- matrix(rnorm(16), 4,4)
mat3 <- matrix(rnorm(4*100000), 4,100000)
vec <- rnorm(4)

matpd <- crossprod(mat1)


test_that("interfacing objects to/from R", {
## returning sparse matrix to R (seen above) -> dgCMatrix
nf <- nFunction(
    fun = function(Q1 = 'nSparseMatrix', Q2 = 'nSparseMatrix') {
      result <- Q1 %*% Q2
      return(result)
    }, returnType = 'nSparseMatrix')
cnf <- nCompile(nf)

truth <- spmatG %*% spmatG
expect_identical(nf(spmatG, spmatG), truth)
expect_identical(cnf(spmatG, spmatG), truth)

# cnf(spmatS, spmatS)  # Error: need dgCMatrix

## returning dense chol -> dense mat
nf = nFunction(
    function(X = 'numericMatrix') {
        ch <- chol(X)
      return(ch)
}, returnType='numericMatrix'
)
cnf <-nCompile(nf)
truth <- chol(matpd)
expect_identical(nf(matpd), truth)
expect_equal(cnf(matpd), truth)


## returning (and passing in) sparse chol factor

nf <- nFunction(
  fun = function(Q = 'nSparseMatrix') {
    ch <- Cholesky(Q) 
    return(ch)
  }, returnType = 'sparseCholFactor' 
)
cnf <- nCompile(nf)
truth <- Cholesky(spmatGpd)
expect_identical(nf(spmatGpd), truth)
spChFac <- cnf(spmatGpd)
expect_true(inherits(spChFac, "sparseCholFactor"))

nf2 = nFunction(
    function(ch = 'sparseCholFactor', b = 'numericMatrix') {
        result <- nSolve(ch, b)
      return(result)
}, returnType='numericMatrix'
)
cnf2 <- nCompile(nf2)

ch <- Cholesky(spmatGpd)
truth <- as.matrix(solve(ch, matrix(vec,4)))
expect_error(nf2(spChFac, matrix(vec, 4)), "not supported for Cholesky factor")
expect_equal(cnf2(spChFac, matrix(vec, 4)), truth)


nf <- nFunction(
  fun = function(X = 'numericMatrix') {
    ch <- Cholesky(X) 
    return(ch)
  }, returnType = 'denseCholFactor' 
)
cnf <- nCompile(nf) 

truth <- Cholesky(matpd)
expect_identical(nf(matpd), truth)
dChFac <- cnf(matpd)
expect_true(inherits(dChFac, "denseCholFactor"))

nf2 = nFunction(
    function(ch = 'denseCholFactor', b = 'numericMatrix') {
        result <- nSolve(ch, b)
      return(result)
}, returnType='numericMatrix'
)
cnf2 <- nCompile(nf2)

ch <- Cholesky(matpd)
truth <- as.matrix(solve(ch, matrix(vec,4)))
expect_error(nf2(dChFac, matrix(vec, 4)), "not supported for Cholesky factor")
expect_equal(cnf2(dChFac, matrix(vec, 4)), truth)


})

test_that("matrix multiplication", {

## all dense

### mat*mat
nf <- nFunction(
  function(X = 'numericMatrix', Y = 'numericMatrix') {
    result <- X %*% Y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)

truth <- mat1 %*% mat2
expect_identical(nf(mat1, mat2), truth)
expect_equal(cnf(mat1, mat2), truth)

truth <- mat1[1:2,] %*% mat2[,1:2]  # non-square
expect_identical(nf(mat1[1:2,], mat2[,1:2]), truth)
expect_equal(cnf(mat1[1:2,], mat2[,1:2]), truth)

### mat*vec (returns mat)
nf <- nFunction(
  function(X = 'numericMatrix', y = 'numericVector') {
    result <- X %*% y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)

truth <- mat1 %*% vec
expect_identical(nf(mat1, vec), truth)
expect_equal(cnf(mat1, vec), truth)

### mat*vec, then manual demote
nf <- nFunction(
  function(X = 'numericMatrix', y = 'numericVector') {
    result <- (X %*% y)[,1]
    return(result)
  }, returnType = 'numericVector'
)
cnf <- nCompile(nf)
truth <- (mat1 %*% vec)[,1]
expect_identical(nf(mat1, vec), truth)
expect_equal(cnf(mat1, vec), truth)

### vec*mat (makes conformable; returns mat)
nf <- nFunction(
  function(x = 'numericVector', Y = 'numericMatrix') {
    result <- x %*% Y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)

truth <- vec %*% mat1
expect_identical(nf(vec, mat1), truth)
expect_equal(cnf(vec, mat1), truth)

### vec*vec; inner-product (returns 1x1 mat)
nf <- nFunction(
  function(x = 'numericVector', y = 'numericVector') {
    result <- x %*% y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)

truth <- vec %*% vec
expect_identical(nf(vec, vec), truth)
expect_equal(cnf(vec, vec), truth)

## sparse cases

### sparse mat * dens mat = dens mat

nf <- nFunction(
    fun = function(Q = 'nSparseMatrix', X = 'numericMatrix') {
      result <- Q %*% X
      return(result)
    }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

truth <- spmatG %*% mat1
expect_identical(nf(spmatG, mat1), truth)
expect_equal(cnf(spmatG, mat1), as.matrix(truth))  # Note lack of output types being the same for uncompiled/compiled.

### sparse mat * sparse mat = sparse mat
nf <- nFunction(
    fun = function(Q1 = 'nSparseMatrix', Q2 = 'nSparseMatrix') {
      result <- Q1 %*% Q2
      return(result)
    }, returnType = 'nSparseMatrix')
cnf <- nCompile(nf)

truth <- spmatG %*% spmatG
expect_identical(nf(spmatG, spmatG), truth)
expect_identical(cnf(spmatG, spmatG), truth)


### dense mat * sparse mat = dens mat
nf <- nFunction(
    fun = function(Q = 'nSparseMatrix', X = 'numericMatrix') {
      result <- X %*% Q
      return(result)
    }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

truth <- mat1 %*% spmatG
expect_identical(nf(spmatG, mat1), truth)
expect_equal(cnf(spmatG, mat1), as.matrix(truth))  # Note lack of output types being the same for uncompiled/compiled.

### sparse*vec = dens mat

nf <- nFunction(
    fun = function(Q = 'nSparseMatrix', x = 'numericVector') {
      result <- Q %*% x
      return(result)
    }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

truth <- spmatG %*% vec
expect_identical(nf(spmatG, vec), truth)
expect_equal(cnf(spmatG, vec), as.matrix(truth))  # Note lack of output types being the same for uncompiled/compiled.


### vec*sparse = dens mat (makes conformable)

nf <- nFunction(
    fun = function(Q = 'nSparseMatrix', x = 'numericVector') {
      result <- x %*% Q
      return(result)
    }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

truth <- vec %*% spmatG
expect_identical(nf(spmatG, vec), truth)
expect_equal(cnf(spmatG, vec), as.matrix(truth))  # Note lack of output types being the same for uncompiled/compiled.

### sparse chol factor: should multiply by L (use case: sparse covariance to generate MVN)

nf <- nFunction(
  fun = function(Q = 'nSparseMatrix', X = 'numericMatrix') {
    ch <- Cholesky(Q)
    result <- ch %*% X
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

L <- t(chol(as.matrix(spmatGpd)))
result1 <- L%*%mat3

result2 <- nf(spmatGpd, mat3)  # the same correlation structure but different deviates 
result3 <- cnf(spmatGpd, mat3) # the same correlation structure but different deviates 

expect_equal(cov(t(result1)), cov(as.matrix(t(result2))), tolerance = 2e-2)
expect_equal(cov(t(result1)), cov(t(result3)), tolerance = 2e-2)


nf <- nFunction(
  fun = function(Q = 'nSparseMatrix', x = 'numericVector') {
    ch <- Cholesky(Q)
    result <- ch %*% x
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

expect_identical(dim(cnf(spmatGpd, vec)), c(4L,1L))  # the same correlation structure but different deviates


### dense chol factor: should multiply by L (use case: dense covariance to generate MVN)

nf <- nFunction(
  fun = function(X = 'numericMatrix', Y = 'numericMatrix') {
    ch <- Cholesky(X)
    result <- ch %*% Y
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

L <- t(chol(matpd))
result1 <- L%*%mat3

result2 <- nf(matpd, mat3)  # the same correlation structure but different deviates 
result3 <- cnf(matpd, mat3) 

expect_equal(cov(t(result1)), cov(as.matrix(t(result2))), tolerance = 1e-2)
expect_equal(result1, result3)   # Eigen (dense) LLT does not pivot, so matches chol().


nf <- nFunction(
  fun = function(X = 'numericMatrix', x = 'numericVector') {
    ch <- Cholesky(X)
    result <- ch %*% x
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

expect_identical(dim(cnf(crossprod(mat1), vec)), c(4L, 1L))

nf <- nFunction(
  fun = function(X = 'numericMatrix', Q = 'nSparseMatrix') {
    ch <- Cholesky(X)
    result <- ch %*% Q
    return(result)
  }, returnType = 'numericMatrix')
expect_error(cnf <- nCompile(nf), "is not supported")
expect_true(inherits(nf(matpd, spmatGpd), 'dgeMatrix'))  # Cholesky pivots, so doesn't match values from base R.

nf <- nFunction(
  fun = function(Q = 'nSparseMatrix', P = 'nSparseMatrix') {
    ch <- Cholesky(Q)
    result <- ch %*% P
    return(result)
  }, returnType = 'nSparseMatrix')
expect_error(cnf <- nCompile(nf), "is not supported")
expect_true(inherits(nf(spmatGpd, spmatGpd), 'dgCMatrix')) # Cholesky pivots, so doesn't match values from base R.



### error trap cases: 2nd arg is cho factor

nf <- nFunction(
  function(X = 'numericMatrix', Y = 'numericMatrix') {
    ch <- Cholesky(Y)
    result <- X %*% ch
    return(result)
  }, returnType = 'numericMatrix'
)
expect_error(cnf <- nCompile(nf), "is not supported")
expect_error(nf(mat1, matpd), "implemented only for")

## dense chol factor cases

nf = nFunction(
    function(X = 'numericMatrix') {
      ch <- chol(X)
      return(ch)
}, returnType='numericMatrix'
)
cnf <-nCompile(nf)

U <- chol(matpd)
expect_identical(nf(matpd), U)
expect_equal(cnf(matpd), U)

nf <- nFunction(
    fun = function(X = 'numericMatrix') {
      ch <- Cholesky(X) # denseChol(X) 
      return(ch)
    }, returnType = 'denseCholFactor')
cnf <- nCompile(nf)
expect_true(inherits(cnf(matpd), "denseCholFactor"))


## sparse chol factor cases

nf <- nFunction(
    fun = function(Q = 'nSparseMatrix') {
      ch <- Cholesky(Q) # sparseChol(Q) 
      return(ch)
    }, returnType = 'sparseCholFactor')
cnf <- nCompile(nf)
expect_true(inherits(cnf(spmatGpd), "sparseCholFactor"))


### chol(sparse) correctly errors
nf <- nFunction(
    fun = function(Q = 'nSparseMatrix') {
      ch <- chol(Q) 
      return(ch)
    }, returnType = 'numericMatrix')
expect_error(cnf <- nCompile(nf), "is not supported")


nf <- nFunction(
  function(x = 'numericVector') {
    ch <- chol(x)
    return(ch)
  }, returnType = 'numericVector'
)
expect_error(cnf <- nCompile(nf), "is not possible")


nf <- nFunction(
  function(x = 'numericVector') {
    ch <- Cholesky(x)
    return(ch)
  }, returnType = 'numericVector'
)
expect_error(cnf <- nCompile(nf), "is not possible")

})

test_that("solving", {

## nSolve

### dense cases

nf <- nFunction(
  fun = function(X = 'numericMatrix', Y = 'numericMatrix') {
    result <- nSolve(X, Y)
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

truth <- solve(matpd, mat2)
expect_identical(nf(matpd, mat2), truth)
expect_equal(nf(matpd, mat2), truth)

nf <- nFunction(
  fun = function(X = 'nMatrix', y = 'numericVector') {
    result <- nSolve(X, y)
    return(result)
  }, returnType = 'numericVector')
cnf <- nCompile(nf)

truth <- solve(matpd, vec)
expect_identical(nf(matpd, vec), truth)
expect_equal(nf(matpd, vec), truth)


### Note that Matrix::solve(dcGMatrix,y) works (using LU)
### but we haven't created a path via Eigen to do that without
### using Cholesky, since LU is less efficient.
nf <- nFunction(
  fun = function(Q = 'nSparseMatrix', Y = 'numericMatrix') {
    result <- nSolve(Q, Y)
    return(result)
  }, returnType = 'numericMatrix')
expect_error(cnf <- nCompile(nf), "first use `Cholesky`")
result <- as.matrix(solve(spmatGpd, mat1))
expect_equal(nf(spmatGpd, mat1), result)

nf <- nFunction(
  fun = function(Q = 'nSparseMatrix', y = 'numericVector') {
    result <- nSolve(Q, y)
    return(result)
  }, returnType = 'numericVector')
expect_error(cnf <- nCompile(nf), "first use `Cholesky`")
result <- solve(spmatGpd, vec)
expect_equal(nf(spmatGpd, vec), result)



### chol factor cases

#### sparse: does solve(U^t U, Y); need to document that is is solving with Q not with U.
nf <- nFunction(
  fun = function(Q = 'nSparseMatrix', Y = 'numericMatrix') {
    ch <- Cholesky(Q)
    result <- nSolve(ch, Y)
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

U <- Cholesky(spmatGpd)
truth <- Matrix::solve(U, mat2)
U2 <- chol(as.matrix(spmatGpd))
truth2 <- backsolve(U2, forwardsolve(t(U2),mat2))
expect_identical(nf(spmatGpd, mat2), truth)
expect_equal(as.matrix(nf(spmatGpd, mat2)), truth2)
expect_equal(cnf(spmatGpd, mat2), as.matrix(truth))
expect_equal(cnf(spmatGpd, mat2), truth2)


nf <- nFunction(
  fun = function(Q = 'nSparseMatrix', y = 'numericVector') {
    ch <- Cholesky(Q)
    result <- nSolve(ch, y)
    return(result)
  }, returnType = 'numericVector')
cnf <- nCompile(nf)

truth2 <- backsolve(U2, forwardsolve(t(U2), vec))
expect_equal(nf(spmatGpd, vec), truth2)
expect_equal(cnf(spmatGpd, vec), truth2)

#### dense
nf <- nFunction(
  fun = function(X = 'numericMatrix', Y = 'numericMatrix') {
    ch <- Cholesky(X)
    result <- nSolve(ch, Y)
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

U <- Cholesky(matpd)
truth <- Matrix::solve(U, mat2)
U2 <- chol(as.matrix(matpd))
truth2 <- backsolve(U2, forwardsolve(t(U2),mat2))
expect_identical(nf(matpd, mat2), truth)
expect_equal(as.matrix(nf(matpd, mat2)), truth2)
expect_equal(cnf(matpd, mat2), as.matrix(truth))
expect_equal(cnf(matpd, mat2), truth2)


nf <- nFunction(
  fun = function(X = 'numericMatrix', y = 'numericVector') {
    ch <- Cholesky(X)
    result <- nSolve(ch, y)
    return(result)
  }, returnType = 'numericVector')
cnf <- nCompile(nf)

truth2 <- backsolve(U2, forwardsolve(t(U2),vec))
expect_equal(nf(matpd, vec), truth2)
expect_equal(cnf(matpd, vec), truth2)



nf <- nFunction(
  fun = function(X = 'numericMatrix', Q = 'nSparseMatrix') {
    ch <- Cholesky(X)
    result <- nSolve(ch, Q)
    return(result)
  }, returnType = 'numericMatrix')
expect_error(cnf <- nCompile(nf), "is not supported")
result <- solve(matpd, spmatGpd)
expect_equal(nf(matpd, spmatGpd), result)

nf <- nFunction(
  fun = function(X = 'numericMatrix') {
    ch <- Cholesky(X)
    result <- nSolve(ch, ch)
    return(result)
  }, returnType = 'numericMatrix')
expect_error(cnf <- nCompile(nf), "is not supported")
expect_error(nf(matpd), "not implemented")

nf <- nFunction(
  fun = function(Q = 'nSparseMatrix') {
    ch <- Cholesky(Q)
    result <- nSolve(ch, ch)
    return(result)
  }, returnType = 'numericMatrix')
expect_error(cnf <- nCompile(nf), "is not supported")
expect_error(nf(spmatGpd), "not implemented")

## nBacksolve

### dense cases

nf <- nFunction(
  fun = function(X = 'numericMatrix', Y = 'numericMatrix') {
    result <- nBacksolve(X, Y)
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

U <- chol(matpd)
truth <- backsolve(U, mat2)
expect_identical(nf(U, mat2), truth)
expect_equal(cnf(U, mat2), truth)


nf <- nFunction(
  fun = function(X = 'numericMatrix', Y = 'numericVector') {
    result <- nBacksolve(X, Y)
    return(result)
  }, returnType = 'numericVector')
cnf <- nCompile(nf)

U <- chol(matpd)
truth <- backsolve(U, vec)
expect_identical(nf(U, vec), truth)
expect_equal(cnf(U, vec), truth)


### chol factor cases: should backsolve against U (use case: sparse/dense precision to generate MVN)

### TODO: should probably error trap uncompiled execution with Cholesky factor RHS.

#### sparse cases

nf <- nFunction(
  fun = function(Q = 'nSparseMatrix', Y = 'numericMatrix') {
    ch <- Cholesky(Q)
    result <- nBacksolve(ch, Y)
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

ch <- chol(as.matrix(spmatGpd))
result1 <- backsolve(ch, mat3)

result2 <- nf(spmatGpd, mat3) # the same cov structure but different deviates 
result3 <- cnf(spmatGpd, mat3) # the same cov structure but different deviates 

expect_equal(cov(t(result1)), cov(t(as.matrix(result2))), tol = 1e-1)
expect_equal(cov(t(result1)), cov(t(result3)), tol = 1e-1)


nf <- nFunction(
  fun = function(Q = 'nSparseMatrix', y = 'numericVector') {
    ch <- Cholesky(Q)
    result <- nBacksolve(ch, y)
    return(result)
  }, returnType = 'numericVector')
cnf <- nCompile(nf)

expect_identical(length(nf(spmatGpd, vec)), 4L)
expect_identical(length(cnf(spmatGpd, vec)), 4L)


#### dense
nf <- nFunction(
  fun = function(X = 'numericMatrix', Y = 'numericMatrix') {
    ch <- Cholesky(X)
    result <- nBacksolve(ch, Y)
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

ch <- chol(matpd)
result1 <- backsolve(ch, mat3)

result2 <- nf(matpd, mat3)  # the same cov structure but different deviates
result3 <- cnf(matpd, mat3) 

expect_equal(cov(t(result1)), cov(t(as.matrix(result2))), tol = 1e-2)
expect_equal(result1, result3)  # Eigen LLT does not pivot, so matches chol().


nf <- nFunction(
  fun = function(X = 'numericMatrix', y = 'numericVector') {
    ch <- Cholesky(X)
    result <- nBacksolve(ch, y)
    return(result)
  }, returnType = 'numericVector')
cnf <- nCompile(nf)

result <- backsolve(chol(matpd), vec)
expect_silent(nf(matpd, vec))  # the same cov structure but different deviates
expect_equal(cnf(matpd, vec), result)

## nForwardsolve

### dense cases

nf <- nFunction(
  fun = function(X = 'numericMatrix', Y = 'numericMatrix') {
    result <- nForwardsolve(X, Y)
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)

L <- t(chol(matpd))
truth <- forwardsolve(L, mat2)

expect_identical(nf(L, mat2), truth)
expect_equal(cnf(L, mat2), truth)


nf <- nFunction(
  fun = function(X = 'numericMatrix', y = 'numericVector') {
    result <- nForwardsolve(X, y)
    return(result)
  }, returnType = 'numericVector')
cnf <- nCompile(nf)

L <- t(chol(matpd))
truth <- forwardsolve(L, vec)

expect_identical(nf(L, vec), truth)
expect_equal(cnf(L, vec), truth)

nf <- nFunction(
  fun = function(X = 'numericMatrix', y = 'numericVector') {
    ch <- Cholesky(X)
    result <- nForwardsolve(ch, y)
    return(result)
  }, returnType = 'numericVector')
expect_error(cnf <- nCompile(nf), "not supported")
expect_error(nf(matpd, vec), "not implemented")

nf <- nFunction(
  fun = function(Q = 'nSparseMatrix', y = 'numericVector') {
    ch <- Cholesky(Q)
    result <- nForwardsolve(ch, y)
    return(result)
  }, returnType = 'numericVector')
expect_error(cnf <- nCompile(nf), "not supported")
expect_error(nf(matpd, vec), "not implemented")

})

test_that("nLogdet", {

### dense

nf <- nFunction(
  fun = function(X = 'numericMatrix') {
    result <- nLogdet(X)
    return(result)
  }, returnType = 'numericScalar')
cnf <- nCompile(nf)

truth <- 2*sum(log(diag(chol(matpd))))
expect_equal(nf(matpd), truth)
expect_equal(cnf(matpd), truth)



### sparse

nf <- nFunction(
  fun = function(Q = 'nSparseMatrix') {
    result <- nLogdet(Q)
    return(result)
  }, returnType = 'numericScalar')
cnf <- nCompile(nf) 

truth <- 2*sum(log(diag(chol(spmatGpd))))
expect_equal(nf(spmatGpd), truth)
expect_equal(cnf(spmatGpd), truth)



### chol factor cases

nf <- nFunction(
    fun = function(Q = 'nSparseMatrix') {
        ch <- Cholesky(Q)
        out <- nLogdet(ch)  # This is the logdet of ch, not of Q (unlike nSolve(ch, x), which actually does solve(Q,x)).
        return(out)
    }, returnType = 'numericScalar')
cnf <- nCompile(nf)

truth <- sum(log(diag(chol(as.matrix(spmatGpd)))))
expect_equal(nf(spmatGpd), truth)
expect_equal(cnf(spmatGpd), truth)

### dense

nf <- nFunction(
    fun = function(X = 'numericMatrix') {
        ch <- Cholesky(X)
        out <- nLogdet(ch) # this is the logdet of ch, not of X
        return(out)
    }, returnType = 'numericScalar')
cnf <- nCompile(nf)

truth <- sum(log(diag(chol(matpd))))
expect_equal(nf(matpd), truth)
expect_equal(cnf(matpd), truth)

})


test_that("element-wise arithmetic", {

nf <- nFunction(
  function(X = 'numericMatrix', Y = 'numericMatrix') {
    result <- X + Y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_identical(nf(mat1, mat2), mat1+mat2)
expect_identical(cnf(mat1, mat2), mat1+mat2)
expect_identical(nf(mat1[1:2,], mat2[1:2,]), mat1[1:2,]+mat2[1:2,])
expect_identical(cnf(mat1[1:2,], mat2[1:2,]), mat1[1:2,]+mat2[1:2,])
expect_error(nf(mat1[1:2,], mat2[1:3,]), "non-conformable")
expect_error(cnf(mat1[1:2,], mat2[1:3,]))  # not conformable; dim mismatch

nf <- nFunction(
  function(X = 'numericMatrix', y = 'numericVector') {
    result <- X + y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_identical(nf(mat1, vec), mat1+vec)
expect_error(cnf(mat1, vec))  # Error: nCompiler::binaryOpReshape - Tensors have unequal size.

##### We don't error out at compilation stage above because these next two are legitimate:
expect_identical(cnf(mat1[,1, drop=FALSE], vec), matrix(mat1[,1]+vec, 4))
expect_identical(cnf(mat1[1,, drop=FALSE], vec), matrix(mat1[1,]+vec, 1))

nf <- nFunction(
  function(X = 'numericMatrix', Y = 'nSparseMatrix') {
    result <- X + Y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
result <- mat1+spmatG
expect_identical(nf(mat1, spmatG), result)
expect_identical(cnf(mat1, spmatG), as.matrix(result))

result <- mat1[1:2,] + spmatG[1:2,]
expect_identical(nf(mat1[1:2,], spmatG[1:2,]), result)
expect_identical(cnf(mat1[1:2,], spmatG[1:2,]), as.matrix(result))

nf <- nFunction(
  function(X = 'numericMatrix', Y = 'nSparseMatrix') {
    result <- Y + X
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
result <- spmatG+mat1
expect_identical(nf(mat1, spmatG), result)
expect_identical(cnf(mat1, spmatG), as.matrix(result))

nf <- nFunction(
  function(X = 'nSparseMatrix', Y = 'nSparseMatrix') {
    result <- X + Y
    return(result)
  }, returnType = 'nSparseMatrix'
)
cnf <- nCompile(nf)
result <- spmatG+spmatGpd
expect_identical(nf(spmatG,spmatGpd), result)
expect_identical(cnf(spmatG,spmatGpd), result)

nf <- nFunction(
  function(Q = 'nSparseMatrix', y = 'numericVector') {
    result <- Q + y
    return(result)
  }, returnType = 'numericMatrix'
)
expect_error(cnf <- nCompile(nf), "not supported")
expect_identical(nf(spmatG, vec), spmatG + vec)

nf <- nFunction(
  function(Q = 'nSparseMatrix', X = 'numericMatrix') {
    ch <- Cholesky(Q)
    result <- ch + X
    return(result)
  }, returnType = 'numericMatrix'
)
expect_error(cnf <- nCompile(nf), "not supported")
expect_error(nf(spmatGpd, vec), "non-numeric argument")

# subtraction

nf <- nFunction(
  function(X = 'numericMatrix', Y = 'numericMatrix') {
    result <- X - Y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_identical(nf(mat1,mat2), mat1-mat2)
expect_identical(cnf(mat1,mat2), mat1-mat2)

result <- mat1[1:2,] - mat2[1:2,]  # non-square
expect_identical(nf(mat1[1:2,], mat2[1:2,]), result)
expect_identical(cnf(mat1[1:2,], mat2[1:2,]), result)
expect_error(nf(mat1[1:2,], mat2[1:3,]), "non-conformable")
expect_error(cnf(mat1[1:2,], mat2[1:3,])) # Error: dimensions_match(m_leftImpl.dimensions(), m_rightImpl.dimensions())

nf <- nFunction(
  function(X = 'numericMatrix', y = 'numericVector') {
    result <- X - y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_identical(nf(mat1,vec), mat1-vec)
expect_error(cnf(mat1, vec))  # Error: nCompiler::binaryOpReshape - Tensors have unequal size.

nf <- nFunction(
  function(X = 'numericMatrix', Y = 'nSparseMatrix') {
    result <- X - Y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
result <- mat1 - spmatG
expect_identical(nf(mat1, spmatG), result)
expect_identical(cnf(mat1, spmatG), as.matrix(result))
result <- mat1[1:2,] - spmatG[1:2,]
expect_identical(nf(mat1[1:2,], spmatG[1:2,]), result)
expect_identical(cnf(mat1[1:2,], spmatG[1:2,]), as.matrix(result))

nf <- nFunction(
  function(X = 'numericMatrix', Y = 'nSparseMatrix') {
    result <- Y - X
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
result <- spmatG - mat1
expect_identical(nf(mat1, spmatG), result)
expect_identical(cnf(mat1, spmatG), as.matrix(result))


nf <- nFunction(
  function(X = 'nSparseMatrix', Y = 'nSparseMatrix') {
    result <- X - Y
    return(result)
  }, returnType = 'nSparseMatrix'
)
cnf <- nCompile(nf)
result <- spmatG - spmatGpd
expect_identical(nf(spmatG, spmatGpd), result)
expect_identical(cnf(spmatG, spmatGpd), result)


# Hadamard product

nf <- nFunction(
  function(X = 'numericMatrix', Y = 'numericMatrix') {
    result <- X * Y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)

truth <- mat1*mat2
expect_identical(nf(mat1,mat2), truth)
expect_equal(cnf(mat1,mat2), truth)
truth <- mat1[1:2,]*mat2[1:2,]
expect_identical(nf(mat1[1:2,],mat2[1:2,]), truth)
expect_equal(cnf(mat1[1:2,],mat2[1:2,]), truth)

expect_error(nf(mat1[1:2,], mat2[1:3,]), "non-conformable")
expect_error(cnf(mat1[1:2,], mat2[1:3,]))


nf <- nFunction(
  function(X = 'numericMatrix', y = 'numericVector') {
    result <- X * y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_identical(nf(mat1,vec), mat1*vec)
expect_error(cnf(mat1, vec))  # Error: nCompiler::binaryOpReshape - Tensors have unequal size.

nf <- nFunction(
  function(X = 'numericMatrix', Q = 'nSparseMatrix') {
    result <- X * Q
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)

truth <- mat1 * spmatG
expect_identical(nf(mat1,spmatG), truth)
expect_equal(cnf(mat1,spmatG), as.matrix(truth))
expect_equal(cnf(mat1[1:2,], spmatG[1:2,]), as.matrix(mat1[1:2,] * spmatG[1:2,]))

nf <- nFunction(
  function(X = 'numericMatrix', Q = 'nSparseMatrix') {
    result <- Q * X
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)

expect_identical(nf(mat1,spmatG), truth)
expect_equal(cnf(mat1,spmatG), as.matrix(truth))
expect_equal(cnf(mat1[1:2,], spmatG[1:2,]), as.matrix(mat1[1:2,] * spmatG[1:2,]))


nf <- nFunction(
  function(X = 'nSparseMatrix', y = 'numericVector') {
    result <- X * y
    return(result)
  }, returnType = 'numericMatrix'
)
expect_error(cnf <- nCompile(nf), "not supported")
expect_identical(nf(spmatG, vec), spmatG*vec)



nf <- nFunction(
  function(Q = 'nSparseMatrix', P = 'nSparseMatrix') {
    result <- Q * P
    return(result)
  }, returnType = 'nSparseMatrix'
)
cnf <- nCompile(nf)

truth <- spmatG * spmatGpd
expect_identical(nf(spmatG,spmatGpd), truth)
expect_equal(cnf(spmatG,spmatGpd), truth)

nf <- nFunction(
  function(Q = 'nSparseMatrix', Y = 'numericMatrix') {
    result <- Q / Y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)

truth <- spmatG / mat1
expect_identical(nf(spmatG,mat1), truth)
expect_equal(cnf(spmatG,mat1), as.matrix(truth))

##### This should not work (divide by 0).
nf <- nFunction(
  function(Q = 'nSparseMatrix', P = 'nSparseMatrix') {
    result <- Q / P
    return(result)
  }, returnType = 'nSparseMatrix'
)
expect_error(nf(spmatG/spmatGpd), "not yet implemented")
expect_error(cnf <- nCompile(nf) , "not supported")


# This should not work (divide by 0).
nf <- nFunction(
  function(Q = 'nSparseMatrix', Y = 'numericMatrix') {
    result <- Y / Q
    return(result)
  }, returnType = 'numericMatrix'
)
expect_error(nf(spmatG/spmatGpd), "not yet implemented")
expect_error(cnf <- nCompile(nf) , "not supported")

})

test_that("diagonal", {

# diag (extract)

nf <- nFunction(
    fun = function(X = 'numericMatrix') {
        out <- nDiag(X)
        return(out)
    }, returnType = 'numericVector'
)
cnf <- nCompile(nf)

truth <- diag(mat1)
expect_identical(nf(mat1), truth)
expect_identical(cnf(mat1), truth)
expect_identical(cnf(mat1[,1:3]), truth[1:3])

nf <- nFunction(
    fun = function(X = 'nSparseMatrix') {
        out <- nDiag(X)
        return(out)
    }, returnType = 'numericVector'
)
cnf <- nCompile(nf)

truth <- diag(spmatG)
expect_identical(nf(spmatG), truth)
expect_identical(cnf(spmatG), truth)
expect_identical(cnf(spmatG[,1:3]), truth[1:3])

nf <- nFunction(
    fun = function(X = 'numericMatrix') {
        ch <- Cholesky(X)
        out <- nDiag(ch)
        return(out)
    }, returnType = 'numericVector'
)
expect_error(nf(matpd), "not implemented")  # TODO: to support this we'd need to deal with LL/LDL and permutation.
expect_error(cnf <- nCompile(nf), "not supported")  # TODO: to support this we'd need to permute.

# diag (create)

nf <- nFunction(
  function(n = 'numericScalar') {
    result <- nDiag(n)
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_identical(nf(4), diag(4))
expect_identical(cnf(4), diag(4))


nf <- nFunction(
  function(x = 'numericVector') {
    result <- nDiag(x)
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_identical(nf(vec), diag(vec))
expect_identical(cnf(vec), diag(vec))

nf <- nFunction(
  function(x = 'numericVector', nr = 'numericScalar', nc = 'numericScalar') {
    result <- nDiag(x, nr, nc)
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_identical(nf(vec, 4, 5), diag(vec, 4, 5))
expect_identical(cnf(vec, 4, 5), diag(vec, 4, 5))
expect_error(cnf(vec, 5, 5), "does not match")

nf <- nFunction(
  function(x = 'numericVector') {
    result <- nDiag(x)
    return(result)
  }, returnType = 'nSparseMatrix'
)
expect_error(cnf <- nCompile(nf), "does not match the nFunction's return type")

# nDiagonal to create a diagonal matrix (sparse only)

nf <- nFunction(
  function(n = 'numericScalar') {
    result <- nDiagonal(n)
    return(result)
  }, returnType = 'nSparseMatrix'
)
cnf <- nCompile(nf)
result <- sparseMatrix(i=1:4,j=1:4,x=1)
expect_identical(nf(4), result)
expect_identical(cnf(4), result)

nf <- nFunction(
  function(x = 'numericVector') {
    result <- nDiagonal(x)
    return(result)
  }, returnType = 'numericMatrix'
)
expect_error(cnf <- nCompile(nf), "does not match the nFunction's return type")

nf <- nFunction(
  function(x = 'numericVector') { 
      result <- nDiagonal(x)
    return(result)
  }, returnType = 'nSparseMatrix'
)
cnf <- nCompile(nf)
result <- sparseMatrix(i=1:4,j=1:4,x=vec)
expect_identical(nf(vec), result)
expect_identical(cnf(vec), result)

nf <- nFunction(
  function(x = 'numericVector', nr = 'integerScalar', nc = 'integerScalar') {
      result <- nDiagonal(x, nr, nc)
    return(result)
  }, returnType = 'nSparseMatrix'
)
cnf <- nCompile(nf)
truth <- Matrix(diag(vec, 4, 5),sparse=TRUE)
expect_identical(nf(vec, 4, 5), truth)
expect_identical(cnf(vec, 4, 5), truth)
expect_error(cnf(vec, 5, 5), "does not match")


# nDiagonal to extract (not documented, but we transform during compilation to `nDiag`).

nf <- nFunction(
    fun = function(X = 'numericMatrix') {
        out <- nDiagonal(X)
        return(out)
    }, returnType = 'numericVector'
)
cnf <- nCompile(nf)

truth <- diag(mat1)
expect_identical(nf(mat1), truth)
expect_identical(cnf(mat1), truth)
expect_identical(cnf(mat1[,1:3]), truth[1:3])

nf <- nFunction(
    fun = function(X = 'nSparseMatrix') {
        out <- nDiagonal(X)
        return(out)
    }, returnType = 'numericVector'
)
cnf <- nCompile(nf)

truth <- diag(spmatG)
expect_identical(nf(spmatG), truth)
expect_identical(cnf(spmatG), truth)
expect_identical(cnf(spmatG[,1:3]), truth[1:3])


nf <- nFunction(
    fun = function(X = 'numericMatrix') {
        ch <- Cholesky(X)
        out <- nDiagonal(ch)
        return(out)
    }, returnType = 'numericVector'
)
expect_error(nf(matpd), "not implemented")  # TODO: to support this we'd need to deal with LL/LDL and permutation.
expect_error(cnf <- nCompile(nf), "not supported")  # TODO: to support this we'd need to permute.

})

test_that("transpose", {

nf <- nFunction(
  function(X = 'numericMatrix') {
    result <- t(X)
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_identical(nf(mat1),t(mat1))
expect_identical(cnf(mat1),t(mat1))
expect_identical(nf(mat1[,1:3]),t(mat1[,1:3]))
expect_identical(cnf(mat1[,1:3]),t(mat1[,1:3]))

nf <- nFunction(
  function(x = 'numericVector') {
    result <- t(x)
    return(result)
  }, returnType = 'numericVector'
)
cnf <- nCompile(nf)
expect_identical(nf(vec), matrix(vec,1,4))
expect_identical(cnf(vec), vec)

nf <- nFunction(
  function(x = 'numericScalar') {
    result <- t(x)
    return(result)
  }, returnType = 'numericMatrix'
)
expect_error(cnf <- nCompile(nf), "cannot transpose")
expect_identical(nf(3), matrix(3))


nf <- nFunction(
  function(X = 'numericMatrix') {
    ch <- Cholesky(X)
    result <- t(ch)
    return(0)
  }, returnType = 'numericScalar'
)
expect_error(nf(matpd), "not a matrix")
expect_error(cnf <- nCompile(nf), "not supported")

##### Check that input is not modified.

nf <- nFunction(
  function(X = 'numericMatrix') {
    result <- t(X)
    return(X)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
mat1_save <- mat1  # Just in case.
expect_identical(nf(mat1), mat1)
expect_identical(cnf(mat1), mat1)
expect_identical(mat1, mat1_save)

nf <- nFunction(
  function(X = 'nSparseMatrix') {
    result <- t(X)
    return(X)
  }, returnType = 'nSparseMatrix'
)
cnf <- nCompile(nf)  
tmp <- spmatG
tmp[1,2] = .99
tmp_save <- tmp  # Just in case.
expect_identical(nf(tmp), tmp)
expect_identical(cnf(tmp), tmp)
expect_identical(tmp, tmp_save)

})

test_that("composite/chained operations", {

# These are a fairly haphazard collection.

nf <- nFunction(
    fun = function(Q = 'nSparseMatrix') {
        ch <- Cholesky(Q)
        out <- exp(ch) 
        return(0)
    }, returnType = 'numericScalar')
expect_error(cnf <- nCompile(nf), "not supported")


nf <- nFunction(
  function(X = 'numericMatrix', Y = 'numericMatrix') {
    result <- t(X) %*% Y %*% Y
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_equal(cnf(mat1,mat2), t(mat1)%*%mat2%*%mat2)

nf <- nFunction(
  function(x = 'numericVector') {
      result <- nDiagonal(x+x)
    return(result)
  }, returnType = 'nSparseMatrix'
)
cnf <- nCompile(nf) 
expect_equal(cnf(vec), as(diag(vec+vec), "dgCMatrix"))


nf <- nFunction(
  function(X = 'numericMatrix', Y = 'numericMatrix') {
    result <- chol(X+Y)
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_equal(cnf(matpd,matpd), chol(2*matpd))

nf <- nFunction(
  function(X = 'numericMatrix') {
    result <- exp(X)
    return(result)
  }, returnType = 'numericMatrix'
)
cnf <- nCompile(nf)
expect_equal(cnf(mat1), exp(mat1))

nf <- nFunction(
  function(X = 'numericMatrix', y = 'numericVector') {
    ch <- Cholesky(X)
    result <- nBacksolve(ch, y + y)
    return(result)
  }, returnType = 'numericVector')
cnf <- nCompile(nf)
expect_equal(cnf(matpd, vec), backsolve(chol(matpd), 2*vec))


nf <- nFunction(
  fun = function(X = 'numericMatrix', Y = 'numericMatrix') {
    result <- asSparse(X+Y)
    return(result)
  }, returnType = 'nSparseMatrix')
cnf <- nCompile(nf)
tmp1 <- tmp2 <- matrix(0, 4, 4)
tmp1[1,1] <- 3
tmp2[3,4] <- 2.5
expect_identical(cnf(tmp1, tmp2), as(tmp1+tmp2, 'dgCMatrix'))

nf <- nFunction(
    fun = function(X = 'numericMatrix', Y = 'numericMatrix') {
        result <- nIsSymmetric(X+Y)  # Not currently handled by nCompiler.
        return(result)
    }, returnType = 'logicalScalar')
expect_error(cnf <- nCompile(nf), "No operator definition") 
cnf(crossprod(mat1), crossprod(mat2)) 


##### Let's go crazy.
nf <- nFunction(
  fun = function(X = 'numericMatrix', Y = 'numericMatrix', Z = 'numericMatrix') {
    n <- nDim(X)[1]
    result <- Z + nBacksolve(Cholesky(X+diag(n)), X%*%Y)
    return(result)
  }, returnType = 'numericMatrix')
cnf <- nCompile(nf)  
expect_equal(cnf(matpd,mat1,mat2), mat2 + backsolve(chol(matpd+diag(4)), matpd%*%mat1))

})

# asSparse (with prune), asDense are covered in Josh' tests

# dmnorm with sparse chol

library(Rcpp)
library(testthat)
# general support for sparse matrices
library(Matrix)
# methods to construct specific classes of sparse covariance matrices
library(spam)
library(SparseM)

# Storage format notes:
#  - spam only stores matrices in compressed row storage format
#  - Matrix can store matrices in compressed row/col format (and more), with a 
#    default of compressed col format
#  - SparseM also supports compressed row/col format
#
#  - spam provides methods to convert between sparse matrix classes, such as 
#      spam::as.dgCMatrix.spam(x) to go to Matrix format from spam format.  
#      Other conversions are possible, including to/from spam, Matrix, and 
#      SparseM classes.  The function spam::as.spam can also do some 
#      conversions into spam class objects.  See help('foreign', 'spam') 
#      and help('as.spam', 'spam') for more details.

# Modeling support notes:
#   - for practical use of sparse matrix methods, we will probably need to 
#     support conversion between different classes that represent sparse
#     matrices within R.
# 
#   - spam provides methods to generate sparse matrices useful for analyses, 
#     such as precision matrices for 1st order random walks via 
#     spam::precmat.RW1, and others.  The SparseM package contains generators 
#     for other types of sparse matrices as well.

# Roadmap to extend Rcpp supported types (new, published 4/2021):
#   - overall, though, not sure this necessarily buys us much
# 
#   - some demo code out there suggesting how to build additional support within
#     Rcpp, natively, for different R classes that express sparse matrices:
#     https://www.r-bloggers.com/2021/04/constructing-a-sparse-matrix-class-in-rcpp/
#
#     OR copied here:
#     https://gallery.rcpp.org/articles/sparse-matrix-class/
#
#   - demo code also refers to a forthcoming RcppSparse package, which is on 
#     github and appears to be under active development:
#     https://github.com/zdebruine/RcppSparse.  
#     however, the package appears to be relatively sparse at the moment, and 
#     not published to CRAN
#
#  - additionally, the Rcpp gallery has many different stubs and initial demos 
#    for allowing some basic support for sparse matrices via Eigen:
#    Search "sparse eigen" on https://gallery.rcpp.org

# compile demo code
base_dir = file.path('sketches', '2021_11_19-sparse')
filename <- "sparse_ex.cpp"
sourceCpp(file.path(base_dir, filename))


#
# generate demo objects
#

# size of matrices
m = 7
n = 10
# proportion of non-zero entries
p = .1

# set up a matrix with only a few random non-zero entries
M = matrix(data = 0, nrow = m, ncol = n)
M[sample(x = length(M), size = p * length(M))] = runif(n = p * length(M))

# convert matrix to sparse format, using spam and Matrix classes
M_sparse = Matrix::Matrix(M, sparse = TRUE)
M_sparse_spam = as.spam(M)

# set up a second matrix with only a few random non-zero entries
M2 = matrix(data = 0, nrow = m, ncol = n)
M2[sample(x = length(M2), size = p * length(M2))] = runif(n = p * length(M2))

# convert matrix to sparse format, using spam and Matrix classes
M2_sparse = Matrix::Matrix(M2, sparse = TRUE)
M2_sparse_spam = as.spam(M2)

# build a symmetric matrix, which can be decomposed
Msym = t(M_sparse) %*% M_sparse + diag(ncol(M_sparse))


#
# test sparse matrix methods
#

# RcppEigen supports conversion from dgCMatrix objects only
# (see Table 1, https://cran.r-project.org/web/packages/RcppEigen/vignettes/RcppEigen-Introduction.pdf)
expect_identical(M_sparse + M2_sparse, add(M_sparse, M2_sparse))

# expect error when converting from dgRMatrix object
expect_error(
  add(as.dgRMatrix.spam(M_sparse_spam), as.dgRMatrix.spam(M2_sparse_spam))
)

# expect error when converting from spam object
expect_error(add(M_sparse_spam, M2_sparse_spam))

# multiplication, and addition back-and-forth between R and C++
expect_identical(
  Msym, 
  add(as(Diagonal(ncol(M_sparse)), 'dgCMatrix'), mult(t(M_sparse), M_sparse))
)

# note: Matrix::Diagonal returns a sparse matrix of type ddiMatrix, which 
#   RcppEigen does not support as an argument type
expect_error(mult(Diagonal(ncol(M_sparse)), mult(t(M_sparse), M_sparse)))

# we can recover the cholesky decomposition, where Eigen documentation 
# suggests a preference for LDLt decompositions with pivoting to reduce the 
# bandwidth of the sparse matrix; other decompositions are available
Msym_chol = chol_eigen(Msym)
expect_equal( 
  Msym,
  as(Msym_chol$perm + 1, 'pMatrix') %*% 
    Msym_chol$L %*%
    Diagonal(x = Msym_chol$D) %*% 
    t(Msym_chol$L) %*%
    as(Msym_chol$perm_inv + 1, 'pMatrix')
)

# we can map to/from dense matrices
expect_identical(M, asDense(M_sparse))
expect_identical(M_sparse, asSparse(M))

# we can mix sparse and dense operations
addSparseDense(x = M_sparse, y = M)

# Matrix package will automatically switch between sparse and dense 
# representations depending on which has smaller memory footprint
class(M_sparse + matrix(runif(m*n), nrow = m))
class(M_sparse + M)

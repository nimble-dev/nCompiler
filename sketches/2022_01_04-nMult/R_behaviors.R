# document the decisions that R makes with vector/matrix multiplication

# vector dimension
n = 10

# vector stored as numeric
v = runif(n = n)

# explicit row and column vectors
cv = matrix(data = v, ncol = 1)
rv = matrix(data = v, nrow = 1)

# square matrix
m = matrix(data = runif(n = n^2), nrow = n)

# CODE        INTERPRETATION                TENSOR DIMS     OPERATION
dim(v %*% v)     # LHS: row-vec; RHS: col-vec    1 x 1      (INNER PRODUCT)
dim(v %*% cv)    # LHS: row-vec                  1 x 2      (INNER PRODUCT)
dim(t(v) %*% v)  #               RHS: col-vec    2 x 1      (INNER PRODUCT)
dim(rv %*% v)    #               RHS: col-vec    2 x 1      (INNER PRODUCT)

# CODE        INTERPRETATION                TENSOR DIMS     OPERATION
dim(v %*% m)     # LHS: row-vec                  1 x 2      (MATRIX PRE-MULT.)
dim(m %*% v)     #               RHS: col-vec    2 x 1      (MATRIX POST-MULT.)

# CODE        INTERPRETATION                TENSOR DIMS     OPERATION
dim(v %*% t(v))  # LHS: col-vec                  1 x 2      (OUTER PRODUCT)
dim(v %*% rv)    # LHS: col-vec                  1 x 2      (OUTER PRODUCT)
dim(cv %*% v)    #               RHS: row-vec    2 x 1      (OUTER PRODUCT)

# We see that R "generally" interprets a vector as a row-vec when it appears on 
# the LHS, and as a col-vec when it appears on the RHS, but these rules are 
# reversed for outer products since this is what is required to ensure that all
# operations have conformable dimensions.
# 
# So, I think that the general rule to follow when mapping a 1-dim tensor input 
# to a matrix object is to know whether the object appears on the LHS or RHS of 
# a multiplication, and what the runtime row/col dimensions are for the object  
# on the opposite side of the multiplication operator %*%.
# 
# Furthermore, in all cases, R always promotes the output to a matrix object, 
# regardless if the output can be expressed as matrix, vector, or scalar

# ultimately need to write x * y where both x and y are Eigen Matrix or 
# SparseMatrix types
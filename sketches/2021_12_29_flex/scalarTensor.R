# test ability to assign data between tensors and scalars

library(nCompiler)

identity <- function(x) {
  return(x)
}

nScalarVector <- nFunction(
  fun = identity, 
  argTypes = list(x = 'numericVector'), 
  returnType = 'double'
)

nScalarMatrix <- nFunction(
  fun = identity, 
  argTypes = list(x = 'nMatrix'), 
  returnType = 'double'
)

nVectorMatrix <- nFunction(
  fun = identity, 
  argTypes = list(x = 'nMatrix'), 
  returnType = 'numericVector'
)



cScalarVector <- nCompile(nScalarVector)
cVectorMatrix <- nCompile(nVectorMatrix)



#
#
#

pickOne <- function(x) {
  y <- x[1,1]
  return(y)
}

nPickOne <- nFunction(
  fun = pickOne, 
  argTypes = list(x = 'nMatrix'), 
  returnType = 'double'
)

cPickOne <- nCompile(nPickOne)


#
# assignment from a tensor argument to a local double, which is then returned
#

# y is defined locally as a scalar, and then returned
total <- function(x) {
  y <- sum(x)
  return(y)
}

nTotal <- nFunction(
  fun = total, 
  argTypes = list(x = 'nMatrix'), 
  returnType = 'double'
)

# flex class works for local assignment to a local Scalar variable from an
# Eigen::Tensor<Scalar, 0> object, which is what is returned by the member 
# function sum
cTotal <- nCompile(nTotal)

# buut, the code doesn't run, quite possibly because we don't know how to flex
# to a double?
cTotal(x = runif(10))

system(paste('open', tempdir()))


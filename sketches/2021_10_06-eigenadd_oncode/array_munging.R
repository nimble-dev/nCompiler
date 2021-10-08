#
# Demo 1a. R refuses to add two arrays of completely arbitrary dimension
#

Xdim = c(2,6,3,5)
X = array(data = 1:prod(Xdim), dim = Xdim)

Ydim = c(1,13,19)
Y = array(data = 1:prod(Ydim), dim = Ydim)

# error; non-conformable arrays
Z = X + Y

# error; R does not like that Y has more elements than X
Z = as.matrix(X) + as.numeric(Y)

# ok, but R warns that vector recyling is uneven (and array structure is lost)
Z = as.matrix(Y) + as.numeric(X)

# *should* we implement a componentwise operation when the input tensors
# have different numbers of elements in addition to different numbers of 
# dimensions?  if so, then it seems like we should use vector recycling ideas,
# but as seen above, R has some restrictions here as well.
c(lenX = length(X), lenY = length(Y))


#
# Demo 1b. R still refuses to add two arrays of completely arbitrary dimension
#

Xdim = c(2,6,3,5)
X = array(data = 1:prod(Xdim), dim = Xdim)

Ydim = c(2,90)
Y = array(data = 1:prod(Ydim), dim = Ydim)

# error; non-conformable arrays
Z = X + Y

# error still occurs even though the two arrays have the same number of elements
c(lenX = length(X), lenY = length(Y))

# fine, but of course, the array structure is lost
Z = as.matrix(X) + as.numeric(Y)

#
# Demo 2a: Ways R does munge in useful ways to emulate for binary component-wise operators
#
V1 <- 1:3
M1 <- matrix(1:3, nrow = 1)
M2 <- matrix(1:3, ncol = 1)
M3 <- matrix(1:6, ncol = 2)
M4 <- matrix(1:6, nrow = 1)
A1 <- array(1:3, dim = c(1, 1, 3))
A2 <- array(1:3, dim = c(1, 3, 1))
A3 <- array(1:3, dim = c(3, 1, 1))
A4 <- array(1:6, dim = c(1, 3, 2))

# unless noted with a comment, the following work
# and would make sense to support through compilation
V1 + M1
V1 + M2
M1 + M2 # error: non-comformable
M1 + M3 # error: non-comformable
M1 + M4 # error: non-comformable
V1 + M3 # recycling rule - not to be supported
V1 + A1
V1 + A2
V1 + A3
V1 + A4 # recycling rule - not to be supported
M1 + A1
M1 + A2
M1 + A3

#
# Demo 2a: Ways R does munge in useful ways to emulate for matrix multiplication
#
V1 <- 1:3
M1 <- matrix(1:3, nrow = 1)
M2 <- matrix(1:6, nrow = 2)
M3 <- matrix(1:9, nrow = 3)
M1 %*% V1 # V1 treated as 1-column matrix
M2 %*% V1 # V1 treated as 1-column matrix
M3 %*% V1 # V1 treated as 1-column matrix
V1 %*% M1 # V1 treated as 1-column matrix
V1 %*% M2 # error: non-comformable
V1 %*% M3 # *** V1 treated as 1-row matrix because that conforms with M3 ***

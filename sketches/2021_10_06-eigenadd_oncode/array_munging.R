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

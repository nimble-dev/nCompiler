## These were from working files for general indexing.
## They should be cleaned up into testthat format and compile from the cpp directory.

library(testthat)
library(Rcpp)

sourceCpp('general_indexing_examples.cpp')
NULL
# warming up and making sure Rcpp works:
# Should print: hello world.  first element is 10
hw(10:20)

##### SINGLE CHIPS
# Single access via chip (scalar index)
x <- matrix(1:20, nrow = 4)
ex1(x)
x[3,]
ex1b(x)

# Single assign via chip
v <- x[3,] + 100
ex2(x, v)
ans <- x; ans[3,] <- v
ans
ex2b(x, v)

##### CHIP OF CHIP
## Access
x3 <- array(1:(4*5*6), dim = c(4, 5, 6))
ex1p1(x3)
x3[,4,3]
ex1p1b(x3)

## Assign
v <- x3[,4,3] + 100
test <- ex2p1(x3, v)
ans <- x3
ans[, 4, 3] <- v
max(abs(test - ans))
test2 <- ex2p1b(x3, v)
max(abs(test2 - ans))

###### ELEMENT FROM CHIP OF CHIP USING TENSORREF
## Access
ex1p2(x3)
x3[2, 4, 3]
ex1p2b(x3)

## Assign
test <- ex2p2(x3, 1000)
ans <- x3
ans[2 , 4, 3] <- 1000
max(abs(test - ans))
test2 <- ex2p2b(x3, 1000)
max(abs(test2 - ans))

#####  SINGLE INDEX VECS
# Single access via indexing vector
iv <- c(1, 3) # 2, 4 in R
ex3(x, iv)
x[c(2, 4),]

# Single assign via indexing vector
v <- x[c(2, 4), ] + 100
ex4(x, iv, v)
ans <- x; ans[c(2, 4), ] <- v
ans

#####  INDEX VEC OF INDEX VEC
# Access
iv <- c(1, 3) # 2, 4 in R
iv2 <- c(3, 4) # 4, 5 in R
ex3p1(x, iv, iv2)
x[c(2,4), c(4, 5)]

# Assign
v <- x[c(2,4), c(4, 5)] + 100
ex4p1(x, iv, iv2, v)
ans <- x; ans[c(2,4), c(4, 5)] <- v
ans

###### Index vec of index scalar and vice versa
## Access
ex3p2(x, iv)
x[2, iv+1]
ex4p2(x, iv)

## Assign
v <- x[2, iv+1] + 100
ex3p3(x, iv, v)
ans <- x; ans[2, iv+1] <- v
ans
ex4p3(x, iv, v)

sourceCpp('general_indexing_examples.cpp') #CONVIENT NOT TO FLIP TO TOP
############ SINGLE SEQUENCES
# Single access via sequence
ex5(x)
x[2:3,]
ex5b(x)

# Single access via sequence in two indices
ex5p1(x)
x[2:3, 2:4]

# Single assign via sequence
v <- x[2:3,] + 100
ex6(x, matrix(v, nrow = 1))
ans <- x; ans[2:3,] <- v
ans

############ CHIP OF INDEX SEQ
# Access
ex7(x)
x[2:3, 3]

# Assign
v <- x[2:3, 3] + 100
ex8(x, v)
ans <- x; ans[2:3, 3] <- v
ans

############ INDEX SEQ OF CHIP
# Access
ex7p1(x)
x[2:3, 3]

# Assign
v <- x[2:3, 3] + 100
ex8p1(x, v)
ans <- x; ans[2:3, 3] <- v
ans


############ INDEX VEC OF INDEX SEQ
# Access
iv <- sample(1:3, 8, replace = TRUE)
ex9(x, iv-1)
x[, 2:4][,iv]

# Assign
iv <- c(1, 3)
v <- x[, 2:4][,iv] + 100
ex10(x, iv-1, v)
ans <- x; ans[, 2:4][,iv] <- v
ans

sourceCpp('general_indexing_examples.cpp') #CONVIENT NOT TO FLIP TO TOP

## To-do:
## Last combo is

############ INDEX SEQ OF INDEX VEC

## Work on syntax for readability
## Work on {} syntax

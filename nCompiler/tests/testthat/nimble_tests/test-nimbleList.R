test_that("nimbleList bridge works", {
    ## Passing ... args to nimbleList().
    nc <- nimbleList(x = integer(0), y = double(2))
    
    robj <- nc$new()
    robj$x <- 3
    
    cnc <- nCompile(nc)
    cobj <- cnc$new()
    cobj$x <- 3
    
    mynfun <- nFunction(
        fun = function(vals) {
            onesMatrix <- matrix(value = 1, nrow = 2, ncol = 2)
            vals$y <- onesMatrix
            return(vals)
        }, returnType = 'nc',
        argTypes=list(vals = "nc")
    )
    
    cmynfun <- nCompile(mynfun)

    result <- mynfun(robj)
    cresult <- cmynfun(cobj)
    expect_true(inherits(result, "nClass")) 
    expect_true(inherits(cresult, "CnClass")) 
    expect_identical(mynfun(robj)$y, matrix(1, 2, 2))
    expect_identical(cmynfun(cobj)$y, matrix(1, 2, 2))
    
    ## Passing list of nimbleTypes to nimbleList().
    nimbleListTypes <- list(nimbleType(name = 'x', type = 'integer', dim = 0),
                            nimbleType(name = 'y', type = 'double', dim = 2))
    
    nc <- nimbleList(nimbleListTypes)
    
    robj <- nc$new()
    robj$x <- 3
    
    cnc <- nCompile(nc)
    cobj <- cnc$new()
    cobj$x <- 3

    mynfun <- nFunction(
        fun = function(vals) {
            onesMatrix <- matrix(value = 1, nrow = 2, ncol = 2)
            vals$y <- onesMatrix
            return(vals)
        }, returnType = 'nc',
        argTypes=list(vals = "nc")
    )
    
    cmynfun <- nCompile(mynfun)
    
    expect_true(inherits(result, "nClass")) 
    expect_true(inherits(cresult, "CnClass")) 
    expect_identical(mynfun(robj)$y, matrix(1, 2, 2))
    expect_identical(cmynfun(cobj)$y, matrix(1, 2, 2))
})

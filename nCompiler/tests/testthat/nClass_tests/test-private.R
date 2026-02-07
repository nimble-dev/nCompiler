test_that("Classes with private members -- basic usage", {

    nc1 <- nClass(
        Rpublic = list(
            Rv = 2,
            Rmethod = function(x) Rmethod2(x),
            R_setRv2 = function(x) Rv2 <<- x,   # TODO: we may need to document need for `<<-`.
            R_getRv2 = function() return(Rv2)
        ),
        Rprivate = list(
            Rv2 = 2,
            Rmethod2 = function(x) x+Rv2),
        Cpublic = list(
            Cv = 'numericScalar',
            Cmethod = nFunction(
                fun = function(x) {
                    return(Cmethod2(x))
                },
                argTypes = list(x = 'numericScalar'),
                returnType = 'numericScalar'
            ),
            Cset_Cv2 = nFunction(
                fun = function(x) {
                    Cv2 <<- x                  # TODO: we may need to document need for `<<-`.
                },
                argTypes = list(x = 'numericScalar')
            ),
            Cget_Cv2 = nFunction(
                fun = function() {
                    return(Cv2)
                }, returnType = 'numericScalar'
            )
        ),
        Cprivate = list(
            Cv2 = 'numericScalar',
            Cmethod2 = nFunction(
                fun = function(x) {
                    return(x + Cv2)
                },
                argTypes = list(x = 'numericScalar'),
                returnType = 'numericScalar'
            )
        )
    )

    ## Uncompiled operations.
    
    Robj <- nc1$new()
    expect_identical(Robj$Rv, 2)
    expect_identical(Robj$Rv2, NULL)  # private, so not found
    expect_identical(Robj$Cv, "numericScalar")
    expect_identical(Robj$Cv2, NULL)  # private, so not found
    expect_identical(Robj$Rmethod2, NULL)
    expect_identical(Robj$Cmethod2, NULL)
    Robj$Rv <- 5
    expect_identical(Robj$Rv, 5)
    expect_error(Robj$Rv2 <- 5, "cannot add bindings to a locked environment")

    Robj$R_setRv2(77)
    expect_identical(Robj$R_getRv2(), 77)
    expect_identical(Robj$Rmethod(3), 80)

    Robj$Cv <- 55
    expect_identical(Robj$Cv, 55)
    
    Robj$Cset_Cv2(33)
    expect_identical(Robj$Cget_Cv2(), 33)

    expect_identical(Robj$Cmethod(12), 45)
    
    ## Compiled operations.
    
    cnc1 <- nCompile(nc1)
    Cobj <- cnc1$new()     
    
    expect_identical(Cobj$Rv, 2)
    expect_identical(Cobj$Rv2, NULL)  # private, so not found
    expect_identical(Cobj$Cv2, NULL)  # private, so not found
    expect_identical(Cobj$Rmethod2, NULL)
    expect_identical(Cobj$Cmethod2, NULL)
    Cobj$Rv <- 5
    expect_identical(Cobj$Rv, 5)
    expect_error(Cobj$Rv2 <- 5, "cannot add bindings to a locked environment")

    Cobj$R_setRv2(77)
    expect_identical(Cobj$R_getRv2(), 77)
    expect_identical(Cobj$Rmethod(3), 80)

    Cobj$Cset_Cv2(88)
    expect_identical(Cobj$Cget_Cv2(), 88)

    expect_identical(Cobj$Cmethod(12), 100)

})

test_that("Access to private members", {
    
    nc1 <- nClass(
        Cpublic = list(
            Cv = 'numericScalar',
            Cmethod = nFunction(
                fun = function(x) {
                    return(Cmethod2(x))
                },
                argTypes = list(x = 'numericScalar'),
                returnType = 'numericScalar'
            ),
            Cset_Cv2 = nFunction(
                fun = function(x) {
                    Cv2 <<- x                
                },
                argTypes = list(x = 'numericScalar')
            ),
            Cget_Cv2 = nFunction(
                fun = function() {
                    return(Cv2)
                }, returnType = 'numericScalar'
            )
        ),
        Cprivate = list(
            Cv2 = 'numericScalar',
            Cmethod2 = nFunction(
                fun = function(x) {
                    return(x + Cv2)
                },
                argTypes = list(x = 'numericScalar'),
                returnType = 'numericScalar'
            )
        )
    )

    ## Can't access private members from external function.
    myfun <- nFunction(
        fun = function(obj, x) {
            return(obj$Cmethod2(x))
        }, argTypes = list(obj = 'nc1', x='numericScalar'),
        returnType = 'numericScalar'
    )
    ## We get a WARN in testing output from this. Should clean things up if possible.
    expect_error(cmyfun <- nCompile(myfun), "could not be found")

    ## Can't access private members from objects of a different class.
    ncOuter <- nClass(
        Cpublic = list(
            Cfoo = nFunction(
                fun = function(obj, x) {
                    return(obj$Cmethod2(x))
                },
                argTypes = list(obj = 'nc1', x='numericScalar'),
                returnType = 'numericScalar'
            )
        )
    )
   
    expect_error(cncOuter <- nCompile(ncOuter), "could not be found")

    ## Can access private members from separate objects of the same class.
    nc1 <- nClass(
        Cpublic = list(
            Cmethod = nFunction(
                fun = function(obj, x) {
                    return(obj$Cmethod2(x))
                },
                argTypes = list(obj = 'nc1', x = 'numericScalar'),
                returnType = 'numericScalar'
            )
        ),
        Cprivate = list(
            Cmethod2 = nFunction(
                fun = function(x) {
                    return(x + 3)
                },
                argTypes = list(x = 'numericScalar'),
                returnType = 'numericScalar'
            )
        )
    )
    cnc1 <- nCompile(nc1)
    Cobj1 <- cnc1$new()
    Cobj2 <- cnc1$new()
    expect_identical(Cobj2$Cmethod(Cobj1, 5), 8)
    
})

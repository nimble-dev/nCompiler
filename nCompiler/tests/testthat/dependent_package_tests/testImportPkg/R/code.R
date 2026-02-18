## If a package happens to define an nClass within a function and
## a method uses member data, R CMD check does static code checking
## that reports a problem with the member data seen as a global variable
## unless one uses `utils::globalVariables`. See issue #101.

fun_with_nClass_using_member_data <- function(x, returnGen = FALSE, returnObj = FALSE) {
    nc <- nClass(
        Rpublic = list(
            Rv = NULL,
            Rfoo = function(x) {
                return(x+Rv)
            }
        ),
        Cpublic = list(
            Cv = 'numericScalar',
            Ca = 'numericVector',
            Cfoo = nFunction(
                fun = function(x) {
                    return(x+Cv)
          },
          argTypes = list(x = 'numericScalar'),
          returnType = 'numericScalar')
        )
    )
    return(nc)
}

## Recommend this to developers of nCompiler-depending packages
## to avoid R CMD check issues.
utils::globalVariables(c('Rv','Cv','Ca'))
## Otherwise one gets:
## * checking R code for possible problems ... NOTE
## fun_with_nClass_using_member_data : <anonymous>: no visible binding for
##   global variable ‘Rv’
## fun_with_nClass_using_member_data : <anonymous>: no visible binding for
##   global variable ‘Cv’
## Undefined global functions or variables:
##   Cv Rv

## Define nClass and nFunctions in package.

nc <- nClass(
    Rpublic = list(
        Rv = NULL,
        Rfoo = function(x) {
            y <- nRep(1, 2)
            return(x+y)
        },
        Rfoo2 = function(x) {
            return(x + Rv)
        },
        Rfoo3 = function(x) {
            return(x + innerFun(x) + outerFun(x))
        }
    ),
    Cpublic = list(
        Cv = 'numericScalar',
        Ca = 'numericVector',
        Cfoo = nFunction(
            fun = function(x) {
                y <- nRep(1, 2)
                return(x+y)
            },
            argTypes = list(x = 'numericScalar'),
            returnType = 'numericVector'), 
        Cfoo2 = nFunction(
            fun = function(x) {
                return(x+Cv)
            },
            argTypes = list(x = 'numericScalar'),
            returnType = 'numericScalar'),
        Cfoo3 = nFunction(
            fun = function(x) {
                return(x+innerFun(x)+outerFun(x))
            },
            argTypes = list(x = 'numericScalar'),
            returnType = 'numericScalar')
    )
)

innerFun <- nFunction(
    fun = function(x) {
        return(x+1)
    },
    argTypes = list(x = 'numericScalar'),
    returnType = 'numericScalar'
)

outerFun <- nFunction(
    fun = function(x) {
        return(innerFun(x)+1)
    },
    argTypes = list(x = 'numericScalar'),
    returnType = 'numericScalar'
)

fun_using_package_nClass <- function(x, returnGen = FALSE, returnObj = FALSE) {
    Cnc <- nCompile(nc)
    if(returnGen)
        return(Cnc)
    Robj <- nc$new()
    Cobj <- Cnc$new()
    if(returnObj)
        return(list(Robj = Robj, Cobj = Cobj))

    Robj$Rv <- 2
    Robj$Cv <- 2
    Cobj$Rv <- 2
    Cobj$Cv <- 2
    return(c(Robj$Rfoo(x), Robj$Cfoo(x), Cobj$Rfoo(x), Cobj$Cfoo(x),
             Robj$Rfoo2(x), Robj$Cfoo2(x), Cobj$Rfoo2(x), Cobj$Cfoo2(x),
             Robj$Rfoo3(x), Robj$Cfoo3(x), Cobj$Rfoo3(x), Cobj$Cfoo3(x)))
}

fun_using_package_nFun <- function(x, returnFun = FALSE) {
    cOuterFun <- nCompile(outerFun)
    if(returnFun)
        return(cOuterFun)
    return(c(outerFun(x), cOuterFun(x)))
}
               

## Code that defines and uses a user-defined operator.

## Uncompiled execution function.
nimArray <- function(value, dim) {
    if(length(dim) == 1)
        return(rep(value, dim))
    array(value, dim)
}

nimArrayHandler <- function(code,...) {
    code$name <- 'nArray'
    NULL
}

opDefs <-
    list(nimArray =
             list(
                 matchDef = function(value=0, dim=c(1,1), init=TRUE,
                                     fillZeros=TRUE, recycle=TRUE, nDim,
                                     type="double") {},
                 simpleTransformations=list(handler = nimArrayHandler)))

testfun <- nFunction(
    function() {
        x <- innerFun(3)
        ans <- nimArray(6, dim = 2)
        return(ans)
        returnType('double(1)')
    }
)

fun_using_testfun_with_op <- function(returnFun = FALSE) {
    ctestfun <- nCompile(testfun)
    if(returnFun)
        return(ctestfun)
    ctestfun()
}

nc_userOp <- nClass(
    Cpublic = list(
        foo = nFunction(
            function() {
                ans <- nimArray(6, dim = 2)
                return(ans)
                returnType('double(1)')
            }
        )
    ))

fun_using_class_with_op <- function(returnGen = FALSE, returnObj = FALSE) {
    Cnc <- nCompile(nc_userOp)
    if(returnGen)
        return(Cnc)
    Robj <- nc_userOp$new()
    Cobj <- Cnc$new()
    if(returnObj)
        return(list(Robj = Robj, Cobj = Cobj))
    return(c(Robj$foo(), Cobj$foo()))
}


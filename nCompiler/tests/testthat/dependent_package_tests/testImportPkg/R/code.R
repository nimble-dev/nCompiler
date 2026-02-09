fun_using_nClass <- function(x, returnGen = FALSE, returnObj = FALSE) {
    nc <- nClass(
        Rpublic = list(
            Rv = NULL,
            Rfoo = function(x) {
                y <- nRep(1, 2)
                return(x+y)
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
          returnType = 'numericVector')
        )
    )

    Cnc <- nCompile(nc)
    if(returnGen)
        return(list(Rgen = nc, Cgen = Cnc))
    Robj <- nc$new()
    Cobj <- Cnc$new()
    if(returnObj)
        return(list(Robj = Robj, Cobj = Cobj))
    
    return(c(Robj$Rfoo(x), Robj$Cfoo(x), Cobj$Rfoo(x), Cobj$Cfoo(x)))
}

fun_using_nFun <- function(x, returnFun = FALSE) {
    nf <- nFunction(
        fun = function(x) {
            y <- nRep(1, 2)
            return(x+y)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericVector')
    cnf <- nCompile(nf)
    if(returnFun)
        return(list(nf = nf, cnf = cnf))
    return(c(nf(x), cnf(x)))
}
               
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

nc <- nClass(
    Cpublic = list(
        Cfoo = nFunction(
            fun = function(x) {
                return(x+1)
            },
            argTypes = list(x = 'numericScalar'),
            returnType = 'numericScalar')
    )
)

fun_using_nClass_in_pkg <- function(x, returnGen = FALSE, returnObj = FALSE) {

    Cnc <- nCompile(nc)
    if(returnGen)
        return(Cnc)
    Robj <- nc$new()
    Cobj <- Cnc$new()
    if(returnObj)
        return(list(Robj = Robj, Cobj = Cobj))
    
    return(c(Robj$Cfoo(x), Cobj$Cfoo(x)))
}

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
               

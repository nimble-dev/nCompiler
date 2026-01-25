myfun <- function(x) {
    nc <- nCompiler::nClass(
        Rpublic = list(
            Rv = NULL,
            Rfoo = function(x) x+1
        ),
        Cpublic = list(
            Cv = 'numericScalar',
            Ca = 'numericVector',
            Cfoo = nCompiler::nFunction(
                fun = function(x) {
                    return(x+1)
          },
          argTypes = list(x = 'numericScalar'),
          returnType = 'numericScalar')
        )
    )
    Robj <- nc$new()
    Cnc <- nCompiler::nCompile(nc)
    Cobj <- Cnc$new()

    return(Cobj$Cfoo(x))
}

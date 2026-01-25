myfun <- function(x) {
    nc <- nClass(
        Rpublic = list(
            Rv = NULL,
            Rfoo = function(x) x+1
        ),
        Cpublic = list(
            Cv = 'numericScalar',
            Ca = 'numericVector',
            Cfoo = nFunction(
                fun = function(x) {
                    return(x+1)
          },
          argTypes = list(x = 'numericScalar'),
          returnType = 'numericScalar')
        )
    )
    Robj <- nc$new()
    Cnc <- nCompile(nc)
    Cobj <- Cnc$new()

    return(Cobj$Cfoo(x))
}


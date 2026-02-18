library(nCompiler)

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
                return(x+y+Cv)
            },
            argTypes = list(x = 'numericScalar'),
            returnType = 'numericVector')
    )
)

innerFun <- nFunction(
    fun = function(x) {
        y <- nRep(1, 2)
        return(x+y)
    },
    argTypes = list(x = 'numericScalar'),
    returnType = 'numericVector'
)

outerFun <- nFunction(
    fun = function(x) {
        ans <- innerFun(x)
        return(ans)
    },
    argTypes = list(x = 'numericScalar'),
    returnType = 'numericVector'
)

writePackage(nc, innerFun, outerFun, pkgName = 'writePackageTestPkg', dir = '/tmp', modify = 'clear')

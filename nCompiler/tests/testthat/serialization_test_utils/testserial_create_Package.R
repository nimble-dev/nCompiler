# The target behavior here is as follows:
# 1. A package is created containing an nClass definition (this file)
# 2. An object of that nClass is instantiated and saved, without needing to 
#    create a new package containing the class defn
# 3. The serialized nClass is read back and itself points to its definition
#    package, which is assumed to be available. If not, an error is thrown.

library(nCompiler)
library(testthat)

set_nOption("serialize", TRUE)

nc1Packaged <- nClass(
  classname = "nc1Packaged",
  Cpublic = list(
    Cv = 'numericScalar()',
    Cx = 'integerScalar',
    Cfoo = nFunction(
      fun = function(x) {
        return(x+3)
      },
      argTypes = list(x = 'numericScalar'),
      returnType = 'numericScalar')
  )
)

writePackage(nc1Packaged, 
             package.name = "saveNClassPkg",
             dir = 'testserial_nCompInternalOnly')
buildPackage("saveNClassPkg", dir = "testserial_nCompInternalOnly")

expect_true(require(saveNClassPkg))


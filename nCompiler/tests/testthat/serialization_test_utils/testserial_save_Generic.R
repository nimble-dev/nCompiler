# This script is used for the purposes of testing saving/loading of nClasses
# across R sessions.

library(nCompiler)

# Define the nClass
nc1 <- nClass(
  classname = "nc1_test",
  Cpublic = list(
    Cv = 'numericScalar()',
    Cx = 'integerScalar',
    Cfoo = nFunction(
      fun = function(x) {
        return(x+1)
      },
      argTypes = list(x = 'numericScalar'),
      returnType = 'numericScalar')
  )
)
set_nOption('serialize', TRUE)

# Instantiate an object of this nClass
nc1_generator <- nCompile_nClass(nc1, interface = "generic")
obj <- nc1_generator[[1]]()
value(obj, "Cx") <- 10
value(obj, "Cv") <- 0.1

# Save the nClass instance and its member data
file = file.path('testserial_nCompInternalOnly', 'savedObj.rds')

save_nClass(ncObj = obj, ncDef = nc1,
            file = file, 
            package.name = "savedObjPkg", 
            dir = tempdir())

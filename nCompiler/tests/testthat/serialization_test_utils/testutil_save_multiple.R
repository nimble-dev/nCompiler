# This script is used for the purposes of testing saving/loading of nClasses
# across R sessions.

# At the moment, this workflow can only handle one nClass per session, because
# the nComp_serialize and nComp_deserialize functions are generated into the
# global environment

library(nCompiler)
set_nOption('serialize', TRUE)

# Define the nClass
nc1 <- nClass(
  classname = "nc1_test",
  Cpublic = list(
    C1 = 'numericScalar()',
    C2 = 'numericScalar',
    Cfoo = nFunction(
      fun = function(x) {
        return(x+1)
      },
      argTypes = list(x = 'numericScalar'),
      returnType = 'numericScalar')
  )
)

# Instantiate an object of this nClass
nc1_generator <- nCompile_nClass(nc1, interface = "generic")
obj1 <- nc1_generator[[1]]()
value(obj1, "C2") <- 100
value(obj1, "C1") <- 3.14

# Save the nClass instance and its data
save_nClass(ncObj = obj1, ncDef = nc1,
            file = "serialization_test_utils/savedObj1.rds", 
            package.name = "savedObjPkgMult1", 
            dir = tempdir())

# Within the same session, save a 2nd nclass with its data
nc2 <- nClass(
  classname = "nc2_test",
  Cpublic = list(
    Cm = 'numericScalar()',
    Cs = 'integerScalar',
    Cfoo = nFunction(
      fun = function(x) {
        return(x+1000)
      },
      argTypes = list(x = 'numericScalar'),
      returnType = 'numericScalar')
  )
)

# Instantiate an object of this nClass
nc2_generator <- nCompile_nClass(nc2, interface = "generic")
obj2 <- nc2_generator[[1]]()
value(obj2, "Cm") <- 2.17
value(obj2, "Cs") <- 3
save_nClass(ncObj = obj2, ncDef = nc2,
            file = "serialization_test_utils/savedObj2.rds", 
            package.name = "savedObjPkgMult2", 
            dir = tempdir())

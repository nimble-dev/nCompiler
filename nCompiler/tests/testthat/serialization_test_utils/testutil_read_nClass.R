library(nCompiler)
library(testthat)

set_nOption('serialize', TRUE)

deserialized <- read_nClass(file = "serialization_test_utils/savedObj.rds", 
                            package.name = "savedObjPkg", 
                            dir = tempdir())

expect_equal(value(deserialized, "Cx"), 10)
expect_equal(value(deserialized, "Cv"), 0.1)
expect_equal(method(deserialized, "Cfoo")(1.5), 2.5)

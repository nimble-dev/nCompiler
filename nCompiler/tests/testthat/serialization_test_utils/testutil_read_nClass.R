library(nCompiler)
library(testthat)

set_nOption('serialize', TRUE)
file = system.file(file.path('tests', 'testthat', 'serialization_test_utils', 
                             'savedObj.rds'), 
                   package = 'nCompiler')

deserialized <- read_nClass(file = file, 
                            package.name = "savedObjPkg", 
                            dir = tempdir())

expect_equal(value(deserialized, "Cx"), 10)
expect_equal(value(deserialized, "Cv"), 0.1)
expect_equal(method(deserialized, "Cfoo")(1.5), 2.5)

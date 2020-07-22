library(nCompiler)
library(testthat)

set_nOption('serialize', TRUE)
file1 = system.file(file.path('tests', 'testthat', 'serialization_test_utils', 
                             'savedObj1.rds'), 
                   package = 'nCompiler')
file2 = system.file(file.path('tests', 'testthat', 'serialization_test_utils', 
                              'savedObj2.rds'), 
                    package = 'nCompiler')


deserialized1 <- read_nClass(file = file1, 
                             package.name = "savedObjPkgMult1", 
                             dir = tempdir())

expect_true(nCompiler:::is.loadedObjectEnv(deserialized1))
expect_equal(value(deserialized1, "C2"), 100)
expect_equal(value(deserialized1, "C1"), 3.14)
expect_equal(method(deserialized1, "Cfoo")(1.5), 2.5)

deserialized2 <- read_nClass(file = file2, 
                             package.name = "savedObjPkgMult2", 
                             dir = tempdir())

expect_true(nCompiler:::is.loadedObjectEnv(deserialized2))
expect_equal(value(deserialized2, "Cm"), 2.17)
expect_equal(value(deserialized2, "Cs"), 3)
expect_equal(method(deserialized2, "Cfoo")(1.5), 1001.5)

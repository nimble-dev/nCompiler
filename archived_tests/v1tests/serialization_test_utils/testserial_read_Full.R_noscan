library(nCompiler)
library(testthat)

set_nOption('serialize', TRUE)
file = file.path('testserial_nCompInternalOnly', 'savedObj_Full.rds')

deserialized <- read_nClass(file = file)

expect_equal(deserialized$Cx, 10)
expect_equal(deserialized$Cv, 0.1)
expect_equal(deserialized$Cfoo(1.5), 2.5)

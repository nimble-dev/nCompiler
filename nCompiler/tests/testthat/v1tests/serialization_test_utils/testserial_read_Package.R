library(nCompiler)
library(testthat)
set_nOption("serialize", TRUE)
expect_true(require(saveNClassPkg))

file <- file.path("testserial_nCompInternalOnly", "nc1_from_pkg.Rds")

my_nc1_read  <- read_nClass(file = file)

expect_equal(my_nc1_read$Cx, 1000)
expect_equal(my_nc1_read$Cv, -25)
expect_equal(my_nc1_read$Cfoo(10), 13)

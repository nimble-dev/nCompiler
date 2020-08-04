library(nCompiler)
library(testthat)
set_nOption("serialize", TRUE)
require(saveNClassPkg)
expect_true(require(saveNClassPkg))

my_nc1 <- saveNClassPkg::nc1Packaged$new()

my_nc1$Cx <- 1000
my_nc1$Cv <- -25
expect_equal(my_nc1$Cfoo(10), 13)

file <- file.path("testserial_nCompInternalOnly", "nc1_from_pkg.Rds")

save_nClass(ncObj = my_nc1, 
            classname = "nc1Packaged",
            file = file, 
            packageWithDefn = "saveNClassPkg")


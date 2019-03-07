context("Testing Rcpp_nCompilerPacket for Rcpp-managed compilation")

test_that("basics",
          {
            cpp1 <- nCompiler:::Rcpp_nCompilerPacket(
              filebase = "cpp1",
              cppContent = paste(
                '#include "cpp1.h"',
                "// [[Rcpp::export]]",
                "double add1(double x) {",
                "return(x+1);",
                "}",
                sep = "\n"),
              hContent = paste(
                "#ifndef __cpp1_H",
                "#define __cpp1_H",
                "double add1(double x);",
                "#endif",
                sep = "\n"
              )
            )
            test1 <- cpp_nCompiler(cpp1,
                                   verbose = FALSE,
                                   rebuild = TRUE)
            expect_equal(test1(1.5),
                         2.5)    
          })

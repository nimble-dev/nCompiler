context("cpp_nFunctionClass")

test_that("basic cpp_nFunctionClass works",
{
    args <- nCompiler:::symbolTableClass$new()
    args$addSymbol(nCompiler:::cppDouble(name = "x"))
    code <- nCompiler:::cppCodeBlockClass$new()
    code$symbolTable <- nCompiler:::symbolTableClass$new()
    code$symbolTable$addSymbol(nCompiler:::cppDouble(name = "x"))
    code$code <- nCompiler:::nParse(quote({y <- x; return(y);}))
    returnType <- nCompiler:::cppDouble()
    
    expect_identical(code$generate(),
                     list("double x;",
                          "y = x;",
                          "return(y);"))
    
    test <- nCompiler:::cpp_nFunctionClass$new(
        name = "test"
      , returnType = returnType
      , args = args
      , code = code
    )
    
    expect_identical(test$generate(declaration = TRUE),
                     "double  test ( double x );")
    expect_identical(
        test$generate()
       ,
        list(
            "double  test ( double x )  {",
            "double x;",
            "y = x;",
            "return(y);",
            "}"
        )
    )
})


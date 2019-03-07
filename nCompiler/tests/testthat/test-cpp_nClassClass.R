context("cpp_nClassClass")

test_that("basic cpp_nClassClass",
{
    args <- nCompiler:::symbolTableClass$new()
    args$addSymbol(nCompiler:::cppDouble(name = "x"))
    code <- nCompiler:::cppCodeBlockClass$new()
    code$symbolTable <- nCompiler:::symbolTableClass$new()
    code$symbolTable$addSymbol(nCompiler:::cppDouble(name = "x"))
    code$code <- nCompiler:::nParse(quote({y <- x + z; return(y);}))
    returnType <- nCompiler:::cppDouble()

    memberSymTab <- nCompiler:::symbolTableClass$new()
    memberSymTab$addSymbol(nCompiler:::cppDouble(name = "z"))
    
    expect_identical(
        code$generate()
       ,
        list("double x;",
             "y = x + z;",
             "return(y);")
    )

    args$setParentST(memberSymTab)
    
    memberFunctionDef <-
        nCompiler:::cpp_nFunctionClass$new(
            name = "test"
          , returnType = returnType
          , args = args
          , code = code
        )

    classDef <-
        nCompiler:::cpp_nClassClass$new(
            name = "test_class"
          , cppFunctionDefs = list(test = memberFunctionDef)
          , symbolTable = memberSymTab
        )
    
    ## This fails because of issues of character vs. list
    expect_identical(
        as.character(classDef$generate(FALSE))
       ,
       c(
         "double  test_class::test ( double x )  {",
         "double x;",
         "y = x + z;",
         "return(y);",
         "}"
       )
    )
    expect_identical(
        as.character(classDef$generate(TRUE))
       ,
        c(
          "class test_class  {",
          "public:",
          "  double z;",
          "double  test ( double x );",
          "};"
        )
    )
})

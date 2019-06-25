context("Testing cppVariable classes")
## More tests should be filled in here.

test_that("cppVariable classes work", 
{
    TR1 <- nCompiler:::cppEigenTensorRef("arg1",
                                         nDim = 2,
                                         'double')
    expect_identical(
        TR1$generate()
       ,
        "Eigen::Tensor<double, 2> & arg1"
    )
}
)

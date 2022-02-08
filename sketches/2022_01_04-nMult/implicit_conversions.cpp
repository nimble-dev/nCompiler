// Demonstrate template functions which evaluate Tensor expression arguments
// that avoid creating temporary copies of already-evaluated, concrete Tensor
// objects

#include <RcppEigen.h>
#include <Rcpp.h>

#include <nCompiler/nCompiler_Eigen.h>
#include <nCompiler/tensorOperations.h>

using namespace Rcpp;

// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]

/**
 * IMPLEMENTING A BAD TEMPLATE PROGRAMMING SOLUTION
 *
 * Implicitly convert an arbitrary input to an Eigen::Tensor object.
 *
 * Behaviors:
 *   1) For a tensor expression input, this function will evaluate x
 *   2) For an Eigen::Tensor input, this function will return a copy of x
 *
 *  This is a demonstration function that we do not want to use in production
 *  because it will create an unnecessary copy of an evaluated tensor input.
 *
 * @tparam TensorXpr  Eigen::Tensor or tensor expression object type
 * @param x reference to input tensor
 * @return An Eigen::Tensor object containing an evaluated/copied version of x
 */
template<typename TensorXpr>
Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions> eval_copy(
    TensorXpr & x
) {
    Rcpp::Rcout << "copy/conversion" << std::endl;
    return x;
}

/**
 * Implicitly convert an arbitrary input to an Eigen::Tensor object, as needed
 *
 * The compiler uses implicit conversion to decide how to convert the TensorXpr
 * input x to an Eigen::Tensor<Scalar, NumDimensions> object. Naturally, the
 * conversion process allocates memory for the Tensor, then populates the
 * Tensor's entries.
 *
 * Uses SFINAE to restrict the template function (nominally) to only work with
 * Tensor expression inputs vs. with evaluated, concrete Eigen::Tensor objects.
 *
 * If the input is already an Eigen::Tensor object, the desired behavior is to
 * return a reference to the object (implemented in an overloaded function)
 * because the input is already an Eigen::Tensor object, which is the desired
 * output class.  Calling this function on an Eigen::Tensor object (i.e., if
 * SFINAE restriction is removed) only consumes memory and time, creating a
 * copy of an otherwise reasonable input.
 *
 * @tparam TensorXpr Nominally, an unevaluated tensor expression
 */
template<
    typename TensorXpr,
    typename std::enable_if<
        !std::is_base_of<
            Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions>,
            TensorXpr
        >::value,
        TensorXpr
    >::type* = nullptr
>
Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions> eval(
    TensorXpr & x
) {
    Rcpp::Rcout << "implicit conversion" << std::endl;
    return x;
}

/**
 * Return a reference to an Eigen::Tensor object, avoiding copying the input
 *
 * Uses SFINAE to restrict the template function to only work with Eigen::Tensor
 * inputs vs. with Tensor expression objects.
 *
 * If the input is a Tensor expression object, the desired behavior is to return
 * an Eigen::Tensor object with the results of the evaluated Tensor expression
 * (implemented in an overloaded function).  Calling this function on a Tensor
 * expression object (i.e., if SFINAE restriction is removed) will cause type
 * errors when eval() is nested within other functions that use Tensors, but
 * require evaluated inputs, such as wrappers for matrix multiplication and
 * linear solvers.
 *
 * @tparam TensorXpr An Eigen::Tensor object type
 */
template<
    typename TensorXpr,
    typename std::enable_if<
        std::is_base_of<
            Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions>,
            TensorXpr
        >::value,
        TensorXpr
    >::type* = nullptr
>
TensorXpr& eval(TensorXpr & x) {
    Rcpp::Rcout << "reference return" << std::endl;
    return x;
}

/**
 * Constant version of eval() with Eigen::Tensor inputs.
 *
 * See documentation for eval() with Eigen::Tensor inputs for more details.
 *
 * @tparam TensorXpr An Eigen::Tensor object type
 */
template<
    typename TensorXpr,
    typename std::enable_if<
        std::is_base_of<
            Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions>,
            TensorXpr
        >::value,
        TensorXpr
    >::type* = nullptr
>
const TensorXpr& eval(const TensorXpr & x) {
    Rcpp::Rcout << "const reference return" << std::endl;
    return x;
}

// implement componentwise multiplication for tensor inputs
template<typename Scalar, int NumDimensions>
Eigen::Tensor<Scalar, NumDimensions> times(
    const Eigen::Tensor<Scalar, NumDimensions> & x,
    const Eigen::Tensor<Scalar, NumDimensions> & y
) {
    return x * y;
}

/**
 * componentwise multiplication wrapper for arbitrary inputs
 *
 * will evaluate tensor expression arguments as needed, or return references to
 * tensor inputs
 */
template<typename Xpr, typename Ypr>
Eigen::Tensor<typename Xpr::Scalar, Xpr::NumDimensions> times(
    const Xpr & x,
    const Ypr & y
) {
    typedef Eigen::Tensor<typename Xpr::Scalar, Xpr::NumDimensions> XTensor;
    typedef Eigen::Tensor<typename Ypr::Scalar, Ypr::NumDimensions> YTensor;

    const XTensor & evalX = eval(x);
    const YTensor & evalY = eval(y);

    return times(evalX, evalY);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> eval_test(
    Eigen::Tensor<double, 2> x,
    Eigen::Tensor<double, 2> y,
    Eigen::Tensor<double, 2> z
) {
    Rcpp::Rcout << std::endl << "--- Running eval(z) ---" << std::endl;
    const Eigen::Tensor<double, 2> &z_eval = eval(z);

    Rcpp::Rcout << std::endl << "--- Running eval_copy(z) ---" << std::endl;
    const Eigen::Tensor<double, 2> &z_eval_copy = eval_copy(z);

    // &z and &z_eval point to the same location in memory;
    // &z_eval_copy points to a copy of z, which demonstrates how eval() makes
    // "smart" copies of the input if the input is already an Eigen::Tensor obj.
    Rcpp::Rcout << std::endl << "--- Study outputs ---" << std::endl;
    Rcpp::Rcout << "Input z address: " << &z << std::endl;
    Rcpp::Rcout << "eval(z) address: " << &z_eval << std::endl;
    Rcpp::Rcout << "eval_copy(z) address: " << &z_eval_copy << std::endl;

    // the "times" function can be called for inputs that are either tensors or
    // tensor expressions
    Rcpp::Rcout << std::endl <<  "--- Running times(z, x + y) ---" << std::endl;
    return times(z, x + y);
}
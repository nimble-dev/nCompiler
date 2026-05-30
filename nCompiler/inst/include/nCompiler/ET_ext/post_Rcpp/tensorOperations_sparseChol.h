#ifndef TENSOROPERATIONS_SPARSECHOL_H
#define TENSOROPERATIONS_SPARSECHOL_H

std::shared_ptr<sparseCholFactor> Cholesky(const Eigen::SparseMatrix<double>& x) {
  std::shared_ptr<sparseCholFactor> ch = nClass_builder<sparseCholFactor>()();
  ch->llt.compute(x);
  if (ch->llt.info() != Eigen::Success)
    throw std::runtime_error("sparse Cholesky factorization failed");
  return ch;
}

// TODO: remove as redundant
std::shared_ptr<sparseCholFactor> sparseChol(const Eigen::SparseMatrix<double>& x) {
  std::shared_ptr<sparseCholFactor> ch = nClass_builder<sparseCholFactor>()();
  ch->llt.compute(x);
  if (ch->llt.info() != Eigen::Success)
    throw std::runtime_error("sparse Cholesky factorization failed");
  return ch;
}


// This overloads nLogdet, on top of use for standard matrices with calculation
// done via the SVD.
double nLogdet(std::shared_ptr<sparseCholFactor> ch) {
    Eigen::SparseMatrix<double> L = ch->llt.matrixL();
    return L.diagonal().array().log().sum();
}


// Should we implement nLogdet for operation directly on sparse matrix?

template<typename RHS>
Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> nSolve(std::shared_ptr<sparseCholFactor> ch, const RHS & b
  ) {
    // explicit Eigen::Tensor types for inputs
    typedef Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> bTensor;
    // evaluate arguments, if necessary
    const auto & b_eval = eval(b);
    // initialize storage for solution, given problem dimensions
    auto bdim = b.dimensions();
    bTensor res = Eigen::Tensor<typename RHS::Scalar, 2>(bdim[0], bdim.size() > 1 ? bdim[1]: 1);
    // map tensor objects to Eigen::Matrix types
    auto bmap = matmap(b_eval);
    auto resMap = matmap(res);
    // solve linear system
    resMap = ch->llt.solve(bmap);
    return res;
}


template<typename RHS>
Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> nBacksolve(std::shared_ptr<sparseCholFactor> ch, const RHS & b
  ) {
    // explicit Eigen::Tensor types for inputs
    typedef Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> bTensor;
    // evaluate arguments, if necessary
    const auto & b_eval = eval(b);
    // initialize storage for solution, given problem dimensions
    auto bdim = b.dimensions();
    bTensor res = Eigen::Tensor<typename RHS::Scalar, 2>(bdim[0], bdim.size() > 1 ? bdim[1]: 1);
    // map tensor objects to Eigen::Matrix types
    auto bmap = matmap(b_eval);
    auto resMap = matmap(res);
    // solve linear system
    resMap = ch->llt.permutationPinv() * ch->llt.matrixU().triangularView<Eigen::Upper>().solve(bmap);
    return res;
}


// Multiply L by matrix.
template<
    typename Ypr,
    typename std::enable_if<
        HasNumDimensionsN<Ypr, 2>(),
        Ypr
    >::type* = nullptr,
    typename ResultType = typename std::conditional<
        IsSparseType<Ypr>::value,
        Eigen::SparseMatrix<typename Ypr::Scalar>,
        Eigen::Tensor<typename Ypr::Scalar, 2>
    >::type
>
ResultType nMul(std::shared_ptr<sparseCholFactor> ch, const Ypr & y) {
    // evaluate arguments, if necessary
    const auto & yeval = eval(y);
    // map inputs and initialize output
    auto ymap = matmap(yeval);
    ResultType res(ymap.rows(), ymap.cols());
    // map and multiply!
    matmap(res) = ch->llt.permutationPinv() * (ch->llt.matrixL() * ymap);
    return res;
}


// Multiply L by vector.
// TODO: is there any way for us to return a vector instead of a matrix?
template<
    typename Ypr,
    typename std::enable_if<
        HasNumDimensionsN<Ypr, 1>(),
        Ypr
    >::type* = nullptr,
    typename ResultType = typename std::conditional<
        IsSparseType<Ypr>::value,
        Eigen::SparseMatrix<typename Ypr::Scalar>,
        Eigen::Tensor<typename Ypr::Scalar, 2>
    >::type
>
ResultType nMul(std::shared_ptr<sparseCholFactor> ch, const Ypr & y) {
    // evaluate arguments, if necessary
    const auto & yeval = eval(y);
    // map inputs and initialize output
    auto ymap = matmap(yeval);
    // initialize output
    ResultType res(ymap.size(), 1);
    // map and multiply!
    matmap(res) = ch->llt.permutationPinv() * (ch->llt.matrixL() * ymap);
    return res;
}



#endif

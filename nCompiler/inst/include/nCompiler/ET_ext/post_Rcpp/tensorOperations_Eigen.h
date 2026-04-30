#ifndef TENSOROPERATIONS_EIGEN_H
#define TENSOROPERATIONS_EIGEN_H

std::shared_ptr<EigenDecomp> make_EigenDecomp() {
  return(std::shared_ptr<EigenDecomp>(new EigenDecomp));
}

std::shared_ptr<EigenDecomp> nEigen(const Eigen::Tensor<double, 2> &x, bool symmetric = true, bool valuesOnly = false) {
    auto x_map = matmap(x);
    int nrows(x_map.rows());
    int ncols(x_map.cols());
    // potentially error-trap of nrows == ncols.
    std::shared_ptr<EigenDecomp> ans(new EigenDecomp);
    ans->values.resize(nrows);
    auto values_map = matmap(ans->values);
    if(!valuesOnly){
        ans->vectors.resize(std::array<Eigen::Index, 2>({{nrows, ncols}}));
    }
    Eigen::DecompositionOptions eigOpts = valuesOnly ? Eigen::EigenvaluesOnly : Eigen::ComputeEigenvectors;
    if(symmetric) {
        Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> solver1(x_map, eigOpts);
        values_map = solver1.eigenvalues().reverse();
        if(!valuesOnly){
    Eigen::Map<Eigen::MatrixXd> vecs_map = matmap(ans->vectors);
    vecs_map = solver1.eigenvectors().rowwise().reverse();
        }
    } else {
        Eigen::EigenSolver<Eigen::MatrixXd> solver2(x_map, eigOpts);
        std::vector<std::pair<double,int> > sortIndices;
        for(int i = 0; i < nrows; i++){
    sortIndices.push_back(std::make_pair(abs(solver2.eigenvalues().real()(i)),i));
        }
        std::sort(sortIndices.begin(), sortIndices.end());
        std::reverse(sortIndices.begin(), sortIndices.end());

        for(int i = 0; i < nrows ; ++i){
    if(solver2.eigenvalues().imag()(sortIndices[i].second) != 0){
        // emit error message like thsi from nimble
        // _nimble_global_output <<"Run-time warning: matrix used in call to nimEigen() has a complex valued eigenvector."<<"\n"; nimble_print_to_R(_nimble_global_output);
        values_map(i) = NAN;
    } else {
        values_map(i) = solver2.eigenvalues().real()(sortIndices[i].second);
    }
        }
        if(!valuesOnly){
    Eigen::Map<Eigen::MatrixXd> vecs_map = matmap(ans->vectors);
    for(int i = 0; i < ncols; i++){
        vecs_map.col(i) = solver2.eigenvectors().real().col(sortIndices[i].second);
        for(int j = 0; j < nrows; j++){
        if(solver2.eigenvectors().imag()(j, sortIndices[i].second) != 0){
            // emit warning something like this from nimble:
            // _nimble_global_output <<"Run-time warning: matrix used in call to nimEigen() has a complex valued eigenvector."<<"\n"; nimble_print_to_R(_nimble_global_output);
            vecs_map(j, i) = NAN;
        }
        }
    }
        }
    }
    return ans;
}

#endif

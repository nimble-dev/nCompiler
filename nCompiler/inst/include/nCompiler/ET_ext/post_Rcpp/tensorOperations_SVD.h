#ifndef TENSOROPERATIONS_SVD_H
#define TENSOROPERATIONS_SVD_H

std::shared_ptr<SVDDecomp> nSvd(
    const Eigen::Tensor<double, 2> &x, int vectors
) {
    auto xm = matmap(x);
    std::shared_ptr<SVDDecomp> ans = nClass_builder<SVDDecomp>()();

    int n = xm.rows();
    int p = xm.cols();
 	int nu = std::min(n, p);

 	Eigen::JacobiSVD<Eigen::MatrixXd> svd;

 	/* note: if nu > 16, bidiagonialization algo. is recommended on eigen
 	   website.  not currently available w/ nimble's version of eigen, but may
 	   be in future. */
 	if(vectors == 0) {
 	    svd.compute(xm);
 	}
 	else {
 	    int leftSVs = nu;
 	    int rightSVs = nu;

 	    if(vectors == 1) {
 	        svd.compute(xm, Eigen::ComputeThinU | Eigen::ComputeThinV);
 	    }
 	    if(vectors == 2) {
 	        leftSVs = xm.rows();
 	        rightSVs = xm.cols();
 	        svd.compute(xm, Eigen::ComputeFullU | Eigen::ComputeFullV);
 	    }

 	    ans->u.resize(std::array<Eigen::Index, 2>({{n, leftSVs}}));
 	    auto u = matmap(ans->u);
 	    u = svd.matrixU();

 	    ans->v.resize(std::array<Eigen::Index, 2>({{p, rightSVs}}));
 	    auto v = matmap(ans->v);
        v = svd.matrixV();
 	}

    ans->d.resize(nu);
 	auto d = matmap(ans->d);
 	d = svd.singularValues();

 	return ans;
}


#endif

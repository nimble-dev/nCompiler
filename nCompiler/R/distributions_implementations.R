## This file retains a lot of original nimble content that has not been updated.
##
## nCompiler develpment
## Rcpp tries to take over all interfaces even from seeing a .Call in source code
## So for now I am commenting the ones here out

                                        # additional distributions provided by nCompiler
# in general we use doubles on C side so convert parameters to doubles here before passing to C

#' The Wishart Distribution
#'
#' Density and random generation for the Wishart distribution, using the Cholesky factor of either the scale matrix or the rate matrix.
#'
#' @name Wishart
#' @aliases wishart
#' 
#' @param x vector of values.
#' @param n number of observations (only \code{n=1} is handled currently).
#' @param cholesky upper-triangular Cholesky factor of either the scale matrix (when \code{scale_param} is TRUE) or rate matrix (otherwise).
#' @param df degrees of freedom.
#' @param scale_param logical; if TRUE the Cholesky factor is that of the scale matrix; otherwise, of the rate matrix.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @author Christopher Paciorek
#' @details See Gelman et al., Appendix A or the BUGS manual for mathematical details. The rate matrix as used here is defined as the inverse of the scale matrix, \eqn{S^{-1}}, given in Gelman et al. 
#' @return \code{dwish_chol} gives the density and \code{rwish_chol} generates random deviates.
#' @references Gelman, A., Carlin, J.B., Stern, H.S., and Rubin, D.B. (2004) \emph{Bayesian Data Analysis}, 2nd ed. Chapman and Hall/CRC.
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' df <- 40
#' ch <- chol(matrix(c(1, .7, .7, 1), 2))
#' x <- rwish_chol(1, ch, df = df)
#' dwish_chol(x, ch, df = df)
#'
NULL

#' @rdname Wishart
#' @export
dwish_chol <- function(x, cholesky, df, scale_param = TRUE, log = FALSE) {
  # scale_param = TRUE is the GCSR parameterization (i.e., scale matrix); scale_param = FALSE is the BUGS parameterization (i.e., rate matrix)
    if(storage.mode(cholesky) != 'double')
          storage.mode(cholesky) <- 'double'
    if(storage.mode(x) != 'double')
            storage.mode(x) <- 'double'
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dwish_chol, x, cholesky, as.double(df), as.double(scale_param), as.logical(log))
}

#' @rdname Wishart
#' @export
rwish_chol <- function(n = 1, cholesky, df, scale_param = TRUE) {
    if(n != 1) warning('rwish_chol only handles n = 1 at the moment')
    if(storage.mode(cholesky) != 'double')
    	storage.mode(cholesky) <- 'double'
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##out <- .Call(C_rwish_chol, cholesky, as.double(df), as.double(scale_param))
    if(!is.null(out)) out <- matrix(out, nrow = sqrt(length(cholesky)))
    return(out)
}

#' The Inverse Wishart Distribution
#'
#' Density and random generation for the Inverse Wishart distribution, using the Cholesky factor of either the scale matrix or the rate matrix.
#'
#' @name Inverse-Wishart
#' @aliases inverse-wishart
#' 
#' @param x vector of values.
#' @param n number of observations (only \code{n=1} is handled currently).
#' @param cholesky upper-triangular Cholesky factor of either the scale matrix (when \code{scale_param} is TRUE) or rate matrix (otherwise).
#' @param df degrees of freedom.
#' @param scale_param logical; if TRUE the Cholesky factor is that of the scale matrix; otherwise, of the rate matrix.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @author Christopher Paciorek
#' @details See Gelman et al., Appendix A for mathematical details. The rate matrix as used here is defined as the inverse of the scale matrix, \eqn{S^{-1}}, given in Gelman et al. 
#' @return \code{dinvwish_chol} gives the density and \code{rinvwish_chol} generates random deviates.
#' @references Gelman, A., Carlin, J.B., Stern, H.S., and Rubin, D.B. (2004) \emph{Bayesian Data Analysis}, 2nd ed. Chapman and Hall/CRC.
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' df <- 40
#' ch <- chol(matrix(c(1, .7, .7, 1), 2))
#' x <- rwish_chol(1, ch, df = df)
#' dwish_chol(x, ch, df = df)
#'
NULL

#' @rdname Inverse-Wishart
#' @export
dinvwish_chol <- function(x, cholesky, df, scale_param = TRUE, log = FALSE) {
  # scale_param = FALSE is the GCSR parameterization (i.e., inverse scale matrix); scale_param = TRUE is the parameterization best for conjugacy calculations (i.e., scale matrix)
    if(storage.mode(cholesky) != 'double')
          storage.mode(cholesky) <- 'double'
    if(storage.mode(x) != 'double')
            storage.mode(x) <- 'double'
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dinvwish_chol, x, cholesky, as.double(df), as.double(scale_param), as.logical(log))
}

#' @rdname Inverse-Wishart
#' @export
rinvwish_chol <- function(n = 1, cholesky, df, scale_param = TRUE) {
    if(n != 1) warning('rinvwish_chol only handles n = 1 at the moment')
    if(storage.mode(cholesky) != 'double')
    	storage.mode(cholesky) <- 'double'
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##out <- .Call(C_rinvwish_chol, cholesky, as.double(df), as.double(scale_param))
    if(!is.null(out)) out <- matrix(out, nrow = sqrt(length(cholesky)))
    return(out)
}


#' The Improper Uniform Distribution
#'
#' Improper flat distribution for use as a prior distribution in BUGS models
#'
#' @name flat
#' @aliases halfflat
#'
#' @param x vector of values. 
#' @param n number of observations.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#'
#' @author Christopher Paciorek
#' @return \code{dflat} gives the pseudo-density value of 1, while \code{rflat} and \code{rhalfflat} return \code{NaN},
#' since one cannot simulate from an improper distribution. Similarly, \code{dhalfflat}
#' gives a pseudo-density value of 1 when \code{x} is non-negative.
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' dflat(1)
NULL

#' @rdname flat
#' @export
dflat <- function(x, log = FALSE) {  
   if(log) out <- rep(0, length(x)) else  out <- rep(1, length(x))
   nas <- is.na(x)
   out[nas] <- x[nas]
   return(out)
}

#' @rdname flat
#' @export
rflat <- function(n = 1) {
  return(rep(NaN, n))
}

#' @rdname flat
#' @export
dhalfflat <- function(x, log = FALSE) {
  out <- rep(0, length(x))
  out[x < 0] <- -Inf
  nas <- is.na(x)
  out[nas] <- x[nas]
  if(log) return(out) else return(exp(out))
}

#' @rdname flat
#' @export
rhalfflat <- function(n = 1) {
  return(rep(NaN, n))
}

#' The Dirichlet Distribution
#'
#' Density and random generation for the Dirichlet distribution
#'
#' @name Dirichlet
#' @aliases dirichlet
#' 
#' @param x vector of values.
#' @param n number of observations (only \code{n=1} is handled currently).
#' @param alpha vector of parameters of same length as \code{x}
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @author Christopher Paciorek
#' @details See Gelman et al., Appendix A or the BUGS manual for mathematical details. 
#' @return \code{ddirch} gives the density and \code{rdirch} generates random deviates.
#' @references Gelman, A., Carlin, J.B., Stern, H.S., and Rubin, D.B. (2004) \emph{Bayesian Data Analysis}, 2nd ed. Chapman and Hall/CRC.
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' alpha <- c(1, 10, 30)
#' x <- rdirch(1, alpha)
#' ddirch(x, alpha)
NULL

#' @rdname Dirichlet
#' @export
ddirch <- function(x, alpha, log = FALSE) {
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_ddirch, as.double(x), as.double(alpha), as.logical(log))
}

#' @rdname Dirichlet
#' @export
rdirch <- function(n = 1, alpha) {
    if(n != 1) warning('rdirch only handles n = 1 at the moment')
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_rdirch, as.double(alpha))
}

#' The Multinomial Distribution
#'
#' Density and random generation for the multinomial distribution
#'
#' @name Multinomial
#' @aliases multinomial
#' 
#' @param x vector of values.
#' @param n number of observations (only \code{n=1} is handled currently).
#' @param size number of trials.
#' @param prob vector of probabilities, internally normalized to sum to one, of same length as \code{x}
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @author Christopher Paciorek
#' @details See Gelman et al., Appendix A or the BUGS manual for mathematical details. 
#' @return \code{dmulti} gives the density and \code{rmulti} generates random deviates.
#' @references Gelman, A., Carlin, J.B., Stern, H.S., and Rubin, D.B. (2004) \emph{Bayesian Data Analysis}, 2nd ed. Chapman and Hall/CRC.
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' size <- 30
#' probs <- c(1/4, 1/10, 1 - 1/4 - 1/10)
#' x <- rmulti(1, size, probs)
#' dmulti(x, size, probs)
NULL

#' @rdname Multinomial
#' @export
dmulti <- function(x, size = sum(x), prob, log = FALSE) {
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dmulti, as.double(x), as.double(size), as.double(prob), as.logical(log))
}

#' @rdname Multinomial
#' @export
rmulti <- function(n = 1, size, prob) {
  if(n != 1) warning('rmulti only handles n = 1 at the moment')
  stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
  ##.Call(C_rmulti, as.double(size), as.double(prob))
}

#' The Categorical Distribution
#'
#' Density and random generation for the categorical distribution
#'
#' @name Categorical
#' 
#' @param x non-negative integer-value numeric value.
#' @param n number of observations.
#' @param prob vector of probabilities, internally normalized to sum to one.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @author Christopher Paciorek
#' @details See the BUGS manual for mathematical details. 
#' @return \code{dcat} gives the density and \code{rcat} generates random deviates.
##' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' probs <- c(1/4, 1/10, 1 - 1/4 - 1/10)
#' x <- rcat(n = 30, probs)
#' dcat(x, probs)
#'
NULL

#' @rdname Categorical
#' @export
dcat <- function(x, prob, log = FALSE) {
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dcat, as.double(x), as.double(prob), as.logical(log))
}

#' @rdname Categorical
#' @export
rcat <- function(n = 1, prob) {
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_rcat, as.integer(n), as.double(prob))
}


#' The t Distribution
#'
#' Density, distribution function, quantile function and random generation
#' for the t distribution with \code{df} degrees of freedom,
#' allowing non-zero location, \code{mu},
#' and non-unit scale, \code{sigma} 
#'
#' @name t
#' 
#' @param x vector of values.
#' @param n number of observations.
#' @param df vector of degrees of freedom values.
#' @param p vector of probabilities.
#' @param q vector of quantiles.
#' @param mu vector of location values.
#' @param sigma vector of scale values.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @param log.p logical; if TRUE, probabilities p are given by user as log(p).
#' @param lower.tail logical; if TRUE (default) probabilities are \eqn{P[X \le x]}; otherwise, \eqn{P[X > x]}.
#' @author Christopher Paciorek
#' @details See Gelman et al., Appendix A or the BUGS manual for mathematical details. 
#' @return \code{dt_nonstandard} gives the density, \code{pt_nonstandard} gives the distribution
#' function, \code{qt_nonstandard} gives the quantile function, and \code{rt_nonstandard}
#' generates random deviates.
#' @references Gelman, A., Carlin, J.B., Stern, H.S., and Rubin, D.B. (2004) \emph{Bayesian Data Analysis}, 2nd ed. Chapman and Hall/CRC.
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' x <- rt_nonstandard(50, df = 1, mu = 5, sigma = 1)
#' dt_nonstandard(x, 3, 5, 1)
NULL

#' @rdname t
#' @export
dt_nonstandard <- function(x, df = 1, mu = 0, sigma = 1, log = FALSE) {
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dt_nonstandard, as.double(x), as.double(df), as.double(mu), as.double(sigma), as.logical(log))
}

#' @rdname t
#' @export
rt_nonstandard <- function(n, df = 1, mu = 0, sigma = 1) {
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_rt_nonstandard, as.integer(n), as.double(df), as.double(mu), as.double(sigma))
}

#' @rdname t
#' @export
pt_nonstandard <- function(q, df = 1, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_pt_nonstandard, as.double(q), as.double(df), as.double(mu), as.double(sigma), as.logical(lower.tail), as.logical(log.p))
}

#' @rdname t
#' @export
qt_nonstandard <- function(p, df = 1, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_qt_nonstandard, as.double(p), as.double(df), as.double(mu), as.double(sigma), as.logical(lower.tail), as.logical(log.p))
}

#' The Multivariate Normal Distribution
#'
#' Density and random generation for the multivariate normal distribution, using the Cholesky factor of either the precision matrix or the covariance matrix.
#'
#' @name MultivariateNormal
#' 
#' @param x vector of values.
#' @param n number of observations (only \code{n=1} is handled currently).
#' @param mean vector of values giving the mean of the distribution.
#' @param cholesky upper-triangular Cholesky factor of either the precision matrix (when \code{prec_param} is TRUE) or covariance matrix (otherwise).
#' @param prec_param logical; if TRUE the Cholesky factor is that of the precision matrix; otherwise, of the covariance matrix.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @author Christopher Paciorek
#' @details See Gelman et al., Appendix A or the BUGS manual for mathematical details. The rate matrix as used here is defined as the inverse of the scale matrix, \eqn{S^{-1}}, given in Gelman et al. 
#' @return \code{dmnorm_chol} gives the density and \code{rmnorm_chol} generates random deviates.
#' @references Gelman, A., Carlin, J.B., Stern, H.S., and Rubin, D.B. (2004) \emph{Bayesian Data Analysis}, 2nd ed. Chapman and Hall/CRC.
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' mean <- c(-10, 0, 10)
#' covmat <- matrix(c(1, .9, .3, .9, 1, -0.1, .3, -0.1, 1), 3)
#' ch <- chol(covmat)
#' x <- rmnorm_chol(1, mean, ch, prec_param = FALSE)
#' dmnorm_chol(x, mean, ch, prec_param = FALSE)
NULL

#' @rdname MultivariateNormal
#' @export
dmnorm_chol <- function(x, mean, cholesky, prec_param = TRUE, log = FALSE) {
  # cholesky should be upper triangular
  # FIXME: allow cholesky to be lower tri
    if(storage.mode(cholesky) != 'double')
         storage.mode(cholesky) <- 'double'
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dmnorm_chol, as.double(x), as.double(mean), cholesky, as.double(prec_param), as.logical(log))
}

#' @rdname MultivariateNormal
#' @export
rmnorm_chol <- function(n = 1, mean, cholesky, prec_param = TRUE) {
 ## cholesky should be upper triangular
 ## FIXME: allow cholesky to be lower tri
    if(n != 1) warning('rmnorm_chol only handles n = 1 at the moment')
    if(storage.mode(cholesky) != 'double')
         storage.mode(cholesky) <- 'double'
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_rmnorm_chol, as.double(mean), cholesky, as.double(prec_param))
}

#' The Multivariate t Distribution
#'
#' Density and random generation for the multivariate t distribution, using the Cholesky factor of either the precision matrix (i.e., inverse scale matrix) or the scale matrix.
#'
#' @name Multivariate-t
#'
#' @aliases mvt multivariate-t 
#' 
#' @param x vector of values.
#' @param n number of observations (only \code{n=1} is handled currently).
#' @param mu vector of values giving the location of the distribution.
#' @param cholesky upper-triangular Cholesky factor of either the precision matrix (i.e., inverse scale matrix) (when \code{prec_param} is TRUE) or scale matrix (otherwise).
#' @param df degrees of freedom.
#' @param prec_param logical; if TRUE the Cholesky factor is that of the precision matrix; otherwise, of the scale matrix.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @author Peter Sujan
#' @details See Gelman et al., Appendix A or the BUGS manual for mathematical details. The 'precision' matrix as used here is defined as the inverse of the scale matrix, \eqn{\Sigma^{-1}}, given in Gelman et al. 
#' @return \code{dmvt_chol} gives the density and \code{rmvt_chol} generates random deviates.
#' @references Gelman, A., Carlin, J.B., Stern, H.S., and Rubin, D.B. (2004) \emph{Bayesian Data Analysis}, 2nd ed. Chapman and Hall/CRC.
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' mu <- c(-10, 0, 10)
#' scalemat <- matrix(c(1, .9, .3, .9, 1, -0.1, .3, -0.1, 1), 3)
#' ch <- chol(scalemat)
#' x <- rmvt_chol(1, mu, ch, df = 1, prec_param = FALSE)
#' dmvt_chol(x, mu, ch, df = 1, prec_param = FALSE)
#' 
NULL

#' @rdname Multivariate-t
#' @export
dmvt_chol <- function(x, mu, cholesky, df, prec_param = TRUE, log = FALSE) {
  # cholesky should be upper triangular
  # FIXME: allow cholesky to be lower tri
    if(storage.mode(cholesky) != 'double')
       storage.mode(cholesky) <- 'double'
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dmvt_chol, as.double(x), as.double(mu), cholesky,
    ## as.double(df), as.double(prec_param), as.logical(log))
}

#' @rdname Multivariate-t
#' @export
rmvt_chol <- function(n = 1, mu, cholesky, df, prec_param = TRUE) {
  ## cholesky should be upper triangular
  ## FIXME: allow cholesky to be lower tri
    if(n != 1) warning('rmvt_chol only handles n = 1 at the moment')
    if(storage.mode(cholesky) != 'double')
         storage.mode(cholesky) <- 'double'
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_rmvt_chol, as.double(mu), cholesky,
    ##    as.double(df), as.double(prec_param))
}

#' Interval calculations 
#'
#' Calculations to handle censoring
#'
#' @name Interval
#' 
#' @param x vector of interval indices.
#' @param n number of observations.
#' @param t vector of values.
#' @param c vector of one or more values delineating the intervals.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @author Christopher Paciorek
#' @details Used for working with censoring in BUGS code.
#' Taking \code{c} to define the endpoints of two or more intervals (with implicit endpoints of plus/minus infinity), \code{x} (or the return value of \code{rinterval}) gives the non-negative integer valued index of the interval in which \code{t} falls. See the nCompiler manual for additional details. 
#' @return \code{dinterval} gives the density and \code{rinterval} generates random deviates,
#' but these are unusual as the density is 1 if \code{x} indicates the interval in which \code{t}
#' falls and 0 otherwise and the deviates are simply the interval(s) in which \code{t} falls.
##' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' endpoints <- c(-3, 0, 3)
#' vals <- c(-4, -1, 1, 5)
#' x <- rinterval(4, vals, endpoints)
#' dinterval(x, vals, endpoints)
#' dinterval(c(1, 5, 2, 3), vals, endpoints)
NULL

 
#' @rdname Interval
#' @export
dinterval <- function(x, t, c, log = FALSE) {
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dinterval, as.double(x), as.double(t), as.double(c), as.logical(log))
}

#' @rdname Interval
#' @export
rinterval <- function(n = 1, t, c) {
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_rinterval, as.integer(n), as.double(t), as.double(c))
}

#' Constraint calculations in nCompiler
#'
#' Calculations to handle censoring
#'
#' @name Constraint
#'
#' @param x value indicating whether \code{cond} is TRUE or FALSE
#' @param n number of observations (only \code{n=1} is handled currently).
#' @param cond logical value   
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @author Christopher Paciorek
#' @details Used for working with constraints in BUGS code.
#' See the nCompiler manual for additional details. 
#' @return \code{dconstraint} gives the density and \code{rconstraint} generates random deviates,
#' but these are unusual as the density is 1 if \code{x} matches \code{cond} and
#' 0 otherwise and the deviates are simply the value of \code{cond}
##' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' constr <- 3 > 2 && 4 > 0
#' x <- rconstraint(1, constr)
#' dconstraint(x, constr)
#' dconstraint(0, 3 > 4)
#' dconstraint(1, 3 > 4)
#' rconstraint(1, 3 > 4)
NULL

#' @rdname Constraint
#' @export
dconstraint <- function(x, cond, log = FALSE) {
    if(length(x) > 1 || length(cond) > 1) stop('dconstraint is not vectorized')
    if(is.na(x) || is.na(cond)) return(x + cond) # mimic how R's C functions handle NA and NaN inputs
    if(x == cond || x == 0) result <- 1 else result <- 0
    if(log) return(log(result)) else return(result)
}

#' @rdname Constraint
#' @export
rconstraint <- function(n = 1, cond) {
    if(n != 1 || length(cond) > 1) stop('rconstraint only handles n = 1 at the moment')
    if(is.na(cond)) {
        warning("NAs produced")
        return(NaN)
    }
    return(as.integer(cond))
}

# exp_nimble extends R to allow rate or scale and provide common interface via 'rate' to C functions

#' The Exponential Distribution
#'
#'   Density, distribution function, quantile function and random
#'   generation for the exponential distribution with rate
#'     (i.e., mean of \code{1/rate}) or scale parameterizations.
#' 
#' @name Exponential
#' 
#' @param x vector of values.
#' @param n number of observations.
#' @param p vector of probabilities.
#' @param q vector of quantiles.
#' @param rate vector of rate values.
#' @param scale vector of scale values.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @param log.p logical; if TRUE, probabilities p are given by user as log(p).
#' @param lower.tail logical; if TRUE (default) probabilities are \eqn{P[X \le x]}; otherwise, \eqn{P[X > x]}.
#' @author Christopher Paciorek
#' @details nimble's exponential distribution functions use Rmath's functions
#' under the hood, but are parameterized to take both rate and scale and to
#' use 'rate' as the core parameterization in C, unlike Rmath, which uses 'scale'.
#' See Gelman et al., Appendix A or
#' the BUGS manual for mathematical details. 
#' @return \code{dexp_nimble} gives the density, \code{pexp_nimble} gives the distribution
#' function, \code{qexp_nimble} gives the quantile function, and \code{rexp_nimble}
#' generates random deviates.
#' @references Gelman, A., Carlin, J.B., Stern, H.S., and Rubin, D.B. (2004) \emph{Bayesian Data Analysis}, 2nd ed. Chapman and Hall/CRC.
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' x <- rexp_nimble(50, scale = 3)
#' dexp_nimble(x, scale = 3)
NULL


#' @rdname Exponential
#' @export
dexp_nimble <- function(x, rate = 1/scale, scale = 1, log = FALSE) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dexp_nimble, as.double(x), as.double(rate), as.logical(log))
}

#' @rdname Exponential
#' @export
rexp_nimble <- function(n = 1, rate = 1/scale, scale = 1) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_rexp_nimble, as.integer(n), as.double(rate))
}

#' @rdname Exponential
#' @export
pexp_nimble <- function(q, rate = 1/scale, scale = 1, lower.tail = TRUE, log.p = FALSE) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_pexp_nimble, as.double(q), as.double(rate), as.logical(lower.tail), as.logical(log.p))
}

#' @rdname Exponential
#' @export
qexp_nimble <- function(p, rate = 1/scale, scale = 1, lower.tail = TRUE, log.p = FALSE) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_qexp_nimble, as.double(p), as.double(rate), as.logical(lower.tail), as.logical(log.p))
}

#' The Inverse Gamma Distribution
#'
#'   Density, distribution function, quantile function and random
#'   generation for the inverse gamma distribution with rate
#'     or scale (mean = scale / (shape - 1)) parameterizations.
#' 
#' @name Inverse-Gamma
#' 
#' @param x vector of values.
#' @param n number of observations.
#' @param p vector of probabilities.
#' @param q vector of quantiles.
#' @param shape vector of shape values, must be positive.
#' @param rate vector of rate values, must be positive.
#' @param scale vector of scale values, must be positive.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#' @param log.p logical; if TRUE, probabilities p are given by user as log(p).
#' @param lower.tail logical; if TRUE (default) probabilities are \eqn{P[X \le x]}; otherwise, \eqn{P[X > x]}.
#' @author Christopher Paciorek
#' @details The inverse gamma distribution with parameters \code{shape} \eqn{=\alpha}{= a} and
#' \code{scale} \eqn{=\sigma}{= s} has density
#' \deqn{
#'   f(x)= \frac{s^a}{\Gamma(\alpha)} {x}^{-(\alpha+1)} e^{-\sigma/x}%
#'  }{f(x)= (s^a / Gamma(a)) x^-(a+1) e^-(s/x)}
#' for \eqn{x \ge 0}, \eqn{\alpha > 0}{a > 0} and \eqn{\sigma > 0}{s > 0}.
#' (Here \eqn{\Gamma(\alpha)}{Gamma(a)} is the function implemented by \R's
#'  \link{gamma}) and defined in its help.
#'
#'  The mean and variance are
#'  \eqn{E(X) = \frac{\sigma}{\alpha}-1}{E(X) = s/(a-1)} and
#' \eqn{Var(X) = \frac{\sigma^2}{(\alpha-1)^2 (\alpha-2)}}{Var(X) = s^2 / ((a-1)^2 * (a-2))},
#' with the mean defined only
#' for \eqn{\alpha > 1}{a > 1} and the variance only for \eqn{\alpha > 2}{a > 2}.
#'
#' See Gelman et al., Appendix A or
#' the BUGS manual for mathematical details. 
#' @return \code{dinvgamma} gives the density, \code{pinvgamma} gives the distribution
#' function, \code{qinvgamma} gives the quantile function, and \code{rinvgamma}
#' generates random deviates.
#' @references Gelman, A., Carlin, J.B., Stern, H.S., and Rubin, D.B. (2004) \emph{Bayesian Data Analysis}, 2nd ed. Chapman and Hall/CRC.
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' x <- rinvgamma(50, shape = 1, scale = 3)
#' dinvgamma(x, shape = 1, scale = 3)
NULL


#' @rdname Inverse-Gamma
#' @export
dinvgamma <- function(x, shape, scale = 1, rate = 1/scale, log = FALSE) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dinvgamma, as.double(x), as.double(shape), as.double(rate), as.logical(log))
}

#' @rdname Inverse-Gamma
#' @export
rinvgamma <- function(n = 1, shape, scale = 1, rate = 1/scale) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_rinvgamma, as.integer(n), as.double(shape), as.double(rate))
}


#' @rdname Inverse-Gamma
#' @export
pinvgamma <- function(q, shape, scale = 1, rate = 1/scale, lower.tail = TRUE, log.p = FALSE) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_pinvgamma, as.double(q), as.double(shape), as.double(rate), as.logical(lower.tail), as.logical(log.p))
}

#' @rdname Inverse-Gamma
#' @export
qinvgamma <- function(p, shape, scale = 1, rate = 1/scale, lower.tail = TRUE, log.p = FALSE) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_qinvgamma, as.double(p), as.double(shape), as.double(rate), as.logical(lower.tail), as.logical(log.p))
}

# sqrtinvgamma is intended solely for use in conjugacy with dhalfflat
#' @export
dsqrtinvgamma <- function(x, shape, scale = 1, rate = 1/scale, log = FALSE) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dsqrtinvgamma, as.double(x), as.double(shape), as.double(rate), as.logical(log))
}

#' @export
rsqrtinvgamma <- function(n = 1, shape, scale = 1, rate = 1/scale) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_rsqrtinvgamma, as.integer(n), as.double(shape), as.double(rate))
}


#' The CAR-Normal Distribution
#'
#'   Density function and random generation for the improper (intrinsic)
#'   Gaussian conditional autoregressive (CAR) distribution.
#' 
#' @name CAR-Normal
#' 
#' @param x vector of values.
#' @param n number of observations.
#' @param adj vector of indices of the adjacent locations (neighbors) of each spatial location.  This is a sparse representation of the full adjacency matrix.
#' @param weights vector of symmetric unnormalized weights associated with each pair of adjacent locations, of the same length as adj.  If omitted, all weights are taken to be one.
#' @param num vector giving the number of neighbors of each spatial location, with length equal to the total number of locations.
#' @param tau scalar precision of the Gaussian CAR prior.
#' @param c integer number of constraints to impose on the improper density function.  If omitted, \code{c} is calculated as the number of disjoint groups of spatial locations in the adjacency structure. Note that \code{c} should be equal to the number of eigenvalues of the precision matrix that are zero. For example if the neighborhood structure is based on a second-order Markov random field in one dimension has two zero eigenvalue and in two dimensinos has three zero eigenvalues. See Rue and Held (2005) for more information.
#' @param zero_mean integer specifying whether to set the mean of all locations to zero during MCMC sampling of a node specified with this distribution in BUGS code (default \code{0}). This argument is used only in BUGS model code when specifying models in nimble. If \code{0}, the overall process mean is included implicitly in the value of each location in a BUGS model; if \code{1}, then during MCMC sampling, the mean of all locations is set to zero at each MCMC iteration, and a separate intercept term should be included in the BUGS model. Note that centering during MCMC as implemented in nimble follows the ad hoc approach of \pkg{WinBUGS} and does not sample under the constraint that the mean is zero as discussed on p. 36 of Rue and Held (2005).  See details.
#' @param log logical; if TRUE, probability density is returned on the log scale.
#'
#' @author Daniel Turek
#' 
#' @details 
#'
#' When specifying a CAR distribution in BUGS model code, the \code{zero_mean} parameter should be specified as either 0 or 1 (rather than TRUE or FALSE).
#' 
#' Note that because the distribution is improper, \code{rcar_normal} does not generated a sample from the distribution, though as discussed in Rue and Held (2005), it is possible to generate a sample from the distribution under constraints imposed based on the eigenvalues of the precision matrix that are zero.
#' 
#' @return \code{dcar_normal} gives the density, and \code{rcar_normal} returns the current process values, since this distribution is improper.
#' 
#' @references
#' Banerjee, S., Carlin, B.P., and Gelfand, A.E. (2015). \emph{Hierarchical Modeling and Analysis for Spatial Data}, 2nd ed. Chapman and Hall/CRC.
#'
#' Rue, H. and L. Held (2005). \emph{Gaussian Markov Random Fields}, Chapman and Hall/CRC.
#' 
#' @seealso \link{Distributions} for other standard distributions
#' 
#' @examples
#' x <- c(1, 3, 3, 4)
#' num <- c(1, 2, 2, 1)
#' adj <- c(2, 1,3, 2,4, 3)
#' weights <- c(1, 1, 1, 1, 1, 1)
#' lp <- dcar_normal(x, adj, weights, num, tau = 1)
NULL

#' @rdname CAR-Normal
#' @export
dcar_normal <- function(x, adj, weights, num, tau, c = CAR_calcNumIslands(adj, num), zero_mean = 0, log = FALSE) {
    CAR_checkAdjWeightsNum(adj, weights, num)
    if(storage.mode(x) != 'double')   storage.mode(x) <- 'double'
    if(storage.mode(adj) != 'double')   storage.mode(adj) <- 'double'
    if(storage.mode(weights) != 'double')   storage.mode(weights) <- 'double'
    if(storage.mode(num) != 'double')   storage.mode(num) <- 'double'
    ##
    ##k <- length(x)
    ##c <- 1
    ##lp <- 0
    ##count <- 1
    ##for(i in 1:k) {
    ##    if(num[i] == 0)   c <- c + 1
    ##    xi <- x[i]
    ##    for(j in 1:num[i]) {
    ##        xj <- x[adj[count]]
    ##        lp <- lp + weights[count] * (xi-xj)^2
    ##        count <- count + 1
    ##    }
    ##}
    ##if(count != (length(adj)+1)) stop('something wrong')
    ##lp <- lp / 2
    ##lp <- lp * (-1/2) * tau
    ##lp <- lp + (k-c)/2 * log(tau/2/pi)
    ##if(log) return(lp)
    ##return(exp(lp))
    ##
    stop('Actual .Call commented out while working on Rcpp compileAttributs() system')
    ##.Call(C_dcar_normal, as.double(x), as.double(adj), as.double(weights), as.double(num), as.double(tau), as.double(c), as.double(zero_mean), as.logical(log))
}

#' @rdname CAR-Normal
#' @export
rcar_normal <- function(n = 1, adj, weights, num, tau, c = CAR_calcNumIslands(adj, num), zero_mean = 0) {
    ## it's important that simulation via rcar_normal() does *not* set all values to NA (or NaN),
    ## since initializeModel() will call this simulate method if there are any NA's present,
    ## (which is allowed for island components), which over-writes all the other valid initial values.
    ##return(rep(NaN, length(num)))
    currentValues <- eval(quote(model[[nodes]]), parent.frame(3))
    return(currentValues)
}


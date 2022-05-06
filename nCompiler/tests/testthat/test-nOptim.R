test_that("nOptim", {
  
  # test use of nOptim with, and without reference arguments
  refArgsList = list(list(), 'par') 
  
  for(refArgs in refArgsList) {
    
    ncOptimProblem <- nClass(
      classname = 'OptimProblem',
      Cpublic = list(
        gr_calls = 'integer',
        f = nFunction(
          fun = function(par) {
            ans <- sum(par^2)
            return(ans) 
          }, 
          argTypes = list(par = 'numericVector'),
          refArgs = refArgs,
          returnType = 'double'
        ),
        gr = nFunction(
          fun = function(par) {
            gr_calls <- gr_calls + 1
            ans <- 2 * par
            return(ans)
          },
          argTypes = list(par = 'numericVector'),
          refArgs = refArgs,
          returnType = 'numericVector'
        ),
        optim = nFunction(
          fun = function(init, method, lower, upper, hessian, maxit) {
            gr_calls <- 0
            control <- OptimControlList$new()
            control$initToDefaults()
            control$maxit <- maxit
            result <- nOptim(
              init, f, "", method, lower, upper, control, hessian
            )  
            return(result)
          }, 
          argTypes = list(
            init = 'numericVector',
            method = 'string',
            lower = 'numericVector',
            upper = 'numericVector',
            hessian = 'logical',
            maxit = 'integer'
          ), 
          returnType = 'OptimResultList'
        ),
        optimgr = nFunction(
          fun = function(init, method, lower, upper, hessian, maxit) {
            control <- OptimControlList$new()
            control$initToDefaults()
            control$maxit <- maxit
            result <- nOptim(
              init, f, gr, method, lower, upper, control, hessian
            )  
            return(result)
          }, 
          argTypes = list(
            init = 'numericVector',
            method = 'string',
            lower = 'numericVector',
            upper = 'numericVector',
            hessian = 'logical',
            maxit = 'integer'
          ), 
          returnType = 'OptimResultList'
        )
      )
    )
    
    cOptimProblem <- nCompile(ncOptimProblem, OptimControlList, OptimResultList)
    
    x <- cOptimProblem$ncOptimProblem$new()
    
    
    #
    # check optimization with analytic gradients
    #
    
    r_bfgs <- x$optimgr(
      init = -20, 
      method = 'L-BFGS-B', 
      lower = -100, 
      upper = 100,
      hessian = FALSE, 
      maxit = 100 
    )
    
    # check gradient calls
    expect_gt(x$gr_calls, 0)
    x$gr_calls = 0
    
    r_nm <- x$optimgr(
      init = -20, 
      method = 'Nelder-Mead', 
      lower = -100, 
      upper = 100,
      hessian = FALSE, 
      maxit = 100 
    )
    
    # check gradient calls (Nelder-Mead doesn't use gradients)
    expect_equal(x$gr_calls, 0)
    
    o_bfgs = optim(par = -20, fn = x$f, gr = x$gr, method = 'L-BFGS-B', 
                   lower = -100, upper = 100)
    
    # check gradient calls
    expect_gt(x$gr_calls, 0)
    x$gr_calls = 0
    
    # stats::optim will silently change method = 'Nelder-Mead' to 'L-BFGS-B' if
    # bounds are included
    o_nm = optim(par = -20, fn = x$f, gr = x$gr, method = 'Nelder-Mead')
    
    # check gradient calls
    expect_equal(x$gr_calls, 0)
    
    # compare basic outputs, as some of the additional details will differ based 
    # on small differences in the control list tolerance settings
    extract_results = function(r) {
      list(par = as.numeric(r$par), value = as.numeric(r$value), 
           convergence = r$convergence)
    }
    
    # compare optimization results with r
    expect_equal(extract_results(o_bfgs), extract_results(r_bfgs))
    expect_equal(extract_results(o_nm), extract_results(r_nm))
    
    
    #
    # check optimization with analytic gradients
    #
    
    x$gr_calls = 0
    
    r_bfgs <- x$optim(
      init = -20, 
      method = 'L-BFGS-B', 
      lower = -100, 
      upper = 100,
      hessian = FALSE, 
      maxit = 100 
    )
    
    # check gradient calls
    expect_equal(x$gr_calls, 0)
    
    r_nm <- x$optim(
      init = -20, 
      method = 'Nelder-Mead', 
      lower = -100, 
      upper = 100,
      hessian = FALSE, 
      maxit = 100 
    )
    
    # check gradient calls (Nelder-Mead doesn't use gradients)
    expect_equal(x$gr_calls, 0)
    
    o_bfgs = optim(par = -20, fn = x$f, method = 'L-BFGS-B', 
                   lower = -100, upper = 100)
    
    # check gradient calls
    expect_equal(x$gr_calls, 0)
    
    # stats::optim will silently change method = 'Nelder-Mead' to 'L-BFGS-B' if
    # bounds are included
    o_nm = optim(par = -20, fn = x$f, method = 'Nelder-Mead')
    
    # check gradient calls
    expect_equal(x$gr_calls, 0)
    
    # compare optimization results with r
    expect_equal(extract_results(o_bfgs), extract_results(r_bfgs))
    expect_equal(extract_results(o_nm), extract_results(r_nm))
    
  }
  
})
    
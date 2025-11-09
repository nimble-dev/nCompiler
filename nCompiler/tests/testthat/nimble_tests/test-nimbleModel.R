# Test code needed for new nimbleModel system.
# Some or all of this should eventually go in a separate package.

library(nCompiler)
library(testthat)

#nCompile(nodeFxnBase_nClass, nodeInstr_nClass, control=list(generate_predefined=TRUE))
#nCompile(nodeInstr_nClass, calcInstr_nClass, modelBase_nClass, nodeFxnBase_nClass, calcInstrList_nClass, control=list(generate_predefined=TRUE))

test_that("nimble model prototype works", {
  nodeVarInfo <- list(list(name = "x", nDim  = 1), list(name = "mu", nDim = 1),
                  list(name = "sd", nDim = 0))
  calc_one <- nFunction(
    name = "calc_one",
    fun = function(inds = 'integerVector') {
      returnType('numericScalar')
      ans <- x[inds[1]]
      return(ans)
    }
  )
  my_nodeFxn <- make_node_fun(nodeVarInfo, list(calc_one=calc_one), "test_node")
  my_nodeInfo <- nCompiler:::make_node_info("beta_NF1", "my_nodeFxn", "test_node", nodeVarInfo)

  modelVarInfo <- list(list(name="x", nDim = 1),
                       list(name = "mu", nDim = 1),
                       list(name = "sd", nDim = 0),
                       list(name = "gamma", nDim = 2))
  #debug(makeModel_nClass)
  ncm1 <- makeModel_nClass(modelVarInfo, list(my_nodeInfo), classname = "my_model")
  #undebug(nCompiler:::addGenericInterface_impl)
  #undebug(nCompiler:::nCompile_finish_nonpackage)
  Cncm1 <- nCompile(modelBase_nClass, nodeFxnBase_nClass, calcInstrList_nClass, calcInstr_nClass, nodeInstr_nClass, ncm1, my_nodeFxn)
  obj <- Cncm1$ncm1$new()

  obj$do_setup_node_mgmt()
  nodeObj <- obj$beta_NF1
  obj$x <- 1:3
  expect_equal(obj$x, 1:3)

  obj$set_from_list(list(x = 10:11))
  # expect Problem msg: (alpha is not a field in the class)
  obj$set_from_list(list(mu = 110, x = 11:20, alpha = 101))
  obj$mu

  obj$resize_from_list(list(x = 7))
  # expect Problem msg:
  obj$resize_from_list(list(alpha = 5, mu = 3, gamma = c(2, 4)))
  expect_equal(length(obj$mu), 3)
  expect_equal(dim(obj$gamma), c(2, 4))
  obj$resize_from_list(list(x = 5, gamma = c(3, 5)))
  expect_equal(length(obj$x), 5)
  expect_equal(dim(obj$gamma), c(3, 5))

  obj$x <- 11:15
  expect_equal(nodeObj$calc_one(c(3)), 13)
  rm(obj, nodeObj); gc()
})

test_that("nodeInstr_nClass and calcInstr_nClass basics work", {
  test <- nCompile(nodeInstr_nClass, calcInstr_nClass, calcInstrList_nClass, control=list(generate_predefined=TRUE))
  calcInstrList <- test$calcInstrList_nClass$new()
  calcInstr <- test$calcInstr_nClass$new()
  expect_equal(calcInstr$nodeInstrVec, list())
  ni1 <- test$nodeInstr_nClass$new()
  ni2 <- test$nodeInstr_nClass$new()
  ni1$methodInstr <- 1
  ni2$methodInstr <- 2
  ni1$indsInstrVec <- list(1:2, 3:4)
  ni2$indsInstrVec <- list(11:12, 13:14)
  calcInstr$nodeInstrVec <- list(ni1, ni2)
  expect_true(length(calcInstr$nodeInstrVec)==2)
  expect_identical(calcInstr$nodeInstrVec[[1]]$indsInstrVec, list(1:2, 3:4))
  expect_identical(calcInstr$nodeInstrVec[[2]]$indsInstrVec, list(11:12, 13:14))
  calcInstrList$calcInstrList <- list(calcInstr)
  expect_equal(calcInstrList$calcInstrList, list(calcInstr))
  rm(calcInstrList, calcInstr, ni1, ni2); gc()
})

######

## This test works but is disabled b/c we don't have nimbleModel
## in the testing setup yet.
if(FALSE) {
library(nimbleModel)
code <- quote({
  sd ~ dunif(0, 10)
  for(i in 1:5) {
    y[i] ~ dnorm(x[i+1], sd = sd)
  }
})
m <- modelClass$new(code)
varInfo <- nCompiler:::get_varInfo_from_nimbleModel(m)
modelVars <- varInfo$vars
# Try making a model with no nodeFxns
ncm1 <- makeModel_nClass(modelVars, list(), classname = "my_model")
Cncm1 <- nCompile(modelBase_nClass, nodeFxnBase_nClass, calcInstrList_nClass, calcInstr_nClass, nodeInstr_nClass, ncm1)
obj <- Cncm1$ncm1$new()
obj$resize_from_list(varInfo$sizes)
expect_equal(length(obj$x), 6)
expect_equal(length(obj$y), 5)
expect_equal(length(obj$logProb_y), 5)
}
########
# nOptions(pause_after_writing_files=TRUE)
# Try automating the whole model creation including nodeFxns
# Ditto: this works but relies on nimbleModel
if(FALSE) {
  library(nimbleModel)
  code <- quote({
    sd ~ dunif(0, 10)
    for(i in 1:5) {
      y[i] ~ dnorm(x[i+1], sd = sd)
    }
  })
  m <- modelClass$new(code)
test <- nCompiler:::make_model_from_nimbleModel(m)

obj <- test$model$new()
obj$do_setup_node_mgmt()
vals <- list(x = 2:7, y = 11:15, sd = 8)
obj$set_from_list(vals)

nodeFxn_2_nodeIndex <- c(nodeFxn_1 = 1, nodeFxn_3 = 2)

calcInputList <- list(list(nodeFxn="nodeFxn_1",        # which declaration (nodeFxn)
                           nodeInputVec = list(list(methodInput=1,  # which index iteration method
                                                    indsInputVec=list(1))))) # input(s) to index iterations

calcInstrList <- calcInputList_to_calcInstrList(calcInputList, test)

obj$calculate(calcInstrList)
}
########

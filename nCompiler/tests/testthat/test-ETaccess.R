## Test the ET (Eigen::Tensor) access classes.
## These are classes we wrote to provide more flexible access
## to Eigen::Tensor objects, for example dropping or adding
## singleton dimensions.
##
## They also provide a type-generic way to access variables by name
## through the generic interface system.

## To-do: add tests of expecting the wrong scalar type via scalar(), ref() and map()

test_that("access and ETaccess work for doubles", {
  nc <- nClass(
    Cpublic = list(
      DS = 'numericScalar',
      DV = 'numericVector',
      DM = 'numericMatrix',
      ##### numericScalar
      DS_get_set_inter_ref = nFunction(
        function() {
          cppLiteral('ans = 10 * access("DS")->scalar();', types = list(ans='numericScalar'))
          cppLiteral('access("DS")->scalar() = 2*ans;')
          cppLiteral('return access("DS")->scalar();')
          returnType('numericScalar')
        }
      )
    , DS_get_set_inter_map_larger = nFunction(
      function() {
        cppLiteral('ans = 10 * access("DS")->map<1>();', types = list(ans='numericVector'))
        cppLiteral('access("DS")->map<1>() = 2*ans;')
        cppLiteral('return access("DS")->map<1>();')
        returnType('numericVector')
      }
    )
  , DS_get_set_arg_ref = nFunction(
    function(DSx = 'numericScalar') {
      cppLiteral('ans = 10 * ETaccess(DSx).scalar();', types = list(ans='numericScalar'))
      cppLiteral('ETaccess(DSx).scalar() = 2*ans;')
      cppLiteral('return ETaccess(DSx).scalar();')
      returnType('numericScalar')
    }
  )
, DS_get_set_arg_map_larger = nFunction(
  function(DSx = 'numericScalar') {
    cppLiteral('ans = 10 * ETaccess(DSx).map<1>();', types = list(ans='numericVector'))
    cppLiteral('ETaccess(DSx).map<1>() = 2*ans;')
    cppLiteral('return ETaccess(DSx).map<1>();')
    returnType('numericVector')
  }
)
, DS_trap_ref = nFunction(
  function() {
    ## expect error:
    cppLiteral('flex_(ans) = 10 * access("DS")->ref<0>();', types = list(ans='numericScalar'))
    cppLiteral('return ans;')
    returnType('numericScalar')
  }
)
##### numericVector
, DV_get_set_inter_ref = nFunction(
  function() {
    cppLiteral('ans = 10 * access("DV")->ref<1>();', types = list(ans='numericVector'))
    cppLiteral('access("DV")->ref<1>() = 2*ans;')
    cppLiteral('return access("DV")->ref<1>();')
    returnType('numericVector')
  }
)
, DV_get_set_inter_map_larger = nFunction(
  function() {
    cppLiteral('ans = 10 * access("DV")->map<2>();', types = list(ans='numericMatrix'))
    cppLiteral('access("DV")->map<2>() = 2*ans;')
    cppLiteral('return access("DV")->map<2>();')
    returnType('numericMatrix')
  }
)
, DV_get_set_inter_map_smaller = nFunction(
  function() {
    cppLiteral('ans = 10 * access("DV")->scalar();', types = list(ans='numericScalar'))
    cppLiteral('access("DV")->scalar() = 2*ans;')
    cppLiteral('return access("DV")->scalar();')
    returnType('numericScalar')
  }
)
, DV_get_set_arg_ref = nFunction(
  function(DVx = 'numericVector') {
    cppLiteral('ans = 10 * ETaccess(DVx).ref<1>();', types = list(ans='numericVector'))
    cppLiteral('ETaccess(DVx).ref<1>() = 2*ans;')
    cppLiteral('return ETaccess(DVx).ref<1>();')
    returnType('numericVector')
  }
)
, DV_get_set_arg_map_larger = nFunction(
  function(DVx = 'numericVector') {
    cppLiteral('ans = 10 * ETaccess(DVx).map<2>();', types = list(ans='numericMatrix'))
    cppLiteral('ETaccess(DVx).map<2>() = 2*ans;')
    cppLiteral('return ETaccess(DVx).map<2>();')
    returnType('numericMatrix')
  }
)
, DV_get_set_arg_map_smaller = nFunction(
  function(DVx = 'numericVector') {
    cppLiteral('ans = 10 * ETaccess(DVx).scalar();', types = list(ans='numericScalar'))
    cppLiteral('ETaccess(DVx).scalar() = 2*ans;')
    cppLiteral('return ETaccess(DVx).scalar();')
    returnType('numericScalar')
  }
)

##### numericMatrix
, DM_get_set_inter_ref = nFunction(
  function() {
    cppLiteral('ans = 10 * access("DM")->ref<2>();', types = list(ans='numericMatrix'))
    cppLiteral('access("DM")->ref<2>() = 2*ans;')
    cppLiteral('return access("DM")->ref<2>();')
    returnType('numericMatrix')
  }
)
, DM_get_set_inter_map_larger = nFunction(
  function() {
    cppLiteral('ans = 10 * access("DM")->map<3>();', types = list(ans='numericArray(nDim=3)'))
    cppLiteral('access("DM")->map<3>() = 2*ans;')
    cppLiteral('return access("DM")->map<3>();')
    returnType('numericArray(nDim=3)')
  }
)
, DM_get_set_inter_map_smaller = nFunction(
  function() {
    cppLiteral('ans = 10 * access("DM")->map<1>();', types = list(ans='numericVector'))
    cppLiteral('access("DM")->map<1>() = 2*ans;')
    cppLiteral('return access("DM")->map<1>();')
    returnType('numericVector')
  }
)
,  DM_get_set_inter_map_scalar = nFunction(
  function() {
    cppLiteral('ans = 10 * access("DM")->scalar();', types = list(ans='numericScalar'))
    cppLiteral('access("DM")->scalar() = 2*ans;')
    cppLiteral('return access("DM")->scalar();')
    returnType('numericScalar')
  }
)  , DM_get_set_arg_ref = nFunction(
  function(DMx = 'numericMatrix') {
    cppLiteral('ans = 10 * ETaccess(DMx).ref<2>();', types = list(ans='numericMatrix'))
    cppLiteral('ETaccess(DMx).ref<2>() = 2*ans;')
    cppLiteral('return ETaccess(DMx).ref<2>();')
    returnType('numericMatrix')
  }
)
, DM_get_set_arg_map_larger = nFunction(
  function(DMx = 'numericMatrix') {
    cppLiteral('ans = 10 * ETaccess(DMx).map<3>();', types = list(ans='numericArray(nDim=3)'))
    cppLiteral('ETaccess(DMx).map<3>() = 2*ans;')
    cppLiteral('return ETaccess(DMx).map<3>();')
    returnType('numericArray(nDim=3)')
  }
)
, DM_get_set_arg_map_smaller = nFunction(
  function(DMx = 'numericMatrix') {
    cppLiteral('ans = 10 * ETaccess(DMx).map<1>();', types = list(ans='numericVector'))
    cppLiteral('ETaccess(DMx).map<1>() = 2*ans;')
    cppLiteral('return ETaccess(DMx).map<1>();')
    returnType('numericVector')
  }
)
, DM_get_set_arg_map_scalar = nFunction(
  function(DMx = 'numericMatrix') {
    cppLiteral('ans = 10 * ETaccess(DMx).scalar();', types = list(ans='numericScalar'))
    cppLiteral('ETaccess(DMx).scalar() = 2*ans;')
    cppLiteral('return ETaccess(DMx).scalar();')
    returnType('numericScalar')
  }
)
)
)
  Cnc <- nCompile(nc)
  # it all works but breaks for member variable that is not an Eigen::Tensor.

  obj <- Cnc$new()
  DS <- 1.23456
  DV <- DS * 1:3
  DM <- matrix(DS * 1:12, nrow = 3)
  reset <- function(obj) {
    obj$DS <- DS
    obj$DV <- DV
    obj$DM <- DM
  }

  # Double scalar cases
  reset(obj); expect_equal(obj$DS_get_set_inter_ref(), DS*20)
  reset(obj); expect_equal(obj$DS_get_set_inter_map_larger(), DS*20)
  expect_equal(obj$DS_get_set_arg_ref(DS), DS*20)
  expect_equal(obj$DS_get_set_arg_map_larger(DS), DS*20)
  reset(obj); expect_error(obj$DS_trap_ref())

  # Double vector cases (via interface)
  reset(obj); expect_equal    (obj$DV_get_set_inter_ref(), DV*20)
  reset(obj); expect_identical(obj$DV_get_set_inter_map_larger(), matrix(DV*20))
  reset(obj); expect_error    (obj$DV_get_set_inter_map_smaller())
  reset(obj);
  obj$DV<-DS; expect_identical(obj$DV_get_set_inter_map_smaller(), DS*20)
  # Double vector cases (via arg)
  expect_equal    (obj$DV_get_set_arg_ref(DV), DV*20)
  expect_identical(obj$DV_get_set_arg_map_larger(DV), matrix(DV*20))
  expect_error    (obj$DV_get_set_arg_map_smaller(DV))
  expect_identical(obj$DV_get_set_arg_map_smaller(DS), DS*20)

  # Double matrix cases (via interface)
  reset(obj); expect_equal    (obj$DM_get_set_inter_ref(), DM*20)
  reset(obj); expect_identical(obj$DM_get_set_inter_map_larger(), array(DM*20, dim=c(nrow(DM), ncol(DM), 1)))
  reset(obj); expect_error    (obj$DM_get_set_inter_map_smaller())
  reset(obj);
  obj$DM<-matrix(DV); expect_identical(obj$DM_get_set_inter_map_smaller(), DV*20)
  reset(obj); expect_error    (obj$DM_get_set_inter_map_scalar())
  reset(obj);
  obj$DM<-matrix(DS); expect_identical(obj$DM_get_set_inter_map_smaller(), DS*20)
  # Double matrix cases (via arg)
  expect_equal    (obj$DM_get_set_arg_ref(DM), DM*20)
  expect_identical(obj$DM_get_set_arg_map_larger(DM), array(DM*20, dim=c(nrow(DM), ncol(DM), 1)))
  expect_error    (obj$DM_get_set_arg_map_smaller(DM))
  expect_identical(obj$DM_get_set_arg_map_smaller(matrix(DV)), DV*20)
  expect_error    (obj$DM_get_set_arg_map_scalar(DM))
  expect_identical(obj$DM_get_set_arg_map_scalar(matrix(DS)), DS*20)
})

test_that("access and ETaccess work for integers", {
  nc <- nClass(
    Cpublic = list(
      IS = 'integerScalar',
      IV = 'integerVector',
      IM = 'integerMatrix',
      ##### integerScalar
      IS_get_set_inter_ref = nFunction(
        function() {
          cppLiteral('ans = 10 * access("IS")->scalar<int>();', types = list(ans='integerScalar'))
          cppLiteral('access("IS")->scalar<int>() = 2*ans;')
          cppLiteral('return access("IS")->scalar<int>();')
          returnType('integerScalar')
        }
      )
    , IS_get_set_inter_map_larger = nFunction(
      function() {
        cppLiteral('ans = 10 * access("IS")->map<1,int>();', types = list(ans='integerVector'))
        cppLiteral('access("IS")->map<1,int>() = 2*ans;')
        cppLiteral('return access("IS")->map<1,int>();')
        returnType('integerVector')
      }
    )
  , IS_get_set_arg_ref = nFunction(
    function(ISx = 'integerScalar') {
      cppLiteral('ans = 10 * ETaccess(ISx).scalar<int>();', types = list(ans='integerScalar'))
      cppLiteral('ETaccess(ISx).scalar<int>() = 2*ans;')
      cppLiteral('return ETaccess(ISx).scalar<int>();')
      returnType('integerScalar')
    }
  )
, IS_get_set_arg_map_larger = nFunction(
  function(ISx = 'integerScalar') {
    cppLiteral('ans = 10 * ETaccess(ISx).map<1,int>();', types = list(ans='integerVector'))
    cppLiteral('ETaccess(ISx).map<1,int>() = 2*ans;')
    cppLiteral('return ETaccess(ISx).map<1,int>();')
    returnType('integerVector')
  }
)
, IS_trap_ref = nFunction(
  function() {
    ## expect error:
    cppLiteral('flex_(ans) = 10 * access("IS")->ref<0,int>();', types = list(ans='integerScalar'))
    cppLiteral('return ans;')
    returnType('integerScalar')
  }
)
##### integerVector
, IV_get_set_inter_ref = nFunction(
  function() {
    cppLiteral('ans = 10 * access("IV")->ref<1,int>();', types = list(ans='integerVector'))
    cppLiteral('access("IV")->ref<1,int>() = 2*ans;')
    cppLiteral('return access("IV")->ref<1,int>();')
    returnType('integerVector')
  }
)
, IV_get_set_inter_map_larger = nFunction(
  function() {
    cppLiteral('ans = 10 * access("IV")->map<2,int>();', types = list(ans='integerMatrix'))
    cppLiteral('access("IV")->map<2,int>() = 2*ans;')
    cppLiteral('return access("IV")->map<2,int>();')
    returnType('integerMatrix')
  }
)
, IV_get_set_inter_map_smaller = nFunction(
  function() {
    cppLiteral('ans = 10 * access("IV")->scalar<int>();', types = list(ans='integerScalar'))
    cppLiteral('access("IV")->scalar<int>() = 2*ans;')
    cppLiteral('return access("IV")->scalar<int>();')
    returnType('integerScalar')
  }
)
, IV_get_set_arg_ref = nFunction(
  function(IVx = 'integerVector') {
    cppLiteral('ans = 10 * ETaccess(IVx).ref<1,int>();', types = list(ans='integerVector'))
    cppLiteral('ETaccess(IVx).ref<1,int>() = 2*ans;')
    cppLiteral('return ETaccess(IVx).ref<1,int>();')
    returnType('integerVector')
  }
)
, IV_get_set_arg_map_larger = nFunction(
  function(IVx = 'integerVector') {
    cppLiteral('ans = 10 * ETaccess(IVx).map<2,int>();', types = list(ans='integerMatrix'))
    cppLiteral('ETaccess(IVx).map<2,int>() = 2*ans;')
    cppLiteral('return ETaccess(IVx).map<2,int>();')
    returnType('integerMatrix')
  }
)
, IV_get_set_arg_map_smaller = nFunction(
  function(IVx = 'integerVector') {
    cppLiteral('ans = 10 * ETaccess(IVx).scalar<int>();', types = list(ans='integerScalar'))
    cppLiteral('ETaccess(IVx).scalar<int>() = 2*ans;')
    cppLiteral('return ETaccess(IVx).scalar<int>();')
    returnType('integerScalar')
  }
)

##### integerMatrix
, IM_get_set_inter_ref = nFunction(
  function() {
    cppLiteral('ans = 10 * access("IM")->ref<2,int>();', types = list(ans='integerMatrix'))
    cppLiteral('access("IM")->ref<2,int>() = 2*ans;')
    cppLiteral('return access("IM")->ref<2,int>();')
    returnType('integerMatrix')
  }
)
, IM_get_set_inter_map_larger = nFunction(
  function() {
    cppLiteral('ans = 10 * access("IM")->map<3,int>();', types = list(ans='integerArray(nDim=3)'))
    cppLiteral('access("IM")->map<3,int>() = 2*ans;')
    cppLiteral('return access("IM")->map<3,int>();')
    returnType('integerArray(nDim=3)')
  }
)
, IM_get_set_inter_map_smaller = nFunction(
  function() {
    cppLiteral('ans = 10 * access("IM")->map<1,int>();', types = list(ans='integerVector'))
    cppLiteral('access("IM")->map<1,int>() = 2*ans;')
    cppLiteral('return access("IM")->map<1,int>();')
    returnType('integerVector')
  }
)
,  IM_get_set_inter_map_scalar = nFunction(
  function() {
    cppLiteral('ans = 10 * access("IM")->scalar<int>();', types = list(ans='integerScalar'))
    cppLiteral('access("IM")->scalar<int>() = 2*ans;')
    cppLiteral('return access("IM")->scalar<int>();')
    returnType('integerScalar')
  }
)  , IM_get_set_arg_ref = nFunction(
  function(IMx = 'integerMatrix') {
    cppLiteral('ans = 10 * ETaccess(IMx).ref<2,int>();', types = list(ans='integerMatrix'))
    cppLiteral('ETaccess(IMx).ref<2,int>() = 2*ans;')
    cppLiteral('return ETaccess(IMx).ref<2,int>();')
    returnType('integerMatrix')
  }
)
, IM_get_set_arg_map_larger = nFunction(
  function(IMx = 'integerMatrix') {
    cppLiteral('ans = 10 * ETaccess(IMx).map<3,int>();', types = list(ans='integerArray(nDim=3)'))
    cppLiteral('ETaccess(IMx).map<3,int>() = 2*ans;')
    cppLiteral('return ETaccess(IMx).map<3,int>();')
    returnType('integerArray(nDim=3)')
  }
)
, IM_get_set_arg_map_smaller = nFunction(
  function(IMx = 'integerMatrix') {
    cppLiteral('ans = 10 * ETaccess(IMx).map<1,int>();', types = list(ans='integerVector'))
    cppLiteral('ETaccess(IMx).map<1,int>() = 2*ans;')
    cppLiteral('return ETaccess(IMx).map<1,int>();')
    returnType('integerVector')
  }
)
, IM_get_set_arg_map_scalar = nFunction(
  function(IMx = 'integerMatrix') {
    cppLiteral('ans = 10 * ETaccess(IMx).scalar<int>();', types = list(ans='integerScalar'))
    cppLiteral('ETaccess(IMx).scalar<int>() = 2*ans;')
    cppLiteral('return ETaccess(IMx).scalar<int>();')
    returnType('integerScalar')
  }
)
)
)
  Cnc <- nCompile(nc)
  # it all works but breaks for member variable that is not an Eigen::Tensor.

  obj <- Cnc$new()
  IS <- 7L
  IV <- IS * 1:3
  IM <- matrix(IS * 1:12, nrow = 3)
  reset <- function(obj) {
    obj$IS <- IS
    obj$IV <- IV
    obj$IM <- IM
  }

  # Integer scalar cases
  reset(obj); expect_equal(obj$IS_get_set_inter_ref(), IS*20L)
  reset(obj); expect_equal(obj$IS_get_set_inter_map_larger(), IS*20L)
  expect_equal(obj$IS_get_set_arg_ref(IS), IS*20L)
  expect_equal(obj$IS_get_set_arg_map_larger(IS), IS*20L)
  reset(obj); expect_error(obj$IS_trap_ref())

  # Integer vector cases (via interface)
  reset(obj); expect_equal    (obj$IV_get_set_inter_ref(), IV*20L)
  reset(obj); expect_identical(obj$IV_get_set_inter_map_larger(), matrix(IV*20L))
  reset(obj); expect_error    (obj$IV_get_set_inter_map_smaller())
  reset(obj);
  obj$IV<-IS; expect_identical(obj$IV_get_set_inter_map_smaller(), IS*20L)
  # Integer vector cases (via arg)
  expect_equal    (obj$IV_get_set_arg_ref(IV), IV*20L)
  expect_identical(obj$IV_get_set_arg_map_larger(IV), matrix(IV*20L))
  expect_error    (obj$IV_get_set_arg_map_smaller(IV))
  expect_identical(obj$IV_get_set_arg_map_smaller(IS), IS*20L)

  # Integer matrix cases (via interface)
  reset(obj); expect_equal    (obj$IM_get_set_inter_ref(), IM*20L)
  reset(obj); expect_identical(obj$IM_get_set_inter_map_larger(), array(IM*20L, dim=c(nrow(IM), ncol(IM), 1)))
  reset(obj); expect_error    (obj$IM_get_set_inter_map_smaller())
  reset(obj);
  obj$IM<-matrix(IV); expect_identical(obj$IM_get_set_inter_map_smaller(), IV*20L)
  reset(obj); expect_error    (obj$IM_get_set_inter_map_scalar())
  reset(obj);
  obj$IM<-matrix(IS); expect_identical(obj$IM_get_set_inter_map_smaller(), IS*20L)
  # Integer matrix cases (via arg)
  expect_equal    (obj$IM_get_set_arg_ref(IM), IM*20L)
  expect_identical(obj$IM_get_set_arg_map_larger(IM), array(IM*20L, dim=c(nrow(IM), ncol(IM), 1)))
  expect_error    (obj$IM_get_set_arg_map_smaller(IM))
  expect_identical(obj$IM_get_set_arg_map_smaller(matrix(IV)), IV*20L)
  expect_error    (obj$IM_get_set_arg_map_scalar(IM))
  expect_identical(obj$IM_get_set_arg_map_scalar(matrix(IS)), IS*20L)
})


test_that("access and ETaccess work for logicals", {
  nc <- nClass(
    Cpublic = list(
      LS = 'logicalScalar',
      LV = 'logicalVector',
      LM = 'logicalMatrix',
      ##### logicalScalar
      LS_get_set_inter_ref = nFunction(
        function() {
          cppLiteral('ans = !(access("LS")->scalar<bool>());',
                     types = list(ans='logicalScalar'))
          cppLiteral('access("LS")->scalar<bool>() = ans;')
          cppLiteral('return access("LS")->scalar<bool>();')
          returnType('logicalScalar')
        }
      )
    , LS_get_set_inter_map_larger = nFunction(
      function() {
        cppLiteral('ans = (1-(access("LS")->map<1,bool>()).cast<double>()).cast<bool>();',
                   types = list(ans='logicalVector'))
        cppLiteral('access("LS")->map<1,bool>() = ans;')
        cppLiteral('return access("LS")->map<1,bool>();')
        returnType('logicalVector')
      }
    )
  , LS_get_set_arg_ref = nFunction(
    function(LSx = 'logicalScalar') {
      cppLiteral('ans =  !(ETaccess(LSx).scalar<bool>());', types = list(ans='logicalScalar'))
      cppLiteral('ETaccess(LSx).scalar<bool>() = ans;')
      cppLiteral('return ETaccess(LSx).scalar<bool>();')
      returnType('logicalScalar')
    }
  )
, LS_get_set_arg_map_larger = nFunction(
  function(LSx = 'logicalScalar') {
    cppLiteral('ans = (1-(ETaccess(LSx).map<1,bool>()).cast<double>()).cast<bool>();',
               types = list(ans='logicalVector'))
    cppLiteral('ETaccess(LSx).map<1,bool>() = ans;')
    cppLiteral('return ETaccess(LSx).map<1,bool>();')
    returnType('logicalVector')
  }
)
## , LS_trap_ref = nFunction(
##   function() {
##     ## expect error:
##     cppLiteral('flex_(ans) =  access("LS")->ref<0,bool>();', types = list(ans='logicalScalar'))
##     cppLiteral('return ans;')
##     returnType('logicalScalar')
##   }
## )
##### logicalVector
, LV_get_set_inter_ref = nFunction(
  function() {
    cppLiteral('ans = (1-(access("LV")->ref<1,bool>()).cast<double>()).cast<bool>();',
               types = list(ans='logicalVector'))
    cppLiteral('access("LV")->ref<1,bool>() = ans;')
    cppLiteral('return access("LV")->ref<1,bool>();')
    returnType('logicalVector')
  }
)
, LV_get_set_inter_map_larger = nFunction(
  function() {
    cppLiteral('ans =  (1-(access("LV")->map<2,bool>()).cast<double>()).cast<bool>();', types = list(ans='logicalMatrix'))
    cppLiteral('access("LV")->map<2,bool>() = ans;')
    cppLiteral('return access("LV")->map<2,bool>();')
    returnType('logicalMatrix')
  }
)
, LV_get_set_inter_map_smaller = nFunction(
  function() {
    cppLiteral('ans =  !access("LV")->scalar<bool>();', types = list(ans='logicalScalar'))
    cppLiteral('access("LV")->scalar<bool>() = ans;')
    cppLiteral('return access("LV")->scalar<bool>();')
    returnType('logicalScalar')
  }
)
, LV_get_set_arg_ref = nFunction(
  function(LVx = 'logicalVector') {
    cppLiteral('ans = (1-(ETaccess(LVx).ref<1,bool>()).cast<double>()).cast<bool>();', types = list(ans='logicalVector'))
    cppLiteral('ETaccess(LVx).ref<1,bool>() = ans;')
    cppLiteral('return ETaccess(LVx).ref<1,bool>();')
    returnType('logicalVector')
  }
)
, LV_get_set_arg_map_larger = nFunction(
  function(LVx = 'logicalVector') {
    cppLiteral('ans = (1-(ETaccess(LVx).map<2,bool>()).cast<double>()).cast<bool>();', types = list(ans='logicalMatrix'))
    cppLiteral('ETaccess(LVx).map<2,bool>() = ans;')
    cppLiteral('return ETaccess(LVx).map<2,bool>();')
    returnType('logicalMatrix')
  }
)
, LV_get_set_arg_map_smaller = nFunction(
  function(LVx = 'logicalVector') {
    cppLiteral('ans = !ETaccess(LVx).scalar<bool>();', types = list(ans='logicalScalar'))
    cppLiteral('ETaccess(LVx).scalar<bool>() = ans;')
    cppLiteral('return ETaccess(LVx).scalar<bool>();')
    returnType('logicalScalar')
  }
)
##### logicalMatrix
, LM_get_set_inter_ref = nFunction(
  function() {
    cppLiteral('ans = (1-(access("LM")->ref<2,bool>()).cast<double>()).cast<bool>();',
               types = list(ans='logicalMatrix'))
    cppLiteral('access("LM")->ref<2,bool>() = ans;')
    cppLiteral('return access("LM")->ref<2,bool>();')
    returnType('logicalMatrix')
  }
)
, LM_get_set_inter_map_larger = nFunction(
  function() {
    cppLiteral('ans = (1-(access("LM")->map<3,bool>()).cast<double>()).cast<bool>();', types = list(ans='logicalArray(nDim=3)'))
    cppLiteral('access("LM")->map<3,bool>() = ans;')
    cppLiteral('return access("LM")->map<3,bool>();')
    returnType('logicalArray(nDim=3)')
  }
)
, LM_get_set_inter_map_smaller = nFunction(
  function() {
    cppLiteral('ans = (1-(access("LM")->map<1,bool>()).cast<double>()).cast<bool>();', types = list(ans='logicalVector'))
    cppLiteral('access("LM")->map<1,bool>() = ans;')
    cppLiteral('return access("LM")->map<1,bool>();')
    returnType('logicalVector')
  }
)
,  LM_get_set_inter_map_scalar = nFunction(
  function() {
    cppLiteral('ans = !access("LM")->scalar<bool>();', types = list(ans='logicalScalar'))
    cppLiteral('access("LM")->scalar<bool>() = ans;')
    cppLiteral('return access("LM")->scalar<bool>();')
    returnType('logicalScalar')
  }
)  , LM_get_set_arg_ref = nFunction(
  function(LMx = 'logicalMatrix') {
    cppLiteral('ans = (1-(ETaccess(LMx).ref<2,bool>()).cast<double>()).cast<bool>();', types = list(ans='logicalMatrix'))
    cppLiteral('ETaccess(LMx).ref<2,bool>() = ans;')
    cppLiteral('return ETaccess(LMx).ref<2,bool>();')
    returnType('logicalMatrix')
  }
)
, LM_get_set_arg_map_larger = nFunction(
  function(LMx = 'logicalMatrix') {
    cppLiteral('ans = (1-(ETaccess(LMx).map<3,bool>()).cast<double>()).cast<bool>();',
               types = list(ans='logicalArray(nDim=3)'))
    cppLiteral('ETaccess(LMx).map<3,bool>() = ans;')
    cppLiteral('return ETaccess(LMx).map<3,bool>();')
    returnType('logicalArray(nDim=3)')
  }
)
, LM_get_set_arg_map_smaller = nFunction(
  function(LMx = 'logicalMatrix') {
    cppLiteral('ans = (1-(ETaccess(LMx).map<1,bool>()).cast<double>()).cast<bool>();',
               types = list(ans='logicalVector'))
    cppLiteral('ETaccess(LMx).map<1,bool>() = ans;')
    cppLiteral('return ETaccess(LMx).map<1,bool>();')
    returnType('logicalVector')
  }
)
, LM_get_set_arg_map_scalar = nFunction(
  function(LMx = 'logicalMatrix') {
    cppLiteral('ans = !ETaccess(LMx).scalar<bool>();', types = list(ans='logicalScalar'))
    cppLiteral('ETaccess(LMx).scalar<bool>() = ans;')
    cppLiteral('return ETaccess(LMx).scalar<bool>();')
    returnType('logicalScalar')
  }
)
)
)
  Cnc <- nCompile(nc)
  # it all works but breaks for member variable that is not an Eigen::Tensor.

  obj <- Cnc$new()
  LS <- TRUE
  LV <- c(TRUE, FALSE, TRUE)
  LM <- matrix(rep(LV, 4), nrow = 3)
  reset <- function(obj) {
    obj$LS <- LS
    obj$LV <- LV
    obj$LM <- LM
  }

  # Logical scalar cases
  reset(obj); expect_equal(obj$LS_get_set_inter_ref(), !LS)
  reset(obj); expect_equal(obj$LS_get_set_inter_map_larger(), !LS)
  expect_equal(obj$LS_get_set_arg_ref(LS), !LS)
  expect_equal(obj$LS_get_set_arg_map_larger(LS), !LS)
  reset(obj); expect_error(obj$LS_trap_ref())

  # Logical vector cases (via interface)
  reset(obj); expect_equal    (obj$LV_get_set_inter_ref(), !LV)
  reset(obj); expect_identical(obj$LV_get_set_inter_map_larger(), matrix(!LV))
  reset(obj); expect_error    (obj$LV_get_set_inter_map_smaller())
  reset(obj);
  obj$LV<-LS; expect_identical(obj$LV_get_set_inter_map_smaller(), !LS)
  # Logical vector cases (via arg)
  expect_equal    (obj$LV_get_set_arg_ref(LV), !LV)
  expect_identical(obj$LV_get_set_arg_map_larger(LV), matrix(!LV))
  expect_error    (obj$LV_get_set_arg_map_smaller(LV))
  expect_identical(obj$LV_get_set_arg_map_smaller(LS), !LS)

  # Logical matrix cases (via interface)
  reset(obj); expect_equal    (obj$LM_get_set_inter_ref(), !LM)
  reset(obj); expect_identical(obj$LM_get_set_inter_map_larger(), array(!LM, dim=c(nrow(LM), ncol(LM), 1)))
  reset(obj); expect_error    (obj$LM_get_set_inter_map_smaller())
  reset(obj);
  obj$LM<-matrix(LV); expect_identical(obj$LM_get_set_inter_map_smaller(), !LV)
  reset(obj); expect_error    (obj$LM_get_set_inter_map_scalar())
  reset(obj);
  obj$LM<-matrix(LS); expect_identical(obj$LM_get_set_inter_map_smaller(), !LS)
  # Logical matrix cases (via arg)
  expect_equal    (obj$LM_get_set_arg_ref(LM), !LM)
  expect_identical(obj$LM_get_set_arg_map_larger(LM), array(!LM, dim=c(nrow(LM), ncol(LM), 1)))
  expect_error    (obj$LM_get_set_arg_map_smaller(LM))
  expect_identical(obj$LM_get_set_arg_map_smaller(matrix(LV)), !LV)
  expect_error    (obj$LM_get_set_arg_map_scalar(LM))
  expect_identical(obj$LM_get_set_arg_map_scalar(matrix(LS)), !LS)
})

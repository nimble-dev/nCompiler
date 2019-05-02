## Function to create a "full interface" to a compiled nClass.
## By "full interface", we mean an R6 class with active bindings
## for Cpublic member data and functions for Cpublic methods that will
## call the corresponding compiled methods.

# build_compiled_nClass generates an R6 class to interface to a compiled nClass.
# This commented code shows a protoype of the kind of class definition created.
#
# If nc1 is created as follows:
# nc1 <- nClass(
#   Rpublic = list(
#     Rv = NULL,
#     Rfoo = function(x) x+1
#   ),
#   Cpublic = list(
#     Cv = 'numericScalar',
#     Cfoo = nFunction(
#       fun = function(x) {
#         return(x+1)
#       },
#       argTypes = list(x = 'numericScalar'),
#       returnType = 'numericScalar')
#   )
# )
#
# then build_nClassInterface would be like the following:
#
# FI <- R6::R6Class(
#   private = list(
#     CppObj = NULL
#   ),
#   public = list(
#     initialize = function(CppObj) {
#       if(missing(CppObj))
#         stop("Cannot create a nClass full interface object without a CppObj.")
#       private$CppObj <- CppObj
#     },
#     Rv = NULL,                 #R fields  (Cgenerator$public_fields[RfieldNames])
#     Rfoo = function(x) x+1,    #R methods (NCgenerator$public_methods[RmethodNames])
#     Cfoo = function(x) {       #C methods (CinterfaceMethods)
#       nCompiler:::call_method(getExtptr(private$CppObj), 'Cfoo', list(x))
#     }
#   ),
#   active = list(
#     Cv = function(value) {     #C fields (activeBindings)
#       if(missing(value))
#         nCompiler:::get_value(getExtptr(private$CppObj), 'Cv')
#       else
#         nCompiler:::set_value(getExtptr(private$CppObj), 'Cv', value)
#     }
#   )
# )
# 

#' @export
build_compiled_nClass <- function(NCgenerator,
                                  newCobjFun,
                                  env = parent.frame(),
                                  quoted = FALSE) {
  NCI <- NCinternals(NCgenerator)
  CmethodNames <- NCI$methodNames
  RmethodNames <- setdiff(names(NCgenerator$public_methods), 
                          c(CmethodNames, 'clone'))
  CinterfaceMethods <- mapply(buildMethod_for_compiled_nClass,
                              NCgenerator$public_methods[CmethodNames],
                              CmethodNames)
  CfieldNames <- NCI$fieldNames
  RfieldNames <- setdiff(names(NCgenerator$public_fields),
                         CfieldNames)
  activeBindings <- lapply(CfieldNames,
                           buildActiveBinding_for_compiled_nClass)
  names(activeBindings) = CfieldNames
  classname <- paste0(NCgenerator$classname, '_compiled')

  ## get the RcppPacket to pass to compileCpp_nCompiler
  RcppPacket <- NCinternals(NCgenerator)$RcppPacket

  ans <- substitute(
    expr = R6::R6Class(
                 classname = CLASSNAME,
                 private = list(
                   CppObj = NULL
                 ),
                 public = c(
                   list(initialize = function(CppObj) {
                     if(missing(CppObj)) {
                       ## codeDir is added as a private field of generators
                       ## embedded in packages created by writePackage()
                       newCobjFun <- nCompiler:::compileCpp_nCompiler(
                                                   RCPPPACKET,
                                                   dir = self$private$codeDir
                                                 )
                       if(is.null(newCobjFun))
                         stop("Cannot create a nClass full interface object without a newCobjFun or a CppObj argument.")
                       CppObj <- newCobjFun()
                     }
                     private$CppObj <- CppObj
                   }),
                   RPUBLIC,
                   RFIELDS,
                   CINTERFACE),
                 active = ACTIVEBINDINGS,
                 portable = FALSE,
                 inherit = nCompiler:::nClassClass,
                 parent_env = NULL ## when quoted = TRUE, env argument is not used
               ),
    env = list(
      CLASSNAME = classname,
      RCPPPACKET = RcppPacket,
      RPUBLIC = parse(text = deparse(
                        NCgenerator$public_methods[RmethodNames]
                      ))[[1]],
      RFIELDS = parse(text = deparse(
                        NCgenerator$public_fields[RfieldNames])
                      )[[1]],
      CINTERFACE = parse(text = deparse(CinterfaceMethods))[[1]],
      ACTIVEBINDINGS = parse(text = deparse(activeBindings))[[1]]
    )
  )

  if (quoted) return(ans)

  ans <- eval(ans)
  ans$public_methods$initialize <- function(CppObj) {
    if(missing(CppObj)) {
      newCobjFun <- parent.env(parent.env(self))$.newCobjFun
      if(is.null(newCobjFun))
        stop("Cannot create a nClass full interface object without a newCobjFun or a CppObj argument.")
      CppObj <- newCobjFun()
    }
    private$CppObj <- CppObj
  }

  new_env <- new.env(parent = env)
  ans$parent_env <- new_env

  if(!missing(newCobjFun)) {
    # Similar to .internals in nClass,
    # we put newCobjFun in two places:
    # 1. In the generator
    # 2. In the environment that every object will have as its parent.env
    ans$.newCobjFun <- newCobjFun
    new_env$.newCobjFun <- newCobjFun
  } else {
    ans$.newCobjFun <- NULL
    new_env$.newCobjFun <- NULL
  }
  ans
}

buildActiveBinding_for_compiled_nClass <- function(name) {
  ans <- function(value) {}
  body(ans) <- substitute(
    {
      if(missing(value))
        nCompiler:::get_value(nCompiler:::getExtptr(private$CppObj), NAME)
      else
        nCompiler:::set_value(nCompiler:::getExtptr(private$CppObj), NAME, value)
    },
    list(NAME = name)
  )
  ans
}

buildMethod_for_compiled_nClass <- function(fun, name) {
  if(is.null(fun)) return(NULL) ## convenient for how this is used from mapply
  argNames <- names(formals(fun))
  ans <- fun
  environment(ans) <- new.env()
  ## The internet says that R6 methods are assigned their environments
  ## during a call to methodGenerator$new().  We put a new.env()
  ## here anyway as insurance against the possibility of quirky 
  ## environment problems.
  listcode <- quote(list())
  for(i in seq_along(argNames))
    listcode[[i+1]] <- as.name(argNames[i])
  body(ans) <- substitute(
    nCompiler:::call_method(nCompiler:::getExtptr(private$CppObj), NAME, LISTCODE),
    list(NAME = name,
         LISTCODE = listcode)
  )
  ans
}

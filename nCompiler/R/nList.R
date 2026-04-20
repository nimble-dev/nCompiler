# This is a rudimentary place-holder for uncompiled nList.
# It simply returns a list of the given length and ignores the type
# It means that test-nList is a bit klugey.

#' @export
nList <- function(type, length) vector("list", length)


#' @keywords internal
#' @noRd
#' @rawNamespace export(nList2Base_nClass)
NULL

nList2Base_nClass <- nClass(
  classname = "nList2Base_nClass",
  Cpublic = list(
    ping = nFunction(
        name = "ping",
        function() {return(TRUE); returnType(logical())},
        compileInfo = list(virtual=TRUE)
    )
  ),
  # See comment above about needing to ensure a virtual destructor
  predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |> file.path("nList2Base_nC")),
  compileInfo=list(interface="full",
                   createFromR = FALSE,
                   # Hincludes = paste0("<", file.path("nCompiler", "predef", "nList2_", "nList2_.h"),  ">"),
                   #needed_units = list("nList2Base_nClass"),
                   exportName = "nList2Base_nClass_new",
                   packageNames = c(uncompiled="nList2Base_nClass", compiled="nList2Base_nClass_C")
                   )
)

#' @export
nList2_nClass <- function(type) {
  Rtype <- type
  sym <- argType2symbol(type)
  Ctype <- sym$genCppVar()$generate()

  classname <- "nList2"
  cpp_classname <- Rname2CppName(paste0("nList2_", as.character(type)))
  baseclass <- paste0("nList2_<", Ctype, ">")
  RtypeObj <- eval(substitute(nMakeType(TYPE), list(TYPE = Rtype)))
#  classTypeObj <- eval(substitute(nMakeType(TYPE), list(TYPE = classname)))
  CpublicMethods <- list(
    Rcontents = 'RcppList', # used only for uncompiled
    setLength = nFunction(
      name = "setLength",
      function(length) {
        length(Rcontents) <<- length; length},
      returnType = 'integerScalar',
      compileInfo=list(
        C_fun = function(length='integerScalar')
        {cppLiteral('return setLength_(length)')}
      )),
    getLength = nFunction(
      name = "getLength",
      function() {
        length(Rcontents)},
      returnType = 'integerScalar',
      compileInfo=list(
        C_fun = function()
        {cppLiteral('return getLength_()')}
      )),
    as_list = nFunction(
      name = "as_list",
      function() {
        Rcontents
      },
      returnType = 'RcppList',
      compileInfo=list(
        C_fun = function()
        {cppLiteral('return as_list_()')}
      )),
    # Note that returning from the cpp set methods
    # vs the R set methods is different, inconsistent, and intentional.
    # In C++: a = b = c, the return value of b = c is c.
    # In R: a = b = c, the return value of b = c is b (the object being assigned to).
    # In R, this is necessary for chained assignments, like
    # a$b$c <- v: `$<-`(a, "b", `$<-`(a$b, "c", v))
    # In C++, we don't want to return nList2 objects when we
    # don't need to (semantically) because there is some construction innvolved
    # in doing so, whereas returning the value is free.
    singleBracket_get_cpp = nFunction(
      name = "singleBracket_get_cpp",
      function(inds) {
        stop("singleBracket_get_cpp is for internal compiled use only.")},
      compileInfo=list(
        callFromR = FALSE,
        template = "template<typename EigenDerived, int AccessLevel>",
        C_fun = function(inds = 'nCpp("Eigen::TensorBase<EigenDerived, AccessLevel>", const=TRUE, ref=TRUE )'){
          returnType("myclass")
          res <- myclass$new()
          cppLiteral('res->contents() = singleBracket_get_cpp_(inds); return res;')
        }
      )),
    singleBracket_set_cpp = nFunction(
      name = "singleBracket_set_cpp",
      function(inds, values) {
        stop("singleBracket_set_cpp is for internal compiled use only.")},
      compileInfo=list(
        callFromR = FALSE,
        template = "template<typename EigenDerived, int AccessLevel>",
        C_fun = function(inds = 'nCpp("Eigen::TensorBase<EigenDerived, AccessLevel>", const=TRUE, ref=TRUE )',
                         values = 'myclass') {
          returnType("myclass")
          cppLiteral('singleBracket_set_nList_cpp_(inds, values);')
          cppLiteral('return values;')
        }
      )),
    # The next two might be superfluous; the handler could directly insert the internal call.
    doubleBracket_get_cpp = nFunction(
      name = "doubleBracket_get_cpp",
      function(inds) {
        stop("doubleBracket_get_cpp is for internal compiled use only.")},
      returnType = Rtype,
      compileInfo=list(
        callFromR = FALSE,
        C_fun = function(inds = integer()) {
          cppLiteral('return doubleBracket_get_cpp_(inds);')
        }
      )),
    doubleBracket_set_cpp = nFunction(
      name = "doubleBracket_set_cpp",
      function(inds, value) {
        stop("doubleBracket_set_cpp is for internal compiled use only.")},
      returnType = Rtype,
      compileInfo=list(
        callFromR = FALSE,
        C_fun = function(inds = integer(),
                         value = T(RtypeObj)) {
          cppLiteral('return doubleBracket_set_cpp_(inds, value);')
        }
      )),
    singleBracket_get = nFunction(
      name = "singleBracket_get",
      function(inds) {
        if(is.numeric(inds)) {
          r_inds <- range(inds)
          if(r_inds[1] < 1 || r_inds[2] > length(Rcontents))
            stop("Index out of bounds.")
        } else if (is.logical(inds)) {
          if(length(inds) > length(Rcontents))
            stop("Logical index is too long.")
        } else {
          stop("Invalid index type.")
        }
        res <- myclass$new()
        res$Rcontents <- self$Rcontents[inds]
        res},
      compileInfo=list(
        C_fun = function(inds = 'SEXP') {
          returnType("myclass")
          res <- myclass$new()
          cppLiteral('res->contents() = singleBracket_get_(inds); return res;')
        }
      )),
    doubleBracket_get = nFunction(
      name = "doubleBracket_get",
      function(i) {
        Rcontents[[i]]},
      returnType = Rtype,
      compileInfo=list(
        C_fun = function(i = 'SEXP')
        {cppLiteral('return doubleBracket_get_(i)')}
      )),
    # This setter does not need to return anything
    # Because the `[<-.nList` method will do that.
    singleBracket_set = nFunction(
      name = "singleBracket_set",
      function(inds, values) {
        Rcontents[inds] <<- values
        self
      },
      compileInfo = list(
        C_fun = function(inds = 'SEXP', values = 'RcppList') {
          cppLiteral('singleBracket_set_(inds, values);')
        }
      )),
    singleBracket_set_single = nFunction(
      name = "singleBracket_set_single",
      function(inds, value) {
        Rcontents[inds] <<- value
        self
      },
      compileInfo = list(
        C_fun = function(inds = 'SEXP', value = T(RtypeObj)) {
          cppLiteral('singleBracket_set_single_(inds, value);')
        }
      )),
    singleBracket_set_nList = nFunction(
      name = "singleBracket_set_nList",
      function(inds, values) {
        Rcontents[inds] <<- as.list(values)
        self
      },
      compileInfo = list(
        C_fun = function(inds = 'SEXP', values = 'myclass') {
          cppLiteral('singleBracket_set_(inds, values->contents());')
        }
      )),
    doubleBracket_set = nFunction(
      name = "doubleBracket_set",
      function(i, value) {
        Rcontents[[i]] <<- value
        self
      },
      returnType = Rtype,
      compileInfo = list(
        C_fun = function(i = 'SEXP', value = T(RtypeObj)) {
          cppLiteral('return doubleBracket_set_(i, value)')
        }
      ))
    )
    ## Superseded by doubleBracket_get / doubleBracket_set:
    # setOne = nFunction(
    #   name = "setOne",
    #   function(i, v) {
    #     Rcontents[[i]] <<- v; v},
    #   returnType = Rtype,
    #   compileInfo=list(
    #     C_fun = function(i = 'integerScalar', v=T(RtypeObj))
    #     {cppLiteral('return setOne_(i, v)')}
    #   )),
    # getOne = nFunction(
    #   name = "getOne",
    #   function(i) {
    #     Rcontents[[i]]},
    #   returnType = Rtype,
    #   compileInfo=list(
    #     C_fun = function(i = 'integerScalar')
    #     {cppLiteral('return getOne_(i)')}
    #   )),
    ## Superseded by singleBracket_get:
    # getMany = nFunction(
    #   name = "getMany",
    #   function(inds) {
    #     Rcontents[inds]
    #   },
    #   compileInfo = list(
    #     C_fun = function(inds = 'integerVector') {
    #       returnType("myclass")
    #       res <- myclass$new()
    #       cppLiteral('res->contents() = getMany_(inds); return res;')
    #     }
    #   )),
    # getManyToList = nFunction(
    #   name = "getManyToList",
    #   function(inds) {
    #     Rcontents[inds]
    #   },
    #   compileInfo = list(
    #     C_fun = function(inds = 'integerVector') {
    #       returnType("RcppList")
    #       cppLiteral('return getManyToList_(inds);')
    #     }
    #   )),
    # getManyLogical = nFunction(
    #   name = "getManyLogical",
    #   function(inds) {
    #     Rcontents[inds]
    #   },
    #   compileInfo = list(
    #     C_fun = function(bools = 'logicalVector') {
    #       returnType("myclass")
    #       res <- myclass$new()
    #       cppLiteral('res->contents() = getManyLogical_(bools); return res;')
    #     }
    #   )),
    # getManyToListLogical = nFunction(
    #   name = "getManyToListLogical",
    #   function(inds) {
    #     Rcontents[inds]
    #   },
    #   compileInfo = list(
    #     C_fun = function(bools = 'logicalVector') {
    #       returnType("RcppList")
    #       cppLiteral('return getManyToListLogical_(bools);')
    #     }
    #   )),
    ## Superseded by singleBracket_set / singleBracket_set_single / singleBracket_set_nList:
    # setMany = nFunction(
    #   name = "setMany",
    #   function(inds, vals) {
    #     Rcontents[inds] <<- vals; vals
    #   },
    #   compileInfo = list(
    #     C_fun = function(inds = 'integerVector', vals = 'myclass') {
    #       returnType("myclass")
    #       cppLiteral('setMany_(inds, vals->contents()); return vals;')
    #     }
    #   )),
    # setManySingle = nFunction(
    #   name = "setManySingle",
    #   function(inds, val) {
    #     Rcontents[inds] <<- val; val
    #   },
    #   compileInfo = list(
    #     C_fun = function(inds = 'integerVector', val = T(RtypeObj)) {
    #       returnType(T(RtypeObj))
    #       cppLiteral('return setManySingle_(inds, val);')
    #     }
    #   )),
    # setManyFromList = nFunction(
    #   name = "setManyFromList",
    #   function(inds, vals) {
    #     Rcontents[inds] <<- vals; vals
    #   },
    #   compileInfo = list(
    #     C_fun = function(inds = 'integerVector', vals = 'RcppList') {
    #       returnType("RcppList")
    #       cppLiteral('return setManyFromList_(inds, vals);')
    #     }
    #   )),
    # setManyLogical = nFunction(
    #   name = "setManyLogical",
    #   function(bools, val) {
    #     Rcontents[bools] <<- val; val
    #   },
    #   compileInfo = list(
    #     C_fun = function(bools = 'logicalVector', vals = 'myclass') {
    #       returnType("myclass")
    #       cppLiteral('setManyLogical_(bools, vals->contents()); return vals;')
    #     }
    #   )),
    # setManyFromListLogical = nFunction(
    #   name = "setManyFromListLogical",
    #   function(bools, vals) {
    #     Rcontents[bools] <<- vals; vals
    #   },
    #   compileInfo = list(
    #     C_fun = function(bools = 'logicalVector', vals = 'RcppList') {
    #       returnType("RcppList")
    #       cppLiteral('return setManyFromListLogical_(bools, vals);')
    #     }
    #   )),
    # setManyLogicalSingle = nFunction(
    #   name = "setManyLogicalSingle",
    #   function(bools, val) {
    #     Rcontents[bools] <<- val; val
    #   },
    #   compileInfo = list(
    #     C_fun = function(bools = 'logicalVector', val = T(RtypeObj)) {
    #       returnType(T(RtypeObj))
    #       cppLiteral('return setManyLogicalSingle_(bools, val);')
    #     }
    #   ))
  nList2_singleBracket_labelAbsTypes <- function(code, symTab, auxEnv, handlingInfo) {
    inserts <- nCompiler:::labelAbstractTypesEnv$recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    arg1 <- code$args[[1]]
    # Return type is the same nClass type as the list (contrast with [[ which returns the element type)
    code$type <- arg1$type$clone(deep=TRUE)
    if(is.null(inserts)) list() else inserts
  }
  nList2_doubleBracket_labelAbsTypes <- function(code, symTab, auxEnv, handlingInfo) {
    # used for both getting and setting so be careful if checking args
    inserts <- nCompiler:::labelAbstractTypesEnv$recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    arg1 <- code$args[[1]]
    NCgen <- arg1$type$NCgenerator
    elementSym <- NCinternals(NCgen)$symbolTable$getSymbol("x")
    code$type <- elementSym$clone(deep=TRUE)
    if(is.null(inserts)) list() else inserts
    # should return something non-null, either an inserts list or list() or TRUE. TRUE is more distinct so we use that.
  }
  nList2_length_LAT <- function(code, symTab, auxEnv, handlingInfo) {
    # used for both getting and setting so be careful if checking args
    inserts <- nCompiler:::labelAbstractTypesEnv$recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    code$type <- symbolBasic$new(nDim = 0, type = 'integer')
    if(is.null(inserts)) list() else inserts
  }
  ans <- substitute(
    nClass(
      classname = CLASSNAME,
      inherit = nList2Base_nClass,
      Cpublic = c(
        list(
          x = TYPE),
        CpublicMethods
      ),
      Rpublic = list(
        initialize = function(...) { # ... needed to allow init from compiled code to return an object of the class.
          super$initialize(...)
          self$Rcontents <- list()
        }
      ),
      compileInfo = list(
        cpp_classname = CPP_CLASSNAME,
#          needed_units = list("nList2Base_nClass"),
        nClass_inherit = list(base=BASECLASS),
        overloadDefs = list(
          "[" = list(
            labelAbstractTypes = list(handler = nList2_singleBracket_labelAbsTypes),
            eigenImpl = list(handler = 'PtrMethod', methodName = 'singleBracket_get_cpp')
          ),
          "[<-" = list(
            labelAbstractTypes = list(handler = nList2_singleBracket_labelAbsTypes),
            eigenImpl = list(handler = 'PtrMethod', methodName = 'singleBracket_set_cpp')
          ),
          "[[" = list(
            labelAbstractTypes = list(handler = nList2_doubleBracket_labelAbsTypes),
            eigenImpl = list(handler = 'PtrMethod', methodName = 'doubleBracket_get_cpp')
          ),
          "[[<-" = list(
            labelAbstractTypes = list(handler = nList2_doubleBracket_labelAbsTypes),
            eigenImpl = list(handler = 'PtrMethod', methodName = 'doubleBracket_set_cpp')
          ),
          length = list(
            labelAbstractTypes = list(handler = nList2_length_LAT),
            eigenImpl = list(handler = 'Method'),
            cppOutput = list(cppString = 'getLength')
          ),
          "length<-" = list(
            labelAbstractTypes = list(handler = nList2_length_LAT)
          )
        )
      )
    ),
    list(
      CLASSNAME = classname,
      TYPE = Rtype,
      BASECLASS = baseclass,
      CPP_CLASSNAME = cpp_classname
    )
  )
  myclass <- eval(ans)
  myclass
}

## Actually double bracket is only for one value and single bracket for multiple

#' @exportS3Method
#' @method as.list nList2
as.list.nList2 <- function(x) {
  x$as_list()
}

#' @exportS3Method
#' @method length nList2
length.nList2 <- function(x) {
  x$getLength()
}

#' @exportS3Method
#' @method `length<-` nList2
`length<-.nList2` <- function(x, value) {
  x$setLength(value)
  x
}

#' @exportS3Method
#' @method `[` nList2
`[.nList2` <- function(x, inds) {
  x$singleBracket_get(inds)
#  if(is.logical(inds)) return(x$getManyLogical(inds))
#  if(length(inds) == 1) return(x$getOne(inds))
#  else return(x$getMany(inds))
}

#' @exportS3Method
#' @method `[[` nList2
`[[.nList2` <- function(x, i) {
  x$doubleBracket_get(i)
#  if(is.logical(i)) return(x$getManyLogical(i))
#  if(length(i) == 1) return(x$getOne(i))
#  else return(x$getMany(i))
}

#' @exportS3Method
#' @method `[<-` nList2
`[<-.nList2` <- function(x, inds, value) {
  if(inherits(value, "nList2"))  x$singleBracket_set_nList(inds, value)
  else if(is.list(value))        x$singleBracket_set(inds, value)
  else                           x$singleBracket_set_single(inds, value)
  x
}

#' @exportS3Method
#' @method `[[<-` nList2
`[[<-.nList2` <- function(x, i, value) {
  x$doubleBracket_set(i, value)
  x
}

# Draft for a new version of nList.
#' @export
nList2 <- function(type, .ID = FALSE) {
  if(isTRUE(.ID))
    return(Rname2CppName(paste0("nList2_", as.character(type))))
  ans <- nList2_nClass(type)
  # classID is set to cpp_classname by NC_InternalsClass initialize; no override needed
  ans
}
class(nList2) <- c("function", "nClassBuilder")




# Sketch of the plan:
# Imitate the model setup
# Make a predefined nList2 base class.
# Make specialized nList2 classes inherit from a CRTP base manually included in the predefined
# Make the appropriate derived class methods call the CRTP methods.
# Fill in the functionality.
# Make the specialized classes provide opDefs for compilation of bracket and double-bracket operators.

# nListBase_nClass <- nClass(
#   classname = "nListBase_nClass",
#   Cpublic = list(
#     ping = nFunction(
#         name = "ping",
#         function() {return(TRUE); returnType(logical())},
#         compileInfo = list(virtual=TRUE)
#     ),
#     getOne = nFunction(
#         name = "getOne",
#         function(i = 'integerScalar') {
#           return(1); returnType(numericScalar())
#         },
#         compileInfo = list(virtual=TRUE)
#     ),
#     calculate = nFunction(
#         name = "calculate",
#         function(calcInstrList) {
#           cat("In uncompiled calculate\n")
#           # This is where uncompiled stepping through the calcInstrList happens.
#           for(calcInstr in calcInstrList$calcInstrList) {
#             nodeIdx <- calcInstr$nodeIndex
#             nodemember_name <- self$nodeObjNames[nodeIdx] # nodeObjNames is found in the derived class
#             for(nodeInstr in calcInstr$nodeInstrVec) {
#               self[[nodemember_name]]$calculate(nodeInstr)
#             }
#           }
#           return(0)
#         },
#         returnType = 'numericScalar',
#         compileInfo = list(
#           C_fun = function(calcInstrList='calcInstrList_nClass') {
#             cppLiteral('Rprintf("modelBase_nClass calculate (should not see this)\\n");'); return(0)},
#           virtual=TRUE
#         )
#     )
#   ),
#   # See comment above about needing to ensure a virtual destructor
#   predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |> file.path("modelBase_nC")),
#   compileInfo=list(interface="full",
#                    createFromR = FALSE,
#                    Hincludes = c('"nodeFxnBase_nClass_c_.h"', '"calcInstrList_nClass_c_.h"'), # do we need "<nodeFxnBase_nClass_c_.h>" too?
#                    needed_units = list("nodeFxnBase_nClass","calcInstrList_nClass"), #do we need nodeFxnBase_nClass here too?
#                    exportName = "modelBase_nClass_new",
#                    packageNames = c(uncompiled="modelBase_nClass", compiled="modelBase_nClass_C")
#                    )
# )

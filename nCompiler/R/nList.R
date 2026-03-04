# This is a rudimentary place-holder for uncompiled nList.
# It simply returns a list of the given length and ignores the type
# It means that test-nList is a bit klugey.

#' @export
nList <- function(type, length) vector("list", length)

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

nList2_nClass <- function(Rtype, Ctype) {
  classname <- "nList2"
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
    setOne = nFunction(
      name = "setOne",
      function(i, v) {
        Rcontents[[i]] <<- v; v},
      returnType = Rtype,
      compileInfo=list(
        C_fun = function(i = 'integerScalar', v=T(RtypeObj))
        {cppLiteral('return setOne_(i, v)')}
      )),
    getOne = nFunction(
      name = "getOne",
      function(i) {
        Rcontents[[i]]},
      returnType = Rtype,
      compileInfo=list(
        C_fun = function(i = 'integerScalar')
        {cppLiteral('return getOne_(i)')}
      )),
    getMany = nFunction(
      name = "getMany",
      function(inds) {
        Rcontents[inds]
      },
#      returnType = quote(T(classTypeObj)),
      compileInfo = list(
        C_fun = function(inds = 'integerVector') {
          returnType("myclass")
          res <- myclass$new()
          cppLiteral('res->contents() = getMany_(inds); return res;')
        }
      )),
    getManyToList = nFunction(
      name = "getManyToList",
      function(inds) {
        Rcontents[inds]
      },
      compileInfo = list(
        C_fun = function(inds = 'integerVector') {
          returnType("RcppList")
          cppLiteral('return getManyToList_(inds);')
        }
      )),
    getManyLogical = nFunction(
      name = "getManyLogical",
      function(inds) {
        Rcontents[inds]
      },
      compileInfo = list(
        C_fun = function(bools = 'logicalVector') {
          returnType("myclass")
          res <- myclass$new()
          cppLiteral('res->contents() = getManyLogical_(bools); return res;')
        }
      )),
    getManyToListLogical = nFunction(
      name = "getManyToListLogical",
      function(inds) {
        Rcontents[inds]
      },
      compileInfo = list(
        C_fun = function(bools = 'logicalVector') {
          returnType("RcppList")
          cppLiteral('return getManyToListLogical_(bools);')
        }
      )),
    setMany = nFunction(
      name = "setMany",
      function(inds, vals) {
        Rcontents[inds] <<- vals; vals
      },
      compileInfo = list(
        C_fun = function(inds = 'integerVector', vals = 'myclass') {
          returnType("myclass")
          cppLiteral('setMany_(inds, vals->contents()); return vals;')
        }
      )),
    setManySingle = nFunction(
      name = "setManySingle",
      function(inds, val) {
        Rcontents[inds] <<- val; val
      },
      compileInfo = list(
        C_fun = function(inds = 'integerVector', val = T(RtypeObj)) {
          returnType(T(RtypeObj))
          cppLiteral('return setManySingle_(inds, val);')
        }
      )),
    setManyFromList = nFunction(
      name = "setManyFromList",
      function(inds, vals) {
        Rcontents[inds] <<- vals; vals
      },
      compileInfo = list(
        C_fun = function(inds = 'integerVector', vals = 'RcppList') {
          returnType("RcppList")
          cppLiteral('return setManyFromList_(inds, vals);')
        }
      )),
    setManyLogical = nFunction(
      name = "setManyLogical",
      function(bools, val) {
        Rcontents[bools] <<- val; val
      },
      compileInfo = list(
        C_fun = function(bools = 'logicalVector', vals = 'myclass') {
          returnType("myclass")
          cppLiteral('setManyLogical_(bools, vals->contents()); return vals;')
        }
      )),
    setManyFromListLogical = nFunction(
      name = "setManyFromListLogical",
      function(bools, vals) {
        Rcontents[bools] <<- vals; vals
      },
      compileInfo = list(
        C_fun = function(bools = 'logicalVector', vals = 'RcppList') {
          returnType("RcppList")
          cppLiteral('return setManyFromListLogical_(bools, vals);')
        }
      )),
    setManyLogicalSingle = nFunction(
      name = "setManyLogicalSingle",
      function(bools, val) {
        Rcontents[bools] <<- val; val
      },
      compileInfo = list(
        C_fun = function(bools = 'logicalVector', val = T(RtypeObj)) {
          returnType(T(RtypeObj))
          cppLiteral('return setManyLogicalSingle_(bools, val);')
        }
      ))
    )
  ans <- substitute(
    nClass(
      classname = CLASSNAME,
        inherit = nList2Base_nClass,
        Cpublic = c(
          list(
            x = TYPE),
          CpublicMethods
        ),
        compileInfo = list(
#          needed_units = list("nList2Base_nClass"),
          nClass_inherit = list(base=BASECLASS)
        )
    ),
    list(
      CLASSNAME = classname,
      TYPE = Rtype,
      BASECLASS = baseclass
    )
  )
  myclass <- eval(ans)
  myclass
}

## Actually double bracket is only for one value and single bracket for multiple

`[[.nList2` <- function(x, i) {
  if(is.logical(i)) return(x$getManyLogical(i))
  if(length(i) == 1) return(x$getOne(i))
  else return(x$getMany(i))
}

`[[<-.nList2` <- function(x, i, value) {
  if(length(i) == 1) return(x$setOne(i, value))
  else return(x$setMany(i, value))
}



# Draft for a new version of nList.
nList2 <- function(type, .ID = FALSE) {
  classID <- paste0("nList2_", as.character(type))
  if(isTRUE(.ID))
    return(classID)
  ans <- nList2_nClass(type)
  NCinternals(ans)$classID <- classID
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

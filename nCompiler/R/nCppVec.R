# This is a rudimentary place-holder for uncompiled nCppVec.
# It simply returns a list of the given length and ignores the type
# It means that test-nCppVec is a bit klugey.

#' @export
nCppVec <- function(type, length) vector("list", length)


#' @keywords internal
#' @noRd
#' @rawNamespace export(nListBase_nClass)
NULL

# Normally set this flag to NULL
# Setting it non-null supports
# development work on nClass itself
# by allowing package building 
# even if nClass is broken.
NLdevel <- NULL

nList_singleBracket_labelAbsTypes <- function(code, symTab, auxEnv, handlingInfo) {
  inserts <- nCompiler:::labelAbstractTypesEnv$recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
  arg1 <- code$args[[1]]
  # Return type is the same nClass type as the list (contrast with [[ which returns the element type)
  code$type <- arg1$type$clone(deep=TRUE)
  if(is.null(inserts)) list() else inserts
}
nList_doubleBracket_labelAbsTypes <- function(code, symTab, auxEnv, handlingInfo) {
  # used for both getting and setting so be careful if checking args
  inserts <- nCompiler:::labelAbstractTypesEnv$recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
  arg1 <- code$args[[1]]
  NCgen <- arg1$type$NCgenerator
  elementSym <- NCinternals(NCgen)$symbolTable$getSymbol("x")
  elementSym <- nCompiler:::resolveOneTBDsymbol(elementSym, env = env)
  code$type <- elementSym$clone(deep=TRUE)
  if(is.null(inserts)) list() else inserts
  # should return something non-null, either an inserts list or list() or TRUE. TRUE is more distinct so we use that.
}
nList_length_labelAbsTypes <- function(code, symTab, auxEnv, handlingInfo) {
  # used for both getting and setting so be careful if checking args
  inserts <- nCompiler:::labelAbstractTypesEnv$recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
  code$type <- symbolBasic$new(nDim = 0, type = 'integer')
  if(is.null(inserts)) list() else inserts
}

nListBase_nClass <- NLdevel %||% nClass(
  classname = "nListBase_nClass",
  Cpublic = list(
    ping = nFunction(
        name = "ping",
        function() {return(TRUE); returnType(logical())},
        compileInfo = list(virtual=TRUE)
    ),
    setLength = nFunction(
      name = "setLength",
      function(length) {
        stop("Uncompiled base class setLength should not be called.")
      },
      returnType = "integerScalar()",
      compileInfo = list(
        C_fun = function(length='integerScalar') {
          cppLiteral('std::cout<<"Compiled base class setLength should not be called."<<std::endl;')
          return(0)
        },
      virtual=TRUE)
    ),
    getLength = nFunction(
      name = "getLength",
      function() {
        stop("Uncompiled base class getLength should not be called.")
      },
      returnType = "integerScalar()",
      compileInfo = list(
        C_fun = function() {
          cppLiteral('std::cout<<"Compiled base class getLength should not be called."<<std::endl;')
          return(0)
        },
      virtual=TRUE)
    ),
    get_interface_ptr_at = nFunction(
      name = "get_interface_ptr_at",
      function(i) {
        stop("Uncompiled base class get_interface_ptr_at should not be called.")
      },
      returnType = 'nCpp("std::shared_ptr<genericInterfaceBaseC>")',
      compileInfo = list(
        C_fun = function(i='integerScalar') {
          cppLiteral('Rcpp::stop("nListBase_nClass::get_interface_ptr_at should be called.")')
          cppLiteral("return(nullptr)")
        },
      virtual=TRUE)
    ),
    access_at = nFunction(
      name = "access_at",
      function(i) {
        stop("Uncompiled base class access_at should not be called.")
      },
      returnType = 'nCpp("std::unique_ptr<ETaccessorBase>")',
      compileInfo = list(
        C_fun = function(i='integerScalar') {
          cppLiteral('Rcpp::stop("nListBase_nClass::access_at should be called.")')
          cppLiteral("return(nullptr)")
        },
      virtual=TRUE)
    )
  ),
  # See comment above about needing to ensure a virtual destructor
  predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |> file.path("nListBase_nC")),
  compileInfo=list(interface="full",
                   createFromR = FALSE,
                   # Hincludes = paste0("<", file.path("nCompiler", "predef", "nList_", "nList_.h"),  ">"),
                   #needed_units = list("nListBase_nClass"),
                   cpp_classname = "nListBase_nClass",
                   exportName = "nListBase_nClass_new",
                   packageNames = c(uncompiled="nListBase_nClass", compiled="nListBase_nClass_C"),
                   interfaceExclude = c("get_interface_ptr_at", "access_at"),
                   overloadDefs = list(
                    length = list(
                      labelAbstractTypes = list(handler = nList_length_labelAbsTypes),
                      eigenImpl = list(handler = 'PtrMethod', methodName = "getLength")
                    ),
                    "length<-" = list(
                      labelAbstractTypes = list(handler = nList_length_labelAbsTypes ),
                      eigenImpl = list(handler = 'LengthAssign', ptr=TRUE, methodName = "setLength")
                    )
                   )
  )
)
# Manually add
# #include <nCompiler/predef/nList_/nList_.h>
# to the nListBase_nClass header file after the class declaration.
rm(NLdevel)

#' @export
nList_nClass <- function(type, env = parent.frame()) {
  ttype <- nCaptureType(type)
  Rtype <- type

  classname <- "nList"
  inner_cpp_typename <- type2cpp_typename({{ttype}}, where = env)
  cpp_classname <- Rname2CppName(paste0("nList_", type2uniqueID({{ttype}}, where = env)))
  # We need the C++ type for the nClass_inherit$base class,
  # but we can defer determining that until code generation
  # by providing a function. The reason to do this is so that
  # the type need not actually be defined and function at the
  # time one creates this nList generator.
  baseclassfun <- function() {
#    sym <- nCompiler:::type2symbol(type)
#    sym <- nCompiler:::resolveOneTBDsymbol(sym, env = env)
#    Ctype <- sym$genCppVar()$generate()
    baseclass <- paste0("nList_<", inner_cpp_typename, ">")
    baseclass
  }
  RtypeObj <- nType({{ttype}})
  # RtypeObj <- eval(substitute(nMakeType(TYPE), list(TYPE = Rtype)))
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
    # In C++, we don't want to return nList objects when we
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
      returnType = {{RtypeObj}},
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
      returnType = {{RtypeObj}},
      compileInfo=list(
        callFromR = FALSE,
        C_fun = function(inds = integer(),
                         value = {{RtypeObj}}) {
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
      returnType = {{RtypeObj}},
      compileInfo=list(
        C_fun = function(i = 'SEXP')
        {cppLiteral('return doubleBracket_get_(i)')}
      )),
    # This setter does not need to return anything
    # Because the `[<-.nCppVec` method will do that.
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
        C_fun = function(inds = 'SEXP', value = {{RtypeObj}}) {
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
      returnType = {{RtypeObj}},
      compileInfo = list(
        C_fun = function(i = 'SEXP', value = {{RtypeObj}}) {
          cppLiteral('return doubleBracket_set_(i, value)')
        }
      )),
    set_all_values = nFunction(
      name = "set_all_values",
      # R-level (uncompiled): handle plain list, uncompiled nList, or compiled
      # nList (all via duck-typing on as_list).
      function(Robj = 'SEXP') {
        if(is.list(Robj)) {
          Rcontents <<- Robj
        } else if(is.environment(Robj) &&
                  exists("as_list", envir = Robj, inherits = FALSE)) {
          Rcontents <<- Robj$as_list()
        } else {
          stop("Uncompiled nList set_all_values requires a list or an nList object.")
        }
      },
      compileInfo = list(
        # Delegates to set_all_values_() in nList_<Element>, which handles all
        # four input types without needing explicit namespace qualification.
        C_fun = function(Robj = 'SEXP') {
          cppLiteral('set_all_values_(Robj);')
        }
      ))
    )
  ans <- substitute(
    nClass(
      classname = CLASSNAME,
      inherit = nCompiler::nListBase_nClass,
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
        exportName = paste0(CPP_CLASSNAME, "_new"), # necessary to avoid "nList_new" as default later
        packageNames = c(uncompiled = paste0(CPP_CLASSNAME, "_R"), compiled = CPP_CLASSNAME), # These are not tested!
#          needed_units = list("nListBase_nClass"),
        nClass_inherit = list(base=BASECLASSFUN),
        overloadDefs = list(
          "[" = list(
            labelAbstractTypes = list(handler = nList_singleBracket_labelAbsTypes),
            eigenImpl = list(handler = 'PtrMethod', methodName = 'singleBracket_get_cpp')
          ),
          "[<-" = list(
            labelAbstractTypes = list(handler = nList_singleBracket_labelAbsTypes),
            eigenImpl = list(handler = 'PtrMethod', methodName = 'singleBracket_set_cpp')
          ),
          "[[" = list(
            labelAbstractTypes = list(handler = nList_doubleBracket_labelAbsTypes),
            eigenImpl = list(handler = 'PtrMethod', methodName = 'doubleBracket_get_cpp')
          ),
          "[[<-" = list(
            labelAbstractTypes = list(handler = nList_doubleBracket_labelAbsTypes),
            eigenImpl = list(handler = 'PtrMethod', methodName = 'doubleBracket_set_cpp')
          )
          # ,
          # length = list(
          #   labelAbstractTypes = list(handler = nList_length_labelAbsTypes),
          #   eigenImpl = list(handler = 'PtrMethod', methodName = "getLength")
          # ),
          # "length<-" = list(
          #   labelAbstractTypes = list(handler = nList_length_labelAbsTypes ),
          #   eigenImpl = list(handler = 'LengthAssign', ptr=TRUE, methodName = "setLength")
          # )
        )
      )
    ),
    list(
      CLASSNAME = classname,
      TYPE = Rtype,
      BASECLASSFUN = baseclassfun,
      CPP_CLASSNAME = cpp_classname
    )
  )
  myclass <- eval(ans)
  myclass
}

## Actually double bracket is only for one value and single bracket for multiple

#' @exportS3Method
#' @method as.list nList
as.list.nList <- function(x) {
  x$as_list()
}

#' @exportS3Method
#' @method length nList
length.nList <- function(x) {
  x$getLength()
}

#' @exportS3Method
#' @method `length<-` nList
`length<-.nList` <- function(x, value) {
  x$setLength(value)
  x
}

#' @exportS3Method
#' @method `[` nList
`[.nList` <- function(x, inds) {
  x$singleBracket_get(inds)
#  if(is.logical(inds)) return(x$getManyLogical(inds))
#  if(length(inds) == 1) return(x$getOne(inds))
#  else return(x$getMany(inds))
}

#' @exportS3Method
#' @method `[[` nList
`[[.nList` <- function(x, i) {
  x$doubleBracket_get(i)
#  if(is.logical(i)) return(x$getManyLogical(i))
#  if(length(i) == 1) return(x$getOne(i))
#  else return(x$getMany(i))
}

#' @exportS3Method
#' @method `[<-` nList
`[<-.nList` <- function(x, inds, value) {
  if(inherits(value, "nList"))  x$singleBracket_set_nList(inds, value)
  else if(is.list(value))        x$singleBracket_set(inds, value)
  else                           x$singleBracket_set_single(inds, value)
  x
}

#' @exportS3Method
#' @method `[[<-` nList
`[[<-.nList` <- function(x, i, value) {
  x$doubleBracket_set(i, value)
  x
}

#' @export
nList <- function(type, .ID = FALSE, env = parent.frame()) {
  ttype <- nCaptureType(type)
  if(isTRUE(.ID)) {
#    uID <- type2uniqueID({{type}})
    return(Rname2CppName(paste0("nList_", type2uniqueID({{ttype}}, where = env))))
  }
  ans <- nList_nClass({{ttype}}, env = env)
  # classID is set to cpp_classname by NC_InternalsClass initialize; no override needed
  ans
}
class(nList) <- c("function", "nClassBuilder")

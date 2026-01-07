nClassLabelMaker <- labelFunctionCreator('nClass')

nClassClass <- R6::R6Class(
  classname = "nClass",
  portable = FALSE,
  public = list(
    initialize = function(...) {
      initialize_Cpublic(...)
    },
    initialize_Cpublic = function(...) {
      private$Cpublic_obj <- private$initialize_Cpublic_obj(...)
    }
  )
)

CpubClass <- R6::R6Class(
  classname = "CpubClass",
  portable = FALSE
)


#' Create a nClass definition
#'
#' A nClass is like an R6 class, but it allows some data and
#' methods (written as nFunctions) to be automatically compiled
#' via C++, while others are in pure R.  nClasses are implemented
#' using R6 classes.
#'
#' @param classname name for the class.
#' @param Rpublic list of public data and methods (functions) for use from R
#'   only.
#' @param Cpublic list of public data (with type declarations) and methods
#'   (nFunctions) that can be turned into C++ via \link{nCompile_nClass}. As in
#'   R6 classes (see \link{R6Class}), data and methods go in the same list.
#' @param enableDerivs list or character vector of methods in Cpublic for which
#'   derivatives should be enabled.
#' @param enableSaving logical indicating whether C++ should include support for
#'   saving objects. Defaults to the value of nOptions("enableSerialize"), which
#'   defaults to \code{TRUE}.
#' @param env An environment that should be used as the enclosing environment
#'   for objects created from the nClass
#' @return An R6 class generator enhanced with information needed for
#'   compilation.
#' @details Use of \code{enableDerivs} will substantially slow down C++
#'   compilation. See \code{help(nDerivs)} about how to obtain derivatives from
#'   enabled methods.
#'
#' Setting \code{enableSaving=TRUE} will somewhat slow down C++ compilation. See
#' \code{help(nSave)} and \code{help(nSerialize)} about how to save objects.
#' (The technical jargon for converting data structures such as class objects
#' into data suitable for saving and later loading is "serialization".)
#'
#' The internal information used for compilation can be accessed with
#'   \code{NCinternals(nClassGenerator)} and modified with
#'   \code{NCinternals(nClassGenerator)<-}, where \code{nClassGenerator} was
#'   returned from a call to \code{nClass}.
#' @examples
#' \donttest{
#'     nc1 <- nClass(
#'     Rpublic = list(
#'         Rv = NULL,
#'         Rfoo = function(x) x+1
#'     ),
#'     Cpublic = list(
#'         Cv = 'numericScalar',
#'         Cfoo = nFunction(
#'             fun = function(x) {
#'                 return(x+1)
#'             },
#'             argTypes = list(x = 'numericScalar'),
#'             returnType = 'numericScalar')
#'         )
#'   )
#' }
#' @export
nClass <- function(classname,
                   Rpublic = list(),
                   Cpublic = list(),
                   enableDerivs = character(),
                   enableSaving = get_nOption("enableSaving"),
                   inherit = NULL,
                  # control = list(),
                   compileInfo = list(),
                   predefined = FALSE,
                   env = parent.frame()) {
  # supported elements of compileInfo:
  # exportName: name of the R function to call the
  #    C/C++ function for a new object. Defaults to paste0("new_", classname)
  # interface ("full"): "full", "generic" or "none". First two: build interface. Fine-grained control possible in future.
  # interfaceMembers(NULL): character vector of which members (variables and methods) to include. Default to all
  # depends (list()): Values for Cpp::depends.
  # inherit: list of C++ class inheritances (all public inheritance).
  #   If one is named "base", it will replace the inheritance from
  #   an inherit base nClass. Multiple inheritance among nClasses
  #   is not supported (same for R6 classes), so there is no ambiguity.
  #   If the nClass argument inherit is provided but compileInfo$inherit$base
  #   is not provided, inheritance will be determined from the base nClass.
  #   All inheritance provided by compileInfo$inherit should include any
  #   accessor specifier, typically "public", e.g. "public some_class".
  #   Similarly, template arguments (include CRTP) should be in the text explicitly.
  # needed_units: list of needed nClasses and nFunctions to include, by name or object
  #
  # constructor(s) and destructor:
  #
  # constructors should be nFunctions with compileInfo = list(constructor=TRUE)
  # constructors can have any name, which will become the class name in C++.
  # If a constructor has the same name as the class, it will replace the default
  # constructor (which will otherwise be provided). If said constructor has a
  # non-empty argument list, then the C++ class will not have a (C++) default constructor.
  #
  # A field compileInfo$initializerList can be provided for constructors. It should be a
  # list of entries that can be parsed by nParse, i.e. either character string code
  # or code inside quote(). This code will not go through much compilation processing
  # and will instead be directly output. For code as simple as "x(x_)" (initialization of
  # a member variable from a constructor argument), it should work fine. Any more complicated
  # expression can be done using nCpp("some->code()").
  #
  # A destructor should be an nFunction with compileInfo = list(destructor=TRUE).
  # Its name in C++ will be replaced with "~<class name>".
  #
  # If an R6 initialize or finalize method is desired, those should be in Rpublic.
  # Currently it is not supported to have an initialize or finalize that is a directly
  # compiled method. It is also not supported to create the C++ object for an R6 interface
  # object by anything but a default constructor.
  compileInfo <- updateDefaults(
    list(exportName = NULL, interface = "full",
         interfaceMembers = NULL,
         depends = list(),
         inherit = list(),
         nClass_inherit = list()),
    compileInfo
  )
  if(missing(classname))
    classname <- nClassLabelMaker()
  if(is.null(compileInfo$classname))
    compileInfo$classname <- paste0(classname, "_compiled")
  if('finalize' %in% names(Cpublic)) {
    if('finalize' %in% names(Rpublic))
      stop("If a finalize method is provided in Rpublic, it can't be provided in Cpublic.",
           "If you want a C++ destructor that is not an R finalizer, name it 'destructor'.")
    if(!isTRUE(NFinternals(Cpublic[['finalize']])$compileInfo$destructor))
      stop("In nFunction 'finalize', use 'compileInfo = list(destructor=TRUE)'.")
  } else if('destructor' %in% names(Cpublic)) {
    if(!isTRUE(NFinternals(Cpublic[['destructor']])$compileInfo$destructor))
      stop("In nFunction 'destructor', use 'compileInfo = list(destructor=TRUE)'.")
  }
  if('initialize' %in% names(Cpublic)) {
    if('initialize' %in% names(Rpublic))
      stop("If an initialize method is provided in Rpublic, it can't be provided in Cpublic.",
           "If you want a C++ constructor that is not an R constructor, give it a name and set",
           "compileInfo$constructor=TRUE.")
    if(!isTRUE(NFinternals(Cpublic[['initialize']])$compileInfo$constructor))
      stop("In nFunction 'initialize', use 'compileInfo = list(constructor=TRUE)'.")
  }

  inheritQ <- substitute(inherit)
  inherit_provided <- !is.null(inheritQ)

  internals = NC_InternalsClass$new(classname = classname,
                                    Cpublic = Cpublic,
                                    RpublicNames = names(Rpublic),
                                    enableDerivs = enableDerivs,
                                    enableSaving = enableSaving,
                                    inheritQ = inheritQ,
                                 #   control = control,
                                    compileInfo = compileInfo,
                                    predefined = predefined,
                                    env = env)
  ## We put the internals in 2 places:
  ## 1. in an environment layer around every instance

  # The R6Class inherit argument has weird handling:
  # "captured as an unevaluated expression which is evaluated in parent_env each time an object is instantiated."
  # so if provided in the nClass call, we stick it in new_env.
  # (That is not the only reason for new_env.)
  # Also note that the inherit arg is for nClass inheritance. The compileInfo$inherit element is for hard-coded C++ inheritance statements.
  #if(!is.null(inherit)) new_env$.inherit_obj <- inherit

  # Uncompiled behavior for Cpublic fields needs to be handled.
  # Right now a type string like 'numericScalar' just becomes a
  # default value.
  if("isCompiled" %in% names(Cpublic)) {
    stop("The name 'isCompiled' in Cpublic is reserved for nCompiler internal use.",
         call. = FALSE)
  }
  # Cpublic$initialize may be provided and should check isCompiled() for behavior
  # because it will be inherited directly in the compiled Cpublic class.
  #
  # It should not normally be necessary.
  #
  Cpub_class_code <- make_uncompiled_Cpub_class_code(
    classname = classname,
    inheritQ = inheritQ,
    Cpublic = NULL # indicates to leave quote(Cpublic)
  )

  main_class_code <- make_nClass_code(
    internals = internals,
    Cpublic = Cpublic, # Allows to make methods that call the Cpub_object
    Rpublic = NULL # indicates to leave quote(Rpublic)
  )

  ## Build the R6 class generator
  NCgenerator <- eval(main_class_code)
  Cpub_generator <- eval(Cpub_class_code)
  connect_nClass_envs(NCgenerator, Cpub_generator, env)

  NCgenerator$parent_env$.NCinternals <- internals
  ## Store the internals in two places:
  ## 2. in the generator
  NCgenerator$.nCompiler <- internals
  ## NB: We want to avoid having every object
  ## include the generator, to keep saving light.
  NCgenerator
}

make_uncompiled_Cpub_class_code <- function(classname, 
                                            inheritQ = NULL, 
                                            Cpublic = NULL) {
  inherit_provided <- !is.null(inheritQ)
  Cpublic_code <- quote(Cpublic)
  if(!is.null(Cpublic)) {
    parsedcopy <- \(f) {ans <- substitute(\() BODY, list(BODY=body(f))) |> removeSource(); if(!is.null(formals(f))) ans[[2]] <- formals(f); ans}
    Cpublic_code_list <- Cpublic |> lapply(\(x) if(is.function(x)) parsedcopy(x) else x)
    Cpublic_code <- do.call("call", c("list",
                              Cpublic_code_list))
  }
  substitute(
    R6::R6Class(
      classname = CLASSNAME,
      public = c(CPUBLIC,
                  list(isCompiled=function() FALSE)),
      portable = FALSE,
      inherit = INHERIT,
      parent_env = NULL
    ),
    list(CLASSNAME = paste0(classname, "_Cpub_uncompiled"),
          CPUBLIC = Cpublic_code,
          INHERIT =
            if(inherit_provided) substitute((INHERITQ)$parent_env$.Cpub_class, list(INHERITQ = inheritQ))
            else quote(nCompiler::CpubClass))
  )
}

make_nClass_code <- function(internals,
                    Cpublic = NULL, # if nFunctions (called from nClass), create method. If R function (called from WP_writeRinterfaces), use that ()
                    Rpublic = NULL  # If NULL (called from nClass), use quote(Rpbulic. If provided (from WP_writeRinterfaces), deparse.)
                    ) {
  classname <- internals$classname
  inheritQ <- internals$inheritQ
  fieldNames <- internals$fieldNames
  CmethodNames <- internals$methodNames
                            
  inherit_provided <- !is.null(inheritQ)

  activeBindings_code <- fieldNames |> lapply(
    function(name) {
      substitute(
        function(value) {
        if(missing(value))
          private$Cpublic_obj$NAME
        else
          private$Cpublic_obj$NAME <- value
        },
        list(NAME = name)
      ) |> removeSource()} # otherwise future srcref persists as fourth list element -- confusing!
    ) |> structure(names = fieldNames)
  activeBindings_list_code <- do.call("call", c("list", activeBindings_code))

  if(length(CmethodNames)) {
    Cmethods_code_list <- mapply(build_Cmethod_code_for_nClass,
                                fun = Cpublic[CmethodNames],
                                name = CmethodNames)
  } else {
    Cmethods_code_list <- list()
  }
  Cmethods_code <- do.call("call", c("list",
                                Cmethods_code_list))

  builtIn_code_list <- list(isCompiled = quote(function() FALSE)) # Will be overridden in compiled version to return TRUE

  Rpublic_code <- quote(Rpublic)
  if(!is.null(Rpublic)) {
    parsedcopy <- \(f) {ans <- substitute(\() BODY, list(BODY=body(f))) |> removeSource(); if(!is.null(formals(f))) ans[[2]] <- formals(f)}
    Rpublic_code_list <- Rpublic |> lapply(\(x) if(is.function(x)) parsedcopy(x) else x)
    Rpublic_code <- do.call("call", c("list",
                              Rpublic_code_list))
  }

  substitute(
    R6::R6Class(
      classname = CLASSNAME,
      public = c(RPUBLIC,
                CMETHODS,
                list(isCompiled = function() FALSE)),
      private = list(
        Cpublic_obj = NULL,
        init_Cpublic_obj_code = quote(.Cpub_class$new()),
        initialize_Cpublic_obj = function(...) {
          private$Cpublic_obj <- eval(private$init_Cpublic_obj_code)
        }
      ),
      active = ACTIVE,
      portable = FALSE,
      inherit = INHERIT,
      parent_env = new.env() # We do this so that the parsed code for writePackage is clean, and we modify the result below.
    ),
    list(CLASSNAME = classname,
         RPUBLIC = Rpublic_code,
         CMETHODS = Cmethods_code,
         ACTIVE = activeBindings_list_code,
         INHERIT =
           if(inherit_provided) inheritQ
         else quote(nCompiler::nClassClass))
  )
}

connect_nClass_envs <- function(NCgen, Cpub_gen, env, .NCgenerator=NULL) {
  # The NCgen at this point has been created by R6::R6Class
  # with "parent_env = new.env()".
  # The Cpub_gen has been created by R6::R6Class
  # with "parent_env = NULL"
  # env is environment to be treated as the parent env
  # of the call to nClass.
  #
  # The reason to do the below steps here in a separate
  # function is to be able to call it from either
  # nCompile pathway: package = FALSE or package = TRUE
  # as well as from uncompiled nClass. This allows
  # the environment arrangements to be done in one place
  # for all pathways for consistency. In the package=TRUE
  # case, it also allows this step to be done in .onLoad
  # so that the objects saved with the class (e.g. R6 class
  # generators) are a bit simpler than they would otherwise be.
  Cpub_gen$parent_env <- (new_env <- NCgen$parent_env)
  parent.env(new_env) <- env
  new_env$.Cpub_class <- Cpub_gen
  if(!is.null(.NCgenerator))
    new_env$.NCgenerator <- .NCgenerator # It would be (for overall network of environments) not to need this reference
}

# Provenance of names for an nClass:
#
# classname is provided by the user or generated automatically.
#   This is used as the R6 classname.
#
# NC_InternalsClass does not keep track of the name.
#
# In nCompile, the classname is used as the cpp_name.
# See nCompile comments for more.

build_Cmethod_code_for_nClass <- function(fun, name) {
  if(is.null(fun)) return(NULL) ## convenient for how this is used from mapply
  
  if(!isNF(fun)) {
    if(is.function(fun)) { # This was called from writePackge with a method from a previously built nClass
      return(parse(text = deparse(fun), keep.source = FALSE)[[1]])
    } else {
      stop("In nClass, Cpublic method ", name, " is not a function or nFunction.")
    }
  }
  if(!NFinternals(fun)$compileInfo$callFromR) {
    ans <- substitute(
      function(...) {
        stop("method ", NAME, " cannot be called directly from R (because compileInfo$callFromR is FALSE).")
      },
      list(NAME = name)
    ) |> removeSource()
    return(ans)
  }

  refArgs <- NFinternals(fun)$refArgs
  blockRefArgs <- NFinternals(fun)$blockRefArgs

  if(length(refArgs) + length(blockRefArgs) == 0) {
    ans <- substitute(function(...) {
      private$Cpublic_obj$NAME(...)
    },
      list(NAME = name)
    ) |> removeSource()
    return(ans)
  }

  ## Create Cpublic_obj$method(A = A, B = B) call
  ## We need the arguments in place instead of using ...
  ## so that we can use passByReference if needed.
  formals_fun <- formals(fun)
  innerCallDollarPart <- substitute(private$Cpublic_obj$NAME,
                                   list(NAME = name))
  innerArgsList <- names(formals_fun) |> lapply(as.name) |> structure(names = names(formals_fun))
  innerCallTemplate <- as.call(c(list(as.name("CALL__")),
                               innerArgsList))
  innerCall <- do.call("substitute",
                       list(expr = innerCallTemplate,
                            env = list(CALL__ = innerCallDollarPart)))

  ans <- substitute(
    function() {
      INNERCALL
  },
    list(INNERCALL = innerCall)
  ) |> removeSource()
  if(!is.null(formals_fun)) ans[[2]] <- formals_fun
  if(!is.null(ans[[3]]))
    ans[[3]] <- passByReference(ans[[3]], refArgs, blockRefArgs)
  ans
}

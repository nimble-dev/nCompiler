#' Determine if an object is a nClass object
#'
#' Determine if an object is a nClass object, returned by a nClass generator.
#'
#' @param x Object to be inspected.
#'
#' @return \code{TRUE} or \code{FALSE}
#' 
#' @export
isNC <- function(x) inherits(x, 'nClass')

#' @export
isCNC <- function(x) inherits(x, 'nClass') && isTRUE(x$isCompiled())


#' Determine if an object is a nClass generator
#'
#' Determine if an object is a nClass generator, returned by a call to \link{nClass}.
#'
#' @param x Object to be inspected
#'
#' @return \code{TRUE} or \code{FALSE}
#' 
#' @export
isNCgenerator <- function(x) {
  if(inherits(x, "R6ClassGenerator"))
    exists(".nCompiler", x)
  else
    FALSE
}

#' Determine if an object is a compiled nClass generator
#'
#' Determine if an object is a compiled nClass generator, returned by a call to \link{nCompile_nClass} with \code{interface} set to \code{"full"} or \code{"both"}.
#'
#' @param x Object to be inspected
#'
#' @return \code{TRUE} or \code{FALSE}
#' 
#' @export
isCompiledNCgenerator <- function(x) {
  if(inherits(x, "R6ClassGenerator"))
    exists(".newCobjFun", x$parent_env)
  else
    FALSE
}

#' Access internal information of a nClass object
#'
#' Only for advanced use.
#'
#' @param x A nClass object.
#' 
#' @export
NCinternals <- function(x) {
  if(isNC(x))
    stop("NCinternals for an nClass object is not supported.") # deprecated?: parent.env(x)$.NCinternals
  else if(isNCgenerator(x))
    x$.nCompiler
  else
    stop(paste('Invalid input to NCinternals.  Argument x is of invalid class',
               paste(class(x), collapse = ',')),
         call. = FALSE)
}

#' Set internal information of a nClass object
#'
#' Only for advanced use.
#'
#' @param x A nClass object.
#' 
#' @export
`NCinternals<-` <- function(x, value) {
  if(isNC(x))
    stop("NCinternals<- for an nClass object is not supported.") # deprecated?: parent.env(x)$.NCinternals <- value
  else if(isNCgenerator(x))
    x$.nCompiler <- value
  else
    stop(paste('Invalid input to NCinternals<-.  Argument x is of invalid class',
               paste(class(x), collapse = ',')),
         call. = FALSE)
  x
}

# Utility function to allow searching up an inheritance
# ladder to find a method.
NC_find_method <- function(NCgenerator, name, inherits=TRUE) {
  if(!isNCgenerator(NCgenerator))
    stop("Input must be a nClass generator.")
  current_NCgen <- NCgenerator
  done <- FALSE
  method <- NULL
  while(!done) {
    if(name %in% NCinternals(current_NCgen)$methodNames) {
      method <- NC_get_Cpub_class(current_NCgen)$public_methods[[name]]
      done <- TRUE
    } else {
      if(inherits)  {
        current_NCgen <- current_NCgen$get_inherit() #parent_env$.inherit_obj # same as current_NCgen$get_inherit() if there is inheritance, but get_inherit returns the base class at the top
        done <- !isNCgenerator(current_NCgen)
      } else
        done <- TRUE
    }
  }
  method
}

# This function will be called from nCompile after going through the 
# NCinternals for all units and calling connect_inherit and then process_inherit
# (with all connect_inherits called before all process_inherits)
# At that point we are ready to check for disallowed method overloading
# (we don't allow the same method name in different levels of the hierarchy unless it is virtual
# and all signatures match, i.e. we don't allow C-style overloading because it wouldn't work in 
# uncompiled (R) execution. This can be changed by an option, indicating one wants only the
# compiled behavior and doesn't care about uncompiled inconsistency.)
# and disallowed duplicate member variable names (for a similar reason: In C++
# different levels of a hierarchy could each have their own "x", but that is not
# the case in an R6 class hierarchy, so we disallow it unless a user allows it by option).
#
# The previous calls will have initialized NCint$check_inherit_done to FALSE
NC_check_inheritance <- function(NCgenerator) {
  allow_method_overloading <- isTRUE(get_nOption('allow_method_overloading'))
  allow_inherited_field_duplicates <- isTRUE(get_nOption('allow_inherited_field_duplicates'))
  if(allow_method_overloading && allow_inherited_field_duplicates) return(invisible(NULL))

  if(!isNCgenerator(NCgenerator))
    stop("Input to NC_check_inheritance must be a nClass generator.")
  NCint <- NCinternals(NCgenerator)

  if(is.null(NCint$inheritQ)) {
    NCint$check_inherit_done <- TRUE
    NCint$virtualMethodNames <- NCint$virtualMethodNames_self
    return(NCint$virtualMethodNames_self)
  }
  if(NCint$check_inherit_done) return(NCint$virtualMethodNames)
  # At this point, we have inheritance and have checked this NCgenerator yet.
  inheritNCinternals <- NCint$inheritNCinternals
  inheritNCgenerator <- eval(NCint$inheritQ, envir = NCint$env)
  # Recurse up the inheritance ladder
  # A design dilemma here was that the virtual marker is in the NFinternals,
  # which can be accessed from the NCgenerator but not the NCinternals.
  # That is why this function is not a method of NCinternals.
  inherit_virtualMethodNames <- NC_check_inheritance(inheritNCgenerator)
  new_virtualMethodNames <- character()

  if(!allow_method_overloading) {
    local_virtualMethodNames <- NCint$virtualMethodNames_self
    # default: check for disallowed method overloading
    allMethodNames <- NCint$allMethodNames
    for(mN in allMethodNames) {
      # if a method is not in the self method names, it was inherited, so there is nothing to check
      if(!(mN %in% NCint$allMethodNames_self)) next
      if(!(mN %in% inheritNCinternals$allMethodNames)) {
        # current level is the first one with this method name, so here we tag its virtual status
        new_virtualMethodNames <- c(new_virtualMethodNames, mN)
        next
      }
      # At this point the current level has the method and it is inherited
      localMethod <- NC_get_Cpub_class(NCgenerator)$public_methods[[mN]]
      inheritMethod <- NC_find_method(inheritNCgenerator, mN)
      if(is.null(inheritMethod))
        stop("Problem finding inherited method ", mN, " in NC_check_inheritance.", call. = FALSE)
      if(!NF_types_match(localMethod, inheritMethod))
        stop(paste0("Method ", mN, " does not have the same arguments names,", 
                    " and/or argument types, and/or returnType as a base class method of the same name.",
                    " Methods of the same name in an nClass hierarchy must have all of these the same",
                    " and the top-level one must be marked with compileInfo(virtual=TRUE).",
                    " (If you want to allow method overloading in C++ by turning off these requirements,",
                    " set nOptions(allow_method_overloading=TRUE)"),
             call. = FALSE)
      if(!(mN %in% inherit_virtualMethodNames))
        stop(paste0("Method ", mN, " is inherited, so", 
                " it must be marked with compileInfo(virtual=TRUE) in the top-level nClass that includes it.",
                " That does not appear to be the case.",
                " (If you want to allow method over-loading in C++ by turning off this requirement,",
                " set nOptions(allow_method_overloading=TRUE)"),
          call. = FALSE)
    }
  }
  if(!allow_inherited_field_duplicates) {
    # This would be slightly more efficient to do in NC_InternalsClass::process_inherit
    # but we keep it here so all the checking is together here.
    #
    # If any of my own field names already existed from my inherited classes, 
    # that's not allowed
    badFields <- NCint$allFieldNames_self %in% inheritNCinternals$allFieldNames
    if(any(badFields))
      stop(paste0("Problem with field(s): ", paste(NCint$allFieldNames_self[badFields], collapse = ", "),
                  ". Fields with the same name are not allowed in base and inherited classes.",
                  " (If you want to allow local fields of the same name in C++ by turning off this requirement,",
                " set nOptions(allow_inherited_field_duplicates=TRUE)"),
           call. = FALSE  )
  }
  NCint$check_inherit_done <- TRUE
  c(new_virtualMethodNames, inherit_virtualMethodNames)
}

NC_get_Cpub_class <- function(NCgenerator) {
  if(!isNCgenerator(NCgenerator))
    stop("Input to NC_get_Cpub_class must be a nClass generator.")
  NCgenerator$parent_env$.Cpub_class
}

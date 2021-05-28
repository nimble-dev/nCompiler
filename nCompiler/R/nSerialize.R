## In a fully functioning system, this would need to become the
# CppObj member of the deserialized (from R) R6 class "add_class_compiled"

## Deserialization will reverse the steps
##

# Invokes R serializes with NULL connection, binary representation and no byte-swapping.
# Reference objects dispatched to locally-defined function.

#' @export
nSerialize <- function(obj) {
    return(serialize(obj, NULL, FALSE, FALSE, refhook = serializeLOE))
}


getSerializationMgr <- function(LOE) {
    parent.env(LOE)$new_serialization_mgr
}


serializeLOE <- function(LOE) {
    if (class(LOE) != "loadedObjectEnv") {
        print(paste("Advisory:  not serializing reference class ", class(LOE)))
        newIndex <- -1
    }
    else {
        newIndex <- method(getSerializationMgr(LOE)(), "add_extptr")(nCompiler:::getExtptr(LOE))
    }
    as.character(newIndex)
}

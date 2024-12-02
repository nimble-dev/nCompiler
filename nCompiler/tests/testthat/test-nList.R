
library(nCompiler)

#debug(nCompiler:::argType2symbol)

foo <- nFunction(
  function() {
    ans <- nList('numericScalar', 3)
    x <- ans[[1]]
 ##   ans[[2]] <- x+1 # breaks at getVarInfo... for alias risk
    return(ans)
  },
  returnType = "nList('numericScalar')" # How do alternate modes of saying this go through?
)

set_nOption("startDebugStage", "labelAbstractTypes", "compilerOptions")
set_nOption("endDebugStage", "labelAbstractTypes", "compilerOptions")
nOptions(pause_after_writing_files=TRUE)
cfoo <- nCompile(foo)

set_nOption("startDebugStage", "end", "compilerOptions")
set_nOption("endDebugStage", "end", "compilerOptions")

foo <- nFunction(
  function() {
    ans <- nList('numericVector', 3)
    x <- ans[[1]]
    ans[[2]] <- x+1 # breaks at getVarInfo... for alias risk
    return(ans)
  },
  returnType = "nList('numericVector')" # How do alternate modes of saying this go through?
)

set_nOption("startDebugStage", "labelAbstractTypes", "compilerOptions")
set_nOption("endDebugStage", "labelAbstractTypes", "compilerOptions")
nOptions(pause_after_writing_files=TRUE)
cfoo <- nCompile(foo)

set_nOption("startDebugStage", "end", "compilerOptions")
set_nOption("endDebugStage", "end", "compilerOptions")

# STOPPED HERE
# NEED TO GET #define FLAG GENERATED
# AND THEN SOME OTHER KIND OF PROBLEM COMES UP
# I think it is the as/wrap obtained by the generic accessor
nc <- nClass(
  Cpublic = list(
#    list_vec = "nList('numericScalar')"
    list_vec = "nList('numericVector')",
    foo = nFunction(function(x = "nList('numericScalar')") {return(x); returnType("nList('numericScalar')")})
  )
)
nOptions(pause_after_writing_files = TRUE)
Cnc <- nCompile(nc)
obj <- Cnc$new()
obj$list_vec <- list(1:3, 2:5, 3:10)
obj$list_vec

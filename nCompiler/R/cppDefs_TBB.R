## cppDefs for parallel loop bodies for TBB

cppParallelBodyClass <- R6::R6Class(
  'cppParallelBodyClass',
  inherit = cppClassClass,
  portable = FALSE,
  public = list()
)

cppConstructorClass <- R6::R6Class(
  'cppConstructorClass',
  inherit = cppFunctionClass,
  portable = FALSE,
  public = list()
)
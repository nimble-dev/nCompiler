# test user-defined operator definitions
# These can be:
# 1. provided globally to define (or take over) a new keyword via registerOpDef
#    - precedence goes to a user-defined opDef over a built-in one
# 2. provided as an opDef list to an nFunction
# 3. provided as an opDef list (within compileInfo) to an nFunction that is an nClass method
# 4. provided as a list of opDef lists (within compileInfo) to an nClass

# The definition "closest" to the nFunction takes precedence.
#   e.g. if an nFunction has an opDef list, that takes precedence over
#     an opDef list provided to the nClass that contains the nFunction.


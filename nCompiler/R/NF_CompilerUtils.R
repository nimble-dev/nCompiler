compilerUtilsEnv <- new.env()

assignInCompilerUtilsEnv <- function(name, value) {
  compilerUtilsEnv[[name]] <- value
}

make_cpp_filebase <- function(cpp_code_name) {
  paste0(Rname2CppName(cpp_code_name), '_c_')
}

nf_substituteExceptFunctionsAndDollarSigns <- function(code, subList) {
    ## this is almost like doing substitute with code and subList, but it doesn't traverse the RHS of a '$' operator
    ## and it doesn't replace and function names
    if(is.character(code)) return(code)
    if(is.numeric(code)) return(code)
    if(is.logical(code)) return(code)
    if(is.null(code)) return(code)
    if(is.name(code)) {
        maybeAns <- subList[[as.character(code)]]
        return(
            if(is.null(maybeAns))
                code
            else
                maybeAns
        )
    }
    if(is.call(code)) {
        if(length(code) == 1)
            return(code)
        else {
            if(is.call(code[[1]]))
                indexRange <- 1:length(code)
            else {
                if(as.character(code[[1]])=='$')
                    indexRange <- 2
                else 
                    indexRange <- 2:length(code)
            }
        }
        for(i in indexRange)
            code[[i]] <-
                nf_substituteExceptFunctionsAndDollarSigns(code[[i]], subList)
        return(code)
    }
    if(is.list(code)) {
        ## Keyword processing stage of compilation may have stuck
        ## lists into the argument list of a call (for maps)
        code <- lapply(code,
                       nf_substituteExceptFunctionsAndDollarSigns,
                       subList)
        return(code)
    }
    stop(paste("Error doing replacement for code ", deparse(code)))
}

assignInCompilerUtilsEnv(
  'cpp_keywords',
  c(
    # C++ language keywords
    'alignas', 'alignof', 'and', 'and_eq', 'asm', 'auto', 'bitand', 'bitor',
    'bool', 'break', 'case', 'catch', 'char', 'char16_t', 'char32_t', 'class',
    'compl', 'const', 'constexpr', 'const_cast', 'continue', 'decltype',
    'default', 'delete', 'do', 'double', 'dynamic_cast', 'else', 'enum',
    'explicit', 'export', 'extern', 'false', 'float', 'for', 'friend', 'goto',
    'if', 'inline', 'int', 'long', 'mutable', 'namespace', 'new', 'noexcept',
    'not', 'not_eq', 'nullptr', 'operator', 'or', 'or_eq', 'private',
    'protected', 'public', 'register', 'reinterpret_cast', 'return', 'short',
    'signed', 'sizeof', 'static', 'static_assert', 'static_cast', 'struct',
    'switch', 'template', 'this', 'thread_local', 'throw', 'true', 'try',
    'typedef', 'typeid', 'typename', 'union', 'unsigned', 'using', 'virtual',
    'void', 'volatile', 'wchar_t', 'while', 'xor', 'xor_eq',
    # cmath keywords
    'cos', 'sin', 'tan', 'acos', 'asin', 'atan', 'atan2', 'cosh', 'sinh',
    'tanh', 'acosh', 'asinh', 'atanh', 'exp', 'frexp', 'ldexp', 'log', 'log10',
    'modf', 'exp2', 'expm1', 'ilogb', 'log1p', 'log2', 'logb', 'scalbn',
    'scalbln', 'pow', 'sqrt', 'cbrt', 'hypot', 'erf', 'erfc', 'tgamma',
    'lgamma', 'ceil', 'floor', 'fmod', 'trunc', 'round', 'lround', 'llround',
    'rint', 'lrint', 'llrint', 'nearbyint', 'remainder', 'remquo', 'copysign',
    'nan', 'nextafter', 'nexttoward', 'fdim', 'fmax', 'fmin', 'fabs', 'abs',
    'fma', 'fpclassify', 'isfinite', 'isinf', 'isnan', 'isnormal', 'signbit',
    'isgreater', 'isgreaterequal', 'isless', 'islessequal', 'islessgreater',
    'isunordered', 'math_errhandling', 'INFINITY', 'NAN', 'HUGE_VAL',
    'HUGE_VALF', 'HUGE_VALL', 'MATH_ERRNO', 'MATH_ERREXCEPT', 'FP_FAST_FMA',
    'FP_FAST_FMAF', 'FP_FAST_FMAL', 'FP_INFINITE', 'FP_NAN', 'FP_NORMAL',
    'FP_SUBNORMAL', 'FP_ZERO', 'FP_ILOGB0', 'FP_ILOGBNAN', 'double_t',
    'float_t'
  )
)

mangleArgumentNames <- function(argNames) {
  ans <- character()
  if(length(argNames) > 0) {
    conflicts <- which(argNames %in% compilerUtilsEnv$cpp_keywords)
    new_names <- paste0("ARG_", Rname2CppName(argNames[conflicts]), "_")
    ans <- argNames
    ans[conflicts] <- new_names
  }
  invisible(ans)
}

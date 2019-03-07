# nCompiler

`nCompiler` is designed to provide a new R development tool for code-generating C++ and easily interfacing between R and C++.  It harnesses numerous packages, including:

- Rcpp
- Eigen (included via RcppEigen)
- CppAD (included via RcppEigenAD)
- Cereal (included via Rcereal)
- Threading Building Blocks (in the future, to be included via RcppParallel)

`nCompiler` grew out of the `nimble` package.  `nimble` is a framework for hierarchical statistical models and algorithms such as Markov chain Monte Carlo (MCMC).   `nimble` includes the "nimble compiler", which code-generates C++ from models and algorithms written as `nimbleFunction`s.   Basic math, including distributions, vectorized math and linear algebra, as well as basic flow control, is automatically generated and interfaced from R without directly coding any C++.  The `nimble` developers recognized that the successful ideas from the nimble compiler could be redesigned and into a more general package that could provide a more general R programming tool.  We dubbed this "nCompiler" to give a first-letter call-out to its `nimble` roots but emphasize its compiler capabilities.

`nCompiler` will provide a C++ code-generation and integration system to support:
- Automatic C++ code-generation for many of R's math functions and possibly more R features.
- Classes and functions that support mixing of R and C++.
- Support for uncompiled execution (for debugging) or compiled execution of the same source code when possible.
- Automatic differentiation.
- Parallelization.
- Saving and loading compiled objects.
- Embedding `nCompiler` code into R packages.

To learn more and see examples, go to the User Manual, which is really a mix of what works and future goals.

Specifically, `nCompiler` provides two basic constructs:

- `nClass`: a class with reference semantics that can have some data and methods implemented in R and others in C++, either code-generated or hand-coded.  (`nClass` definitions inherit from R6 classes.)
- `nFunction`: a function that can be compiled via C++, either code-generated or hand-coded, with static types.  `nClass` methods are `nFunction`s.

Currently, the strategy for building `nCompiler` is to achieve working skeletons of all major design goals before fleshing out all details such as all function supported for C++ code-generation.

In more detail, some of the main goals for `nCompiler` and their status are as follows:

- Make use of the Tensor classes of the Eigen C++ library as the core numeric objects.  This differs from `nimble`, which code-generates for Eigen one- and two-dimensional arrays and matrices but does not support math above two dimensions.  This use of `Eigen::Tensor`s works but needs to be fleshed out.  A `nClass` or `nFunction` programmer simply declares a type like `numericArray`.
- Code-generate C++ from R math and distributions and automatically interface to `nFunction`s.  This works, but with limited functions supported.  Much remains to be ported from `nimble`, but the basic implementation is similar.  These features mean that a programmer can boost performance without directly coding any C++.
- Code-generate C++ and automatically interface to `nClass`es.  This works.  Interfaces can be "full", meaning there is a specialized class definition whose purpose it to access methods and fields in a compiled C++ object.  Or an interface can be "generic", meaning there is simply an external pointer with ad hoc access to data and methods available when needed.  The full interface design is inspired by but different from Rcpp modules.
- Provide basic flow-control structures such as if-then-else, for loops, etc.  These have not yet been ported from the `nimble` compiler, but doing so should be straightforward.
- Support either pass-by-copy or pass-by-reference.  Pass-by-reference is important for efficient compiled execution.  Pass-by-copy is important for R-like semantics.  In `nimble`, code-generated C++ always uses pass-by-reference, but there is some inconsistent behavior between compiled and uncompiled execution.   C++ classes to implement these options have been drafted but not integrated into code-generation.
- Make use of the CppAD C++ library to provide automatic differentiation from `nFunction` code.   This ports features from `nimble` that are development versions but are not released.  This works in a basic way.  It allows a programmer to simply declare that they need derivatives of a function and obtain them automatically.  `nCompiler` generates the necessary CppAD code.
- Make use of the cereal C++ library for serialization.  This will allow `nClass` objects to be naturally saved and loaded.   It allows the programmer to simply save an object.  Serialization code for cereal is automatically generated and used.   This works, but handling of objects that point to each other remains to be developed.
- Allow `nClass` and `nFunction` code to be easily integrated into R libraries.  This works.
- Allow directly coded C++ and code-generated C++ to be mixed and matched.  This capability is not built into `nimble` and has not been developed yet.
- Make use of TBB (Intel Threading Building Blocks) for automatic parallelization.  The idea here is that a `nClass` or `nFunction` programmer will be able to specify where paralellization should happen, what objects need to be shared or copied, and automatically generate a TBB implementation.  This capability has been prototyped in C++, but code-generation has not been developed yet.
- Support access to more kinds of native R objects.  This has not be implemented in code-generation.  It will be facilitied by Rcpp.
- Harness many features of Rcpp:  its compilation management; its C++ annotation for automatically generating R  functions that interface to compiled functions (Rcpp::export) and automatically manage package dependencies (Rcpp::depends and Rcpp::plugin); its `as<>` and `wrap<>` C++ template system; and its classes for easily coding C++ that uses R objects.

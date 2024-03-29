% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NC.R
\name{nClass}
\alias{nClass}
\title{Create a nClass definition}
\usage{
nClass(
  classname,
  Rpublic = list(),
  Cpublic = list(),
  enableDerivs = character(),
  predefined = FALSE,
  env = parent.frame()
)
}
\arguments{
\item{classname}{A name for the class.}

\item{Rpublic}{A list of public data and methods (functions) for
use from R only.}

\item{Cpublic}{A list of public data (with type declarations) and
methods (nFunctions) that can be turned into C++ via
\link{nCompile_nClass}.  As in R6 classes (see
\link{R6Class}), data and methods go in the same list.}

\item{enableDerivs}{A list or character vector of methods in Cpublic 
for which derivatives should be enabled.}

\item{env}{An environment that should be used as the
enclosing environment for objects created from the nClass}
}
\value{
An R6 class generator enhanced with information needed for
    compilation.
}
\description{
A nClass is like an R6 class, but it allows some data and
methods (written as nFunctions) to be automatically compiled
via C++, while others are in pure R.  nClasses are implemented
using R6 classes.
}
\details{
The internal information used for compilation can be
    accessed with \code{NCinternals(nClassGenerator)} and
    modified with \code{NCinternals(nClassGenerator)<-}, where
    \code{nClassGenerator} was returned from a call to
    \code{nClass}.
}
\examples{
\donttest{
    nc1 <- nClass(
    Rpublic = list(
        Rv = NULL,
        Rfoo = function(x) x+1
    ),
    Cpublic = list(
        Cv = 'numericScalar',
        Cfoo = nFunction(
            fun = function(x) {
                return(x+1)
            },
            argTypes = list(x = 'numericScalar'),
            returnType = 'numericScalar')
        )
  )
}
}

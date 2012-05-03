\name{packageEnv}
\alias{isPackageInstalled}
\alias{packageEnv}
\alias{packageName}
\alias{packagePath}
\title{Package Development Utilities}
\usage{
  packageEnv(envir = parent.frame(),
    matchThisEnv = getOption("topLevelEnvironment"))

  packageName()

  packagePath(..., PACKAGE = NULL)

  isPackageInstalled(..., lib.loc = NULL)
}
\arguments{
  \item{...}{arguments passed to \code{\link{file.path}}.}

  \item{envir}{environment.}

  \item{matchThisEnv}{return this environment, if it
  matches before any other criterion is satisfied.  The
  default, the option \samp{topLevelEnvironment}, is set by
  \code{\link{sys.source}}, which treats a specific
  environment as the top level environment.  Supplying the
  argument as \code{NULL} means it will never match.}
}
\value{
  an environment

  a character string

  a character string
}
\description{
  \code{packageEnv} is a slight modification from
  \code{\link{topenv}}, which returns the caller's top
  environment, also in the case of development packages
  which are wrapped in an environment by
  \code{\link[devtools]{load_all}}.

  \code{packageName} returns the current package's name.

  \code{packagePath} returns the current package's root
  directory, which is its installation/loading directory in
  the case of an installed package, or its source directory
  served by devtools.

  Tests if a package is installed
}


\name{packageEnv}
\alias{isPackageInstalled}
\alias{packageEnv}
\alias{packageName}
\alias{packagePath}
\title{Package Development Utilities}
\usage{
  packageEnv(pkg)

  packageName()

  packagePath(..., PACKAGE = NULL)

  isPackageInstalled(..., lib.loc = NULL)
}
\arguments{
  \item{pkg}{package name. If missing the environment of
  the caller package is returned.}

  \item{...}{arguments passed to \code{\link{file.path}}.}
}
\value{
  an environment

  a character string

  a character string
}
\description{
  \code{packageEnv} is a slight modification from
  \code{\link{topenv}}, which returns the caller's top
  environment, which in the case of development packages is
  the environment into which the source files are loaded by
  \code{\link[devtools]{load_all}}.

  \code{packageName} returns the current package's name.

  \code{packagePath} returns the current package's root
  directory, which is its installation/loading directory in
  the case of an installed package, or its source directory
  served by devtools.

  Tests if a package is installed
}

